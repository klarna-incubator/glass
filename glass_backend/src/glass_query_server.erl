-module(glass_query_server).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("glass_ast.hrl").

-compile(export_all).

-export([
  start_link/0,
  search/3
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2
]).

-record(state, {
  next_id :: integer(),
  processes :: [{node(), pid(), integer()}]
}).

-record(search_state, {
  workspace :: atom(),
  node :: node(),
  id :: integer()
}).

-type query() ::
  {bind, Variable :: atom()}
| {form, Type :: atom(), Attributes :: map(), Children :: [query()]}
| {constrain, query(), constraint()}
| hole.

-type constraint() ::
  {'and', constraint(), constraint()}
| {'or', constraint(), constraint()}
| {'not', constraint()}
| {'=', constraint_term(), constraint_term()}
| constraint_term().

-type constraint_term() ::
  {variable, atom()}
| {value, term()}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

search(Workspace, Query, Node) ->
  gen_server:cast(?MODULE, {search, Workspace, Query, Node}).


init(_Args) ->
  {ok, #state{
    next_id = 1,
    processes = []
  }}.

handle_call(Msg, From, State) ->
  ?LOG_ERROR("Unknown call message ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast({search, Workspace, Query, Node}, State) ->
  Id = State#state.next_id,
  Pid = spawn(fun() -> run_search(Workspace, Query, Node, Id) end),
  {noreply, State#state{
    next_id = Id + 1,
    processes = [{Node, Pid, Id}]
  }};

handle_cast(Msg, State) ->
  ?LOG_ERROR("Unknown cast message ~p", [Msg]),
  {noreply, State}.


run_search(Workspace, Query, Node, Id) ->
  Forms = lists:map(fun entity_to_form/1, glass_index_server:get_entities(Workspace, function)),
  State = #search_state{
    workspace = Workspace,
    node = Node,
    id = Id
  },
  lists:foreach(fun(Form) -> publish(State, Form, match_form(Query, Form)) end, Forms).

publish(State, Form, Results) ->
  lists:foreach(fun(Result) -> publish_result(State, Form, Result) end, Results).

publish_result(State, Form, Result) ->
  _Node = State#search_state.node,
  Id = State#search_state.id,
  Workspace = State#search_state.workspace,
  io:format("[workspace: ~p id: ~p]~nform: ~p~nresult: ~p", [Workspace, Id, Form, Result]).
  % rpc:call(Node, glass_report, on_result, [Workspace, Id, Form, Result]).

entity_to_form({_, Form, _}) -> Form.

parse_query(QueryString) ->
  {ok, Tokens} = glass_parser:tokenise(QueryString ++ "."),
  {ok, [Expr]} = glass_parser:parse_exprs(Tokens),
  compile_query(Expr).

compile_query(Query) ->
  glass_to_query(glass_ast:node_to_glass(Query)).

glass_to_query(#glass_node{type = var, attributes = #{ name := '_'}}) ->
  hole;
glass_to_query(#glass_node{type = var, attributes = #{ name := Name }}) ->
  {bind, Name};
glass_to_query(#glass_node{type = Type, attributes = Attrs, children = Children }) ->
  {form, Type, maps:without([position], Attrs), lists:map(fun glass_to_query/1, Children)}.

match_form(Query, Form) ->
  glass_ast:search(Form, fun(Node) -> apply_query(Query, Node) end).

apply_query(Query, Node) ->
  case match_form(#{}, Query, Node) of
    false -> false;
    Env -> {true, {Env, Node}}
  end.

match_form(false, _, _) ->
  false;
match_form(Env, hole, _Form) ->
  Env;
match_form(Env, {bind, Variable}, Form) ->
  assign(Env, Variable, Form);
match_form(
  Env,
  {form, Type, Attrs, Children},
  #glass_node{type = Type1, attributes = Attrs1, children = Children1}
) ->
  case Type =:= Type1 andalso attributes_match(Attrs, Attrs1) of
    true ->
      match_forms(Env, Children, Children1);
    false ->
      false
  end.

match_forms(Env, [], []) ->
  Env;
match_forms(Env, [L | LRest], [R | RRest]) ->
  case match_form(Env, L, R) of
    false -> false;
    NewEnv -> match_forms(NewEnv, LRest, RRest)
  end;
match_forms(_, _, _) ->
  false.

assign(false, _, _) ->
  false;
assign(Env, Variable, Form) ->
  case maps:find(Variable, Env) of
    error -> Env#{ Variable => Form };
    {ok, BoundForm} ->
      case form_matches(Form, BoundForm) of
        true -> Env;
        false -> false
      end;
    _ -> false
  end.

form_matches(
  #glass_node{type = Type, attributes = Attrs1, children = Children1},
  #glass_node{type = Type, attributes = Attrs2, children = Children2}
) ->
  attributes_match(Attrs1, Attrs2) 
    andalso lists:all(fun({A, B}) -> form_matches(A, B) end, lists:zip(Children1, Children2)).

attributes_match(Map1, Map2) ->
  minimal_attributes(Map1) =:= minimal_attributes(Map2).

minimal_attributes(Map) ->
  maps:without([position], Map).
