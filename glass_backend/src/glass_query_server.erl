-module(glass_query_server).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("glass_ast.hrl").

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

handle_cast({search, Workspace, QuerySource, Node}, State) ->
  Query = glass_query:parse(QuerySource),
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
  Entities = glass_index_server:get_entities(Workspace, function),
  State = #search_state{
    workspace = Workspace,
    node = Node,
    id = Id
  },
  lists:foreach(fun({EntityId, Form, Meta}) ->
                  Results = glass_query:unify(Query, Form),
                  publish(State, {EntityId, Form, Meta}, Results)
                end, Entities).

publish(State, Form, Results) ->
  lists:foreach(fun(Result) -> publish_result(State, Form, Result) end, Results).

publish_result(State, {EntityId, Form, Meta}, {_Env, Match}) ->
  _Node = State#search_state.node,
  _Id = State#search_state.id,
  _Workspace = State#search_state.workspace,
  show({EntityId, Form, Meta}, Match).
  % rpc:call(Node, glass_report, on_result, [Workspace, Id, Form, Result]).

show({{function, {Name, Arity}}, Form, Meta}, Match) ->
  FormLine = glass_ast:get_line(Form),
  ErlangAst = glass_ast:glass_to_node(Match),
  Line = glass_ast:get_line(Match),
  Code = erl_prettypr:format(erl_syntax:form_list([ErlangAst])),
  Filename = proplists:get_value(glass_filename, Meta),
  io:format("%% in ~p/~p at ~s:~p~n~p| ~s~n~n",
            [Name, Arity, Filename, FormLine, Line, Code]).