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
  gen_server:call(?MODULE, {search, Workspace, Query, Node}).

init(_Args) ->
  {ok, #state{
    next_id = 1,
    processes = []
  }}.

handle_cast(Msg, State) ->
  ?LOG_ERROR("Unknown cast message ~p", [Msg]),
  {noreply, State}.

handle_call({search, Workspace, QuerySource, Node}, _From, State) ->
  Query = glass_query:parse(QuerySource),
  Id = State#state.next_id,
  Pid = spawn(fun() -> run_search(Workspace, Query, Node, Id) end),
  { reply
  , {ok, Id}
  , State#state{
    next_id = Id + 1,
    processes = [{Node, Pid, Id}]
  }};

handle_call(Msg, From, State) ->
  ?LOG_ERROR("Unknown call message ~p from ~p", [Msg, From]),
  {noreply, State}.


run_search(Workspace, Query, Node, Id) ->
  io:format("*** Searching ~p~n", [Workspace]),
  Entities = glass_index_server:get_entities(Workspace, function),
  State = #search_state{
    workspace = Workspace,
    node = Node,
    id = Id
  },
  lists:foreach(fun({EntityId, Form, Meta}) ->
                  Results = glass_query:unify(Query, Form),
                  publish(State, {EntityId, Form, Meta}, Results)
                end, Entities),
  rpc_finish(Id, Node),
  io:format("*** Search completed.~n").

publish(State, Form, Results) ->
  lists:foreach(fun(Result) -> publish_result(State, Form, Result) end, Results).

publish_result(State, Form, Result) ->
  Node = State#search_state.node,
  Id = State#search_state.id,
  Workspace = State#search_state.workspace,
  rpc_result(Id, Result, Node),
  glass_report:on_result(Workspace, Id, Form, Result).

rpc_result(Id, Result, Node) ->
  case node() of
    Node -> no_op; % local node, do nothing
    _ -> rpc:call(Node, 'Elixir.GlassCLI', result, [Id, Result])
  end.

rpc_finish(Id, Node) ->
  case node() of
    Node -> no_op;
    _ -> rpc:call(Node, 'Elixir.GlassCLI', finish, [Id])
  end.
