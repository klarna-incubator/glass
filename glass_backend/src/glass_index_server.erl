-module(glass_index_server).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("glass_index.hrl").

-define(ENTITY_INDEX, glass_index_entities).
-define(ENTITY_META, glass_index_metadata).

-export([ start_link/0
        , get_entities/2
        , index/3
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_entities(Workspace, Type) ->
  gen_server:call(?MODULE, {get_entities, Workspace, Type}).

index(Workspace, Path, Node) ->
  gen_server:cast(?MODULE, {index, Workspace, Path, Node}).


init(_Args) ->
  new_index(),
  {ok, #state{}}.

handle_call({get_entities, Workspace, Type}, _From, State) ->
  Entities = find_entities(Workspace, Type),
  {reply, Entities, State};

handle_call(Msg, From, State) ->
  ?LOG_ERROR("Received unknown call message ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast({index, Workspace, Path, Node}, State) ->
  spawn(fun() -> index_workspace(Workspace, Path, Node) end),
  {noreply, State};

handle_cast(Msg, State) ->
  ?LOG_ERROR("Received unknown cast message ~p", [Msg]),
  {noreply, State}.


find_entities(Workspace, _Type) ->
  Spec = ets:fun2ms(
          fun({EntityWorkspace, Entities}) when EntityWorkspace =:= Workspace ->
            Entities
          end),
  [Entities] = ets:select(?ENTITY_INDEX, Spec),
  Entities.

index_workspace(Workspace, Root, Node) ->
  io:format("*** Indexing ~p~n", [Workspace]),
  Beams = beams_in_directory(Root),
  Entities = lists:flatmap(fun(Beam) -> index_beam(Workspace, Beam) end, Beams),
  ets:insert(?ENTITY_INDEX, {Workspace, Entities}),
  case node() of
    Node -> no_op;
    _ -> rpc:call(Node, 'Elixir.GlassCLI', finish_index, [])
  end,
  io:format("*** Finished indexing ~p~n", [Workspace]).

index_beam(Workspace, Beam) ->
  io:format("*** Indexing ~p in ~p~n", [Beam, Workspace]),
  {App, Module, Forms} = get_beam_forms(Beam),
  lists:flatmap(fun(Form) ->
                  index_beam_form(Workspace, {App, Module, Beam}, Form)
                end,
                Forms).

index_beam_form(Workspace, {App, Module, Beam}, Form) ->
  case form_id(Form) of
    none ->
      [];
    Id ->
      GlassNode = glass_ast:node_to_glass(Form),
      [{
        {Workspace, App, Module, Id},
        GlassNode,
        [
          {?glass_position, form_position(Form)},
          {?glass_filename, Beam}
        ]
      }]
  end.

form_id({function, _, Name, Arity, _}) -> {function, {Name, Arity}};
form_id(_) -> none.

form_position({function, Line, _, _, _}) -> {Line, 0};
form_position(_) -> none.

get_beam_forms(Beam) ->
  {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
    beam_lib:chunks(Beam, [abstract_code]),
  {get_app_name(Beam, Module), Module, Forms}.

get_app_name(_Filename, _Module) ->
  app. %% TODO: get app name for beam

beams_in_directory(Root) ->
  Files = get_all_files(Root),
  lists:filter(fun(Filename) -> ends_with(".beam", Filename) end, Files).

get_all_files(Root) ->
  {ok, Files} = file:list_dir(Root),
  lists:flatmap(fun(File) -> get_subdirectory_files(Root, File) end, Files).

get_subdirectory_files(Root, File) ->
  Fullpath = filename:absname_join(Root, File),
  case filelib:is_dir(Fullpath) of
    true -> get_all_files(Fullpath);
    false -> [Fullpath]
  end.

ends_with(Suffix, Filename) ->
  ReverseSuffix = lists:reverse(Suffix),
  ReverseFilename = lists:reverse(Filename),
  string:prefix(ReverseFilename, ReverseSuffix) =/= nomatch.

new_index() ->
  ets:new(?ENTITY_INDEX, [ordered_set, named_table, public]).
