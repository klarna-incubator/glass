-module(glass_index_server).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("glass_index.hrl").

-compile(export_all).

-export([ start_link/0
        , get_entities/2
        , index/2
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-record(state, {
  index :: ets:tid() | atom()
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_entities(Workspace, Type) ->
  gen_server:call(?MODULE, {get_entities, Workspace, Type}).

index(Workspace, Path) ->
  gen_server:cast(?MODULE, {index, Workspace, Path}).


init(_Args) ->
  {ok, #state{
    index = new_index()
  }}.

handle_call({get_entities, Workspace, Type}, _From, State) ->
  Entities = find_entities(State#state.index, Workspace, Type),
  {reply, Entities, State};

handle_call(Msg, From, State) ->
  ?LOG_ERROR("Received unknown call message ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast({index, Workspace, Path}, State) ->
  spawn(fun() -> index_workspace(Workspace, Path, State#state.index) end),
  {noreply, State};

handle_cast(Msg, State) ->
  ?LOG_ERROR("Received unknown cast message ~p", [Msg]),
  {noreply, State}.


find_entities(Index, Workspace, Type) ->
  Spec = ets:fun2ms(
          fun(
            #glass_index_entry{
                path = {EntryWorkspace, _, _, {EntryType, _}},
                id = Id,
                form = Form
            }
          ) when EntryWorkspace =:= Workspace andalso EntryType =:= Type ->
            {Id, Form}
          end
         ),
  Entities = ets:select(Index, Spec),
  lists:map(fun({Id, Form}) -> {Id, Form, find_metadata(Index, Id)} end, Entities).

find_metadata(Index, Id) ->
  Spec = ets:fun2ms(
           fun(#glass_index_metadata{id = MetaId, field = Key, value = Value})
           when MetaId =:= Id ->
             {Key, Value}
           end
         ),
  ets:select(Index, Spec).

index_workspace(Workspace, Root, Index) ->
  Beams = beams_in_directory(Root),
  lists:foreach(fun(Beam) -> index_beam(Workspace, Beam, Index) end, Beams).

index_beam(Workspace, Beam, Index) ->
  {App, Module, Forms} = get_beam_forms(Beam),
  lists:foreach(fun(Form) ->
                  index_beam_form(Workspace, {App, Module, Beam}, Form, Index)
                end,
                Forms).

index_beam_form(Workspace, {App, Module, _Beam}, Form, Index) ->
  case form_id(Form) of
    none ->
      ok;
    Id ->
      GlassNode = glass_ast:node_to_glass(Form),
      ets:insert(Index, #glass_index_entry{
        path = {Workspace, App, Module, Id},
        id = Id,
        form = GlassNode
      }),
      ok
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
  ets:new(?MODULE, [ordered_set, named_table, {keypos, #glass_index_entry.path}, public]).
