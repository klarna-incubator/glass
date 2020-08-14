%%%-------------------------------------------------------------------
%% @doc glass_backend top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(glass_backend_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
      index_server_spec(),
      query_server_spec()
    ],
    {ok, {SupFlags, ChildSpecs}}.

index_server_spec() ->
  #{
    id => glass_index_server,
    start => {glass_index_server, start_link, []},
    restart => permanent,
    type => worker
  }.

query_server_spec() ->
  #{
    id => glass_query_server,
    start => {glass_query_server, start_link, []},
    restart => permanent,
    type => worker
  }.

%% internal functions
