%%%-------------------------------------------------------------------
%% @doc glass_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(glass_backend_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    glass_backend_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
