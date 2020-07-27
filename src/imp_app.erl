%%%-------------------------------------------------------------------
%% @doc imp public API
%% @end
%%%-------------------------------------------------------------------

-module(imp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    imp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
