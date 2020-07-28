%%%-------------------------------------------------------------------
%% @doc imp public API
%% @end
%%%-------------------------------------------------------------------

-module(imp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    AuthToken = getenv("IMP_TOKEN"),
    ConfigRepo = getenv("IMP_CONFIG_REPO"),
    imp_sup:start_link(AuthToken, ConfigRepo).

stop(_State) ->
    ok.

%% internal functions

getenv(Name) ->
    case os:getenv(Name) of
        false -> throw({missing_envvar, Name});
        V -> V
    end.
