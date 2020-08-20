-module(imp_docker).

-export([run/2, run/3, run/4, stop/1, rm/1, ps/0, ps/1, inspect/1]).

%% API functions

run(Image, Version) ->
    run(Image, Version, []).

run(Image, Version, Opts) ->
    run(Image, Version, Opts, []).

run(Image, Version, Opts, Cmd) ->
    case docker("run", Opts ++ ["-d", Image ++ ":" ++ Version] ++ Cmd) of
        {0, Id} -> {ok, string:trim(Id)};
        Err -> {error, Err}
    end.

stop(Container) ->
    case docker("stop", Container) of
        {0, _} -> ok;
        {_, Msg} -> {error, string:trim(Msg)}
    end.

rm(Container) ->
    case docker("rm", Container) of
        {0, _} -> ok;
        {_, Msg} -> {error, string:trim(Msg)}
    end.

ps() -> ps([]).

ps(Args) ->
    case docker("ps", Args) of
        {0, Msg} -> {ok, string:trim(Msg)};
        {_, Msg} -> {error, string:trim(Msg)}
    end.

inspect(Id) ->
    case docker("inspect", [Id]) of
        {0, Msg} -> {ok, Msg};
        {_, Msg} -> {error, string:trim(Msg)}
    end.

%% internal functions

docker(Cmd, Args) ->
    imp_exec:exec(string:join(["docker", Cmd|Args], " ")).
