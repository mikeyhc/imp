-module(imp_docker).

-export([run/2, run/3, stop/1, rm/1]).

%% API functions

run(Image, Version) ->
    run(Image, Version, []).

run(Image, Version, Args) ->
    case docker("run", ["-d", Image ++ ":" ++ Version|Args]) of
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

%% internal functions

docker(Cmd, Args) ->
    imp_exec:exec(string:join(["docker", Cmd|Args], " ")).
