-module(imp_docker).

-export([run/2, run/3, stop/1, rm/1, ps/0, ps/1]).

%% API functions

run(Image, Version) ->
    run(Image, Version, []).

run(Image, Version, Args) ->
    case docker("run", Args ++ ["-d", Image ++ ":" ++ Version]) of
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

%% internal functions

docker(Cmd, Args) ->
    imp_exec:exec(string:join(["docker", Cmd|Args], " ")).
