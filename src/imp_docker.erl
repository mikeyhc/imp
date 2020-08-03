-module(imp_docker).

-export([run/2, run/3]).

%% API functions

run(Image, Version) ->
    run(Image, Version, []).

run(Image, Version, Args) ->
    case docker("run", ["-d", Image ++ ":" ++ Version|Args]) of
        {0, Id} -> {ok, string:trim(Id)};
        Err -> {error, Err}
    end.

%% internal functions

docker(Cmd, Args) ->
    imp_exec:exec(string:join(["docker", Cmd|Args], " ")).
