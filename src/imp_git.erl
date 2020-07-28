-module(imp_git).
-export([new/3, init/1, clone/1]).

-record(git, {dir :: string(),
              remote :: string(),
              token :: string()
             }).
-type git() :: #git{}.

-spec new(string(), string(), string()) -> git().
new(Dir, Remote, Token) ->
    #git{dir=Dir,
         remote=Remote,
         token=Token}.

init(Git) ->
    {0, _} = git(Git, "init"),
    ok.

clone(#git{dir=Dir, remote=Remote, token=Token}) ->
    Cmd = "git clone https://x-access-token:" ++ Token ++ "@" ++ Remote ++ " "
        ++ Dir,
    {0, _} = exec(Cmd),
    ok.

git(Git, Cmd) -> git(Git, Cmd, []).

git(#git{dir=Dir}, Cmd, Args) ->
    exec(string:join(["git", "-C", Dir, Cmd|Args], " ")).

exec(Command) ->
    Port = open_port({spawn, Command},
                     [stream, in, eof, hide, exit_status, stderr_to_stdout]),
    get_data(Port, []).

get_data(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, [Sofar|Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} -> true
            end,
            receive
                {'EXIT',  Port,  _} -> ok
            after 1 -> ok   % force context switch
            end,
            ExitCode = receive
                           {Port, {exit_status, Code}} -> Code
                       end,
            {ExitCode, lists:flatten(Sofar)}
    end.
