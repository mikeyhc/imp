-module(imp_git).
-export([new/1, new/3, init/1, clone/1, pull/1, rev_parse/2, diff/4,
         ls_files/1, dir/1]).

-record(git, {dir :: string(),
              remote :: string() | undefined,
              token :: string() | undefined
             }).
-type git() :: #git{}.

%% API functions

-spec new(string()) -> git().
new(Dir) ->
    #git{dir=Dir}.

-spec new(string(), string(), string()) -> git().
new(Dir, Remote, Token) ->
    #git{dir=Dir,
         remote=Remote,
         token=Token}.

dir(Git) ->
    Git#git.dir.

init(Git) ->
    {0, _} = git(Git, "init"),
    ok.

clone(#git{dir=Dir, remote=Remote, token=Token}) ->
    Cmd = "git clone https://x-access-token:" ++ Token ++ "@" ++ Remote ++ " "
        ++ Dir,
    {0, _} = imp_exec:exec(Cmd),
    ok.

pull(Git) ->
    {0, _} = git(Git, "pull"),
    ok.

rev_parse(Git, Ref) ->
    {0, Sha} = git(Git, "rev-parse", [Ref]),
    string:trim(Sha).

diff(Git, Start, End, Options) ->
    {0, Output} = git(Git, "diff", Options ++ [Start, End]),
    lists:filter(fun empty_filter/1, string:split(Output, "\n", all)).

ls_files(Git) ->
    {0, Output} = git(Git, "ls-files"),
    lists:filter(fun empty_filter/1, string:split(Output, "\n", all)).

%% internal functions

git(Git, Cmd) -> git(Git, Cmd, []).

git(#git{dir=Dir}, Cmd, Args) ->
    imp_exec:exec(string:join(["git", "-C", Dir, Cmd|Args], " ")).

empty_filter([]) -> false;
empty_filter([_|_]) -> true.
