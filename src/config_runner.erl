-module(config_runner).
-behaviour(gen_server).

-export([start_link/2, notify_update/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {git :: imp_git:git(),
                last_commit :: string() | undefined,
                handlers :: [atom()]
               }).

%% API functions

start_link(Dir, Handlers) ->
    gen_server:start_link(?MODULE, [Dir, Handlers], []).

notify_update(Pid) ->
    gen_server:cast(Pid, updated).

%% gen_server callbacks

init([Dir, Handlers]) ->
    {ok, #state{git=imp_git:new(Dir), handlers=Handlers}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(updated,
            State=#state{git=Git, last_commit=Last, handlers=Handlers}) ->
    Current = imp_git:rev_parse(Git, "HEAD"),
    if Current =:= Last ->
           logger:info("repo still at commit ~s, no action taken", [Current]),
           {noreply, State};
       true ->
           handle_update(Git, Current, Last, Handlers),
           {noreply, State#state{last_commit=Current}}
    end.

%% internal functions

handle_update(Git, _Current, undefined, Handlers) ->
    logger:info("first run, running all files"),
    Files = imp_git:ls_files(Git),
    logger:info("found ~w files", [length(Files)]),
    handle_changes(imp_git:dir(Git), Files, Handlers);
handle_update(Git, Current, Last, Handlers) ->
    logger:info("comparing ~s to ~s", [Last, Current]),
    Files = imp_git:diff(Git, Current, Last, ["--name-only"]),
    logger:info("found ~w updates", [length(Files)]),
    handle_changes(imp_git:dir(Git), Files, Handlers).

handle_changes(Dir, Files, Handlers) ->
    Fn = fun(F) -> lists:foreach(fun(H) -> H:handle(Dir, F) end, Handlers) end,
    lists:foreach(Fn, Files).
