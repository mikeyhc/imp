-module(config_runner).
-behaviour(gen_server).

-export([start_link/3, notify_update/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {git :: imp_git:git(),
                last_commit :: string() | undefined,
                handlers :: [atom()],
                change_handlers :: [atom()]
               }).

%% API functions

start_link(Dir, Handlers, ChangeHandlers) ->
    gen_server:start_link(?MODULE, [Dir, Handlers, ChangeHandlers], []).

notify_update(Pid) ->
    gen_server:cast(Pid, updated).

%% gen_server callbacks

init([Dir, Handlers, ChangeHandlers]) ->
    {ok, #state{git=imp_git:new(Dir), handlers=Handlers,
                change_handlers=ChangeHandlers}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(updated,
            State=#state{git=Git, last_commit=Last, handlers=Handlers,
                         change_handlers=ChangeHandlers}) ->
    Current = imp_git:rev_parse(Git, "HEAD"),
    if Current =:= Last ->
           logger:info("repo still at commit ~s", [Current]),
           run_handlers(Git, Handlers),
           {noreply, State};
       true ->
           handle_update(Git, Current, Last, ChangeHandlers),
           run_handlers(Git, Handlers),
           {noreply, State#state{last_commit=Current}}
    end;
handle_cast({commit_and_push, Files}, State=#state{git=Git}) ->
    lists:foreach(fun(File) -> imp_git:add(Git, File) end, Files),
    imp_git:commit(Git, "deployment upgrade"),
    imp_git:push(Git),
    Current = imp_git:rev_parse(Git, "HEAD"),
    logger:info("pushed commit with id ~s", [Current]),
    {noreply, State#state{last_commit=Current}}.

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
    Fn = fun(F) ->
                 Runs = lists:map(fun(H) -> H:handle(Dir, F) end, Handlers),
                 {F, lists:member(update, Runs)}
         end,
    Filter = fun({File, true}) -> {true, File};
                (_) -> false
             end,
    Changes = lists:filtermap(Filter, lists:map(Fn, Files)),
    case Changes of
        [] ->
            logger:info("no changes required"),
            ok;
        _ ->
            logger:info("found changes, creating a new commit"),
            gen_server:cast(self(), {commit_and_push, Changes})
    end.

run_handlers(Git, Handlers) ->
    Files = imp_git:ls_files(Git),
    logger:info("found ~w files", [length(Files)]),
    handle_changes(imp_git:dir(Git), Files, Handlers).
