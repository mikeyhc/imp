-module(config_puller).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-define(INTERVAL, 300000). % 5 minutes

-record(state, {dir :: string(),
                token :: string(),
                repo :: string(),
                timer :: timer:tref() | undefined,
                git :: imp_git:git() | undefined,
                last_commit :: string() | undefined
               }).

%% API functions

start_link(Dir, Token, ConfigRepo) ->
    gen_server:start_link(?MODULE, [Dir, Token, ConfigRepo], []).

%% gen_server callbacks

init([Dir, Token, ConfigRepo]) ->
    gen_server:cast(self(), start),
    {ok, #state{dir=Dir, token=Token, repo=ConfigRepo}}.

terminate(_Reason, #state{timer=TRef}) ->
    case TRef of
        undefined -> ok;
        _ -> stop_timer(TRef)
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(start, State=#state{dir=Dir, repo=Repo, token=Token}) ->
    Git = imp_git:new(Dir, Repo, Token),
    case filelib:is_dir(Dir) of
        true -> ok;
        false ->
            ok = imp_git:clone(Git)
    end,
    {ok, TRef} = start_timer(),
    {noreply, State#state{timer=TRef, git=Git}};
handle_cast(run, State=#state{git=Git}) ->
    ok = imp_git:pull(Git),
    ConfigRunner = imp_sup:get_config_runner(),
    config_runner:notify_update(ConfigRunner),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% internal functions

start_timer() ->
    logger:info("starting config pull job"),
    gen_server:cast(self(), run),
    timer:apply_interval(?INTERVAL, gen_server, cast, [self(), run]).

stop_timer(TRef) ->
    {ok, cancel} = timer:cancel(TRef),
    ok.
