-module(config_puller).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-define(INTERVAL, 300000). % 5 minutes
-define(DIR, "priv/repo").

-record(state, {token :: string(),
                repo :: string(),
                timer :: timer:tref() | undefined,
                git :: imp_git:git() | undefined
               }).

%% API functions

start_link(Token, ConfigRepo) ->
    gen_server:start_link(?MODULE, [Token, ConfigRepo], []).

%% gen_server callbacks

init([Token, ConfigRepo]) ->
    gen_server:cast(self(), start),
    {ok, #state{token=Token, repo=ConfigRepo}}.

terminate(_Reason, #state{timer=TRef}) ->
    stop_timer(TRef).

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(start, State=#state{repo=Repo, token=Token}) ->
    Git = imp_git:new(?DIR, Repo, Token),
    case filelib:is_dir(Repo) of
        true -> ok;
        false ->
            ok = imp_git:clone(Git)
    end,
    {ok, TRef} = start_timer(),
    {noreply, State#state{timer=TRef}};
handle_cast(run, State) ->
    logger:info("pulling configuration repository"),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% internal functions

start_timer() ->
    logger:info("starting config pull timer"),
    gen_server:cast(self(), run),
    timer:apply_interval(?INTERVAL, gen_server, cast, [self(), run]).

stop_timer(TRef) ->
    {ok, cancel} = timer:cancel(TRef),
    ok.
