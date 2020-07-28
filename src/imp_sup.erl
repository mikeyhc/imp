%%%-------------------------------------------------------------------
%% @doc imp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(imp_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Token, ConfigRepo) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Token, ConfigRepo]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([Token, ConfigRepo]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => config_puller,
                    start => {config_puller, start_link, [Token, ConfigRepo]}}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
