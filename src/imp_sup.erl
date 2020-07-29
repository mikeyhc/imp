%%%-------------------------------------------------------------------
%% @doc imp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(imp_sup).

-behaviour(supervisor).

-export([start_link/2, get_config_runner/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(DIR, "priv/repo").

start_link(Token, ConfigRepo) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Token, ConfigRepo]).

get_config_runner() ->
    Children = supervisor:which_children(?SERVER),
    {_, Pid, _, _} = lists:keyfind(config_runner, 1, Children),
    Pid.

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
                    start => {config_puller, start_link,
                              [?DIR, Token, ConfigRepo]}},
                  #{id => config_runner,
                    start => {config_runner, start_link, [?DIR]}}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
