-module(rsocket_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags =
        #{
            strategy => one_for_all,
            intensity => 0,
            period => 1
        },
    RSocketConnectionSup =
        #{
            id => rsocket_connection_sup,
            start => {rsocket_connection_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [rsocket_connection_sup]
        },
    ChildSpecs = [RSocketConnectionSup],
    {ok, {SupFlags, ChildSpecs}}.
