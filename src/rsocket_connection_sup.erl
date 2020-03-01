-module(rsocket_connection_sup).
-behaviour(supervisor).

%% API
-export([
         start_link/0,
         accept_connection/2,
         initiate_connection/2
        ]).

%% Supervisor callbacks
-export([
         init/1
        ]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

accept_connection(Module, Transport) ->
    supervisor:start_child(?SERVER, [accept, Module, Transport]).

initiate_connection(Module, Transport) ->
    supervisor:start_child(?SERVER, [initiate, Module, Transport]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(Args :: term()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}} |
          ignore.
init([]) ->
    SupFlags = #{
                 strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5
                },
    ChildSpec = #{
                  id => rsocket_connection,
                  start => {rsocket_connection, start_link, []},
                  restart => temporary,
                  shutdown => 3000,
                  type => worker,
                  modules => [rsocket_connection]
                 },
    {ok, {SupFlags, [ChildSpec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
