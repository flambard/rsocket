-module(rsocket_stream_sup).
-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_stream/1
        ]).

%% Supervisor callbacks
-export([
         init/1
        ]).


-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_stream(StreamID) ->
    supervisor:start_child(?SERVER, [StreamID]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
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
                  id => rsocket_stream,
                  start => {rsocket_stream, start_link, []},
                  restart => temporary,
                  shutdown => 3000,
                  type => worker,
                  modules => [rsocket_stream]
                 },
    {ok, {SupFlags, [ChildSpec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
