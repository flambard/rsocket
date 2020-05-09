-module(rsocket_rpc).

%% API
-export([
         cast/2,
         cast/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

cast(Connection, Message) ->
    rsocket:request_fnf(Connection, Message, []).

cast(Connection, Message, Options) ->
    rsocket:request_fnf(Connection, Message, Options).


%%%===================================================================
%%% Internal functions
%%%===================================================================
