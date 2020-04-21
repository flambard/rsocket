-module(rsocket).

%% API
-export([
         cast/2,
         close_connection/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

cast(Connection, Message) ->
    rsocket_connection:send_fnf(Connection, Message).

close_connection(Connection) ->
    rsocket_connection:close(Connection).


%%%===================================================================
%%% Internal functions
%%%===================================================================
