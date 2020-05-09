-module(rsocket).

%% API
-export([
         cancel/2,
         close_connection/1,
         lease/3,
         lease/4,
         metadata_push/2,
         request_fnf/2,
         request_fnf/3,
         request_response/3,
         request_response/4
        ]).

%%%===================================================================
%%% API
%%%===================================================================

cancel(Connection, StreamID) ->
    rsocket_connection:send_cancel(Connection, StreamID).

close_connection(Connection) ->
    rsocket_connection:close(Connection).

lease(Connection, TimeToLive, NumberOfRequests) ->
    lease(Connection, TimeToLive, NumberOfRequests, []).

lease(Connection, TimeToLive, NumberOfRequests, Options) ->
    rsocket_connection:send_lease(
      Connection, TimeToLive, NumberOfRequests, Options).

metadata_push(Connection, Metadata) ->
    rsocket_connection:send_metadata_push(Connection, Metadata).

request_fnf(Connection, Message) ->
    request_fnf(Connection, Message, []).

request_fnf(Connection, Message, Options) ->
    rsocket_connection:send_request_fnf(Connection, Message, Options).

request_response(Connection, Request, Handler) ->
    request_response(Connection, Request, Handler, []).

request_response(Connection, Request, Handler, Options) ->
    rsocket_connection:send_request_response(
      Connection, Request, Handler, Options).


%%%===================================================================
%%% Internal functions
%%%===================================================================
