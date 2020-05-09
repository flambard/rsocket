-module(rsocket).

%% API
-export([
         call/2,
         call/3,
         close_connection/1,
         lease/3,
         lease/4,
         metadata_push/2,
         request_fnf/2,
         request_fnf/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

call(Connection, Request) ->
    call(Connection, Request, []).

call(Connection, Request, Options) ->
    Self = self(),
    Ref = make_ref(),
    Handler = fun(Response) -> Self ! {response, Ref, Response} end,
    SendResult = rsocket_connection:send_request_response(
                   Connection, Request, Handler, Options),
    case SendResult of
        {error, lease_expired} -> {error, lease_expired};
        {ok, _StreamID} ->
            receive
                {response, Ref, Response} -> Response
            after 5000 ->
                    {error, timeout}
            end
    end.

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
