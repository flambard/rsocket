-module(rsocket).

%% API
-export([
         call/2,
         cast/2,
         close_connection/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

call(Connection, Request) ->
    Self = self(),
    Ref = make_ref(),
    Handler = fun(Response) -> Self ! {response, Ref, Response} end,
    ok = rsocket_connection:send_request_response(Connection, Request, Handler),
    receive
        {response, Ref, Response} -> Response
    after 5000 ->
            {error, timeout}
    end.

cast(Connection, Message) ->
    rsocket_connection:send_request_fnf(Connection, Message).

close_connection(Connection) ->
    rsocket_connection:close(Connection).


%%%===================================================================
%%% Internal functions
%%%===================================================================
