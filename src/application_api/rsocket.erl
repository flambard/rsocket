-module(rsocket).

%% API
-export([
         call/2,
         call/3,
         cast/2,
         cast/3,
         close_connection/1
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
    ok = rsocket_connection:send_request_response(
           Connection, Request, Handler, Options),
    receive
        {response, Ref, Response} -> Response
    after 5000 ->
            {error, timeout}
    end.

cast(Connection, Message) ->
    cast(Connection, Message, []).

cast(Connection, Message, Options) ->
    rsocket_connection:send_request_fnf(Connection, Message, Options).

close_connection(Connection) ->
    rsocket_connection:close(Connection).


%%%===================================================================
%%% Internal functions
%%%===================================================================
