-module(rsocket_rpc).

%% API
-export([call/2, call/3, cast/2, cast/3]).

%%%===================================================================
%%% API
%%%===================================================================

call(Connection, Request) ->
    call(Connection, Request, []).

call(Connection, Request, Options) ->
    Self = self(),
    Ref = make_ref(),
    Handler = fun(Response) -> Self ! {response, Ref, Response} end,
    Result = rsocket:request_response(Connection, Request, Handler, Options),
    case Result of
        {error, lease_expired} ->
            {error, lease_expired};
        {ok, _StreamID} ->
            receive
                {response, Ref, Response} ->
                    Response
            after 5000 ->
                {error, timeout}
            end
    end.

cast(Connection, Message) ->
    rsocket:request_fnf(Connection, Message, []).

cast(Connection, Message, Options) ->
    rsocket:request_fnf(Connection, Message, Options).

%%%===================================================================
%%% Internal functions
%%%===================================================================
