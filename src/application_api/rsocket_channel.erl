-module(rsocket_channel).

%% API
-export([cancel/1, error/3, payload/3, request_n/2]).

-callback init(Request :: map(), Args :: term()) -> {ok, State :: term()}.
-callback handle_payload(Payload :: binary(), Options :: list(), State :: term()) ->
                            term().
-callback handle_request_n(N :: pos_integer(), State :: term()) -> term().

%%%===================================================================
%%% API
%%%===================================================================

cancel(Stream) ->
    rsocket_stream:send_cancel(Stream).

error(Stream, ErrorType, ErrorData) ->
    rsocket_stream:send_error(Stream, ErrorType, ErrorData).

payload(Stream, Payload, Options) ->
    rsocket_stream:send_payload(Stream, Payload, Options).

request_n(Stream, N) ->
    rsocket_stream:send_request_n(Stream, N).

%%%===================================================================
%%% Internal functions
%%%===================================================================
