-module(rsocket_stream_responder).

%% API
-export([
         error/3,
         payload/3
        ]).

-callback init(Args :: term()) -> {ok, State :: term()}.

-callback handle_request_n(N :: pos_integer(), State :: term()) -> term().

%%%===================================================================
%%% API
%%%===================================================================

error(Stream, ErrorType, ErrorData) ->
    rsocket_stream:send_error(Stream, ErrorType, ErrorData).

payload(Stream, Payload, Options) ->
    rsocket_stream:send_payload(Stream, Payload, Options).


%%%===================================================================
%%% Internal functions
%%%===================================================================
