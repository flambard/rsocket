-module(rsocket_stream_requester).

%% API
-export([
         cancel/1,
         request_n/2
        ]).

-callback init(Args :: term()) -> {ok, State :: term()}.

-callback handle_payload(Payload :: binary(),
                         Options :: list(),
                         State :: term()) ->
    term().

%%%===================================================================
%%% API
%%%===================================================================

cancel(Stream) ->
    rsocket_stream:send_cancel(Stream).

request_n(Stream, N) ->
    rsocket_stream:send_request_n(Stream, N).


%%%===================================================================
%%% Internal functions
%%%===================================================================
