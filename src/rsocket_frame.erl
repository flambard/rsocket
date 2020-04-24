-module(rsocket_frame).

-include("rsocket_format.hrl").

%% API
-export([
         new_setup/0,
         new_request_fnf/2,
         new_request_response/2,
         new_payload/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

new_setup() ->
    Setup = ?RSOCKET_SETUP(0, 2, 30000, 40000, <<>>),
    ?RSOCKET_FRAME_HEADER(0, ?FRAME_TYPE_SETUP, 0, 0, 0, Setup).

new_request_fnf(StreamID, Message) ->
    Fnf = ?RSOCKET_REQUEST_FNF(Message),
    %% TODO: Set up the frame header correctly
    ?RSOCKET_FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_FNF, 0, 0, 0, Fnf).

new_request_response(StreamID, Request) ->
    RR = ?RSOCKET_REQUEST_RESPONSE(Request),
    %% TODO: Set up the frame header correctly
    ?RSOCKET_FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_RESPONSE, 0, 0, 0, RR).

new_payload(StreamID, Payload) ->
    P = ?RSOCKET_PAYLOAD(Payload),
    %% TODO: Set up the frame header correctly
    ?RSOCKET_FRAME_HEADER(StreamID, ?FRAME_TYPE_PAYLOAD, 0, 0, 0, P).


%%%===================================================================
%%% Internal functions
%%%===================================================================
