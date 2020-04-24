-module(rsocket_frame).

-include("rsocket_format.hrl").

%% API
-export([
         parse/1,
         new_setup/0,
         new_request_fnf/2,
         new_request_response/2,
         new_payload/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

parse(Frame) ->
    ?RSOCKET_FRAME_HEADER(
       StreamID, FrameType, IgnoreFlag, MetadataFlag, OtherFlags, FramePayload
      ) = Frame,
    case FrameType of
        ?FRAME_TYPE_RESERVED ->
            {error, not_implemented};
        ?FRAME_TYPE_SETUP ->
            {ok, {setup, StreamID}};
        ?FRAME_TYPE_LEASE ->
            {error, not_implemented};
        ?FRAME_TYPE_KEEPALIVE ->
            {error, not_implemented};
        ?FRAME_TYPE_REQUEST_RESPONSE ->
            {ok, {request_response, StreamID, FramePayload}};
        ?FRAME_TYPE_REQUEST_FNF ->
            {ok, {request_fnf, StreamID, FramePayload}};
        ?FRAME_TYPE_REQUEST_STREAM ->
            {error, not_implemented};
        ?FRAME_TYPE_REQUEST_CHANNEL ->
            {error, not_implemented};
        ?FRAME_TYPE_REQUEST_N ->
            {error, not_implemented};
        ?FRAME_TYPE_CANCEL ->
            {error, not_implemented};
        ?FRAME_TYPE_PAYLOAD ->
            {ok, {payload, StreamID, FramePayload}};
        ?FRAME_TYPE_ERROR ->
            {error, not_implemented};
        ?FRAME_TYPE_METADATA_PUSH ->
            {error, not_implemented};
        ?FRAME_TYPE_RESUME ->
            {error, not_implemented};
        ?FRAME_TYPE_EXT ->
            {error, not_implemented}
    end.

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
