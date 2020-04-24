-module(rsocket_frame).

-include("rsocket_format.hrl").

%% API
-export([
         parse/1,
         new_setup/0,
         new_request_fnf/2,
         new_request_response/2,
         new_payload/2,
         new_error/2,
         new_error/3
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
    ?RSOCKET_FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_FNF, 0, 0, 0, Fnf).

new_request_response(StreamID, Request) ->
    RR = ?RSOCKET_REQUEST_RESPONSE(Request),
    ?RSOCKET_FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_RESPONSE, 0, 0, 0, RR).

new_payload(StreamID, Payload) ->
    P = ?RSOCKET_PAYLOAD(Payload),
    %% TODO: Set up the frame header correctly
    %% Flags: M = 0, F = 0, C = 1, N = 1
    ?RSOCKET_FRAME_HEADER(StreamID, ?FRAME_TYPE_PAYLOAD, 0, 0, 0, P).

new_error(StreamID, ErrorType) ->
    new_error(StreamID, ErrorType, <<"">>).

new_error(StreamID, ErrorType, ErrorData) ->
    ErrorCode = maps:get(ErrorType, error_codes()),
    E = ?RSOCKET_ERROR(ErrorCode, ErrorData),
    ?RSOCKET_FRAME_HEADER(StreamID, ?FRAME_TYPE_ERROR, 0, 0, 0, E).

%%%===================================================================
%%% Internal functions
%%%===================================================================

error_codes() ->
    #{
      invalid_setup =>     16#001,
      unsupported_setup => 16#002,
      rejected_setup =>    16#003,
      rejected_resume =>   16#004,
      connection_error =>  16#101,
      connection_close =>  16#102,
      application_error => 16#201,
      rejected =>          16#202,
      canceled =>          16#203,
      invalid =>           16#204
     }.
