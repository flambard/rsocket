-module(rsocket_frame).

-include("rsocket_format.hrl").

%% API
-export([
         parse/1,
         new_keepalive/1,
         new_setup/2,
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
    ?FRAME_HEADER(StreamID, FrameType, Flags, FramePayload) = Frame,
    case FrameType of
        ?FRAME_TYPE_RESERVED ->
            {error, not_implemented};
        ?FRAME_TYPE_SETUP ->
            ?SETUP(0, 2, KeepaliveInterval, MaxLifetime, _) = FramePayload,
            {ok, {setup, StreamID, KeepaliveInterval, MaxLifetime}};
        ?FRAME_TYPE_LEASE ->
            {error, not_implemented};
        ?FRAME_TYPE_KEEPALIVE ->
            {ok, {keepalive, Flags band ?FLAG_KEEPALIVE_RESPOND =/= 0}};
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

new_keepalive(Options) ->
    Flags = case proplists:is_defined(respond, Options) of
                false -> 0;
                true  -> ?FLAG_KEEPALIVE_RESPOND
            end,
    K = ?KEEPALIVE,
    ?FRAME_HEADER(0, ?FRAME_TYPE_KEEPALIVE, Flags, K).

new_setup(TimeBetweenKeepaliveFrames, MaxLifetime) ->
    Flags = 0,
    Setup = ?SETUP(0, 2, TimeBetweenKeepaliveFrames, MaxLifetime, <<>>),
    ?FRAME_HEADER(0, ?FRAME_TYPE_SETUP, Flags, Setup).

new_request_fnf(StreamID, Message) ->
    Flags = 0,
    Fnf = ?REQUEST_FNF(Message),
    ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_FNF, Flags, Fnf).

new_request_response(StreamID, Request) ->
    Flags = 0,
    RR = ?REQUEST_RESPONSE(Request),
    ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_RESPONSE, Flags, RR).

new_payload(StreamID, Payload) ->
    %% TODO: Set up the frame header correctly
    %% Flags: M = 0, F = 0, C = 1, N = 1
    Flags = 0,
    P = ?PAYLOAD(Payload),
    ?FRAME_HEADER(StreamID, ?FRAME_TYPE_PAYLOAD, Flags, P).

new_error(StreamID, ErrorType) ->
    new_error(StreamID, ErrorType, <<"">>).

new_error(StreamID, ErrorType, ErrorData) ->
    Flags = 0,
    ErrorCode = maps:get(ErrorType, error_codes()),
    E = ?ERROR(ErrorCode, ErrorData),
    ?FRAME_HEADER(StreamID, ?FRAME_TYPE_ERROR, Flags, E).

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
