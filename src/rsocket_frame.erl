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
    {maps:get(FrameType, frame_types()), StreamID, Flags, FramePayload}.

frame_types() ->
    #{
      ?FRAME_TYPE_RESERVED         => reserved,
      ?FRAME_TYPE_SETUP            => setup,
      ?FRAME_TYPE_LEASE            => lease,
      ?FRAME_TYPE_KEEPALIVE        => keepalive,
      ?FRAME_TYPE_REQUEST_RESPONSE => request_response,
      ?FRAME_TYPE_REQUEST_FNF      => request_fnf,
      ?FRAME_TYPE_REQUEST_STREAM   => request_stream,
      ?FRAME_TYPE_REQUEST_CHANNEL  => request_channel,
      ?FRAME_TYPE_REQUEST_N        => request_n,
      ?FRAME_TYPE_CANCEL           => cancel,
      ?FRAME_TYPE_PAYLOAD          => payload,
      ?FRAME_TYPE_ERROR            => error,
      ?FRAME_TYPE_METADATA_PUSH    => metadata_push,
      ?FRAME_TYPE_RESUME           => resume,
      ?FRAME_TYPE_RESUME_OK        => resume_ok,
      ?FRAME_TYPE_EXT              => ext
     }.

new_keepalive(Options) ->
    Respond = bool_to_bit(proplists:is_defined(respond, Options)),
    Flags = ?KEEPALIVE_FLAGS(Respond),
    K = ?KEEPALIVE,
    ?FRAME_HEADER(0, ?FRAME_TYPE_KEEPALIVE, Flags, K).

new_setup(TimeBetweenKeepaliveFrames, MaxLifetime) ->
    Flags = ?SETUP_FLAGS(0, 0, 0),
    Setup = ?SETUP(0, 2, TimeBetweenKeepaliveFrames, MaxLifetime, <<>>),
    ?FRAME_HEADER(0, ?FRAME_TYPE_SETUP, Flags, Setup).

new_request_fnf(StreamID, Message) ->
    MetadataPresent = 0,
    Follows = 0,
    Flags = ?REQUEST_FNF_FLAGS(MetadataPresent, Follows),
    Fnf = ?REQUEST_FNF(Message),
    ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_FNF, Flags, Fnf).

new_request_response(StreamID, Request) ->
    MetadataPresent = 0,
    Follows = 0,
    Flags = ?REQUEST_RESPONSE_FLAGS(MetadataPresent, Follows),
    RR = ?REQUEST_RESPONSE(Request),
    ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_RESPONSE, Flags, RR).

new_payload(StreamID, Payload) ->
    MetadataPresent = 0,
    Follows = 0,
    Complete = 1,
    Next = 1,
    Flags = ?PAYLOAD_FLAGS(MetadataPresent, Follows, Complete, Next),
    P = ?PAYLOAD(Payload),
    ?FRAME_HEADER(StreamID, ?FRAME_TYPE_PAYLOAD, Flags, P).

new_error(StreamID, ErrorType) ->
    new_error(StreamID, ErrorType, <<"">>).

new_error(StreamID, ErrorType, ErrorData) ->
    ErrorCode = maps:get(ErrorType, error_codes()),
    Flags = ?ERROR(ErrorCode, ErrorData),
    E = ?ERROR(ErrorCode, ErrorData),
    ?FRAME_HEADER(StreamID, ?FRAME_TYPE_ERROR, Flags, E).

%%%===================================================================
%%% Internal functions
%%%===================================================================

error_codes() ->
    #{
      invalid_setup     => 16#001,
      unsupported_setup => 16#002,
      rejected_setup    => 16#003,
      rejected_resume   => 16#004,
      connection_error  => 16#101,
      connection_close  => 16#102,
      application_error => 16#201,
      rejected          => 16#202,
      canceled          => 16#203,
      invalid           => 16#204
     }.

bool_to_bit(false) -> 0;
bool_to_bit(true)  -> 1.

bit_to_bool(0) -> false;
bit_to_bool(1) -> true.
