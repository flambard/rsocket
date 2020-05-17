-module(rsocket_frame).

-include("rsocket_format.hrl").

%% API
-export([
         error_code_names/0,
         parse/1,
         new_cancel/1,
         new_keepalive/1,
         new_metadata_push/1,
         new_setup/5,
         new_request_channel/4,
         new_request_fnf/3,
         new_request_response/3,
         new_request_stream/4,
         new_request_n/2,
         new_payload/3,
         new_lease/3,
         new_error/2,
         new_error/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

error_code_names() ->
    #{
      16#001 => invalid_setup,
      16#002 => unsupported_setup,
      16#003 => rejected_setup,
      16#004 => rejected_resume,
      16#101 => connection_error,
      16#102 => connection_close,
      16#201 => application_error,
      16#202 => rejected,
      16#203 => canceled,
      16#204 => invalid
     }.

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

new_cancel(StreamID) ->
    Flags = ?CANCEL_FLAGS,
    C = ?CANCEL,
    ?FRAME_HEADER(StreamID, ?FRAME_TYPE_CANCEL, Flags, C).

new_keepalive(Options) ->
    Respond = bool_to_bit(proplists:is_defined(respond, Options)),
    Flags = ?KEEPALIVE_FLAGS(Respond),
    K = ?KEEPALIVE,
    ?FRAME_HEADER(0, ?FRAME_TYPE_KEEPALIVE, Flags, K).

new_metadata_push(Metadata) ->
    Flags = ?METADATA_PUSH_FLAGS,
    M = ?METADATA_PUSH(Metadata),
    ?FRAME_HEADER(0, ?FRAME_TYPE_METADATA_PUSH, Flags, M).

new_setup(TimeBetweenKeepaliveFrames, MaxLifetime,
          MetadataMimeType, DataMimeType,
          Options) ->
    L = bool_to_bit(proplists:is_defined(leasing, Options)),
    Flags = ?SETUP_FLAGS(0, 0, L),
    Payload = <<>>,
    Setup = ?SETUP(0, 2, TimeBetweenKeepaliveFrames, MaxLifetime,
                   byte_size(MetadataMimeType), MetadataMimeType,
                   byte_size(DataMimeType), DataMimeType,
                   Payload),
    ?FRAME_HEADER(0, ?FRAME_TYPE_SETUP, Flags, Setup).

new_request_channel(StreamID, N, Request, Options) ->
    Follows = bool_to_bit(proplists:is_defined(follows, Options)),
    Complete = bool_to_bit(proplists:is_defined(complete, Options)),
    case proplists:lookup(metadata, Options) of
        none ->
            Flags = ?REQUEST_CHANNEL_FLAGS(0, Follows, Complete),
            RC = ?REQUEST_CHANNEL(N, Request),
            ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_CHANNEL, Flags, RC);
        {metadata, Metadata} ->
            Flags = ?REQUEST_CHANNEL_FLAGS(1, Follows, Complete),
            RC = ?REQUEST_CHANNEL(N, Request),
            Size = byte_size(Metadata),
            M = ?METADATA(Size, Metadata, RC),
            ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_CHANNEL, Flags, M)
    end.

new_request_fnf(StreamID, Message, Options) ->
    Follows = bool_to_bit(proplists:is_defined(follows, Options)),
    case proplists:lookup(metadata, Options) of
        none ->
            Flags = ?REQUEST_FNF_FLAGS(0, Follows),
            Fnf = ?REQUEST_FNF(Message),
            ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_FNF, Flags, Fnf);
        {metadata, Metadata} ->
            Flags = ?REQUEST_FNF_FLAGS(1, Follows),
            Fnf = ?REQUEST_FNF(Message),
            Size = byte_size(Metadata),
            M = ?METADATA(Size, Metadata, Fnf),
            ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_FNF, Flags, M)
    end.

new_request_response(StreamID, Request, Options) ->
    Follows = bool_to_bit(proplists:is_defined(follows, Options)),
    case proplists:lookup(metadata, Options) of
        none ->
            Flags = ?REQUEST_RESPONSE_FLAGS(0, Follows),
            RR = ?REQUEST_RESPONSE(Request),
            ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_RESPONSE, Flags, RR);
        {metadata, Metadata} ->
            Flags = ?REQUEST_RESPONSE_FLAGS(1, Follows),
            RR = ?REQUEST_RESPONSE(Request),
            Size = byte_size(Metadata),
            M = ?METADATA(Size, Metadata, RR),
            ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_RESPONSE, Flags, M)
    end.

new_request_stream(StreamID, N, Request, Options) ->
    Follows = bool_to_bit(proplists:is_defined(follows, Options)),
    case proplists:lookup(metadata, Options) of
        none ->
            Flags = ?REQUEST_STREAM_FLAGS(0, Follows),
            RS = ?REQUEST_STREAM(N, Request),
            ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_STREAM, Flags, RS);
        {metadata, Metadata} ->
            Flags = ?REQUEST_STREAM_FLAGS(1, Follows),
            RS = ?REQUEST_STREAM(N, Request),
            Size = byte_size(Metadata),
            M = ?METADATA(Size, Metadata, RS),
            ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_STREAM, Flags, M)
    end.

new_request_n(StreamID, N) ->
    Flags = ?REQUEST_N_FLAGS,
    R = ?REQUEST_N(N),
    ?FRAME_HEADER(StreamID, ?FRAME_TYPE_REQUEST_N, Flags, R).

new_payload(StreamID, Payload, Options) ->
    Follows = bool_to_bit(proplists:is_defined(follows, Options)),
    Complete = bool_to_bit(proplists:is_defined(complete, Options)),
    Next = bool_to_bit(proplists:is_defined(next, Options)),
    case proplists:lookup(metadata, Options) of
        none ->
            Flags = ?PAYLOAD_FLAGS(0, Follows, Complete, Next),
            P = ?PAYLOAD(Payload),
            ?FRAME_HEADER(StreamID, ?FRAME_TYPE_PAYLOAD, Flags, P);
        {metadata, Metadata} ->
            Flags = ?PAYLOAD_FLAGS(1, Follows, Complete, Next),
            P = ?PAYLOAD(Payload),
            Size = byte_size(Metadata),
            M = ?METADATA(Size, Metadata, P),
            ?FRAME_HEADER(StreamID, ?FRAME_TYPE_PAYLOAD, Flags, M)
    end.

new_lease(TimeToLive, NumberOfRequests, Options) ->
    case proplists:lookup(metadata, Options) of
        none ->
            Flags = ?LEASE_FLAGS(0),
            L = ?LEASE(TimeToLive, NumberOfRequests, <<>>),
            ?FRAME_HEADER(0, ?FRAME_TYPE_LEASE, Flags, L);
        {metadata, Metadata} ->
            Flags = ?LEASE_FLAGS(1),
            L = ?LEASE(TimeToLive, NumberOfRequests, Metadata),
            ?FRAME_HEADER(0, ?FRAME_TYPE_LEASE, Flags, L)
    end.

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
