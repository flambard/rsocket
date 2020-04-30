-define(FRAME_HEADER(StreamID, FrameType, Flags, FramePayload),
        <<
          0              :1,
          (StreamID)     :31,
          (FrameType)    :6,
          (Flags)        :10/bitstring,
          (FramePayload) /binary
        >>
       ).

-define(METADATA(MetadataLength, MetadataPayload),
        <<
          (MetadataLength)  :24,
          (MetadataPayload) /binary
        >>
       ).

-define(METADATA(MetadataPayload),
        <<
          (MetadataPayload) /binary
        >>
       ).

-define(FRAME_TYPE_RESERVED,         16#00).
-define(FRAME_TYPE_SETUP,            16#01).
-define(FRAME_TYPE_LEASE,            16#02).
-define(FRAME_TYPE_KEEPALIVE,        16#03).
-define(FRAME_TYPE_REQUEST_RESPONSE, 16#04).
-define(FRAME_TYPE_REQUEST_FNF,      16#05).
-define(FRAME_TYPE_REQUEST_STREAM,   16#06).
-define(FRAME_TYPE_REQUEST_CHANNEL,  16#07).
-define(FRAME_TYPE_REQUEST_N,        16#08).
-define(FRAME_TYPE_CANCEL,           16#09).
-define(FRAME_TYPE_PAYLOAD,          16#0A).
-define(FRAME_TYPE_ERROR,            16#0B).
-define(FRAME_TYPE_METADATA_PUSH,    16#0C).
-define(FRAME_TYPE_RESUME,           16#0D).
-define(FRAME_TYPE_RESUME_OK,        16#0E).
-define(FRAME_TYPE_EXT,              16#3F).


%%%
%%% SETUP
%%%

-define(SETUP_FLAGS(MetadataPresent, ResumeEnable, Lease),
        <<
          0                 :1,
          (MetadataPresent) :1,
          (ResumeEnable)    :1,
          (Lease)           :1,
          0                 :6
        >>
       ).

-define(SETUP(MajorVersion, MinorVersion, TimeBetweenKeepaliveFrames,
              MaxLifetime, MetadataAndPayload),
        <<
          (MajorVersion)               :16,
          (MinorVersion)               :16,
          0                            :1,
          (TimeBetweenKeepaliveFrames) :31,
          0                            :1,
          (MaxLifetime)                :31,
          (MetadataAndPayload)         /binary
        >>
       ).

-define(SETUP(MajorVersion, MinorVersion, TimeBetweenKeepaliveFrames,
              MaxLifetime, TokenLength, ResumeIdentificationToken),
        <<
          (MajorVersion)               :16,
          (MinorVersion)               :16,
          0                            :1,
          (TimeBetweenKeepaliveFrames) :31,
          0                            :1,
          (MaxLifetime)                :31,
          (TokenLength)                :16,
          (ResumeIdentificationToken)  /binary
        >>
       ).

%%%
%%% KEEPALIVE
%%%

-define(KEEPALIVE_FLAGS(Respond),
        <<
          0         :2,
          (Respond) :1,
          0         :7
        >>
       ).

-define(KEEPALIVE,
        <<
          0    :1,
          0    :63,
          <<>> /binary
        >>
       ).

-define(KEEPALIVE(LastReceivedPosition, Data),
        <<
          0                      :1,
          (LastReceivedPosition) :63,
          (Data)                 /binary
        >>
       ).


%%%
%%% REQUEST_FNF
%%%

-define(REQUEST_FNF_FLAGS(MetadataPresent, Follows),
        <<
          0                 :1,
          (MetadataPresent) :1,
          (Follows)         :1,
          0                 :7
        >>
       ).

-define(REQUEST_FNF(Message),
        <<
          (Message) /binary
        >>
       ).


%%%
%%% REQUEST_REQUEST
%%%

-define(REQUEST_RESPONSE_FLAGS(MetadataPresent, Follows),
        <<
          0                 :1,
          (MetadataPresent) :1,
          (Follows)         :1,
          0                 :7
        >>
       ).

-define(REQUEST_RESPONSE(Request),
        <<
          (Request) /binary
        >>
       ).


%%%
%%% PAYLOAD
%%%

-define(PAYLOAD_FLAGS(MetadataPresent, Follows, Complete, Next),
        <<
          0                 :1,
          (MetadataPresent) :1,
          (Follows)         :1,
          (Complete)        :1,
          (Next)            :1,
          0                 :5
        >>
       ).

-define(PAYLOAD(Payload),
        <<
          (Payload) /binary
        >>
       ).


%%%
%%% ERROR
%%%

-define(ERROR_FLAGS, <<0:10>>).

-define(ERROR(ErrorCode, ErrorData),
        <<
          (ErrorCode) :32,
          (ErrorData) /binary
        >>
       ).
