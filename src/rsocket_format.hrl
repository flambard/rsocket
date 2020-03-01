-define(RSOCKET_FRAME_HEADER(StreamID, FrameType, IgnoreFlag, MetadataFlag,
                             OtherFlags, FramePayload),
        <<
          0              :1,
          (StreamID)     :31,
          (FrameType)    :6,
          (IgnoreFlag)   :1,
          (MetadataFlag) :1,
          (OtherFlags)   :8,
          (FramePayload) /binary
        >>
       ).

-define(RSOCKET_METADATA(MetadataLength, MetadataPayload),
        <<
          (MetadataLength)  :24,
          (MetadataPayload) /binary
        >>
       ).

-define(RSOCKET_METADATA(MetadataPayload),
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


-define(RSOCKET_SETUP(MajorVersion, MinorVersion, TimeBetweenKeepaliveFrames,
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

-define(RSOCKET_SETUP(MajorVersion, MinorVersion, TimeBetweenKeepaliveFrames,
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
