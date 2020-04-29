-define(HEADER(StreamID, Frame),
       <<
         0          :1,
         (StreamID) :31,
         (Frame)    /binary
       >>
       ).

-define(SETUP(MetadataPresent, ResumeEnable, Lease, MajorVersion, MinorVersion,
              TimeBetweenKeepaliveFrames, MaxLifetime, MetadataAndPayload),
        <<
          16#01                        :6,
          %% Flags
          0                            :1,
          (MetadataPresent)            :1,
          (ResumeEnable)               :1,
          (Lease)                      :1,
          0                            :6,
          %%
          (MajorVersion)               :16,
          (MinorVersion)               :16,
          0                            :1,
          (TimeBetweenKeepaliveFrames) :31,
          0                            :1,
          (MaxLifetime)                :31,
          (MetadataAndPayload)         /binary
        >>
       ).

-define(SETUP(MetadataPresent, ResumeEnable, Lease, MajorVersion, MinorVersion,
              TimeBetweenKeepaliveFrames, MaxLifetime, TokenLength,
              ResumeIdentificationToken),
        <<
          16#01                        :6,
          %% Flags
          0                            :1,
          (MetadataPresent)            :1,
          (ResumeEnable)               :1,
          (Lease)                      :1,
          0                            :6,
          %%
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

-define(KEEPALIVE, ?KEEPALIVE(0, <<>>)).

-define(KEEPALIVE(Respond, LastReceivedPosition, Data),
        <<
          16#03                  :6,
          %% Flags
          0                      :2,
          (Respond)              :1,
          0                      :7,
          0                      :1,
          %%
          (LastReceivedPosition) :63,
          (Data)                 /binary
        >>
       ).

-define(REQUEST_FNF(MetadataPresent, Follows, Message),
        <<
          16#05             :6,
          %% Flags
          0                 :1,
          (MetadataPresent) :1,
          (Follows)         :1,
          0                 :7,
          %%
          (Message)         /binary
        >>
       ).

-define(REQUEST_RESPONSE(MetadataPresent, Follows, Request),
        <<
          16#04             :6,
          %% Flags
          0                 :1,
          (MetadataPresent) :1,
          (Follows)         :1,
          0                 :7,
          %%
          (Request)         /binary
        >>
       ).

-define(PAYLOAD(Payload),
        <<
          16#0A             :6,
          %% Flags
          0                 :1,
          (MetadataPresent) :1,
          (Follows)         :1,
          (Complete)        :1,
          (Next)            :1,
          0                 :5,
          %%
          (Payload)         /binary
        >>
       ).

-define(ERROR(ErrorCode, ErrorData),
        <<
          16#0B       :6,
          %% Flags
          0           :10,
          %%
          (ErrorCode) :32,
          (ErrorData) /binary
        >>
       ).


%%%
%%% OLD
%%%

-define(RSOCKET_FRAME_HEADER(StreamID, FrameType, Flags, FramePayload),
        <<
          0              :1,
          (StreamID)     :31,
          (FrameType)    :6,
          (Flags)        :10,
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

-define(FLAG_IGNORE,            2#1000000000).
-define(FLAG_METADATA,          2#0100000000).
-define(FLAG_SETUP_RESUME,      2#0010000000).
-define(FLAG_SETUP_LEASE,       2#0001000000).
-define(FLAG_KEEPALIVE_RESPOND, 2#0010000000).
-define(FLAG_FRAGMENT_FOLLOWS,  2#0010000000).
-define(FLAG_COMPLETE,          2#0001000000).
-define(FLAG_PAYLOAD_NEXT,      2#0000100000).


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

-define(RSOCKET_KEEPALIVE,
        <<
          0    :1,
          0    :63,
          <<>> /binary
        >>
       ).

-define(RSOCKET_KEEPALIVE(LastReceivedPosition, Data),
        <<
          0                      :1,
          (LastReceivedPosition) :63,
          (Data)                 /binary
        >>
       ).

-define(RSOCKET_REQUEST_FNF(Message),
        <<
          (Message) /binary
        >>
       ).

-define(RSOCKET_REQUEST_RESPONSE(Request),
        <<
          (Request) /binary
        >>
       ).

-define(RSOCKET_PAYLOAD(Payload),
        <<
          (Payload) /binary
        >>
       ).

-define(RSOCKET_ERROR(ErrorCode, ErrorData),
        <<
          (ErrorCode) :32,
          (ErrorData) /binary
        >>
       ).
