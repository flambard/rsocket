-module(rsocket_fragment).

-export([
    plan_request_response/2,
    plan_request_fnf/2,
    plan_request_stream/2,
    plan_request_channel/2,
    plan_payload/2
]).

-define(PAYLOAD_SPACE, 16777209).
-define(REQUEST_RESPONSE_SPACE, 16777209).
-define(REQUEST_FNF_SPACE, 16777209).
-define(REQUEST_STREAM_SPACE, 16777205).
-define(REQUEST_CHANNEL_SPACE, 16777205).

plan_request_response(MetadataLength, DataLength) ->
    plan(request_response, ?REQUEST_RESPONSE_SPACE, MetadataLength, DataLength).

plan_request_fnf(MetadataLength, DataLength) ->
    plan(request_fnf, ?REQUEST_FNF_SPACE, MetadataLength, DataLength).

plan_request_stream(MetadataLength, DataLength) ->
    plan(request_stream, ?REQUEST_STREAM_SPACE, MetadataLength, DataLength).

plan_request_channel(MetadataLength, DataLength) ->
    plan(request_channel, ?REQUEST_CHANNEL_SPACE, MetadataLength, DataLength).

plan_payload(MetadataLength, DataLength) ->
    plan(payload, ?PAYLOAD_SPACE, MetadataLength, DataLength).

plan(Request, Space, MetadataLength, DataLength) when MetadataLength > Space ->
    Remaining = MetadataLength - Space,
    Following = plan(payload, ?PAYLOAD_SPACE, Remaining, DataLength),
    [{Request, true, Space, 0} | Following];
plan(Request, Space, MetadataLength, DataLength) when
    MetadataLength + DataLength > Space
->
    DataFragmentLength = Space - MetadataLength,
    Remaining = DataLength - DataFragmentLength,
    Following = plan(payload, ?PAYLOAD_SPACE, 0, Remaining),
    [{Request, true, MetadataLength, DataFragmentLength} | Following];
plan(Request, _Space, MetadataLength, DataLength) ->
    [{Request, false, MetadataLength, DataLength}].
