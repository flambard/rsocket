-module(rsocket_fragment).

-export([
         plan_payload/2
        ]).

-define(PAYLOAD_SPACE, 16777209).

plan_payload(MetadataLength, DataLength) when MetadataLength > ?PAYLOAD_SPACE ->
    Remaining = MetadataLength - ?PAYLOAD_SPACE,
    Following = plan_payload(Remaining, DataLength),
    [{payload, ?PAYLOAD_SPACE, 0} | Following];

plan_payload(MetadataLength, DataLength)
  when MetadataLength + DataLength > ?PAYLOAD_SPACE ->
    DataFragmentLength = ?PAYLOAD_SPACE - MetadataLength,
    Remaining = DataLength - DataFragmentLength,
    Following = plan_payload(0, Remaining),
    [{payload, MetadataLength, DataFragmentLength} | Following];

plan_payload(MetadataLength, DataLength) ->
    [{payload, MetadataLength, DataLength}].
