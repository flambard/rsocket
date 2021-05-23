-module(rsocket_fragment).

-export([
         plan_request_response/2,
         plan_payload/2
        ]).

-define(PAYLOAD_SPACE, 16777209).
-define(REQUEST_RESPONSE_SPACE, 16777209).

%%%
%%% REQUEST_RESPONSE
%%%

plan_request_response(MetadataLength, DataLength)
  when MetadataLength > ?REQUEST_RESPONSE_SPACE ->
    Remaining = MetadataLength - ?REQUEST_RESPONSE_SPACE,
    Following = plan_payload(Remaining, DataLength),
    [{request_response, ?REQUEST_RESPONSE_SPACE, 0} | Following];

plan_request_response(MetadataLength, DataLength)
  when MetadataLength + DataLength > ?REQUEST_RESPONSE_SPACE ->
    DataFragmentLength = ?REQUEST_RESPONSE_SPACE - MetadataLength,
    Remaining = DataLength - DataFragmentLength,
    Following = plan_payload(0, Remaining),
    [{request_response, MetadataLength, DataFragmentLength} | Following];

plan_request_response(MetadataLength, DataLength) ->
    [{request_response, MetadataLength, DataLength}].


%%%
%%% PAYLOAD
%%%

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
