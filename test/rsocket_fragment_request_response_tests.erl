-module(rsocket_fragment_request_response_tests).

-include_lib("eunit/include/eunit.hrl").

small_request_response_no_metadata_test() ->
    MetadataLength = 0,
    DataLength = 2000,
    [{request_response, MetadataLength, DataLength}] =
        rsocket_fragment:plan_request_response(MetadataLength, DataLength).

small_request_response_small_metadata_test() ->
    MetadataLength = 2500,
    DataLength = 1800,
    [{request_response, MetadataLength, DataLength}] =
        rsocket_fragment:plan_request_response(MetadataLength, DataLength).

large_request_response_small_metadata_test() ->
    MetadataLength = 2500,
    DataLength = 20000000,
    [
     {request_response, MetadataLength, DataLength1},
     {payload, 0, DataLength2}
    ] = rsocket_fragment:plan_request_response(MetadataLength, DataLength),
    DataLength = DataLength1 + DataLength2.

small_request_response_large_metadata_test() ->
    MetadataLength = 25000000,
    DataLength = 3400,
    [
     {request_response, MetadataLength1, 0},
     {payload, MetadataLength2, DataLength}
    ] = rsocket_fragment:plan_request_response(MetadataLength, DataLength),
    MetadataLength = MetadataLength1 + MetadataLength2.

large_request_response_large_metadata_test() ->
    MetadataLength = 25000000,
    DataLength = 34000000,
    [
     {request_response, MetadataLength1, 0},
     {payload, MetadataLength2, DataLength1},
     {payload, 0, DataLength2},
     {payload, 0, DataLength3}
    ] = rsocket_fragment:plan_request_response(MetadataLength, DataLength),
    MetadataLength = MetadataLength1 + MetadataLength2,
    DataLength = DataLength1 + DataLength2 + DataLength3.