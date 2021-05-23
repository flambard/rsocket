-module(rsocket_fragment_request_channel_tests).

-include_lib("eunit/include/eunit.hrl").

small_request_channel_no_metadata_test() ->
    MetadataLength = 0,
    DataLength = 2000,
    [{request_channel, false, MetadataLength, DataLength}] =
        rsocket_fragment:plan_request_channel(MetadataLength, DataLength).

small_request_channel_small_metadata_test() ->
    MetadataLength = 2500,
    DataLength = 1800,
    [{request_channel, false, MetadataLength, DataLength}] =
        rsocket_fragment:plan_request_channel(MetadataLength, DataLength).

large_request_channel_small_metadata_test() ->
    MetadataLength = 2500,
    DataLength = 20000000,
    [
     {request_channel, true, MetadataLength, DataLength1},
     {payload, false, 0, DataLength2}
    ] = rsocket_fragment:plan_request_channel(MetadataLength, DataLength),
    DataLength = DataLength1 + DataLength2.

small_request_channel_large_metadata_test() ->
    MetadataLength = 25000000,
    DataLength = 3400,
    [
     {request_channel, true, MetadataLength1, 0},
     {payload, false, MetadataLength2, DataLength}
    ] = rsocket_fragment:plan_request_channel(MetadataLength, DataLength),
    MetadataLength = MetadataLength1 + MetadataLength2.

large_request_channel_large_metadata_test() ->
    MetadataLength = 25000000,
    DataLength = 34000000,
    [
     {request_channel, true, MetadataLength1, 0},
     {payload, true, MetadataLength2, DataLength1},
     {payload, true, 0, DataLength2},
     {payload, false, 0, DataLength3}
    ] = rsocket_fragment:plan_request_channel(MetadataLength, DataLength),
    MetadataLength = MetadataLength1 + MetadataLength2,
    DataLength = DataLength1 + DataLength2 + DataLength3.
