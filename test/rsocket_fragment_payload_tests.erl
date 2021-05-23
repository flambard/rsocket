-module(rsocket_fragment_payload_tests).

-include_lib("eunit/include/eunit.hrl").

small_payload_no_metadata_test() ->
    MetadataLength = 0,
    DataLength = 2000,
    [{payload, MetadataLength, DataLength}] =
        rsocket_fragment:plan_payload(MetadataLength, DataLength).

small_payload_small_metadata_test() ->
    MetadataLength = 2500,
    DataLength = 1800,
    [{payload, MetadataLength, DataLength}] =
        rsocket_fragment:plan_payload(MetadataLength, DataLength).

large_payload_small_metadata_test() ->
    MetadataLength = 2500,
    DataLength = 20000000,
    [
     {payload, MetadataLength, DataLength1},
     {payload, 0, DataLength2}
    ] = rsocket_fragment:plan_payload(MetadataLength, DataLength),
    DataLength = DataLength1 + DataLength2.

small_payload_large_metadata_test() ->
    MetadataLength = 25000000,
    DataLength = 3400,
    [
     {payload, MetadataLength1, 0},
     {payload, MetadataLength2, DataLength}
    ] = rsocket_fragment:plan_payload(MetadataLength, DataLength),
    MetadataLength = MetadataLength1 + MetadataLength2.

large_payload_large_metadata_test() ->
    MetadataLength = 25000000,
    DataLength = 34000000,
    [
     {payload, MetadataLength1, 0},
     {payload, MetadataLength2, DataLength1},
     {payload, 0, DataLength2},
     {payload, 0, DataLength3}
    ] = rsocket_fragment:plan_payload(MetadataLength, DataLength),
    MetadataLength = MetadataLength1 + MetadataLength2,
    DataLength = DataLength1 + DataLength2 + DataLength3.
