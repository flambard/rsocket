-module(rsocket_lease_tests).

-include_lib("eunit/include/eunit.hrl").

spending_without_initate_test() ->
    Lease = rsocket_lease:new(),
    0 = rsocket_lease:spend_1(Lease).

spending_test() ->
    Lease = rsocket_lease:new(),
    ok = rsocket_lease:initiate(Lease, 10000, 5),
    5 = rsocket_lease:spend_1(Lease),
    4 = rsocket_lease:spend_1(Lease),
    3 = rsocket_lease:spend_1(Lease),
    2 = rsocket_lease:spend_1(Lease),
    1 = rsocket_lease:spend_1(Lease),
    0 = rsocket_lease:spend_1(Lease),
    0 = rsocket_lease:spend_1(Lease).

expire_test() ->
    Lease = rsocket_lease:new(),
    ok = rsocket_lease:initiate(Lease, 100, 5),
    5 = rsocket_lease:spend_1(Lease),
    4 = rsocket_lease:spend_1(Lease),
    receive after 200 -> ok end,
    0 = rsocket_lease:spend_1(Lease),
    0 = rsocket_lease:spend_1(Lease).

reinitiate_overrides_lease_test() ->
    Lease = rsocket_lease:new(),
    ok = rsocket_lease:initiate(Lease, 100, 5),
    5 = rsocket_lease:spend_1(Lease),
    4 = rsocket_lease:spend_1(Lease),
    ok = rsocket_lease:initiate(Lease, 100, 10),
    10 = rsocket_lease:spend_1(Lease),
    9 = rsocket_lease:spend_1(Lease),
    8 = rsocket_lease:spend_1(Lease),
    receive after 200 -> ok end,
    0 = rsocket_lease:spend_1(Lease),
    0 = rsocket_lease:spend_1(Lease).
