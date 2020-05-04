-module(rsocket_connection_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:ensure_started(gproc),
    application:ensure_started(rsocket),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [
     test_open_close_connection
    ].


test_open_close_connection(_Config) ->
    Self = self(),
    Ref = make_ref(),
    AtConnectFun = fun(RSocket) -> Self ! {connected, Ref, RSocket} end,
    Config = #{ at_connect => AtConnectFun,
                handlers => #{}
              },
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, ClientRSocket} = rsocket_loopback:connect(Listener),
    receive
        {connected, Ref, _ServerRSocket} ->
            ok = rsocket:close_connection(ClientRSocket)
    after 10000 ->
            exit(connection_failed)
    end.
