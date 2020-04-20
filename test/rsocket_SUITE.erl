-module(rsocket_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
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
    Config = #{ at_connect => fun() -> Self ! {connected, Ref} end },
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, Connection} = rsocket_loopback:connect(Listener),
    receive
        {connected, Ref} ->
            ok = rsocket:close_connection(Connection)
    after 10000 ->
            exit(connection_failed)
    end.
