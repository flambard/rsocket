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
     test_open_close_connection,
     test_client_fnf
    ].


test_open_close_connection(_Config) ->
    Self = self(),
    Ref = make_ref(),
    Config = #{ at_connect => fun() -> Self ! {connected, Ref} end,
                handlers => #{}
              },
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    receive
        {connected, Ref} ->
            ok = rsocket:close_connection(RSocket)
    after 10000 ->
            exit(connection_failed)
    end.


test_client_fnf(_Config) ->
    Self = self(),
    Ref = make_ref(),
    FnfHandler = fun(Message) -> Self ! {fnf, Ref, Message} end,
    Config = #{ handlers => #{ fire_and_forget => FnfHandler }},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    Message = <<"The Message">>,
    ok = rsocket:cast(RSocket, Message),
    receive
        {fnf, Ref, Message} ->
            ok = rsocket:close_connection(RSocket)
    after 1000 ->
            exit(did_not_handle_fnf_request)
    end.
