-module(rsocket_request_fnf_SUITE).

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
     test_client_fnf,
     test_client_fnf_with_metadata,
     test_server_fnf
    ].


test_client_fnf(_Config) ->
    Self = self(),
    Ref = make_ref(),
    FnfHandler = fun(#{request := Message}) ->
                         Self ! {fnf, Ref, Message}
                 end,
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

test_client_fnf_with_metadata(_Config) ->
    Self = self(),
    Ref = make_ref(),
    FnfHandler = fun(#{request := Message, metadata := Metadata}) ->
                         Self ! {fnf, Ref, Message, Metadata}
                 end,
    Config = #{ handlers => #{ fire_and_forget => FnfHandler }},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    Message = <<"The Message">>,
    Metadata = <<"About The Message">>,
    ok = rsocket:cast(RSocket, Message, [{metadata, Metadata}]),
    receive
        {fnf, Ref, Message, Metadata} ->
            ok
    after 1000 ->
            exit(did_not_handle_fnf_request)
    end,
    ok = rsocket:close_connection(RSocket).

test_server_fnf(_Config) ->
    Self = self(),
    Ref = make_ref(),
    AtConnectFun = fun(RSocket) -> Self ! {connected, Ref, RSocket} end,
    ServerConfig = #{ at_connect => AtConnectFun },
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    FnfHandler = fun(#{ request := Message }) ->
                         Self ! {fnf, Ref, Message}
                 end,
    ClientConfig = #{ handlers => #{ fire_and_forget => FnfHandler }},
    {ok, ClientRSocket} = rsocket_loopback:connect(Listener, ClientConfig),
    receive
        {connected, Ref, ServerRSocket} ->
            Message = <<"The Message">>,
            ok = rsocket:cast(ServerRSocket, Message),
            receive
                {fnf, Ref, Message} ->
                    ok = rsocket:close_connection(ClientRSocket)
            after 1000 ->
                    exit(did_not_handle_fnf_request)
            end
    after 10000 ->
            exit(connection_failed)
    end.
