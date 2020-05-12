-module(rsocket_request_stream_SUITE).

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
     test_client_request_stream
    ].

test_client_request_stream(_Config) ->
    Request = <<"PING">>,
    Ref = make_ref(),
    Self = self(),
    AtConnectFun = fun(RSocket) ->
                           Self ! {connected, Ref, RSocket}
                   end,
    ServerConfig = #{ at_connect => AtConnectFun,
                      handlers =>
                          #{ stream_responder =>
                                 {rsocket_test_stream_responder, [Ref, Self]}
                           }},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    ClientConfig = #{ handlers =>
                          #{ stream_requester =>
                                 {rsocket_test_stream_requester, [Ref, Self]}
                           }},
    {ok, ClientRSocket} = rsocket_loopback:connect(Listener, ClientConfig),
    ServerRSocket =
        receive
            {connected, Ref, RSocket} -> RSocket
        after 10000 ->
                exit(connection_failed)
        end,
    N = 3,
    Options = [],
    {ok, StreamID} = rsocket:request_stream(ClientRSocket, N, Request, Options),
    ResponderName = {n, l, {rsocket_stream, ServerRSocket, StreamID}},
    {Responder, _} = gproc:await(ResponderName),
    receive
        {responder, handle_request_n, Ref, N} -> ok
    end,
    Payload1 = <<"ONE">>,
    Payload2 = <<"TWO">>,
    Payload3 = <<"THREE">>,
    ok = rsocket_stream_responder:payload(Responder, Payload1, []),
    ok = rsocket_stream_responder:payload(Responder, Payload2, []),
    ok = rsocket_stream_responder:payload(Responder, Payload3, []),
    receive
        {requester, handle_payload, Ref, Payload1, _} -> ok
    end,
    receive
        {requester, handle_payload, Ref, Payload2, _} -> ok
    end,
    receive
        {requester, handle_payload, Ref, Payload3, _} -> ok
    end,
    ok = rsocket:close_connection(ClientRSocket).
