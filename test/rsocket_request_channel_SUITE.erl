-module(rsocket_request_channel_SUITE).

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
     test_request_channel
    ].

test_request_channel(_Config) ->
    Request = <<"PING">>,
    Ref = make_ref(),
    Self = self(),
    AtConnectFun = fun(RSocket) ->
                           Self ! {connected, Ref, RSocket}
                   end,
    ServerConfig = #{ at_connect => AtConnectFun,
                      handlers =>
                          #{ channel =>
                                 { rsocket_passthrough_channel, [Ref, Self] }
                           }},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    ClientConfig = #{ handlers =>
                          #{ channel =>
                                 { rsocket_passthrough_channel, [Ref, Self] }
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
    {ok, StreamID} = rsocket:request_channel(ClientRSocket, N, Request, Options),
    Requester = rsocket_stream:find(ClientRSocket, StreamID),
    Responder = rsocket_stream:await(ServerRSocket, StreamID),
    receive
        {channel, Requester, handle_request_n, Ref, N} -> ok
    end,
    P1 = <<"ONE">>,
    P2 = <<"TWO">>,
    P3 = <<"THREE">>,
    ok = rsocket_passthrough_channel:send_payload(Requester, P1, []),
    ok = rsocket_passthrough_channel:send_payload(Responder, P1, []),
    ok = rsocket_passthrough_channel:send_payload(Requester, P2, []),
    ok = rsocket_passthrough_channel:send_payload(Responder, P2, []),
    ok = rsocket_passthrough_channel:send_payload(Requester, P3, []),
    ok = rsocket_passthrough_channel:send_payload(Responder, P3, []),
    {error, no_credits} =
        rsocket_passthrough_channel:send_payload(Requester, P3, []),
    {error, no_credits} =
        rsocket_passthrough_channel:send_payload(Responder, P3, []),
    receive
        {channel, Responder, handle_payload, Ref, P1, _} -> ok
    end,
    receive
        {channel, Requester, handle_payload, Ref, P1, _} -> ok
    end,
    receive
        {channel, Responder, handle_payload, Ref, P2, _} -> ok
    end,
    receive
        {channel, Requester, handle_payload, Ref, P2, _} -> ok
    end,
    receive
        {channel, Responder, handle_payload, Ref, P3, _} -> ok
    end,
    receive
        {channel, Requester, handle_payload, Ref, P3, _} -> ok
    end,
    ok = rsocket:close_connection(ClientRSocket).
