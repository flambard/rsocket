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
     test_request_stream,
     test_responder_completes_stream,
     test_requester_cancels_stream,
     test_responder_sends_error
    ].

test_request_stream(_Config) ->
    Request = <<"PING">>,
    Ref = make_ref(),
    Self = self(),
    AtConnectFun = fun(RSocket) ->
                           Self ! {connected, Ref, RSocket}
                   end,
    ServerConfig = #{ at_connect => AtConnectFun,
                      handlers =>
                          #{ stream_responder =>
                                 {
                                  rsocket_passthrough_stream_responder,
                                  [Ref, Self]
                                 }
                           }},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    ClientConfig = #{ handlers =>
                          #{ stream_requester =>
                                 {
                                  rsocket_passthrough_stream_requester,
                                  [Ref, Self]
                                 }
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
    P1 = <<"ONE">>,
    P2 = <<"TWO">>,
    P3 = <<"THREE">>,
    ok = rsocket_passthrough_stream_responder:send_payload(Responder, P1, []),
    ok = rsocket_passthrough_stream_responder:send_payload(Responder, P2, []),
    ok = rsocket_passthrough_stream_responder:send_payload(Responder, P3, []),
    receive
        {requester, handle_payload, Ref, P1, _} -> ok
    end,
    receive
        {requester, handle_payload, Ref, P2, _} -> ok
    end,
    receive
        {requester, handle_payload, Ref, P3, _} -> ok
    end,
    ok = rsocket:close_connection(ClientRSocket).


test_responder_completes_stream(_Config) ->
    Request = <<"PING">>,
    Ref = make_ref(),
    Self = self(),
    AtConnectFun = fun(RSocket) ->
                           Self ! {connected, Ref, RSocket}
                   end,
    ServerConfig = #{ at_connect => AtConnectFun,
                      handlers =>
                          #{ stream_responder =>
                                 {
                                  rsocket_passthrough_stream_responder,
                                  [Ref, Self]
                                 }
                           }},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    ClientConfig = #{ handlers =>
                          #{ stream_requester =>
                                 {
                                  rsocket_passthrough_stream_requester,
                                  [Ref, Self]
                                 }
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
    RequesterName = {n, l, {rsocket_stream, ClientRSocket, StreamID}},
    Requester = gproc:where(RequesterName),
    ResponderName = {n, l, {rsocket_stream, ServerRSocket, StreamID}},
    {Responder, _} = gproc:await(ResponderName),
    receive
        {responder, handle_request_n, Ref, N} -> ok
    end,
    RequesterMonitor = erlang:monitor(process, Requester),
    ResponderMonitor = erlang:monitor(process, Responder),
    ClientMonitor = erlang:monitor(process, ClientRSocket),
    ServerMonitor = erlang:monitor(process, ServerRSocket),
    P1 = <<"ONE">>,
    P2 = <<"TWO">>,
    ok = rsocket_passthrough_stream_responder:send_payload(Responder, P1, []),
    ok = rsocket_passthrough_stream_responder:send_payload(
           Responder, P2, [complete]),
    receive
        {requester, handle_payload, Ref, P1, _} -> ok
    end,
    receive
        {requester, handle_payload, Ref, P2, _} -> ok
    end,
    receive
        {'DOWN', ClientMonitor, process, ClientRSocket, _} ->
            exit(client_rsocket_terminated);
        {'DOWN', ServerMonitor, process, ServerRSocket, _} ->
            exit(server_rsocket_terminated)
    after 1000 ->
            ok
    end,
    receive
        {'DOWN', RequesterMonitor, process, Requester, _} ->
            ok
    after 100 ->
            exit(requester_did_not_terminate)
    end,
    receive
        {'DOWN', ResponderMonitor, process, Responder, _} ->
            ok
    after 100 ->
            exit(responder_did_not_terminate)
    end,
    ok = rsocket:close_connection(ClientRSocket).


test_requester_cancels_stream(_Config) ->
    Request = <<"PING">>,
    Ref = make_ref(),
    Self = self(),
    AtConnectFun = fun(RSocket) ->
                           Self ! {connected, Ref, RSocket}
                   end,
    ServerConfig = #{ at_connect => AtConnectFun,
                      handlers =>
                          #{ stream_responder =>
                                 {
                                  rsocket_passthrough_stream_responder,
                                  [Ref, Self]
                                 }
                           }},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    ClientConfig = #{ handlers =>
                          #{ stream_requester =>
                                 {
                                  rsocket_passthrough_stream_requester,
                                  [Ref, Self]
                                 }
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
    RequesterName = {n, l, {rsocket_stream, ClientRSocket, StreamID}},
    Requester = gproc:where(RequesterName),
    ResponderName = {n, l, {rsocket_stream, ServerRSocket, StreamID}},
    {Responder, _} = gproc:await(ResponderName),
    receive
        {responder, handle_request_n, Ref, N} -> ok
    end,
    P1 = <<"ONE">>,
    P2 = <<"TWO">>,
    ok = rsocket_passthrough_stream_responder:send_payload(Responder, P1, []),
    ok = rsocket_passthrough_stream_responder:send_payload(Responder, P2, []),
    receive
        {requester, handle_payload, Ref, P1, _} -> ok
    end,
    receive
        {requester, handle_payload, Ref, P2, _} -> ok
    end,
    RequesterMonitor = erlang:monitor(process, Requester),
    ResponderMonitor = erlang:monitor(process, Responder),
    ClientMonitor = erlang:monitor(process, ClientRSocket),
    ServerMonitor = erlang:monitor(process, ServerRSocket),
    ok = rsocket_passthrough_stream_requester:send_cancel(Requester),
    receive
        {'DOWN', ClientMonitor, process, ClientRSocket, _} ->
            exit(client_rsocket_terminated);
        {'DOWN', ServerMonitor, process, ServerRSocket, _} ->
            exit(server_rsocket_terminated)
    after 1000 ->
            ok
    end,
    receive
        {'DOWN', RequesterMonitor, process, Requester, _} ->
            ok
    after 100 ->
            exit(requester_did_not_terminate)
    end,
    receive
        {'DOWN', ResponderMonitor, process, Responder, _} ->
            ok
    after 100 ->
            exit(responder_did_not_terminate)
    end,
    ok = rsocket:close_connection(ClientRSocket).


test_responder_sends_error(_Config) ->
    Request = <<"PING">>,
    Ref = make_ref(),
    Self = self(),
    AtConnectFun = fun(RSocket) ->
                           Self ! {connected, Ref, RSocket}
                   end,
    ServerConfig = #{ at_connect => AtConnectFun,
                      handlers =>
                          #{ stream_responder =>
                                 {
                                  rsocket_passthrough_stream_responder,
                                  [Ref, Self]
                                 }
                           }},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    ClientConfig = #{ handlers =>
                          #{ stream_requester =>
                                 {
                                  rsocket_passthrough_stream_requester,
                                  [Ref, Self]
                                 }
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
    RequesterName = {n, l, {rsocket_stream, ClientRSocket, StreamID}},
    Requester = gproc:where(RequesterName),
    ResponderName = {n, l, {rsocket_stream, ServerRSocket, StreamID}},
    {Responder, _} = gproc:await(ResponderName),
    receive
        {responder, handle_request_n, Ref, N} -> ok
    end,
    P1 = <<"ONE">>,
    ok = rsocket_passthrough_stream_responder:send_payload(Responder, P1, []),
    receive
        {requester, handle_payload, Ref, P1, _} -> ok
    end,
    RequesterMonitor = erlang:monitor(process, Requester),
    ResponderMonitor = erlang:monitor(process, Responder),
    ClientMonitor = erlang:monitor(process, ClientRSocket),
    ServerMonitor = erlang:monitor(process, ServerRSocket),
    ok = rsocket_passthrough_stream_responder:send_error(
           Responder, application_error, <<"Something went wrong">>),
    receive
        {'DOWN', ClientMonitor, process, ClientRSocket, _} ->
            exit(client_rsocket_terminated);
        {'DOWN', ServerMonitor, process, ServerRSocket, _} ->
            exit(server_rsocket_terminated)
    after 1000 ->
            ok
    end,
    receive
        {'DOWN', RequesterMonitor, process, Requester, _} ->
            ok
    after 100 ->
            exit(requester_did_not_terminate)
    end,
    receive
        {'DOWN', ResponderMonitor, process, Responder, _} ->
            ok
    after 100 ->
            exit(responder_did_not_terminate)
    end,
    ok = rsocket:close_connection(ClientRSocket).
