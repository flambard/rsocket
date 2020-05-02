-module(rsocket_SUITE).

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
     test_open_close_connection,
     test_client_fnf,
     test_client_fnf_with_metadata,
     test_server_fnf,
     test_client_request_response,
     test_server_request_response,
     test_concurrent_client_request_responses
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


test_client_request_response(_Config) ->
    Request = <<"PING">>,
    Response = <<"PONG">>,
    ServerRRHandler = fun(_Request) -> Response end,
    Config = #{ handlers => #{ request_response => ServerRRHandler }},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    case rsocket:call(RSocket, Request) of
        {ok, Response} -> rsocket:close_connection(RSocket);
        {error, Reason} ->
            exit({call_returned_error, Reason});
        _ ->
            exit(unexpected_return_value)
    end.

test_server_request_response(_Config) ->
    Self = self(),
    Ref = make_ref(),
    AtConnectFun = fun(RSocket) -> Self ! {connected, Ref, RSocket} end,
    ServerConfig = #{ at_connect => AtConnectFun },
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    Request = <<"PING">>,
    Response = <<"PONG">>,
    ClientRRHandler = fun(_Request) -> Response end,
    ClientConfig = #{ handlers => #{ request_response => ClientRRHandler }},
    {ok, ClientRSocket} = rsocket_loopback:connect(Listener, ClientConfig),
    receive
        {connected, Ref, ServerRSocket} ->
            case rsocket:call(ServerRSocket, Request) of
                {ok, Response} -> rsocket:close_connection(ClientRSocket);
                {error, Reason} ->
                    exit({call_returned_error, Reason});
                _ ->
                    exit(unexpected_return_value)
            end
    after 10000 ->
            exit(connection_failed)
    end.

test_concurrent_client_request_responses(_Config) ->
    Request1 = <<"PING1">>,
    Request2 = <<"PING2">>,
    ServerRRHandler = fun(Request) ->
                              receive
                              after 100 -> <<Request/binary, "PONG">>
                              end
                      end,
    Response1 = <<Request1/binary, "PONG">>,
    Response2 = <<Request2/binary, "PONG">>,
    Config = #{ handlers => #{ request_response => ServerRRHandler }},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    spawn_link(fun() ->
                       case rsocket:call(RSocket, Request1) of
                           {ok, Response1} -> ok;
                           {error, Reason} ->
                               exit({call_returned_error, Reason});
                           _ ->
                               exit(unexpected_return_value)
                       end
               end),
    case rsocket:call(RSocket, Request2) of
        {ok, Response2} -> ok;
        {error, Reason} ->
            exit({call_returned_error, Reason});
        _ ->
            exit(unexpected_return_value)
    end.
