-module(rsocket_rpc_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

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
        test_cast,
        test_cast_with_metadata,
        test_client_call,
        test_client_call_with_metadata,
        test_server_call,
        test_concurrent_client_calls
    ].

test_cast(_Config) ->
    Self = self(),
    Ref = make_ref(),
    FnfHandler = fun(#{request := Message}) -> Self ! {fnf, Ref, Message} end,
    Config = #{handlers => #{fire_and_forget => FnfHandler}},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    Message = <<"The Message">>,
    ok = rsocket_rpc:cast(RSocket, Message),
    receive
        {fnf, Ref, Message} ->
            ok = rsocket:close_connection(RSocket)
    after 1000 ->
        exit(did_not_handle_fnf_request)
    end.

test_cast_with_metadata(_Config) ->
    Self = self(),
    Ref = make_ref(),
    FnfHandler =
        fun(#{request := Message, metadata := Metadata}) ->
            Self ! {fnf, Ref, Message, Metadata}
        end,
    Config = #{handlers => #{fire_and_forget => FnfHandler}},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    Message = <<"The Message">>,
    Metadata = <<"About The Message">>,
    ok = rsocket_rpc:cast(RSocket, Message, [{metadata, Metadata}]),
    receive
        {fnf, Ref, Message, Metadata} ->
            ok
    after 1000 ->
        exit(did_not_handle_fnf_request)
    end,
    ok = rsocket:close_connection(RSocket).

test_client_call(_Config) ->
    Request = <<"PING">>,
    Response = <<"PONG">>,
    ServerRRHandler = fun(_Request) -> {reply, Response} end,
    Config = #{handlers => #{request_response => ServerRRHandler}},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    case rsocket_rpc:call(RSocket, Request) of
        {ok, Response, _Options} ->
            ok;
        {error, Reason} ->
            exit({call_returned_error, Reason});
        _ ->
            exit(unexpected_return_value)
    end,
    ok = rsocket:close_connection(RSocket).

test_client_call_with_metadata(_Config) ->
    Request = <<"PING">>,
    Response = <<"PONG">>,
    Metadata = <<"TABLE TENNIS">>,
    ServerRRHandler =
        fun(#{request := _Request, metadata := M}) -> {reply, Response, [{metadata, M}]} end,
    Config = #{handlers => #{request_response => ServerRRHandler}},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    case rsocket_rpc:call(RSocket, Request, [{metadata, Metadata}]) of
        {ok, Response, Options} ->
            case proplists:lookup(metadata, Options) of
                none ->
                    exit(metadata_not_sent_with_response);
                {metadata, Metadata} ->
                    ok
            end;
        {error, Reason} ->
            exit({call_returned_error, Reason});
        Other ->
            exit({unexpected_return_value, Other})
    end,
    ok = rsocket:close_connection(RSocket).

test_server_call(_Config) ->
    Self = self(),
    Ref = make_ref(),
    AtConnectFun = fun(RSocket) -> Self ! {connected, Ref, RSocket} end,
    ServerConfig = #{at_connect => AtConnectFun},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    Request = <<"PING">>,
    Response = <<"PONG">>,
    ClientRRHandler = fun(_Request) -> {reply, Response} end,
    ClientConfig = #{handlers => #{request_response => ClientRRHandler}},
    {ok, ClientRSocket} = rsocket_loopback:connect(Listener, ClientConfig),
    receive
        {connected, Ref, ServerRSocket} ->
            case rsocket_rpc:call(ServerRSocket, Request) of
                {ok, Response, _Options} ->
                    ok;
                {error, Reason} ->
                    exit({call_returned_error, Reason});
                _ ->
                    exit(unexpected_return_value)
            end
    after 10000 ->
        exit(connection_failed)
    end,
    ok = rsocket:close_connection(ClientRSocket).

test_concurrent_client_calls(_Config) ->
    Request1 = <<"PING1">>,
    Request2 = <<"PING2">>,
    ServerRRHandler =
        fun(#{request := Request}) ->
            receive
            after 100 ->
                {reply, <<Request/binary, "PONG">>}
            end
        end,
    Response1 = <<Request1/binary, "PONG">>,
    Response2 = <<Request2/binary, "PONG">>,
    Config = #{handlers => #{request_response => ServerRRHandler}},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    spawn_link(fun() ->
        case rsocket_rpc:call(RSocket, Request1) of
            {ok, Response1, _Options} ->
                ok;
            {error, Reason} ->
                exit({call_returned_error, Reason});
            _ ->
                exit(unexpected_return_value)
        end
    end),
    case rsocket_rpc:call(RSocket, Request2) of
        {ok, Response2, _Options} ->
            ok;
        {error, Reason} ->
            exit({call_returned_error, Reason});
        _ ->
            exit(unexpected_return_value)
    end.
