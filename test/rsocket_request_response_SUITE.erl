-module(rsocket_request_response_SUITE).

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
     test_client_request_response,
     test_client_request_response_with_metadata,
     test_server_request_response,
     test_concurrent_client_request_responses,
     test_cancel_request_response
    ].

test_client_request_response(_Config) ->
    Request = <<"PING">>,
    ExpectedResponse = <<"PONG">>,
    ServerRRHandler = fun(_Request) -> {reply, ExpectedResponse} end,
    ServerConfig = #{ handlers => #{ request_response => ServerRRHandler }},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    Ref = make_ref(),
    Self = self(),
    ClientRRHandler = fun(Response) -> Self ! {response, Ref, Response} end,
    case rsocket:request_response(RSocket, Request, ClientRRHandler) of
        {ok, _StreamID} -> ok;
        {error, Reason} ->
            exit({call_returned_error, Reason})
    end,
    receive
        {response, Ref, {error, timeout}} ->
            exit(timeout_waiting_for_payload);
        {response, Ref, {ok, ExpectedResponse, _PayloadOptions}} ->
            ok;
        {response, Ref, Response} ->
            exit({unexpected_response, Response})
    end,
    ok = rsocket:close_connection(RSocket).

test_client_request_response_with_metadata(_Config) ->
    Request = <<"PING">>,
    ExpectedResponse = <<"PONG">>,
    Metadata = <<"TABLE TENNIS">>,
    ServerRRHandler = fun(#{ request := _Request, metadata := M }) ->
                              {reply, ExpectedResponse, [{metadata, M}]}
                      end,
    Config = #{ handlers => #{ request_response => ServerRRHandler }},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    Ref = make_ref(),
    Self = self(),
    ClientRRHandler = fun(Response) -> Self ! {response, Ref, Response} end,
    Options = [{metadata, Metadata}],
    case rsocket:request_response(RSocket, Request, ClientRRHandler, Options) of
        {ok, _StreamID} -> ok;
        {error, Reason} ->
            exit({call_returned_error, Reason})
    end,
    receive
        {response, Ref, {error, timeout}} ->
            exit(timeout_waiting_for_payload);
        {response, Ref, {ok, ExpectedResponse, PayloadOptions}} ->
            case proplists:lookup(metadata, PayloadOptions) of
                none                 -> exit(metadata_not_sent_with_response);
                {metadata, Metadata} -> ok
            end;
        {response, Ref, Response} ->
            exit({unexpected_response, Response})
    end,
    ok = rsocket:close_connection(RSocket).

test_server_request_response(_Config) ->
    Self = self(),
    Ref = make_ref(),
    AtConnectFun = fun(RSocket) -> Self ! {connected, Ref, RSocket} end,
    ServerConfig = #{ at_connect => AtConnectFun },
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    Request = <<"PING">>,
    ExpectedResponse = <<"PONG">>,
    ClientRRHandler = fun(_Request) -> {reply, ExpectedResponse} end,
    ClientConfig = #{ handlers => #{ request_response => ClientRRHandler }},
    {ok, ClientRSocket} = rsocket_loopback:connect(Listener, ClientConfig),
    receive
        {connected, Ref, ServerRSocket} ->
            Handler = fun(Response) -> Self ! {response, Ref, Response} end,
            case rsocket:request_response(ServerRSocket, Request, Handler) of
                {ok, _StreamID} -> ok;
                {error, Reason} ->
                    exit({call_returned_error, Reason})
            end
    after 10000 ->
            exit(connection_failed)
    end,
    receive
        {response, Ref, {error, timeout}} ->
            exit(timeout_waiting_for_payload);
        {response, Ref, {ok, ExpectedResponse, _PayloadOptions}} ->
            ok;
        {response, Ref, Response} ->
            exit({unexpected_response, Response})
    end,
    ok = rsocket:close_connection(ClientRSocket).

test_concurrent_client_request_responses(_Config) ->
    Request1 = <<"PING1">>,
    Request2 = <<"PING2">>,
    ServerRRHandler = fun(#{request := Request}) ->
                              receive
                              after 100 -> {reply, <<Request/binary, "PONG">>}
                              end
                      end,
    Response1 = <<Request1/binary, "PONG">>,
    Response2 = <<Request2/binary, "PONG">>,
    Config = #{ handlers => #{ request_response => ServerRRHandler }},
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    Ref = make_ref(),
    Self = self(),
    Handler = fun(Response) -> Self ! {response, Ref, Response} end,
    case rsocket:request_response(RSocket, Request1, Handler) of
        {ok, StreamID1} ->
            spawn_link(
              fun() ->
                      RR = rsocket:request_response(RSocket, Request2, Handler),
                      case RR of
                          {ok, StreamID1} ->
                              exit(stream_id_reused);
                          {ok, _StreamID2} ->
                              ok;
                          {error, Reason} ->
                              exit({call_returned_error, Reason})
                      end
              end);
        {error, Reason} ->
            exit({call_returned_error, Reason})
    end,
    receive
        {response, Ref, {error, timeout}} ->
            exit(timeout_waiting_for_payload);
        {response, Ref, {ok, Response1, _}} ->
            ok;
        {response, Ref, {ok, Response2, _}} ->
            ok;
        {response, Ref, UnexpectedResponse1} ->
            exit({unexpected_response, UnexpectedResponse1})
    end,
    receive
        {response, Ref, {error, timeout}} ->
            exit(timeout_waiting_for_payload);
        {response, Ref, {ok, Response1, _}} ->
            ok;
        {response, Ref, {ok, Response2, _}} ->
            ok;
        {response, Ref, UnexpectedResponse2} ->
            exit({unexpected_response, UnexpectedResponse2})
    end,
    ok = rsocket:close_connection(RSocket).

test_cancel_request_response(_Config) ->
    Request = <<"PING">>,
    ExpectedResponse = <<"PONG">>,
    ServerRRHandler = fun(_Request) ->
                              receive
                              after 1000 -> {reply, ExpectedResponse}
                              end
                      end,
    ServerConfig = #{ handlers => #{ request_response => ServerRRHandler }},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    {ok, RSocket} = rsocket_loopback:connect(Listener),
    Ref = make_ref(),
    Self = self(),
    ClientRRHandler = fun(Response) -> Self ! {response, Ref, Response} end,
    case rsocket:request_response(RSocket, Request, ClientRRHandler) of
        {error, Reason} ->
            exit({call_returned_error, Reason});
        {ok, StreamID} ->
            Key = {n, l, {rsocket_stream, RSocket, StreamID}},
            {Stream, _} = gproc:await(Key, 2000),
            Monitor = monitor(process, Stream),
            ok = rsocket:cancel(RSocket, StreamID),
            receive
                {'DOWN', Monitor, process, Stream, canceled} ->
                    ok;
                {response, Ref, Response} ->
                    exit({request_not_canceled, Response})
            after 5000 ->
                    exit(no_response_and_no_down_message)
            end
    end,
    ok = rsocket:close_connection(RSocket).
