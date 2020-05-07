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
     test_open_close_connection,
     test_client_push_metadata,
     test_server_push_metadata,
     test_client_not_honoring_lease,
     test_client_honoring_lease
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

test_client_push_metadata(_Config) ->
    Self = self(),
    Ref = make_ref(),
    MetadataPushHandler = fun(Metadata) -> Self ! {metadata, Ref, Metadata} end,
    Config = #{ handlers => #{ metadata_push => MetadataPushHandler } },
    {ok, Listener} = rsocket_loopback:start_listener(Config),
    {ok, ClientRSocket} = rsocket_loopback:connect(Listener),
    TheMetadata = <<"binary">>,
    ok = rsocket:metadata_push(ClientRSocket, TheMetadata),
    receive
        {metadata, Ref, TheMetadata} ->
            ok
    after 10000 ->
            exit(connection_failed)
    end,
    ok = rsocket:close_connection(ClientRSocket).

test_server_push_metadata(_Config) ->
    Self = self(),
    Ref = make_ref(),
    AtConnectFun = fun(RSocket) -> Self ! {connected, Ref, RSocket} end,
    ServerConfig = #{ at_connect => AtConnectFun },
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    MetadataPushHandler = fun(Metadata) -> Self ! {metadata, Ref, Metadata} end,
    ClientConfig = #{ handlers => #{ metadata_push => MetadataPushHandler }},
    {ok, ClientRSocket} = rsocket_loopback:connect(Listener, ClientConfig),
    receive
        {connected, Ref, ServerRSocket} ->
            TheMetadata = <<"binary">>,
            ok = rsocket:metadata_push(ServerRSocket, TheMetadata),
            receive
                {metadata, Ref, TheMetadata} ->
                    ok
            after 1000 ->
                    exit(did_not_handle_metadata_push)
            end
    after 10000 ->
            exit(connection_failed)
    end,
    ok = rsocket:close_connection(ClientRSocket).

test_client_not_honoring_lease(_Config) ->
    Self = self(),
    Ref = make_ref(),
    FnfHandler = fun(#{request := Message}) ->
                         Self ! {fnf, Ref, Message}
                 end,
    AtConnectFun = fun(RSocket) ->
                           Self ! {connected, Ref, RSocket}
                   end,
    ServerConfig = #{ at_connect => AtConnectFun,
                      handlers => #{ fire_and_forget => FnfHandler }},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    ClientConfig = #{ leasing => true },
    {ok, RSocket} = rsocket_loopback:connect(Listener, ClientConfig),
    Message = <<"Unsolicited Request">>,
    ok = rsocket:cast(RSocket, Message),
    receive
        {connected, Ref, _ServerRSocket} ->
            ok
    after 10000 ->
            exit(connection_failed)
    end,
    receive
        {fnf, Ref, Message} ->
            exit(request_fnf_went_through_without_lease)
    after 500 ->
            ok
    end,
    ok = rsocket:close_connection(RSocket).

test_client_honoring_lease(_Config) ->
    Self = self(),
    Ref = make_ref(),
    FnfHandler = fun(#{request := Message}) ->
                         Self ! {fnf, Ref, Message}
                 end,
    AtConnectFun = fun(RSocket) ->
                           Self ! {connected, Ref, RSocket}
                   end,
    ServerConfig = #{ at_connect => AtConnectFun,
                      handlers => #{ fire_and_forget => FnfHandler }},
    {ok, Listener} = rsocket_loopback:start_listener(ServerConfig),
    ClientConfig = #{ leasing => true },
    {ok, RSocket} = rsocket_loopback:connect(Listener, ClientConfig),
    receive
        {connected, Ref, ServerRSocket} ->
            TimeToLive = 1000,
            NumberOfRequests = 1,
            ok = rsocket:lease(ServerRSocket, TimeToLive, NumberOfRequests)
    after 10000 ->
            exit(connection_failed)
    end,
    Message = <<"Expected Request">>,
    ok = rsocket:cast(RSocket, Message),
    receive
        {fnf, Ref, Message} ->
            ok
    after 500 ->
            exit(request_fnf_did_not_went_through_with_lease)
    end,
    ok = rsocket:close_connection(RSocket).
