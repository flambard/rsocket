-module(rsocket_loopback).
-behaviour(rsocket_transport).

%% API
-export([
         start_listener/0,
         start_listener/1,
         connect/1,
         connect/2
        ]).

%% rsocket_transport callbacks
-export([
         send_frame/2,
         close_connection/1
        ]).


%%%===================================================================
%%% API
%%%===================================================================

start_listener() ->
    Config = #{ handlers => #{} },
    start_listener(Config).

start_listener(Config) ->
    {ok, spawn(fun() -> accept_connection(Config) end)}.

connect(Pid) ->
    Config = #{ handlers => #{} },
    connect(Pid, Config).

connect(Pid, Config) ->
    Self = self(),
    spawn(fun() -> initiate_connection(Pid, Self, Config) end),
    receive
        {rsocket, RSocket} ->
            {ok, RSocket}
    after 1000 ->
            {error, failed_to_connect}
    end.


%%%===================================================================
%%% rsocket_transport callbacks
%%%===================================================================

send_frame(Connection, Frame) ->
    Connection ! {send, iolist_to_binary(Frame)},
    ok.

close_connection(Connection) ->
    Connection ! close_connection,
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

accept_connection(Config) ->
    Handlers = maps:get(handlers, Config, #{}),
    Options = maps:remove(handlers, Config),
    receive
        {connect, Pid} ->
            {ok, RSocket} =
                rsocket_transport:accept_connection(?MODULE, Handlers, Options),
            loop(#{pid => Pid, rsocket => RSocket})
    end.

initiate_connection(Pid, Application, Config) ->
    Pid ! {connect, self()},
    Handlers = maps:get(handlers, Config, #{}),
    Options = maps:remove(handlers, Config),
    {ok, RSocket} =
        rsocket_transport:initiate_connection(?MODULE, Handlers, Options),
    Application ! {rsocket, RSocket},
    loop(#{pid => Pid, rsocket => RSocket}).

loop(State = #{pid := Pid, rsocket := RSocket}) ->
    receive
        {send, Frame} ->
            Pid ! {frame, Frame},
            loop(State);
        close_connection ->
            Pid ! connection_closed;
        connection_closed ->
            ok;
        {frame, Frame} ->
            rsocket_transport:recv_frame(RSocket, Frame),
            loop(State)
    end.
