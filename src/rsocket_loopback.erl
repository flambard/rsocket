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
    start_listener(fun() -> ok end).

start_listener(AtConnect) ->
    {ok, spawn(fun() -> accept_connection(AtConnect) end)}.

connect(Pid) ->
    connect(Pid, fun() -> ok end).

connect(Pid, AtConnect) ->
    {ok, spawn(fun() -> initiate_connection(Pid, AtConnect) end)}.


%%%===================================================================
%%% rsocket_transport callbacks
%%%===================================================================

send_frame(Connection, Frame) ->
    Connection ! {send, Frame},
    ok.

close_connection(Connection) ->
    Connection ! close_connection,
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

accept_connection(AtConnect) ->
    receive
        {connect, Pid} ->
            {ok, RSocket} = rsocket_transport:accept_connection(?MODULE),
            AtConnect(),
            loop(#{pid => Pid, rsocket => RSocket})
    end.

initiate_connection(Pid, AtConnect) ->
    Pid ! {connect, self()},
    {ok, RSocket} = rsocket_transport:initiate_connection(?MODULE),
    AtConnect(),
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
