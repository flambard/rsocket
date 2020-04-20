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
    Config = #{ at_connect => fun() -> ok end,
                handlers => #{}
              },
    start_listener(Config).

start_listener(Config) ->
    {ok, spawn(fun() -> accept_connection(Config) end)}.

connect(Pid) ->
    Config = #{ at_connect => fun() -> ok end,
                handlers => #{}
              },
    connect(Pid, Config).

connect(Pid, Config) ->
    {ok, spawn(fun() -> initiate_connection(Pid, Config) end)}.


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

accept_connection(#{at_connect := AtConnect, handlers := Handlers}) ->
    receive
        {connect, Pid} ->
            {ok, RSocket} =
                rsocket_transport:accept_connection(?MODULE, Handlers),
            AtConnect(),
            loop(#{pid => Pid, rsocket => RSocket})
    end.

initiate_connection(Pid, #{at_connect := AtConnect, handlers := Handlers}) ->
    Pid ! {connect, self()},
    {ok, RSocket} = rsocket_transport:initiate_connection(?MODULE, Handlers),
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
