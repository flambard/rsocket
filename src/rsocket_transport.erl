-module(rsocket_transport).

%%%
%%% rsocket_transport is the API module for RSocket transport implementations
%%%
%%
%% A transport implementation must:
%% 1. ...
%%

%% API
-export([
         accept_connection/1,
         initiate_connection/1,
         recv_frame/2
        ]).


-callback send_frame(Transport :: term(), Frame :: binary()) -> ok.
-callback close_connection(Transport :: term()) -> ok.


%%%===================================================================
%%% API
%%%===================================================================

-spec accept_connection(Module :: atom()) -> {ok, Connection :: term()}.
accept_connection(Module) ->
    rsocket_connection_sup:accept_connection(Module, self()).

-spec initiate_connection(Module :: atom()) -> {ok, Connection :: term()}.
initiate_connection(Module) ->
    rsocket_connection_sup:initiate_connection(Module, self()).

-spec recv_frame(RSocket :: term(), Frame :: binary()) -> ok.
recv_frame(RSocket, Frame) ->
    rsocket_connection:recv_frame(RSocket, Frame).


%%%===================================================================
%%% Internal functions
%%%===================================================================
