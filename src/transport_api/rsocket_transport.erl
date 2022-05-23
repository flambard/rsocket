-module(rsocket_transport).

%%%
%%% rsocket_transport is the API module for RSocket transport implementations
%%%

%% API
-export([accept_connection/3, initiate_connection/3, recv_frame/2]).

-callback send_frame(Transport :: term(), Frame :: iolist()) -> ok.
-callback close_connection(Transport :: term()) -> ok.

%%%===================================================================
%%% API
%%%===================================================================

-spec accept_connection(Module :: atom(), Handlers :: map(), Options :: map()) ->
                           {ok, Connection :: term()}.
accept_connection(Module, Handlers, Options) ->
    rsocket_connection_sup:accept_connection(Module, self(), Handlers, Options).

-spec initiate_connection(Module :: atom(), Handlers :: map(), Options :: map()) ->
                             {ok, Connection :: term()}.
initiate_connection(Module, Handlers, Options) ->
    rsocket_connection_sup:initiate_connection(Module, self(), Handlers, Options).

-spec recv_frame(RSocket :: term(), Frame :: binary()) -> ok.
recv_frame(RSocket, Frame) ->
    rsocket_connection:recv_frame(RSocket, Frame).

%%%===================================================================
%%% Internal functions
%%%===================================================================
