-module(rsocket).

%% API
-export([
         cancel/2,
         close_connection/1,
         lease/3,
         lease/4,
         metadata_push/2,
         request_channel/3,
         request_channel/4,
         request_fnf/2,
         request_fnf/3,
         request_response/3,
         request_response/4,
         request_stream/3,
         request_stream/4
        ]).

-type connection() :: pid().
-type stream_id() :: pos_integer().
-type request() :: binary().
-type metadata() :: binary().
-type handler() :: function().

%%%===================================================================
%%% API
%%%===================================================================

-spec cancel(connection(), stream_id()) -> ok.
cancel(Connection, StreamID) ->
    rsocket_connection:send_cancel(Connection, StreamID).

-spec close_connection(connection()) -> ok.
close_connection(Connection) ->
    rsocket_connection:close(Connection).

-spec lease(connection(), pos_integer(), pos_integer()) -> ok.
lease(Connection, TimeToLive, NumberOfRequests) ->
    lease(Connection, TimeToLive, NumberOfRequests, []).

-spec lease(connection(), pos_integer(), pos_integer(), list()) -> ok.
lease(Connection, TimeToLive, NumberOfRequests, Options) ->
    rsocket_connection:send_lease(
      Connection, TimeToLive, NumberOfRequests, Options).

-spec metadata_push(connection(), metadata()) -> ok.
metadata_push(Connection, Metadata) ->
    rsocket_connection:send_metadata_push(Connection, Metadata).

-spec request_channel(connection(), non_neg_integer(), request())
                     -> {ok, stream_id()} | {error, Reason :: atom()}.
request_channel(Connection, N, Request) ->
    request_channel(Connection, N, Request, []).

-spec request_channel(connection(), non_neg_integer(), request(), list())
                     -> {ok, stream_id()} | {error, Reason :: atom()}.
request_channel(Connection, N, Request, Options) ->
    rsocket_connection:send_request_channel(Connection, N, Request, Options).

-spec request_fnf(connection(), request()) -> ok | {error, Reason :: atom()}.
request_fnf(Connection, Message) ->
    request_fnf(Connection, Message, []).

-spec request_fnf(connection(), request(), list()) -> ok | {error, Reason :: atom()}.
request_fnf(Connection, Message, Options) ->
    rsocket_connection:send_request_fnf(Connection, Message, Options).

-spec request_response(connection(), request(), handler())
                      -> {ok, stream_id()} | {error, Reason :: atom()}.
request_response(Connection, Request, Handler) ->
    request_response(Connection, Request, Handler, []).

-spec request_response(connection(), request(), handler(), list())
                      -> {ok, stream_id()} | {error, Reason :: atom()}.
request_response(Connection, Request, Handler, Options) ->
    rsocket_connection:send_request_response(
      Connection, Request, Handler, Options).

-spec request_stream(connection(), non_neg_integer(), request())
                    -> {ok, stream_id()} | {error, Reason :: atom()}.
request_stream(Connection, N, Request) ->
    request_stream(Connection, N, Request, []).

-spec request_stream(connection(), non_neg_integer(), request(), list())
                    -> {ok, stream_id()} | {error, Reason :: atom()}.
request_stream(Connection, N, Request, Options) ->
    rsocket_connection:send_request_stream(Connection, N, Request, Options).


%%%===================================================================
%%% Internal functions
%%%===================================================================
