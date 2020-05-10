-module(rsocket_stream).
-behaviour(gen_server).

%% API
-export([
         start_link/3,
         recv_payload/3,
         recv_request_n/2,
         send_cancel/1,
         send_error/3,
         send_payload/3,
         send_request_n/2
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2
        ]).

-record(state,
        {
         id,
         connection,
         module,
         application_state
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link(StreamID, Connection, Module) ->
    gen_server:start_link(?MODULE, [StreamID, Connection, Module], []).

recv_payload(Stream, Payload, Options) ->
    gen_server:cast(Stream, {recv_payload, Payload, Options}).

recv_request_n(Stream, N) ->
    gen_server:cast(Stream, {recv_request_n, N}).

send_cancel(Stream) ->
    gen_server:cast(Stream, send_cancel).

send_error(Stream, ErrorType, ErrorData) ->
    gen_server:cast(Stream, {send_error, ErrorType, ErrorData}).

send_payload(Stream, Payload, Options) ->
    gen_server:cast(Stream, {send_payload, Payload, Options}).

send_request_n(Stream, N) ->
    gen_server:cast(Stream, {send_request_n, N}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([StreamID, Connection, Module]) ->
    AppInitArgs = #{}, %% TODO: Initialize the application side correctly
    {ok, AppState} = Module:init(AppInitArgs),
    {ok, #state{
            id = StreamID,
            connection = Connection,
            module = Module,
            application_state = AppState
           }}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({recv_payload, Payload, Options}, State) ->
    #state{ module = Module, application_state = AppState } = State,
    Module:handle_payload(Payload, Options, AppState),
    {noreply, State};

handle_cast({recv_request_n, N}, State) ->
    #state{ module = Module, application_state = AppState } = State,
    Module:handle_request_n(N, AppState),
    {noreply, State};

handle_cast(send_cancel, State) ->
    #state{ id = StreamID, connection = Connection } = State,
    rsocket_connection:send_cancel(Connection, StreamID),
    {noreply, State};

handle_cast({send_error, ErrorType, ErrorData}, State) ->
    #state{ id = StreamID, connection = Connection } = State,
    rsocket_connection:send_error(Connection, StreamID, ErrorType, ErrorData),
    {stop, {error, ErrorType, ErrorData}, State};

handle_cast({send_payload, Payload, Options}, State) ->
    #state{ id = StreamID, connection = Connection } = State,
    rsocket_connection:send_payload(Connection, StreamID, Payload, Options),
    {noreply, State};

handle_cast({send_request_n, N}, State) ->
    #state{ id = StreamID, connection = Connection } = State,
    rsocket_connection:send_request_n(Connection, StreamID, N),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.


-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.


%%%===================================================================
%%% Internal functions
%%%===================================================================
