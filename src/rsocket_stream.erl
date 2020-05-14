-module(rsocket_stream).
-behaviour(gen_server).

%% API
-export([
         start_link/4,
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
         request,
         module,
         recv_credits = 0,
         send_credits = 0,
         application_state
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link(StreamID, Request, Handler, Options) ->
    Name = {via, gproc, {n, l, {rsocket_stream, self(), StreamID}}},
    gen_server:start_link(
      Name, ?MODULE, [StreamID, self(), Request, Handler, Options], []).

send_cancel(Stream) ->
    gen_server:cast(Stream, send_cancel).

send_error(Stream, ErrorType, ErrorData) ->
    gen_server:cast(Stream, {send_error, ErrorType, ErrorData}).

send_payload(Stream, Payload, Options) ->
    gen_server:call(Stream, {send_payload, Payload, Options}).

send_request_n(Stream, N) ->
    gen_server:cast(Stream, {send_request_n, N}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([StreamID, Connection, Request, {Module, AppInitArgs}, Options]) ->
    {ok, AppState} = Module:init(Request, AppInitArgs),
    {ok, #state{
            id = StreamID,
            connection = Connection,
            request = Request,
            module = Module,
            recv_credits = proplists:get_value(recv_credits, Options, 0),
            send_credits = proplists:get_value(send_credits, Options, 0),
            application_state = AppState
           }}.


handle_call({send_payload, _P, _O}, _From, S = #state{ send_credits = 0 }) ->
    {reply, {error, no_credits}, S};
handle_call({send_payload, Payload, Options}, _From, State) ->
    #state{
       id = StreamID,
       connection = Connection,
       send_credits = Credits
      } = State,
    rsocket_connection:send_payload(Connection, StreamID, Payload, Options),
    NewState = State#state{ send_credits = Credits - 1 },
    case proplists:is_defined(complete, Options) of
        false -> {reply, ok, NewState};
        true  -> {stop, completed, ok, NewState}
    end.


handle_cast(send_cancel, State) ->
    #state{ id = StreamID, connection = Connection } = State,
    rsocket_connection:send_cancel(Connection, StreamID),
    {noreply, State};

handle_cast({send_error, ErrorType, ErrorData}, State) ->
    #state{ id = StreamID, connection = Connection } = State,
    rsocket_connection:send_error(Connection, StreamID, ErrorType, ErrorData),
    {stop, {error, ErrorType, ErrorData}, State};

handle_cast({send_request_n, N}, State) ->
    #state{
       id = StreamID,
       connection = Connection,
       recv_credits = Credits
      } = State,
    rsocket_connection:send_request_n(Connection, StreamID, N),
    NewState = State#state{ recv_credits = Credits + N },
    {noreply, NewState}.


handle_info({recv_payload, _P, _O}, S = #state{ recv_credits = 0 }) ->
    #state{ id = ID, connection = Connection } = S,
    Error = <<"REQUEST_N credits exceeded">>,
    rsocket_connection:send_error(Connection, ID, rejected, Error),
    {stop, {error, rejected, Error}, S};

handle_info({recv_payload, Payload, Options}, State) ->
    #state{
       module = Module,
       application_state = AppState,
       recv_credits = Credits
      } = State,
    Module:handle_payload(Payload, Options, AppState),
    NewState = State#state{ recv_credits = Credits - 1 },
    case proplists:is_defined(complete, Options) of
        false -> {noreply, NewState};
        true  -> {stop, completed, NewState}
    end;

handle_info({recv_request_n, N}, State) ->
    #state{
       module = Module,
       application_state = AppState,
       send_credits = Credits
      } = State,
    Module:handle_request_n(N, AppState),
    NewState = State#state{ send_credits = Credits + N },
    {noreply, NewState}.


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
