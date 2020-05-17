-module(rsocket_stream).
-behaviour(gen_server).

%% API
-export([
         start_link_fire_and_forget/3,
         start_link_rr_requester/2,
         start_link_rr_responder/3,
         start_link_stream_requester/4,
         start_link_stream_responder/4,
         recv_payload/3,
         recv_request_n/2,
         send_cancel/1,
         send_error/3,
         send_payload/3,
         send_request_n/2,
         find/2
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
         recv_state,
         send_state,
         application_state
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link_fire_and_forget(StreamID, Request, Handler) ->
    Self = self(),
    proc_lib:spawn_link(
      fun() ->
              register_stream(Self, StreamID),
              Handler(Request)
      end).

start_link_rr_requester(StreamID, Handler) ->
    Self = self(),
    proc_lib:spawn_link(
      fun() ->
              register_stream(Self, StreamID),
              receive
                  {recv_payload, Payload, PayloadOptions} ->
                      Handler({ok, Payload, PayloadOptions})
              after 5000 ->
                      Handler({error, timeout})
              end
      end).

start_link_rr_responder(StreamID, Request, Handler) ->
    Self = self(),
    proc_lib:spawn_link(
      fun() ->
              register_stream(Self, StreamID),
              Options =
                  case Handler(Request) of
                      {reply, Response}     -> [complete, next];
                      {reply, Response, Os} -> [complete, next | Os]
                  end,
              rsocket_connection:send_payload(Self, StreamID, Response, Options)
      end).

start_link_stream_requester(StreamID, Request, Handler, N) ->
    Options = [{recv_credits, N},
               {recv_state, open},
               {send_state, completed}],
    Name = {via, gproc, {n, l, {rsocket_stream, self(), StreamID}}},
    gen_server:start_link(
      Name, ?MODULE, [StreamID, self(), Request, Handler, Options], []).

start_link_stream_responder(StreamID, Request, Handler, N) ->
    Options = [{recv_state, completed},
               {send_state, open}],
    Name = {via, gproc, {n, l, {rsocket_stream, self(), StreamID}}},
    {ok, Pid} =
        gen_server:start_link(
          Name, ?MODULE, [StreamID, self(), Request, Handler, Options], []),
    recv_request_n(Pid, N),
    {ok, Pid}.


recv_payload(Stream, Payload, Options) ->
    Stream ! {recv_payload, Payload, Options}.

recv_request_n(Stream, N) ->
    Stream ! {recv_request_n, N}.

send_cancel(Stream) ->
    gen_server:cast(Stream, send_cancel).

send_error(Stream, ErrorType, ErrorData) ->
    gen_server:cast(Stream, {send_error, ErrorType, ErrorData}).

send_payload(Stream, Payload, Options) ->
    gen_server:call(Stream, {send_payload, Payload, Options}).

send_request_n(Stream, N) ->
    gen_server:call(Stream, {send_request_n, N}).

find(RSocket, StreamID) ->
    gproc:where({n, l, {rsocket_stream, RSocket, StreamID}}).


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
            recv_state = proplists:get_value(recv_state, Options),
            send_state = proplists:get_value(send_state, Options),
            application_state = AppState
           }}.


handle_call({send_request_n, _N}, _From, S = #state{ recv_state = completed }) ->
    {reply, {error, stream_completed}, S};
handle_call({send_request_n, N}, _From, State) ->
    #state{
       id = StreamID,
       connection = Connection,
       recv_credits = Credits
      } = State,
    rsocket_connection:send_request_n(Connection, StreamID, N),
    NewState = State#state{ recv_credits = Credits + N },
    {reply, ok, NewState};

handle_call({send_payload, _P, _O}, _From, S = #state{ send_credits = 0 }) ->
    {reply, {error, no_credits}, S};
handle_call({send_payload, _P, _O}, _F, S = #state{ send_state = completed }) ->
    {reply, {error, stream_completed}, S};
handle_call({send_payload, Payload, Options}, _From, State) ->
    #state{
       id = StreamID,
       connection = Connection,
       send_credits = Credits,
       recv_state = RecvState
      } = State,
    rsocket_connection:send_payload(Connection, StreamID, Payload, Options),
    NewState = State#state{ send_credits = Credits - 1 },
    case proplists:is_defined(complete, Options) of
        false -> {reply, ok, NewState};
        true  ->
            CompletedState = NewState#state{ send_state = completed },
            case RecvState of
                completed -> {stop, completed, ok, CompletedState};
                open      -> {reply, ok, CompletedState}
            end
    end.


handle_cast(send_cancel, State) ->
    #state{ id = StreamID, connection = Connection } = State,
    rsocket_connection:send_cancel(Connection, StreamID),
    {noreply, State};

handle_cast({send_error, ErrorType, ErrorData}, State) ->
    #state{ id = StreamID, connection = Connection } = State,
    rsocket_connection:send_error(Connection, StreamID, ErrorType, ErrorData),
    {stop, {error, ErrorType, ErrorData}, State}.


handle_info({recv_payload, _P, _O}, S = #state{ recv_state = completed }) ->
    %% ignore
    {noreply, S};
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

handle_info({recv_request_n, _N}, S = #state{ send_state = completed }) ->
    %% ignored
    {noreply, S};
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

register_stream(RSocket, StreamID) ->
    true = gproc:reg({n, l, {rsocket_stream, RSocket, StreamID}}).
