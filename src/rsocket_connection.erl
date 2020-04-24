-module(rsocket_connection).
-behaviour(gen_statem).

%% API
-export([
         start_link/4,
         recv_frame/2,
         send_request_fnf/2,
         send_request_response/3,
         send_payload/3,
         close/1
        ]).

%% gen_statem callbacks
-export([
         callback_mode/0,
         init/1,
         terminate/3,
         code_change/4
        ]).

%% gen_statem states
-export([
         setup_connection/3,
         awaiting_setup/3,
         connected/3
        ]).


-record(data,
        {
         transport_pid,
         transport_mod,
         stream_handlers,
         next_stream_id
        }).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Mode :: accept | initiate,
                 Module :: atom(),
                 Transport :: pid(),
                 Handlers :: map()) ->
          {ok, Pid :: pid()} |
          ignore |
          {error, Error :: term()}.
start_link(Mode, Module, Transport, Handlers) ->
    gen_statem:start_link(?MODULE, [Mode, Module, Transport, Handlers], []).

recv_frame(Server, Frame) ->
    gen_statem:cast(Server, {recv, Frame}).

send_request_fnf(Server, Message) ->
    gen_statem:cast(Server, {send_request_fnf, Message}).

send_request_response(Server, Request, Handler) ->
    gen_statem:cast(Server, {send_request_response, Request, Handler}).

send_payload(Server, StreamID, Payload) ->
    gen_statem:cast(Server, {send_payload, StreamID, Payload}).

close(Server) ->
    gen_statem:cast(Server, close_connection).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.


-spec init(Args :: term()) -> gen_statem:init_result(atom()).
init([accept, Module, Transport, Handlers]) ->
    Data = #data{
              transport_mod = Module,
              transport_pid = Transport,
              stream_handlers = Handlers,
              next_stream_id = 2
             },
    {ok, awaiting_setup, Data};

init([initiate, Module, Transport, Handlers]) ->
    Data = #data{
              transport_mod = Module,
              transport_pid = Transport,
              stream_handlers = Handlers,
              next_stream_id = 1
             },
    gen_statem:cast(self(), send_setup),
    {ok, setup_connection, Data}.


-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
          any().
terminate(_Reason, _State, _Data) ->
    void.


-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
          {ok, NewState :: term(), NewData :: term()} |
          (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%%%===================================================================
%%% States
%%%===================================================================

setup_connection(cast, send_setup, Data) ->
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Frame = rsocket_frame:new_setup(),
    ok = Mod:send_frame(Pid, Frame),
    {next_state, connected, Data}.


awaiting_setup(cast, {recv, Frame}, Data) ->
    case rsocket_frame:parse(Frame) of
        {ok, {setup, 0}} ->
            {next_state, connected, Data};
        _ ->
            #data{ transport_pid = Pid, transport_mod = Mod } = Data,
            Frame = rsocket_frame:new_error(0, invalid_setup),
            ok = Mod:send_frame(Pid, Frame),
            {stop, invalid_setup}
    end.


connected(cast, {recv, Frame}, Data) ->
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    case rsocket_frame:parse(Frame) of
        {ok, {request_fnf, StreamID, Message}} when StreamID =/= 0 ->
            case maps:find(fire_and_forget, Data#data.stream_handlers) of
                error ->
                    Error = <<"No fire-and-forget handler">>,
                    Frame = rsocket_frame:new_error(StreamID, reject, Error),
                    ok = Mod:send_frame(Pid, Frame),
                    {keep_state, Data};
                {ok, {_Mod, _Fun, _Args}} ->
                    Error = <<"MFA tuples not yet supported">>,
                    Frame = rsocket_frame:new_error(StreamID, reject, Error),
                    ok = Mod:send_frame(Pid, Frame),
                    {keep_state, Data};
                {ok, FnfHandler} when is_function(FnfHandler, 1) ->
                    proc_lib:spawn(fun() -> FnfHandler(Message) end),
                    {keep_state, Data}
            end;
        {ok, {request_response, StreamID, Request}} when StreamID =/= 0 ->
            case maps:find(request_response, Data#data.stream_handlers) of
                error ->
                    Error = <<"No request-response handler">>,
                    Frame = rsocket_frame:new_error(StreamID, reject, Error),
                    ok = Mod:send_frame(Pid, Frame),
                    {keep_state, Data};
                {ok, {_Mod, _Fun, _Args}} ->
                    Error = <<"MFA tuples not yet supported">>,
                    Frame = rsocket_frame:new_error(StreamID, reject, Error),
                    ok = Mod:send_frame(Pid, Frame),
                    {keep_state, Data};
                {ok, RRHandler} when is_function(RRHandler, 1) ->
                    Self = self(),
                    proc_lib:spawn(
                      fun() ->
                              Response = RRHandler(Request),
                              ok = send_payload(Self, StreamID, Response)
                      end),
                    {keep_state, Data}
            end;
        {ok, {payload, StreamID, Payload}} when StreamID =/= 0 ->
            case find_stream(self(), StreamID) of
                undefined -> ok;
                Stream    -> Stream ! {recv_payload, Payload}
            end,
            {keep_state, Data};
        _ ->
            {stop, unexpected_message}
    end;

connected(cast, {send_request_fnf, Message}, Data) ->
    ID = Data#data.next_stream_id,
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Frame = rsocket_frame:new_request_fnf(ID, Message),
    ok = Mod:send_frame(Pid, Frame),
    {keep_state, Data#data{ next_stream_id = ID + 2 }};

connected(cast, {send_request_response, Request, Handler}, Data) ->
    StreamID = Data#data.next_stream_id,
    Self = self(),
    proc_lib:spawn(fun() ->
                           register_stream(Self, StreamID),
                           receive
                               {recv_payload, Payload} ->
                                   Handler({ok, Payload})
                           after 5000 ->
                                   Handler({error, timeout})
                           end
                   end),
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Frame = rsocket_frame:new_request_response(StreamID, Request),
    ok = Mod:send_frame(Pid, Frame),
    {keep_state, Data#data{ next_stream_id = StreamID + 2 }};

connected(cast, {send_payload, StreamID, Payload}, Data) ->
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Frame = rsocket_frame:new_payload(StreamID, Payload),
    ok = Mod:send_frame(Pid, Frame),
    {keep_state, Data};

connected(cast, close_connection, Data) ->
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Mod:close_connection(Pid),
    {stop, disconnect};

connected({call, Caller}, _Msg, Data) ->
    {keep_state, Data, [{reply, Caller, ok}]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

register_stream(RSocket, StreamID) ->
    true = gproc:reg({n, l, {rsocket_stream, RSocket, StreamID}}).

find_stream(RSocket, StreamID) ->
    gproc:where({n, l, {rsocket_stream, RSocket, StreamID}}).
