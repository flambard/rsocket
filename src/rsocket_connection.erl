-module(rsocket_connection).
-behaviour(gen_statem).

-include("rsocket_format.hrl").

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
    Setup = ?RSOCKET_SETUP(0, 2, 30000, 40000, <<>>),
    Frame = ?RSOCKET_FRAME_HEADER(0, ?FRAME_TYPE_SETUP, 0, 0, 0, Setup),
    ok = Mod:send_frame(Pid, Frame),
    {next_state, connected, Data}.


awaiting_setup(cast, close_connection, Data) ->
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Mod:close_connection(Pid),
    {stop, disconnect};

awaiting_setup(cast, {recv, Frame}, Data) ->
    ?RSOCKET_FRAME_HEADER(
       StreamID, FrameType, IgnoreFlag, MetadataFlag, OtherFlags, FramePayload
      ) = Frame,
    case {StreamID, FrameType} of
        {0, ?FRAME_TYPE_SETUP} ->
            {next_state, connected, Data};
        {0, ?FRAME_TYPE_RESUME} ->
            {next_state, connected, Data};
        _ ->
            %% TODO: send ERROR[INVALID_SETUP]
            {stop, invalid_setup}
    end;

awaiting_setup({call, Caller}, _Msg, Data) ->
    {next_state, awaiting_setup, Data, [{reply, Caller, ok}]}.


connected(cast, {recv, Frame}, Data) ->
    ?RSOCKET_FRAME_HEADER(
       StreamID, FrameType, IgnoreFlag, MetadataFlag, OtherFlags, FramePayload
      ) = Frame,
    case {StreamID, FrameType} of
        {_, ?FRAME_TYPE_REQUEST_FNF} ->
            case maps:find(fire_and_forget, Data#data.stream_handlers) of
                error ->
                    %% TODO: Send REJECT
                    {keep_state, Data};
                {ok, {Mod, Fun, Args}} ->
                    %% TODO: Not yet supported, send REJECT
                    {keep_state, Data};
                {ok, FnfHandler} when is_function(FnfHandler, 1) ->
                    proc_lib:spawn(fun() -> FnfHandler(FramePayload) end),
                    {keep_state, Data}
            end;
        {_, ?FRAME_TYPE_REQUEST_RESPONSE} ->
            case maps:find(request_response, Data#data.stream_handlers) of
                error ->
                    %% TODO: Send REJECT
                    {keep_state, Data};
                {ok, {Mod, Fun, Args}} ->
                    %% TODO: Not yet supported, send REJECT
                    {keep_state, Data};
                {ok, RRHandler} when is_function(RRHandler, 1) ->
                    Self = self(),
                    proc_lib:spawn(
                      fun() ->
                              Response = RRHandler(FramePayload),
                              ok = send_payload(Self, StreamID, Response)
                      end),
                    %% TODO: send back the response
                    {keep_state, Data}
            end;
        {_, ?FRAME_TYPE_PAYLOAD} ->
            case gproc:where({n, l, {rsocket_stream, self(), StreamID}}) of
                undefined -> ok;
                Stream    -> Stream ! {recv_payload, FramePayload}
            end,
            {keep_state, Data};
        _ ->
            {stop, unexpected_message}
    end;

connected(cast, {send_request_fnf, Message}, Data) ->
    ID = Data#data.next_stream_id,
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Fnf = ?RSOCKET_REQUEST_FNF(Message),
    %% TODO: Set up the frame header correctly
    Frame = ?RSOCKET_FRAME_HEADER(ID, ?FRAME_TYPE_REQUEST_FNF, 0, 0, 0, Fnf),
    ok = Mod:send_frame(Pid, Frame),
    {next_state, connected, Data#data{ next_stream_id = ID + 2 }};

connected(cast, {send_request_response, Request, Handler}, Data) ->
    ID = Data#data.next_stream_id,
    Self = self(),
    %% Start the handler process (with ID)
    proc_lib:spawn(fun() ->
                           true = gproc:reg({n, l, {rsocket_stream, Self, ID}}),
                           receive
                               {recv_payload, Payload} ->
                                   Handler({ok, Payload})
                           after 5000 ->
                                   Handler({error, timeout})
                           end
                   end),
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    RR = ?RSOCKET_REQUEST_RESPONSE(Request),
    %% TODO: Set up the frame header correctly
    Frame = ?RSOCKET_FRAME_HEADER(ID, ?FRAME_TYPE_REQUEST_RESPONSE, 0, 0, 0, RR),
    ok = Mod:send_frame(Pid, Frame),
    {next_state, connected, Data#data{ next_stream_id = ID + 2 }};

connected(cast, {send_payload, StreamID, Payload}, Data) ->
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    P = ?RSOCKET_PAYLOAD(Payload),
    %% TODO: Set up the frame header correctly
    Frame = ?RSOCKET_FRAME_HEADER(StreamID, ?FRAME_TYPE_PAYLOAD, 0, 0, 0, P),
    ok = Mod:send_frame(Pid, Frame),
    {next_state, connected, Data};

connected({call, Caller}, _Msg, Data) ->
    {next_state, connected, Data, [{reply, Caller, ok}]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
