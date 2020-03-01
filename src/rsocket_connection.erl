-module(rsocket_connection).
-behaviour(gen_statem).

-include("rsocket_format.hrl").

%% API
-export([
         start_link/3,
         recv_frame/2,
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
         awaiting_setup/3
        ]).


-record(data,
        {
         transport_pid,
         transport_mod
        }).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Mode :: accept | initiate,
                 Module :: atom(),
                 Transport :: pid()) ->
          {ok, Pid :: pid()} |
          ignore |
          {error, Error :: term()}.
start_link(Mode, Module, Transport) ->
    gen_statem:start_link(?MODULE, [Mode, Module, Transport], []).

recv_frame(Server, Frame) ->
    gen_statem:cast(Server, {recv, Frame}).

close(Server) ->
    gen_statem:cast(Server, close_connection).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.


-spec init(Args :: term()) -> gen_statem:init_result(atom()).
init([accept, Module, Transport]) ->
    Data = #data{
              transport_mod = Module,
              transport_pid = Transport
             },
    {ok, awaiting_setup, Data};

init([initiate, Module, Transport]) ->
    Data = #data{
              transport_mod = Module,
              transport_pid = Transport
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
    Mod:send_frame(Pid, Frame),
    {next_state, setup_connection, Data};

setup_connection(cast, {recv, Frame}, Data) ->
    ?RSOCKET_FRAME_HEADER(
       StreamID, FrameType, IgnoreFlag, MetadataFlag, OtherFlags, FramePayload
      ) = Frame,
    case {StreamID, FrameType} of
        {0, ?FRAME_TYPE_ERROR} ->
            {stop, invalid_setup};
        {0, ?FRAME_TYPE_LEASE} ->
            %% TODO: set up a lease
            {next_state, connected, Data};
        {0, ?FRAME_TYPE_REQUEST_RESPONSE} ->
            %% TODO: start a single-response request
            {next_state, connected, Data};
        {0, ?FRAME_TYPE_REQUEST_FNF} ->
            %% TODO: start a fire-and-forget request
            {next_state, connected, Data};
        {0, ?FRAME_TYPE_REQUEST_STREAM} ->
            %% TODO: start a stream
            {next_state, connected, Data};
        {0, ?FRAME_TYPE_REQUEST_CHANNEL} ->
            %% TODO: start a channel
            {next_state, connected, Data};
        {0, ?FRAME_TYPE_REQUEST_N} ->
            %% TODO: top-up the request credit
            {next_state, connected, Data};
        _ ->
            %% TODO: send ERROR[INVALID_SETUP]
            {stop, invalid_setup}
    end.

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


%%%===================================================================
%%% Internal functions
%%%===================================================================
