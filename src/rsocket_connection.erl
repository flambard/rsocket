-module(rsocket_connection).
-behaviour(gen_statem).

%% API
-export([
         start_link/4,
         recv_frame/2,
         send_keepalive/1,
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
         next_stream_id,
         keepalive_interval,
         max_lifetime,
         keepalive_response_timer
        }).

-define(CLIENT_INITIAL_STREAM_ID, 1).
-define(SERVER_INITIAL_STREAM_ID, 2).

-define(OPTION_DEFAULTS,
        #{
          keepalive_interval => 3000,
          max_lifetime => 4000
         }
       ).


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
    Options = #{}, %% TODO: Take options as parameter
    AllOptions = maps:merge(?OPTION_DEFAULTS, Options),
    gen_statem:start_link(
      ?MODULE, [Mode, Module, Transport, Handlers, AllOptions], []).

recv_frame(Server, Frame) ->
    gen_statem:cast(Server, {recv, Frame}).

send_keepalive(Server) ->
    gen_statem:cast(Server, send_keepalive).

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
init([accept, Module, Transport, Handlers, _Options]) ->
    Data = #data{
              transport_mod = Module,
              transport_pid = Transport,
              stream_handlers = Handlers,
              next_stream_id = ?SERVER_INITIAL_STREAM_ID
             },
    {ok, awaiting_setup, Data};

init([initiate, Module, Transport, Handlers, Options]) ->
    #{ keepalive_interval := KeepaliveInterval,
       max_lifetime := MaxLifetime
     } = Options,
    Data = #data{
              transport_mod = Module,
              transport_pid = Transport,
              stream_handlers = Handlers,
              next_stream_id = ?CLIENT_INITIAL_STREAM_ID,
              keepalive_interval = KeepaliveInterval,
              max_lifetime = MaxLifetime
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
    #data{
       transport_pid = Pid,
       transport_mod = Mod,
       keepalive_interval = Interval,
       max_lifetime = MaxLifetime
      } = Data,
    Frame = rsocket_frame:new_setup(Interval, MaxLifetime),
    ok = Mod:send_frame(Pid, Frame),
    {ok, _TRef} =
        timer:apply_interval(Interval, ?MODULE, send_keepalive, [self()]),
    {next_state, connected, Data}.


awaiting_setup(cast, {recv, Frame}, Data) ->
    case rsocket_frame:parse(Frame) of
        {ok, {setup, 0, KeepaliveInterval, MaxLifetime}} ->
            NewData = Data#data{
                        keepalive_interval = KeepaliveInterval,
                        max_lifetime = MaxLifetime
                       },
            {next_state, connected, NewData};
        _ ->
            #data{ transport_pid = Pid, transport_mod = Mod } = Data,
            Frame = rsocket_frame:new_error(0, invalid_setup),
            ok = Mod:send_frame(Pid, Frame),
            {stop, invalid_setup}
    end;

awaiting_setup(cast, _, Data) ->
    {keep_state, Data, [postpone]}.


connected(cast, {recv, ReceivedFrame}, Data) ->
    #data{
       transport_pid = Pid,
       transport_mod = Mod,
       stream_handlers = StreamHandlers,
       keepalive_response_timer = TRef
      } = Data,
    case rsocket_frame:parse(ReceivedFrame) of
        {ok, {keepalive, Respond}} ->
            case Respond of
                false ->
                    {ok, cancel} = timer:cancel(TRef);
                true ->
                    Frame = rsocket_frame:new_keepalive([]),
                    ok = Mod:send_frame(Pid, Frame)
            end,
            {keep_state, Data};
        {ok, {request_fnf, StreamID, Message}} when StreamID =/= 0 ->
            case maps:find(fire_and_forget, StreamHandlers) of
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
            case maps:find(request_response, StreamHandlers) of
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
    #data{
       transport_pid = Pid,
       transport_mod = Mod,
       next_stream_id = ID
      } = Data,
    Frame = rsocket_frame:new_request_fnf(ID, Message),
    ok = Mod:send_frame(Pid, Frame),
    {keep_state, Data#data{ next_stream_id = ID + 2 }};

connected(cast, {send_request_response, Request, Handler}, Data) ->
    #data{
       transport_pid = Pid,
       transport_mod = Mod,
       next_stream_id = StreamID
      } = Data,
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
    Frame = rsocket_frame:new_request_response(StreamID, Request),
    ok = Mod:send_frame(Pid, Frame),
    {keep_state, Data#data{ next_stream_id = StreamID + 2 }};

connected(cast, {send_payload, StreamID, Payload}, Data) ->
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Frame = rsocket_frame:new_payload(StreamID, Payload),
    ok = Mod:send_frame(Pid, Frame),
    {keep_state, Data};

connected(cast, send_keepalive, Data) ->
    #data{
       transport_pid = Pid,
       transport_mod = Mod,
       max_lifetime = MaxLifetime
      } = Data,
    Frame = rsocket_frame:new_keepalive([respond]),
    ok = Mod:send_frame(Pid, Frame),
    {ok, TRef} = timer:send_after(MaxLifetime, keepalive_timeout),
    {keep_state, Data#data{ keepalive_response_timer = TRef }};

connected(info, keepalive_timeout, Data) ->
    %% TODO: Determine what the protocol is actually supposed to do
    %% when the other end does not respond to KEEPALIVEs
    {keep_state, Data};

connected(cast, close_connection, Data) ->
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Mod:close_connection(Pid),
    {stop, disconnect}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

register_stream(RSocket, StreamID) ->
    true = gproc:reg({n, l, {rsocket_stream, RSocket, StreamID}}).

find_stream(RSocket, StreamID) ->
    gproc:where({n, l, {rsocket_stream, RSocket, StreamID}}).
