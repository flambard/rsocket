-module(rsocket_connection).
-behaviour(gen_statem).

-include("rsocket_format.hrl").

%% API
-export([
         start_link/5,
         recv_frame/2,
         send_error/4,
         send_keepalive/1,
         send_lease/4,
         send_metadata_push/2,
         send_request_fnf/3,
         send_request_response/4,
         send_request_stream/4,
         send_request_n/3,
         send_payload/4,
         send_cancel/2,
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
         at_connect,
         transport_pid,
         transport_mod,
         stream_handlers,
         next_stream_id,
         keepalive_interval,
         max_lifetime,
         keepalive_response_timer,
         metadata_mime_type,
         data_mime_type,
         use_leasing = false,
         send_lease_tracker,
         recv_lease_tracker
        }).


-define(CLIENT_INITIAL_STREAM_ID, 1).
-define(SERVER_INITIAL_STREAM_ID, 2).

-define(OPTION_DEFAULTS,
        #{
          at_connect => fun(_RSocket) -> ok end,
          keepalive_interval => 3000,
          max_lifetime => 4000,
          leasing => false,
          %% TODO: Are there reasonable defaults for MIME types?
          metadata_mime_type => <<"application/json">>,
          data_mime_type => <<"application/json">>
         }
       ).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Mode :: server | client,
                 Module :: atom(),
                 Transport :: pid(),
                 Handlers :: map(),
                 Options :: map()) ->
          {ok, Pid :: pid()} |
          ignore |
          {error, Error :: term()}.
start_link(Mode, Module, Transport, Handlers, Options) ->
    AllOptions = maps:merge(?OPTION_DEFAULTS, Options),
    gen_statem:start_link(
      ?MODULE, [Mode, Module, Transport, Handlers, AllOptions], []).

recv_frame(Server, Frame) ->
    gen_statem:cast(Server, {recv, Frame}).

send_error(Server, StreamID, ErrorType, ErrorData) ->
    gen_statem:cast(Server, {send_error, StreamID, ErrorType, ErrorData}).

send_keepalive(Server) ->
    gen_statem:cast(Server, send_keepalive).

send_lease(Server, TimeToLive, NumberOfRequests, Options) ->
    gen_statem:cast(
      Server, {send_lease, TimeToLive, NumberOfRequests, Options}).

send_metadata_push(Server, Metadata) ->
    gen_statem:cast(Server, {send_metadata_push, Metadata}).

send_request_fnf(Server, Message, Options) ->
    gen_statem:call(Server, {send_request_fnf, Message, Options}).

send_request_response(Server, Request, Handler, Options) ->
    gen_statem:call(Server, {send_request_response, Request, Handler, Options}).

send_request_stream(Server, N, Request, Options) ->
    gen_statem:call(Server, {send_request_stream, N, Request, Options}).

send_request_n(Server, StreamID, N) ->
    gen_statem:cast(Server, {send_request_n, StreamID, N}).

send_payload(Server, StreamID, Payload, Options) ->
    gen_statem:cast(Server, {send_payload, StreamID, Payload, Options}).

send_cancel(Server, StreamID) ->
    gen_statem:cast(Server, {send_cancel, StreamID}).

close(Server) ->
    gen_statem:cast(Server, close_connection).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> [state_functions, state_enter].


-spec init(Args :: term()) -> gen_statem:init_result(atom()).
init([server, Module, Transport, Handlers, Options]) ->
    process_flag(trap_exit, true),
    #{ at_connect := AtConnect } = Options,
    Data = #data{
              at_connect = AtConnect,
              transport_mod = Module,
              transport_pid = Transport,
              stream_handlers = Handlers,
              next_stream_id = ?SERVER_INITIAL_STREAM_ID
             },
    {ok, awaiting_setup, Data};

init([client, Module, Transport, Handlers, Options]) ->
    process_flag(trap_exit, true),
    #{ at_connect := AtConnect,
       keepalive_interval := KeepaliveInterval,
       max_lifetime := MaxLifetime,
       metadata_mime_type := MetadataMimeType,
       data_mime_type := DataMimeType,
       leasing := UseLeasing
     } = Options,
    Data0 = #data{
               at_connect = AtConnect,
               transport_mod = Module,
               transport_pid = Transport,
               stream_handlers = Handlers,
               next_stream_id = ?CLIENT_INITIAL_STREAM_ID,
               keepalive_interval = KeepaliveInterval,
               max_lifetime = MaxLifetime,
               metadata_mime_type = MetadataMimeType,
               data_mime_type = DataMimeType,
               use_leasing = UseLeasing
              },
    Data1 = case UseLeasing of
                false -> Data0;
                true  ->
                    Data0#data{
                      send_lease_tracker = rsocket_lease_tracker:new(),
                      recv_lease_tracker = rsocket_lease_tracker:new()
                     }
            end,
    {ok, setup_connection, Data1}.


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

%%%
%%% Setup Connection (client side)
%%%

setup_connection(enter, _, Data) ->
    gen_statem:cast(self(), send_setup),
    {keep_state, Data};

setup_connection(cast, send_setup, Data) ->
    #data{
       keepalive_interval = Interval,
       max_lifetime = MaxLifetime,
       metadata_mime_type = MetadataMimeType,
       data_mime_type = DataMimeType,
       use_leasing = UseLeasing
      } = Data,
    SetupOptions = case UseLeasing of
                       false -> [];
                       true  -> [leasing]
                   end,
    Frame = rsocket_frame:new_setup(Interval, MaxLifetime,
                                    MetadataMimeType, DataMimeType,
                                    SetupOptions),
    transport_frame(Frame, Data),
    {ok, _TRef} =
        timer:apply_interval(Interval, ?MODULE, send_keepalive, [self()]),
    {next_state, connected, Data}.


%%%
%%% Awaiting Setup (server side)
%%%

awaiting_setup(enter, _, Data) ->
    {keep_state, Data};

awaiting_setup(cast, {recv, ReceivedFrame}, Data) ->
    {FrameType, StreamID, Flags, FrameData} =
        rsocket_frame:parse(ReceivedFrame),
    case FrameType of
        setup when StreamID =:= 0 ->
            handle_setup(Flags, FrameData, Data);
        _ ->
            Frame = rsocket_frame:new_error(0, invalid_setup),
            transport_frame(Frame, Data),
            {stop, invalid_setup}
    end;

awaiting_setup(cast, _, Data) ->
    {keep_state, Data, [postpone]}.


%%%
%%% Connected (connection established)
%%%

connected(enter, _, Data) ->
    #data{ at_connect = AtConnect } = Data,
    AtConnect(self()),
    {keep_state, Data};

connected(cast, {recv, ReceivedFrame}, Data) ->
    #data{
       use_leasing = UseLeasing,
       recv_lease_tracker = RecvLeaseTracker
      } = Data,
    {FrameType, StreamID, Flags, FrameData} =
        rsocket_frame:parse(ReceivedFrame),
    case FrameType of
        keepalive ->
            handle_keepalive(Flags, Data);
        metadata_push when StreamID =:= 0 ->
            handle_metadata_push(FrameData, Data);
        request_fnf when StreamID =/= 0 andalso not UseLeasing ->
            handle_request_fnf(Flags, StreamID, FrameData, Data);
        request_fnf when StreamID =/= 0 andalso UseLeasing ->
            case rsocket_lease_tracker:spend_1(RecvLeaseTracker) of
                0 ->
                    Frame = rsocket_frame:new_error(StreamID, rejected),
                    transport_frame(Frame, Data),
                    {keep_state, Data};
                _ ->
                    handle_request_fnf(Flags, StreamID, FrameData, Data)
            end;
        request_response when StreamID =/= 0 andalso not UseLeasing ->
            handle_request_response(Flags, StreamID, FrameData, Data);
        request_response when StreamID =/= 0 andalso UseLeasing ->
            case rsocket_lease_tracker:spend_1(RecvLeaseTracker) of
                0 ->
                    Frame = rsocket_frame:new_error(StreamID, rejected),
                    transport_frame(Frame, Data),
                    {keep_state, Data};
                _ ->
                    handle_request_response(Flags, StreamID, FrameData, Data)
            end;
        request_stream when StreamID =/= 0 andalso not UseLeasing ->
            handle_request_stream(Flags, StreamID, FrameData, Data);
        request_stream when StreamID =/= 0 andalso UseLeasing ->
            case rsocket_lease_tracker:spend_1(RecvLeaseTracker) of
                0 ->
                    Frame = rsocket_frame:new_error(StreamID, rejected),
                    transport_frame(Frame, Data),
                    {keep_state, Data};
                _ ->
                    handle_request_stream(Flags, StreamID, FrameData, Data)
            end;
        request_n when StreamID =/= 0 ->
            handle_request_n(Flags, StreamID, FrameData, Data);
        payload when StreamID =/= 0 ->
            handle_payload(Flags, StreamID, FrameData, Data);
        lease when StreamID =:= 0 ->
            handle_lease(Flags, FrameData, Data);
        cancel when StreamID =/= 0 ->
            handle_cancel(StreamID, Data);
        error ->
            handle_error(Flags, StreamID, FrameData, Data);
        _ ->
            {stop, unexpected_message}
    end;

connected(cast, {send_error, StreamID, ErrorType, ErrorData}, Data) ->
    Frame = rsocket_frame:new_error(StreamID, ErrorType, ErrorData),
    transport_frame(Frame, Data),
    {keep_state, Data};

connected(cast, {send_lease, Time, Count, Options}, Data) ->
    #data{
       use_leasing = UseLeasing,
       recv_lease_tracker = LeaseTracker
      } = Data,
    case UseLeasing of
        false -> ok;
        true  ->
            L = rsocket_frame:new_lease(Time, Count, Options),
            transport_frame(L, Data),
            ok = rsocket_lease_tracker:start_lease(LeaseTracker, Time, Count)
    end,
    {keep_state, Data};

connected(cast, {send_metadata_push, Metadata}, Data) ->
    Frame = rsocket_frame:new_metadata_push(Metadata),
    transport_frame(Frame, Data),
    {keep_state, Data};

connected({call, From}, {send_request_fnf, Message, Options}, Data) ->
    #data{
       next_stream_id = StreamID,
       use_leasing = UseLeasing,
       send_lease_tracker = LeaseTracker
      } = Data,
    N = case UseLeasing of
            false -> 1;
            true  -> rsocket_lease_tracker:spend_1(LeaseTracker)
        end,
    case N of
        0 ->
            {keep_state, Data, [{reply, From, {error, lease_expired}}]};
        _ ->
            Frame = rsocket_frame:new_request_fnf(StreamID, Message, Options),
            transport_frame(Frame, Data),
            NewData = Data#data{ next_stream_id = StreamID + 2 },
            {keep_state, NewData, [{reply, From, ok}]}
    end;

connected({call, F}, {send_request_response, Request, Handler, Opts}, Data) ->
    #data{
       next_stream_id = StreamID,
       use_leasing = UseLeasing,
       send_lease_tracker = LeaseTracker
      } = Data,
    N = case UseLeasing of
            false -> 1;
            true  -> rsocket_lease_tracker:spend_1(LeaseTracker)
        end,
    case N of
        0 ->
            {keep_state, Data, [{reply, F, {error, lease_expired}}]};
        _ ->
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
              end),
            RR = rsocket_frame:new_request_response(StreamID, Request, Opts),
            transport_frame(RR, Data),
            NewData = Data#data{ next_stream_id = StreamID + 2 },
            {keep_state, NewData, [{reply, F, {ok, StreamID}}]}
    end;

connected({call, F}, {send_request_stream, N, Request, Options}, Data) ->
    #data{
       stream_handlers = StreamHandlers,
       next_stream_id = ID,
       use_leasing = UseLeasing,
       send_lease_tracker = LeaseTracker
      } = Data,
    Credit = case UseLeasing of
                 false -> 1;
                 true  -> rsocket_lease_tracker:spend_1(LeaseTracker)
             end,
    case {Credit, maps:find(stream_requester, StreamHandlers)} of
        {0, _} ->
            {keep_state, Data, [{reply, F, {error, lease_expired}}]};
        {_, error} ->
            Reply = {error, no_stream_requester_handler},
            {keep_state, Data, [{reply, F, Reply}]};
        {_, {ok, {_Module, _InitData} = Handler}} ->
            Map = case proplists:lookup(metadata, Options) of
                      none ->
                          #{ request => Request };
                      {metadata, Metadata} ->
                          #{ request => Request, metadata => Metadata }
                  end,
            {ok, _Pid} = rsocket_stream:start_link(
                           ID, Map, Handler, [{recv_credits, N}]),
            RS = rsocket_frame:new_request_stream(ID, N, Request, Options),
            transport_frame(RS, Data),
            NewData = Data#data{ next_stream_id = ID + 2 },
            {keep_state, NewData, [{reply, F, {ok, ID}}]}
    end;

connected(cast, {send_request_n, StreamID, N}, Data) ->
    Frame = rsocket_frame:new_request_n(StreamID, N),
    transport_frame(Frame, Data),
    {keep_state, Data};

connected(cast, {send_payload, StreamID, Payload, Options}, Data) ->
    Frame = rsocket_frame:new_payload(StreamID, Payload, Options),
    transport_frame(Frame, Data),
    {keep_state, Data};

connected(cast, send_keepalive, Data) ->
    #data{ max_lifetime = MaxLifetime } = Data,
    Frame = rsocket_frame:new_keepalive([respond]),
    transport_frame(Frame, Data),
    {ok, TRef} = timer:send_after(MaxLifetime, keepalive_timeout),
    {keep_state, Data#data{ keepalive_response_timer = TRef }};

connected(cast, {send_cancel, StreamID}, Data) ->
    case find_stream(self(), StreamID) of
        undefined -> ok;
        Stream    -> exit(Stream, canceled)
    end,
    Frame = rsocket_frame:new_cancel(StreamID),
    transport_frame(Frame, Data),
    {keep_state, Data};

connected(info, keepalive_timeout, Data) ->
    %% TODO: Determine what the protocol is actually supposed to do
    %% when the other end does not respond to KEEPALIVEs
    {keep_state, Data};

connected(info, {'EXIT', _Stream, _Reason}, Data) ->
    %% A stream has terminated
    {keep_state, Data};

connected(cast, close_connection, Data) ->
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Mod:close_connection(Pid),
    {stop, disconnect}.


%%%===================================================================
%%% Frame reception handlers
%%%===================================================================

handle_setup(?SETUP_FLAGS(M, _R, L), FrameData, Data) ->
    ?SETUP(0, 2, KeepaliveInterval, MaxLifetime,
           _MDMTL, MetadataMimeType,
           _DMTL, DataMimeType,
           SetupData) = FrameData,
    _Map = case M of
               0 ->
                   #{ payload => SetupData };
               1 ->
                   ?METADATA(_Size, Metadata, Payload) = SetupData,
                   #{ payload => Payload, metadata => Metadata }
           end,
    %% TODO: What do we do with SETUP Data and Metadata?
    %% From the RSocket spec:
    %% "Setup Data: includes payload describing connection capabilities of the
    %% endpoint sending the Setup header."
    %%
    Data0 = Data#data{
              keepalive_interval = KeepaliveInterval,
              max_lifetime = MaxLifetime,
              metadata_mime_type = MetadataMimeType,
              data_mime_type = DataMimeType,
              use_leasing = bit_to_bool(L)
             },
    Data1 = case L of
                0 -> Data0;
                1 ->
                    Data0#data{
                      send_lease_tracker = rsocket_lease_tracker:new(),
                      recv_lease_tracker = rsocket_lease_tracker:new()
                     }
            end,
    {next_state, connected, Data1}.


handle_keepalive(?KEEPALIVE_FLAGS(1), Data) ->
    Frame = rsocket_frame:new_keepalive([]),
    transport_frame(Frame, Data),
    {keep_state, Data};

handle_keepalive(?KEEPALIVE_FLAGS(0), Data) ->
    #data{ keepalive_response_timer = TRef } = Data,
    {ok, cancel} = timer:cancel(TRef),
    {keep_state, Data}.


handle_metadata_push(Metadata, Data) ->
    #data{ stream_handlers = StreamHandlers } = Data,
    case maps:find(metadata_push, StreamHandlers) of
        error ->
            Error = <<"No metadata-push handler">>,
            Frame = rsocket_frame:new_error(0, reject, Error),
            transport_frame(Frame, Data);
        {ok, MetadataPushHandler} when is_function(MetadataPushHandler, 1) ->
            proc_lib:spawn_link(fun() -> MetadataPushHandler(Metadata) end)
    end,
    {keep_state, Data}.


handle_request_fnf(?REQUEST_FNF_FLAGS(M, _F), StreamID, FrameData, Data) ->
    #data{ stream_handlers = StreamHandlers } = Data,
    case maps:find(fire_and_forget, StreamHandlers) of
        error ->
            Error = <<"No fire-and-forget handler">>,
            Frame = rsocket_frame:new_error(StreamID, reject, Error),
            transport_frame(Frame, Data),
            {keep_state, Data};
        {ok, {_Mod, _Fun, _Args}} ->
            Error = <<"MFA tuples not yet supported">>,
            Frame = rsocket_frame:new_error(StreamID, reject, Error),
            transport_frame(Frame, Data),
            {keep_state, Data};
        {ok, FnfHandler} when is_function(FnfHandler, 1) ->
            Map =
                case M of
                    0 ->
                        #{ request => FrameData };
                    1 ->
                        ?METADATA(_Size, Metadata, Request) = FrameData,
                        #{ request => Request, metadata => Metadata }
                end,
            Self = self(),
            proc_lib:spawn_link(fun() ->
                                        register_stream(Self, StreamID),
                                        FnfHandler(Map)
                                end),
            {keep_state, Data}
    end.


handle_request_response(?REQUEST_RESPONSE_FLAGS(M, _F), ID, FrameData, Data) ->
    #data{ stream_handlers = StreamHandlers } = Data,
    case maps:find(request_response, StreamHandlers) of
        error ->
            Error = <<"No request-response handler">>,
            Frame = rsocket_frame:new_error(ID, reject, Error),
            transport_frame(Frame, Data),
            {keep_state, Data};
        {ok, {_Mod, _Fun, _Args}} ->
            Error = <<"MFA tuples not yet supported">>,
            Frame = rsocket_frame:new_error(ID, reject, Error),
            transport_frame(Frame, Data),
            {keep_state, Data};
        {ok, RRHandler} when is_function(RRHandler, 1) ->
            Map =
                case M of
                    0 ->
                        #{ request => FrameData };
                    1 ->
                        ?METADATA(_Size, Metadata, Request) = FrameData,
                        #{ request => Request, metadata => Metadata }
                end,
            Self = self(),
            proc_lib:spawn_link(
              fun() ->
                      register_stream(Self, ID),
                      PayloadOptions =
                          case RRHandler(Map) of
                              {reply, Response}     -> [complete, next];
                              {reply, Response, Os} -> [complete, next | Os]
                          end,
                      send_payload(Self, ID, Response, PayloadOptions)
              end),
            {keep_state, Data}
    end.

handle_request_stream(?REQUEST_STREAM_FLAGS(M, _F), ID, FrameData, Data) ->
    #data{ stream_handlers = StreamHandlers } = Data,
    case maps:find(stream_responder, StreamHandlers) of
        error ->
            Error = <<"No request_stream handler">>,
            Frame = rsocket_frame:new_error(ID, reject, Error),
            transport_frame(Frame, Data),
            {keep_state, Data};
        {ok, {_Mod, _Fun, _Args}} ->
            Error = <<"MFA tuples not yet supported">>,
            Frame = rsocket_frame:new_error(ID, reject, Error),
            transport_frame(Frame, Data),
            {keep_state, Data};
        {ok, {_Module, _InitData} = Handler} ->
            ?REQUEST_STREAM(N, RequestData) = FrameData,
            Map = case M of
                      0 ->
                          #{ request => RequestData };
                      1 ->
                          ?METADATA(_Size, Metadata, Request) = RequestData,
                          #{ request => Request, metadata => Metadata }
                  end,
            {ok, Stream} = rsocket_stream:start_link(ID, Map, Handler, []),
            Stream ! {recv_request_n, N},
            {keep_state, Data}
    end.


handle_request_n(?REQUEST_N_FLAGS, StreamID, FrameData, Data) ->
    ?REQUEST_N(N) = FrameData,
    Stream = find_stream(self(), StreamID),
    Stream ! {recv_request_n, N},
    {keep_state, Data}.


handle_payload(?PAYLOAD_FLAGS(M, F, C, N), StreamID, FrameData, Data) ->
    case find_stream(self(), StreamID) of
        undefined -> ok;
        Stream    ->
            Flags = lists:append([
                                  [follows  || F =:= 1],
                                  [complete || C =:= 1],
                                  [next     || N =:= 1]
                                 ]),
            case M of
                0 ->
                    Stream ! {recv_payload, FrameData, Flags};
                1 ->
                    ?METADATA(_Size, Metadata, Payload) = FrameData,
                    PayloadOptions = [{metadata, Metadata} | Flags],
                    Stream ! {recv_payload, Payload, PayloadOptions}
            end
    end,
    {keep_state, Data}.

handle_lease(?LEASE_FLAGS(_M), FrameData, Data) ->
    #data{
       use_leasing = true,
       send_lease_tracker = LeaseTracker
      } = Data,
    ?LEASE(Time, Count, _Metadata) = FrameData,
    %% TODO: What to do with the (possibly included) metadata?
    ok = rsocket_lease_tracker:start_lease(LeaseTracker, Time, Count),
    {keep_state, Data}.

handle_cancel(StreamID, Data) ->
    case find_stream(self(), StreamID) of
        undefined -> ok;
        Stream    -> exit(Stream, canceled)
    end,
    {keep_state, Data}.

handle_error(?ERROR_FLAGS, 0, FrameData, _Data) ->
    ?ERROR(ErrorCode, ErrorData) = FrameData,
    ErrorType = maps:get(ErrorCode, rsocket_frame:error_code_names()),
    {stop, {rsocket_error, ErrorType, ErrorData}};

handle_error(?ERROR_FLAGS, StreamID, FrameData, Data) ->
    ?ERROR(ErrorCode, ErrorData) = FrameData,
    ErrorType = maps:get(ErrorCode, rsocket_frame:error_code_names()),
    case find_stream(self(), StreamID) of
        undefined -> ok;
        Stream    -> exit(Stream, {rsocket_error, ErrorType, ErrorData})
    end,
    {keep_state, Data}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

transport_frame(Frame, #data{ transport_pid = Pid, transport_mod = Mod }) ->
    ok = Mod:send_frame(Pid, Frame).

register_stream(RSocket, StreamID) ->
    true = gproc:reg({n, l, {rsocket_stream, RSocket, StreamID}}).

find_stream(RSocket, StreamID) ->
    gproc:where({n, l, {rsocket_stream, RSocket, StreamID}}).

bit_to_bool(0) -> false;
bit_to_bool(1) -> true.
