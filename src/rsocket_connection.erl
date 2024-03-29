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
    send_request_channel/4,
    send_request_fnf/3,
    send_request_response/4,
    send_request_stream/4,
    send_request_n/3,
    send_payload/4,
    send_cancel/2,
    close/1
]).
%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
%% gen_statem states
-export([setup_connection/3, awaiting_setup/3, connected/3]).

-record(data, {
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
-define(OPTION_DEFAULTS, #{
    at_connect => fun(_RSocket) -> ok end,
    keepalive_interval => 3000,
    max_lifetime => 4000,
    leasing => false,
    %% TODO: Are there reasonable defaults for MIME types?
    metadata_mime_type => <<"application/json">>,
    data_mime_type => <<"application/json">>
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(
    Mode :: server | client,
    Module :: atom(),
    Transport :: pid(),
    Handlers :: map(),
    Options :: map()
) ->
    {ok, Pid :: pid()} | ignore | {error, Error :: term()}.
start_link(Mode, Module, Transport, Handlers, Options) ->
    AllOptions = maps:merge(?OPTION_DEFAULTS, Options),
    gen_statem:start_link(?MODULE, [Mode, Module, Transport, Handlers, AllOptions], []).

recv_frame(Server, Frame) ->
    gen_statem:cast(Server, {recv, Frame}).

send_error(Server, StreamID, ErrorType, ErrorData) ->
    gen_statem:cast(Server, {send_error, StreamID, ErrorType, ErrorData}).

send_keepalive(Server) ->
    gen_statem:cast(Server, send_keepalive).

send_lease(Server, TimeToLive, NumberOfRequests, Options) ->
    gen_statem:cast(Server, {send_lease, TimeToLive, NumberOfRequests, Options}).

send_metadata_push(Server, Metadata) ->
    gen_statem:cast(Server, {send_metadata_push, Metadata}).

send_request_channel(Server, N, Request, Options) ->
    gen_statem:call(Server, {send_request_channel, N, Request, Options}).

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
callback_mode() ->
    [state_functions, state_enter].

-spec init(Args :: term()) -> gen_statem:init_result(atom()).
init([server, Module, Transport, Handlers, Options]) ->
    process_flag(trap_exit, true),
    #{at_connect := AtConnect} = Options,
    Data =
        #data{
            at_connect = AtConnect,
            transport_mod = Module,
            transport_pid = Transport,
            stream_handlers = Handlers,
            next_stream_id = ?SERVER_INITIAL_STREAM_ID
        },
    {ok, awaiting_setup, Data};
init([client, Module, Transport, Handlers, Options]) ->
    process_flag(trap_exit, true),
    #{
        at_connect := AtConnect,
        keepalive_interval := KeepaliveInterval,
        max_lifetime := MaxLifetime,
        metadata_mime_type := MetadataMimeType,
        data_mime_type := DataMimeType,
        leasing := UseLeasing
    } =
        Options,
    Data0 =
        #data{
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
    Data1 =
        case UseLeasing of
            false ->
                Data0;
            true ->
                Data0#data{
                    send_lease_tracker = rsocket_lease_tracker:new(),
                    recv_lease_tracker = rsocket_lease_tracker:new()
                }
        end,
    {ok, setup_connection, Data1}.

-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> any().
terminate(_Reason, _State, _Data) ->
    void.

-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: term(),
    Data :: term(),
    Extra :: term()
) ->
    {ok, NewState :: term(), NewData :: term()} | (Reason :: term()).
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
    } =
        Data,
    SetupOptions =
        case UseLeasing of
            false ->
                [];
            true ->
                [leasing]
        end,
    Frame =
        rsocket_frame:new_setup(
            Interval,
            MaxLifetime,
            MetadataMimeType,
            DataMimeType,
            SetupOptions
        ),
    transport_frame(Frame, Data),
    {ok, _TRef} = timer:apply_interval(Interval, ?MODULE, send_keepalive, [self()]),
    {next_state, connected, Data}.

%%%
%%% Awaiting Setup (server side)
%%%

awaiting_setup(enter, _, Data) ->
    {keep_state, Data};
awaiting_setup(cast, {recv, ReceivedFrame}, Data) ->
    case rsocket_frame:parse(ReceivedFrame) of
        {setup, 0, Flags, FrameData} ->
            handle_setup(Flags, FrameData, Data);
        {_FrameType, _StreamID, _Flags, _FrameData} ->
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
    #data{at_connect = AtConnect} = Data,
    AtConnect(self()),
    {keep_state, Data};
connected(cast, {recv, Frame}, Data) ->
    {FrameType, StreamID, Flags, FrameData} = rsocket_frame:parse(Frame),
    handle_frame(FrameType, StreamID, Flags, FrameData, Data);
connected(cast, {send_error, StreamID, ErrorType, ErrorData}, Data) ->
    Frame = rsocket_frame:new_error(StreamID, ErrorType, ErrorData),
    transport_frame(Frame, Data),
    {keep_state, Data};
connected(cast, {send_lease, Time, Count, Options}, Data) ->
    #data{use_leasing = UseLeasing, recv_lease_tracker = LeaseTracker} = Data,
    case UseLeasing of
        false ->
            ok;
        true ->
            L = rsocket_frame:new_lease(Time, Count, Options),
            transport_frame(L, Data),
            ok = rsocket_lease_tracker:start_lease(LeaseTracker, Time, Count)
    end,
    {keep_state, Data};
connected(cast, {send_metadata_push, Metadata}, Data) ->
    Frame = rsocket_frame:new_metadata_push(Metadata),
    transport_frame(Frame, Data),
    {keep_state, Data};
connected({call, From}, {send_request_channel, N, Request, Options}, Data) ->
    #data{stream_handlers = StreamHandlers, next_stream_id = ID} = Data,
    Credit = spend_send_lease_credit(Data),
    case {Credit, maps:find(channel, StreamHandlers)} of
        {0, _} ->
            {keep_state, Data, [{reply, From, {error, lease_expired}}]};
        {_, error} ->
            Reply = {error, no_channel_handler},
            {keep_state, Data, [{reply, From, Reply}]};
        {_, {ok, {_Module, _InitData} = Handler}} ->
            Map =
                case proplists:lookup(metadata, Options) of
                    none ->
                        #{request => Request};
                    {metadata, Metadata} ->
                        #{request => Request, metadata => Metadata}
                end,
            {ok, _} = rsocket_stream:start_link_channel_requester(ID, Map, Handler, N),
            RS = rsocket_frame:new_request_channel(ID, N, Request, Options),
            transport_frame(RS, Data),
            NewData = Data#data{next_stream_id = ID + 2},
            {keep_state, NewData, [{reply, From, {ok, ID}}]}
    end;
connected({call, From}, {send_request_fnf, Message, Options}, Data) ->
    #data{next_stream_id = StreamID} = Data,
    case spend_send_lease_credit(Data) of
        0 ->
            {keep_state, Data, [{reply, From, {error, lease_expired}}]};
        _ ->
            Metadata = proplists:get_value(metadata, Options, <<>>),
            MetadataLength = byte_size(Metadata),
            MessageLength = byte_size(Message),
            _Plan = rsocket_fragment:plan_request_fnf(MetadataLength, MessageLength),
            %% [{Request, true, Space, 0} | Following]
            %% TODO: construct and send frames
            Frame = rsocket_frame:new_request_fnf(StreamID, Message, Options),
            transport_frame(Frame, Data),
            NewData = Data#data{next_stream_id = StreamID + 2},
            {keep_state, NewData, [{reply, From, ok}]}
    end;
connected({call, F}, {send_request_response, Request, Handler, Opts}, Data) ->
    #data{next_stream_id = StreamID} = Data,
    case spend_send_lease_credit(Data) of
        0 ->
            {keep_state, Data, [{reply, F, {error, lease_expired}}]};
        _ ->
            rsocket_stream:start_link_rr_requester(StreamID, Handler),
            RR = rsocket_frame:new_request_response(StreamID, Request, Opts),
            transport_frame(RR, Data),
            NewData = Data#data{next_stream_id = StreamID + 2},
            {keep_state, NewData, [{reply, F, {ok, StreamID}}]}
    end;
connected({call, F}, {send_request_stream, N, Request, Options}, Data) ->
    #data{stream_handlers = StreamHandlers, next_stream_id = ID} = Data,
    Credit = spend_send_lease_credit(Data),
    case {Credit, maps:find(stream_requester, StreamHandlers)} of
        {0, _} ->
            {keep_state, Data, [{reply, F, {error, lease_expired}}]};
        {_, error} ->
            Reply = {error, no_stream_requester_handler},
            {keep_state, Data, [{reply, F, Reply}]};
        {_, {ok, {_Module, _InitData} = Handler}} ->
            Map =
                case proplists:lookup(metadata, Options) of
                    none ->
                        #{request => Request};
                    {metadata, Metadata} ->
                        #{request => Request, metadata => Metadata}
                end,
            {ok, _} = rsocket_stream:start_link_stream_requester(ID, Map, Handler, N),
            RS = rsocket_frame:new_request_stream(ID, N, Request, Options),
            transport_frame(RS, Data),
            NewData = Data#data{next_stream_id = ID + 2},
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
    #data{max_lifetime = MaxLifetime} = Data,
    Frame = rsocket_frame:new_keepalive([respond]),
    transport_frame(Frame, Data),
    {ok, TRef} = timer:send_after(MaxLifetime, keepalive_timeout),
    {keep_state, Data#data{keepalive_response_timer = TRef}};
connected(cast, {send_cancel, StreamID}, Data) ->
    case rsocket_stream:find(self(), StreamID) of
        undefined ->
            ok;
        Stream ->
            exit(Stream, canceled)
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
    #data{transport_pid = Pid, transport_mod = Mod} = Data,
    Mod:close_connection(Pid),
    {stop, disconnect}.

%%%===================================================================
%%% Frame reception handlers
%%%===================================================================

handle_setup(?SETUP_FLAGS(M, _R, L), FrameData, Data) ->
    ?SETUP(
        0,
        2,
        KeepaliveInterval,
        MaxLifetime,
        _MDMTL,
        MetadataMimeType,
        _DMTL,
        DataMimeType,
        SetupData
    ) = FrameData,
    _Map =
        case M of
            0 ->
                #{payload => SetupData};
            1 ->
                ?METADATA(_Size, Metadata, Payload) = SetupData,
                #{payload => Payload, metadata => Metadata}
        end,
    Data0 = Data#data{
        keepalive_interval = KeepaliveInterval,
        max_lifetime = MaxLifetime,
        metadata_mime_type = MetadataMimeType,
        data_mime_type = DataMimeType,
        use_leasing = bit_to_bool(L)
    },
    Data1 =
        case L of
            0 ->
                Data0;
            1 ->
                Data0#data{
                    send_lease_tracker = rsocket_lease_tracker:new(),
                    recv_lease_tracker = rsocket_lease_tracker:new()
                }
        end,
    {next_state, connected, Data1}.

%% TODO: What do we do with SETUP Data and Metadata?
%% From the RSocket spec:
%% "Setup Data: includes payload describing connection capabilities of the
%% endpoint sending the Setup header."
%%

handle_frame(keepalive, _StreamID, ?KEEPALIVE_FLAGS(1), _FrameData, Data) ->
    Frame = rsocket_frame:new_keepalive([]),
    transport_frame(Frame, Data),
    {keep_state, Data};
handle_frame(keepalive, _StreamID, ?KEEPALIVE_FLAGS(0), _FrameData, Data) ->
    #data{keepalive_response_timer = TRef} = Data,
    {ok, cancel} = timer:cancel(TRef),
    {keep_state, Data};
handle_frame(metadata_push, 0, _Flags, Metadata, Data) ->
    #data{stream_handlers = StreamHandlers} = Data,
    case maps:find(metadata_push, StreamHandlers) of
        error ->
            Error = <<"No metadata-push handler">>,
            Frame = rsocket_frame:new_error(0, reject, Error),
            transport_frame(Frame, Data);
        {ok, MetadataPushHandler} when is_function(MetadataPushHandler, 1) ->
            proc_lib:spawn_link(fun() -> MetadataPushHandler(Metadata) end)
    end,
    {keep_state, Data};
handle_frame(request_channel, StreamID, Flags, FrameData, Data) when StreamID > 0 ->
    case spend_recv_lease_credit(Data) of
        0 ->
            Frame = rsocket_frame:new_error(StreamID, rejected),
            transport_frame(Frame, Data),
            {keep_state, Data};
        _ ->
            handle_request_channel(Flags, StreamID, FrameData, Data)
    end;
handle_frame(request_fnf, StreamID, Flags, FrameData, Data) when StreamID > 0 ->
    case spend_recv_lease_credit(Data) of
        0 ->
            Frame = rsocket_frame:new_error(StreamID, rejected),
            transport_frame(Frame, Data),
            {keep_state, Data};
        _ ->
            handle_request_fnf(Flags, StreamID, FrameData, Data)
    end;
handle_frame(request_response, StreamID, Flags, FrameData, Data) when StreamID > 0 ->
    case spend_recv_lease_credit(Data) of
        0 ->
            Frame = rsocket_frame:new_error(StreamID, rejected),
            transport_frame(Frame, Data),
            {keep_state, Data};
        _ ->
            handle_request_response(Flags, StreamID, FrameData, Data)
    end;
handle_frame(request_stream, StreamID, Flags, FrameData, Data) when StreamID > 0 ->
    case spend_recv_lease_credit(Data) of
        0 ->
            Frame = rsocket_frame:new_error(StreamID, rejected),
            transport_frame(Frame, Data),
            {keep_state, Data};
        _ ->
            handle_request_stream(Flags, StreamID, FrameData, Data)
    end;
handle_frame(request_n, StreamID, ?REQUEST_N_FLAGS, FrameData, Data) when StreamID > 0 ->
    case rsocket_stream:find(self(), StreamID) of
        undefined ->
            ok;
        Stream ->
            ?REQUEST_N(N) = FrameData,
            rsocket_stream:recv_request_n(Stream, N)
    end,
    {keep_state, Data};
handle_frame(payload, StreamID, ?PAYLOAD_FLAGS(M, F, C, N), FrameData, Data) when StreamID > 0 ->
    case rsocket_stream:find(self(), StreamID) of
        undefined ->
            ok;
        Stream ->
            Flags = lists:append([[follows || F =:= 1], [complete || C =:= 1], [next || N =:= 1]]),
            {PayloadData, Options} =
                case M of
                    0 ->
                        {FrameData, Flags};
                    1 ->
                        ?METADATA(_Size, Metadata, Payload) = FrameData,
                        {Payload, [{metadata, Metadata} | Flags]}
                end,
            rsocket_stream:recv_payload(Stream, PayloadData, Options)
    end,
    {keep_state, Data};
handle_frame(lease, 0, ?LEASE_FLAGS(_M), FrameData, Data) ->
    #data{use_leasing = true, send_lease_tracker = LeaseTracker} = Data,
    ?LEASE(Time, Count, _Metadata) = FrameData,
    ok = rsocket_lease_tracker:start_lease(LeaseTracker, Time, Count),
    {keep_state, Data};
handle_frame(cancel, StreamID, _Flags, _FrameData, Data) when StreamID > 0 ->
    case rsocket_stream:find(self(), StreamID) of
        undefined -> ok;
        Stream -> exit(Stream, canceled)
    end,
    {keep_state, Data};
handle_frame(error, 0, ?ERROR_FLAGS, FrameData, _Data) ->
    ?ERROR(ErrorCode, ErrorData) = FrameData,
    ErrorType = maps:get(ErrorCode, rsocket_frame:error_code_names()),
    {stop, {rsocket_error, ErrorType, ErrorData}};
handle_frame(error, StreamID, ?ERROR_FLAGS, FrameData, Data) ->
    case rsocket_stream:find(self(), StreamID) of
        undefined ->
            ok;
        Stream ->
            ?ERROR(ErrorCode, ErrorData) = FrameData,
            ErrorType = maps:get(ErrorCode, rsocket_frame:error_code_names()),
            exit(Stream, {rsocket_error, ErrorType, ErrorData})
    end,
    {keep_state, Data}.

%% TODO: What to do with the (possibly included) metadata?

handle_request_channel(Flags, ID, FrameData, Data) ->
    ?REQUEST_CHANNEL_FLAGS(M, _F, _C) = Flags,
    #data{stream_handlers = StreamHandlers} = Data,
    case maps:find(channel, StreamHandlers) of
        error ->
            Error = <<"No channel handler">>,
            Frame = rsocket_frame:new_error(ID, reject, Error),
            transport_frame(Frame, Data),
            {keep_state, Data};
        {ok, {_Mod, _Fun, _Args}} ->
            Error = <<"MFA tuples not yet supported">>,
            Frame = rsocket_frame:new_error(ID, reject, Error),
            transport_frame(Frame, Data),
            {keep_state, Data};
        {ok, {_Module, _InitData} = Handler} ->
            ?REQUEST_CHANNEL(N, RequestData) = FrameData,
            Map =
                case M of
                    0 ->
                        #{request => RequestData};
                    1 ->
                        ?METADATA(_Size, Metadata, Request) = RequestData,
                        #{request => Request, metadata => Metadata}
                end,
            {ok, Stream} = rsocket_stream:start_link_channel_responder(ID, Map, Handler, N),
            rsocket_stream:send_request_n(Stream, N),
            {keep_state, Data}
    end.

handle_request_fnf(?REQUEST_FNF_FLAGS(M, _F), StreamID, FrameData, Data) ->
    #data{stream_handlers = StreamHandlers} = Data,
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
        {ok, Handler} when is_function(Handler, 1) ->
            Map =
                case M of
                    0 ->
                        #{request => FrameData};
                    1 ->
                        ?METADATA(_Size, Metadata, Request) = FrameData,
                        #{request => Request, metadata => Metadata}
                end,
            rsocket_stream:start_link_fire_and_forget(StreamID, Map, Handler),
            {keep_state, Data}
    end.

handle_request_response(?REQUEST_RESPONSE_FLAGS(M, _F), ID, FrameData, Data) ->
    #data{stream_handlers = StreamHandlers} = Data,
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
                        #{request => FrameData};
                    1 ->
                        ?METADATA(_Size, Metadata, Request) = FrameData,
                        #{request => Request, metadata => Metadata}
                end,
            rsocket_stream:start_link_rr_responder(ID, Map, RRHandler),
            {keep_state, Data}
    end.

handle_request_stream(?REQUEST_STREAM_FLAGS(M, _F), ID, FrameData, Data) ->
    #data{stream_handlers = StreamHandlers} = Data,
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
            Map =
                case M of
                    0 ->
                        #{request => RequestData};
                    1 ->
                        ?METADATA(_Size, Metadata, Request) = RequestData,
                        #{request => Request, metadata => Metadata}
                end,
            {ok, _} = rsocket_stream:start_link_stream_responder(ID, Map, Handler, N),
            {keep_state, Data}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

spend_recv_lease_credit(#data{use_leasing = false}) ->
    infinity;
spend_recv_lease_credit(#data{use_leasing = true, recv_lease_tracker = Tracker}) ->
    rsocket_lease_tracker:spend_1(Tracker).

spend_send_lease_credit(#data{use_leasing = false}) ->
    infinity;
spend_send_lease_credit(#data{use_leasing = true, send_lease_tracker = Tracker}) ->
    rsocket_lease_tracker:spend_1(Tracker).

transport_frame(Frame, #data{transport_pid = Pid, transport_mod = Mod}) ->
    ok = Mod:send_frame(Pid, Frame).

bit_to_bool(0) ->
    false;
bit_to_bool(1) ->
    true.
