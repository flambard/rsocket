-module(rsocket_passthrough_stream_responder).

-behaviour(rsocket_stream_responder).

%% API
-export([send_payload/3, send_error/3]).
%% rsocket_stream_requester callbacks
-export([init/2, handle_request_n/2]).

-record(state, {test_case_ref, test_case_pid, request}).

%%%===================================================================
%%% API
%%%===================================================================

send_payload(Stream, Payload, Options) ->
    rsocket_stream_responder:payload(Stream, Payload, Options).

send_error(Stream, ErrorType, ErrorData) ->
    rsocket_stream_responder:error(Stream, ErrorType, ErrorData).

%%%===================================================================
%%% rsocket_stream_responder callbacks
%%%===================================================================

init(Request, [TestCaseRef, TestCasePid]) ->
    State =
        #state{
            test_case_ref = TestCaseRef,
            test_case_pid = TestCasePid,
            request = Request
        },
    {ok, State}.

handle_request_n(N, State) ->
    #state{test_case_ref = Ref, test_case_pid = Pid} = State,
    Pid ! {responder, handle_request_n, Ref, N},
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
