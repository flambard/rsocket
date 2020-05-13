-module(rsocket_passthrough_stream_requester).
-behaviour(rsocket_stream_requester).

%% API
-export([
         send_cancel/1,
         send_request_n/2
        ]).

%% rsocket_stream_requester callbacks
-export([
         init/2,
         handle_payload/3
        ]).

-record(state,
        {
         test_case_ref,
         test_case_pid,
         request
        }).

%%%===================================================================
%%% API
%%%===================================================================

send_cancel(Stream) ->
    rsocket_stream_requester:cancel(Stream).

send_request_n(Stream, N) ->
    rsocket_stream_requester:request_n(Stream, N).


%%%===================================================================
%%% rsocket_stream_requester callbacks
%%%===================================================================

init(Request, [TestCaseRef, TestCasePid]) ->
    State = #state{
               test_case_ref = TestCaseRef,
               test_case_pid = TestCasePid,
               request = Request
              },
    {ok, State}.

handle_payload(Payload, Options, State) ->
    #state{
       test_case_ref = Ref,
       test_case_pid = Pid
      } = State,
    Pid ! {requester, handle_payload, Ref, Payload, Options},
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
