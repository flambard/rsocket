-module(rsocket_passthrough_stream_requester).
-behaviour(rsocket_stream_requester).

%% API
-export([
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
