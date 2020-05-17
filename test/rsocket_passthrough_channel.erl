-module(rsocket_passthrough_channel).
-behaviour(rsocket_channel).

%% API
-export([
         send_cancel/1,
         send_error/3,
         send_payload/3,
         send_request_n/2
        ]).

%% rsocket_channel callbacks
-export([
         init/2,
         handle_payload/3,
         handle_request_n/2
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
    rsocket_channel:cancel(Stream).

send_error(Stream, ErrorType, ErrorData) ->
    rsocket_channel:error(Stream, ErrorType, ErrorData).

send_payload(Stream, Payload, Options) ->
    rsocket_channel:payload(Stream, Payload, Options).

send_request_n(Stream, N) ->
    rsocket_channel:request_n(Stream, N).


%%%===================================================================
%%% rsocket_channel callbacks
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
    Pid ! {channel, self(), handle_payload, Ref, Payload, Options},
    {ok, State}.

handle_request_n(N, State) ->
    #state{
       test_case_ref = Ref,
       test_case_pid = Pid
      } = State,
    Pid ! {channel, self(), handle_request_n, Ref, N},
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
