-module(rsocket_lease_tracker).

-export([
         new/0,
         start_lease/3,
         spend_1/1,
         expire/2
        ]).


new() ->
    LeaseTracker = ets:new(lease, [public]),
    ets:insert(LeaseTracker, {count, 0}),
    LeaseTracker.

start_lease(LeaseTracker, Time, Count) ->
    case ets:lookup(LeaseTracker, timer) of
        []                       -> ok;
        [{timer, PreviousTimer}] ->
            {ok, cancel} = timer:cancel(PreviousTimer)
    end,
    Ref = make_ref(),
    ets:insert(LeaseTracker, [{reference, Ref}, {count, Count + 1}]),
    {ok, T} = timer:apply_after(Time, ?MODULE, expire, [LeaseTracker, Ref]),
    ets:insert(LeaseTracker, {timer, T}),
    ok.

spend_1(LeaseTracker) ->
    ets:update_counter(LeaseTracker, count, {2, -1, 0, 0}).

expire(LeaseTracker, Ref) ->
    case ets:lookup(LeaseTracker, reference) of
        [{reference, Ref}] ->
            ets:insert(LeaseTracker, {count, 0}),
            ok;
        [{reference, _ReplacedRef}] ->
            %% Another Ref has been stored. That means another timer has been
            %% started and we should not reset the count to 0.
            ok
    end.
