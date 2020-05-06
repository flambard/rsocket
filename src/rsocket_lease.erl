-module(rsocket_lease).

-export([
         new/0,
         initiate/3,
         spend_1/1,
         expire/2
        ]).


new() ->
    Lease = ets:new(lease, [public]),
    ets:insert(Lease, {count, 0}),
    Lease.

initiate(Lease, Time, Count) ->
    case ets:lookup(Lease, timer) of
        []                       -> ok;
        [{timer, PreviousTimer}] ->
            {ok, cancel} = timer:cancel(PreviousTimer)
    end,
    Ref = make_ref(),
    ets:insert(Lease, [{reference, Ref}, {count, Count + 1}]),
    {ok, Timer} = timer:apply_after(Time, ?MODULE, expire, [Lease, Ref]),
    ets:insert(Lease, {timer, Timer}),
    ok.

spend_1(Lease) ->
    ets:update_counter(Lease, count, {2, -1, 0, 0}).

expire(Lease, Ref) ->
    case ets:lookup(Lease, reference) of
        [{reference, Ref}] ->
            ets:insert(Lease, {count, 0}),
            ok;
        [{reference, _ReplacedRef}] ->
            %% Another Ref has been stored. That means another timer has been
            %% started and we should not reset the count to 0.
            ok
    end.
