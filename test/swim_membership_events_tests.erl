-module(swim_membership_events_tests).

-include_lib("eunit/include/eunit.hrl").

swim_event_handler_test_() ->
    {ok, EventMgrPid} = gen_event:start_link(),
    ok = gen_event:add_handler(EventMgrPid, swim_membership_events, []),
    MembershipEvent = {alive, {{127,0,0,1}, 5000}, 1},
    ok = swim_membership_events:enqueue(EventMgrPid, MembershipEvent),
    Broadcasts = swim_membership_events:dequeue(EventMgrPid, 100, 1),
    ?_assertMatch([MembershipEvent], Broadcasts).
