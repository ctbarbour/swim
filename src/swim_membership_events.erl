-module(swim_membership_events).
-behavior(gen_event).

-export([enqueue/2, dequeue/3]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("swim.hrl").

-type member_state() :: {member_status(), member(), incarnation()}.
-type transmissions() :: non_neg_integer().
-type broadcast_event() :: {member(), transmissions(), member_state()}.

-record(state, {
	  events = [] :: [broadcast_event()]
	 }).

init([]) ->
    {ok, #state{}}.

enqueue(EventMgrPid, Event) ->
    gen_event:notify(EventMgrPid, {membership, Event}).

dequeue(EventMgrPid, MaxSize, MaxTransmissions) ->
    gen_event:call(EventMgrPid, ?MODULE, {dequeue, MaxSize, MaxTransmissions}).

handle_event({membership, {_Status, Member, _Inc} = Event}, State) ->
    #state{events=Events} = State,
    FilteredEvents = lists:filter(fun({M, _, _I}) ->
					  M /= Member
				  end, Events),
    {ok, State#state{events=[{Member, 0, Event} | FilteredEvents]}};
handle_event(_, State) ->
    {ok, State}.

handle_call({dequeue, MaxSize, MaxTransmissions}, State) ->
    #state{events=Events} = State,
    {Broadcast, Keep} = do_dequeue(MaxSize, MaxTransmissions, Events),
    {ok, Broadcast, State#state{events=Keep}};
handle_call(_, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_dequeue(_MaxSize, _MaxTransmissions, []) ->
    {[], []};
do_dequeue(MaxSize, MaxTransmissions, Events) ->
    SortedEvents = lists:keysort(2, Events),
    do_dequeue(MaxSize, MaxTransmissions, SortedEvents, [], []).

do_dequeue(MaxSize, _, Events, Broadcast, Keep)
  when MaxSize =< 0 ->
    {Broadcast, lists:flatten([Events | Keep])};
do_dequeue(_MaxSize, _MaxTransmissions, [], Broadcast, Keep) ->
    {Broadcast, Keep};
do_dequeue(MaxSize, MaxTransmissions, [NextEvent | Events], Broadcast, Keep)
  when MaxSize > 0 ->
    NewSize = MaxSize - 12,
    {Member, Transmissions, Msg} = NextEvent,
    NewBroadcast = [Msg | Broadcast],
    case Transmissions + 1 of
	T when T >= MaxTransmissions ->
	    do_dequeue(NewSize, MaxTransmissions, Events, NewBroadcast, Keep);
	T ->
	    do_dequeue(NewSize, MaxTransmissions, Events, NewBroadcast, [{Member, T, Msg} | Keep])
    end.
