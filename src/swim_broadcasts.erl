-module(swim_broadcasts).
-behavior(gen_event).

-export([membership/2, user/2, enqueue/3, dequeue/2, dequeue/3]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("swim.hrl").

-type member_state() :: {member_status(), member(), incarnation()}.

-record(state, {
	  retransmit_factor = 5  :: pos_integer(),
	  membership_events = [] :: [{non_neg_integer(), member_state()}],
	  user_events       = [] :: [{non_neg_integer(), binary()}]
	 }).

init([]) ->
    init([5]);
init([RetransmitFactor]) ->
    {ok, #state{retransmit_factor=RetransmitFactor}}.

membership(EventMgrPid, Event) ->
    enqueue(EventMgrPid, membership, Event).

user(EventMgrPid, Event) ->
    enqueue(EventMgrPid, user, Event).

enqueue(EventMgrPid, EventCategory, Event) ->
    gen_event:notify(EventMgrPid, {EventCategory, Event}).

dequeue(EventMgrPid, NumMembers) ->
    MaxSize = swim_messages:event_size_limit(),
    dequeue(EventMgrPid, NumMembers, MaxSize).

dequeue(EventMgrPid, NumMembers, MaxSize) ->
    gen_event:call(EventMgrPid, ?MODULE, {dequeue, MaxSize, NumMembers}).

handle_event({user, Event}, State) ->
    #state{user_events=Events} = State,
    {ok, State#state{user_events=[{0, Event} | Events]}};
handle_event({membership, {_Status, Member, _Inc} = Event}, State) ->
    #state{membership_events=Events} = State,
    FilteredEvents = lists:filter(fun({_, {_, M, _}}) ->
					  M /= Member
				  end, Events),
    {ok, State#state{membership_events=[{0, Event}| FilteredEvents]}};
handle_event(_, State) ->
    {ok, State}.

handle_call({dequeue, MaxSize, NumMembers}, State) ->
    #state{membership_events=MEvents, retransmit_factor=RetransmitFactor,
	   user_events=UEvents} = State,
    MaxTransmissions = max_transmissions(NumMembers, RetransmitFactor),
    {SizeLeft, MBroadcast, MKeep} = do_dequeue(MaxSize, MaxTransmissions, MEvents),
    {_, UBroadcast, UKeep} = do_dequeue(SizeLeft, MaxTransmissions, UEvents),
    Broadcast = lists:flatten([
			       [{membership, MB} || MB <- MBroadcast] |
			       [{user, UB} || UB <- UBroadcast]]),
    {ok, Broadcast, State#state{membership_events=MKeep, user_events=UKeep}};
handle_call(_, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

max_transmissions(NumMembers, RetransmitFactor) ->
    round(math:log(NumMembers + 1)) * RetransmitFactor.

do_dequeue(MaxSize, MaxTransmissions, Events) ->
    SortedEvents = lists:keysort(1, Events),
    do_dequeue(MaxSize, MaxTransmissions, SortedEvents, [], []).

do_dequeue(MaxSize, _, Events, Broadcast, Keep)
  when MaxSize =< 0 ->
    {0, Broadcast, lists:flatten([Events | Keep])};
do_dequeue(MaxSize, _MaxTransmissions, [], Broadcast, Keep) ->
    {MaxSize, Broadcast, Keep};
do_dequeue(MaxSize, MaxTransmissions, [NextEvent | Events], Broadcast, Keep)
  when MaxSize > 0 ->
    NewSize = MaxSize - 12,
    {_Transmissions, Msg} = NextEvent,
    NewBroadcast = [Msg | Broadcast],
    do_dequeue(NewSize, MaxTransmissions, Events, NewBroadcast,
	       maybe_keep(MaxTransmissions, NextEvent, Keep)).

maybe_keep(MaxTransmissions, {Transmissions, _Msg}, Keep)
  when Transmissions + 1 >= MaxTransmissions ->
    Keep;
maybe_keep(MaxTransmissions, {Transmissions, Msg}, Keep)
  when Transmissions + 1 < MaxTransmissions ->
    [{Transmissions + 1, Msg} | Keep].
