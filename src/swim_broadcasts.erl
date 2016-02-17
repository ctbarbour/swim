-module(swim_broadcasts).
-behavior(gen_event).

-export([max_transmissions/2, membership/2, user/2, enqueue/3,
	 dequeue/2, dequeue/3]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("swim.hrl").

-type member_state() :: {member_status(), member(), incarnation()}.

-record(state, {
	  retransmit_factor = 4  :: pos_integer(),
	  membership_events = [] :: [{non_neg_integer(), member_state()}],
	  user_events       = [] :: [{non_neg_integer(), binary()}]
	 }).

max_transmissions(NumMembers, RetransmitFactor) ->
    round(math:log(NumMembers + 1)) * RetransmitFactor.

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

init([]) ->
    init([4]);
init([RetransmitFactor]) ->
    {ok, #state{retransmit_factor=RetransmitFactor}}.

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
    {SizeLeft, MBroadcast, MKeep} = do_dequeue(MaxSize, MaxTransmissions,
					       [{T, {membership, MB}} || {T, MB} <- MEvents]),
    {_, UBroadcast, UKeep} = do_dequeue(SizeLeft, MaxTransmissions,
					[{T, {user, UB}} || {T, UB} <- UEvents],
					MBroadcast, []),
    Broadcast = lists:foldl(fun(B, Acc) -> << B/binary, Acc/binary>> end, <<>>, UBroadcast),
    {ok, Broadcast, State#state{membership_events=MKeep, user_events=UKeep}};
handle_call(_, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_dequeue(MaxSize, MaxTransmissions, Events) ->
    SortedEvents = lists:keysort(1, Events),
    do_dequeue(MaxSize, MaxTransmissions, SortedEvents, [], []).

do_dequeue(MaxSize, _MaxTransmissions, [], Broadcast, Keep) ->
    {MaxSize, Broadcast, Keep};
do_dequeue(MaxSize, MaxTransmissions, [NextEvent | Events] = E, Broadcast, Keep) ->
    {_Transmissions, Msg} = NextEvent,
    Wire = swim_messages:encode_event(Msg),
    case MaxSize - size(Wire) of
	NewSize when NewSize >= 0 ->
	    NewBroadcast = [Wire | Broadcast],
	    do_dequeue(NewSize, MaxTransmissions, Events, NewBroadcast,
		       maybe_keep(MaxTransmissions, NextEvent, Keep));
	NewSize when NewSize < 0 ->
	    {0, Broadcast, lists:flatten([E | Keep])}
    end.

maybe_keep(MaxTransmissions, {Transmissions, {_, _Msg}}, Keep)
  when Transmissions + 1 >= MaxTransmissions ->
    Keep;
maybe_keep(MaxTransmissions, {Transmissions, {_, Msg}}, Keep)
  when Transmissions + 1 < MaxTransmissions ->
    [{Transmissions + 1, Msg} | Keep].
