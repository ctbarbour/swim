-module(swim_events).
-behavior(gen_event).

-export([start_link/0, subscribe/3, membership/2]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {
	  subscriptions = [] :: [subscription()]
	 }).

-record(subscription, {
	  pid :: pid(),
	  event_category :: user | membership,
	  mref :: reference()
	 }).

-type subscription() :: #subscription{}.

start_link() ->
    gen_event:start_link().

subscribe(EventMgrPid, EventCategory, Pid) ->
    gen_event:call(EventMgrPid, ?MODULE, {subscribe, EventCategory, Pid}).

membership(EventMgrPid, Event) ->
    gen_event:notify(EventMgrPid, {membership, Event}).

init([]) ->
    {ok, #state{subscriptions=[]}}.

handle_event({EventCategory, Event}, State) ->
    #state{subscriptions=Subscriptions} = State,
    Data = {swim, {EventCategory, Event}},
    _ = [Pid ! Data ||
	    #subscription{pid=Pid, event_category=EC} <- Subscriptions,
	    EC == EventCategory],
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call({subscribe, EventCategory, Pid}, State) ->
    #state{subscriptions=Subscriptions} = State,
    MRef = erlang:monitor(process, Pid),
    Subscription = #subscription{pid=Pid, event_category=EventCategory, mref=MRef},
    {ok, ok, State#state{subscriptions=[Subscription | Subscriptions]}};
handle_call(_Msg, State) ->
    {ok, ok, State}.

handle_info({'DOWN', MRef, process, Pid, _Reason}, State) ->
    #state{subscriptions=Subscriptions} = State,
    case lists:keytake(MRef, #subscription.mref, Subscriptions) of
	{value, #subscription{pid=Pid}, Rest} ->
	    {ok, State#state{subscriptions=Rest}};
	false ->
	    {ok, State}
    end;
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
