-module(swim_user_events).
-behavior(gen_event).

-export([publish/2, flush/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  events = [] :: [binary()]
	 }).

publish(EventMgrPid, Event) ->
    gen_event:notify(EventMgrPid, {publish, Event}).

flush(EventMgrPid) ->
    gen_event:call(EventMgrPid, ?MODULE, flush).

init([]) ->
    {ok, #state{}}.

handle_event({publish, Event}, State) ->
    #state{events=Events} = State,
    {ok, State#state{events=[Event | Events]}};
handle_event(_, State) ->
    {ok, State}.

handle_call(flush, State) ->
    #state{events=Events} = State,
    {ok, Events, State#state{events=[]}};
handle_call(_, State) ->
    {ok, badarg, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
