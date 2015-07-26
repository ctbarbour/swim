

# Module swim_gossip_events #
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2015

__Version:__ Jul 26 2015 10:33:30

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-2">handle_call/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_event-2">handle_event/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#membership-2">membership/2</a></td><td>Publishes a membership update event.</td></tr><tr><td valign="top"><a href="#notify-3">notify/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-3">subscribe/3</a></td><td>Subscribe a Process to receive messages from the gossip group identified
by <code>Name</code> and filtered by <code>EventCategory</code>.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#unsubscribe-3">unsubscribe/3</a></td><td>Unsubscribe a Process from receiving further messages from a gossip group.</td></tr><tr><td valign="top"><a href="#user-2">user/2</a></td><td>Publishes a User event.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="handle_call-2"></a>

### handle_call/2 ###

`handle_call(Msg, State) -> any()`

<a name="handle_event-2"></a>

### handle_event/2 ###

`handle_event(Event, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="membership-2"></a>

### membership/2 ###

`membership(Name, Event) -> any()`

Publishes a membership update event

<a name="notify-3"></a>

### notify/3 ###

`notify(Name, EventCategory, Event) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="subscribe-3"></a>

### subscribe/3 ###

`subscribe(Name, EventCategory, Pid) -> any()`

Subscribe a Process to receive messages from the gossip group identified
by `Name` and filtered by `EventCategory`.

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="unsubscribe-3"></a>

### unsubscribe/3 ###

`unsubscribe(Name, EventCategory, Pid) -> any()`

Unsubscribe a Process from receiving further messages from a gossip group.

<a name="user-2"></a>

### user/2 ###

`user(Name, Event) -> any()`

Publishes a User event

