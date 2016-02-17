

# Module swim_broadcasts #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#dequeue-2">dequeue/2</a></td><td></td></tr><tr><td valign="top"><a href="#dequeue-3">dequeue/3</a></td><td></td></tr><tr><td valign="top"><a href="#enqueue-3">enqueue/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-2">handle_call/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_event-2">handle_event/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#max_transmissions-2">max_transmissions/2</a></td><td></td></tr><tr><td valign="top"><a href="#membership-2">membership/2</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#user-2">user/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="dequeue-2"></a>

### dequeue/2 ###

`dequeue(EventMgrPid, NumMembers) -> any()`

<a name="dequeue-3"></a>

### dequeue/3 ###

`dequeue(EventMgrPid, NumMembers, MaxSize) -> any()`

<a name="enqueue-3"></a>

### enqueue/3 ###

`enqueue(EventMgrPid, EventCategory, Event) -> any()`

<a name="handle_call-2"></a>

### handle_call/2 ###

`handle_call(X1, State) -> any()`

<a name="handle_event-2"></a>

### handle_event/2 ###

`handle_event(X1, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="max_transmissions-2"></a>

### max_transmissions/2 ###

`max_transmissions(NumMembers, RetransmitFactor) -> any()`

<a name="membership-2"></a>

### membership/2 ###

`membership(EventMgrPid, Event) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="user-2"></a>

### user/2 ###

`user(EventMgrPid, Event) -> any()`

