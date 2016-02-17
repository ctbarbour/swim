

# Module swim_membership #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#alive-3">alive/3</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#faulty-3">faulty/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#local_member-1">local_member/1</a></td><td></td></tr><tr><td valign="top"><a href="#members-1">members/1</a></td><td></td></tr><tr><td valign="top"><a href="#num_members-1">num_members/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_status-3">set_status/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#suspect-3">suspect/3</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="alive-3"></a>

### alive/3 ###

`alive(Pid, Member, Incarnation) -> any()`

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="faulty-3"></a>

### faulty/3 ###

`faulty(Pid, Member, Incarnation) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Msg, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="local_member-1"></a>

### local_member/1 ###

`local_member(Pid) -> any()`

<a name="members-1"></a>

### members/1 ###

`members(Pid) -> any()`

<a name="num_members-1"></a>

### num_members/1 ###

`num_members(Pid) -> any()`

<a name="set_status-3"></a>

### set_status/3 ###

`set_status(Pid, Member, Status) -> any()`

<a name="start_link-3"></a>

### start_link/3 ###

`start_link(LocalMember, EventMgrPid, Opts) -> any()`

<a name="suspect-3"></a>

### suspect/3 ###

`suspect(Pid, Member, Incarnation) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

