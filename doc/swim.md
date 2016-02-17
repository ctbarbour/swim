

# Module swim #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#child_spec-4">child_spec/4</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#local_member-1">local_member/1</a></td><td></td></tr><tr><td valign="top"><a href="#members-1">members/1</a></td><td></td></tr><tr><td valign="top"><a href="#publish-2">publish/2</a></td><td></td></tr><tr><td valign="top"><a href="#rotate_keys-2">rotate_keys/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="child_spec-4"></a>

### child_spec/4 ###

`child_spec(Name, LocalMember, Keys, Opts) -> any()`

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

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

<a name="publish-2"></a>

### publish/2 ###

`publish(Pid, Event) -> any()`

<a name="rotate_keys-2"></a>

### rotate_keys/2 ###

`rotate_keys(Pid, NewKey) -> any()`

<a name="start_link-3"></a>

### start_link/3 ###

`start_link(LocalMember, Keys, Opts) -> any()`

<a name="start_link-4"></a>

### start_link/4 ###

`start_link(Name, LocalMember, Keys, Opts) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(Pid) -> any()`

<a name="subscribe-1"></a>

### subscribe/1 ###

`subscribe(Pid) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

