

# Module swim #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-swim_opt">swim_opt()</a> ###


<pre><code>
swim_opt() = {protocol_period, pos_integer()} | {ack_proxies, pos_integer()} | {ack_timeout, pos_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#child_spec-4">child_spec/4</a></td><td></td></tr><tr><td valign="top"><a href="#local_member-1">local_member/1</a></td><td></td></tr><tr><td valign="top"><a href="#members-1">members/1</a></td><td></td></tr><tr><td valign="top"><a href="#publish-2">publish/2</a></td><td></td></tr><tr><td valign="top"><a href="#rotate_keys-2">rotate_keys/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="child_spec-4"></a>

### child_spec/4 ###

`child_spec(Name, LocalMember, Keys, Opts) -> any()`

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

<pre><code>
start_link(LocalMember::<a href="#type-member">member()</a>, Keys::[<a href="#type-key">key()</a>], Opts::[<a href="#type-swim_opt">swim_opt()</a> | <a href="swim_membership.md#type-swim_membership_opt">swim_membership:swim_membership_opt()</a>]) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(Name::atom(), LocalMember::<a href="#type-member">member()</a>, Keys::[<a href="#type-key">key()</a>], Opts::[<a href="#type-swim_opt">swim_opt()</a> | <a href="swim_membership.md#type-swim_membership_opt">swim_membership:swim_membership_opt()</a>]) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

`stop(Pid) -> any()`

<a name="subscribe-1"></a>

### subscribe/1 ###

`subscribe(Pid) -> any()`

