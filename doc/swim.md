

# Module swim #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This is the main swim module.

__Version:__ Jul 26 2015 10:56:59

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#members-1">members/1</a></td><td>Returns the known members for a given gossip peer.</td></tr><tr><td valign="top"><a href="#publish-2">publish/2</a></td><td>Publishes a term to the rest of the gossip peers.</td></tr><tr><td valign="top"><a href="#rotate_keys-2">rotate_keys/2</a></td><td>Rotate the encryption keys for a local gossip peer.</td></tr><tr><td valign="top"><a href="#start_gossip-4">start_gossip/4</a></td><td>Starts a gossip peer.</td></tr><tr><td valign="top"><a href="#stop_gossip-1">stop_gossip/1</a></td><td>Stops a running gossip peer.</td></tr><tr><td valign="top"><a href="#subscribe-2">subscribe/2</a></td><td>Subscribes to events on a given gossip group.</td></tr><tr><td valign="top"><a href="#unsubscribe-2">unsubscribe/2</a></td><td>Unsubscribe from an event category.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="members-1"></a>

### members/1 ###

<pre><code>
members(Name::atom()) -&gt; [<a href="#type-member">member()</a>]
</code></pre>
<br />

Returns the known members for a given gossip peer.

<a name="publish-2"></a>

### publish/2 ###

<pre><code>
publish(Name::atom(), Event::term()) -&gt; ok
</code></pre>
<br />

Publishes a term to the rest of the gossip peers.

<a name="rotate_keys-2"></a>

### rotate_keys/2 ###

<pre><code>
rotate_keys(Name::atom(), Key::<a href="#type-key">key()</a>) -&gt; ok
</code></pre>
<br />

Rotate the encryption keys for a local gossip peer.

<a name="start_gossip-4"></a>

### start_gossip/4 ###

<pre><code>
start_gossip(Name::atom(), ListenIp::<a href="inet.md#type-ip_address">inet:ip_address()</a>, ListenPort::<a href="inet.md#type-port_number">inet:port_number()</a>, Opts::[<a href="swim_gossip.md#type-gossip_opt">swim_gossip:gossip_opt()</a>]) -&gt; {ok, pid()}
</code></pre>
<br />

Starts a gossip peer.

<a name="stop_gossip-1"></a>

### stop_gossip/1 ###

<pre><code>
stop_gossip(Name::atom()) -&gt; ok
</code></pre>
<br />

Stops a running gossip peer.

<a name="subscribe-2"></a>

### subscribe/2 ###

<pre><code>
subscribe(Name::atom(), EventCategory::<a href="#type-event_category">event_category()</a>) -&gt; ok
</code></pre>
<br />

Subscribes to events on a given gossip group.

A subscribing process will receive messages based on the event-category.

<a name="unsubscribe-2"></a>

### unsubscribe/2 ###

<pre><code>
unsubscribe(Name::atom(), EventCategory::<a href="#type-event_category">event_category()</a>) -&gt; ok
</code></pre>
<br />

Unsubscribe from an event category.

