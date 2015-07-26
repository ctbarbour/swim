

# Module swim_vault #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This module is responsible for maintaining the secure keys used for
encrypting and decrypted SWIM protocol messages.

Copyright (c) 2015

__Version:__ Jul 26 2015 10:56:59

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#decrypt-2">decrypt/2</a></td><td>Decrypt and authenticate the provided cipher text using the known keys.</td></tr><tr><td valign="top"><a href="#encrypt-2">encrypt/2</a></td><td>Encrypt the provided plain text using the latest key.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#rotate_keys-2">rotate_keys/2</a></td><td>Add a new key to the rotation.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="decrypt-2"></a>

### decrypt/2 ###

<pre><code>
decrypt(Name::atom() | pid(), Data::binary()) -&gt; {ok, binary()} | {error, failed_verification}
</code></pre>
<br />

Decrypt and authenticate the provided cipher text using the known keys.
We attempt to use the most recently provide and continue attempts until we
have exhausted all known keys.

<a name="encrypt-2"></a>

### encrypt/2 ###

<pre><code>
encrypt(Name::atom() | pid(), Msg::binary()) -&gt; binary()
</code></pre>
<br />

Encrypt the provided plain text using the latest key. For information on
how data encrypted see [`swim_messages:encrypt/3`](swim_messages.md#encrypt-3).

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

<a name="rotate_keys-2"></a>

### rotate_keys/2 ###

<pre><code>
rotate_keys(Name::atom() | pid(), Key::<a href="#type-key">key()</a>) -&gt; ok
</code></pre>
<br />

Add a new key to the rotation. This key will now be used as the key for
[`encrypt/1`](#encrypt-1).

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Keys::[<a href="#type-key">key()</a>]) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Name::atom() | pid()) -&gt; ok
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

