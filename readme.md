# Baby's first chat server in Erlang: *Crosstalk*

Does what the title says on the tin - a basic implementation of a chat
servers with channels, nicknames, and messages posted thru them. Pretty rough around the edges and
should never be used in production. Implementation's pretty
interesting - although ignore the behavior_infos where they shouldn't
be, since -behavior() doesn't recognize -compile(export_all). But first:

## Setting it up

After pulling from git into a folder, `cd(...).` to the root of the
directory, ` make:all([load]).` to process the Emakefile, and then `cd("ebin"), application:start(crosstalk).`
to actually start the app. If you wish, alter the Makefile to set the
value for `port` - the port the server'll listen on.

## How it works.

*I haven't found an acceptable way to comment inline how the whole
 system fits together, so I'm dumping it here. Sorry*

*Crosstalk* works with string-packets containing "commands" - commands
 with verbs and operands delimited by spaces - sent over TCP. Crosstalk so far
 supports an exhaustive 4: `I'm [nickname]`, `join [channel]`, `leave
 [channel]`, `post [channel] [message...`.  `I'm` and `join` are
 asynchronous - essentially, they send a reply that looks like
 `success|failure [reason]`.

Here's how the whole shebang's architectured - with the supervision
tree elided, since all that does in this case is blindly start and
restart. Control flow when commands are recieved and acted upon is
marked with arrows; ownership - "who starts who? who monitors who?" -
is denoted with boxes.

<code>
                 +---------------------------------------------------+
                 |                                                   |
                 |  +------------------------------------------+     |
                 |  |client_session_initiator - the accept loop|     |
                 |  +------------------------------------------+     |
                 |                                                   |
                 |  +-----------------+            +------------+    |
<--to/from client|->|message_forwarder|<---------->|client_proxy|<---|---+
                 |  +-----------------+            +------------+    |   |
                 |                                                   |   |
                 +---------------------------------------------------+   |
                 +---------------------------------------------------+   |
                 |                                                   |   |
                 |                                                   |   |
                 |                                   +-------+       |   |
                 |                                   |channel| <-----|---+
                 |                                   +-------+       |   |
                 |  +----------------+               +-------+       |   |
                 |  |channel_registry|               |channel| <-----|---+
                 |  +----------------+               +-------+       |   |
                 |                                   +-------+       |   |
                 |                                   |channel| <-----|---+
                 |                                   +-------+       |
                 |                                                   |
                 |                                                   |
                 |                                                   |
                 +---------------------------------------------------+
</code>

Let's walk through a normal session.

*Handshaking on first connect:* the moment the server accepts a
connection, it creates a message\_forwarder that farms strings to and
from the client, and a client\_proxy that holds all the client logic
that acts on commands. The client\_proxy doesn't have to know
anything about TCP - it just has to accept {from\_client,
$message} messages and send {to\_client, $message} messages back to
the message forwarder. Currently, though,
the client\_proxy's in the "uninitialized" state where all it can do is wait
for an "I'm" command containing username.

Client will send: "I'm a\_user". Client proxy recieves it, destructures
it, and realizes it should (1) set the nickname on its end, and (2)
finally start interacting with channels and posting messages. So it replies with a "hi a\_user" acknowledging
the message.

*Joining a channel:* user sends in "join a\_channel". client_proxy
  then asks channel\_registry for the channel mapped to "a\_channel", and recieves it in return. Like IRC,
 channel\_registry creates and stores the channel if it doesn't exist. User then registers itself
 with the channel to receive messages. This process might fail: a user with the same
 nickname might already be registered with the channel, for example.

*Posting a message:* user sends in "post a\_channel this is a
 message". Having cached a\_channel to the channel's actual pid,
 client\_proxy goes straight to the channel and gives it the contents of
 the messages and the user's nicknames. The channel then promptly
 hands out the message and the posting nickname to all the
 client_proxies registered to it (even the posting user. I don't know
 whether this is the right thing to do, but it's a quick change if we
 do want to switch), who then forward on a "message a\_channel a\_user
 this is a message" to the message\_forwarder and on.

*Closing up* - when the user wishes to leave a channel, it sends a
 "leave a\_channel" message - client_proxy notifies channel, it does
 the same. After this, the user can simply hang up to sever all ties -
 message\_forwarder tells client_proxy, which tells its channels to
 remove it. The Grim Collecter then reaps it.





