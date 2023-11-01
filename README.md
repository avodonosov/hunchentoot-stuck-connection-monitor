Monitors hunchentoot connections and logs
the connections stuck in the same state for a long time.

Offers an option to shutdown the stuck connections sockets
manually or automatically, thus unblocking
the connection threads and preventing thread and socket leakage.

# Usage
Create your own acceptor subclass and put our mixin
class in the class precedence list before hunchentoot:acceptor,
like this:
https://github.com/avodonosov/hunchentoot-stuck-connection-monitor/blob/main/test-stuck-connection-monitor.lisp#L47

The public API is in the defpackage form at the top of
stuck-connection-monitor.lisp.

# Background

Hunchentoot uses a thread per connection model. To prevent
thread and socket leakage due to slow, inactive or lost clients,
hunchentoot configures IO timeout for connection
socket streams, using a lisp-implementation specific
way: https://github.com/edicl/hunchentoot/blob/master/set-timeouts.lisp#L31

This does not always help.

One [reported](https://github.com/edicl/hunchentoot/issues/189)
scenario of thread leakage is TLS mode
(hunchentoot:ssl-acceptor). The default behaviour of cl+ssl
is to retrieve socket file descriptor from socket lisp
stream, and give the file descriptor to OpenSSL to
perform all IO. In result the timeout settings of
lisp stream are ignored.

To fix that users can switch cl+ssl to LispBIO mode, where
all IO is performed over lisp stream, and so the timeouts
remain working.

To do that either globally
`(setq cl+ssl:*default-unwrap-stream-p* nil)`
or override the `hunchentoot:initialize-connection-stream`
for your acceptor, and inside of this method add `:unwrap-stream-p nil`
parameter to the call to `cl+ssl:make-ssl-server-stream`:
https://github.com/edicl/hunchentoot/blob/460a32c0378c659cf8740ad84a47169484f9b131/ssl.lisp#L83

However, there are more potential cases of stuck connection.

Slow request attack, where client sends data
over the request stream very slowly, say one byte in 15 seconds,
thus preventing timeout from happening, keeping
server resources - socket and connection thread - occupied.

Another possibility is the lisp implementation-specific
IO timeouts not working. For example on SBCL write timeouts
are not supported (https://groups.google.com/g/sbcl-devel/c/-eLw-Wv3Prc).
So if client does not read server response, the connection
thread may be blocked. Although not forever. TCP implementation
will be trying to re-transmit the data not acknowledged by
the client, and will finally give up and close the socket
(online articles suggest it will take round 15 minutes,
in my testing that took 20 minutes).

Therefore it's good to have a health check service
for hucnhentoot, that detects stuck connections.

The file test-stuck-connection-monitor.lisp
contains steps to reproduce various scenarios
described above, and to see how
hunchentoot-stuck-connection-monitor detects them.

Of course, not all applications want to consider
connections staying in the same state for a long time
as a problem. For example, a server offering some
long polling endpoint, or supporting very large file
downloads, may consider such connections as fine.

# Unblocking the threads

To unblock the connection threads we can `shutdown`
the connection socket.

Strictly speaking, calling socket operations
from a thread other than the connection thread that
currently reads or writes from socket is not guaranteed
to work correctly, socket API is single threaded.

But `shutdown` works fine on Linux and unblocks the connection
thread. (Unlike, for example, `close` function).

Some StackOverflow user said (I don't have a link)
that on Windows it's other way around, `shutdown` is unsafe
to call from another thread, while `close` (or `closesocket`,
I don't remember) is fine. I haven't verified that.

Alternatively to socket shutdown, the user can watch
logs for stuck-connection-monitor messages
and, for example, restart the web server container.
 
