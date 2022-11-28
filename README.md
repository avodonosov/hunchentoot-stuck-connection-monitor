Addresses https://github.com/edicl/hunchentoot/issues/189

Monitors hunchentoot connections and logs
the connections stuck in the same state for a long time (due to
slow or inactive clients and network stream timeouts
that hunchentoot tries to utilize not working properly).

Offers an option to shutdown the stuck connections sockets
manually or automatically, thus unblocking
the connection threads and preventing thread and socket leak.

# Usage
Create your own acceptor subclass and put our mixin
class in the class precedence list before hunchentoot:acceptor,
like this:
https://github.com/avodonosov/hunchentoot-stuck-connection-monitor/blob/main/stuck-connection-monitor-test.lisp#L47