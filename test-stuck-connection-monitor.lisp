;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

;;; Copyright (c) 2021, Anton Vadanosau, avodonosov@yandex.ru.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;;; Several simple tests, mostly manual, for the stuck-connection-monitor.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup

(pushnew "~/prj/hunchentoot-stuck-connection-monitor/"
         asdf:*central-registry*
         :test 'equal)

(ql:quickload :hunchentoot :verbose t)
(ql:quickload :hunchentoot-stuck-connection-monitor :verbose t)

(setq hunchentoot:*log-lisp-errors-p* t
      hunchentoot:*log-lisp-backtraces-p* t
      ;cl+ssl::*default-unwrap-stream-p* nil
      )

(defclass my-acceptor (hunch-conn-mon:stuck-connection-monitor
                       hunchentoot:ssl-acceptor
                       )
  ())

;; How to generate a private key and certificate:
;;
;; ```bash
;;
;;     cd ~/prj/hunchentoot/
;;     openssl req -newkey rsa:2048 -new -nodes -x509 -days 3650 -keyout my-work-key.pem -out my-work-cert.pem
;;
;; ```
(defparameter *srv*
  (make-instance 'my-acceptor
                 :ssl-privatekey-file "~/prj/hunchentoot/my-work-key.pem"
                 :ssl-certificate-file "~/prj/hunchentoot/my-work-cert.pem"
                 :port 1443
                 :read-timeout 3
                 :write-timeout 3
                 :monitoring-interval-seconds 30))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testcase 1: stuck connections are logged with their state.
;; 1.1 start the acceptor:
     (hunchentoot:start *srv*)

;;   Expected: the monitoring thread is not started -
;;   the "monitoring thread started" message is not logged
;;;  and slime-list-threads does not show this thread.

;; 1.2 Initialte several "stuck" connections. Shell commands:
;;
;;      # Establish TCP connection, but don't proceed to the TLS handshake.
;;      # This connection will be logged by monitor as stuck in :initializing-stream
;;      telnet localhost 1443
;;
;;      # Preform TLS handshake, but don't proceed to sending
;;      # HTTP request. This connectio will be logged as stuck in :reading-request
;;      openssl s_client -connect localhost:1443
;;
;;      # Send HTTP headers over TLS connection, but pause
;;      # at the sending the body (curl is waiting to read the body data
;;      # from stdin). This connection will be logged as stuck in :processing-request
;;      curl --insecure --request POST --upload-file '.' https://localhost:1443

;; 1.3 The first connection must be accompanied by the "monitoring thread started"
;;   log.
;; 1.4 Give the monitor thread time to log the 3 stuck connections
;;   and check they are logged in their expected states.
;; 1.5 Check the connections in the log are sorted by their description, which is
;;   local-port:remote-host:remote-port
;; 1.6 Make sure normal HTTP requests (from browser) are handled OK:
;;   Open the https://localhost:1443 in browser, accept security exception
;;   for the self-signed certificate.
;;   The default hunchentoot "URL Not Found" html page must be displayed.
;; 1.7 Check that the monitoring thread repeats logging the stuck
;;   connections every :monitoring-interval-seconds.
;; 1.8
     (hunchentoot:stop *srv*)
;; 1.9 Check that the monitoring thread remains running, as well
;;   as the worker threads of the 3 stuck connections
;;   (use slime-list-threads, for example).
;;   Also it must continue logging the remaining stuck connections.
;; 1.10 Terminate the connections. Variations for the test case:
;;   - stop by Ctrl-C on client side,
;;   - terminate by the function call recommended in the log - it must be
       (hunch-conn-mon:shutdown-stuck-connections *srv*)
;; 1.11 Check that the worker threads are freed and the monitoring thread
;;   is terminated after all the workers are completed.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testcase 2: shutdown-sockets-automatically
;; 2.1
     (setf (hunch-conn-mon:shutdown-sockets-automatically *srv*)
           t)
     (hunchentoot:start *srv*)
;; 2.2 Create stuck connections as in the Testcase 1.
;;     Give the monitor time to log them.
;; 2.3 Check that the connections are logged and shut down, the worker threads are freed.
;; 2.2 After the
     (hunchentoot:stop *srv*)
;;   the monitoring thread exits immediately.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testcase 3: in absense of stuck connections, the `(hunchentoot:stop ...)`
;;           terminates the monitoring thread immediately.
;; 3.1
     (hunchentoot:start *srv*)

;; 3.2 Perform several requests from browser.
;; 3.3 Verify the monitoring thread is running
;;   (it logs a message when starts, also slime-list-threads can show it).
;; 3.4
     (hunchentoot:stop *srv*)
;; 3.5 Veirfy the monitoring thread is stopped
;;   (a message is logged, also slime-list-threads does not show it).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testcase 4: input or output timeout of acceptor is not set.
;;
;; - None of the connections is considered stuck - any
;;   duration in a state is fine.

;; 4.1
;;
     (defparameter *timeout-backup*
       (hunchentoot:acceptor-read-timeout *srv*))
     (setf (slot-value *srv* 'hunchentoot::read-timeout) nil
           (slot-value *srv* 'hunchentoot::write-timeout) nil)
     (hunchentoot:start *srv*)
;; 4.2 Create stuck connections as in the Testcase 1.
;;     Wait for more than monitoring-interval - verify they
;;     are not logged.
;; 4..3
     (hunchentoot:stop *srv*)
;;   Verify the monitoring thread remains running
;; 4..4
;;   Terminate the connections by Ctrl-C in console
;;   Verify the monitoring thread stops at this point (logs the message)
;; 4..5
     (setf (slot-value *srv* 'hunchentoot::read-timeout) *timeout-backup*
           (slot-value *srv* 'hunchentoot::write-timeout) *timeout-backup*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testcase 5: slow request attack, for both HTTPS and plain HTTP.

(defun slow-request-attack-body (binary-stream &key
                                                 (sleep-seconds 10)
                                                 (iterations 12))
  "Makes pauses for SLEEP-SECONDS during HTTP request sending,
and then sends one byte. Repeats ITERATIONS times.
Thus prevents socket timeout on server, and keeps the server
handler occupied."
  (let ((s (flexi-streams:make-flexi-stream binary-stream
                                            :external-format '(:utf-8
                                                               :eol-style :crlf
                                                               :little-endian nil))))
    (format s "GET / HTTP/1.1~%")
    (format s "User-Agent: ")
    (finish-output s)
    (dotimes (i iterations)
      (format t "slow request iteration ~A~%" i)
      (finish-output s)
      (sleep sleep-seconds)
      (write-char #\z s)
      (finish-output s))))

(defun slow-request-attack (host port &key (ssl-p nil)
                                        (sleep-seconds 10)
                                        (iterations 12))
  (let ((sock (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let ((sock-stream (usocket:socket-stream sock)))
           (slow-request-attack-body (if ssl-p
                                         (cl+ssl:make-ssl-client-stream sock-stream
                                                                        :verify nil)
                                         sock-stream)
                                     :sleep-seconds sleep-seconds
                                     :iterations iterations))
      (usocket:socket-close sock))))

(defun test-slow-request-attack (srv)
  (let* ((sleep-seconds (1- (hunchentoot:acceptor-read-timeout srv)))
         ;; make sure monitoring interval is reached during the attack, twice
         (iterations (ceiling (* 2 (/ (hunch-conn-mon:monitoring-interval-seconds srv)
                                      sleep-seconds)))))
    (slow-request-attack #(127 0 0 1)
                         (hunchentoot:acceptor-port srv)
                         :ssl-p (hunchentoot:ssl-p srv)
                         :sleep-seconds sleep-seconds
                         :iterations iterations)))
;; 5.1 HTTPS
(setf (hunch-conn-mon:shutdown-sockets-automatically *srv*)
      nil)
(hunchentoot:start *srv*)
(test-slow-request-attack *srv*)
;; Check the logs - the stuck connection should be logged (twice)
(hunchentoot:stop *srv*)
;; Veirfy the monitoring thread is stopped
;;   (a message is logged, also slime-list-threads does not show it).

;; 5.2 Plain HTTP

(defclass my-plain-acceptor (hunch-conn-mon:stuck-connection-monitor
                             hunchentoot:acceptor
                             )
  ())

(defparameter *plain-srv*
  (make-instance 'my-plain-acceptor
                 :port 8088
                 :read-timeout 3
                 :write-timeout 3
                 :monitoring-interval-seconds 30))

(hunchentoot:start *plain-srv*)
(test-slow-request-attack *plain-srv*)
;; Check the logs - the stuck connection should be logged (twice)
(hunchentoot:stop *plain-srv*)
;; Veirfy the monitoring thread is stopped
;;   (a message is logged, also slime-list-threads does not show it).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testcase 6: write timeout does not work on SBCL (at least)

;; 6.1 Create a request handler that responds with body
;;     large enough to fill the socket buffers on
;;     receiver and sender sides:

(defmethod send-large-body ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let ((stream (hunchentoot:send-headers))
        (byte-counter 0))
    (dotimes (i 30000)
      (let* ((bytes-sent-msg (format nil "~8d response bytes sent (~4dth iteration)." byte-counter i))
             (response-fragment (format nil
                                        "~A Hello, hello, hello, world! Hellooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo!~%"
                                        bytes-sent-msg))
             (response-fragment-bytes (flex:string-to-octets response-fragment
                                                             :external-format :utf-8)))
        (format t "~A~%" bytes-sent-msg)
        (write-sequence response-fragment-bytes stream)
        (incf byte-counter (length response-fragment-bytes))))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor my-plain-acceptor)
                                                  request)
  (if (string= "/large-body"
               (hunchentoot:script-name request))
      (send-large-body)
      (call-next-method)))

;; 6.2 Extend the read timeout to 10 sec so that
;;   we have time to copy the request line
;;   into telnet session below.
(defparameter *timeout-backup*
  (hunchentoot:acceptor-read-timeout *plain-srv*))

(setf (slot-value *plain-srv* 'hunchentoot::read-timeout) 10
      ;; for many lisps, write timeout should be equal to read timeout
      (slot-value *plain-srv* 'hunchentoot::write-timeout) 10)

;; 6.3
(hunchentoot:start *plain-srv*)

;; 6.4 Simulate a client that sends the request but does
;;   not read the response. (Note, you need to hit <Return>
;;   twice after the second line:
;;
#|

 ```bash

time telnet localhost 8088 | more ; stty echo
GET /large-body HTTP/1.1


# Be sure to enter the second line quickly,
# before the server read timeout expires.
# And hit <Return> twice after the second
# line.
#
# The `stty echo` in the end is needed to restore
# terminal echoing of input characters.
# Without it, if the `telnet | more` pipeline
# is interrupted with Ctrl-C, the terminal
# echo remains disabled as it is done by the `more`.

```

|#

;; 6.5
;; In the terminal you will see the beginning
;; of the response. As further printing is
;; blocked by more, the response is not read,
;; and the server thread is blocked in write
;; operation.
;;
;; On SBCl the thread will remain waiting
;; way after the write timeout.
;; You will see the stuck-connection-monitor
;; log messages in hunchentoot output.
;;
;; The TCP stack will be repeatedly trying
;; to resent the packets for which the receiver
;; (the `telnet | more` combo) does not send
;; an acknowledgement. After a long delay the
;; TCP stack will close the socket. Online
;; articles sugggest on Linux it will take around
;; 15 minutes. In my tests with SBCL 2.2.6
;; and Linux 5.15.0-87-generic #97-Ubuntu SMP Mon Oct 2 21:09:21 UTC 2023 x86_64 x86_64 x86_64 GNU/Linux
;; it took 20 minutes.
;; Search for "TCP retransmission timeout" and
;; RFC-6298 for more info.

;; 6.6
(hunchentoot:stop *plain-srv*)
(setf (slot-value *plain-srv* 'hunchentoot::read-timeout) *timeout-backup*
      (slot-value *plain-srv* 'hunchentoot::write-timeout) *timeout-backup*)

;; 6.7
(hunchentoot-stuck-connection-monitor:shutdown-stuck-connections *plain-srv*)

;; 6.8
;; Terminate the telnet in the terminal with Ctrl-C

;; Note: why we use telnet to simulate a client
;; that does not read the response, instead of a
;; similar lisp function that would connect to the server,
;; send request line and not read the response:
;; it's because on CCL the sockets implementation
;; automatically closes the client socket if it's
;; receiver buffer is exhausted.
