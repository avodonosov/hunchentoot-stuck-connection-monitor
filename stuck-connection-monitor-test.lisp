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

(pushnew "~/prj/hunchentoot/" asdf:*central-registry* :test 'equal)
;(pushnew "~/prj/usocket/" asdf:*central-registry* :test 'equal)
;(pushnew "~/prj/cl+ssl/cl-plus-ssl/" asdf:*central-registry* :test 'equal)

(ql:quickload :hunchentoot :verbose t)

(setq hunchentoot:*log-lisp-errors-p* t
      hunchentoot:*log-lisp-backtraces-p* t
      ;cl+ssl::*default-unwrap-stream-p* nil
      )

(defclass my-acceptor (hunchentoot:ssl-acceptor
                       hunchentoot-stuck-connection-monitor::stuck-connection-monitor
                       ) ())

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
;;;  and slime-listt-threads does not shot this thread.

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
;; 1.7
     (hunchentoot:stop *srv*)
;; 1.8 Check that the monitoring thread remains running, as well
;;   as the worker threads of the 3 stuck connections
;;   (use slime-list-threads, for example).
;; 1.9 Terminate the connections. Variations for the test case:
;;   - stop by Ctrl-C on client side,
;;   - terminate by the
       (hunchentoot-stuck-connection-monitor::shutdown-stuck-connections *srv*)
;; 1.10 Check that the worker threads are freed and the monitoring thread
;;   is terminated after all the workers are completed.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testcase 2: shutdown-sockets-automatically
;; 2.1
     (setf (hunchentoot-stuck-connection-monitor::shutdown-sockets-automatically *srv*)
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
