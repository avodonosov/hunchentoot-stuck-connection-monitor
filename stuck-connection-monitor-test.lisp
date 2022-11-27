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
(ql:quickload :hunchentoot-stuck-connection-monitor :verbose t)

(setq hunchentoot:*log-lisp-errors-p* t
      hunchentoot:*log-lisp-backtraces-p* t
      ;cl+ssl::*default-unwrap-stream-p* nil
      )

(defclass my-acceptor (hunchentoot-stuck-connection-monitor::stuck-connection-monitor
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
       (hunchentoot-stuck-connection-monitor::shutdown-stuck-connections *srv*)
;; 1.11 Check that the worker threads are freed and the monitoring thread
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

;; 4.1
;;
     (defparameter *timeout-backup*
       (slot-value *srv* 'hunchentoot::read-timeout))
     (setf (slot-value *srv* 'hunchentoot::read-timeout) nil)
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
    (setf (slot-value *srv* 'hunchentoot::read-timeout) *timeout-backup*)


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
         (iterations (ceiling (* 2 (/ (hunchentoot-stuck-connection-monitor::monitoring-interval-seconds srv)
                                      sleep-seconds)))))
    (slow-request-attack #(127 0 0 1)
                         (hunchentoot:acceptor-port srv)
                         :ssl-p (hunchentoot:ssl-p srv)
                         :sleep-seconds sleep-seconds
                         :iterations iterations)))
;; 5.1 HTTPS
(setf (hunchentoot-stuck-connection-monitor::shutdown-sockets-automatically *srv*)
      nil)
(hunchentoot:start *srv*)
(test-slow-request-attack *srv*)
;; Check the logs - the stuck connection should be logged (twice)
(hunchentoot:stop *srv*)
;; Veirfy the monitoring thread is stopped
;;   (a message is logged, also slime-list-threads does not show it).

;; 5.2 Plain HTTP

(defclass my-plain-acceptor (hunchentoot-stuck-connection-monitor::stuck-connection-monitor
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
