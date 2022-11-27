;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2022, Anton Vodonosov, avodonosov@yandex.ru.  All rights reserved.

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

(in-package :hunchentoot)

;;;; The track-progress function below is suggested to be added to the
;;;; oficial upstream hunchentoot.

(deftype progress-state ()
  `(member :initializing-stream
           :reading-request
           :processing-request
           :connection-done))

(defgeneric track-progress (socket new-state-name acceptor)
  (:documentation "Experimental.

A hook that can be used by acceptors to verify that
connection handling threads are not stuck forever
waiting for data on inactive connections.

This method is called multiple times for every connection,
with NEW-STATE-NAME parameter taking one of the PROGRESS-STATE
values. The normal state transitions:

                          <---------------------------------------<-
                         |                                          |
    :initializing-stream -> :reading-request -> :processing-request -> :connection-done

The :CONNECTION-DONE state is activated from UNWIND-PROTECT,
therefore in case of SERIOS-CONDITION or other non-local control
transer, this state can also be transitioned to \"exceptionally\",
from any state, in addition to the normal transitions illustrated
above.

The implementation of this method can remember the time of the
last state change for every socket, and if it stays unchanged
for too long, take some measures (logging, socket shutdown, ...).

Background.

Hunchentoot relies on socket timeouts to make sure
worker threads are not stuck forever on inactive connections -
see SET-TIMEOUTS. Hunchentoot expects an error signalled
when IO hasn't happened for the timeout duration.

However, this approach assumes socket timeouts work well in all Lisp
implementations and that all possible socket stream wrapper layers -
flexi-streams, chunga, cl+ssl - keep the timeouts working.

But currently, streams created by cl+ssl in the `:unwrap-stream-p nil`
mode, which is the default and means \"pass to OpenSSL the file descriptor
of the Lisp socket stream\", do not signal the timeout errors,
at least on several important Lisp implementations (it seems
those implementations handle the timeout on Lisp side,
and not as the socket file descriptor options).

See https://github.com/cl-plus-ssl/cl-plus-ssl/pull/69,
https://github.com/edicl/hunchentoot/issues/189.")
  (:method :around (socket new-state-name acceptor)
           (assert (typep new-state-name 'progress-state))
           ;; catch and log any errors signalled by
           ;; implementations.
           (handler-bind
               ((serious-condition
                 (lambda (c)
                   (acceptor-log-message acceptor
                                         :error
                                         "Error in (track-progress ~S ~S ~S) : ~A. ~A "
                                         socket new-state-name acceptor c (get-backtrace)))))
             (call-next-method)))
  ;; the default method does nothing
  (:method (socket new-state-name acceptor)
    (declare (ignore socket new-state-name acceptor))))
