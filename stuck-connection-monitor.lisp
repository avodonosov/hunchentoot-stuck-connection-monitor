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

(defpackage #:hunchentoot-stuck-connection-monitor
  (:use :cl))

(in-package :hunchentoot-stuck-connection-monitor)

(defclass record ()
  (;; Socket of a hunchentoot HTTP connction
   (socket :initarg :socket)

   ;; A string in the form local-port:remote-host:remote-port
   (description :initarg :description :type string)

   ;; The state specified by the last `(hunchentoot::record-progress ...)`
   ;; for this socket.
   (state :initarg :state :type keyword)

   ;; The time of the last state change
   (state-enter-realtime :initarg :state-enter-realtime)

   ;; The last thread that registered progress (state change)
   (thread :initarg :thread :type bt:thread)

   ;; The time the monitor preformed shutdown for this socket,
   ;; automatically or due to `(shutdown-stuck-connections ...)`
   ;; invoked by someone.
   ;; It is hoped that the socket shutdown
   ;; unblocks the connections with error,
   ;; thus leading to :connection-done state
   ;; which will remove this record.
   ;; In case socket is shutdown more than once somehow,
   ;; this field is not updated - the first shutdown time
   ;; remains recorded.
   (socket-shutdown-realtime :initform nil
                             :type (or null (integer 0)))))

(defclass stuck-connection-monitor ()
  ((lock :initform (bt:make-lock "hunchentoot-stuck-connection-monitor.lock")
         :reader lock)

   ;; Records for all active hunchentoot connections.
   ;; Removed when connection reaches the :connection-done state.
   (records-by-socket :type hash-table
                      :initform (make-hash-table :test #'eq)
                      :accessor records-by-socket)

   (shutdown-sockets-automatically :type boolean
                                   :initform nil
                                   :initarg :shutdown-sockets-automatically
                                   :accessor shutdown-sockets-automatically)

   ;; How often the thread wakes up to check for stuck
   ;; connections.
   (monitoring-interval-seconds :type (integer 0)
                                :initarg :monitoring-interval-seconds
                                :accessor monitoring-interval-seconds
                                :initform 60)

   ;; The monitoring thread.
   ;; May be set a little later than the thread-running-p.
   (thread :type (or null bt:thread)
           :initform nil
           :accessor thread)

   ;; Allows to avoid calling extrnal functions
   ;; (e.g. thread creation) from under a lock,
   ;; when making sure that only one thread will be started.
   ;; If we were using the THREAD slot for that,
   ;; we would need to call external functions
   ;; from under a lock, thus creating potential
   ;; for a deadlock.
   (thread-running-p :type boolean
                     :initform nil
                     :accessor thread-running-p)

   ;; This value is set by the methods
   ;; hunchentoot:stop and hunchentoot:start
   ;; implemented for the monitor.
   ;; When set to T makes the monitoring
   ;; thread exit as soon as all the acceptor
   ;; connections are finished.
   (stop-requested-p :type boolean
                     :initform nil
                     :accessor stop-requested-p)

   ;; The monitoring thread waits on this
   ;; condition variable when sleeping
   ;; for the monitoring-interval-seconds, thus the
   ;; thread can be woken up to re-check the
   ;; termination condition.
   (stop-cond-var :initform (bt:make-condition-variable
                             :name "hunchentoot-stuck-connection-monitor.stop-cond-var")
                  :reader stop-cond-var)

   ))

;; must be called under the LOCK
(defun should-stop-p (acceptor)
  (and (stop-requested-p acceptor)
       ;; zero records means all connections
       ;; have reached the :connection-done state
       (zerop (hash-table-count (records-by-socket acceptor)))))

(defmethod hunchentoot:start :around ((monitor stuck-connection-monitor))
  (bt:with-lock-held ((lock monitor))
    (setf (stop-requested-p monitor) nil))
  (call-next-method))

(defmethod hunchentoot:stop :around ((monitor stuck-connection-monitor) &key soft)
  (declare (ignore soft))
  (bt:with-lock-held ((lock monitor))
    (setf (stop-requested-p monitor) t)
    (bt:condition-notify (stop-cond-var monitor)))
  (call-next-method))

(defun hunch-log (acceptor level format-str &rest format-args)
  (apply #'hunchentoot:acceptor-log-message
         acceptor level format-str format-args))

(defun log-warn (acceptor format-str &rest format-args)
  (apply #'hunch-log acceptor :warn format-str format-args))

(defun log-info (acceptor format-str &rest format-args)
  (apply #'hunch-log acceptor :info format-str format-args))

(defparameter debug-enabled nil)

(defun log-debug (acceptor format-str &rest args)
  ;; Don't reimplement this as a macro that generates
  ;; empty code in disabled mode - such disabling
  ;; will skip evaluation of the expressions whose
  ;; results we log through this method, and we rely
  ;; on side-effects of some of them, like REMHASH.
  (when debug-enabled
    (apply #'hunch-log acceptor :info
           (concatenate 'string "DBG: " format-str)
           args)))

(defun wait-for-stop (monitor &key timeout-seconds)
  "Returns true if the MONINTOR's thread should be stopped,
and false on timeout. As usually, the TIMEOUT-SECONDS can be fractional."
  (let ((start-real-time (get-internal-real-time)))
    (loop
       (let* ((seconds-passed (/ (- (get-internal-real-time)
                                    start-real-time)
                                 internal-time-units-per-second))
              (remaining-seconds (- timeout-seconds seconds-passed)))
         (log-debug monitor
                    "wait-for-stop, seconds-passed: ~A, remaining-seconds: ~A"
                    (floor seconds-passed) (floor remaining-seconds))
         (when (<= remaining-seconds 0)
           (return-from wait-for-stop nil))
         (bt:with-lock-held ((lock monitor))
           (when (should-stop-p monitor)
             (return-from wait-for-stop t))
           (unless (bt:condition-wait (stop-cond-var monitor)
                                      (lock monitor)
                                      :timeout (max remaining-seconds
                                                    ;; just out of fear that some
                                                    ;; implementations may not like too
                                                    ;; small timeouts
                                                    1/1000))
             ;; A nil returned from bt:condition-wait means
             ;; the temeout has expired and the lock is not held anymore.
             ;; (Hopefully, the exit code of the bt:with-lock-held macro
             ;; is prepared to the lock not being held).
             (return-from wait-for-stop nil)))))))

(defgeneric stuck-timeout (acceptor)
  (:documentation
   "Tells the monitor what time without progress indicates a stuck connection.
NIL means no timeout.")
  (:method (acceptor)
    (let ((read-timeout (hunchentoot:acceptor-read-timeout acceptor))
          (write-timeout (hunchentoot:acceptor-write-timeout acceptor)))
      (and read-timeout
           write-timeout
           (* 3 (max read-timeout write-timeout))))))

(defun timed-out-records (monitor)
  (let* ((now (get-internal-real-time)))
    (bt:with-lock-held ((lock monitor))
      (let ((timeout (stuck-timeout monitor))
            (result nil))
        (unless (null timeout)
          (maphash (lambda (id record)
                     (declare (ignore id))
                     (when (> now
                              (+ (slot-value record 'state-enter-realtime)
                                 (* internal-time-units-per-second
                                    timeout)))
                       (push record result)))
                   (records-by-socket monitor)))
        result))))

(defun socket-description (socket)
  (or (ignore-errors
        ;; On CCL at least, signals an error for a disconnected socket,
        ;; (despited the docstring saying ;; "... and tries to act
        ;; robustly in the presence of network problems. "
        (hunchentoot::client-as-string socket))
      "<disconnected?>"))

(defun shutdown-socket (monitor record)
  (with-slots (socket socket-shutdown-realtime) record
    (log-warn monitor
              "Invoking `shutdown` for the socket of the connection ~A: ~A"
              (socket-description socket) socket)
    (handler-case
        (progn
          ;; Shutdown for :INPUT and :OUTPUT separately,
          ;; becuase the direction :IO, despite documented
          ;; in the USOCKET:SOCKET-SHUTDOWN docstring,
          ;; is not supported by many usocket
          ;; backends, yet.
          ;; https://github.com/usocket/usocket/pull/76
          (usocket:socket-shutdown socket :input)
          (usocket:socket-shutdown socket :output)
          ;; TODO: synchronization of the RECORD?
          ;;       (the monitoring logic does not need it, but can
          ;;        we corrupt a CLOS instance by updating
          ;;        different fields from different threads?)
          (unless socket-shutdown-realtime
            (setf socket-shutdown-realtime (get-internal-real-time))))
      (serious-condition (s)
        (log-warn monitor
                  "A SERIOUS-CONDITION is signalled when shutting-down socket by timeout ~A: ~A"
                  ;; TODO: uiop:print-backtrace
                  socket s)))))

(defun seconds-since (a-realtime)
  (floor (/ (max 0 (- (get-internal-real-time) a-realtime))
            internal-time-units-per-second)))

(defun monitor-thread-body (monitor)
  (log-info monitor "stuck-connection-monitor thread started~%")
  (handler-case
      (loop
         while (not (wait-for-stop monitor
                                   :timeout-seconds
                                   (bt:with-lock-held ((lock monitor))
                                     (monitoring-interval-seconds monitor))))
         do
           (let* ((timed-out (timed-out-records monitor)))
             (log-debug monitor "timed-out: ~S" timed-out)
             (when timed-out
               (setq timed-out
                     (sort timed-out
                           #'string<
                           :key (lambda (record)
                                  (slot-value record 'description))))
               (log-warn monitor
                         "~A connection(s) seem stuck, thus leaking their threads.~:[ On Linux use `(hunchentoot-stuck-connection-monitor::shutdown-stuck-connections your-acceptor)` to termitate them manually or run with `:shutdown-sockets-automatically t`.~;~] The connections:"
                         (length timed-out)
                         (shutdown-sockets-automatically monitor))
               (dolist (record timed-out)
                 (with-slots (socket description state state-enter-realtime
                                     thread socket-shutdown-realtime) record
                   (log-warn monitor
                             "~A sits in ~:S for ~A sec. Socket: ~A, thread: ~A~@[, socket-shutdown-realtime: ~A~]."
                             description
                             state
                             (seconds-since state-enter-realtime)
                             socket
                             thread
                             socket-shutdown-realtime)
                   (when (shutdown-sockets-automatically monitor)
                     (shutdown-socket monitor record)))))))
    (serious-condition (c)
      (log-warn monitor
                "A serious condition in the monitoring thread for ~S: ~A~%~A"
                monitor c (hunchentoot::get-backtrace))))
  (log-info monitor "stuck-connection-monitor thread exiting~%")
  (bt:with-lock-held ((lock monitor))
    (setf (thread-running-p monitor) nil
          (thread monitor) nil)))

(defun shutdown-stuck-connections (acceptor)
  (let* ((timed-out (timed-out-records acceptor)))
    (log-info acceptor "Shutting down ~A connections: " (length timed-out))
    (dolist (record timed-out)
      (shutdown-socket acceptor record))))

(defun ensure-monitoring-thread-running (monitor)
  (let ((start-p nil))
    (bt:with-lock-held ((lock monitor))
      (unless (thread-running-p monitor)
        (setf (thread-running-p monitor) t
              start-p t)))
    (when start-p
      (let* ((taskmaster (hunchentoot::acceptor-taskmaster monitor))
             (thread-name (format nil
                                  "hunchentoot-stuck-connection-monotor of ~S"
                                  monitor))
             (thread-thunk (lambda () (monitor-thread-body monitor)))
             (thread (hunchentoot:start-thread taskmaster
                                               thread-thunk
                                               :name thread-name)))
        (bt:with-lock-held ((lock monitor))
          (setf (thread monitor) thread))))))

(defmethod hunchentoot::track-progress (socket
                                        new-state-name
                                        (acceptor stuck-connection-monitor))
  (log-debug acceptor "(record-progress ~S ~S ~S)" socket new-state-name acceptor)
  (bt:with-lock-held ((lock acceptor))
    (if (eq new-state-name :connection-done)
        (log-debug acceptor
                   "remhash: ~S, hash-table-count: ~S"
                   (remhash socket (records-by-socket acceptor))
                   (hash-table-count (records-by-socket acceptor)))

        (let ((record (or (gethash socket (records-by-socket acceptor))
                          (setf (gethash socket (records-by-socket acceptor))
                                (make-instance 'record
                                               :description (socket-description socket)
                                               :socket socket
                                               :thread (bt:current-thread))))))

          (setf (slot-value record 'state)
                new-state-name

                (slot-value record 'thread)
                (bt:current-thread)

                (slot-value record 'state-enter-realtime)
                (get-internal-real-time))))

    ;; If the acceptor is being stopped,
    ;; make sure the monitoring thread exits as soon as
    ;; the last hunchentoot worker thread complectes
    ;; its connection handling. without waiting
    ;; for the monitoring thread wake-up interval.
    ;; The minimum necessary to achieve that
    ;; is to always notify the monitoring thread,
    ;; but in order to optimize away unnecessary wake-ups we
    ;; only notify if the exit condition should-stop-p
    ;; is satisfyied.
    (when (should-stop-p acceptor)
      (bt:condition-notify (stop-cond-var acceptor))))

  (if (typep (hunchentoot::acceptor-taskmaster acceptor)
             'hunchentoot:one-thread-per-connection-taskmaster)
      (ensure-monitoring-thread-running acceptor)))


