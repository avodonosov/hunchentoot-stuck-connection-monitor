;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

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

;; Here we have an exact copy of hunchentoot:process-connection
;; except we added (track-progress ...) in several places
;; and specialized it to our monitor class.
;; The original process-connection code is from
;; git commit 2bda004ef763fd6c85d76ee0311f8f22ed3865b5
;; Date:   Mon Jun 21 15:52:15 2021 +0545
;; It is probably makes sense to add the (track-progress ...)
;; calls to the oficial upstream hunchentoot:process-connection
;; method defined for hunchentoot:acceptor.
;; BTW, in this case the extension users won't need to
;; place our mixin class stuck-connection-monitor earlier
;; in the class precedence list of their acceptors
;; than hunchentoot:acceptor - the mixin will work
;; even if it's placed after.
(defmethod process-connection ((*acceptor* hunchentoot-stuck-connection-monitor::stuck-connection-monitor)
                               (socket t))
  (let* ((socket-stream (make-socket-stream socket *acceptor*))
         (*hunchentoot-stream*)
         (*close-hunchentoot-stream* t)
         (remote (multiple-value-list (get-peer-address-and-port socket)))
         (local (multiple-value-list (get-local-address-and-port socket))))
    (unwind-protect
         ;; process requests until either the acceptor is shut down,
         ;; *CLOSE-HUNCHENTOOT-STREAM* has been set to T by the
         ;; handler, or the peer fails to send a request
         (progn
           (track-progress socket :initializing-stream *acceptor*)
           (setq *hunchentoot-stream* (initialize-connection-stream *acceptor* socket-stream))
           (loop
              (let ((*finish-processing-socket* t))
                (when (acceptor-shutdown-p *acceptor*)
                  (return))
                (track-progress socket :reading-request *acceptor*)
                (multiple-value-bind (headers-in method url-string protocol)
                    (get-request-data *hunchentoot-stream*)
                  ;; check if there was a request at all
                  (unless method
                    (return))
                  ;; bind per-request special variables, then process the
                  ;; request - note that *ACCEPTOR* was bound above already
                  (let ((*reply* (make-instance (acceptor-reply-class *acceptor*)))
                        (*session* nil)
                        (transfer-encodings (cdr (assoc* :transfer-encoding headers-in))))
                    (when transfer-encodings
                      (setq transfer-encodings
                            (split "\\s*,\\s*" transfer-encodings))
                      (when (member "chunked" transfer-encodings :test #'equalp)
                        (cond ((acceptor-input-chunking-p *acceptor*)
                               ;; turn chunking on before we read the request body
                               (setf *hunchentoot-stream* (make-chunked-stream *hunchentoot-stream*)
                                     (chunked-stream-input-chunking-p *hunchentoot-stream*) t))
                              (t (hunchentoot-error "Client tried to use ~
chunked encoding, but acceptor is configured to not use it.")))))
                    (with-acceptor-request-count-incremented (*acceptor*)
                      (track-progress socket :processing-request *acceptor*)
                      (process-request (acceptor-make-request *acceptor* socket
                                                              :headers-in headers-in
                                                              :content-stream *hunchentoot-stream*
                                                              :method method
                                                              :uri url-string
                                                              :remote remote
                                                              :local local
                                                              :server-protocol protocol))))
                  (finish-output *hunchentoot-stream*)
                  (setq *hunchentoot-stream* (reset-connection-stream *acceptor* *hunchentoot-stream*))
                  (when *finish-processing-socket*
                    (return))))))
      (track-progress socket :connection-done *acceptor*)
      (when *close-hunchentoot-stream*
        (flet ((close-stream (stream)
                 ;; as we are at the end of the request here, we ignore all
                 ;; errors that may occur while flushing and/or closing the
                 ;; stream.
                 (ignore-errors
                  (finish-output stream))
                 (ignore-errors
                  (close stream :abort t))))
          (unless (or (not *hunchentoot-stream*)
                      (eql socket-stream *hunchentoot-stream*))
            (close-stream *hunchentoot-stream*))
          (close-stream socket-stream))))))
