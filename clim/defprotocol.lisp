;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1989, 1990 International Lisp Associates.  All rights reserved."
 
(defvar *stream-protocols* nil)

(defmacro define-stream-protocol (name &body accessors)
  `(progn
     (defprotocol ,name nil)
     (defrole ,name () ,(map 'list #'(lambda (accessor-name)
                                       (list accessor-name :accessor accessor-name))
                             accessors))
     ,@(map 'list #'(lambda (accessor-name)
                      `(defgeneric ,accessor-name (stream)))
            accessors)
     (pushnew ',name *stream-protocols*)))

;; CLIM is not in the business of defining every which kind of stream,
;; so we need to do these "protocol predicates" as generic functions.
;;--- Flush this in favor of the below
(defmacro define-protocol-p-method (method-name class-name)
  `(define-group ,method-name define-protocol-p-method
     (defgeneric ,method-name (object))
     (defmethod ,method-name ((object t))
       nil)
     (defmethod ,method-name ((object ,class-name))
       t)
     (defmethod ,method-name ((stream standard-encapsulating-stream))
       ;; pass it on.
       (,method-name (slot-value stream 'stream)))))

(defmacro define-stream-protocol-class (class-name superclass-names)
  (let ((predicate-name (if (find #\- (string class-name))
                            (fintern "~A-~A" class-name 'p)
                            (fintern "~A~A" class-name 'p))))
    `(progn
       (define-protocol-class ,class-name ,superclass-names)
       (defmethod ,predicate-name ((stream standard-encapsulating-stream))
         (,predicate-name (slot-value stream 'stream))))))

;; This gets bound to the outermost stream when using "encapsulating" streams.
;; We side-step the general delegation ("multiple self") problem in a kludgy way:
;; since CLIM currently never nests encapsulating streams, *ORIGINAL-STREAM* is
;; both the outermost stream and the next-highest encapsulator.  If we wanted to
;; solve the more general delegation problem, we would make all streams have a
;; "delegator" slot, but that isn't really practical; e.g., CLIM does not control
;; the implementation of string and file streams.
;;
;; Here's the sort of situation to watch out for:
;; Suppose we have a stream S that implements a protocol with generic functions
;; ACCEPT and PROMPT.  The ACCEPT method on S calls PROMPT.  Suppose further
;; that S is encapsulated by an encapsulating stream E that specializes PROMPT.
;; Now someone calls ACCEPT on E, which trampolines to ACCEPT on S, which calls
;; PROMPT.  At this point, we want to call PROMPT on E, not on S.  Therefore,
;; the ACCEPT method on S needs to call PROMPT on (OR *ORIGINAL-STREAM* S).
;; This idiom occurs in a small number of places in CLIM, watch out for it.  The
;; clever reader will think "what about multiple levels of encapsulation?"
;; Well, CLIM has managed to luck out of this by virtue of a reasonably complete
;; stream protocol that lets us avoid using encapsulating streams all over the
;; place, so we never need to nest them (yet).
(defvar *original-stream* nil)

;; If you change this to maintain a more complex mapping than a simple
;; binding of *ORIGINAL-STREAM*, change ENCAPSULATING-STREAM too.
(defmacro generate-stream-protocol-trampolines ()
  `(progn
     ,@(writing-clauses
         (dolist (protocol *stream-protocols*)
           (clause `(generate-trampolines
                      ,protocol ,protocol
                      standard-encapsulating-stream
                      `(slot-value ,standard-encapsulating-stream 'stream)
                      *original-stream*))))))

#||
;;; For example:
;;; The protocol name is output-recording-protocol.
;;; The accessors are STREAM-OUTPUT-HISTORY and OUTPUT-RECORDING-STREAM-MUMBLE
(define-stream-protocol output-recording-protocol
  stream-output-history
  output-recording-stream-mumble)

;;; Here is an operation.
(defoperation stream-replay output-recording-protocol
  ((stream output-recording-protocol) output-record &optional region))
||#
