;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: stream-class-defs.lisp,v 1.4 92/02/24 13:08:30 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;; It's not really theoretically right to define these methods on
;; STANDARD-ENCAPSULATING-STREAM, but since window streams obey the bounding
;; rectangle protocol, we need them.  It's not practical to define these
;; with DEFINE-STREAM-PROTOCOL and DEFOPERATION.
(defmethod bounding-rectangle* ((stream standard-encapsulating-stream))
  (bounding-rectangle* (slot-value stream 'stream)))

(defmethod bounding-rectangle-set-edges ((stream standard-encapsulating-stream)
					 left top right bottom)
  (bounding-rectangle-set-edges (slot-value stream 'stream) left top right bottom))

(defmethod bounding-rectangle-set-position ((stream standard-encapsulating-stream) x y)
  (bounding-rectangle-set-position (slot-value stream 'stream) x y))

(defmethod bounding-rectangle-set-size ((stream standard-encapsulating-stream) width height)
  (bounding-rectangle-set-size (slot-value stream 'stream) width height))


(defmethod input-stream-p ((stream standard-encapsulating-stream))
  (with-slots (stream) stream
    (input-stream-p stream)))

(defmethod output-stream-p ((stream standard-encapsulating-stream))
  (with-slots (stream) stream
    (output-stream-p stream)))

(defmethod stream-element-type ((stream standard-encapsulating-stream))
  (with-slots (stream) stream
    (stream-element-type stream)))



;;; This class exists solely to define the OUTPUT-RECORDING-STREAM-P method.
;;; Mix this into all output-recording-stream classes.
(define-stream-protocol-class output-recording-stream ())

;;; This class exists solely to define the redisplayable-stream-p method.
;;; Mix this into all output-recording-stream classes.
(define-stream-protocol-class redisplayable-stream ())

;;; The methods for this class are in OUTPUT-RECORDING-PROTOCOL
(defclass output-recording-mixin
	  (output-recording-stream redisplayable-stream)
    ((draw-p :initarg :draw-p 
	     :accessor stream-drawing-p)
     (record-p :initarg :record-p
	       :accessor stream-recording-p)
     (redisplaying-p :initform nil
		     :accessor stream-redisplaying-p)
     (output-record :initarg :output-record
		    :accessor stream-output-history)
     (current-output-record-stack :initform nil
				  :accessor stream-current-output-record)
     (output-record-absolute-position :initform (make-point 0 0)
				      :accessor stream-output-history-position)
     (redisplay-output-record :initform nil
			      :accessor stream-current-redisplay-record)
     (text-output-record :initform nil 
			 :accessor stream-text-output-record)
     (highlighted-presentation :initform nil
			       :accessor stream-highlighted-presentation))
  (:default-initargs :draw-p t :record-p t	;!!!
		     :output-record (make-instance 'standard-tree-output-history)))


#+CLIM-1-compatibility
(progn
(define-compatibility-function (output-recording-stream-output-history
				stream-output-history) 
			       (stream)
  (stream-output-history stream))

(define-compatibility-function (output-recording-stream-current-output-record-stack
				stream-current-output-record)
			       (stream)
  (stream-current-output-record stream))

(define-compatibility-function (output-recording-stream-replay stream-replay)
			       (stream &optional region)
  (stream-replay stream region))
)	;#+CLIM-1-compatibility


;;; Exists to hang window&output-recording :around methods on.
(defclass window-output-recording () ())

;;; Exists to hang window&graphics :around methods on.
(defclass graphics-output-recording () ())

;;; This is the class you mix in (in addition) when you've mixed together
;;; the extended-input-protocol and the window-protocol.
(defclass input-and-window-protocol-intermediary () ())

;;; This is the class you mix in (in addition) when you've mixed together
;;; the extended-output-protocol and the window-protocol.
(defclass output-and-window-protocol-intermediary () ())

;;; This is the class you mix in (in addition) when you've mixed together
;;; the extended-output-protocol and output-recording-mixin.
(defclass output-and-recording-protocol-intermediary () ())

