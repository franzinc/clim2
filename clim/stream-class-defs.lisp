;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: stream-class-defs.lisp,v 1.8 92/07/01 15:47:02 cer Exp $

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
  (input-stream-p (slot-value stream 'stream)))

(defmethod output-stream-p ((stream standard-encapsulating-stream))
  (output-stream-p (slot-value stream 'stream)))

(defmethod stream-element-type ((stream standard-encapsulating-stream))
  (stream-element-type (slot-value stream 'stream)))



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


;; For any window-specific output recording methods
(defclass window-output-recording () ())

;; For any window-specific graphics output recording methods
(defclass graphics-output-recording () ())
