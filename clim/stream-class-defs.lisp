;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;; It's not really theoretically right to define these methods on
;; STANDARD-ENCAPSULATING-STREAM, but since window streams obey the bounding
;; rectangle protocol, we need them.  It's not practical to define these
;; with DEFINE-STREAM-PROTOCOL and DEFOPERATION.
(defmethod bounding-rectangle* ((stream standard-encapsulating-stream))
  (bounding-rectangle* (encapsulating-stream-stream stream)))

(defmethod bounding-rectangle-set-edges ((stream standard-encapsulating-stream)
                                         left top right bottom)
  (bounding-rectangle-set-edges (encapsulating-stream-stream stream)
                                left top right bottom))

(defmethod bounding-rectangle-set-position ((stream standard-encapsulating-stream) x y)
  (bounding-rectangle-set-position (encapsulating-stream-stream stream) x y))

(defmethod bounding-rectangle-set-size ((stream standard-encapsulating-stream) width height)
  (bounding-rectangle-set-size (encapsulating-stream-stream stream) width height))


(defmethod input-stream-p ((stream standard-encapsulating-stream))
  (input-stream-p (encapsulating-stream-stream stream)))

(defmethod output-stream-p ((stream standard-encapsulating-stream))
  (output-stream-p (encapsulating-stream-stream stream)))

(defmethod stream-element-type ((stream standard-encapsulating-stream))
  (stream-element-type (encapsulating-stream-stream stream)))



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
                               :accessor stream-highlighted-presentation)
     #+allegro
     ;; Support for allegro presentations
     (excl-presentation-stack :initform nil
                              :accessor
                              stream-excl-presentation-stack)
     #+allegro
     (excl-recording-p :accessor stream-excl-recording-p
                       :initform nil
                       :initarg :excl-recording-p))
  (:default-initargs :draw-p t :record-p t        ;!!!

                     :output-record 
                     ;; this used to be standard-tree-output-history
                     ;; but because of longstanding bugs in
                     ;; coordinate sorted sets, we should avoid this
                     ;; for the default (cim 11/30/93)
                     (make-instance 'standard-sequence-output-history)))


;; For any window-specific output recording methods
(defclass window-output-recording () ())

;; For any window-specific graphics output recording methods
(defclass graphics-output-recording () ())
