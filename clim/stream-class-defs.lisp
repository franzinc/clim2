;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader$


(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; Encapsulating-stream-mixin needs to be defined early on, because many methods are
;;; written on this class.

;;; What about xstream (multiple self) problem?
(defclass encapsulating-stream-mixin (fundamental-stream)
	  ((stream :initarg :stream))
    #+excl (:default-initargs :element-type 'extended-char)
  )

(defgeneric encapsulating-stream-p (stream))

(defmethod encapsulating-stream-p ((stream t)) nil)

(defmethod encapsulating-stream-p ((stream encapsulating-stream-mixin)) t)

;;;

(defmethod input-stream-p ((stream encapsulating-stream-mixin))
  (with-slots (stream) stream
    (input-stream-p stream)))

(defmethod output-stream-p ((stream encapsulating-stream-mixin))
  (with-slots (stream) stream
    (output-stream-p stream)))

(defmethod stream-element-type ((stream encapsulating-stream-mixin))
  (with-slots (stream) stream
    (stream-element-type stream)))

;;;

(defmacro define-protocol-p-method (method-name class-name)
  `(define-group ,method-name define-protocol-p-method
     (defgeneric ,method-name (entity))
     (defmethod ,method-name ((entity t))
       nil)
     (defmethod ,method-name ((entity ,class-name))
       t)
     (defmethod ,method-name ((estream encapsulating-stream-mixin))
       ;; pass it on.
       (with-slots (stream) estream
	 (,method-name stream)))))

;;;

;;; This class exists solely to define the output-recording-stream-p method.
;;; Mix this into all output-recording-stream classes.
(defclass fundamental-output-recording ()
     ()
  )

(define-protocol-p-method output-recording-stream-p fundamental-output-recording)

;;; This class exists solely to define the stream-redisplayable-p method.
;;; Mix this into all output-recording-stream classes.

(defclass fundamental-redisplayable-output ()
     ()
  )

(define-protocol-p-method stream-redisplayable-p fundamental-redisplayable-output)

;;; The initialize-instance method for this class is in output-recording

(defclass basic-output-recording (fundamental-output-recording fundamental-redisplayable-output)
     ((draw-p :initarg :draw-p :accessor stream-draw-p)
      (record-p :initarg :record-p :accessor stream-record-p)
      (redisplaying-p :accessor stream-redisplaying-p :initform nil)
      (output-record :accessor output-recording-stream-output-record
		     :initarg :output-record)
      ;; --- this is called a stack at present although
      ;; --- it's really just a location that gets LETF'd.
      (current-output-record-stack
	:initform nil :accessor output-recording-stream-current-output-record-stack)
      (output-record-absolute-position ;; a hint
	:accessor output-recording-stream-output-record-absolute-position
	:initform (make-point 0 0))
      (redisplay-output-record
	:accessor output-recording-stream-redisplay-output-record
	:initform nil)
      ;; output record for items in the margins (e.g., scroll bars)
      (text-output-record-element :initform nil 
				  :accessor output-recording-stream-text-output-record)
      (highlighted-presentation :initform nil
				:accessor output-recording-stream-highlighted-presentation))
  (:default-initargs :draw-p t
		     :record-p t	;!!!
		     :output-record (make-instance 'stream-output-history-mixin)
		     )
  )

;;; Exists to hang window&graphics whoppers on.
(defclass graphics-output-recording ()
     ()
  )

#||
;;; Exists to hang window&output-recording whoppers on.
(defclass window-output-recording ()
     ()
  )

;;; This is the class you mix in (in addition) when you've mixed together
;;; the extended-input-protocol and the window-protocol.
(defclass input-and-window-protocol-intermediary ()
     ()
  )

;;; This is the class you mix in (in addition) when you've mixed together
;;; the extended-output-protocol and the window-protocol.
(defclass output-and-window-protocol-intermediary ()
     ()
  )

;;; This class needs to be mixed in before INPUT-AND-WINDOW-PROTOCOL-INTERMEDIARY
;;; whenever you will also have presentations in margin components, e.g., scroll bars.
;;; The methods in here correct for the viewport w/r/t the scroll bars.
(defclass pointer-interaction-with-margins-intermediary ()
     ()
  )
||#
