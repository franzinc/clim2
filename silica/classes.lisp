;; -*- mode: common-lisp; package: silica -*-
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
;; $fiHeader: classes.cl,v 1.2 92/01/02 15:09:07 cer Exp $

(in-package :silica)

(defclass port () 
  ((server-path :reader port-server-path)
   (properties :accessor port-properties :initform nil)
   (lock :initform (mp::make-process-lock) :reader port-lock)
   (grafts :initform nil :accessor port-grafts)
   (process :initform nil :accessor port-process)
   (mirror->sheet-table :initform (make-hash-table)
			:reader port-mirror->sheet-table)
   (focus :accessor port-keyboard-focus :initform nil)
   (trace-thing :initform (make-array 10 :fill-pointer 0 :adjustable t)
		:reader port-trace-thing)
   (media-cache :initform nil :accessor port-media-cache)
   (color-cache :initform (make-hash-table) :reader port-color-cache)
   (port :initform nil :accessor port-pointer)
   (mapping-table :initform (make-hash-table :test #'equal))
   (undefined-text-style :initform nil :accessor device-undefined-text-style)
   ))

(defclass sheet ()
  ((port :initform nil :reader sheet-port)
   (graft :initform nil :reader sheet-graft)
   (parent :initform nil
	   :accessor sheet-parent)
   (region :initarg :region
	   :accessor sheet-region
	   :initform (make-bounding-rectangle 0 0 100 100))
   (enabledp :initform nil :accessor sheet-enabled-p)))

(defclass medium ()
  ((port :initarg :port :reader sheet-port)
   (sheet :initarg :sheet :initform nil :accessor medium-sheet)
   (foreground :accessor medium-foreground :initform +black+)
   (background :accessor medium-background :initform +white+)
   (ink :accessor medium-ink :initform +foreground-ink+)
   (text-style :accessor medium-text-style 
	       :initform *default-text-style*)
   (default-text-style :accessor medium-default-text-style 
		       :initform *default-text-style*)
   (merged-text-style :initform *default-text-style*
		      :writer (setf medium-merged-text-style))
   (merged-text-style-valid :initform nil
			    :accessor medium-merged-text-style-valid)
   (line-style :accessor medium-line-style :initform (make-line-style))
   (transformation :accessor medium-transformation 
		   :initarg :transformation
		   :initform +identity-transformation+)
   (region :accessor medium-clipping-region
	   :initarg :region
	   :initform +everywhere+)
   (+y-upward-p :initform nil :accessor medium-+y-upward-p)))


;;; Event types

(defmacro define-event-class (name supers slots &rest rest)
  `(progn
     (defclass ,name ,supers 
       ((type :initform ,(intern (subseq (string name) 
					 0 
					 (search (string '-event) (string name) :from-end t))
				 :keyword)
	      :allocation :class)
	,@slots)
       ,@rest)))

(define-event-class event () 
  ((timestamp :reader event-timestamp
	      :initform 0
	      :initarg :timestamp)))

(defmethod event-type ((event event))
  (slot-value event 'type))


(define-event-class device-event (event) 
  ((sheet :reader event-sheet :initarg :sheet)
   (modifier-state :reader event-modifier-state
		   :initarg :modifiers)))


(define-event-class keyboard-event (device-event)
  ((key-name :reader keyboard-event-key-name :initarg :key-name)
   (character :reader keyboard-event-character :initarg :character)))


(define-event-class key-press-event (keyboard-event) ())
(define-event-class key-release-event (keyboard-event) ())

(define-event-class pointer-event (device-event)
  ((x :reader pointer-event-x :initarg :x)
   (y :reader pointer-event-y :initarg :y)
   (native-x :reader pointer-event-native-x :initarg :native-x)
   (native-y :reader pointer-event-native-y :initarg :native-y)
   (pointer :reader pointer-event-pointer :initarg :pointer
	    :initform nil)
   (button :reader pointer-event-button :initarg :button)))

#+CLIM-1-compatibility
(define-compatibility-function (pointer-event-shift-mask event-modifier-state)
			       (pointer-event)
  (event-modifier-state pointer-event))

(define-event-class pointer-button-event (pointer-event) ())
(define-event-class pointer-button-press-event (pointer-button-event) ())
(define-event-class pointer-button-release-event (pointer-button-event) ())
(define-event-class pointer-click-event (pointer-event) ())
(define-event-class pointer-click-hold-event (click-event) ())
(define-event-class pointer-double-click-event (click-event) ())

(define-event-class pointer-motion-or-boundary-event (pointer-event) ())
(define-event-class pointer-motion-event (pointer-motion-or-boundary-event) ())
(define-event-class pointer-exit-event (pointer-motion-or-boundary-event) ())
(define-event-class pointer-enter-event (pointer-motion-or-boundary-event) ())

(define-event-class window-event (event)
  ((region :reader window-event-region :initarg :region)
   (native-region :reader window-event-native-region :initarg :native-region)
   (mirrored-sheet :reader window-event-mirrored-sheet
		   :reader event-sheet
		   :initarg :sheet)))

(define-event-class window-configuration-event (window-event) ())
(define-event-class window-repaint-event (window-event) ())

(define-event-class timer-event (event) ())

(defclass frame-manager () 
  ((port :reader sheet-port :initarg :port)))
