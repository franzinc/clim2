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
;; $fiHeader: gadgets.cl,v 1.4 92/01/06 20:44:20 cer Exp Locker: cer $

(in-package :silica)


(defclass gadget ()
	  ((id :initarg :id :reader gadget-id :initform nil)
	   (client :initarg :client :initform nil :accessor gadget-client)))

(defclass value-gadget (gadget) ())

(defmethod value-changed-callback ((v gadget) (client t) (id t) (value t))
  ())

(defgeneric gadget-value (gadget))

(defgeneric (setf gadget-value) (gadget))

(defclass action-gadget (gadget) ())

(defmethod activate-callback ((v gadget) (client t) (id t))
  ())

;;;;;;;;;;;;;;;;;;;; The intent is that the real implementations
;;;;;;;;;;;;;;;;;;;; inherit from these

;; slider

(defclass slider () ())

;; push-button

(defclass silica::push-button () ())

;; toggle-button
;; scroll-bar


(defclass scrollbar ()
	  ((current-value :initform nil)
	   (current-size :initform nil)))

;; Caption
;; option menu
;; label

;; radio-box [exclusive-choice]
;; .. [inclusive-choice]

(defclass silica::radio-box () ())

;; menu-bar


;; [cascade]

;; As well as the callback mechanism
;; a we want to specify a binding to commands
;; text-field

;; text-edit

(defclass text-field () ())

;; toggle buttton

(defclass toggle-button () ())

;;; Viewport

(defclass silica::viewport () ())

;;; Then there is the layout stuff and scrolling macros

(defmacro scrolling (options contents)
  `(realize-pane 'scroller-pane
		 :contents ,contents
		 ,@options))

(defmethod realize-pane-internal ((framem frame-manager) (frame application-frame)
				  name &rest options)
  (apply #'make-instance 
	 name
	 :frame frame
	 :frame-manager framem
	 options))

;; Callbacks on widgets generate these events

(defclass gadget-event (event) 
	  ((gadget :initarg :gadget :reader event-sheet)))

(defclass value-changed-gadget-event (gadget-event) 
  ((value :initarg :value :reader event-value)))

(defmethod handle-event ((gadget value-gadget) (event value-changed-gadget-event))
  (value-changed-callback gadget (gadget-client gadget) (gadget-id gadget) (slot-value event 'value)))

(defclass activate-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget  action-gadget) (event activate-gadget-event))
  (activate-callback gadget (gadget-client gadget) (gadget-id gadget)))

;; Does this make sense

(defclass foreground-background-and-text-style-mixin ()
	  ((foreground :initform nil :initarg :foreground)
	   (background :initform nil :initarg :background)
	   (text-style :initform nil :initarg :text-style)))

;;; Do these have readers and writers?
