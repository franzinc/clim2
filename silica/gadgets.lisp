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
;; $fiHeader: gadgets.cl,v 1.6 92/01/17 17:49:45 cer Exp $

(in-package :silica)

;;; Does this make sense
;;; We want to be able to specify visual attributes of gadgets

(defclass foreground-background-and-text-style-mixin ()
	  ((foreground :initform nil :initarg :foreground)
	   (background :initform nil :initarg :background)
	   (text-style :initform nil :initarg :text-style)))

(defclass gadget (foreground-background-and-text-style-mixin)
	  ((id :initarg :id :reader gadget-id :initform nil)
	   (client :initarg :client :initform nil :accessor gadget-client)))

(defclass value-gadget (gadget) 
	  ((value :initarg :value
		  :initform nil
		  :reader gadget-initial-value)
	   (value-changed-callback 
	    :initarg :value-changed-callback
	    :initform nil
	    :reader gadget-value-changed-callback)))

(defmethod value-changed-callback ((gadget gadget) (client t) (id t) (value t))
  (when (gadget-value-changed-callback gadget)
    (invoke-callback-function
     (gadget-value-changed-callback gadget)
     gadget
     value)))

(defun invoke-callback-function (function &rest args)
  (if (consp function)
      (apply (car function) (append args (cdr function)))
    (apply function args)))

(defgeneric gadget-value (gadget))

(defmethod gadget-value ((gadget gadget))
  (with-slots (value) gadget
    value))

(defmethod value-changed-callback :before ((gadget gadget) client id new-value)
  (declare (ignore id client))
  (with-slots (value) gadget
    (setf value new-value)))

(defgeneric (setf gadget-value) (nv gadget))

(defmethod (setf gadget-value) :after (nv (gadget gadget))
  (with-slots (value) gadget
    (setf value nv)))

;;;;;;;;;;;;;

(defclass action-gadget (gadget) 
	  ((action-callback 
	    :initarg :action-callback
	    :initform nil
	    :reader gadget-action-callback)))  

(defmethod activate-callback ((gadget gadget) (client t) (id t))
  (when (gadget-action-callback gadget)
    (invoke-callback-function
     (gadget-action-callback gadget)
     gadget)))

;;; Basic gadgets-mixins

(defclass oriented-gadget ()
	  ((orientation :initarg :orientation
			:reader gadget-orientation))
  
  (:default-initargs :orientation :horizontal))

(defclass labelled-gadget ()
	  ((label :initarg :label
		  :reader gadget-label))
  (:default-initargs :label nil))


;;;;;;;;;;;;;;;;;;;; The intent is that the real implementations
;;;;;;;;;;;;;;;;;;;; inherit from these

;; slider

(defclass slider (value-gadget oriented-gadget labelled-gadget)
	  ())

;; push-button

(defclass push-button (action-gadget labelled-gadget) 
	  ())

;; scroll-bar


(defclass scrollbar (value-gadget oriented-gadget)
	  ((current-value :initform nil)
	   (current-size :initform nil)))

;; Caption
;; option menu
;; label

;; radio-box [exclusive-choice]
;; .. [inclusive-choice]

(defclass radio-box (value-gadget oriented-gadget) 
	  ())

;; menu-bar


;; [cascade]

;; As well as the callback mechanism
;; a we want to specify a binding to commands
;; text-field

;; text-edit

(defclass text-field (value-gadget action-gadget) 
	  ())

;; toggle buttton

(defclass toggle-button (value-gadget labelled-gadget) 
	  ((indicator-type :initarg :indicator-type :initform :some-of
			   :reader gadget-indicator-type)))

;;; Viewport

(defclass silica::viewport () ())

;;; Then there is the layout stuff and scrolling macros

(defmacro scrolling (options contents)
  `(realize-pane 'scroller-pane
		 :contents ,contents
		 ,@options))

(defmethod realize-pane-1 ((framem frame-manager) frame name &rest options)
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
  (value-changed-callback gadget 
			  (gadget-client gadget)
			  (gadget-id gadget)
			  (slot-value event 'value)))

(defclass activate-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget  action-gadget) (event activate-gadget-event))
  (activate-callback gadget (gadget-client gadget) (gadget-id gadget)))

;;; Do these have readers and writers?
