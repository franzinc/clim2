;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: gadgets.lisp,v 1.9 92/03/04 16:19:41 cer Exp Locker: cer $

"Copyright (c) 1991, 1992 by Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)


;;; Does this make sense
;;; We want to be able to specify visual attributes of gadgets

(defclass foreground-background-and-text-style-mixin ()
    ((foreground :initform nil :initarg :foreground)
     (background :initform nil :initarg :background)
     (text-style :initform *default-text-style* :initarg :text-style)))

(defmethod initialize-instance :after ((pane foreground-background-and-text-style-mixin)
				       &key &allow-other-keys)
  (with-slots (text-style) pane
    ;; Convert the text style if necesary
    (etypecase text-style
      (cons (setq text-style (parse-text-style text-style)))
      (text-style nil))))


(defclass gadget (foreground-background-and-text-style-mixin)
    ((id :initarg :id :reader gadget-id :initform nil)
     (client :initarg :client :initform nil :accessor gadget-client)
     (armed-callback :initarg :armed-callback :initform nil
		     :reader gadget-armed-callback)
     (disarmed-callback :initarg :disarmed-callback :initform nil
			:reader gadget-disarmed-callback)))

(defmethod armed-callback ((gadget gadget) (client t) (id t))
  (when (gadget-armed-callback gadget)
    (invoke-callback-function (gadget-armed-callback gadget) gadget)))

(defmethod disarmed-callback ((gadget gadget) (client t) (id t))
  (when (gadget-disarmed-callback gadget)
    (invoke-callback-function (gadget-disarmed-callback gadget) gadget)))

(defun invoke-callback-function (function &rest args)
  (declare (dynamic-extent args))
  (if (consp function)
      (with-stack-list* (args args (cdr function))
	(apply (car function) args))
      (apply function args)))


(defclass value-gadget (gadget) 
    ((value :initarg :value :initform nil
	    :reader gadget-initial-value)
     (value-changed-callback :initarg :value-changed-callback :initform nil
			     :reader gadget-value-changed-callback)))

(defmethod value-changed-callback ((gadget value-gadget) (client t) (id t) value)
  (when (gadget-value-changed-callback gadget)
    (invoke-callback-function (gadget-value-changed-callback gadget) gadget value)))

(defgeneric gadget-value (gadget))

(defmethod gadget-value ((gadget value-gadget))
  (with-slots (value) gadget
    value))

(defmethod value-changed-callback :before ((gadget value-gadget) client id new-value)
  (declare (ignore id client))
  (with-slots (value) gadget
    (setf value new-value)))

(defgeneric (setf gadget-value) (nv gadget))

(defmethod (setf gadget-value) (nv (gadget value-gadget))
  (declare (ignore nv)))

(defmethod (setf gadget-value) :after (nv (gadget value-gadget))
  (with-slots (value) gadget
    (setf value nv)))


(defclass action-gadget (gadget) 
    ((activate-callback :initarg :activate-callback :initform nil
			:reader gadget-activate-callback)))  

(defmethod activate-callback ((gadget action-gadget) (client t) (id t))
  (when (gadget-activate-callback gadget)
    (invoke-callback-function (gadget-activate-callback gadget) gadget)))


;;; Basic gadgets-mixins

(defclass oriented-gadget ()
    ((orientation :initarg :orientation
		  :reader gadget-orientation))
  (:default-initargs :orientation :horizontal))


(defclass labelled-gadget ()
    ((label :initarg :label
	    :accessor gadget-label)
     (alignment :initarg :halign ;;--- compat hack
		:initarg :alignment
		:accessor gadget-alignment))
  (:default-initargs :label nil))

;;--- Do the right thing
#---ignore
(defmethod compute-gadget-label-size ((pane labelled-gadget))
  (values 50 20))

#+++ignore
(defun compute-gadget-label-size ((pane labelled-gadget))
  (let ((text (gadget-label pane))
	(text-style (slot-value pane 'text-style)))
    (with-sheet-medium (medium pane)
      (values (+ 4 (stream-string-width text text-style medium))
	      (+ 4 (text-style-height text-style medium))))))


;;; The intent is that the real implementations inherit from these

;;; Slider
(defclass slider
	  (value-gadget oriented-gadget labelled-gadget)
    ((drag-callback :initarg :drag-callback :initform nil
		    :reader slider-drag-callback)))

(defmethod drag-callback ((gadget slider) (client t) (id t) value)
  (when (slider-drag-callback gadget)
    (invoke-callback-function (slider-drag-callback gadget) gadget value)))


;; Scroll bar
(defclass scrollbar
	  (value-gadget oriented-gadget)
    ((current-value :initform nil)
     (current-size :initform nil)
     (drag-callback :initarg :drag-callback :initform nil
		    :reader scrollbar-drag-callback)))

(defmethod drag-callback ((gadget scrollbar) (client t) (id t) value)
  (when (scrollbar-drag-callback gadget)
    (invoke-callback-function (scrollbar-drag-callback gadget) gadget value)))


;;; Push-button
(defclass push-button 
	  (action-gadget labelled-gadget) 
	  ())

(defclass label-pane 
	  (labelled-gadget) 
    ())


;; Toggle button
(defclass toggle-button 
	  (value-gadget labelled-gadget) 
    ((indicator-type :initarg :indicator-type :initform :some-of
		     :reader gadget-indicator-type)))


;; Menu button
(defclass menu-button 
	  (value-gadget labelled-gadget) 
    ((indicator-type :initarg :indicator-type :initform :some-of
		     :reader gadget-indicator-type)))


;;; Caption
;;; option menu
;;; label

;;; Radio box [exclusive-choice] .. [inclusive-choice]
(defclass radio-box 
	  (value-gadget oriented-gadget) 
    ((selections :initform nil 
		 :reader radio-box-selections)
     (current-selection :initform nil
			:initarg :current-selection
			:accessor radio-box-current-selection)))

(defmethod initialize-instance :after ((rb radio-box) &key choices)
  (let ((fr (pane-frame rb)))
    (with-look-and-feel-realization ((frame-manager fr) fr)
      (dolist (choice choices)
	(realize-pane 'toggle-button 
		      :value (equal (radio-box-current-selection rb) choice)
		      :label (string choice)
		      :id choice
		      :parent rb)))))
;;; Menu-bar


;;; [cascade]


;;; Text edit
;; As well as the callback mechanism a we want to specify a binding to commands
(defclass text-field 
	  (value-gadget action-gadget) 
    ())


;;; Viewport

;;--- CLIM 0.9 has this VIEWPORT-LAYOUT-MIXIN -- do we need it?
(defclass viewport
	  (sheet-single-child-mixin
	   sheet-permanently-enabled-mixin
	   wrapping-space-mixin
	   pane)
    ;; This describes the region that we are displaying
    ((viewport-region :accessor viewport-viewport-region)))

(defmethod initialize-instance :after ((viewport viewport) &key)
  (multiple-value-bind (width height)
      (bounding-rectangle-size viewport)
    (setf (viewport-viewport-region viewport)
	  (make-bounding-rectangle 0 0 width height))))

(defmethod allocate-space ((viewport viewport) width height)
  ;; We do nothing to the child of a viewport
  nil)

(defmethod allocate-space :after ((viewport viewport) width height)
  (bounding-rectangle-set-size
    (viewport-viewport-region viewport) width height)
  (update-scrollbars viewport)
  (viewport-region-changed (sheet-child viewport) viewport))


;;; Then there is the layout stuff and scrolling macros

(defmacro scrolling (options &body contents)
  `(realize-pane 'scroller-pane
		 :contents ,@contents
		 ,@options))

(defmethod realize-pane-1 ((framem standard-frame-manager) frame name &rest options)
  (declare (dynamic-extent options))
  (apply #'make-instance 
	 name
	 :frame frame :frame-manager framem
	 options))

;; Callbacks on widgets generate these events

(defclass gadget-event (event) 
    ((gadget :initarg :gadget :reader event-sheet)))

(defclass value-changed-gadget-event (gadget-event) 
    ((value :initarg :value :reader event-value)))

(defmethod handle-event ((gadget value-gadget) (event value-changed-gadget-event))
  (value-changed-callback
    gadget (gadget-client gadget) (gadget-id gadget) (slot-value event 'value)))

(defclass activate-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget  action-gadget) (event activate-gadget-event))
  (activate-callback gadget (gadget-client gadget) (gadget-id gadget)))

;;; Do these have readers and writers?
