;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: gadgets.lisp,v 1.18 92/04/21 16:12:40 cer Exp Locker: cer $

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
			      :reader gadget-disarmed-callback)
	   (active :initarg :active :accessor gadget-active-p))
  (:default-initargs :active t))

(defmethod armed-callback ((gadget gadget) (client t) (id t))
  (invoke-callback-function (gadget-armed-callback gadget) gadget))

(defmethod disarmed-callback ((gadget gadget) (client t) (id t))
  (invoke-callback-function (gadget-disarmed-callback gadget) gadget))

(defun invoke-callback-function (function &rest args)
  (declare (dynamic-extent args))
  (when function
    (if (consp function)
	(apply (car function) (append args (cdr function)))
	(apply function args))))


(defclass value-gadget (gadget) 
    ((value :initarg :value :initform nil
	    :reader gadget-initial-value)
     (value-changed-callback :initarg :value-changed-callback :initform nil
			     :reader gadget-value-changed-callback)))

(defmethod value-changed-callback ((gadget value-gadget) (client t) (id t) value)
  (invoke-callback-function (gadget-value-changed-callback gadget) gadget value))

(defgeneric gadget-value (gadget))

(defmethod gadget-value ((gadget value-gadget))
  (with-slots (value) gadget
    value))

(defmethod value-changed-callback :before ((gadget value-gadget) client id new-value)
  (declare (ignore id client))
  (with-slots (value) gadget
    (setf value new-value)))

(defgeneric (setf gadget-value) (nv gadget &key))

(defmethod (setf gadget-value) (nv (gadget value-gadget) &key)
  (declare (ignore nv)))

;;--- What is the correct default for INVOKE-CALLBACK?
(defmethod (setf gadget-value) :after (nv (gadget value-gadget) &key invoke-callback)
  (with-slots (value) gadget
    (setf value nv))
  (when invoke-callback
    (value-changed-callback gadget (gadget-client gadget) (gadget-id gadget) nv)))


(defclass action-gadget (gadget) 
    ((activate-callback :initarg :activate-callback :initform nil
			:reader gadget-activate-callback)))  

(defmethod activate-callback ((gadget action-gadget) (client t) (id t))
  (invoke-callback-function (gadget-activate-callback gadget) gadget))


;;; Basic gadgets-mixins

(defclass oriented-gadget ()
    ((orientation :initarg :orientation
		  :reader gadget-orientation))
  (:default-initargs :orientation :horizontal))


(defclass labelled-gadget ()
    ((label :initarg :label
	    :accessor gadget-label)
     (alignment :initarg :align-x
		:accessor gadget-alignment))
  (:default-initargs :label nil :align-x :center))

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

;;--- We might want a way of changing the range and the value together.
(defclass range-gadget-mixin ()
    ((min-value :initarg :min-value :accessor gadget-min-value)
     (max-value :initarg  :max-value :accessor gadget-max-value))
  (:default-initargs :min-value 0.0 :max-value 1.0))

(defmethod gadget-range ((gadget range-gadget-mixin))
  (- (gadget-max-value gadget)
     (gadget-min-value gadget)))

(defmethod gadget-range* ((gadget range-gadget-mixin))
  (values (gadget-min-value gadget)
	  (gadget-max-value gadget)))


;;; The intent is that the real implementations inherit from these

;;; Slider
(defclass slider
	  (value-gadget oriented-gadget range-gadget-mixin labelled-gadget)
    ((drag-callback :initarg :drag-callback :initform nil
		    :reader slider-drag-callback)
     (show-value-p :initarg :show-value-p 
		   :accessor gadget-show-value-p))
  (:default-initargs :show-value-p nil))


(defmethod drag-callback ((gadget slider) (client t) (id t) value)
  (invoke-callback-function (slider-drag-callback gadget) gadget value))


;; Scroll bar

(defclass scroll-bar
	  (value-gadget range-gadget-mixin oriented-gadget)
    ((current-value :initform nil :accessor scroll-bar-current-value)
     (current-size :initform nil :accessor scroll-bar-current-size)
     (drag-callback :initarg :drag-callback :initform nil
		    :reader scroll-bar-drag-callback)))

(defmethod drag-callback ((gadget scroll-bar) (client t) (id t) value)
  (invoke-callback-function (scroll-bar-drag-callback gadget) gadget value))


;;; Push-button
(defclass push-button 
	  (action-gadget labelled-gadget) 
    ())


;; Toggle button
(defclass toggle-button (value-gadget labelled-gadget) 
	  ((indicator-type :initarg :indicator-type :initform :some-of
			   :type (member :some-of :one-of)
			   :reader gadget-indicator-type)))


;; Menu button
(defclass menu-button 
	  (value-gadget labelled-gadget) 
    ((indicator-type :initarg :indicator-type :initform :some-of
		     :reader gadget-indicator-type)))


(defclass label-pane
	  (gadget labelled-gadget)
    ())


;;; Caption
;;; option menu
;;; label

;;; Radio box [exclusive-choice] .. [inclusive-choice]
(defclass radio-box 
	  (value-gadget oriented-gadget) 
    ((selections :initform nil 
		 :reader radio-box-selections)
     ;;--- think about this...
     (value :initform nil
 	    :initarg :current-selection
 	    :accessor radio-box-current-selection)))

(defmethod initialize-instance :after ((rb radio-box) &key choices)
  (let ((frame (pane-frame rb)))
    (with-look-and-feel-realization ((frame-manager frame) frame)
      (dolist (choice choices)
	(make-pane 'toggle-button 
		   :value (equal (radio-box-current-selection rb) choice)
		   :label (if (stringp choice)
			      (string choice)
			      (gadget-label choice))
		   :indicator-type :one-of
		   :id choice
		   :parent rb)))))


;; Check-box

(defclass check-box 
	  (value-gadget oriented-gadget) 
    ((selections :initform nil 
		 :reader check-box-selections)
     ;;--- think about this...
     (value :initform nil
 	    :initarg :current-selection
 	    :accessor check-box-current-selection)))

(defmethod initialize-instance :after ((rb check-box) &key choices)
  (let ((frame (pane-frame rb)))
    (with-look-and-feel-realization ((frame-manager frame) frame)
      (dolist (choice choices)
	(make-pane 'toggle-button 
		   :value (equal (check-box-current-selection rb) choice)
		   :label (if (stringp choice)
			      (string choice)
			    (gadget-label choice))
		   :indicator-type :some-of
		   :id choice
		   :parent rb)))))


;;; Menu-bar


;;; [cascade]


;;; Text edit
;; As well as the callback mechanism a we want to specify a binding to commands
(defclass text-field 
	  (value-gadget action-gadget) 
    ())

(defclass text-editor (text-field) 
    ((ncolumns :initarg :ncolumns
	       :accessor gadget-columns)
     (nlines :initarg :nlines
	     :accessor gadget-lines))
  (:default-initargs :ncolumns 1 :nlines 1))


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
  ;;-- Make sure the child is at least as big as the viewport
  ;;-- Viewport-region-changed actually does this also
  (let* ((child (sheet-child viewport)))
    (multiple-value-bind (cwidth cheight)
	(bounding-rectangle-size child)
      (when (or (< cwidth width)
		(< cheight height))
	(resize-sheet* child
		       (max width cwidth)
		       (max height cheight))))))

(defmethod compose-space ((viewport viewport) &key width height)
  (declare (ignore width height))
  (let ((sr (call-next-method)))
    (setq sr (copy-space-requirement sr))
    (setf (space-requirement-min-width sr) 0
	  (space-requirement-min-height sr) 0)
    sr))

(defmethod allocate-space :after ((viewport viewport) width height)
  (bounding-rectangle-set-size
    (viewport-viewport-region viewport) width height)
  ;; At this point  it might make sense to move the viewport if it is
  ;; big enough to contain the contents
  (update-scroll-bars viewport)
  (viewport-region-changed (sheet-child viewport) viewport))

;;--- Work on this

(defun update-dynamic-scroll-bars (sp 
				   changedp
				   hscroll-bar hscroll-bar-enabled-p
				   vscroll-bar vscroll-bar-enabled-p
				   &optional relayout)
  (when changedp
    (when hscroll-bar
      (setf (sheet-enabled-p hscroll-bar) hscroll-bar-enabled-p))
    (when vscroll-bar
      (setf (sheet-enabled-p vscroll-bar) vscroll-bar-enabled-p))
    
    (when (or (and hscroll-bar (not hscroll-bar-enabled-p))
	      (and vscroll-bar (not vscroll-bar-enabled-p)))
      (let* ((contents (slot-value sp 'contents))
	     (c-extent (viewport-contents-extent
			(pane-viewport contents))))
	(multiple-value-bind
	    (vx vy) (window-viewport-position contents)
	  
	  (window-set-viewport-position
	   contents
	   (if (and hscroll-bar (not hscroll-bar-enabled-p))
	       (bounding-rectangle-min-x c-extent)
	     vx)
	   (if (and vscroll-bar (not vscroll-bar-enabled-p))
	       (bounding-rectangle-min-y c-extent)
	     vy)))))

    (clear-space-requirement-caches-in-tree sp)
    (when relayout
      (multiple-value-bind
	  (w h)
	  (bounding-rectangle-size sp)
	  (allocate-space sp w h)))))

(defun compute-dynamic-scroll-bar-values (sp)
  (let* ((hscroll-bar (scroller-pane-horizontal-scroll-bar sp))
	 (vscroll-bar (scroller-pane-vertical-scroll-bar sp))
	 (scroll-bar-policy (scroller-pane-scroll-bar-policy sp)))
    (if (eq scroll-bar-policy :dynamic)
	(multiple-value-bind
	    (vwidth vheight) (bounding-rectangle-size sp)
	  (multiple-value-bind
	      (cwidth cheight) 
	      (bounding-rectangle-size 
	       (viewport-contents-extent (slot-value sp 'viewport)))
	    (let ((ohenp (sheet-enabled-p hscroll-bar))
		  (ovenp (sheet-enabled-p vscroll-bar))
		  (nhenp (> cwidth vwidth))
		  (nvenp (> cheight vheight)))
	      (values
	       (not (and (eq ohenp nhenp)
			 (eq ovenp nvenp)))
	       (and (not (eq ohenp nhenp)) hscroll-bar)
	       nhenp
	       (and (not (eq ovenp nvenp)) vscroll-bar)
	       nvenp))))
      (values nil nil nil nil nil))))

(defun viewport-contents-extent (viewport)
  (let ((c (sheet-child viewport)))
    (stream-output-history c)))

;;--- Need something in update-scroll-bars also.
l	    
;;; Then there is the layout stuff and scrolling macros

(defmacro scrolling (options &body contents)
  `(make-pane 'scroller-pane
	      :contents ,@contents
	      ,@options))

(defmethod make-pane-1 ((framem standard-frame-manager) frame name &rest options)
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

;;; Activation/Deactivation protocol

(defmethod activate-gadget ((gadget gadget))
  (unless (gadget-active-p gadget)
    (setf (gadget-active-p gadget) t)
    (note-gadget-activated (gadget-client gadget) gadget)))

(defmethod note-gadget-activated ((client t) (gadget gadget))
  (port-note-gadget-activated (port gadget) gadget))

(defmethod deactivate-gadget ((gadget gadget))
  (when (gadget-active-p gadget)
    (setf (gadget-active-p gadget) nil)
    (note-gadget-deactivated (gadget-client gadget) gadget)))

(defmethod note-gadget-deactivated ((client t) (gadget gadget))
  (port-note-gadget-deactivated (port gadget) gadget))
