;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: gadgets.lisp,v 1.15 92/04/10 14:26:32 cer Exp Locker: cer $

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
      (apply (car function) (append args (cdr function)))
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

(defgeneric (setf gadget-value) (nv gadget &key))

(defmethod (setf gadget-value) (nv (gadget value-gadget) &key)
  (declare (ignore nv)))

;;-- What is the correct default?

(defmethod (setf gadget-value) :after (nv (gadget value-gadget) &key invoke-callback)
  (with-slots (value) gadget
    (setf value nv))
  (when invoke-callback
    (value-changed-callback
     gadget 
     (gadget-client gadget)
     (gadget-id gadget)
     nv)))


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
     (alignment :initarg :x-align
		:accessor gadget-alignment))
  (:default-initargs :label nil :x-align :center))

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

;;;- We might want a way of changing the range and the value together.

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
  (when (slider-drag-callback gadget)
    (invoke-callback-function (slider-drag-callback gadget) gadget value)))


;; Scroll bar

(defclass scrollbar
	  (value-gadget range-gadget-mixin oriented-gadget)
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
     ;;--- think about this
     (value :initform nil
	    :initarg :current-selection
	    :initarg :current
	    :accessor radio-box-current-selection)))

(defmethod initialize-instance :after ((rb radio-box) &key choices)
  (let ((fr (pane-frame rb)))
    (with-look-and-feel-realization ((frame-manager fr) fr)
      (dolist (choice choices)
	(make-pane 'toggle-button 
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
    (multiple-value-bind
	(cwidth cheight)
	(bounding-rectangle-size child)
      (when (or (< cwidth width)
		(< cheight height))
	(resize-sheet* child
		       (max width cwidth)
		       (max height cheight))))))

(defmethod compose-space ((viewport viewport) &key width height)
  (declare (ignore width height))
  (let ((sr (call-next-method)))
    (setq sr (silica::copy-space-requirement sr))
    (setf (space-requirement-min-width sr) 0
	  (space-requirement-min-height sr) 0)
    sr))

(defmethod allocate-space :after ((viewport viewport) width height)
  (bounding-rectangle-set-size
   (viewport-viewport-region viewport) width height)
  ;; At this point  it might make sense to move the viewport if it is
  ;; big enough to contain the contents
  (update-scrollbars viewport)
  (viewport-region-changed (sheet-child viewport) viewport))

;;--- Work on this


(defmethod note-sheet-region-changed :around ((viewport viewport) &key port-did-it)
  (declare (ignore port-did-it))
  (multiple-value-bind
      (changedp 
       hscrollbar hscrollbar-enabled-p
       vscrollbar vscrollbar-enabled-p)
      (compute-dynamic-scrollbar-values viewport)
    (if changedp
	(update-dynamic-scrollbars 
	 viewport
	 changedp
	 hscrollbar hscrollbar-enabled-p
	 vscrollbar vscrollbar-enabled-p)
      (call-next-method))))


(defun update-dynamic-scrollbars (viewport
				  changedp
				  hscrollbar hscrollbar-enabled-p
				  vscrollbar vscrollbar-enabled-p)
  (when changedp
    (when hscrollbar
      (setf (sheet-enabled-p hscrollbar) hscrollbar-enabled-p))
    (when vscrollbar
      (setf (sheet-enabled-p vscrollbar) vscrollbar-enabled-p))
    (let ((table (sheet-parent viewport)))
      (clear-space-requirement-caches-in-tree table)
      (multiple-value-call
	  #'allocate-space
	table
	(bounding-rectangle-size table)))))

(defun compute-dynamic-scrollbar-values (viewport)
  (with-slots ((hscrollbar horizontal-scrollbar )
	       (vscrollbar vertical-scrollbar)
	       (scroll-bar-policy scroll-bars))
      (sheet-parent (sheet-parent viewport))
    (if (eq scroll-bar-policy :dynamic)
	(multiple-value-bind
	    (vwidth vheight) (bounding-rectangle-size viewport)
	  (multiple-value-bind
	      (cwidth cheight) 
	      (bounding-rectangle-size 
	       (viewport-contents-extent viewport))
	    (let ((ohenp (sheet-enabled-p hscrollbar))
		  (ovenp (sheet-enabled-p vscrollbar))
		  (nhenp (> cwidth vwidth))
		  (nvenp (> cheight vheight)))
	      (values
	       (not (and (eq ohenp nhenp)
			 (eq ovenp nvenp)))
	       (and (not (eq ohenp nhenp)) hscrollbar)
	       nhenp
	       (and (not (eq ovenp nvenp)) vscrollbar)
	       nvenp))))
      (values nil nil nil nil nil))))

(defun viewport-contents-extent (viewport)
  (let ((c (sheet-child viewport)))
    (if (typep c 'clim-internals::output-recording-mixin)
	(stream-output-history c)
      c)))

;;--- Need something in update-scrollbars also.
	    
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
