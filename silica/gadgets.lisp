;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: gadgets.lisp,v 1.25 92/06/29 14:04:36 cer Exp Locker: cer $

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

(defun-inline invoke-callback-function (function &rest args)
  (declare (dynamic-extent args))
  (if (consp function)
      (apply (car function) (append args (cdr function)))
      (apply function args)))


(defclass gadget (foreground-background-and-text-style-mixin)
    ((id :initarg :id :reader gadget-id :initform nil)
     (client :initarg :client :initform nil :accessor gadget-client)
     (armed-callback :initarg :armed-callback :initform nil
		     :reader gadget-armed-callback)
     (disarmed-callback :initarg :disarmed-callback :initform nil
			:reader gadget-disarmed-callback)
     (active :initarg :active :accessor gadget-active-p))
  (:default-initargs :active t))

;;; Arming and disarming

(defmethod armed-callback :around ((gadget gadget) (client t) (id t))
  (let ((callback (gadget-armed-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget)
        (call-next-method))))

(defmethod armed-callback ((gadget gadget) (client t) (id t))
  nil)

(defmethod disarmed-callback :around ((gadget gadget) (client t) (id t))
  (let ((callback (gadget-disarmed-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget)
        (call-next-method))))

(defmethod disarmed-callback ((gadget gadget) (client t) (id t))
  nil)

;;; Activation and deactivation, not intended to be like callbacks

(defmethod activate-gadget ((gadget gadget))
  (unless (gadget-active-p gadget)
    (setf (gadget-active-p gadget) t)
    (note-gadget-activated (gadget-client gadget) gadget)))
 
(defmethod note-gadget-activated ((client t) (gadget gadget))
  nil)
 
(defmethod deactivate-gadget ((gadget gadget))
  (when (gadget-active-p gadget)
    (setf (gadget-active-p gadget) nil)
    (note-gadget-deactivated (gadget-client gadget) gadget)))
 
(defmethod note-gadget-deactivated ((client t) (gadget gadget))
  nil)


(defclass value-gadget (gadget) 
    ((value :initarg :value :initform nil
	    :reader gadget-initial-value)
     (value-changed-callback :initarg :value-changed-callback :initform nil
			     :reader gadget-value-changed-callback)))

(defgeneric gadget-value (gadget))

(defmethod gadget-value ((gadget value-gadget))
  (slot-value gadget 'value))

(defgeneric (setf gadget-value) (value gadget &key invoke-callback))

(defmethod (setf gadget-value) (value (gadget value-gadget) &key invoke-callback)
  (declare (ignore value invoke-callback)))

;;--- What is the correct default for INVOKE-CALLBACK?
(defmethod (setf gadget-value) :after (value (gadget value-gadget) &key invoke-callback)
  (setf (slot-value gadget 'value) value)
  (when invoke-callback
    (value-changed-callback gadget (gadget-client gadget) (gadget-id gadget) value)))

(defmethod value-changed-callback :around ((gadget value-gadget) (client t) (id t) value)
  (setf (slot-value gadget 'value) value)
  (let ((callback (gadget-value-changed-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget value)
        (call-next-method))))

(defmethod value-changed-callback ((gadget value-gadget) (client t) (id t) value)
  (declare (ignore value))
  nil)


(defclass action-gadget (gadget) 
    ((activate-callback :initarg :activate-callback :initform nil
			:reader gadget-activate-callback)))  

(defmethod activate-callback :around ((gadget action-gadget) (client t) (id t))
  (let ((callback (gadget-activate-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget)
        (call-next-method))))

(defmethod activate-callback ((gadget action-gadget) (client t) (id t))
  nil)


;;; Basic gadgets-mixins

(defclass oriented-gadget-mixin ()
    ((orientation :initarg :orientation
		  :accessor gadget-orientation))
  (:default-initargs :orientation :horizontal))


(defclass labelled-gadget-mixin ()
    ((label :initarg :label
	    :accessor gadget-label)
     (alignment :initarg :align-x
		:accessor gadget-alignment))
  (:default-initargs :label "" :align-x :center))

(defmethod compute-gadget-label-size ((pane labelled-gadget-mixin))
  (let ((label (or (gadget-label pane) "Label"))
	(text-style (slot-value pane 'text-style)))
    #+++ignore
    (with-sheet-medium (medium pane)
      (multiple-value-bind (width height)
	  (text-size medium label :text-style text-style)
	(value (+ width 8) (+ height 4))))
    #---ignore
    (with-sheet-medium (medium pane)
      (values (+ 8 (* (length label) (text-style-width text-style medium)))
	      (+ 4 (text-style-height text-style medium))))))

;;--- We might want a way of changing the range and the value together.
(defclass range-gadget-mixin ()
    ((min-value :initarg :min-value :accessor gadget-min-value)
     (max-value :initarg :max-value :accessor gadget-max-value))
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
	  (value-gadget oriented-gadget-mixin range-gadget-mixin labelled-gadget-mixin)
    ((drag-callback :initarg :drag-callback :initform nil
		    :reader slider-drag-callback)
     (decimal-places :initarg :decimal-places
		     :reader slider-decimal-places)
     (show-value-p :initarg :show-value-p 
		   :accessor gadget-show-value-p))
  (:default-initargs :decimal-places 0
		     :show-value-p nil))

(defmethod drag-callback :around ((gadget slider) (client t) (id t) value)
  (let ((callback (slider-drag-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget value)
        (call-next-method))))

(defmethod drag-callback ((gadget slider) (client t) (id t) value)
  (declare (ignore value))
  nil)


;;; Scroll bar
(defclass scroll-bar
	  (value-gadget range-gadget-mixin oriented-gadget-mixin)
    ((current-value :initform nil :accessor scroll-bar-current-value)
     (current-size :initform nil :accessor scroll-bar-current-size)
     (drag-callback :initarg :drag-callback :initform nil
		    :reader scroll-bar-drag-callback)))

(defmethod destroy-mirror :after  ((port port) (sheet scroll-bar))
  ;; This invalidates any caching that is going on
  (setf (scroll-bar-current-size sheet) nil))

(defmethod drag-callback :around ((gadget scroll-bar) (client t) (id t) value)
  (let ((callback (scroll-bar-drag-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget value)
        (call-next-method))))

(defmethod drag-callback ((gadget scroll-bar) (client t) (id t) value)
  (declare (ignore value))
  nil)


;;; Push-button
(defclass push-button 
	  (action-gadget labelled-gadget-mixin) 
    ())


;;; Toggle button
(defclass toggle-button 
	  (value-gadget labelled-gadget-mixin)
    ((indicator-type :initarg :indicator-type :initform :some-of
		     :type (member :some-of :one-of)
		     :reader gadget-indicator-type)))


;;; Menu button
(defclass menu-button 
	  (value-gadget labelled-gadget-mixin) 
    ((indicator-type :initarg :indicator-type :initform :some-of
		     :reader gadget-indicator-type)))


(defclass label-pane
	  (gadget labelled-gadget-mixin)
    ())


;;; Caption
;;; option menu
;;; label

;;; Radio box [exclusive-choice] .. [inclusive-choice]
(defclass radio-box 
	  (value-gadget oriented-gadget-mixin) 
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
	(if (panep choice)
	    (sheet-adopt-child rb choice)
	  ;; Sometimes the user calls MAKE-PANE within a call to
	  ;; WITH-RADIO-BOX, so don't mess up
	  (make-pane 'toggle-button 
		     :value (equal (radio-box-current-selection rb) choice)
		     :label (if (stringp choice)
				(string choice)
			      (gadget-label choice))
		     :id choice
		     :parent rb))))))



;;; Check-box

(defclass check-box 
	  (value-gadget oriented-gadget-mixin) 
    ((selections :initform nil 
		 :reader check-box-selections)
     ;;--- think about this...
     (value :initform nil
 	    :initarg :current-selection
 	    :accessor check-box-current-selection)))

(defmethod initialize-instance :after ((cb check-box) &key choices)
  (let ((frame (pane-frame cb)))
    (with-look-and-feel-realization ((frame-manager frame) frame)
      (dolist (choice choices)
	(if (panep choice)
	    (sheet-adopt-child cb choice)
	  ;; Sometimes the user calls MAKE-PANE within a call to
	  ;; WITH-RADIO-BOX, so don't mess up
	  (make-pane 'toggle-button 
		     :value (equal (check-box-current-selection cb) choice)
		     :label (if (stringp choice)
				(string choice)
			      (gadget-label choice))
		     :indicator-type :some-of
		     :id choice
		     :parent cb))))))


;;; Menu-bar


;;; [cascade]


;;; Text edit
;;--- Do we want to specify a binding to commands?
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
	  ((viewport-region :accessor viewport-viewport-region)
	   (scroller-pane :initarg :scroller-pane :reader viewport-scroller-pane)))

(defmethod initialize-instance :after ((viewport viewport) &key)
  (multiple-value-bind (width height)
      (bounding-rectangle-size viewport)
    (setf (viewport-viewport-region viewport)
	  (make-bounding-rectangle 0 0 width height))))

(defmethod allocate-space ((viewport viewport) width height)
  ;;--- Make sure the child is at least as big as the viewport
  ;;--- VIEWPORT-REGION-CHANGED actually does this also
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
  (let ((vr (viewport-viewport-region viewport)))
    (multiple-value-bind (owidth oheight) (bounding-rectangle-size vr)
      (bounding-rectangle-set-size vr width height)
      ;; If previously it was too small to display the entire contents
      ;; but now it is large enough, scroll the window
      (multiple-value-bind (ox oy) (bounding-rectangle-position vr)
	(with-bounding-rectangle* (cleft ctop cright cbottom)
	  (viewport-contents-extent viewport)
	  (let ((cw (- cright cleft))
		(ch (- cbottom ctop)))
	    (let ((x ox) (y oy))
	      (when (and (< owidth cw) (<= cw width)) (setq x cleft))
	      (when (and (< oheight ch) (<= ch height)) (setq y ctop))
	      (if (or (/= x ox) (/= y oy))
		  (scroll-extent (sheet-child viewport) :x x  :y y)
		(progn
		  (update-scroll-bars viewport)
		  (viewport-region-changed (sheet-child viewport) viewport))))))))))

;;--- Work on this
#+++ignore
(defmethod note-sheet-region-changed :around ((viewport viewport) &key port-did-it)
  (declare (ignore port-did-it))
  (multiple-value-bind (changedp 
			hscroll-bar hscroll-bar-enabled-p
			vscroll-bar vscroll-bar-enabled-p)
      (compute-dynamic-scroll-bar-values viewport)
    (if changedp
 	(update-dynamic-scroll-bars 
	  viewport changedp
	  hscroll-bar hscroll-bar-enabled-p
	  vscroll-bar vscroll-bar-enabled-p)
        (call-next-method))))

(defun update-dynamic-scroll-bars (scroller changedp
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
      (let* ((contents (slot-value scroller 'contents))
 	     (c-extent (viewport-contents-extent
			 (pane-viewport contents))))
 	(multiple-value-bind (vx vy) 
	    (window-viewport-position contents)
 	  (window-set-viewport-position
	    contents
	    (if (and hscroll-bar (not hscroll-bar-enabled-p))
		(bounding-rectangle-min-x c-extent)
		vx)
	    (if (and vscroll-bar (not vscroll-bar-enabled-p))
		(bounding-rectangle-min-y c-extent)
		vy)))))
    (clear-space-requirement-caches-in-tree scroller)
    (when relayout
      ;;--- This is kinda bogus. If this was a generic scroller then
      ;;--- then you want to layout the table. 
      (let ((table (slot-value scroller	'viewport)))
 	(multiple-value-bind (width height)
 	    (bounding-rectangle-size table)
 	  (allocate-space table width height))))))

(defun compute-dynamic-scroll-bar-values (scroller)
  (let* ((hscroll-bar (scroller-pane-horizontal-scroll-bar scroller))
	 (vscroll-bar (scroller-pane-vertical-scroll-bar scroller))
	 (scroll-bar-policy (scroller-pane-scroll-bar-policy scroller)))
    (if (eq scroll-bar-policy :dynamic)
	(multiple-value-bind (vwidth vheight) 
	    (bounding-rectangle-size scroller)
	  (multiple-value-bind (cwidth cheight) 
	      (bounding-rectangle-size 
		(viewport-contents-extent (slot-value scroller 'viewport)))
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
  (let ((contents (sheet-child viewport)))
    (or (and (output-recording-stream-p contents)
	     (stream-output-history contents))
        contents)))

;;; Then there is the layout stuff and scrolling macros

(defmacro scrolling (options &body contents)
  `(make-pane 'generic-scroller-pane
	      :contents ,@contents
	      ,@options))


;;; List panes and option menus

(defclass set-gadget-mixin ()
    ((items :initarg :items :accessor set-gadget-items)
     (name-key :initarg :name-key :accessor set-gadget-name-key)
     (value-key :initarg :value-key :accessor set-gadget-value-key)
     (test :initarg :test :accessor set-gadget-test))
  (:default-initargs :items nil
		     :test #'eql
		     :value-key #'identity
		     :name-key #'princ-to-string))
 

(defclass list-pane (set-gadget-mixin value-gadget)
    ;;--- Should this be :ONE-OF/:SOME-OF, as radio boxes are?
    ((mode :initarg :mode :type (member :exclusive :nonexclusive)
	   :accessor list-pane-mode))
  (:default-initargs :mode :exclusive))
 
(defun compute-list-pane-selected-items (sheet value)
  (with-accessors ((items set-gadget-items)
		   (value-key set-gadget-value-key)
		   (test set-gadget-test)
		   (mode list-pane-mode)
		   (name-key set-gadget-name-key)) sheet
    (ecase mode
      (:exclusive
	(let ((x (find value items :test test :key value-key)))
	  (and x (list (funcall name-key x)))))
      (:nonexclusive
	(mapcar name-key
		(remove-if-not #'(lambda (item)
				   (member (funcall value-key item) value :test test))
			       items))))))

(defun list-pane-selected-item-p (sheet item)
  (with-accessors ((items set-gadget-items)
		   (value gadget-value)
		   (value-key set-gadget-value-key)
		   (test set-gadget-test)
		   (mode list-pane-mode)
		   (name-key set-gadget-name-key)) sheet
    (ecase mode
      (:exclusive
	(funcall test (funcall value-key item) value))
      (:nonexclusive
	(member (funcall value-key item) value :test test)))))


(defclass option-pane (set-gadget-mixin 
		       labelled-gadget-mixin
		       value-gadget)
    ())


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
