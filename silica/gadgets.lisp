;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: gadgets.lisp,v 1.57 1993/09/07 21:46:41 colin Exp $

"Copyright (c) 1991, 1992 by Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)


(define-protocol-class gadget (pane))

(defclass basic-gadget (sheet-with-resources-mixin gadget)
    ((id :initarg :id :reader gadget-id :initform nil)
     (client :initarg :client :initform nil :accessor gadget-client)
     (armed-callback :initarg :armed-callback :initform nil
		     :reader gadget-armed-callback)
     (disarmed-callback :initarg :disarmed-callback :initform nil
			:reader gadget-disarmed-callback)
     (active :initarg :active :accessor gadget-active-p)
     (help-callback :initform nil 
		    :initarg :help-callback 
		    :accessor gadget-help-callback))
  (:default-initargs :active t))

;;; Arming and disarming

(defun-inline invoke-callback-function (function &rest args)
  (declare (dynamic-extent args))
  (if (consp function)
      (apply (car function) (append args (cdr function)))
      (apply function args)))

(defmethod armed-callback :around ((gadget basic-gadget) (client t) (id t))
  (let ((callback (gadget-armed-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget)
        (call-next-method))))

(defmethod armed-callback ((gadget basic-gadget) (client t) (id t))
  nil)

(defmethod disarmed-callback :around ((gadget basic-gadget) (client t) (id t))
  (let ((callback (gadget-disarmed-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget)
        (call-next-method))))

(defmethod disarmed-callback ((gadget basic-gadget) (client t) (id t))
  nil)

;;; Activation and deactivation, not intended to be like callbacks

(defmethod activate-gadget ((gadget basic-gadget))
  (unless (gadget-active-p gadget)
    (setf (gadget-active-p gadget) t)
    (note-gadget-activated (gadget-client gadget) gadget)))
 
(defmethod note-gadget-activated ((client t) (gadget basic-gadget))
  nil)
 
(defmethod deactivate-gadget ((gadget basic-gadget))
  (when (gadget-active-p gadget)
    (setf (gadget-active-p gadget) nil)
    (note-gadget-deactivated (gadget-client gadget) gadget)))
 
(defmethod note-gadget-deactivated ((client t) (gadget basic-gadget))
  nil)


(defclass value-gadget (basic-gadget) 
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


(defclass action-gadget (basic-gadget)
    ((activate-callback :initarg :activate-callback :initform nil
			:reader gadget-activate-callback)))  

(defmethod activate-callback :around ((gadget action-gadget) (client t) (id t))
  (let ((callback (gadget-activate-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget)
        (call-next-method))))

(defmethod activate-callback ((gadget action-gadget) (client t) (id t))
  nil)


(defclass focus-gadget (basic-gadget)
    ((focus-out-callback :initarg :focus-out-callback :initform nil
			 :reader gadget-focus-out-callback)
     (focus-in-callback :initarg :focus-in-callback :initform nil
			:reader gadget-focus-in-callback)))

(defmethod focus-out-callback :around ((gadget focus-gadget) (client t) (id t)) 
  (let ((callback (gadget-focus-out-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget)
	(call-next-method))))

(defmethod focus-out-callback ((gadget focus-gadget) (client t) (id t))
  nil)

(defmethod focus-in-callback :around ((gadget focus-gadget) (client t) (id t)) 
  (let ((callback (gadget-focus-in-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget)
	(call-next-method))))

(defmethod focus-in-callback ((gadget focus-gadget) (client t) (id t))
  nil)


;;; Basic gadgets-mixins

(defclass oriented-gadget-mixin ()
    ((orientation :initarg :orientation
		  :type (member :horizontal :vertical)
		  :accessor gadget-orientation))
  (:default-initargs :orientation :horizontal))


(defclass labelled-gadget-mixin ()
    ((label :initarg :label
	    :accessor gadget-label)
     (alignment :initarg :align-x
		:accessor gadget-alignment))
  (:default-initargs :label "" :align-x :center))

(defmethod compute-gadget-label-size ((pane labelled-gadget-mixin))
  (let ((label (gadget-label pane)))
    (etypecase label
      (string
	(let ((text-style (slot-value pane 'text-style)))
	  (with-sheet-medium (medium pane)
	    (multiple-value-bind (width height)
		(text-size medium label :text-style text-style)
	      (values (+ width (text-style-width text-style medium))
		      (+ height (floor (text-style-height text-style medium) 2)))))))
      (null (values 0 0))
      (pattern
	(values (pattern-width label) (pattern-height label)))
      (pixmap
       (values (pixmap-width label) (pixmap-height label))))))

(defmethod print-object ((object labelled-gadget-mixin) stream)
  (if (and (slot-boundp object 'label)
	   (stringp (slot-value object 'label)))
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "~A" (slot-value object 'label)))
      (call-next-method)))




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

(defparameter *default-slider-label-text-style* 
	      (make-text-style :sans-serif :bold :small))
(defparameter *default-slider-range-label-text-style*
	      (make-text-style :sans-serif :bold :very-small))

;;; Slider
(defclass slider
	  (value-gadget oriented-gadget-mixin range-gadget-mixin labelled-gadget-mixin)
    ((drag-callback :initarg :drag-callback :initform nil
		    :reader slider-drag-callback)
     (decimal-places :initarg :decimal-places
		     :reader slider-decimal-places)
     (show-value-p :initarg :show-value-p 
		   :accessor gadget-show-value-p)
     (min-label :initarg :min-label)
     (max-label :initarg :max-label)
     (range-label-text-style :initarg :range-label-text-style)
     (number-of-tick-marks :initarg :number-of-tick-marks)
     (number-of-quanta :initarg :number-of-quanta)
     (editable-p :initarg :editable-p :accessor gadget-editable-p))
  (:default-initargs :decimal-places 0
		     :show-value-p nil
		     :min-label nil
		     :max-label nil
		     :range-label-text-style *default-slider-range-label-text-style*
		     :number-of-tick-marks 0
		     :number-of-quanta nil
		     :editable-p t))

(defmethod initialize-instance :after ((pane slider) 
				       &key (decimal-places 0 places-p)
				       &allow-other-keys)
  (declare (ignore decimal-places))
  (unless places-p
    (let ((range (gadget-range pane)))
      (cond ((<= range 1)
	     (setf (slot-value pane 'decimal-places) 2))
	    ((<= range 10)
	     (setf (slot-value pane 'decimal-places) 1))))))

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

(defmethod destroy-mirror :after  ((port basic-port) (sheet scroll-bar))
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
    ((show-as-default :initform nil :initarg :show-as-default
		      :accessor push-button-show-as-default)
     (pattern :initarg :pattern)
     (icon-pattern :initarg :icon-pattern)))


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



;;; Menu bar
(defclass menu-bar (pane
		    oriented-gadget-mixin
		    sheet-with-resources-mixin)
    ((command-table :initarg :command-table :initform nil
		    :accessor menu-bar-command-table)))


;;; Cascade button
;;; Caption
;;; Option menu

(defclass row-column-gadget-mixin (oriented-gadget-mixin)
	  ((rows :initarg :rows :initform nil :accessor gadget-rows)
	   (columns :initarg :columns :initform nil :accessor gadget-columns)))

;;; Radio box [exclusive-choice] .. [inclusive-choice]
(defclass radio-box 
	  (value-gadget row-column-gadget-mixin) 
    ((selections :initform nil 
		 :reader radio-box-selections)
     ;;--- think about this...
     (value :initform nil
 	    :initarg :current-selection
 	    :accessor radio-box-current-selection)))

(defmethod (setf gadget-value) (new-value (gadget radio-box) &key invoke-callback)
  (declare (ignore invoke-callback))
  (unless (eq new-value (radio-box-current-selection gadget))
    (dolist (toggle (radio-box-selections gadget))
      (setf (gadget-value toggle)
	(eq new-value toggle))))
  (call-next-method))

(defmethod initialize-instance :after ((rb radio-box) &key choices)
  (let* ((frame (pane-frame rb))
	 (framem (frame-manager frame))
	 (selections nil))
    (assert (and frame framem) ()
      "There must be both a frame and frame manager active")
    (with-look-and-feel-realization (framem frame)
      (dolist (choice choices)
	(etypecase choice
	  (pane
	   ;; Adopt the button.  Some framems will disown the button
	   ;; in order to do layout, but that will happen later.
	   (unless (sheet-parent choice)
	     (sheet-adopt-child rb choice))
	   (push choice selections)
	   (setf (gadget-value choice) (eq choice (radio-box-current-selection rb))))
	  (string
	   ;; Create a button if the user supplied an abbreviation
	   (let* ((value (equal (radio-box-current-selection rb) choice))
		  (button (make-pane 'toggle-button 
				     :value value
				     :label choice
				     :indicator-type :one-of
				     :client rb
				     :id choice
				     :parent rb)))
	     (push button selections)
	     (when value
	       (setf (radio-box-current-selection rb) button)))))))
    (setf (slot-value rb 'selections) (nreverse selections))
    (check-type (radio-box-current-selection rb) (or null pane))))

(defmethod value-changed-callback :around 
	   ((selection basic-gadget) (client radio-box) gadget-id value)
  (declare (ignore gadget-id))
  ;;--- The following comment is wrong.  Perhaps these should be :BEFORE
  ;;--- methods since the user could define a more specific around method only.
  ;; This and the one below have to be :AROUND because if the user has
  ;; specified a callback function only :AROUNDs ever get executed.
  (when (eq value t)
    (setf (radio-box-current-selection client) selection)
    (value-changed-callback 
     client (gadget-client client) (gadget-id client) selection))
  (call-next-method))


;;; Check-box

(defclass check-box 
    (value-gadget row-column-gadget-mixin) 
    ((selections :initform nil 
		 :reader check-box-selections)
     ;;--- think about this...
     (value :initform nil
 	    :initarg :current-selection
 	    :accessor check-box-current-selection)))

(defmethod (setf gadget-value) (new-value (gadget check-box) &key invoke-callback)
  (declare (ignore invoke-callback))
  (unless (equal new-value (check-box-current-selection gadget))
    (dolist (toggle (check-box-selections gadget))
      (setf (gadget-value toggle)
	(member toggle new-value))))
  (call-next-method))

(defmethod initialize-instance :after ((cb check-box) &key choices)
  (let* ((frame (pane-frame cb))
	 (framem (frame-manager frame))
	 (selections nil))
    (assert (and frame framem) ()
      "There must be both a frame and frame manager active")
    (with-look-and-feel-realization (framem frame)
      (dolist (choice choices)
	(etypecase choice
	  (toggle-button
	   ;; Adopt the button.  Some framems will disown the button
	   ;; in order to do layout, but that will happen later.
	   (push choice selections)
	   (unless (sheet-parent choice)
	     (sheet-adopt-child cb choice))
	   (setf (gadget-value choice) (member choice (check-box-current-selection cb))))
	  (string
	   ;; Create a button if the user supplied an abbreviation
	   (let* ((value (member choice (check-box-current-selection cb) :test #'equal))
		  (button (make-pane 'toggle-button 
				     :value (and value t)
				     :label choice
				     :indicator-type :some-of
				     :client cb
				     :id choice
				     :parent cb)))
	     (push button selections)
	     (when value
	       (setf (car value) button)))))))
    (setf (slot-value cb 'selections) (nreverse selections))
    (assert (every #'panep (check-box-current-selection cb)) () "Current selection must be a list of panes")))

(defmethod value-changed-callback :around 
	   ((selection basic-gadget) (client check-box) gadget-id value)
  (declare (ignore gadget-id))
  (if (eq value t)
      (pushnew selection (check-box-current-selection client))
    (setf (check-box-current-selection client)
      (delete selection (check-box-current-selection client))))
  (value-changed-callback
   client (gadget-client client) (gadget-id client) (check-box-current-selection client))
  (call-next-method))


;; This macro is just an example of one possible syntax.  The obvious "core"
;; syntax is (MAKE-PANE 'RADIO-BOX :CHOICES (LIST ...) :SELECTION ...)
(defmacro with-radio-box ((&rest options &key (type ':one-of) &allow-other-keys)
			  &body body)
  (setq options (remove-keywords options '(:type)))
  (let ((current-selection (gensym))
	(choices (gensym)))
    (ecase type
      (:one-of
	`(let ((,current-selection nil))
	   (macrolet ((radio-box-current-selection (form)
			`(setq ,',current-selection ,form)))
	     (let ((,choices (list ,@body)))
	       (make-pane 'radio-box
		 :choices ,choices
		 :current-selection ,current-selection
		 ,@options)))))
      (:some-of
	`(let ((,current-selection nil))
	   (macrolet ((radio-box-current-selection (form)
			(let ((button (gensym)))
			  `(let ((,button ,form))
			     (setq ,',current-selection 
			       (append ,',current-selection (list ,button)))
			     ,button)))
		      (check-box-current-selection (form)
			(let ((button (gensym)))
			  `(let ((,button ,form))
			     (setq ,',current-selection 
			       (append ,',current-selection (list ,button)))
			     ,button))))
	     (let ((,choices (list ,@body)))
	       (make-pane 'check-box
		 :choices ,choices
		 :current-selection ,current-selection
		 ,@options))))))))


;;; Text edit
;;--- Do we want to specify a binding to commands?
(defclass text-field 
	  (value-gadget focus-gadget action-gadget) 
    ((editable-p :initarg :editable-p :accessor gadget-editable-p))
  (:default-initargs :editable-p t))

(defclass text-editor (text-field) 
    ((ncolumns :initarg :ncolumns
	       :accessor gadget-columns)
     (nlines :initarg :nlines
	     :accessor gadget-lines)
     (word-wrap :initarg :word-wrap
		:accessor gadget-word-wrap))
  (:default-initargs :ncolumns 30 :nlines 1 :editable-p t :word-wrap nil))

(defmethod gadget-current-selection ((text-field text-field))
  nil)

;;; Viewport

;;--- CLIM 0.9 has this VIEWPORT-LAYOUT-MIXIN -- do we need it?
(defclass viewport
	  (sheet-single-child-mixin
	   sheet-permanently-enabled-mixin
	   wrapping-space-mixin
	   sheet-with-resources-mixin
	   basic-pane)
    ;; This describes the region that we are displaying
    ((viewport-region :accessor viewport-viewport-region)
     (scroller-pane :initarg :scroller-pane :reader viewport-scroller-pane)))

(defmethod initialize-instance :after ((viewport viewport) &key)
  (multiple-value-bind (width height)
      (bounding-rectangle-size viewport)
    (setf (viewport-viewport-region viewport)
	  (make-bounding-rectangle 0 0 width height))))

(defmethod viewportp ((x t)) nil)
(defmethod viewportp ((x viewport)) t)

(defmethod compose-space ((viewport viewport) &key width height)
  (declare (ignore width height))
  (let ((sr (call-next-method)))
    (multiple-value-bind (width min-width max-width
			  height min-height max-height)
	(space-requirement-components sr)
      (declare (ignore min-width min-height))
      (make-space-requirement
	:width width :min-width 0 :max-width max-width
	:height height :min-height 0 :max-height max-height))))


(defmethod allocate-space ((viewport viewport) width height)
  ;; Make sure the child is at least as big as the viewport
  ;; (VIEWPORT-REGION-CHANGED actually does this also)
  (let* ((child (sheet-child viewport)))
    (if (typep child 'clim-stream-pane)
	(multiple-value-bind (cwidth cheight)
	    (bounding-rectangle-size child)
	  (when (or (< cwidth width)
		    (< cheight height))
	    (resize-sheet
	     child (max width cwidth) (max height cheight))))
      (progn
	(let* ((sr (compose-space child :width width :height height))
	       (max-width (space-requirement-max-width sr))
	       (max-height (space-requirement-max-height sr))
	       (desired-width (space-requirement-width sr))
	       (desired-height (space-requirement-height sr)))
	  (resize-sheet child 
			(if (>= max-width +fill+)
			    (max width desired-width)
			  (max desired-width max-width))
			(if (>= max-height +fill+)
			    (max height desired-height)
			  (max desired-height max-height))))))))

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
	      (when (and (< owidth cw) (<= cw width)) 
		(setq x cleft))
	      (when (and (< oheight ch) (<= ch height))
		(setq y ctop))
	      (if (or (/= x ox) (/= y oy))
		  (scroll-extent (sheet-child viewport) x y)
		  (progn
		    (update-scroll-bars viewport)
		    (viewport-region-changed (sheet-child viewport) viewport))))))))))

;;-- This method seems applicable to a whole bunch of panes.
;;-- The problem we have is that a whole bunch of panes dont get the
;;-- right background so it looks crap.

#+ignore
(defmethod repaint-sheet ((sheet viewport) region)
  (let ((region (region-intersection (sheet-region sheet) region)))
    (dolist (child (sheet-children sheet))
      (setq region (region-difference region (transform-region (sheet-transformation child) (sheet-region child)))))
    (with-sheet-medium (medium sheet)
      (with-drawing-options (medium  :ink +background-ink+)
	(dolist (rect (region-set-regions region))
	  (unless (eq +nowhere+ rect)
	    (with-bounding-rectangle* (left top right bottom) rect
	      (draw-rectangle* medium left top right bottom))))))))

(defmethod note-space-requirements-changed ((pane viewport) inner)
  (declare (ignore inner))
  (allocate-space 
    pane 
    (bounding-rectangle-width pane) (bounding-rectangle-height pane)))

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


;(defun update-dynamic-scroll-bars (scroller changedp
;				   hscroll-bar hscroll-bar-enabled-p
;				   vscroll-bar vscroll-bar-enabled-p
;				   &optional relayout)
;  (when changedp
;    (when hscroll-bar
;      (setf (sheet-enabled-p hscroll-bar) hscroll-bar-enabled-p))
;    (when vscroll-bar
;      (setf (sheet-enabled-p vscroll-bar) vscroll-bar-enabled-p))
;    (when (or (and hscroll-bar (not hscroll-bar-enabled-p))
; 	      (and vscroll-bar (not vscroll-bar-enabled-p)))
;      (let* ((contents (slot-value scroller 'contents))
; 	     (c-extent (viewport-contents-extent
;			 (pane-viewport contents))))
; 	(multiple-value-bind (vx vy) 
;	    (window-viewport-position contents)
; 	  (window-set-viewport-position
;	    contents
;	    (if (and hscroll-bar (not hscroll-bar-enabled-p))
;		(bounding-rectangle-min-x c-extent)
;		vx)
;	    (if (and vscroll-bar (not vscroll-bar-enabled-p))
;		(bounding-rectangle-min-y c-extent)
;		vy)))))
;    (clear-space-requirement-caches-in-tree scroller)
;    (when relayout
;      ;;--- This is kinda bogus.  If this was a generic scroller then
;      ;;--- you want to layout the table.
;      (let ((table (slot-value scroller 'viewport)))
; 	(multiple-value-bind (width height)
; 	    (bounding-rectangle-size table)
; 	  (allocate-space table width height))))))

;(defun compute-dynamic-scroll-bar-values (scroller)
;  (let* ((hscroll-bar (scroller-pane-horizontal-scroll-bar scroller))
;	 (vscroll-bar (scroller-pane-vertical-scroll-bar scroller))
;	 (scroll-bar-policy (scroller-pane-scroll-bar-policy scroller)))
;    (if (eq scroll-bar-policy :dynamic)
;	(multiple-value-bind (vwidth vheight) 
;	    (bounding-rectangle-size scroller)
;	  (multiple-value-bind (cwidth cheight) 
;	      (bounding-rectangle-size 
;		(viewport-contents-extent (slot-value scroller 'viewport)))
;	    (let ((ohenp (sheet-enabled-p hscroll-bar))
;		  (ovenp (sheet-enabled-p vscroll-bar))
;		  (nhenp (> cwidth vwidth))
;		  (nvenp (> cheight vheight)))
;	      (values
;		(not (and (eq ohenp nhenp)
;			  (eq ovenp nvenp)))
;		(and (not (eq ohenp nhenp)) hscroll-bar)
;		nhenp
;		(and (not (eq ovenp nvenp)) vscroll-bar)
;		nvenp))))
;        (values nil nil nil nil nil))))

(defun viewport-contents-extent (viewport)
  (let ((contents (sheet-child viewport)))
    (or (and (output-recording-stream-p contents)
	     (stream-output-history contents))
        contents)))


(defmacro scrolling (options &body contents)
  (assert (null (cdr contents)) (contents)
	  "The ~S layout macro can have only a single pane as its contents"
	  'scrolling)
  `(make-pane 'scroller-pane
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
	   :accessor list-pane-mode)
     (visible-items :initarg :visible-items :reader gadget-visible-items))
  (:default-initargs :mode :exclusive :visible-items nil))
 
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
    ;;--- Should this be :ONE-OF/:SOME-OF, as radio boxes are?
    ((mode :initarg :mode :type (member :exclusive :nonexclusive)
	   :accessor option-pane-mode)
     (printer :initarg :printer :reader option-pane-printer))
  (:default-initargs :mode :exclusive :printer nil))


;; Callbacks on widgets generate these events

(defclass gadget-event (event) 
    ((gadget :initarg :gadget :reader event-sheet)))


(defclass value-changed-gadget-event (gadget-event) 
    ((value :initarg :value :reader event-value)))

(defmethod handle-event ((gadget value-gadget) (event value-changed-gadget-event))
  (value-changed-callback
    gadget (gadget-client gadget) (gadget-id gadget) (slot-value event 'value)))


(defclass activate-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget action-gadget) (event activate-gadget-event))
  (activate-callback gadget (gadget-client gadget) (gadget-id gadget)))


(defclass drag-gadget-event (gadget-event) 
    ((value :initarg :value :reader event-value)))

(defmethod handle-event ((gadget value-gadget) (event drag-gadget-event))
  (drag-callback
   gadget (gadget-client gadget) (gadget-id gadget) (slot-value event 'value)))


(defclass focus-out-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget focus-gadget) (event focus-out-gadget-event))
  (focus-out-callback gadget (gadget-client gadget) (gadget-id gadget)))

(defclass focus-in-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget focus-gadget) (event focus-in-gadget-event))
  (focus-in-callback gadget (gadget-client gadget) (gadget-id gadget)))

(defun move-focus-to-gadget (gadget)
  (port-move-focus-to-gadget (port gadget) gadget))

(defmethod port-move-focus-to-gadget ((port t) (gadget t))
  nil)
