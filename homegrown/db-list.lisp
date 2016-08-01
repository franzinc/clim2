;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-


"Copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)	;yes, this file is in the Silica package

;;; List panes and option panes


;; We use "simple" toggle buttons inside of list and option panes
(defclass simple-toggle-button-pane (toggle-button button-pane-mixin)
    ()
  (:default-initargs :label nil 
		     :text-style *default-button-label-text-style*
		     :show-as-default nil))

(defparameter *check-mark-pattern*
	      (make-pattern #2a((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
				(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
				(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0)
				(0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0)
				(0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0)
				(0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0)
				(0 0 1 1 0 0 0 0 1 1 1 0 0 0 0 0)
				(0 1 1 1 1 0 0 1 1 1 0 0 0 0 0 0)
				(1 1 1 1 1 0 1 1 1 1 0 0 0 0 0 0)
				(0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0)
				(0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0)
				(0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0)
				(0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0)
				(0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0))
			    (list +background-ink+ +foreground-ink+)))

(defmethod compose-space ((pane simple-toggle-button-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (width height)
      (compute-gadget-label-size pane)
    (incf width  (+ (pattern-width *check-mark-pattern*)  2))
    (maxf height (+ (pattern-height *check-mark-pattern*) 1))
    (make-space-requirement :width width :height height)))

;; Draw the text, invert the whole button when it's armed.  Add a check
;; mark when the button is toggled on.
(defmethod handle-repaint ((pane simple-toggle-button-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (let ((text (gadget-label pane))
	  (text-style (slot-value pane 'text-style))
	  (armed (slot-value pane 'armed))
	  (pattern *check-mark-pattern*))
      (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
	(if (gadget-value pane)
	    (draw-pattern* medium pattern (- right (+ (pattern-width pattern) 2)) (+ top 1))
	    (draw-rectangle* medium 
			     (- right (+ (pattern-width pattern) 2)) (+ top 1) right bottom
			     :filled t :ink +background-ink+))
	(draw-rectangle* medium left top (1- right) (1- bottom)
			 :filled nil)
	(draw-text* medium text (+ left 2) (+ top (floor (- bottom top) 2))
		    :text-style text-style
		    :align-x :left :align-y :center))
      (when (eq armed :active)
	(highlight-button pane medium)))))

(defmethod handle-event ((pane simple-toggle-button-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (eq armed :active)
      (setf armed t)
      (with-sheet-medium (medium pane)
	(highlight-button pane medium))
      (setf (gadget-value pane :invoke-callback t) (not (gadget-value pane))))))

;; This is done in the (SETF GADGET-VALUE) method because that's where it 
;; would be done in, say, Motif.  (That is, we'd pass the SET-GADGET-VALUE
;; off to Motif which would toggle the indicator.)
(defmethod (setf gadget-value) :after (value (pane simple-toggle-button-pane)
				       &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (port pane)
    (handle-repaint pane +everywhere+)))


(defclass generic-list-pane
	  (list-pane
	   wrapping-space-mixin
	   sheet-permanently-enabled-mixin
	   sheet-mute-input-mixin
	   sheet-multiple-child-mixin
	   space-requirement-mixin
	   basic-pane)
    ())

(defmethod initialize-instance :after ((pane generic-list-pane) &key)
  (with-slots (items name-key value-key test mode) pane
    (let* ((frame (pane-frame pane))
	   (framem (frame-manager frame)))
      (assert (and frame framem) ()
	      "There must be both a frame and frame manager active")
      (with-look-and-feel-realization (framem frame)
	(dolist (item items)
	  (make-pane 'simple-toggle-button-pane
	    :value (ecase mode
		     (:exclusive
		       (funcall test (funcall value-key item) (gadget-value pane)))
		     (:nonexclusive
		       (and (member (funcall value-key item) (gadget-value pane) :test test)
			    t)))
	    :label (funcall name-key item)
	    :id item
	    :parent pane))
	(let ((buttons (copy-list (sheet-children pane))))
	  (setq items buttons)			;save them away
	  (dolist (button buttons)
	    (setf (gadget-client button) pane) 
	    (sheet-disown-child pane button))
	  (let ((inferiors
		  (make-pane 'vbox-pane
		    :spacing 3
		    :contents buttons)))
	    (sheet-adopt-child pane inferiors)))))))

(defmethod value-changed-callback :around 
	   ((selection toggle-button) (client generic-list-pane) gadget-id value)
  (declare (ignore gadget-id))
  (with-slots (items value-key mode) client
    (let ((real-value (funcall value-key (gadget-id selection)))
	  (old-selection nil))
      (ecase mode
	(:exclusive
	  (setq old-selection (gadget-value client))
	  (setf (gadget-value client) (and value real-value)))
	(:nonexclusive
	  (if value
	      (pushnew real-value (gadget-value client))
	      (setf (gadget-value client) (delete real-value (gadget-value client))))))
      (when old-selection
	(let ((button (find old-selection items :key #'gadget-id)))
	  (setf (gadget-value button :invoke-callback nil) nil)))
      (value-changed-callback
	client (gadget-client client) (gadget-id client) (gadget-value client))))
  (call-next-method))

(defmethod handle-event :after ((pane generic-list-pane) (event pointer-event))
  (deallocate-event event))


(defclass generic-option-pane (option-pane push-button-pane)
    ((menu :initform nil))
  (:default-initargs :pattern *right-triangle-button-pattern*))

;;--- The idea is the the option pane itself is a pushbutton which, when
;;--- pressed, pops up a menu containing the options.
(defmethod initialize-instance :after ((pane generic-option-pane) &key)
  (with-slots (external-label label
	       items name-key value-key test mode) pane
    (shiftf external-label label nil)
    (let* ((frame (pane-frame pane))
	   (framem (frame-manager frame))
	   (buttons nil))
      (assert (and frame framem) ()
	      "There must be both a frame and frame manager active")
      (with-look-and-feel-realization (framem frame)
	(dolist (item items)
	  (push (make-pane 'simple-toggle-button-pane
		  :value (ecase mode
			   (:exclusive
			     (funcall test (funcall value-key item) (gadget-value pane)))
			   (:nonexclusive
			     (and (member (funcall value-key item) (gadget-value pane) :test test)
				  t)))
		  :label (funcall name-key item)
		  :id item
		  :client pane)
		buttons))
	(setq buttons (nreverse buttons))
	(setq items (copy-list buttons))	;save them away
	(let ((menu (make-pull-down-menu :port (port frame))))
	  (initialize-pull-down-menu menu buttons)
	  (setf (slot-value pane 'menu) menu))))))

(defmethod handle-event ((pane generic-option-pane) (event pointer-button-release-event))
  (with-slots (armed menu) pane
    (when (eq armed :active)
      (setf armed t)
      (with-sheet-medium (medium pane)
	(highlight-button pane medium))
      (choose-from-pull-down-menu menu pane))))

(defmethod value-changed-callback :around 
	   ((selection toggle-button) (client generic-option-pane) gadget-id value)
  (declare (ignore gadget-id))
  (with-slots (items value-key mode) client
    (let ((real-value (funcall value-key (gadget-id selection)))
	  (old-selection nil))
      (ecase mode
	(:exclusive
	  (setq old-selection (gadget-value client))
	  (setf (gadget-value client) (and value real-value)))
	(:nonexclusive
	  (if value
	      (pushnew real-value (gadget-value client))
	      (setf (gadget-value client) (delete real-value (gadget-value client))))))
      (when old-selection
	(let ((button (find old-selection items :key #'gadget-id)))
	  (setf (gadget-value button :invoke-callback nil) nil)))
      (value-changed-callback
	client (gadget-client client) (gadget-id client) (gadget-value client))))
  (call-next-method))
