;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: db-button.lisp,v 1.7 92/05/07 13:11:10 cer Exp $

"Copyright (c) 1990, 1991 International Lisp Associates.
 Portions copyright (c) 1991, 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)


(defclass button-pane-mixin 
	  (space-requirement-mixin
	   leaf-pane)
    ;; ARMED has three states:
    ;;  NIL ==> the button is not armed
    ;;  T   ==> the button is armed, waiting for a pointer button press
    ;;  :ACTIVE ==> the button is armed, waiting for a pointer button release
    ((armed :initform nil)))

;; General highlight-by-inverting method
(defmethod highlight-button ((pane button-pane-mixin) medium)
  (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
    (draw-rectangle* medium left top right bottom
		     :ink +flipping-ink+ :filled t)))

(defmethod handle-event ((pane button-pane-mixin) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (cond ((and (pointer-event-button event)
		  (not (zerop (pointer-event-button event))))
	     (setf armed :active)
	     (with-sheet-medium (medium pane)
	       (highlight-button pane medium)))
	    (t (setf armed t)))
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane button-pane-mixin) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (when (eq armed :active)
	(with-sheet-medium (medium pane)
	  (highlight-button pane medium)))
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane button-pane-mixin) (event pointer-button-press-event))
  (with-slots (armed) pane
    (when armed
      (setf armed :active)
      (with-sheet-medium (medium pane)
	(highlight-button pane medium)))))

(defmethod handle-event ((pane button-pane-mixin) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (eq armed :active)
      (setf armed t)
      (with-sheet-medium (medium pane)
	(highlight-button pane medium))
      (activate-callback pane (gadget-client pane) (gadget-id pane)))))


;;; Push buttons
(defclass push-button-pane (push-button button-pane-mixin)
    ((show-as-default :initarg :show-as-default
		      :accessor push-button-show-as-default))
  (:default-initargs :label nil :text-style *default-text-style*
		     :show-as-default nil))

(defmethod compose-space ((pane push-button-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (width height)
      (compute-gadget-label-size pane)
    (make-space-requirement :width width :height height)))

;; Draw the text, invert the whole button when actively armed
(defmethod repaint-sheet ((pane push-button-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (let ((text (gadget-label pane))
	  (text-style (slot-value pane 'text-style))
	  (armed (slot-value pane 'armed))
	  (region (sheet-region pane)))
      (multiple-value-call #'draw-rectangle*
        medium (bounding-rectangle* (sheet-region pane))
	:filled nil)
      (draw-text medium text (clim-utils::bounding-rectangle-center region)
		 :text-style text-style
		 ;;--- :align-x ':center :align-y ':center
		 :align-x ':left :align-y ':center)
      (when (eq armed :active)
	(highlight-button pane medium)))))


;;; Toggle buttons
(defclass toggle-button-pane (toggle-button button-pane-mixin)
    ((radius :initarg :indicator-radius :initform 5))
  (:default-initargs :label nil :text-style *default-text-style*
		     :indicator-type ':one-of))

(defmethod compose-space ((pane toggle-button-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (width height)
      (compute-gadget-label-size pane)
    (let ((radius (slot-value pane 'radius)))
      (make-space-requirement :width (+ width (* radius 2) 5)
			      :height (max height (* radius 2))))))

(defmethod repaint-sheet ((pane toggle-button-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (declare (ignore right))
      (let* ((text (gadget-label pane))
	     (text-style (slot-value pane 'text-style))
	     (radius (slot-value pane 'radius))
	     (cx (+ left radius))
	     (cy (+ top (round (- bottom top) 2))))
	(draw-circle* medium cx cy radius :filled nil)
	(draw-circle* medium cx cy (round radius 2)
		      :ink (if (gadget-value pane) +foreground-ink+ +background-ink+))
	(draw-text* medium text (+ cx (* radius 2) 5) cy
		    :text-style text-style
		    :align-x ':left :align-y ':center)))))

;; This is done in the (SETF GADGET-VALUE) method because that's where it 
;; would be done in, say, Motif.  (That is, we'd pass the SET-GADGET-VALUE
;; off to Motif which would toggle the indicator.)
(defmethod (setf gadget-value) :after (value (pane toggle-button-pane) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (port pane)
    ;; If it's not grafted, don't draw it.
    (with-sheet-medium (medium pane)
      (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
	(declare (ignore right))
	(let* ((radius (slot-value pane 'radius))
	       (cx (+ left radius))
	       (cy (+ top (round (- bottom top) 2))))
	  (draw-circle* medium cx cy (round radius 2)
			:ink (if (gadget-value pane) +foreground-ink+ +background-ink+)))))))

;; Highlighting is a no-op
(defmethod highlight-button ((pane toggle-button-pane) medium)
  (declare (ignore medium)))

(defmethod handle-event ((pane toggle-button-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (eq armed :active)
      (setf armed t)
      (with-sheet-medium (medium pane)
	(highlight-button pane medium))
      (setf (gadget-value pane :invoke-callback t) (not (gadget-value pane))))))


;;; Menu buttons
(defclass menu-button-pane (menu-button button-pane-mixin)
    ((menu-group :initarg :menu-group :reader menu-button-menu-group)
     (show-arrow :initarg :show-arrow))
  (:default-initargs :label nil :label-text-style *default-text-style*
		     :menu-group nil :show-arrow t))


;;; Radio boxes

(defclass radio-box-pane 
	  (radio-box
	   wrapping-space-mixin
	   sheet-permanently-enabled-mixin
	   sheet-mute-input-mixin
	   sheet-multiple-child-mixin
	   space-requirement-mixin
	   pane)
    ())

;; When one of the radio-box's managed gadgets gets selected (or deselected)
;; we may have to turn on or off some item.  If we turn off the currently selected
;; item, we just turn it back on again!
(defmethod value-changed-callback :after
	   ((selection toggle-button) (client radio-box-pane) gadget-id new-value)
  (declare (ignore gadget-id))
  (let ((old-selection (radio-box-current-selection client)))
    ;;--- Note the the Motif version of this sets the current selection
    ;;--- to the ID, *not* the new value.  Howcum?
    (cond ((eql selection old-selection)
	   (setf (radio-box-current-selection client) (and new-value selection)))
	  (old-selection
	   (setf (gadget-value old-selection :invoke-callback t) nil)
	   ;; We could just SETF RADIO-BOX-CURRENT-SELECTION, but this
	   ;; way we get the right callback behavior...
	   (setf (gadget-value client :invoke-callback t) (and new-value selection)))
	  (t
	   (setf (gadget-value client :invoke-callback t) (and new-value selection))))))

(defmethod initialize-instance :after ((pane radio-box-pane) 
				       &key choices selection frame-manager frame)
  ;;--- This really needs to be more robust
  (dolist (choice choices)
    (setf (gadget-client choice) pane))
  (let ((inferiors
	  (with-look-and-feel-realization (frame-manager frame)
	    (make-pane 'hbox-pane
		       :spacing 5
		       :contents choices))))
    (sheet-adopt-child pane inferiors)
    (when selection
      (setf (gadget-value selection) t))))

;; This macro is just an example of one possible syntax.  The obvious "core"
;; syntax is (MAKE-PANE 'RADIO-BOX :CHOICES (LIST ...) :SELECTION ...)
(defmacro with-radio-box ((&rest options) &body body)
  (let ((current-selection '#:current-selection)
	(choices '#:choices))
    `(let ((,current-selection nil))
       (macrolet ((radio-box-current-selection (form)
		    `(setq ,',current-selection ,form)))
	 (let ((,choices (list ,@body)))
	   (make-pane 'radio-box
		      :choices ,choices
		      :selection ,current-selection
		      ,@options))))))
