;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1990, 1991 International Lisp Associates.
 Portions copyright (c) 1991, 1992 by Symbolics, Inc.  All rights reserved."

;; $fiHeader$

(in-package :silica)


;;; Sliders
(defclass slider-pane 
	  (slider
	   leaf-pane
	   sheet-permanently-enabled-mixin
	   mute-repainting-mixin
	   space-requirement-mixin)
    ((thickness :initarg :thickness :initform 20)
     (min-value :initarg :min-value
		:accessor slider-min-value)
     (max-value :initarg :max-value
		:accessor slider-max-value)
     (show-value :initarg :show-value
		 :accessor slider-show-value)
     (armed :initform nil))
  (:default-initargs :value 0 :min-value 0 :max-value 1
		     :show-value nil))

(defmethod compose-space ((pane slider-pane) &key width height)
  (multiple-value-bind (width height)
      (compute-gadget-label-size pane)
    (let ((thickness (slot-value pane 'thickness)))
      ;;--- Not right...
      (ecase (gadget-orientation pane)
	(:vertical
	  (make-space-requirement :width thickness
				  :height 0 :min-height 0 :max-height +fill+))
	(:horizontal 
	  (make-space-requirement :width width :min-width width :max-width +fill+
				  :height (+ thickness height 5)))))))

(defmethod repaint-sheet ((pane slider-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (let ((text (gadget-label pane))
	  (text-style (slot-value pane 'text-style))
	  (thickness (slot-value pane 'thickness))
	  (region (sheet-region pane)))
      (with-bounding-rectangle* (left top right bottom) region
	(ecase (gadget-orientation pane)
	  (:vertical
	    ;;--- What about the label?
	    (draw-rectangle* medium 
			     left top (+ left thickness) bottom
			     :filled nil))
	  (:horizontal
	    (draw-rectangle* medium 
			     left top right (+ top thickness)
			     :filled nil)
	    (draw-text* medium text left (+ top thickness 2) 
			:text-style text-style
			:align-x ':left :align-y ':top))))
      (draw-slider-indicator pane medium))))

(defmethod draw-slider-indicator ((pane slider-pane) medium &key (ink +foreground-ink+))
  (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
    (let* ((min-value (slot-value pane 'min-value))
	   (max-value (slot-value pane 'max-value))
	   (thickness (slot-value pane 'thickness))
	   (value (gadget-value pane))
	   (normal-value (compute-symmetric-value min-value max-value value 0 1)))
      (ecase (gadget-orientation pane)
	(:vertical
	  (let* ((height (- bottom top))
		 ;; slider marker is proportional
		 (y (+ top (* height normal-value))))
	    (draw-line* medium
			left y (+ left thickness) y
			:line-thickness 2 :ink ink)))
	(:horizontal
	  (let* ((width (- right left))
		 (mid-y (round thickness 2))
		 ;; slider marker is proportional
		 (x (+ left (* width normal-value)))
		 (dx 4)
		 (dy (- mid-y 2)))
	    (draw-polygon* medium (list (- x dx) mid-y
					x (- mid-y dy)
					(+ x dx) mid-y
					x (+ mid-y dy)
					(- x dx) mid-y)
			   :ink ink)))))))

(defmethod handle-event ((pane slider-pane) (event pointer-button-press-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed :active)
      (armed-callback pane (gadget-client pane) (gadget-id pane))
      (let ((x (pointer-event-x event))
	    (y (pointer-event-y event)))
	(with-bounding-rectangle* (left top right bottom) (sheet-region pane)
	  (ecase (gadget-orientation pane)
	    (:vertical
	      (update-slider-value pane y top bottom))
	    (:horizontal
	      (update-slider-value pane x left right))))))))

(defmethod handle-event ((pane slider-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane))
      (let ((x (pointer-event-x event))
	    (y (pointer-event-y event)))
	(with-bounding-rectangle* (left top right bottom) (sheet-region pane)
	  (ecase (gadget-orientation pane)
	    (:vertical
	      (update-slider-value pane y top bottom))
	    (:horizontal
	      (update-slider-value pane x left right)))))
      (value-changed-callback
	pane (gadget-client pane) (gadget-id pane) (gadget-value pane)))))

(defmethod handle-event ((pane slider-pane) (event pointer-motion-event))
  (with-slots (armed) pane
    (when (eql armed :active)
      (let ((x (pointer-event-x event))
	    (y (pointer-event-y event)))
	(with-bounding-rectangle* (left top right bottom) (sheet-region pane)
	  (ecase (gadget-orientation pane)
	    (:vertical
	      (update-slider-value pane y top bottom))
	    (:horizontal
	      (update-slider-value pane x left right)))))
      (drag-callback pane (gadget-client pane) (gadget-id pane) (gadget-value pane)))))

(defmethod handle-event ((pane slider-pane) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane slider-pane) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod update-slider-value ((pane slider-pane) coord min max)
  (let* ((min-value (slot-value pane 'min-value))
	 (max-value (slot-value pane 'max-value))
	 (value (compute-symmetric-value min max coord min-value max-value)))
    (setf (gadget-value pane) value)))

(defmethod (setf gadget-value) (nv (pane slider-pane))
  (with-slots (value) pane
    (setf value nv)))

(defmethod (setf gadget-value) :around (nv (pane slider-pane))
  (declare (ignore nv))
  (if (port pane)
      (with-sheet-medium (medium pane)
	(draw-slider-indicator pane medium :ink +background-ink+)
	(call-next-method)
	(draw-slider-indicator pane medium :ink +foreground-ink+))
      (call-next-method)))

