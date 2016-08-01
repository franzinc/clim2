;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-


"Copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)

;;; Sliders

;; To Do:
;; 
;; We should somehow support mini-sliders that conveniently fit on one
;; line.  This is certainly easy to do and I think I know how I'd like
;; them to look - but we should wait till some of the other gadgets are
;; done so we can give all the mini-gadgets the same look and feel.
;; 
;; We should do a multipane implementation of this where the slider rail
;; and end stops are in different panes. This would make it easier to add
;; a type-in current value field.
;; 
;; We should make disable/graying work
;; 
;; We should make indeterminate state work

(defparameter *default-horizontal-slider-pattern*
	      (make-pattern #2A((0 0 0 0 0 0 0 0 0 0 0 0)
				(0 1 1 1 1 1 1 1 1 1 0 0)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 0 0 0 0 1 0 0 0 0 1 1)
				(1 1 0 0 0 1 0 0 0 1 1 1)
				(0 1 1 1 1 1 1 1 1 1 1 1)
				(0 1 1 1 1 1 1 1 1 1 1 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *default-vertical-slider-pattern*
	      (make-pattern #2a((0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0) 
				(0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) 
				(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) 
				(0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) 
				(0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
				(0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *slider-rail-thickness* 6)
(defparameter *slider-rail-ink* (make-gray-color 2/3))
(defparameter *slider-tick-mark-height* 4)
(defparameter *slider-tick-mark-offset* 4)
(defparameter *slider-range-label-offset* 2)


(defclass slider-pane 
	  (slider
	   space-requirement-mixin
	   leaf-pane)
    ((label-width :initform 0)
     (label-height :initform 0)
     (visible-value-width :initform 0)
     (visible-value-height :initform 0)
     (margin :initform 8)
     (slider-button-pattern :initarg :slider-button-pattern :initform nil)
     ;; ARMED has three states:
     ;;  NIL ==> the slider is not armed
     ;;  T   ==> the slider is armed, waiting for a pointer button press
     ;;  :ACTIVE ==> the slider is armed, waiting for a pointer button release
     (armed :initform nil)
     (background-saved-p :initform nil)
     (saved-background :initform nil))
  (:default-initargs :value 0
		     :text-style *default-slider-label-text-style*))

(defmethod initialize-instance :after ((pane slider-pane) &key &allow-other-keys)
  (with-slots (slider-button-pattern) pane
    (unless slider-button-pattern
      (ecase (gadget-orientation pane)
	(:vertical
	  (setq slider-button-pattern *default-vertical-slider-pattern*)
	  (setf (slot-value pane 'pointer-cursor) :vertical-thumb))
	(:horizontal
	  (setq slider-button-pattern *default-horizontal-slider-pattern*)
	  (setf (slot-value pane 'pointer-cursor) :horizontal-thumb))))))

(defmethod compose-space ((pane slider-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (with-slots (slider-button-pattern label min-label max-label 
		 text-style range-label-text-style label-width label-height 
		 visible-value-width visible-value-height
		 show-value-p number-of-tick-marks) pane
      (let* ((pattern-width (pattern-width slider-button-pattern))
	     (pattern-height (pattern-height slider-button-pattern))
	     (margin (slot-value pane 'margin))
	     (tick-dist (if (> number-of-tick-marks 0)
			    (+ *slider-tick-mark-offset* *slider-tick-mark-height*)
			    0))
	     ;; Value width and height may one day have different text-styles from
	     ;; the label text style
	     (vwidth (* 10 (text-style-width text-style medium)))
	     (vheight (text-style-height text-style medium)))
	(multiple-value-bind (lwidth lheight)
	    (compute-gadget-label-size pane)
	  (setq label-width lwidth)
	  (setq label-height lheight)
	  (setq visible-value-width (if show-value-p vwidth 0))
	  (setq visible-value-height (if show-value-p vheight 0))
	  (ecase (gadget-orientation pane)
	    (:vertical
	      (let* ((max-length 
		       (max (if min-label (length min-label) 0)
			    (if max-label (length max-label) 0)))
		     (range-label-width
		       (if (or min-label max-label)
			   (+ *slider-range-label-offset*
			      (* max-length
				 (text-style-width range-label-text-style medium))
			      *slider-range-label-offset*)
			   0))
		     (rail-width (+ (ceiling *slider-rail-thickness* 2)
				    (* 2 (+ 4 tick-dist range-label-width)))))
		(make-space-requirement
		  :width (max label-width visible-value-width
			      (+ margin (max pattern-width rail-width) margin))
		  :height (+ (+ label-height visible-value-height (* 3 pattern-height)) 100)
		  :min-height (+ label-height visible-value-height (* 3 pattern-height))
		  :max-height +fill+)))
	    (:horizontal
	      (let* ((range-label-height
		       (if (or min-label max-label)
			   (+ *slider-range-label-offset*
			      (text-style-height range-label-text-style medium)
			      *slider-range-label-offset*)
			   0))
		     (rail-height
		       (+ (ceiling *slider-rail-thickness* 2) 4
			  tick-dist range-label-height)))
		(make-space-requirement
		  :width (+ (+ label-width visible-value-width (* 3 pattern-width)) 100)
		  :min-width (+ label-width visible-value-width (* 3 pattern-width))
		  :max-width +fill+
		  :height (+ margin (max pattern-height rail-height lheight) margin))))))))))


(defmethod handle-repaint ((pane slider-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (draw-slider-rail pane medium)
    (draw-slider-indicator pane medium :draw)
    (draw-slider-label pane medium)
    (when (slot-value pane 'show-value-p)
      (draw-visible-value pane medium))))

(defmethod draw-slider-rail ((pane slider-pane) medium)
  (with-slots (label-width label-height text-style min-label max-label
	       number-of-tick-marks margin visible-value-width visible-value-height) pane
    (let ((region (sheet-region pane))
	  (half-thickness (floor *slider-rail-thickness* 2)))
      (with-bounding-rectangle* (left top right bottom) region
	(let* ((center-x (floor (+ right left) 2))
	       (center-y (floor (+ bottom top) 2))
	       (offset 2)
	       (tick-offset 
		 (if (> number-of-tick-marks 0)
		     (+ *slider-tick-mark-offset* *slider-tick-mark-height*)
		     0))
	       (label-offset
		 (+ half-thickness tick-offset *slider-range-label-offset*)))
	  (decf right) (decf bottom)
	  (incf left margin) (incf top margin) (decf right margin) (decf bottom margin)
	  (ecase (gadget-orientation pane)
	    (:vertical
	      (incf top (+ label-height visible-value-height))
	      (draw-rectangle* medium
			       (- center-x half-thickness) top
			       (+ center-x half-thickness) bottom
			       :filled t :ink *slider-rail-ink*)
	      (draw-rectangle* medium
			       (- center-x half-thickness) top
			       (+ center-x half-thickness) bottom
			       :filled nil)
	      (draw-rectangle* medium
			       (- center-x half-thickness offset) (- top offset)
			       (+ center-x half-thickness offset) (+ bottom offset)
			       :filled nil))
	    (:horizontal
	      (incf left (+ label-width visible-value-width))
	      (draw-rectangle* medium
			       left (- center-y half-thickness)
			       right (+ center-y half-thickness)
			       :filled t :ink *slider-rail-ink*)
	      (draw-rectangle* medium
			       left (- center-y half-thickness)
			       right (+ center-y half-thickness)
			       :filled nil)
	      (draw-rectangle* medium
			       (- left offset) (- center-y half-thickness offset) 
			       (+ right offset) (+ center-y half-thickness offset)
			       :filled nil)))
	  ;; Must be after the ecase because of adjustments to left etc...
	  (when (> number-of-tick-marks 0)
	    (draw-slider-tick-marks
	      pane medium center-x center-y tick-offset left top right bottom))
	  (draw-slider-range-labels
	    pane medium center-x center-y label-offset left top right bottom))))))

(defmethod draw-slider-tick-marks ((pane slider-pane)
				   medium center-x center-y tick-offset
				   left top right bottom)
  (with-slots (number-of-tick-marks) pane
    (let ((half-thickness (floor *slider-rail-thickness* 2)))
      (ecase (gadget-orientation pane)
	(:vertical
	  (dotimes (i number-of-tick-marks)
	    (let ((y (compute-symmetric-value 0 number-of-tick-marks i top bottom)))
	      (draw-line* medium
			  (+ center-x half-thickness *slider-tick-mark-offset*) y
			  (+ center-x half-thickness tick-offset) y
			  :ink +foreground-ink+))))
	(:horizontal
	  (dotimes (i number-of-tick-marks)
	    (let ((x (compute-symmetric-value 0 number-of-tick-marks i left right)))
	      (draw-line* medium
			  x (+ center-y half-thickness *slider-tick-mark-offset*)
			  x (+ center-y half-thickness tick-offset)
			  :ink +foreground-ink+))))))))

(defmethod draw-slider-range-labels ((pane slider-pane)
				     medium center-x center-y label-offset
				     left top right bottom)
  (with-slots (min-label max-label range-label-text-style) pane
    (ecase (gadget-orientation pane)
      (:vertical
	(when max-label
	  (draw-text* medium max-label (+ center-x label-offset) bottom
		      :text-style range-label-text-style
		      :align-x ':left :align-y ':bottom))
	(when min-label
	  (draw-text* medium min-label (+ center-x label-offset) top
		      :text-style range-label-text-style
		      :align-x ':left :align-y ':top)))
      (:horizontal
	(when min-label
	  (draw-text* medium min-label left (+ center-y label-offset)
		      :text-style range-label-text-style
		      :align-x ':left :align-y ':top))
	(when max-label
	  (draw-text* medium max-label right (+ center-y label-offset)
		      :text-style range-label-text-style
		      :align-x ':right :align-y ':top))))))

(defmethod draw-slider-indicator ((pane slider-pane) medium action)
  (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
    (with-slots (min-value max-value label-width margin slider-button-pattern
		 visible-value-width background-saved-p saved-background
		 label-height visible-value-height) pane
      (let* ((value (gadget-value pane))
	     (normal-value (compute-symmetric-value min-value max-value value 0 1))
	     (pattern-width (pattern-width slider-button-pattern))
	     (pattern-height (pattern-height slider-button-pattern))
	     width height x y)
	(ecase (gadget-orientation pane)
	  (:vertical
	    (setq width (- right left)
		  height (- bottom top label-height visible-value-height margin margin)
		  x (- (floor width 2) (floor pattern-width 2))
		  y (- (+ top margin label-height visible-value-height (* height normal-value))
		       (floor pattern-height 2))))
	  (:horizontal
	    (setq width (- right left label-width visible-value-width margin margin)
		  height (- bottom top)
		  y (- (floor height 2) (floor pattern-height 2))
		  x (- (+ left label-width visible-value-width margin (* width normal-value))
		       (floor pattern-width 2)))))
	(ecase action
	  (:draw
	    (when (null saved-background)
	      (setq saved-background 
		    (allocate-pixmap medium pattern-width pattern-height)))
	    (copy-to-pixmap medium x y pattern-width pattern-height
			    saved-background)
	    (setq background-saved-p t)
	    (draw-pattern* medium slider-button-pattern x y))
	  (:erase
	    (if background-saved-p
		(progn
		  (copy-from-pixmap saved-background 0 0 pattern-width pattern-height
				    medium x y)
		  (setq background-saved-p nil))
		(draw-slider-rail pane medium))))))))

(defmethod draw-slider-label ((pane slider-pane) medium)
  (with-slots (label-height margin) pane
    (let ((region (sheet-region pane)))
      (with-bounding-rectangle* (left top right bottom) region
	right
	(ecase (gadget-orientation pane)
	  (:vertical
	    (draw-gadget-label pane medium
			       (floor (+ left right) 2) (+ top label-height)
			       :align-x ':center :align-y ':bottom))
	  (:horizontal
	    (draw-gadget-label pane medium
			       (+ left margin) (floor (+ bottom top) 2)
			       :align-x ':left :align-y ':center)))))))

(defmethod draw-visible-value ((pane slider-pane) medium)
  (with-slots (text-style margin label-width label-height
	       visible-value-width visible-value-height 
	       slider-button-pattern decimal-places) pane
    (let* ((region (sheet-region pane))
	   (pattern-width (pattern-width slider-button-pattern))
	   (pattern-height (pattern-height slider-button-pattern))
	   (x-offset (- (floor pattern-width 2)))
	   (y-offset (- (floor pattern-height 2)))
	   (value (gadget-value pane))
	   (places (expt 10 decimal-places)))
      ;; Float it so that it looks better in the display
      (setq value (float (/ (round (* value places)) places)))
      (with-bounding-rectangle* (left top right bottom) region
	(ecase (gadget-orientation pane)
	  (:vertical
	    (medium-clear-area 
	      medium
	      left (+ top label-height)
	      right (+ top margin label-height visible-value-height y-offset))
	    (draw-text* medium (format nil "~A" value)
			(floor (+ left right) 2) (+ top label-height visible-value-height)
			:text-style text-style
			:align-x ':center :align-y ':bottom))
	  (:horizontal
	    (medium-clear-area
	      medium 
	      (+ left margin label-width) top
	      (+ left margin label-width visible-value-width x-offset) bottom)
	    (draw-text* medium (format nil "~A" value)
			(+ left margin label-width) (+ top (floor (- bottom top) 2))
			:text-style text-style
			:align-x ':left :align-y ':center)))))))


(defmethod handle-event :around ((pane slider-pane) (event pointer-event))
  (when (gadget-active-p pane)
    (call-next-method))
  (deallocate-event event))

(defmethod handle-event ((pane slider-pane) (event pointer-enter-event))
  (declare (special *pointer-documentation-output*))
  (with-slots (armed) pane
    (unless armed
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
    (frame-manager-display-pointer-documentation-string
      (frame-manager pane) (pane-frame pane) *pointer-documentation-output*
      "L,M,R: Move the slider indicator.")))

(defmethod handle-event ((pane slider-pane) (event pointer-exit-event))
  (declare (special *pointer-documentation-output*))
  (with-slots (armed) pane
    (when armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))
    (frame-manager-display-pointer-documentation-string
      (frame-manager pane) (pane-frame pane) *pointer-documentation-output* nil)))

(defmethod handle-event ((pane slider-pane) (event pointer-button-press-event))
  (with-slots (armed margin visible-value-width visible-value-height
	       show-value-p label-width label-height) pane
    (when armed
      (setf armed :active)
      (let ((x (pointer-event-x event))
	    (y (pointer-event-y event)))
	(with-bounding-rectangle* (left top right bottom) (sheet-region pane)
	  (ecase (gadget-orientation pane)
	    (:vertical
	      (update-slider-value pane y
				   (+ top margin label-height visible-value-height)
				   (- bottom margin)))
	    (:horizontal
	      (update-slider-value pane x
				   (+ left margin label-width visible-value-width)
				   (- right margin))))))
      (when show-value-p
	(with-sheet-medium (medium pane)
	  (draw-visible-value pane medium))))))

(defmethod handle-event ((pane slider-pane) (event pointer-button-release-event))
  (with-slots (armed margin visible-value-width visible-value-height
	       show-value-p label-width label-height) pane
    (when (eq armed :active)
      (setf armed t)
      (let ((x (pointer-event-x event))
	    (y (pointer-event-y event)))
	(with-bounding-rectangle* (left top right bottom) (sheet-region pane)
	  (ecase (gadget-orientation pane)
	    (:vertical
	      (update-slider-value pane y 
				   (+ top margin label-height visible-value-height)
				   (- bottom margin)))
	    (:horizontal
	      (update-slider-value pane x
				   (+ left margin label-width visible-value-width)
				   (- right margin))))))
      (when show-value-p
	(with-sheet-medium (medium pane)
	  (draw-visible-value pane medium)))
      (value-changed-callback
	pane (gadget-client pane) (gadget-id pane) (gadget-value pane)))))

(defmethod handle-event ((pane slider-pane) (event pointer-motion-event))
  (with-slots (armed margin visible-value-width visible-value-height
	       show-value-p label-width label-height) pane
    (when (eq armed :active)
      (let ((old-value (gadget-value pane))
	    (x (pointer-event-x event))
	    (y (pointer-event-y event)))
	(with-bounding-rectangle* (left top right bottom) (sheet-region pane)
	  (ecase (gadget-orientation pane)
	    (:vertical
	      (update-slider-value pane y 
				   (+ top margin label-height visible-value-height)
				   (- bottom margin)))
	    (:horizontal
	      (update-slider-value pane x
				   (+ left margin label-width visible-value-width)
				   (- right margin)))))
	(unless (eq old-value (gadget-value pane))
	  (when show-value-p
	    (with-sheet-medium (medium pane)
	      (draw-visible-value pane medium)))
	  (drag-callback
	    pane (gadget-client pane) (gadget-id pane) (gadget-value pane)))))))

(defmethod update-slider-value ((pane slider-pane) coord min max)
  (with-slots (min-value max-value number-of-quanta) pane
    (let* ((value (max min-value
		       (min max-value
			    (compute-symmetric-value min max coord min-value max-value)))))
      (when number-of-quanta
	(let ((quanta-size (/ (abs (- max-value min-value)) number-of-quanta)))
	  (setq value (* quanta-size (round (/ value (float quanta-size)))))))
      (setf (gadget-value pane) value))))

(defmethod (setf gadget-value) :around (value (pane slider-pane) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (if (port pane)
      (with-sheet-medium (medium pane)
	(draw-slider-indicator pane medium :erase)
	(call-next-method)
	(draw-slider-indicator pane medium :draw)
	(when (slot-value pane 'show-value-p)
	  (draw-visible-value pane medium)))
      (call-next-method)))

