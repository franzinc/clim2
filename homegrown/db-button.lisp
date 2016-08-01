;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-


"Copyright (c) 1991, 1992 by Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1990, 1991 International Lisp Associates."

(in-package :silica)


#|

To Do:

Split toggle-button into check-box-button and radio-button classes that share a
toggle button base. This way they can share the draw code.

|#


;;; Shared button functionality

(defclass button-pane-mixin 
	  (space-requirement-mixin
	   leaf-pane)
    ;; ARMED has three states:
    ;;  NIL ==> the button is not armed
    ;;  T   ==> the button is armed, waiting for a pointer button press
    ;;  :ACTIVE ==> the button is armed, waiting for a pointer button release
    ((armed :initform nil))
  (:default-initargs :pointer-cursor :button))

;; General highlight-by-inverting method
(defmethod highlight-button ((pane button-pane-mixin) medium)
  (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
    (draw-rectangle* medium left top right bottom
		     :ink +flipping-ink+ :filled t)
    ;; Do this for the benefit of X displays
    (medium-force-output medium)))

(defmethod handle-event :around ((pane button-pane-mixin) (event pointer-event))
  (when (gadget-active-p pane)
    (call-next-method))
  (deallocate-event event))

(defmethod handle-event ((pane button-pane-mixin) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (cond ((let ((pointer (pointer-event-pointer event)))
	       (and (pointer-button-state pointer)
		    (not (zerop (pointer-button-state pointer)))))
	     (setf armed :active)
	     (with-sheet-medium (medium pane)
	       (highlight-button pane medium)))
	    (t (setf armed t)))
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane button-pane-mixin) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (when (prog1 (eq armed :active) (setf armed nil))
	(with-sheet-medium (medium pane)
	  (highlight-button pane medium)))
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


;;; Patterns for push buttons

(defparameter *square-button-pattern*
	      (make-pattern #2a((0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *circle-button-pattern*
	      (make-pattern #2a((0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0) 
				(0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0) 
				(0 0 1 1 0 0 0 0 0 0 0 0 1 1 0 0) 
				(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) 
				(0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0) 
				(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) 
				(0 0 1 1 0 0 0 0 0 0 0 0 1 1 0 0) 
				(0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0) 
				(0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *right-triangle-button-pattern*
	      (make-pattern #2a((0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0) 
				(0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0) 
				(0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0) 
				(0 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0) 
				(0 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0) 
				(0 1 0 0 0 0 0 0 0 0 1 1 1 0 0 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 1 1 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 1 1 1 0) 
				(0 1 0 0 0 0 0 0 0 0 1 1 1 0 0 0) 
				(0 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0) 
				(0 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0) 
				(0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0) 
				(0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0) 
				(0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *left-triangle-button-pattern*
	      (make-pattern #2a((0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0) 
				(0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0) 
				(0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0) 
				(0 0 0 0 0 0 0 1 1 1 0 0 0 0 1 0) 
				(0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 0) 
				(0 0 0 1 1 1 0 0 0 0 0 0 0 0 1 0) 
				(0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 0 0 1 1 1 0 0 0 0 0 0 0 0 1 0) 
				(0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 0) 
				(0 0 0 0 0 0 0 1 1 1 0 0 0 0 1 0) 
				(0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0) 
				(0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0) 
				(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *rounded-button-pattern*
	      (make-pattern #2a((0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0) 
				(0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0) 
				(0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0) 
				(0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0) 
				(0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0) 
				(0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0))
			    (list +background-ink+ +foreground-ink+)))


;;; Push buttons

(defparameter *default-button-label-text-style*
	      (make-text-style :sans-serif :bold :small))

(defclass push-button-pane (push-button button-pane-mixin)
    ((external-label :initarg :external-label)
     (depth :initarg :depth)
     ;; These slots hold the patterns we'll really use...
     (normal-pattern :initarg :normal-pattern)
     (depressed-pattern :initarg :depressed-pattern)
     (x-margin :initarg :x-margin)
     (y-margin :initarg :y-margin)
     ;;--- kludge because DRAW-TEXT* :ALIGN-X :CENTER is wrong
     (internal-label-offset :initform 0))
  (:default-initargs :label nil
		     :text-style *default-button-label-text-style*
		     :show-as-default nil
		     :external-label nil
		     :depth 2
		     :pattern *square-button-pattern*
		     :icon-pattern nil
		     :normal-pattern nil
		     :x-margin 2 :y-margin 0))

(defmethod compose-space ((pane push-button-pane) &key width height)
  (declare (ignore width height))
  (with-slots (normal-pattern external-label x-margin y-margin) pane
    (unless normal-pattern (setup-patterns pane))
    (let* ((pattern-width (pattern-width normal-pattern))
	   (pattern-height (pattern-height normal-pattern))
	   (ext-label-width 0)
	   (ext-label-height 0))
      (when external-label
	(let ((text-style (slot-value pane 'text-style)))
	  (with-sheet-medium (medium pane)
	    (multiple-value-bind (w h)
		(text-size medium external-label :text-style text-style)
	      (setq ext-label-width (+ w (text-style-width text-style medium))
		    ext-label-height (+ h (floor
					    (text-style-height text-style medium) 2)))))))
      (make-space-requirement
	:width (+ x-margin ext-label-width pattern-width x-margin)
	:height (+ y-margin (max ext-label-height pattern-height) y-margin)))))

;; There are 3 cases:
;;  1) A simple pattern - no label
;;  2) A simple pattern with internal label
;;  3) An icon pattern and simple border pattern
;; We don't do this at INITIALIZE-INSTANCE time since the medium is not
;; yet set up at that time, meaning we can't compute label sizes, etc.
(defmethod setup-patterns ((pane push-button-pane))
  (with-slots (pattern icon-pattern normal-pattern depressed-pattern depth
	       label internal-label-offset text-style) pane
    (with-sheet-medium (medium pane)
      (let ((text-margin (text-style-width text-style medium))
	    (label-width 0)
	    (label-height 0)
	    inside-left inside-top inside-right inside-bottom 
	    inside-width inside-height)
	(when (or label icon-pattern)
	  (multiple-value-setq (inside-left inside-top inside-right inside-bottom)
	    (inside-extent-button-pattern pattern))
	  (setq inside-width (- inside-right inside-left)
		inside-height (- inside-bottom inside-top)))
	(when icon-pattern
	  (setq pattern (mount-button-pattern icon-pattern pattern)))
	(when label
	  (multiple-value-setq (label-width label-height)
	    (compute-gadget-label-size pane))
	  ;; I like a tiny bit more margin
	  (incf label-width (* 2 text-margin))
	  (setq label-width (max label-width inside-width)
		label-height (max label-height inside-height))
	  (when (or (> label-width inside-width)
		    (> label-height inside-height))
	    (setq pattern (grow-button-pattern pattern label-width label-height))))
	(setq normal-pattern (refit-button-pattern pattern :overhang 4)
	      depressed-pattern (offset-button-pattern normal-pattern
						       :x-offset depth :y-offset depth))
	;;--- Kludge because DRAW-TEXT* :ALIGN-X :CENTER is wrong
	(when label
	  (setq internal-label-offset
		(+ text-margin (floor (- (pattern-width normal-pattern) label-width) 2))))
	(setq normal-pattern
	      (shadow-button-pattern normal-pattern :depth depth))))))

(defmethod highlight-button ((pane push-button-pane) medium)
  (draw-button-pattern pane medium)
  ;; Do this for the benefit of X displays
  (medium-force-output medium))

(defmethod handle-event ((pane push-button-pane) (event pointer-button-press-event))
  (with-slots (armed) pane
    (when armed
      (setf armed :active)
      (with-sheet-medium (medium pane)
	(highlight-button pane medium))
      ;; So you can see it even if you let go quickly...
      (sleep 0.25))))

(defmethod handle-repaint ((pane push-button-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (draw-button-pattern pane medium :draw-external-label-p t)))

(defmethod draw-button-pattern ((pane push-button-pane) medium
				&key (draw-external-label-p nil))
  (with-slots (normal-pattern depressed-pattern armed external-label
	       text-style label depth internal-label-offset
	       x-margin y-margin) pane
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane) right
      (let* ((pattern (if (eq armed :active) depressed-pattern normal-pattern))
	     (pattern-width (pattern-width pattern))
	     (pattern-height (pattern-height pattern))
	     (pattern-center-y (floor (+ top bottom) 2))
	     (pattern-top (- pattern-center-y (floor pattern-height 2) y-margin))
	     (pattern-left (+ left x-margin)))
	(draw-pattern* medium pattern pattern-left pattern-top)
	;; I don't like this - we really need to be able to draw text into patterns
	(when label
	  (let ((offset (if (eq armed :active) depth 0)))
	    (draw-text* medium label
			;;--- Kludge because DRAW-TEXT* :ALIGN-X :CENTER is wrong
			(+ left x-margin internal-label-offset offset)
			(+ pattern-center-y offset)
			:text-style text-style
			:align-x :left :align-y :center)))
	(when draw-external-label-p
	  (when external-label
	    (draw-text* medium external-label
			(+ pattern-left pattern-width x-margin) pattern-center-y
			:text-style text-style
			:align-x :left :align-y :center)))))))


;;; Utilities for transmogrifying buttons

;; Creates a new pattern which consists of the supplied pattern plus 
;; a drop shadow
(defvar *shadow-button-pattern-cache* (make-hash-table :test 'equal))
(defun shadow-button-pattern (original-pattern &key (depth 2))
  (multiple-value-bind (result foundp)
      (gethash (cons original-pattern depth) *shadow-button-pattern-cache*)
    (when foundp 
      (return-from shadow-button-pattern result)))
  (let* ((result-pattern (copy-pattern original-pattern))
	 (width (pattern-width result-pattern))
	 (height (pattern-height result-pattern))
	 (array (decode-pattern result-pattern)))
    (loop with y = (1- height)
	  for x from 1 to (1- width)
	  do (shadow-button-scan array x y width height depth))
    (loop with x = (1- width)
	  for y from (- height 2) downto 1
	  do (shadow-button-scan array x y width height depth))
    (setf (gethash (cons original-pattern depth) *shadow-button-pattern-cache*)
	  result-pattern)))

(defun copy-pattern (pattern)
  (multiple-value-bind (array designs) (decode-pattern pattern)
    (let* ((pattern-width (pattern-width pattern))
	   (pattern-height (pattern-height pattern))
	   (new-array
	     (make-array (list pattern-height pattern-width)
			 :element-type `(unsigned-byte ,(integer-length (1- (length designs))))
			 :initial-element 0)))
      (loop for x from 0 to (1- pattern-width)
	    do (loop for y from 0 to (1- pattern-height)
		     do (setf (aref new-array y x) (aref array y x))))
      (make-pattern new-array designs))))

(defun shadow-button-scan (array start-x start-y width height depth)
  (loop with x = start-x
	with y = start-y
	with limit = (floor height 2)
	with done = nil
	for steps from 0
	until done
	do (decf x)
	   (decf y)
	   (if (or (<= x 0) (<= y 0) (> steps limit))
	       (setq done t)
	       (when (not (= 0 (aref array y x)))
		 (loop with nx = (1+ x)
		       with ny = (1+ y)
		       repeat depth
		       until (or (>= nx width) (>= ny height))
		       do (setf (aref array ny nx) 1)
			  (incf nx) (incf ny))
		 (setq done t)))))

;; Creates a new pattern which consists of the supplied pattern offset
;; by the specified amount
(defvar *offset-button-pattern-cache* (make-hash-table :test 'equal))
(defun offset-button-pattern (original-pattern &key (x-offset 2) (y-offset 2)
						    (new-width nil) (new-height nil))
  (let ((width (pattern-width original-pattern))
	(height (pattern-height original-pattern)))
    (unless new-width (setq new-width width))
    (unless new-height (setq new-height height))
    (multiple-value-bind (result foundp)
	(gethash (list original-pattern x-offset y-offset new-width new-height)
		 *offset-button-pattern-cache*)
      (when foundp 
	(return-from offset-button-pattern result)))
    (multiple-value-bind (array designs) (decode-pattern original-pattern)
      (let* ((new-array 
	       (make-array (list new-height new-width)
			   :element-type `(unsigned-byte ,(integer-length (1- (length designs))))
			   :initial-element 0))
	     (result-pattern (make-pattern new-array designs)))
	(loop for x from 0 to (1- width)
	      do (loop for y from 0 to (1- height)
		       for new-x = (+ x x-offset)
		       for new-y = (+ y y-offset)
		       unless (or (< new-x 0) (> new-x (1- new-width))
				  (< new-y 0) (> new-y (1- new-height)))
			 do (setf (aref new-array (+ y y-offset) (+ x x-offset))
				  (aref array y x))))
	(setf (gethash (list original-pattern x-offset y-offset new-width new-height)
		       *offset-button-pattern-cache*)
	      result-pattern)))))

;; Creates a new pattern which consists of the supplied pattern plus a drop shadow
(defvar *refit-button-pattern-cache* (make-hash-table :test 'equal))
(defun refit-button-pattern (original-pattern &key (overhang 0))
  (multiple-value-bind (result foundp)
      (gethash (cons original-pattern overhang) *refit-button-pattern-cache*)
    (when foundp 
      (return-from refit-button-pattern result)))
  (multiple-value-bind (original-array designs)
      (decode-pattern original-pattern)
    (multiple-value-bind (left top right bottom)
	(bounding-extent-button-pattern original-pattern)
      (let* ((new-width (+ (- right left) 1 (* 2 overhang)))
	     (new-height (+ (- bottom top) 1 (* 2 overhang)))
	     (x-offset (- left overhang))
	     (y-offset (- top overhang))
	     (new-array 
	       (make-array (list new-height new-width)
			   :element-type `(unsigned-byte ,(integer-length (1- (length designs))))
			   :initial-element 0)))
	(loop for x from left to right
	      do (loop for y from top to bottom
		       do (setf (aref new-array (- y y-offset) (- x x-offset))
				(aref original-array y x))))
	(let ((result-pattern (make-pattern new-array designs)))
	  (setf (gethash (cons original-pattern overhang) *refit-button-pattern-cache*)
		result-pattern))))))

(defun bounding-extent-button-pattern (pattern)
  (let ((width (pattern-width pattern))
	(height (pattern-height pattern))
	(array (decode-pattern pattern))
	left bottom right top)
    (loop with done = nil
	  for x from 0 to (1- width)
	  until done
	  do (loop for y from 0 to (1- height)
		   until done
		   do (unless (= 0 (aref array y x))
			(setq done t
			      left x))))
    (loop with done = nil
	  for x from (1- width) downto 0
	  until done
	  do (loop for y from 0 to (1- height)
		   until done
		   do (unless (= 0 (aref array y x))
			(setq done t
			      right x))))
    (loop with done = nil
	  for y from (1- height) downto 0
	  until done
	  do (loop for x from 0 to (1- width)
		   until done
		   do (unless (= 0 (aref array y x))
			(setq done t
			      bottom y))))
    (loop with done = nil
	  for y from 0 to (1- height)
	  until done
	  do (loop for x from 0 to (1- width)
		   until done
		   do (unless (= 0 (aref array y x))
			(setq done t
			      top y))))
    (values left top right bottom)))

(defun operate-on-button-patterns (source-1 source-2 destination operation)
  (let ((source-1-width (pattern-width source-1))
	(source-2-width (when source-2 (pattern-width source-2)))
	(destination-width (pattern-width destination))
	(source-1-height (pattern-height source-1))
	(source-2-height (when source-2 (pattern-height source-2)))
	(destination-height (pattern-height destination))
	(source-1-array (decode-pattern source-1))
	(source-2-array (when source-2 (decode-pattern source-2)))
	(destination-array (decode-pattern destination)))
    (unless (and (= source-1-width destination-width)
		 (or (null source-2) (= source-2-width source-1-width))
		 (= source-1-height destination-height)
		 (or (null source-2) (= source-2-height source-1-height)))
      (error "Incompatible dimensions"))
    (loop for x from 0 to (1- source-1-width)
	  do (loop for y from 0 to (1- source-1-height)
		   do (if source-2
			  (setf (aref destination-array y x)
				(funcall operation
					 (aref source-1-array y x)
					 (aref source-2-array y x)))
			  (setf (aref destination-array y x)
				(funcall operation
					 (aref source-1-array y x))))))))

;; Creates an outlined copy of the original pattern in the destination
;; pattern, clobbering the destination in the process
(defun outline-button-pattern (original-pattern destination-pattern
			       &key (space 1) (thickness 2))
  (assert (not (eq original-pattern destination-pattern))
	  (original-pattern destination-pattern)
	  "The original and destination patterns must be different objects")
  (let ((width (pattern-width original-pattern))
	(height (pattern-height original-pattern))
	(array (decode-pattern original-pattern))
	(dest-array (decode-pattern destination-pattern)))
    (loop with done = nil
	  for x from 0 to (1- width)
	  until done
	  do (loop for y from 0 to (1- height)
		   until done
		   do (unless (= 0 (aref array y x))
			(outline-button-pattern-scan dest-array x y -1 0 space thickness)
			(setq done t))))
    (loop with done = nil
	  for x from (1- width) downto 0
	  until done
	  do (loop for y from 0 to (1- height)
		   until done
		   do (unless (= 0 (aref array y x))
			(outline-button-pattern-scan dest-array x y 1 0 space thickness)
			(setq done t))))
    (loop with done = nil
	  for y from (1- height) downto 0
	  until done
	  do (loop for x from 0 to (1- width)
		   until done
		   do (unless (= 0 (aref array y x))
			(outline-button-pattern-scan dest-array x y 0 1 space thickness)
			(setq done t))))
    (loop with done = nil
	  for y from 0 to (1- height)
	  until done
	  do (loop for x from 0 to (1- width)
		   until done
		   do (unless (= 0 (aref array y x))
			(outline-button-pattern-scan dest-array x y 0 1 space thickness)
			(setq done t))))))

(defun outline-button-pattern-scan (dest-array start-x start-y dx dy space thickness)
  (loop with x = start-x
	with y = start-y
	repeat space
	do (incf x dx)
	   (incf y dy))
  (loop with x = start-x
	with y = start-y
	with width-1 = (array-dimension dest-array 1)
	with height-1 = (array-dimension dest-array 0)
	repeat thickness
	until (or (< x 0) (< y 0) (> x width-1) (> y height-1))
	do (incf x dx)
	   (incf y dy)
	   (setf (aref dest-array y x) 1)))

;; Desstructively "grays" the supplied pattern
(defun gray-button-pattern (original-pattern)
  (let ((width (pattern-width original-pattern))
	(height (pattern-height original-pattern))
	(array (decode-pattern original-pattern)))
    (loop for x from (1- width) downto 0
	  do (loop for y from (1- height) downto 0
		   do (if (oddp x)
			  (when (evenp y) (setf (aref array y x) 0))
			  (when (oddp y) (setf (aref array y x) 0)))))
    original-pattern))

;; Creates a new pattern consisting of the supplied pattern "stretched"
;; to the new dimensions
(defvar *grow-button-pattern-cache* (make-hash-table :test 'equal))
(defun grow-button-pattern (original-pattern new-width new-height)
  (let ((original-width (pattern-width original-pattern))
	(original-height (pattern-height original-pattern)))
    (maxf new-width original-width)
    (maxf new-height original-height)
    (multiple-value-bind (result foundp)
	(gethash (list original-pattern new-width new-height) *grow-button-pattern-cache*)
      (when foundp 
	(return-from grow-button-pattern result)))
    (multiple-value-bind (original-array designs)
	(decode-pattern original-pattern)
      (let ((center-x (floor original-width 2))
	    (center-y (floor original-height 2))
	    (intermediate-array
	      (make-array (list original-height new-width)
			  :element-type `(unsigned-byte ,(integer-length (1- (length designs)))) 
			  :initial-element 0))
	    (final-array 
	      (make-array (list new-height new-width) 
			  :element-type `(unsigned-byte ,(integer-length (1- (length designs))))
			  :initial-element 0)))
	(loop for x from 0 to center-x
	      do (loop for y from 0 to (1- original-height)
		       do (setf (aref intermediate-array y x) (aref original-array y x))))
	(loop for x from (1- new-width) downto (- new-width center-x)
	      for old-x from (1- original-width) by -1
	      do (loop for y from 0 to (1- original-height)
		       do (setf (aref intermediate-array y x) (aref original-array y old-x))))
	(loop for x from (1+ center-x) to (- new-width center-x 1)
	      do (loop for y from 0 to (1- original-height)
		       do (setf (aref intermediate-array y x)
				(aref original-array y center-x))))
	;; Now the other direction
	(loop for y from 0 to center-y
	      do (loop for x from 0 to (1- new-width)
		       do (setf (aref final-array y x) (aref intermediate-array y x))))
	(loop for y from (1- new-height) downto (- new-height center-y)
	      for old-y from (1- original-height) by -1
	      do (loop for x from 0 to (1- new-width)
		       do (setf (aref final-array y x) (aref intermediate-array old-y x))))
	(loop for y from (1+ center-y) to (- new-height center-y 1)
	      do (loop for x from 0 to (1- new-width)
		       do (setf (aref final-array y x) (aref intermediate-array center-y x))))
	(let ((result-pattern (make-pattern final-array designs)))
	  (setf (gethash (list original-pattern new-width new-height) 
			 *grow-button-pattern-cache*)
		result-pattern))))))

;; Creates a new pattern which consists of the icon pattern mounted within
;; the appropriately sized button pattern
(defvar *mount-button-pattern-cache* (make-hash-table :test 'equal))
(defun mount-button-pattern (icon-pattern button-pattern)
  (multiple-value-bind (result foundp)
      (gethash (cons icon-pattern button-pattern) *mount-button-pattern-cache*)
    (when foundp 
      (return-from mount-button-pattern result)))
  (multiple-value-bind (ileft itop iright ibottom)
      (inside-extent-button-pattern button-pattern)
    (let* ((icon-width (pattern-width icon-pattern))
	   (icon-height (pattern-height icon-pattern))
	   (icon-array (decode-pattern icon-pattern))
	   (background-width (pattern-width button-pattern))
	   (background-height (pattern-height button-pattern))
	   (button-inside-width (- iright ileft))
	   (button-inside-height (- ibottom itop))
	   (white-space 2)
	   (border-width (+ (* 2 white-space) (- background-width button-inside-width)))
	   (border-height (+ (* 2 white-space) (- background-height button-inside-height)))
	   result-pattern)
      (if (or (> icon-width button-inside-width)
	      (> icon-height button-inside-height))
	  (setq button-inside-width (+ (max icon-width button-inside-width) border-width)
		button-inside-height (+ (max icon-height button-inside-height) border-height)
		result-pattern (grow-button-pattern button-pattern
						    button-inside-width
						    button-inside-height))
	  (setq result-pattern (copy-pattern button-pattern)))
      (let ((x-offset (floor border-width 2))
	    (y-offset (floor border-height 2))
	    (result-array (decode-pattern result-pattern)))
	(loop for x from 0 to (1- icon-width)
	      do (loop for y from 0 to (1- icon-height)
		       do (setf (aref result-array (+ y y-offset) (+ x x-offset))
				(aref icon-array y x)))))
      (setf (gethash (cons icon-pattern button-pattern) *mount-button-pattern-cache*)
	    result-pattern))))

(defun inside-extent-button-pattern (pattern)
  (let* ((width (pattern-width pattern))
	 (height (pattern-height pattern))
	 (array (decode-pattern pattern))
	 (center-x (floor width 2))
	 (center-y (floor height 2))
	 (initial-height 1)
	 left bottom right top real-width)
    (loop with done = nil
	  for x from center-x downto 0
	  until done
	  do (loop for y from (- center-y initial-height) to (+ center-y initial-height)
		   until done
		   do (unless (= 0 (aref array y x))
			(setq done t
			      left (1+ x)))))
    (loop with done = nil
	  for x from center-x to (1- width)
	  until done
	  do (loop for y from (- center-y initial-height) to (+ center-y initial-height)
		   until done
		   do (unless (= 0 (aref array y x))
			(setq done t
			      right (1- x)))))
    (setq real-width (floor (- right left) 2))
    (loop with done = nil
	  for y from center-y to (1- height)
	  until done
	  do (loop for x from (- center-x real-width) to (+ center-x real-width)
		   until done
		   do (unless (= 0 (aref array y x))
			(setq done t
			      bottom (1- y)))))
    (loop with done = nil
	  for y from center-y downto 0
	  until done
	  do (loop for x from (- center-x real-width) to (+ center-x real-width)
		   until done
		   do (unless (= 0 (aref array y x))
			(setq done t
			      top (1+ y)))))
    (values left top right bottom)))


;;; Patterns for toggle buttons and check boxes

(defparameter *empty-check-box-pattern*
	      (make-pattern #2a((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *crossed-check-box-pattern*
	      (make-pattern #2a((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 1 1 0 0 0 0 0 1 1 0 1 1 0 0 0) 
				(0 0 0 1 1 0 1 1 1 0 0 0 1 1 1 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 1 1 1 0 1 1 1 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 1 1 1 1 1 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 1 1 1 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 1 1 1 1 1 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 1 1 1 0 1 1 1 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 1 1 1 0 0 0 1 1 1 0 1 1 0 0 0) 
				(0 0 0 1 1 0 1 1 0 0 0 0 0 1 1 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *checked-check-box-pattern*
	      (make-pattern #2a((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 1 1 0 0 0 0 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 0 1 1 1 1 0 0 1 1 1 0 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0 0 0) 
				(0 0 0 1 1 0 1 1 1 1 1 1 1 1 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 1 1 1 1 1 1 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 1 1 1 1 1 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 1 1 1 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 1 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *grayed-check-box-pattern*
	      (make-pattern #2a((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 1 0 1 0 1 0 1 0 1 0 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 0 1 0 1 0 1 0 1 0 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 0 1 0 1 0 1 0 1 0 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 0 1 0 1 0 1 0 1 0 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 0 1 0 1 0 1 0 1 0 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 0 1 0 1 0 1 0 1 0 1 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
				(0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *radio-button-off-pattern*
	      (make-pattern #2a((0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0) 
				(0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0) 
				(0 0 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1 0 0 0 0) 
				(0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 1 1 0 0 0 0) 
				(0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0) 
				(0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0) 
				(0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0) 
				(0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0) 
				(0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0) 
				(0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0) 
				(0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 1 1 0 0 0 0) 
				(0 0 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1 0 0 0 0) 
				(0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0) 
				(0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *radio-button-on-pattern*
	      (make-pattern #2a((0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0) 
				(0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0) 
				(0 0 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1 0 0 0 0) 
				(0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 1 1 0 0 0 0) 
				(0 0 0 1 1 1 0 0 0 0 1 0 0 0 0 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 0 1 1 1 1 1 0 0 0 1 1 0 0 0) 
				(0 0 1 1 1 0 0 1 1 1 1 1 1 1 0 0 1 1 1 0 0) 
				(0 0 1 1 0 0 0 1 1 1 1 1 1 1 0 0 0 1 1 0 0) 
				(0 0 1 1 0 0 1 1 1 1 1 1 1 1 1 0 0 1 1 0 0) 
				(0 0 1 1 0 0 0 1 1 1 1 1 1 1 0 0 0 1 1 0 0) 
				(0 0 1 1 1 0 0 1 1 1 1 1 1 1 0 0 1 1 1 0 0) 
				(0 0 0 1 1 0 0 0 1 1 1 1 1 0 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 0 0 0 0 1 0 0 0 0 1 1 1 0 0 0) 
				(0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 1 1 0 0 0 0) 
				(0 0 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1 0 0 0 0) 
				(0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0) 
				(0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0))
			    (list +background-ink+ +foreground-ink+)))

(defparameter *radio-button-grayed-pattern*
	      (make-pattern #2a((0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0) 
				(0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0) 
				(0 0 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1 0 0 0 0) 
				(0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 1 1 0 0 0 0) 
				(0 0 0 1 1 1 0 0 1 0 1 0 1 0 0 1 1 1 0 0 0) 
				(0 0 0 1 1 0 0 1 0 1 0 1 0 1 0 0 1 1 0 0 0) 
				(0 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 0) 
				(0 0 1 1 0 0 0 1 0 1 0 1 0 1 0 0 0 1 1 0 0) 
				(0 0 1 1 0 0 1 0 1 0 1 0 1 0 1 0 0 1 1 0 0) 
				(0 0 1 1 0 0 0 1 0 1 0 1 0 1 0 0 0 1 1 0 0) 
				(0 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 0) 
				(0 0 0 1 1 0 0 1 0 1 0 1 0 1 0 0 1 1 0 0 0) 
				(0 0 0 1 1 1 0 0 1 0 1 0 1 0 0 1 1 1 0 0 0) 
				(0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 1 1 0 0 0 0) 
				(0 0 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1 0 0 0 0) 
				(0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0) 
				(0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0))
			    (list +background-ink+ +foreground-ink+)))


;;; Toggle buttons

;;--- This should only have the 3 pattern slots on, off and unknown (maybe
;;--- also disabled if its not done with a overlay).  I was hoping that we could
;;--- split this into separate classes because that would be the most natural
;;--- implementation - you then just fill the slots with the right patterns
;;--- using the :DEFAULT-INITARGS list.  I guess we can do the sort of thing
;;--- at initialize instance time - but I still favor splitting - its easier
;;--- to add new toggle button classes that way.
(defclass toggle-button-pane (toggle-button button-pane-mixin)
    ((empty-check-box-pattern :initarg :empty-check-box-pattern)
     (checked-check-box-pattern :initarg :checked-check-box-pattern)
     (grayed-check-box-pattern :initarg :grayed-check-box-pattern)
     (radio-button-off-pattern :initarg :radio-button-off-pattern)
     (radio-button-on-pattern :initarg :radio-button-on-pattern)
     (radio-button-grayed-pattern :initarg :radio-button-grayed-pattern))
  (:default-initargs :label nil
		     :indicator-type ':one-of
		     :empty-check-box-pattern *empty-check-box-pattern*
		     :checked-check-box-pattern *checked-check-box-pattern*
		     :grayed-check-box-pattern *grayed-check-box-pattern*
		     :radio-button-off-pattern *radio-button-off-pattern*
		     :radio-button-on-pattern *radio-button-on-pattern*
		     :radio-button-grayed-pattern *radio-button-grayed-pattern*
		     :text-style *default-button-label-text-style*))

(defmethod compose-space ((pane toggle-button-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (width height)
      (compute-gadget-label-size pane)
    ;;--- Should either make radio buttons and check boxes different classes
    ;;--- or generalize this
    (with-slots (empty-check-box-pattern) pane
      (let ((pattern-width (pattern-width empty-check-box-pattern))
	    (pattern-height (pattern-height empty-check-box-pattern)))
	;; We allow 1/2 pattern width on each side as margin
	(make-space-requirement :width (+ width (* pattern-width 2))
				:height (max height (* pattern-height 2)))))))

(defmethod handle-repaint ((pane toggle-button-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    ;;--- This is pretty gross: toggle buttons and check boxes should
    ;;--- be different classes
    (cond ((typep (gadget-client pane) 'radio-box)
	   (draw-radio-button pane medium :draw-label-p t))
	  (t
	   (draw-toggle-button-check-box pane medium :draw-label-p t)))))

;; This is done in the (SETF GADGET-VALUE) method because that's where it 
;; would be done in, say, Motif.  (That is, we'd pass the SET-GADGET-VALUE
;; off to Motif which would toggle the indicator.)
(defmethod (setf gadget-value) :after (value (pane toggle-button-pane) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (port pane)
    ;; If it's not grafted, don't draw it.
    (with-sheet-medium (medium pane)
      ;;--- This is pretty gross: toggle buttons and check boxes should
      ;;--- be different classes
      (cond ((typep (gadget-client pane) 'radio-box)
	     (draw-radio-button pane medium))
	    (t
	     (draw-toggle-button-check-box pane medium))))))

(defmethod draw-toggle-button-check-box ((pane toggle-button-pane) medium
					 &key (draw-label-p nil))
  (with-slots (empty-check-box-pattern checked-check-box-pattern
	       grayed-check-box-pattern) pane
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (declare (ignore right))
      (let* ((pattern-width (pattern-width empty-check-box-pattern))
	     (pattern-height (pattern-height empty-check-box-pattern))
	     ;; assume a margin of 1/2 pattern width
	     (x (+ left (floor pattern-width 2)))
	     (y (- (floor (+ top bottom) 2) (floor pattern-height 2))))
	(case (gadget-value pane)
	  (:unknown (draw-pattern* medium grayed-check-box-pattern x y))
	  ((nil) (draw-pattern* medium empty-check-box-pattern x y))
	  (otherwise (draw-pattern* medium checked-check-box-pattern x y)))
	(when draw-label-p
	  (draw-gadget-label pane medium
			     (+ left (* pattern-width 2)) (floor (+ top bottom) 2)
			     :align-x :left :align-y :center))))))

(defmethod draw-radio-button ((pane toggle-button-pane) medium
			      &key (draw-label-p nil))
  (with-slots (radio-button-off-pattern radio-button-on-pattern
	       radio-button-grayed-pattern) pane
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (declare (ignore right))
      (let* ((pattern-width (pattern-width radio-button-off-pattern))
	     (pattern-height (pattern-height radio-button-off-pattern))
	     ;; assume a margin of 1/2 pattern width
	     (x (+ left (floor pattern-width 2)))
	     (y (- (floor (+ top bottom) 2) (floor pattern-height 2))))
	(case (gadget-value pane)
	  (:unknown (draw-pattern* medium radio-button-grayed-pattern x y))
	  ((nil) (draw-pattern* medium radio-button-off-pattern x y))
	  (otherwise (draw-pattern* medium radio-button-on-pattern x y)))
	(when draw-label-p
	  (draw-gadget-label pane medium
			     (+ left (* pattern-width 2)) (floor (+ top bottom) 2)
			     :align-x :left :align-y :center))))))

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
  (:default-initargs :label nil :label-text-style *default-button-label-text-style*
		     :menu-group nil :show-arrow t))


;;; Radio boxes and check boxes

(defclass radio-box-pane 
	  (radio-box
	   wrapping-space-mixin
	   sheet-permanently-enabled-mixin
	   sheet-mute-input-mixin
	   sheet-multiple-child-mixin
	   space-requirement-mixin
	   basic-pane)
    ())

(defmethod initialize-instance :after ((pane radio-box-pane) 
				       &key choices selection frame-manager frame)
  (declare (ignore choices))
  ;; Set the button's client and disown it from the radio box so
  ;; that we can do layout, below.
  (let ((buttons (copy-list (sheet-children pane))))
    (dolist (button buttons)
      (setf (gadget-client button) pane)
      (sheet-disown-child pane button))
    (let ((inferiors
	    (with-look-and-feel-realization (frame-manager frame)
	      (make-pane (ecase (gadget-orientation pane)
			   (:horizontal 'hbox-pane)
			   (:vertical 'vbox-pane))
		:spacing 5
		:contents buttons))))
      (sheet-adopt-child pane inferiors)))
  (when selection
    (setf (gadget-value pane) selection)
    (setf (gadget-value selection) t)))

(defmethod value-changed-callback :around
	   ((selection toggle-button) (client radio-box-pane) gadget-id value)
  (declare (ignore gadget-id value))
  (let ((old-selection (radio-box-current-selection client)))
    (when (and old-selection
	       (not (eq selection old-selection)))
      (setf (gadget-value old-selection :invoke-callback t) nil)))
  (call-next-method))

(defmethod handle-event :after ((pane radio-box-pane) (event pointer-event))
  (deallocate-event event))


(defclass check-box-pane 
	  (check-box
	   wrapping-space-mixin
	   sheet-permanently-enabled-mixin
	   sheet-mute-input-mixin
	   sheet-multiple-child-mixin
	   space-requirement-mixin
	   basic-pane)
    ())

(defmethod initialize-instance :after ((pane check-box-pane) 
				       &key choices selection frame-manager frame)
  (declare (ignore choices))
  (let ((buttons (copy-list (sheet-children pane))))
    (dolist (button buttons)
      (setf (gadget-client button) pane)
      (sheet-disown-child pane button))
    (let ((inferiors
	    (with-look-and-feel-realization (frame-manager frame)
	      (make-pane (ecase (gadget-orientation pane)
			   (:horizontal 'hbox-pane)
			   (:vertical 'vbox-pane))
		:spacing 5
		:contents buttons))))
      (sheet-adopt-child pane inferiors)))
  (when selection
    (setf (gadget-value pane) (list selection))
    (setf (gadget-value selection) t)))

(defmethod handle-event :after ((pane check-box-pane) (event pointer-event))
  (deallocate-event event))


