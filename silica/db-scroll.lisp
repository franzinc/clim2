;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: db-scroll.lisp,v 1.38 92/12/07 12:15:15 cer Exp $

"Copyright (c) 1991, 1992 by Franz, Inc.  All rights reserved.
 Portions copyright(c) 1991, 1992 International Lisp Associates.
 Portions copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)


;; The abstract scroller pane class
;;--- Need to be able to specify horizontal and vertical separately
(defclass scroller-pane (foreground-background-and-text-style-mixin pane)
    ((scroll-bars :initarg :scroll-bars
		  :reader scroller-pane-scroll-bar-policy)
     viewport
     (contents :initarg :contents :accessor pane-contents)
     (vertical-scroll-bar :initform nil 
			  :accessor scroller-pane-vertical-scroll-bar)
     (horizontal-scroll-bar :initform nil
			    :accessor scroller-pane-horizontal-scroll-bar)
     (scrolling-supplied-by-gadget :initform nil
				   :accessor scroller-pane-gadget-supplies-scrolling-p))
  (:default-initargs :scroll-bars :both))


;; Returns the viewport of the pane, if there is one
(defmethod pane-viewport ((sheet sheet))
  (let ((parent (sheet-parent sheet)))
    (when parent
      (if (viewportp parent)
	  parent
	  (pane-viewport parent)))))

(defmethod pane-viewport-region ((sheet basic-sheet))
  (let ((viewport (pane-viewport sheet)))
    (and viewport
	 (viewport-viewport-region viewport))))

(defmethod pane-scroller ((sheet basic-sheet))
  (let ((viewport (pane-viewport sheet)))
    (and viewport (viewport-scroller-pane viewport))))

;;--- Use DEFOPERATION
(defmethod pane-viewport ((sheet standard-encapsulating-stream))
  (pane-viewport (encapsulating-stream-stream sheet)))

;;--- Use DEFOPERATION
(defmethod pane-viewport-region ((sheet standard-encapsulating-stream))
  (pane-viewport-region (encapsulating-stream-stream sheet)))

;;--- Use DEFOPERATION
(defmethod pane-scroller ((sheet standard-encapsulating-stream))
  (pane-scroller (encapsulating-stream-stream sheet)))

(defmethod gadget-supplied-scrolling (frame-manager frame contents &rest ignore)
  (declare (ignore frame-manager frame ignore contents))
  nil)

(defvar *inhibit-updating-scroll-bars* nil)
(defmacro inhibit-updating-scroll-bars ((stream) &body body)
  `(unwind-protect
       (let ((*inhibit-updating-scroll-bars* t))
	 ,@body)
     (let ((viewport (pane-viewport ,stream)))
       (when viewport
	 (update-scroll-bars viewport)))))

(defun update-scroll-bars (viewport)
  (unless *inhibit-updating-scroll-bars*
    ;;--- This is not the most efficient thing in the world
    (let ((scroller (viewport-scroller-pane viewport)))
      (multiple-value-bind (changedp
			    hscroll-bar hscroll-bar-enabled-p
			    vscroll-bar vscroll-bar-enabled-p)
	  (compute-dynamic-scroll-bar-values scroller)
	(update-dynamic-scroll-bars
	  scroller changedp
	  hscroll-bar hscroll-bar-enabled-p 
	  vscroll-bar vscroll-bar-enabled-p t))
      (with-bounding-rectangle* (left top right bottom) 
	  (viewport-contents-extent viewport)
	(with-bounding-rectangle* (vleft vtop vright vbottom)
	    (viewport-viewport-region viewport)
	  (let* ((vertical-scroll-bar (scroller-pane-vertical-scroll-bar scroller))
		 (horizontal-scroll-bar (scroller-pane-horizontal-scroll-bar scroller)))
	    (when vertical-scroll-bar
	      (update-scroll-bar vertical-scroll-bar
				 top bottom vtop vbottom))
	    (when horizontal-scroll-bar
	      (update-scroll-bar horizontal-scroll-bar
				 left right vleft vright))))))))

(defmethod note-sheet-grafted :before ((sheet scroll-bar))
  (setf (scroll-bar-current-size sheet) nil))

;;--- In the case where the viewport is bigger than the window this
;;--- code gets things wrong.  Check out the thinkadot demo.  It's
;;--- because (- (--) (- vmin)) is negative.
(defun update-scroll-bar (scroll-bar min max vmin vmax)
  (declare (optimize (safety 0) (speed 3)))
  (let ((current-size (scroll-bar-current-size scroll-bar))
	(current-value (scroll-bar-current-value scroll-bar)))
    ;; Kinda bogus benchmark optimization -- if the scroll-bar was full size
    ;; before, and the viewport is bigger than the extent, don't bother with
    ;; the fancy math.
    (let* ((gmin (float (gadget-min-value scroll-bar) 0s0))
	   (gmax (float (gadget-max-value scroll-bar) 0s0))
	   (range (- gmax gmin)))
      (declare (type single-float range gmin gmax))
      (when (and (and current-size (= (the single-float current-size) range))
		 (> (- vmax vmin) (- max min)))
	(return-from update-scroll-bar))
      ;; The elevator size in 01 units - calculated from the contents
      (let* ((contents-range (float (- max min) 0.0s0))
	     (viewport-range (float (- vmax vmin) 0.0s0))
	     (size (the single-float
		     (* range
			(the single-float
			  (if (= max min)
			      1.0
			      (min 1.0 (the single-float
					 (/ viewport-range contents-range))))))))
	     (pos (the single-float
		    (min 1.0s0 (max 0.0s0
				    (if (<= contents-range viewport-range)
					0.0
					(/ (float (- vmin min) 0.0s0) 
					   ;;--- Uh-oh, the home-grown scroll bars
					   ;;--- seem to have a different contract
					   ;;--- from Motif/OpenLook.  Fix them!
					   #+Allegro (- contents-range viewport-range)
					   #-Allegro contents-range)))))))
	(declare (type single-float pos size))
	(unless (and current-size
		     current-value
		     (= current-size size)
		     (= current-value pos))
	  (setf (scroll-bar-current-value scroll-bar) pos)
	  (setf (scroll-bar-current-size scroll-bar) size)
	  (change-scroll-bar-values scroll-bar 
				    :slider-size size
				    :value pos))))))

(defmethod scroll-bar-value-changed-callback
	   (sheet (client scroller-pane) id value size)
  (with-slots (viewport contents) client
    (let* ((extent (viewport-contents-extent viewport))
	   (region (viewport-viewport-region viewport)))
      (case id
	(:vertical
	  (scroll-extent
	    contents
	    :x (bounding-rectangle-min-x region)
	    :y (+ (bounding-rectangle-min-y extent)
		  (* (max 0 (- (bounding-rectangle-height extent)
			       (bounding-rectangle-height region)))
		     (if (= size (gadget-range sheet))
			 0
			 (/ (- value (gadget-min-value sheet))
			    (- (gadget-range sheet) size)))))))
	(:horizontal
	  (scroll-extent
	    contents
	    :x (+ (bounding-rectangle-min-x extent)
		  (* (max 0 (- (bounding-rectangle-width extent)
			       (bounding-rectangle-width region)))
		     (if (= size (gadget-range sheet))
			 0
			 (/ (- value (gadget-min-value sheet))
			    (- (gadget-range sheet) size)))))
	    :y (bounding-rectangle-min-y region)))))))

(defun update-region (sheet nleft ntop nright nbottom &key no-repaint)
  ;;--- I suspect that we should pass in mins and maxs since this does
  ;;--- assume that the window origin is 0,0 and I think that this
  ;;--- causes the compass menu test to fail since there are graphics at
  ;;--- negative coordinates.
  #---ignore
  (with-bounding-rectangle* (left top right bottom) sheet
    (when (or (< nleft left)
	      (< ntop  top)
	      (> nright  right)
	      (> nbottom bottom))
      ;; It should be safe to modify the sheet's region
      (let ((region (sheet-region sheet)))
	(setf (slot-value region 'left) (min nleft left)
	      (slot-value region 'top)  (min ntop  top)
	      (slot-value region 'right)  (max nright  right)
	      (slot-value region 'bottom) (max nbottom bottom))
	(note-sheet-region-changed sheet))))
  #+++ignore
  (let ((width  (- nright  nleft))
	(height (- nbottom ntop)))
    (when (or (> width  (bounding-rectangle-width  sheet))
	      (> height (bounding-rectangle-height sheet)))
      ;; It should be safe to modify the sheet's region
      (let ((region (sheet-region sheet)))
	(setf (slot-value region 'left) 0
	      (slot-value region 'top)  0
	      (slot-value region 'right)  (max (bounding-rectangle-width  sheet) width)
	      (slot-value region 'bottom) (max (bounding-rectangle-height sheet) height))))))

(defun scroll-extent (sheet &key (x 0) (y 0))
  ;;--- CER says that this really isn't right...
  (fix-coordinates x y)
  (let ((viewport (pane-viewport sheet)))
    (when viewport
      (with-bounding-rectangle* (left top right bottom) 
	  (pane-viewport-region sheet)
	;; Optimize this case, since the rest of this code can be
	;; quite expensive, especially on servers that require COPY-AREA
	;; to be synchronous
 	(unless (and (= x left) (= y top))
	  ;;--- This should actually bash the sheet-transformation
	  (setf (sheet-transformation sheet)
		(make-translation-transformation (- x) (- y)))
	  (bounding-rectangle-set-position (viewport-viewport-region viewport) x y)
	  ;; Must go after bounding-rectangle-set-position
	  (update-scroll-bars viewport)
	  (with-bounding-rectangle* (nleft ntop nright nbottom) 
	      (pane-viewport-region sheet)
	    ;;--- Do we really want to do this here??
	    (update-region sheet nleft ntop nright nbottom)
	    (cond
	      ;; If some of the stuff that was previously on display is still on
	      ;; display, BITBLT it into the proper place and redraw the rest.
	      ((ltrb-overlaps-ltrb-p left top right bottom
				     nleft ntop nright nbottom)
	       ;; Move the old stuff to the new position
	       (window-shift-visible-region sheet 
					    left top right bottom
					    nleft ntop nright nbottom)
	       (let ((rectangles (ltrb-difference nleft ntop nright nbottom
						  left top right bottom)))
		 (with-sheet-medium (medium sheet)
		   (dolist (region rectangles)
		     (with-medium-clipping-region (medium region)
		       (multiple-value-call #'medium-clear-area
					    medium (bounding-rectangle* region))
		       (when (output-recording-stream-p sheet)
			 (replay (stream-output-history sheet) sheet region)))))))
	      ;; Otherwise, just redraw the whole visible viewport
	      ;; adjust for the left and top margins by hand so clear-area doesn't erase
	      ;; the margin components.
	      ((output-recording-stream-p sheet)
	       (let ((region (viewport-viewport-region viewport)))
		 ;;--- We should make the sheet-region bigger at this point.
		 ;;--- Perhaps we do a union of the sheet-region and the viewport.
		 (with-sheet-medium (medium sheet)
		   (multiple-value-call #'medium-clear-area
					medium (bounding-rectangle* region)))
		 (replay (stream-output-history sheet) sheet region)))))
	  (let ((frame (pane-frame sheet)))
	    (when (and (/= left x) (/= top y) frame)
	      (note-viewport-position-changed frame sheet))))))))

(defmethod note-viewport-position-changed (frame pane)
  (declare (ignore frame pane))
  nil)

;;; Given min1, max1 and value1 which is between them it will return
;;; value2 which would be proportionally between min2 and max2.
(defun compute-symmetric-value (min1 max1 value1 min2 max2)
  (declare (values value2))
  (let* ((distance1 (- max1 min1))
	 (fraction1 (if (zerop distance1) 0 (/ (- value1 min1) distance1))))
    (let ((x (+ min2 (* (- max2 min2) fraction1))))
      (if (integerp x) x (float x)))))
