;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: db-scroll.lisp,v 1.49 1993/10/25 16:16:05 cer Exp $

"Copyright (c) 1991, 1992 by Franz, Inc.  All rights reserved.
 Portions copyright(c) 1991, 1992 International Lisp Associates.
 Portions copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)


;; The abstract scroller pane class
;;--- Need to be able to specify horizontal and vertical separately
(defclass scroller-pane (sheet-with-resources-mixin
			 client-overridability-mixin
			 pane)
    ((scroll-bars :initarg :scroll-bars
		  :reader scroller-pane-scroll-bar-policy)
     viewport
     (contents :initarg :contents :accessor pane-contents)
     (vertical-scroll-bar :initform nil 
			  :accessor scroller-pane-vertical-scroll-bar)
     (horizontal-scroll-bar :initform nil
			    :accessor scroller-pane-horizontal-scroll-bar)
     (scrolling-supplied-by-gadget :initform nil
				   :accessor
				   scroller-pane-gadget-supplies-scrolling-p)
     (vertical-line-scroll-amount :initform nil
				  :initarg
				  :vertical-line-scroll-amount)
     (horizontal-line-scroll-amount :initform nil 
				    :initarg :horizontal-line-scroll-amount))
  (:default-initargs :scroll-bars :both))


;; Returns the viewport of the pane, if there is one
(defmethod pane-viewport ((sheet sheet))
  (let ((parent (sheet-parent sheet)))
    (when parent
      (and (viewportp parent)
	   parent
	   ;;--- should this really recurse?
	   #+ignore
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
      #+ignore
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
	  (minf left vleft)
	  (minf top vtop)
	  (maxf right vright)
	  (maxf bottom vbottom)
	  (let* ((vertical-scroll-bar (scroller-pane-vertical-scroll-bar scroller))
		 (horizontal-scroll-bar (scroller-pane-horizontal-scroll-bar scroller)))
	    (when vertical-scroll-bar
	      (update-scroll-bar vertical-scroll-bar
				 top bottom 
				 vtop vbottom
				 :vertical))
	    (when horizontal-scroll-bar
	      (update-scroll-bar horizontal-scroll-bar
				 left right 
				 vleft vright
				 :horizontal))))))))

(defmethod note-sheet-grafted :before ((sheet scroll-bar))
  (setf (scroll-bar-current-size sheet) nil))

;;--- In the case where the viewport is bigger than the window this
;;--- code gets things wrong.  Check out the thinkadot demo.  It's
;;--- because (- (--) (- vmin)) is negative.
(defun update-scroll-bar (scroll-bar min max vmin vmax orientation)
  (declare (optimize (safety 0) (speed 3)))
  ;;-- Is this really the right thing to do?
  ;;-- If in an interactor some draws at -ve coordinates but the
  ;;window is large enough no one changes the viewport but we cannot scroll-either
  (maxf max vmax)
  (minf min vmin)
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
	     ;;-- This does not scale by the range
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
	  ;;-- It would be nice if we could do this at the point of scrolling
	  (let* ((line-scroll (line-scroll-amount (slot-value scroll-bar 'client) orientation nil))
		 (line-scroll (if (zerop contents-range)
				  0 ;-- Who knows
				(* range (/ line-scroll contents-range)))))
	    (change-scroll-bar-values scroll-bar 
				      :slider-size size
				      :value pos
				      :line-increment line-scroll)))))))

(defmethod scroll-bar-value-changed-callback
	   (sheet (client scroller-pane) id value size)
  (with-slots (viewport contents) client
    (let* ((extent (viewport-contents-extent viewport))
	   (region (viewport-viewport-region viewport)))
      (case id
	(:vertical
	  (scroll-extent
	    contents
	     (bounding-rectangle-min-x region)
	     (+ (bounding-rectangle-min-y extent)
		  (* (max 0 (- (bounding-rectangle-height extent)
			       (bounding-rectangle-height region)))
		     (if (= size (gadget-range sheet))
			 0
			 (/ (- value (gadget-min-value sheet))
			    (gadget-range sheet)))))))
	(:horizontal
	  (scroll-extent
	    contents
	    (+ (bounding-rectangle-min-x extent)
		  (* (max 0 (- (bounding-rectangle-width extent)
			       (bounding-rectangle-width region)))
		     (if (= size (gadget-range sheet))
			 0
			 (/ (- value (gadget-min-value sheet))
			    (gadget-range sheet)))))
	    (bounding-rectangle-min-y region))))
      ;;-- Yuck
      (clim-internals::maybe-redraw-input-editor-stream contents (pane-viewport-region contents)))))

(defmethod update-region ((sheet basic-sheet) nleft ntop nright nbottom &key no-repaint)
  (declare (ignore nleft ntop nright nbottom no-repaint))
  nil)

(defun scroll-extent (sheet x y)
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
	  (with-bounding-rectangle* (nleft ntop nright nbottom) 
	      (pane-viewport-region sheet)
	    ;; If we are scrolling programatically then this might
	    ;; reveal more of the sheet than currently exists
	    (update-region sheet nleft ntop nright nbottom)
	    ;; Must go after bounding-rectangle-set-position
	    (update-scroll-bars viewport)
	    (if (ltrb-overlaps-ltrb-p left top right bottom nleft ntop nright nbottom)
		(progn
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
			  (if (output-recording-stream-p sheet)
			      (replay (stream-output-history sheet) sheet region)
			    (repaint-sheet sheet region)))))))
	      (let ((region (viewport-viewport-region viewport)))
		;;--- We should make the sheet-region bigger at this point.
		;;--- Perhaps we do a union of the sheet-region and the viewport.
		(with-sheet-medium (medium sheet)
		  (multiple-value-call #'medium-clear-area
		    medium (bounding-rectangle* region)))
		(if (output-recording-stream-p sheet)
		    (replay (stream-output-history sheet) sheet
			    region)
		  (repaint-sheet sheet region)))))
	  (let ((frame (pane-frame sheet)))
	    (when frame
	      (note-viewport-position-changed frame sheet x y))))))))

(defmethod note-viewport-position-changed (frame pane x y)
  (declare (ignore frame pane x y))
  nil)

;;; Given min1, max1 and value1 which is between them it will return
;;; value2 which would be proportionally between min2 and max2.
(defun compute-symmetric-value (min1 max1 value1 min2 max2)
  (declare (values value2))
  (let* ((distance1 (- max1 min1))
	 (fraction1 (if (zerop distance1) 0 (/ (- value1 min1) distance1))))
    (let ((x (+ min2 (* (- max2 min2) fraction1))))
      (if (integerp x) x (float x)))))

(defmethod line-scroll-amount ((pane scroller-pane) orientation direction)
  ;; --- These are the stubs for user specified line scroll amounts
  ;; req by graphic panes. I haven't enabled them because I want to
  ;; check with SWM for the MAKE-PANE protocol for these. Davo 6/30/92
  ;; Is this right?? - cer
  (or (with-slots (vertical-line-scroll-amount
		   horizontal-line-scroll-amount) pane
	(ecase orientation
	  (:horizontal horizontal-line-scroll-amount)
	  (:vertical vertical-line-scroll-amount)))
      (scrolled-pane-line-scroll-amount
       (pane-contents pane)
       orientation
       direction)))


(defmethod scrolled-pane-line-scroll-amount ((pane sheet)
					     orientation
					     direction)
  (declare (ignore direction))
  (let ((r (pane-viewport-region pane)))
    (/ (ecase orientation
	 (:vertical (bounding-rectangle-height r))
	 (:horizontal (bounding-rectangle-width r)))
       10)))
  

