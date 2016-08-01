;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(define-graphics-operation draw-point (x y)
  :arguments ((point x y))
  :drawing-options :point
  :method-body
    (with-transformed-arguments
      (draw-point-internal stream (coordinate 0) (coordinate 0) 
			   x y
			   (medium-ink stream) (medium-line-style stream))))

(defun draw-points (stream point-seq &rest args)
  (declare (dynamic-extent args))
  (declare (arglist stream point-seq &key . #.(all-drawing-options-lambda-list :point)))
  (flet ((trampoline ()
	   (if (listp point-seq)
	       (loop
		 (when (null point-seq) (return))
		 (let ((point (pop point-seq)))
		   (draw-point-method stream (point-x point) (point-y point))))
	       (do ((i 0 (+ i 1)))
		   ((= i (length point-seq)))
		 (let ((point (aref point-seq i)))
		   (draw-point-method stream (point-x point) (point-y point)))))))
    (declare (dynamic-extent #'trampoline))
    (if args
	(apply #'invoke-with-drawing-options stream #'trampoline args)
	(trampoline))))

(defun draw-points* (stream coord-seq &rest args)
  (declare (dynamic-extent args))
  (declare (arglist stream coord-seq &key . #.(all-drawing-options-lambda-list :point)))
  (flet ((trampoline ()
	   (if (listp coord-seq)
	       (loop
		 (when (null coord-seq) (return))
		 (draw-point-method stream (pop coord-seq) (pop coord-seq)))
	     (do ((i 0 (+ i 2)))
		 ((= i (length coord-seq)))
	       (draw-point-method stream (aref coord-seq i) (aref coord-seq (1+ i)))))))
    (declare (dynamic-extent #'trampoline))
    (if args
	(apply #'invoke-with-drawing-options stream #'trampoline args)
	(trampoline))))

;; QuickDraw and X support a drawing state with a pattern, thickness at the lowest
;; level.  Only Genera needs to turn lines into rectangles or polygons.
(define-graphics-operation draw-line (x1 y1 x2 y2)
  :arguments ((point x1 y1 x2 y2))
  :drawing-options :line-cap
  :method-body
    (with-transformed-arguments
      (draw-line-internal stream (coordinate 0) (coordinate 0) 
			  x1 y1 x2 y2
			  (medium-ink stream) (medium-line-style stream))))

(defun draw-lines (stream point-seq &rest args)
  (declare (dynamic-extent args))
  (declare (arglist stream point-seq &key . #.(all-drawing-options-lambda-list :line-cap)))
  (flet ((trampoline ()
	   (if (listp point-seq)
	       (loop
		 (when (null point-seq) (return))
		 (let* ((point1 (pop point-seq))
			(point2 (pop point-seq)))
		   (draw-line-method stream
				     (point-x point1) (point-y point1)
				     (point-x point2) (point-y point2))))
	     (do ((i 0 (+ i 2)))
		 ((= i (length point-seq)))
	       (let* ((point1 (aref point-seq i))
		      (point2 (aref point-seq (1+ i))))
		 (draw-line-method stream
				   (point-x point1) (point-y point1)
				   (point-x point2) (point-y point2)))))))
    (declare (dynamic-extent #'trampoline))
    (if args
	(apply #'invoke-with-drawing-options stream #'trampoline args)
	(trampoline))))

(defun draw-lines* (stream coord-seq &rest args)
  (declare (dynamic-extent args))
  (declare (arglist stream coord-seq &key . #.(all-drawing-options-lambda-list :line-cap)))
  (flet ((trampoline ()
	   (if (listp coord-seq)
	       (loop
		 (when (null coord-seq) (return))
		 (draw-line-method stream
				   (pop coord-seq) (pop coord-seq)
				   (pop coord-seq) (pop coord-seq)))
	     (do ((i 0 (+ i 4)))
		 ((= i (length coord-seq)))
	       (draw-line-method stream
				 (aref coord-seq i) (aref coord-seq (1+ i))
				 (aref coord-seq (+ i 2)) (aref coord-seq (+ i 3)))))))
    (declare (dynamic-extent #'trampoline))
    (if args
	(apply #'invoke-with-drawing-options stream #'trampoline args)
	(trampoline))))

(define-graphics-operation draw-arrow (x1 y1 x2 y2
				       &key (from-head nil) (to-head t)
					    (head-length 10) (head-width 5))
  :arguments ((point x1 y1 x2 y2))
  :drawing-options :line-cap
  :method-body
    (with-transformed-arguments
      (let* ((dx (- x2 x1))
	     (dy (- y2 y1))
	     (norm (if (zerop dx)
		       (if (zerop dy) nil (/ 1.0 (abs dy)))
		       (if (zerop dy) (/ 1.0 (abs dx)) (/ (sqrt (+ (* dx dx) (* dy dy))))))))
	(when norm
	  (let* ((length-norm (* head-length norm))
		 (ldx (* dx length-norm))
		 (ldy (* dy length-norm))
		 (base-norm (* head-width norm 0.5))
		 (bdx (* dy base-norm))
		 (bdy (* dx base-norm))
		 (ink (medium-ink stream))
		 (line-style (medium-line-style stream)))
	    (draw-line-internal stream (coordinate 0) (coordinate 0) 
				x1 y1 x2 y2 ink line-style)
	    (when from-head
	      (let ((xa (+ x1 ldx)) (ya (+ y1 ldy)))
		(with-stack-list (points x1 y1 (+ xa bdx) (- ya bdy) (- xa bdx) (+ ya bdy))
		  (draw-polygon-internal stream (coordinate 0) (coordinate 0) 
					 points t ink nil))
		(setq x1 xa y1 ya)))
	    (when to-head
	      (let ((xa (- x2 ldx)) (ya (- y2 ldy)))
		(with-stack-list (points x2 y2 (+ xa bdx) (- ya bdy) (- xa bdx) (+ ya bdy))
		  (draw-polygon-internal stream (coordinate 0) (coordinate 0) 
					 points t ink nil)
		  (setq x2 xa y2 ya)))))))))

;;; More complicated.  It has to decide when to call
;;; draw-rectangle-internal vs. draw-polygon-internal
(define-graphics-operation draw-rectangle (x1 y1 x2 y2 &key (filled t))
  :arguments ((point x1 y1 x2 y2))
  :drawing-options :line-joint
  :method-body
    (let ((transform (medium-transformation stream)))
      (cond ((rectilinear-transformation-p transform)
	     (with-transformed-arguments
	       ;; "optimized" case
	       ;; left, top, right, bottom might not actually be those points
	       ;; if we've rotated by a multiple of pi.
	       (let ((ll (min x1 x2))
		     (tt (min y1 y2))
		     (rr (max x1 x2))
		     (bb (max y1 y2)))
		 (draw-rectangle-internal stream (coordinate 0) (coordinate 0) 
					  ll tt rr bb
					  (medium-ink stream)
					  (and (not filled)
					       (medium-line-style stream))))))
	    (t
	     ;; massively inefficient 
	     ;; --- note that "stream" and is a magic name here.
	     (with-stack-list (list x1 y1 x2 y1 x2 y2 x1 y2)
	       (draw-polygon-method stream list t filled))))))

(defun draw-icon* (stream icon x y &key clipping-region transformation)
  (check-type icon pattern)
  (let ((width (pattern-width icon))
	(height (pattern-height icon)))
    (if (or clipping-region transformation)
	(with-drawing-options (stream :clipping-region clipping-region
				      :transformation transformation))
	(draw-rectangle* stream x y (+ x width) (+ y height)
			 :filled t :ink icon))))

(defvar *float-tolerance* .00001)
(defun float-= (n1 n2)
  (< (abs (- n1 n2)) *float-tolerance*))

(define-graphics-operation draw-ellipse (center-x center-y
					 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
					 &key start-angle end-angle (filled t))
  :arguments ((point center-x center-y)
	      (distance radius-1-dx radius-1-dy radius-2-dx radius-2-dy))
  :drawing-options :line-cap
  :method-body
    (with-transformed-arguments
      (when (and start-angle (null end-angle))
	(setq end-angle 2pi))
      (when (and end-angle (null start-angle))
	(setq start-angle 0))
      (let ((transform (medium-transformation stream))
	    (ink (medium-ink stream))
	    (line-style (and (not filled) (medium-line-style stream))))
	(when start-angle
	  (when (reflection-transformation-p transform)
	    (rotatef start-angle end-angle))
	  (unless (<= 0f0 (abs (- end-angle start-angle)) 2pi)
	    (nyi)))
	(draw-ellipse-internal stream (coordinate 0) (coordinate 0) 
			       center-x center-y 
			       radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			       start-angle end-angle
			       ink line-style))))

(defun draw-circle (stream center radius &rest keys)
  (declare (dynamic-extent keys))
  (declare (arglist stream center radius
		    &key start-angle end-angle (filled t)
			 . #.(all-drawing-options-lambda-list :line-cap)))
  (apply #'draw-ellipse stream center radius 0 0 radius keys))

(defun draw-circle* (stream center-x center-y radius &rest keys)
  (declare (dynamic-extent keys))
  (declare (arglist stream center-x center-y radius
		    &key start-angle end-angle (filled t)
			 . #.(all-drawing-options-lambda-list :line-cap)))
  (apply #'draw-ellipse* stream center-x center-y radius 0 0 radius keys))

(define-graphics-operation draw-oval (center-x center-y x-radius y-radius
				      &key (filled t))
  :arguments ((point center-x center-y)
	      (distance x-radius y-radius))
  :drawing-options :line-cap
  :method-body
    (with-transformed-arguments
      (let ((left (- center-x x-radius))
	    (right (+ center-x x-radius))
	    (top (- center-y y-radius))
	    (bottom (+ center-y y-radius))
	    (ink (medium-ink stream))
	    (line-style (and (not filled) (medium-line-style stream))))
	(cond ((or (= x-radius y-radius) 
		   (zerop x-radius))
	       (draw-ellipse-internal stream (coordinate 0) (coordinate 0)
				      center-x center-y y-radius 0 0 y-radius
				      nil nil ink line-style))
	      ((zerop y-radius)
	       (draw-ellipse-internal stream (coordinate 0) (coordinate 0)
				      center-x center-y x-radius 0 0 x-radius
				      nil nil ink line-style))
	      ((> x-radius y-radius)
	       (let ((rect-left (+ left y-radius))
		     (rect-right (- right y-radius)))
		 (cond (line-style
			(draw-line-internal stream (coordinate 0) (coordinate 0) 
					    rect-left top rect-right top
					    ink line-style)
			(draw-line-internal stream 0 0
					    rect-left bottom rect-right bottom
					    ink line-style))
		       (t
			(draw-rectangle-internal stream (coordinate 0) (coordinate 0)
						 rect-left top rect-right bottom
						 ink line-style)))
		 (let ((north (float (* pi 1/2) 0.0))
		       (south (float (* pi 3/2) 0.0)))
		   (draw-ellipse-internal stream (coordinate 0) (coordinate 0)
					  rect-left center-y y-radius 0 0 y-radius
					  north south ink line-style)
		   (draw-ellipse-internal stream (coordinate 0) (coordinate 0)
					  rect-right center-y y-radius 0 0 y-radius
					  south north ink line-style))))
	      (t
	       (let ((rect-top (+ top x-radius))
		     (rect-bottom (- bottom x-radius)))
		 (cond (line-style
			(draw-line-internal stream (coordinate 0) (coordinate 0) 
					    left rect-top left rect-bottom
					    ink line-style)
			(draw-line-internal stream (coordinate 0) (coordinate 0) 
					    right rect-top right rect-bottom
					    ink line-style))
		       (t
			(draw-rectangle-internal stream (coordinate 0) (coordinate 0)
						 left rect-top right rect-bottom
						 ink line-style)))
		 (let ((east 0.0)
		       (west (float pi 0.0)))
		   (draw-ellipse-internal stream (coordinate 0) (coordinate 0)
					  center-x rect-top x-radius 0 0 x-radius
					  west east ink line-style)
		   (draw-ellipse-internal stream (coordinate 0) (coordinate 0)
					  center-x rect-bottom x-radius 0 0 x-radius
					  east west ink line-style))))))))

(define-graphics-operation draw-polygon (list-of-x-and-ys &key (closed t) (filled t))
  :arguments ((point-sequence list-of-x-and-ys))
  :drawing-options :line-joint-cap
  :method-body
    (with-transformed-arguments
      (draw-polygon-internal stream (coordinate 0) (coordinate 0) 
			     list-of-x-and-ys closed
			     (medium-ink stream)
			     (and (not filled) (medium-line-style stream)))))

(defun draw-regular-polygon* (stream x1 y1 x2 y2 nsides
			      &rest args &key (handedness :left) (closed t) &allow-other-keys)
  (declare (dynamic-extent args))
  (declare (arglist stream x1 y1 x2 y2 nsides
		    &key (filled t) (handedness :left) (closed t)
			 . #.(all-drawing-options-lambda-list :line-joint-cap)))
  (let* ((theta (* (float (* pi (/ 2.0 nsides)) 0.0)
		   (ecase handedness
		     (:left +1)
		     (:right -1))))
	 (transform (make-rotation-transformation theta))
	 (coordinates (list x1 y1 x2 y2))
	 (dx (- x2 x1))
	 (dy (- y2 y1))
	 (next-x x2)
	 (next-y y2))
    (repeat (- nsides 2)
      (multiple-value-setq (dx dy)
	(transform-distance transform dx dy))
      (incf next-x dx)
      (incf next-y dy)
      (setq coordinates (append coordinates (list next-x next-y))))
    (when closed
      (setq coordinates (append coordinates (list x1 y1))))
    (with-keywords-removed (args args '(:handedness))
      (apply #'draw-polygon* stream coordinates args))))

(defun draw-regular-polygon (stream point1 point2 nsides &rest args)
  (declare (dynamic-extent args))
  (declare (arglist stream point1 point2 nsides
		    &key (handedness :left) (closed t) (filled t)
			 . #.(all-drawing-options-lambda-list :line-joint-cap)))
  (apply #'draw-regular-polygon* stream
				 (point-x point1) (point-y point1)
				 (point-x point2) (point-y point2)
				 nsides args))

(defun draw-triangle (stream p1 p2 p3 &rest keys)
  (declare (dynamic-extent keys))
  (declare (arglist stream p1 p2 p3
		    &key (filled t) . #.(all-drawing-options-lambda-list :line-joint)))
  (with-stack-list (points p1 p2 p3)
    (apply #'draw-polygon stream points :closed t keys)))

(defun draw-triangle* (stream x1 y1 x2 y2 x3 y3 &rest keys)
  (declare (dynamic-extent keys))
  (declare (arglist stream x1 y1 x2 y2 x3 y3
		    &key (filled t) . #.(all-drawing-options-lambda-list :line-joint)))
  (with-stack-list (points x1 y1 x2 y2 x3 y3)
    (apply #'draw-polygon* stream points :closed t keys)))

(define-graphics-operation draw-string (string x y
					&key (start 0) end
					     (align-x :left)	 ;:center, :right
					     (align-y :baseline));:top, :center, :bottom
  :arguments ((point x y))
  :drawing-options :text
  :method-body
    (with-transformed-arguments
      (draw-string-internal stream (coordinate 0) (coordinate 0)
			    string x y start end align-x align-y
			    (medium-merged-text-style stream)
			    (medium-ink stream))))

(define-graphics-operation draw-character (character x y
					   &key (align-x :left)	    ;:center, :right
						(align-y :baseline));:top, :center, :bottom  
  :arguments ((point x y))
  :drawing-options :text
  :method-body
    (with-transformed-arguments
      (draw-character-internal stream (coordinate 0) (coordinate 0)
			       character x y align-x align-y
			       (medium-merged-text-style stream)
			       (medium-ink stream))))

(defun draw-text (stream text point &rest args)
  (declare (dynamic-extent args))
  (declare (arglist stream text point
		    &key (start 0) end (align-x :left) (align-y :baseline)
			 . #.(all-drawing-options-lambda-list :text)))
  (if (characterp text)
      (apply #'draw-character stream text point args)
      (apply #'draw-string stream text point args)))

(defun draw-text* (stream text x y &rest args)
  (declare (dynamic-extent args))
  (declare (arglist stream text x y
		    &key (start 0) end (align-x :left) (align-y :baseline)
			 . #.(all-drawing-options-lambda-list :text)))
  (if (characterp text)
      (apply #'draw-character* stream text x y args)
      (apply #'draw-string* stream text x y args)))
