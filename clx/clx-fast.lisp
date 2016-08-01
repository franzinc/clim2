;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clx-clim)

;;;"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;;; Fast drawing function constructors

(defmethod medium-make-fast-drawing-function 
	   ((medium clx-medium) (eql function 'medium-draw-point*))
  (let* ((sheet (medium-sheet medium))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (drawable (medium-drawable medium))
	 (gcontext (clx-adjust-ink (clx-decode-ink ink medium) medium line-style 0 0))
	 (thickness (round (line-style-thickness line-style))))
    (if (< thickness 2)
	#'(lambda (x y)
	    (convert-to-device-coordinates (sheet-device-transformation sheet) x y)
	    (xlib:draw-point drawable gcontext x y))
        #'(lambda (x y)
	    (convert-to-device-coordinates (sheet-device-transformation sheet) x y)
	    (xlib:draw-arc drawable gcontext
			   x y thickness thickness 0 2pi t)))))

(defmethod medium-make-fast-drawing-function 
	   ((medium clx-medium) (eql function 'medium-draw-line*))
  (let* ((sheet (medium-sheet medium))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (drawable (medium-drawable medium))
	 (gcontext (clx-adjust-ink (clx-decode-ink ink medium) medium line-style 0 0)))
    #'(lambda (x1 y1 x2 y2)
	(convert-to-device-coordinates (sheet-device-transformation sheet) x1 y1 x2 y2)
	(xlib:draw-line drawable gcontext x1 y1 x2 y2))))

(defmethod medium-make-fast-drawing-function 
	   ((medium clx-medium) (eql function 'medium-draw-rectangle*))
  (let* ((sheet (medium-sheet medium))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (drawable (medium-drawable medium))
	 (gcontext (clx-adjust-ink (clx-decode-ink ink medium) medium line-style 0 0)))
    #'(lambda (left top right bottom filled)
	(convert-to-device-coordinates (sheet-device-transformation sheet)
	  left top right bottom)
	(when (< right left) (rotatef right left))
	(when (< bottom top) (rotatef bottom top))
	(xlib:draw-rectangle drawable gcontext
			     left top (- right left) (- bottom top)
			     filled))))

(defmethod medium-make-fast-drawing-function 
	   ((medium clx-medium) (eql function 'medium-draw-polygon*))
  (let* ((sheet (medium-sheet medium))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (drawable (medium-drawable medium))
	 (gcontext (clx-adjust-ink (clx-decode-ink ink medium) medium line-style 0 0)))
    #'(lambda (position-seq closed filled)
	(let ((transform (sheet-device-transformation sheet)))
	  (with-stack-array (points (if (and closed line-style) (+ length 2) length))
	    (declare (type simple-vector points))
	    (replace points position-seq)
	    (do ((i 0 (+ i 2)))
		((>= i length))
	      (let ((x (svref points i))
		    (y (svref points (1+ i))))
		(convert-to-device-coordinates transform x y)
		(setf (svref points i) x)
		(setf (svref points (1+ i)) y)))
	    (when (and closed line-style)
	      (setf (svref points length) (svref points 0))
	      (setf (svref points (+ length 1)) (svref points 1)))
	    (xlib:draw-lines drawable gcontext
			     points :fill-p filled))))))

(defmethod medium-make-fast-drawing-function 
	   ((medium clx-medium) (eql function 'medium-draw-ellipse*))
  (let* ((sheet (medium-sheet medium))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (drawable (medium-drawable medium))
	 (gcontext (clx-adjust-ink (clx-decode-ink ink medium) medium line-style 0 0)))
    #'(lambda (center-x center-y
	       radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	       start-angle end-angle filled)
	(let ((transform (sheet-device-transformation sheet)))
	  (convert-to-device-coordinates transform center-x center-y)
	  (convert-to-device-distances transform 
	    radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
	  (when (null start-angle)
	    (setq start-angle 0.0
		  end-angle 2pi))
	  (setq start-angle (- 2pi start-angle)
		end-angle (- 2pi end-angle))
	  (rotatef start-angle end-angle)
	  (when (< end-angle start-angle)
	    (setq end-angle (+ end-angle 2pi)))
	  (multiple-value-bind (x-radius y-radius)
	      (cond ((and (= radius-1-dx 0) (= radius-2-dy 0))
		     (values (abs radius-2-dx) (abs radius-1-dy)))
		    ((and (= radius-2-dx 0) (= radius-1-dy 0))
		     (values (abs radius-1-dx) (abs radius-2-dy)))
		    (t
		     (let ((s-1 (+ (* radius-1-dx radius-1-dx) (* radius-1-dy radius-1-dy)))
			   (s-2 (+ (* radius-2-dx radius-2-dx) (* radius-2-dy radius-2-dy))))
		       (if (= s-1 s-2)
			   (let ((r (truncate (sqrt s-1))))
			     (values r r))
			 (values (truncate (sqrt s-1)) 
				 (truncate (sqrt s-2)))))))
	    (let ((angle-size (- end-angle start-angle)))
	      (xlib:draw-arc drawable gcontext
			     (- center-x x-radius) (- center-y y-radius)
			     (* x-radius 2) (* y-radius 2)
			     start-angle angle-size filled)))))))

(defmethod medium-make-fast-drawing-function 
	   ((medium clx-medium) (eql function 'medium-draw-string*))
  (let* ((sheet (medium-sheet medium))
	 (ink (medium-ink medium))
	 (text-style (medium-merged-text-style medium))
	 (drawable (medium-drawable medium))
	 (gcontext (clx-decode-ink ink medium))
	 (font (if text-style
		   (text-style-mapping (port medium) text-style)
		   (clx-get-default-font (port medium) medium)))
	 (ascent (xlib:font-ascent font))
	 (descent (xlib:font-descent font))
	 (height (+ ascent descent)))
    (setf (xlib:gcontext-font gc) font)
    #'(lambda (string x y start end align-x align-y
	       towards-x towards-y transform-glyphs)
	(let ((transform (sheet-device-transformation sheet)))
	  (convert-to-device-coordinates transform x y)
	  (when towards-x
	    (convert-to-device-coordinates transform towards-x towards-y))
	  (unless end
	    (setq end (length string)))
	  (let ((x-adjust 
		 (compute-text-x-adjustment align-x medium string text-style start end))
		(y-adjust
		 (compute-text-y-adjustment align-y descent ascent height)))
	    (incf x x-adjust)
	    (incf y y-adjust)
	    (when towards-x
	      (incf towards-x x-adjust)
	      (incf towards-y y-adjust)))
	  (xlib:draw-glyphs drawable gcontext
			    x y string :start start :end end)))))

(defmethod medium-make-fast-drawing-function 
	   ((medium clx-medium) (eql function 'medium-draw-character*))
  (let* ((sheet (medium-sheet medium))
	 (ink (medium-ink medium))
	 (text-style (medium-merged-text-style medium))
	 (drawable (medium-drawable medium))
	 (gcontext (clx-decode-ink ink medium))
	 (font (if text-style
		   (text-style-mapping (port medium) text-style)
		   (clx-get-default-font (port medium) medium)))
	 (ascent (xlib:font-ascent font))
	 (descent (xlib:font-descent font))
	 (height (+ ascent descent)))
    (setf (xlib:gcontext-font gc) font)
    #'(lambda (character x y align-x align-y
	       towards-x towards-y transform-glyphs)
	(let ((transform (sheet-device-transformation sheet)))
	  (convert-to-device-coordinates transform x y)
	  (when towards-x
	    (convert-to-device-coordinates transform towards-x towards-y))
	  (let ((x-adjust
		 (compute-text-x-adjustment align-x medium character text-style))
		(y-adjust 
		 (compute-text-y-adjustment align-y descent ascent height)))
	    (incf x x-adjust)
	    (incf y y-adjust)
	    (when towards-x
	      (incf towards-x x-adjust)
	      (incf towards-y y-adjust)))
	  (xlib:draw-glyph drawable gcontext x y character)))))
