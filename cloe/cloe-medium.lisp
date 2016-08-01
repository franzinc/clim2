;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


(defclass cloe-medium (basic-medium)
    ((background-dc-image)
     (foreground-dc-image)))

(defmethod engraft-medium :after ((medium cloe-medium) port sheet)
  (declare (ignore port))
  (with-slots (background-dc-image foreground-dc-image) medium
    (setf background-dc-image (dc-image-for-ink medium (medium-background medium)))
    (setf foreground-dc-image (dc-image-for-ink medium (medium-foreground medium)))))

(defmethod (setf medium-background) :after (new-background (medium cloe-medium))
  (with-slots (background-dc-image) medium
    (setf background-dc-image (dc-image-for-ink medium new-background))))

(defmethod (setf medium-foreground) :after (new-foreground (medium cloe-medium))
  (with-slots (foreground-dc-image) medium
    (setf foreground-dc-image (dc-image-for-ink medium new-foreground))))

;;;

(defclass cloe-window-medium (cloe-medium)
    ((window :initform nil :reader medium-drawable)))

(defmethod make-medium ((port cloe-port) sheet)
  (make-instance 'cloe-window-medium
    :port port
    :sheet sheet))

(defmethod engraft-medium :after ((medium cloe-window-medium) port sheet)
  (declare (ignore port))
  (with-slots (window) medium
    (unless window
      (setq window (sheet-mirror sheet)))))

(defmethod degraft-medium :after ((medium cloe-window-medium) port sheet)
  (declare (ignore port sheet))
  (with-slots (window) medium
    (setf window nil))
  nil)

(defmethod select-cloe-dc ((medium cloe-window-medium))
  (let* ((sheet (medium-sheet medium))
	 (region (sheet-device-region sheet))
	 (medium-region (medium-clipping-region medium))
	 cleft ctop cright cbottom
	 (valid nil))
    (unless (eq region +nowhere+)
      (multiple-value-setq (cleft ctop cright cbottom) (bounding-rectangle* region))
      (setf valid t)
      (unless (eq medium-region +everywhere+)
	(with-bounding-rectangle* (mleft mtop mright mbottom) medium-region
	  (multiple-value-setq (valid cleft ctop cright cbottom)
	    (multiple-value-call #'ltrb-overlaps-ltrb-p
				 cleft ctop cright cbottom
				 (transform-rectangle* 
				   (sheet-device-transformation sheet)
				   mleft mtop mright mbottom))))))
    (when valid
      (fix-coordinates cleft ctop cright cbottom)
      (with-slots (window) medium
	(select-window window))
      (select-clip-rectangle cleft ctop cright cbottom))
    valid))



(defmethod dc-image-for-ink (medium (ink (eql +foreground-ink+)))
  (slot-value medium 'foreground-dc-image))

(defmethod dc-image-for-ink (medium (ink (eql +background-ink+)))
  (slot-value medium 'background-dc-image))

(defmethod dc-image-for-ink (medium (ink (eql +black+)))
  (declare (ignore medium))
  *black-image*)

(defmethod dc-image-for-ink (medium (ink (eql +white+)))
  (declare (ignore medium))
  *white-image*)

(defmethod dc-image-for-ink (medium (ink color))
  (declare (ignore medium))
  (multiple-value-bind (red green blue)
      (color-rgb ink)
    (let ((color (dpb (round (* 255 blue)) (byte 8 16)
		      (dpb (round (* 255 green)) (byte 8 8)
			   (round (* 255 red))))))
      (declare (fixnum color))
      (case color
	((#x000000) *black-image*)
	((#xffffff) *white-image*)
	(otherwise
	  (or (gethash color *color-to-image*)
	      (setf (gethash color *color-to-image*)
		    (make-dc-image :solid-1-pen (win::create-pen win::ps_solid 1 color)
				   :brush (win::create-solid-brush color)
				   :text-color color :background-color nil))))))))

(defmethod dc-image-for-ink (medium (ink pattern))
  (or (gethash ink *ink-to-image*)
      (setf (gethash ink *ink-to-image*)
	    ;; This is broken, but it's hard to do right.
	    (multiple-value-bind (array designs)
		(decode-pattern ink)
	      (unless (and (= (length designs) 2)
			   (eq (aref designs 0) +background-ink+)
			   (eq (aref designs 1) +foreground-ink+))
		(error "Can't handle this pattern."))
	      (let ((into (make-array 8 :element-type '(unsigned-byte 16) :initial-element 0))
		    (dc-image (copy-dc-image (slot-value medium 'foreground-dc-image)))
		    (width (array-dimension array 1))
		    (height (array-dimension array 0)))
		(macrolet ((collect-byte (row-no)
			     `(let ((result 0))
				(dotimes (j (min 8 width))
				  (setf (ldb (byte 1 (- 7 j)) result)
					(aref array ,row-no j)))
				(logxor result #2r11111111))))
		  (dotimes (i (min 8 height))
		    (setf (aref into i) (collect-byte i))))
		(let ((bitmap (win::create-bitmap into)))
		  (setf (dc-image-bitmap dc-image) bitmap)
		  (setf (dc-image-brush dc-image) (win::create-pattern-brush bitmap))
		  (setf (dc-image-background-color dc-image)
			(dc-image-text-color (slot-value medium 'background-dc-image))))
		dc-image)))))

(defmethod dc-image-for-ink (medium (ink rectangular-tile))
  ;; The only case we handle right now is stipples
  (or (gethash ink *ink-to-image*)
      (setf (gethash ink *ink-to-image*)
	    (multiple-value-bind (array width height)
		(decode-tile-as-stipple ink)
	      (unless array
		(error "Rectangular tiles other than stipples are not supported yet."))
	      (let ((into (make-array 8 :element-type '(unsigned-byte 16)))
		    (dc-image (copy-dc-image (slot-value medium 'foreground-dc-image))))
		(macrolet ((collect-byte (row-no)
			     `(let ((result 0))
				(dotimes (j 8)
				  (setq result
					(dpb (aref array (mod ,row-no width) (mod j height))
					     (byte 1 (- 7 j)) result)))
				(logxor result #2r11111111))))
		  (dotimes (i 8)
		    (setf (aref into i) (collect-byte i))))
		(let ((bitmap (win::create-bitmap into)))
		  (setf (dc-image-bitmap dc-image) bitmap)
		  (setf (dc-image-brush dc-image) (win::create-pattern-brush bitmap))
		  (setf (dc-image-background-color dc-image)
			(dc-image-text-color (slot-value medium 'background-dc-image))))
		dc-image)))))

(defmethod dc-image-for-ink (medium (ink flipping-ink))
  (declare (ignore medium))
  (or (gethash ink *ink-to-image*)
      (setf (gethash ink *ink-to-image*)
	    (multiple-value-bind (ink1 ink2)
		(decode-flipping-ink ink)
	      (let* ((image1 (dc-image-for-ink medium ink1))
		     (image2 (dc-image-for-ink medium ink2))
		     (color (logxor (dc-image-text-color image1)
				    (dc-image-text-color image2))))
		(unless (and (eql (dc-image-rop2 image1) win::r2_copypen)
			     (eql (dc-image-rop2 image2) win::r2_copypen)
			     (null (dc-image-bitmap image1))
			     (null (dc-image-bitmap image2)))
		  (nyi))
		(make-dc-image :solid-1-pen (win::create-pen win::ps_solid 1 color)
			       :brush (win::create-solid-brush color)
			       :rop2 win::r2_xorpen
			       :text-color color :background-color nil))))))

(defmethod dc-image-for-ink (medium (ink contrasting-ink))
  (dc-image-for-ink medium (make-color-for-contrasting-ink ink)))


(defmethod medium-draw-point* ((medium cloe-medium) x y)
  (when (select-cloe-dc medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (medium-line-style medium)))
      (convert-to-device-coordinates transform x y)
      (set-dc-for-ink medium ink line-style)
      (win::rectangle *dc* x y x y))))

(defmethod medium-draw-points* ((medium cloe-medium) position-seq)
  (nyi))

(defmethod medium-draw-line* ((medium cloe-medium) x1 y1 x2 y2)
  (when (select-cloe-dc medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (medium-line-style medium)))
      (convert-to-device-coordinates transform x1 y1 x2 y2)
      (set-dc-for-ink medium ink line-style)
      (win::move-to *dc* x1 y2)
      (win::line-to *dc* x2 y2))))

(defmethod medium-draw-lines* ((medium cloe-medium) position-seq)
  (nyi))

(defmethod medium-draw-rectangle* ((medium cloe-medium)
				   left top right bottom filled)
  (when (select-cloe-dc medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (and (not filled) (medium-line-style medium))))
      (convert-to-device-coordinates transform
	left top right bottom)
      (when (< right left) (rotatef right left))
      (when (< bottom top) (rotatef bottom top))
      (set-dc-for-ink medium ink line-style)
      (win::rectangle *dc* left top right bottom))))

(defmethod medium-draw-rectangles* ((medium cloe-medium) position-seq filled)
  (nyi))

(defmethod medium-draw-polygon* ((medium cloe-medium) position-seq closed filled)
  (when (select-cloe-dc medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (and (not filled) (medium-line-style medium)))
	   (minx most-positive-fixnum)
	   (miny most-positive-fixnum)
	   (length (length position-seq)))
      ;; These really are fixnums, since we're fixing coordinates below
      (declare (type fixnum minx miny))
      (with-stack-array (points (if (and closed line-style) (+ length 2) length))
	(declare (type simple-vector points))
	(replace points position-seq)		;set up the initial contents
	(do ((i 0 (+ i 2)))
	    ((>= i length))
	  (let ((x (svref points i))
		(y (svref points (1+ i))))
	    (convert-to-device-coordinates transform x y)
	    (setf (svref points i) x)
	    (setf (svref points (1+ i)) y)
	    (minf minx x)
	    (minf miny y)))
	(when (and closed line-style)		;kludge
	  (setf (svref points length) (svref points 0))
	  (setf (svref points (+ length 1)) (svref points 1)))
	(set-dc-for-ink medium ink line-style)
	(if (null line-style)
	    (win::polygon *dc* (coerce points 'list))
	    (win::polyline *dc* (coerce points 'list) closed))))))

(defconstant *ft* 0.0001)
(defun-inline fl-= (x y) (< (abs (- x y)) *ft*))

(defmethod medium-draw-ellipse* ((medium cloe-medium)
				 center-x center-y
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (when (select-cloe-dc medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (and (not filled) (medium-line-style medium))))
      (convert-to-device-coordinates transform center-x center-y)
      (convert-to-device-distances transform 
	radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
      (when (null start-angle)
	(setq start-angle 0.0
	      end-angle 2pi))
      (set-dc-for-ink medium ink line-style)
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
		       ;; Degrade to drawing a rectilinear ellipse
		       (values (truncate (sqrt s-1)) 
			       (truncate (sqrt s-2)))))))
	(let (left top right bottom)
	  (setq left (- center-x x-radius)
		right (+ center-x x-radius)
		top (- center-y y-radius)
		bottom (+ center-y y-radius))
	  (cond ((or (fl-= (- end-angle start-angle) 2pi)
		     (fl-= (- end-angle start-angle) 0))
		 #+++ignore
		 (and (= start-angle 0)
		      (= end-angle 2pi))
		 ;; drawing a full ellipse
		 (win::ellipse *dc* left top right bottom))
		((null line-style)
		 ;; drawing a pie slice
		 (win::pie *dc* left top right bottom
			   (round (+ (* (cos end-angle) x-radius) center-x))
			   (round (+ (* (sin end-angle) y-radius) center-y))
			   (round (+ (* (cos start-angle) x-radius) center-x))
			   (round (+ (* (sin start-angle) y-radius) center-y))))
		(t
		 ;; drawing an arc
		 (win::arc *dc* left top right bottom
			   (round (+ (* (cos end-angle) x-radius) center-x))
			   (round (+ (* (sin end-angle) y-radius) center-y))
			   (round (+ (* (cos start-angle) x-radius) center-x))
			   (round (+ (* (sin start-angle) y-radius) center-y))))))))))

(defmethod medium-draw-string* ((medium cloe-medium)
				string x y start end align-x align-y
				towards-x towards-y transform-glyphs)
  (when (select-cloe-dc medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (text-style (medium-merged-text-style medium)))
      (convert-to-device-coordinates transform x y)
      (when towards-x
	(convert-to-device-coordinates transform towards-x towards-y))
      (unless end
	(setq end (length string)))
      (with-temporary-substring (substring string start end)
	(let* ((font (text-style-mapping (port medium) text-style))
	       (height (cloe-font-height font))
	       (descent (cloe-font-descent font))
	       (ascent (cloe-font-ascent font)))
	  (let ((x-adjust 
		  (compute-text-x-adjustment align-x medium string text-style start end))
		(y-adjust
		  (compute-text-y-adjustment align-y descent ascent height)))
	    (incf x x-adjust)
	    (incf y y-adjust)
	    (when towards-x
	      (incf towards-x x-adjust)
	      (incf towards-y y-adjust)))
	  (decf y ascent)			;text is positioned by its top left on CLOE
	  (set-dc-for-text medium ink (cloe-font-index font))
	  (win::text-out *dc* x y substring))))))

(defmethod medium-draw-character* ((medium cloe-medium)
				   character x y align-x align-y
				   towards-x towards-y transform-glyphs)
  (when (select-cloe-dc medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (text-style (medium-merged-text-style medium)))
      (convert-to-device-coordinates transform x y)
      (when towards-x
	(convert-to-device-coordinates transform towards-x towards-y))
      (let* ((font (text-style-mapping (port medium) text-style))
	     (height (cloe-font-height font))
	     (descent (cloe-font-descent font))
	     (ascent (cloe-font-ascent font)))
	(let ((x-adjust 
		(compute-text-x-adjustment align-x medium character text-style))
	      (y-adjust
		(compute-text-y-adjustment align-y descent ascent height)))
	  (incf x x-adjust)
	  (incf y y-adjust)
	  (when towards-x
	    (incf towards-x x-adjust)
	    (incf towards-y y-adjust)))
	(decf y ascent)				;text is positioned by its top left on CLOE
	(set-dc-for-text medium ink (cloe-font-index font))
	(with-temporary-string (string :length 1)
	  (vector-push character string)
	  (win::text-out *dc* x y string))))))

(defmethod medium-draw-text* ((medium cloe-medium)
			      string-or-char x y start end
			      align-x align-y
			      towards-x towards-y transform-glyphs)
  (if (characterp string-or-char)
      (medium-draw-character* medium string-or-char x y align-x align-y
			      towards-x towards-y transform-glyphs)
      (medium-draw-string* medium string-or-char x y start end align-x align-y
			   towards-x towards-y transform-glyphs)))

(defmethod medium-clear-area ((medium cloe-medium) left top right bottom)
  (when (select-cloe-dc medium)
    (with-slots (background-dc-image) medium
      (let* ((sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(convert-to-device-coordinates transform
	  left top right bottom)
	(when (< right left) (rotatef right left))
	(when (< bottom top) (rotatef bottom top))
	(set-dc-for-filling background-dc-image)
	(win::rectangle *dc* left top right bottom)))))



(defmethod text-style-width ((text-style standard-text-style) (medium cloe-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (cloe-font-average-character-width font)))

(defmethod text-style-height ((text-style standard-text-style) (medium cloe-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (cloe-font-height font)))

(defmethod text-style-ascent ((text-style standard-text-style) (medium cloe-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (cloe-font-ascent font)))

(defmethod text-style-descent ((text-style standard-text-style) (medium cloe-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (cloe-font-descent font)))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (medium cloe-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    ;; Really disgusting, but probably OK
    (= (cloe-font-average-character-width font)
       (cloe-font-maximum-character-width font))))



(defmethod medium-beep ((medium cloe-medium))
  (win::message-beep))

(defmethod medium-force-output ((medium cloe-medium))
  )

(defmethod medium-clear-output ((medium cloe-medium))
  )

(defmethod medium-finish-output ((medium cloe-medium))
  )
