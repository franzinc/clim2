;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: clx-medium.lisp,v 1.5 92/04/15 11:45:53 cer Exp $

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defclass clx-medium (medium)
  ((drawable :initform nil)
   (color-p :initform nil)			;---
   (foreground-gcontext :initform nil)
   (foreground-pixel :initform nil)
   (background-gcontext :initform nil)
   (background-pixel :initform nil)
   (flipping-gcontext :initform nil)
   (copy-gcontext :initform nil)
   (ink-table :initform (make-hash-table :test #'equal))))

(defmethod make-medium ((port clx-port) sheet)
  (make-instance 'clx-medium
		 :port port
		 :sheet sheet))

(defmethod deallocate-medium :after (port (medium clx-medium))
  (declare (ignore port))
  (with-slots (drawable) medium
    (setf drawable nil)))

(defmethod engraft-medium :after ((medium clx-medium) (port clx-port) sheet)
  (with-slots (drawable) medium
    (unless drawable
      (setq drawable (sheet-mirror sheet)))
    (recompute-gcs medium)))

(defmethod degraft-medium :after ((medium clx-medium) (port clx-port) sheet)
  (declare (ignore sheet))
  (with-slots (foreground-gcontext background-gcontext flipping-gcontext) medium
    (macrolet ((release-gc (gc)
		 `(when ,gc
		    (xlib:free-gcontext ,gc)
		    (setf ,gc nil))))
      (release-gc foreground-gcontext)
      (release-gc background-gcontext)
      (release-gc flipping-gcontext))))

(defmethod recompute-gcs ((medium clx-medium))
  (with-slots (foreground-gcontext foreground-pixel background-gcontext background-pixel
	       flipping-gcontext copy-gcontext drawable) medium
    (unless foreground-gcontext
      (setf foreground-gcontext (xlib:create-gcontext :drawable drawable)))
    (unless background-gcontext
      (setf background-gcontext (xlib:create-gcontext :drawable drawable)))
    (unless flipping-gcontext
      (setf flipping-gcontext (xlib:create-gcontext :drawable drawable
					      :function boole-xor)))
    (flet ((do-ground (ink gc)
	     (setf (xlib:gcontext-fill-style gc) :solid)
	     (setf (xlib:gcontext-foreground gc) (clx-decode-color medium ink))))
      (setf foreground-pixel
	    (do-ground (medium-foreground medium) foreground-gcontext))
      (setf background-pixel
	    (do-ground (medium-background medium) background-gcontext)))
    (cond ((and (integerp foreground-pixel) (integerp background-pixel))
	   (setf (xlib:gcontext-background foreground-gcontext) background-pixel)
	   (setf (xlib:gcontext-fill-style flipping-gcontext) :solid)
	   (setf (xlib:gcontext-foreground flipping-gcontext)
		 (logxor foreground-pixel background-pixel)))
	  (t (nyi)))
    (setf (xlib:window-background drawable) background-pixel)
    (unless copy-gcontext
      (setq copy-gcontext (xlib:create-gcontext :drawable drawable
						:exposures :off)))))
      
(defmethod (setf medium-background) :after (ink (medium clx-medium))
  (declare (ignore ink))
  (recompute-gcs medium))

(defmethod (setf medium-foreground) :after (ink (medium clx-medium))
  (declare (ignore ink))
  (recompute-gcs medium))


;;; Translation between CLIM and CLX graphics model

(defun make-stipple-image (height width patterns)
  (xlib:create-image :width width :height height
		     :data (clim-internals::make-stipple-array height width patterns)
		     :bits-per-pixel 1))

(defvar *clx-luminance-stipples*
	(mapcar #'(lambda (entry)
		    (cons (first entry) (apply #'make-stipple-image (second entry))))
		'((0.1 (8 16 (#b0111111111111111
			      #b1111110111111111
			      #b1111111111110111
			      #b1101111111111111
			      #b1111111101111111
			      #b1111111111111101
			      #b1111011111111111
			      #b1111111111011111)))
		  (0.2 (8 8 (#b01111111
			     #b11101111
			     #b11111101
			     #b10111111
			     #b11110111
			     #b11111110
			     #b11011111
			     #b11111011)))
		  (0.3 (4 4 (#b0111
			     #b1101
			     #b1011
			     #b1110)))
		  (0.4 (3 3 (#b011
			     #b101
			     #b110)))
		  (0.6 (2 2 (#b01
			     #b10)))
		  (0.7 (3 3 (#b100
			     #b010
			     #b001)))
		  (0.8 (4 4 (#b1000
			     #b0010
			     #b0100
			     #b0001)))
		  (0.9 (8 8 (#b10000000
			     #b00010000
			     #b00000010
			     #b01000000
			     #b00001000
			     #b00000001
			     #b00100000
			     #b00000100)))
		  (0.95 (8 16 (#b1000000000000000
			       #b0000001000000000
			       #b0000000000001000
			       #b0010000000000000
			       #b0000000010000000
			       #b0000000000000010
			       #b0000100000000000
			       #b0000000000100000))))))

(defvar *clx-opacity-stipples*
	(mapcar #'(lambda (entry)
		    (cons (first entry) (apply #'make-stipple-image (second entry))))
		'((+nowhere+ (1 1 (#b0)))
		  (0.05 (8 16 (#b1000000000000000
			       #b0000001000000000
			       #b0000000000001000
			       #b0010000000000000
			       #b0000000010000000
			       #b0000000000000010
			       #b0000100000000000
			       #b0000000000100000)))
		  (0.1 (8 8 (#b10000000
			     #b00010000
			     #b00000010
			     #b01000000
			     #b00001000
			     #b00000001
			     #b00100000
			     #b00000100)))
		  (0.2 (4 4 (#b1000
			     #b0010
			     #b0100
			     #b0001)))
		  (0.3 (3 3 (#b100
			     #b010
			     #b001)))
		  (0.4 (2 2 (#b10
			     #b01)))
		  (0.6 (3 3 (#b011
			     #b101
			     #b110)))
		  (0.7 (4 4 (#b0111
			     #b1101
			     #b1011
			     #b1110)))
		  (0.8 (8 8 (#b01111111
			     #b11101111
			     #b11111101
			     #b10111111
			     #b11110111
			     #b11111110
			     #b11011111
			     #b11111011)))
		  (0.9 (8 16 (#b0111111111111111
			      #b1111110111111111
			      #b1111111111110111
			      #b1101111111111111
			      #b1111111101111111
			      #b1111111111111101
			      #b1111011111111111
			      #b1111111111011111)))
		  (+everywhere+ (1 1 (#b1))))))
		
;; The xlib:image objects are created at load time to save run time & space.
;; Here a '0' means white, '1' black.
(defun clx-decode-luminance (luminance stipple-p)
  (if (not stipple-p)
      (if (< luminance 0.5) 1 0)
      (if (< luminance 0.05)
	  1
	  (dolist (entry *clx-luminance-stipples* 0)
	    (let ((l (car entry))
		  (stipple (cdr entry)))
	      (when (< luminance l)
		(return-from clx-decode-luminance stipple)))))))

(defun clx-decode-opacity (opacity)
  (let ((opacities *clx-opacity-stipples*))
    (cond ((eq opacity +nowhere+)
	   (cdr (first opacities)))
	  ((eq opacity +everywhere+)
	   (cdr (first (last opacities))))
	  (t
	   (let ((last-op (cdr (first opacities)))
		 (value (opacity-value opacity)))
	     (dolist (op (cdr opacities) last-op)
	       (when (< value (car op))
		 (return last-op))
	       (setq last-op (cdr op))))))))

;; This should only be called on a color screen.
(defmethod clx-decode-color ((medium clx-medium) (ink color))
  (with-slots (screen color-p black-pixel) medium
    (multiple-value-bind (red green blue) (color-rgb ink)
      ;;--- Should probably use COLOR-P here.  Otherwise if *CLX-USE-COLOR* is NIL,
      ;;--- colors can still be used for foreground, background, patterns.
      (handler-case
	  (xlib:alloc-color (xlib:screen-default-colormap screen)
			    ;; Either someone has to ensure the the color values in a color
			    ;; object are floats, or we have to here.
			    (xlib:make-color :red (float red)
					     :green (float green)
					     :blue (float blue)))
	;;--- Have to handle resource exhaustion better here.  -- jdi
	(xlib:alloc-error (condition)
	  (declare (ignore condition))
	  (warn "No more colors available for ~S, using black instead." ink)
	  black-pixel)))))

(defmethod clx-decode-color ((medium clx-medium) (ink (eql +foreground-ink+)))
  (slot-value medium 'foreground-pixel))

(defmethod clx-decode-color ((medium clx-medium) (ink (eql +background-ink+)))
  (slot-value medium 'background-pixel))

;;--- We can surely do better than this
(defmethod clx-decode-color ((medium clx-medium) (ink standard-opacity))
  (if (> (slot-value ink 'clim-utils::value) 0.5)
      (slot-value medium 'foreground-pixel)
      (slot-value medium 'background-pixel)))

(defgeneric clx-decode-ink (ink medium))

(defmethod clx-decode-ink ((ink (eql +foreground-ink+)) medium)
  (slot-value medium 'foreground-gc))

(defmethod clx-decode-ink ((ink (eql +everywhere+)) medium)
  (slot-value medium 'foreground-gc))

(defmethod clx-decode-ink ((ink (eql +background-ink+)) medium)
  (slot-value medium 'background-gc))

(defmethod clx-decode-ink ((ink (eql +flipping-ink+)) medium)
  (slot-value medium 'flipping-gc))

(defmethod clx-decode-ink ((ink flipping-ink) medium)
  (multiple-value-bind (design1 design2)
      (decode-flipping-ink ink)
    (cond ((or (and (eq design1 +foreground-ink+) (eq design2 +background-ink+))
	       (and (eq design1 +background-ink+) (eq design2 +foreground-ink+)))
	   (slot-value medium 'flipping-gc))
	  (t (nyi)))))

(defmethod clx-decode-ink ((ink color) medium)
  (with-slots (foreground-gc color-p black-pixel white-pixel ink-table
	       drawable screen root) medium
    (or (gethash ink ink-table)
	(let ((gc (xlib:create-gcontext :drawable drawable)))
	  (xlib:copy-gcontext foreground-gc gc)
	  (cond (color-p
		 (setf (xlib:gcontext-fill-style gc) :solid
		       (xlib:gcontext-foreground gc) (clx-decode-color medium ink)))
		(t
		 (multiple-value-bind (r g b) (color-rgb ink)
		   (let* ((luminance (color-luminosity r g b))
			  (color (clx-decode-luminance luminance t)))
		     (cond ((eq color 1)
			    (setf (xlib:gcontext-fill-style gc) :solid
				  (xlib:gcontext-foreground gc) black-pixel))
			   ((eq color 0)
			    (setf (xlib:gcontext-fill-style gc) :solid
				  (xlib:gcontext-foreground gc) white-pixel))
			   (t			; color is an image
			    (setf (xlib:gcontext-fill-style gc) :tiled)
			    (let ((pixmap (xlib:create-pixmap
					    :drawable drawable
					    :width (xlib:image-width color)
					    :height (xlib:image-height color)
					    ;;--- is this right?
					    :depth (xlib:screen-root-depth screen))))
			      (xlib:put-image pixmap
					      (slot-value root 'stipple-gc)
					      color :x 0 :y 0 :bitmap-p t)
			      (setf (xlib:gcontext-tile gc) pixmap))))))))
	  (setf (gethash ink ink-table) gc)))))

(defmethod clx-decode-ink ((ink contrasting-ink) medium)
  (clx-decode-ink (make-color-for-contrasting-ink ink) medium))

(defmethod clx-decode-ink ((ink rectangular-tile) medium)
  (with-slots (ink-table) medium
    (or (gethash ink ink-table)
	(multiple-value-bind (pattern width height)
	    (decode-rectangular-tile ink)
	  (clx-decode-pattern pattern medium width height t)))))

(defmethod clx-decode-ink ((ink pattern) medium)
  (clx-decode-pattern ink medium))

(defmethod clx-decode-ink ((ink stencil) medium)
  (declare (ignore medium))
  (error "Stencils and opacities are not supported in this CLIM implementation"))

(defmethod clx-decode-ink ((ink composite-in) medium)
  (declare (ignore medium))
  (error "Compositing is not supported in this CLIM implementation"))

(defmethod clx-decode-ink ((ink composite-out) medium)
  (declare (ignore medium))
  (error "Compositing is not supported in this CLIM implementation"))

(defmethod clx-decode-pattern ((ink design) medium &optional width height tiled-p)
  (declare (ignore medium width height tiled-p))
  (error "Arbitrary patterned designs are not supported in this CLIM implementation"))

(defmethod clx-decode-pattern ((ink color) medium &optional width height tiled-p)
  (declare (ignore medium width height tiled-p))
  (error "A pattern must be a bounded design, ~S isn't" ink))

(defmethod clx-decode-pattern ((ink pattern) medium &optional width height tiled-p)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (foreground-gc foreground-pixel background-gc background-pixel
	       drawable ink-table screen) medium
    (or (gethash ink ink-table)
	(multiple-value-bind (array designs)
	    (decode-pattern ink)
	  (let ((pattern-height (array-dimension array 0))
		(pattern-width  (array-dimension array 1)))
	    (declare (type xlib::array-index pattern-width pattern-height))
	    (unless width
	      (setq width pattern-width))
	    (unless height
	      (setq height pattern-height))
	    #+++ignore
	    (do ((i 0 (1+ i))
		 (design))
		((eq i (length designs)))
	      (setq design (elt designs i))
	      (when (and (not (colorp design))
			 (not (eq design +foreground-ink+))
			 (not (eq design +background-ink+)))
		(error "Pattern designs other than colors are not supported yet.")))
	    (let* ((design-pixels (make-array (length designs)))
		   (depth (xlib:screen-root-depth screen))
		   (image-data 
		     (make-array (list height width)
				 :element-type
				   (cond ((= depth 1) 'xlib::pixarray-1-element-type)
					 ((<= depth 4) 'xlib::pixarray-4-element-type)
					 ((<= depth 8) 'xlib::pixarray-8-element-type)
					 ((<= depth 16) 'xlib::pixarray-16-element-type)
					 ((<= depth 24) 'xlib::pixarray-24-element-type)
					 (t 'xlib::pixarray-32-element-type)))))
	      (declare #+Genera (sys:array-register design-pixels)
		       (simple-vector design-pixels))
	      ;; Cache the decoded designs from the pattern
	      (do* ((num-designs (length designs))
		    (n 0 (1+ n))
		    design)
		   ((eq n num-designs))
		(setq design (elt designs n))
		(setf (svref design-pixels n) (clx-decode-color medium design)))
	      (do ((y 0 (1+ y)))
		  ((eq y (the fixnum height)))
		(declare (type xlib::array-index y))
		(do ((x 0 (1+ x)))
		    ((eq x (the fixnum width)))
		  (declare (type xlib::array-index x))
		  (setf (aref image-data y x)
			(if (or (>= y pattern-height) (>= x pattern-width))
			    background-pixel
			    (svref design-pixels (aref array y x))))))
	      (let ((gc (xlib:create-gcontext :drawable drawable)))
		(xlib:copy-gcontext foreground-gc gc)
		(setf (xlib:gcontext-fill-style gc) :tiled)
		(setf (xlib:gcontext-tile gc)
		      (let ((image
			      (xlib:create-image :depth depth
						 :data image-data
						 :width width :height height))
			    (pixmap
			      (xlib:create-pixmap :width width :height height
						  :drawable drawable
						  :depth depth)))
			(xlib:put-image pixmap gc image :x 0 :y 0)
			pixmap))
		(when (not tiled-p)
		  ;;--- This doesn't work because the clip mask is set below.
		  ;;--- Anyway, the clip-mask applies to the destination, so
		  ;;--- the x and y must be set correctly.  -- jdi
		  (setf (xlib:gcontext-clip-mask gc)
			(list 0 0 pattern-width pattern-height)))
		(setf (gethash ink ink-table) gc))))))))

;;--- It would be nice if we did not have to cons up a new list each
;;--- time.  Perhaps we could test if it had changed.  Perhaps the global
;;--- value of the clipping rectangle could be determined in advance.
(defun compute-gcontext-clip-mask (medium clip-mask)
  (multiple-value-bind (left top right bottom)
      (window-margins medium)
    (let ((s-width (bounding-rectangle-width medium))
	  (s-height (bounding-rectangle-height medium)))
      (if (eq clip-mask :none)
	  (list left top (- s-width right) (- s-height bottom))
	(let* ((x (pop clip-mask))
	       (y (pop clip-mask))
	       (width  (pop clip-mask))
	       (height (pop clip-mask)))
	  (let ((new-clip-left (max left x))
		(new-clip-top (max top y)))
	    (list new-clip-left new-clip-top
		  (- (min (+ left (- s-width right))	;clip-right
			  (+ x width))			;another clip-right
		     new-clip-left)
		   (- (min (+ top (- s-height bottom))	;clip-bottom
			   (+ y height))		;another clip-bottom
		      new-clip-top))))))))

;; This is necessary because the GC used for drawing doesn't depend only
;; on the ink used, unfortunately.  We have to adjust the clip-mask for
;; the medium it's used on.
(defmethod clx-decode-ink :around (ink medium)
  (setq ink (call-next-method))
  (with-slots (clip-mask) medium
    (setf (xlib:gcontext-clip-mask ink)
      #---ignore clip-mask
      #+++ignore (compute-gcontext-clip-mask medium clip-mask)))
  ink)
    
;; This only needs to be called for shapes, not points or characters.
;; Basically, we want to do things in here that operate on cached inks
;; (graphics contexts).
(defmethod clx-adjust-ink (ink medium line-style x-origin y-origin)
  (with-slots (points-to-pixels) medium
    (when (eq (xlib:gcontext-fill-style ink) :tiled)
      (setf (xlib:gcontext-ts-x ink) x-origin
	    (xlib:gcontext-ts-y ink) y-origin))
    (when line-style
      (let ((thickness (line-style-thickness line-style)))
	(ecase (line-style-unit line-style)
	  (:normal)
	  (:point
	   (setf thickness (* thickness points-to-pixels))))
	(setf (xlib:gcontext-line-width ink) 
	  (if (< thickness 2) 0 (round thickness)))
	(let ((dashes (line-style-dashes line-style)))
	  (cond (dashes
		 (setf (xlib:gcontext-line-style ink) :dash)
		 (setf (xlib:gcontext-dashes ink) 
		       #-Allegro (cond ((eq dashes t) #(4 4))
				       ((listp dashes) (coerce dashes 'vector))
				       (t dashes))
		       #+Allegro (cond ((eq dashes t) '(4 4))
				       ((vectorp dashes) (coerce dashes 'list))
				       (t dashes))))
		(t
		 (setf (xlib:gcontext-line-style ink) :solid))))
	(setf (xlib:gcontext-cap-style ink)
	      (ecase (line-style-cap-shape line-style)
		(:butt :butt)
		(:square :projecting)
		(:round :round)
		(:no-end-point :not-last)))
	(setf (xlib:gcontext-join-style ink)
	      (ecase (line-style-joint-shape line-style)
		((:miter :none) :miter)
		(:bevel :bevel)
		(:round :round)))))
    ink)) 


(defmethod port-draw-point* ((port clx-port) sheet medium x y)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(line-style (medium-line-style medium))
	(drawable (slot-value medium 'drawable)))
    (convert-to-device-coordinates transform x y)
    (let ((thickness (line-style-thickness line-style))
	  (gc (clx-decode-ink ink medium)))
      (if (< thickness 2)
	  (xlib:draw-point drawable gc x y)
	  (let ((thickness (round thickness)))
	    (xlib:draw-arc drawable gc x y thickness thickness 0 2pi t))))))

(defmethod port-draw-line* ((port clx-port) sheet medium x1 y1 x2 y2)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(line-style (medium-line-style medium))
	(drawable (slot-value medium 'drawable)))
    (convert-to-device-coordinates transform
      x1 y1 x2 y2)
    (xlib:draw-line drawable
		    (clx-adjust-ink (clx-decode-ink ink medium) medium line-style
				    (min x1 x2) (min y1 y2))
		    x1 y1 x2 y2)))

(defmethod port-draw-rectangle* ((port clx-port) sheet medium
				 left top right bottom filled)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(line-style (medium-line-style medium))
	(drawable (slot-value medium 'drawable)))
    (convert-to-device-coordinates transform
      left top right bottom)
    (xlib:draw-rectangle drawable 
			 (clx-adjust-ink (clx-decode-ink ink medium) medium line-style
					 left top)
			 left top (- right left) (- bottom top) filled)))

(defmethod port-draw-polygon* ((port clx-port) sheet medium points closed filled)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(line-style (medium-line-style medium))
	(drawable (slot-value medium 'drawable)))
    (let ((minx most-positive-fixnum)
	  (miny most-positive-fixnum)
	  (points (copy-list points)))
      ;; These really are fixnums, since we're fixing coordinates below
      (declare (type fixnum minx miny))
      (do* ((points points (cddr points)))
	   ((null points))
	(let ((x (first points))
	      (y (second points)))
	  (convert-to-device-coordinates transform x y)
	  (setf (first points) x)
	  (setf (second points) y)
	  (if (< x minx) (setq minx x))
	  (if (< y miny) (setq miny y))))
      (when (and closed line-style)		;kludge
	(setq points (append points `(,(first points) ,(second points)))))
      (xlib:draw-lines drawable 
		       (clx-adjust-ink (clx-decode-ink ink medium) medium line-style
				       minx miny)
		       points :fill-p filled))))

(defmethod port-draw-ellipse* ((port clx-port) sheet medium
			       center-x center-y
			       radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			       start-angle end-angle filled)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(line-style (medium-line-style medium))
	(drawable (slot-value medium 'drawable)))
    (convert-to-device-coordinates transform center-x center-y)
    (convert-to-device-distances transform 
      radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
    (when (null start-angle)
      (setq start-angle 0.0
	    end-angle 2pi))
    ;; Awaiting implementation of real graphics model
    ;; CLX takes a start angle and a delta-angle relative to it, but measures angles
    ;; such that pi/2 points straight up even though Y increases downwards.  Flip that.
    (setq start-angle (- 2pi start-angle)
	  end-angle (- 2pi end-angle))
    ;; We also have to flip the sense of clockwise.
    (rotatef start-angle end-angle)
    ;; The caller has already coerced start- and end-angle to be in the range 0 <= angle < 2pi
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
		     ;; Degrade to drawing a rectilinear ellipse
		     (values (truncate (sqrt s-1)) 
			     (truncate (sqrt s-2)))))))
      (let ((angle-size (- end-angle start-angle)))
	(xlib:draw-arc drawable 
		       (clx-adjust-ink (clx-decode-ink ink medium) medium
				       line-style
				       (- center-x 
					  (if (zerop radius-1-dx)
					      radius-2-dx
					      radius-1-dx))
				       (- center-y 
					  (if (zerop radius-1-dy)
					      radius-2-dy
					      radius-1-dy)))
		       (- center-x x-radius) (- center-y y-radius)
		       (* x-radius 2) (* y-radius 2)
		       ;; CLX measures the second angle relative to the first
		       start-angle angle-size filled)))))

(defmethod port-draw-string* ((port clx-port) sheet medium
			      string x y start end align-x align-y
			      towards-x towards-y transform-glyphs)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(text-style (medium-merged-text-style medium))
	(drawable (slot-value medium 'drawable)))
    (convert-to-device-coordinates transform x y)
    (when towards-x
      (convert-to-device-coordinates transform towards-x towards-y))
    (unless end
      (setq end (length string)))
    (let* ((font (if text-style
		     (text-style-mapping port text-style)
		     (clx-get-default-font port medium)))
	   (ascent (xlib:font-ascent font))
	   (descent (xlib:font-descent font))
	   (height (+ ascent descent))
	   (gc (clx-decode-ink ink medium)))
      (let ((x-adjust (clim-internals::compute-text-x-adjustment 
			align-x medium string text-style start end))
	    (y-adjust (clim-internals::compute-text-y-adjustment
			align-y descent ascent height)))
	(incf x x-adjust)
	(incf y y-adjust)
	(when towards-x
	  (incf towards-x x-adjust)
	  (incf towards-y y-adjust)))
      (setf (xlib:gcontext-font gc) font)
      (xlib:draw-glyphs drawable gc x y string :start start :end end))))

(defmethod port-draw-character* ((port clx-port) sheet medium
				 character x y align-x align-y
				 towards-x towards-y transform-glyphs)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(text-style (medium-merged-text-style medium))
	(drawable (slot-value medium 'drawable)))
    (convert-to-device-coordinates transform x y)
    (when towards-x
      (convert-to-device-coordinates transform towards-x towards-y))
    (let* ((font (if text-style
		     (text-style-mapping port text-style)
		     (clx-get-default-font port medium)))
	   (ascent (xlib:font-ascent font))
	   (descent (xlib:font-descent font))
	   (height (+ ascent descent))
	   (gc (clx-decode-ink ink medium)))
      (let ((x-adjust (clim-internals::compute-text-x-adjustment 
			align-x medium character text-style))
	    (y-adjust (clim-internals::compute-text-y-adjustment
			align-y descent ascent height)))
	(incf x x-adjust)
	(incf y y-adjust)
	(when towards-x
	  (incf towards-x x-adjust)
	  (incf towards-y y-adjust)))
      (setf (xlib:gcontext-font gc) font)
      (xlib:draw-glyph drawable gc x y character)))) 

(defmethod port-draw-text* ((port clx-port) sheet medium
			    string-or-char x y start end
			    align-x align-y
			    towards-x towards-y transform-glyphs)
  (if (characterp string-or-char)
      (port-draw-character* port sheet medium
			    string-or-char x y align-x align-y
			    towards-x towards-y transform-glyphs)
      (port-draw-string* port sheet medium
			 string-or-char x y start end align-x align-y
			 towards-x towards-y transform-glyphs)))

(defmethod port-beep ((port clx-port) sheet)
  (declare (ignore sheet))
  (xlib:bell (port-display port)))


;;--- FROM-SHEET and TO-SHEET should be mediums, not sheets
;;--- This needs to be able to copy to/from pixmaps, too
;;--- arglist s/b FROM-MEDIUM FROM-X FROM-Y TO-MEDIUM TO-X TO-Y WIDTH HEIGHT
(defmethod port-copy-area ((port clx-port)
			   from-sheet to-sheet
			   from-left from-top from-right from-bottom
			   to-left to-top)
  (assert (eq from-sheet to-sheet))
  ;; coords in "host" coordinate system
  (let ((transform (sheet-native-transformation from-sheet)))
    (convert-to-device-coordinates transform
       from-left from-top from-right from-bottom to-left to-top)
    (let ((width (- from-right from-left))
	  (height (- from-bottom from-top)))
      (when (>= to-left from-left)
	;; shifting to the right
	(setq width (- (abs width))))
      (when (>= to-top from-top)
	(setq height (- (abs height))))
      (with-sheet-medium (medium from-sheet)
	(let ((width (- from-right from-left))
	      (height (- from-bottom from-top))
	      (drawable (slot-value medium 'drawable))
	      (copy-gc (slot-value medium 'copy-gcontext)))
	  (xlib:copy-area drawable copy-gc
			  from-left from-top width height drawable
			  to-left to-top))))))

#||
(defmethod copy-area
	   ((medium clg-medium) x y 
	    (source pixmap) left bottom width height
	    &optional (boole boole-1))
  (with-slots (drawable gcontext port device-transformation)
	      medium
    (multiple-value-setq (x y) 
      (transform-rectangle* device-transformation 
			    x y (+ x width) (+ y height)))
    (xlib:with-gcontext (gcontext :function boole)
      (xlib:copy-area (realize-pixmap port source) gcontext
		      left 
		      (- (pixmap-height source) (+ bottom height))
		      (fix-coordinate width) (fix-coordinate height)
		      drawable 
		      (fix-coordinate x) (fix-coordinate y)))))

(defmethod copy-area
	   ((medium clg-display-medium) x y 
	    (source clg-medium) left bottom width height
	    &optional (boole boole-1))
  (with-slots (drawable gcontext port device-transformation)
	      medium
    (multiple-value-setq (x y)
      (transform-rectangle* device-transformation
			    x y (+ x width) (+ y height)))
    ;; Adjust the offset into the source by the source's transformation
    (with-slots ((source-device-transformation device-transformation)) source
      (multiple-value-setq (left bottom)
	(transform-rectangle* source-device-transformation
			      left bottom
			      (+ left width) (+ bottom height))))
    (xlib:with-gcontext (gcontext :function boole)
      (xlib:copy-area (slot-value source 'drawable) gcontext
		      (fix-coordinate left) (fix-coordinate bottom)
		      (round width) (round height)	;--- round???
		      drawable
		      (fix-coordinate x) (fix-coordinate y)))))

(defmethod copy-area ((pixmap pixmap) dst-x dst-y 
		      (medium clg-display-medium) src-x src-y src-w src-h
		      &optional (boole boole-1))
  ;;; This all only works for pixels coordinate systems.
  (with-slots (drawable gcontext port device-transformation) medium
    (multiple-value-setq (src-x src-y)
      (transform-rectangle* device-transformation
			    src-x src-y (+ src-x src-w)
			    (+ src-y src-h)))
    (let ((xbm (realize-pixmap port pixmap)))
      (xlib:with-gcontext (gcontext :function boole)
	(xlib:copy-area drawable gcontext
			(fix-coordinate src-x) (fix-coordinate src-y)
			src-w src-h
			xbm dst-x (- (pixmap-height pixmap) dst-y src-h))))))
||#
