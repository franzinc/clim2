;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader$


(defclass clx-medium (medium)
  ((foreground-gcontext :reader medium-foreground-gcontext :initform nil)
   (background-gcontext :reader medium-background-gcontext :initform nil)
   (flipping-gcontext :reader medium-flipping-gcontext :initform nil)
   (ink-table :initform (make-hash-table))))

(defmethod engraft-medium :after ((medium clx-medium) (port clx-port) sheet)
  (with-slots 
      (foreground-gcontext background-gcontext flipping-gcontext) 
      medium
    (let ((drawable (tk::display-root-window (port-display port))))
      (unless foreground-gcontext
	(setf foreground-gcontext (tk::make-instance 'tk::gcontext :drawable drawable)))
      (unless background-gcontext
	(setf background-gcontext (tk::make-instance 'tk::gcontext
						     :drawable drawable)))
      (unless flipping-gcontext
	(setf flipping-gcontext
	  (tk::make-instance 'tk::gcontext 
			     :function boole-xor
			     :drawable drawable)))
      (recompute-gcs medium))))

(defmethod degraft-medium :after ((medium clx-medium) (port clx-port) sheet)
  (declare (ignore sheet))
  (with-slots 
      (foreground-gcontext background-gcontext flipping-gcontext)
      medium
    (macrolet ((loose-gc (gc)
		 `(when ,gc
		    (tk::free-gcontext ,gc)
		    (setf ,gc nil))))
      (loose-gc foreground-gcontext)
      (loose-gc background-gcontext)
      (loose-gc flipping-gcontext))))

(defun recompute-gcs (medium)
  (with-slots 
      (foreground-gcontext background-gcontext flipping-gcontext)
      medium
    (when (and foreground-gcontext background-gcontext flipping-gcontext)
      (let ((foreground-pixel
	     (clx-decode-color medium (medium-foreground medium)))
	    (background-pixel
	     (clx-decode-color medium (medium-background medium))))
	(setf (tk::gcontext-foreground foreground-gcontext) foreground-pixel
	      (tk::gcontext-background foreground-gcontext) background-pixel
	      (tk::gcontext-foreground background-gcontext) background-pixel
	      (tk::gcontext-foreground flipping-gcontext)
	      (logxor foreground-pixel background-pixel))))))
      
(defmethod (setf medium-background) :after (ink (medium clx-medium))
  (recompute-gcs medium))

(defmethod (setf medium-foreground) :after (ink (medium clx-medium))
  (recompute-gcs medium))

(defmethod (setf medium-ink) :after (ink (medium clx-medium))
  (recompute-gcs medium))


;;;  Below is stuff from clx-implementation

;;;; Translation between CLIM and CLX graphics model

(defun make-stipple-image (height width patterns)
  (xlib:create-image :width width :height height
		     :data (make-stipple-array height width patterns)
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

;; This should only be called on a color screen.
(defmethod clx-decode-color ((stream clx-window) (ink color))
  (with-slots (screen color-p black-pixel) stream
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

(defmethod clx-decode-color ((stream clx-window) (ink (eql +foreground-ink+)))
  (slot-value stream 'foreground-pixel))

(defmethod clx-decode-color ((stream clx-window) (ink (eql +background-ink+)))
  (slot-value stream 'background-pixel))

;;--- We can surely do better than this
(defmethod clx-decode-color ((stream clx-window) (ink standard-opacity))
  (if (> (slot-value ink 'clim-utils::value) 0.5)
      (slot-value stream 'foreground-pixel)
      (slot-value stream 'background-pixel)))

(defgeneric clx-decode-ink (ink stream))

(defmethod clx-decode-ink ((ink (eql +foreground-ink+)) stream)
  (slot-value stream 'foreground-gc))

(defmethod clx-decode-ink ((ink (eql +background-ink+)) stream)
  (slot-value stream 'background-gc))

(defmethod clx-decode-ink ((ink (eql +flipping-ink+)) stream)
  (slot-value stream 'flipping-gc))

(defmethod clx-decode-ink ((ink flipping-ink) stream)
  (multiple-value-bind (design1 design2)
      (decode-flipping-ink ink)
    (cond ((or (and (eql design1 +foreground-ink+) (eql design2 +background-ink+))
	       (and (eql design1 +background-ink+) (eql design2 +foreground-ink+)))
	   (slot-value stream 'flipping-gc))
	  (t (nyi)))))

(defmethod clx-decode-ink ((ink color) stream)
  (with-slots (window ink-table foreground-gc color-p black-pixel white-pixel root) stream
    (or (gethash ink ink-table)
	(let ((gc (xlib:create-gcontext :drawable window)))
	  (xlib:copy-gcontext foreground-gc gc)
	  (cond (color-p
		 (setf (xlib:gcontext-fill-style gc) :solid
		       (xlib:gcontext-foreground gc) (clx-decode-color stream ink)))
		(t
		 (multiple-value-bind (r g b) (color-rgb ink)
		   (let* ((luminance (color-luminosity r g b))
			  (color (clx-decode-luminance luminance t)))
		     (cond ((eql color 1)
			    (setf (xlib:gcontext-fill-style gc) :solid
				  (xlib:gcontext-foreground gc) black-pixel))
			   ((eql color 0)
			    (setf (xlib:gcontext-fill-style gc) :solid
				  (xlib:gcontext-foreground gc) white-pixel))
			   (t			; color is an image
			    (setf (xlib:gcontext-fill-style gc) :tiled)
			    (let ((pixmap (xlib:create-pixmap
					    :drawable window
					    :width (xlib:image-width color)
					    :height (xlib:image-height color)
					    ;;--- is this right?
					    :depth (xlib:drawable-depth window))))
			      (xlib:put-image pixmap
					      (slot-value root 'stipple-gc)
					      color :x 0 :y 0 :bitmap-p t)
			      (setf (xlib:gcontext-tile gc) pixmap))))))))
	  (setf (gethash ink ink-table) gc)))))

(defmethod clx-decode-ink ((ink contrasting-ink) stream)
  (clx-decode-ink (make-color-for-contrasting-ink ink) stream))

(defmethod clx-decode-ink ((ink rectangular-tile) stream)
  (with-slots (ink-table) stream
    (or (gethash ink ink-table)
	(multiple-value-bind (pattern width height)
	    (decode-rectangular-tile ink)
	  (clx-decode-pattern pattern stream width height t)))))

(defmethod clx-decode-ink ((ink pattern) stream)
  (clx-decode-pattern ink stream))

(defmethod clx-decode-ink ((ink stencil) stream)
  (declare (ignore stream))
  (error "Stencils and opacities are not supported in this CLIM implementation"))

(defmethod clx-decode-ink ((ink composite-in) stream)
  (declare (ignore stream))
  (error "Compositing is not supported in this CLIM implementation"))

(defmethod clx-decode-ink ((ink composite-out) stream)
  (declare (ignore stream))
  (error "Compositing is not supported in this CLIM implementation"))

(defmethod clx-decode-pattern ((ink design) stream &optional width height tiled-p)
  (declare (ignore stream width height tiled-p))
  (error "Arbitrary patterned designs are not supported in this CLIM implementation"))

(defmethod clx-decode-pattern ((ink color) stream &optional width height tiled-p)
  (declare (ignore stream width height tiled-p))
  (error "A pattern must be a bounded design, ~S isn't" ink))

(defmethod clx-decode-pattern ((ink pattern) stream &optional width height tiled-p)
  (with-slots (foreground-gc foreground-pixel background-gc background-pixel
	       window ink-table screen) stream
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
		((eql i (length designs)))
	      (setq design (elt designs i))
	      (when (and (not (colorp design))
			 (not (eql design +foreground-ink+))
			 (not (eql design +background-ink+)))
		(error "Pattern designs other than colors are not supported yet.")))
	     (let* ((depth (xlib:screen-root-depth screen))
		    (image-data (make-array (list height width)
					    :element-type `(unsigned-byte ,depth))))
	       (do ((y 0 (1+ y)))
		   ((eql y (the fixnum height)))
		 (declare (type xlib::array-index y))
		 (do ((x 0 (1+ x)))
		     ((eql x (the fixnum width)))
		   (declare (type xlib::array-index x))
		   (setf (aref image-data y x)
			 (if (or (>= y pattern-height) (>= x pattern-width))
			     background-pixel
			     (clx-decode-color stream (elt designs (aref array y x)))))))
	       (let ((gc (xlib:create-gcontext :drawable window)))
		(xlib:copy-gcontext foreground-gc gc)
		(setf (xlib:gcontext-fill-style gc) :tiled)
		(setf (xlib:gcontext-tile gc)
		  (let ((image
			 (xlib:create-image :depth depth
					    :bits-per-pixel depth
					    :data image-data
					    :width width :height height))
			(pixmap
			  (xlib:create-pixmap :width width :height height
					      :drawable window
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
(defun compute-gcontext-clip-mask (stream clip-mask)
  (multiple-value-bind 
      (left top right bottom)
      (window-margins stream)
    (let ((s-width (bounding-rectangle-width stream))
	  (s-height (bounding-rectangle-height stream)))
      (if (eql clip-mask :none)
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
;; the stream it's used on.
(defmethod clx-decode-ink :around (ink stream)
  (setq ink (call-next-method))
  (with-slots (clip-mask) stream
    (setf (xlib:gcontext-clip-mask ink)
      #---ignore clip-mask
      #+++ignore (compute-gcontext-clip-mask stream clip-mask)))
  ink)
    
;; This only needs to be called for shapes, not points or characters.
;; Basically, we want to do things in here that operate on cached inks
;; (graphics contexts).
(defmethod clx-adjust-ink (ink stream line-style x-origin y-origin)
  (with-slots (points-to-pixels) stream
    (when (eql (xlib:gcontext-fill-style ink) :tiled)
      #---ignore
      (setf (xlib:gcontext-ts-x ink) x-origin
	    (xlib:gcontext-ts-y ink) y-origin)
      #+++ignore
      (with-bounding-rectangle* (left top right bottom) (window-viewport stream)
	(declare (ignore right bottom))
	(let ((size (xlib:drawable-width (xlib:gcontext-tile ink))))
	  (setf (xlib:gcontext-ts-x ink) (mod left size)	;--- 16 is a magic
		(xlib:gcontext-ts-y ink) (mod top size)))))
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


(defmethod port-draw-point* ((port clx-port) sheet medium
			     x y)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(line-style (medium-line-style medium)))
    (convert-to-device-coordinates transform x y)
    (let ((thickness (line-style-thickness line-style))
	  (gc (clx-decode-ink ink stream)))
      (ecase (line-style-unit line-style)
	((:normal :point)
	 (setf thickness (* thickness points-to-pixels))))
      (if (< thickness 2)
	  (xlib:draw-point window gc x y)
	  (let ((thickness (round thickness)))
	    (xlib:draw-arc window gc x y thickness thickness 0 2pi t))))))

(defmethod port-draw-line* ((port clx-port) sheet medium
			    x1 y1 x2 y2)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(line-style (medium-line-style medium)))
    (convert-to-device-coordinates transform
      x1 y1 x2 y2)
    (xlib:draw-line window
		    (clx-adjust-ink (clx-decode-ink ink stream) stream line-style
				    (min start-x end-x) (min start-y end-y))
		    start-x start-y end-x end-y)))

(defmethod port-draw-rectangle* ((port clx-port) sheet medium
				 left top right bottom)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(line-style (medium-line-style medium)))
    (convert-to-device-coordinates transform
      left top right bottom)
    (xlib:draw-rectangle window 
			 (clx-adjust-ink (clx-decode-ink ink stream) stream line-style
					 left top)
			 left top (- right left) (- bottom top) (null line-style))))

(defmethod port-draw-polygon* ((port clx-port) sheet medium
			       points closed ink line-style)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(line-style (medium-line-style medium)))
    (let ((minx most-positive-fixnum)
	  (miny most-positive-fixnum)
	  (points (copy-list points)))
      (declare (fixnum minx miny))
      (do* ((points points (cddr points))a)
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
      (with-slots (window) stream
	(xlib:draw-lines window 
			 (clx-adjust-ink (clx-decode-ink ink stream) stream line-style
					 minx miny)
			 points :fill-p (null line-style))))))

(defmethod port-draw-ellipse* ((port clx-port) sheet medium
			       center-x center-y
			       radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			       start-angle end-angle ink line-style)
  (let ((transform (sheet-device-transformation sheet))
	(ink (medium-ink medium))
	(line-style (medium-line-style medium)))
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
      (with-slots (window) stream
	(let ((angle-size (- end-angle start-angle)))
	  (xlib:draw-arc window 
			 (clx-adjust-ink (clx-decode-ink ink stream) stream
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
			 start-angle angle-size (null line-style)))))))

(defmethod clx-get-default-font ((stream clx-window))
  (with-slots (window display-device-type) stream
    (text-style-mapping
      display-device-type (medium-merged-text-style stream) *standard-character-set* window)))

(defmethod port-draw-string* ((port clx-port) sheet medium
			      string x y start end align-x align-y)
  (when (window-drawing-possible stream)
    (let ((transform (sheet-device-transformation sheet))
	  (ink (medium-ink medium))
	  (text-style (medium-merged-text-style medium)))
      (convert-to-device-coordinates transform x y)
      (unless end
	(setq end (length string)))
      (let* ((font (if text-style
		       (text-style-mapping
			 display-device-type text-style *standard-character-set* window)
		       (clx-get-default-font stream)))
	     (ascent (xlib:font-ascent font))
	     (descent (xlib:font-descent font))
	     (height (+ ascent descent))
	     (gc (clx-decode-ink ink stream)))
	(incf x (compute-text-x-adjustment 
		  align-x stream string text-style start end))
	(incf y (compute-text-y-adjustment
		  align-y descent ascent height))
	(setf (xlib:gcontext-font gc) font)
	(xlib:draw-glyphs window gc x y string :start start :end end)))))

(defmethod port-draw-character* ((port clx-port) sheet medium
				 character x y align-x align-y)
  (when (window-drawing-possible stream)
    (let ((transform (sheet-device-transformation sheet))
	  (ink (medium-ink medium))
	  (text-style (medium-merged-text-style medium)))
      (convert-to-device-coordinates transform x y)
      (let* ((font (if text-style
		       (text-style-mapping
			 display-device-type text-style *standard-character-set* window)
		       (clx-get-default-font stream)))
	     (ascent (xlib:font-ascent font))
	     (descent (xlib:font-descent font))
	     (height (+ ascent descent))
	     (gc (clx-decode-ink ink stream)))
	(incf x (compute-text-x-adjustment 
		  align-x stream character text-style))
	(incf y (compute-text-y-adjustment
		  align-y descent ascent height))
	(setf (xlib:gcontext-font gc) font)
	(xlib:draw-glyph window gc x y character))))) 


(defmethod silica::port-write-string-1 ((port clx-port) medium 
					glyph-buffer start end 
					x-font color x y)
  (unless (= start end)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (window (tk::widget-window (sheet-mirror sheet) nil))
	   (font x-font))
    
      ;; At one point we checked to see whether the widget is unrealized
      ;; or not.  Can we draw on disabled sheets?
    
      (multiple-value-setq (x y) (devicize-point transform x y))
    
      (incf y (tk::font-ascent font))

      (let ((gc (clx-decode-ink (medium-ink medium) medium)))
	(setf (tk::gcontext-font gc) font)
	(etypecase glyph-buffer
	  ((simple-array (unsigned-byte 16))
	   (x11::xdrawstring16
	    (tk::display-handle (tk::object-display window))
	    (tk::object-handle window)
	    (tk::object-handle gc)
	    x 
	    y
	    glyph-buffer
	    (- end start))))))))


(defmethod port-beep ((port clx-port) (sheet t))
  (x11:xbell (tk::display-handle (port-display port)) 100))



(defmethod silica::port-copy-area ((port clx-port)
					   medium
					   from-left
					   from-top 
					   from-right 
					   from-bottom
					   to-left
					   to-top)
  ;; coords in "host" coordinate system
  (let ((transform +identity-transformation+
		   ;; This is really horrible cos sheet-transformation
		   ;; are used to implement scrolling but the
		   ;; coordinates are in the coordinates of the
		   ;; viewport rather than the sheet so we are screwed.
		   #+oh-no(sheet-native-transformation medium)))
    (multiple-value-setq
	(from-left from-top)
      (devicize-point transform from-left from-top))
    (multiple-value-setq
	(from-right from-bottom)
      (devicize-point transform from-right from-bottom))
    (multiple-value-setq
	(to-left to-top)
      (devicize-point transform to-left to-top))
    (let ((window (tk::widget-window (sheet-mirror medium))))
      (let ((width (- from-right from-left))
	    (height (- from-bottom from-top))
	    ;;;-------------------!
	    (copy-gc (make-instance 'tk::gcontext
				    :drawable window)))
	(tk::copy-area 
	 window copy-gc from-left from-top width height window to-left to-top)))))
