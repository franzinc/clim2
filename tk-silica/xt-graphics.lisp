U;; -*- mode: common-lisp; package: tk-silica -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: xt-graphics.lisp,v 1.19 92/04/28 09:26:35 cer Exp Locker: cer $

(in-package :tk-silica)

(defclass xt-medium (medium)
  ((foreground-gcontext :reader medium-foreground-gcontext :initform nil)
   (background-gcontext :reader medium-background-gcontext :initform nil)
   (flipping-gcontext :reader medium-flipping-gcontext :initform nil)
   (drawable :initform nil)
   (color-p)
   (ink-table :initform (make-hash-table :test #'equal))
   (tile-gcontext :initform nil)	; The following don't belong here.
   (white-pixel :initform 0)
   (black-pixel :initform 1)))

(defmethod medium-drawable ((medium xt-medium))
  (with-slots (drawable sheet) medium
    (or drawable
	(setf drawable (fetch-medium-drawable 
			sheet
			(sheet-mirror sheet))))))

(defmethod deallocate-medium :after (port (medium xt-medium))
  (declare (ignore port))
  (with-slots (drawable) medium
    (setf drawable nil)))

(defmethod fetch-medium-drawable (sheet (mirror tk::xt-root-class))
  (declare (ignore sheet))
  (tk::widget-window mirror nil))

(defmethod engraft-medium :after ((medium xt-medium) (port xt-port) sheet)
   (with-slots (foreground-gcontext background-gcontext flipping-gcontext color-p
		drawable tile-gcontext white-pixel black-pixel)
      medium
    (setf (medium-sheet medium) sheet)
    (when (and drawable
	       (not (eq (port-display port)
			(tk::object-display drawable))))
      (error "drawable and display do not match"))
    (let* ((display (port-display port))
	   (screen (tk::display-screen-number display))
	   (drawable (or drawable
			 (tk::display-root-window display))))
      (setf foreground-gcontext (tk::make-instance 'tk::gcontext
				    :drawable drawable))
      (setf background-gcontext (tk::make-instance 'tk::gcontext
				    :drawable drawable))
      (setf flipping-gcontext
	(tk::make-instance 'tk::gcontext 
	  :drawable drawable
	  :function boole-xor))
      (setf color-p (color-medium-p medium))
      (setf white-pixel (x11:xwhitepixel display screen))
      (setf black-pixel (x11:xblackpixel display screen))
      (setf tile-gcontext (make-instance 'tk::gcontext
				 :drawable drawable
				 :foreground black-pixel
				 :background white-pixel))
      (recompute-gcs medium))))

(defmethod degraft-medium :after ((medium xt-medium) (port xt-port) sheet)
  (declare (ignore sheet))
  (with-slots 
       (foreground-gcontext background-gcontext flipping-gcontext tile-gcontext
	drawable)
      medium
    (setf drawable nil
	  (medium-sheet medium) nil)
    (macrolet ((loose-gc (gc)
		 `(when ,gc
		    (tk::free-gcontext ,gc)
		    (setf ,gc nil))))
      (loose-gc foreground-gcontext)
      (loose-gc background-gcontext)
      (loose-gc flipping-gcontext)
      (loose-gc tile-gcontext))))

(defparameter *use-color* t)		; For debugging monochrome
(defun color-medium-p (medium)
  (and *use-color*
       (let ((display (port-display (port (medium-sheet medium)))))
	 (> (x11:xdefaultdepth display (tk::display-screen-number display)) 2))))

(defun recompute-gcs (medium)
  (with-slots 
      (foreground-gcontext background-gcontext flipping-gcontext)
      medium
    (when (and foreground-gcontext background-gcontext flipping-gcontext)
      (let ((foreground-pixel
	     (decode-color medium (medium-foreground medium)))
	    (background-pixel
	     (decode-color medium (medium-background medium))))
	(setf (tk::gcontext-foreground foreground-gcontext) foreground-pixel
	      (tk::gcontext-background foreground-gcontext) background-pixel
	      (tk::gcontext-foreground background-gcontext) background-pixel
	      (tk::gcontext-background background-gcontext) background-pixel
	      (tk::gcontext-foreground flipping-gcontext)
	      (logxor foreground-pixel background-pixel))))))
      
(defmethod (setf medium-background) :after (ink (medium xt-medium))
  (declare (ignore ink))
  (recompute-gcs medium)
  ;;--- Call handle-repaint
  )

(defmethod (setf medium-foreground) :after (ink (medium xt-medium))
  (declare (ignore ink))
  (recompute-gcs medium)
  ;;--- Call handle-repaint
  )

(defmethod (setf medium-ink) :after (ink (medium xt-medium))
  (declare (ignore ink))
  (recompute-gcs medium))


;;; Colors and their monochrome imposters
;;; Much of this is taken from CLX-IMPLEMENTATION

(defvar *luminance-stipples*
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
		
;; The tk::image objects are created at load time to save startup time.
;; Here a '0' means white, '1' black.
(defun decode-luminance (luminance stipple-p)
  (if (not stipple-p)
      (if (< luminance 0.5) 1 0)	; Questionable.  XX
      (if (< luminance 0.05)
	  1
	  (dolist (entry *luminance-stipples* 0)
	    (let ((l (car entry))
		  (stipple (cdr entry)))
	      (when (< luminance l)
		(return-from decode-luminance stipple)))))))

(defgeneric decode-ink (ink medium))

(defmethod decode-ink ((ink (eql +everywhere+)) medium)
  (slot-value medium 'foreground-gcontext))

(defmethod decode-ink ((ink (eql +foreground-ink+)) medium)
  (slot-value medium 'foreground-gcontext))

(defmethod decode-ink ((ink (eql +background-ink+)) medium)
  (slot-value medium 'background-gcontext))

(defmethod decode-ink ((ink (eql +flipping-ink+)) stream)
  (slot-value stream 'flipping-gcontext))

(defmethod decode-ink ((ink color) (medium xt-medium))
  (with-slots (ink-table sheet tile-gcontext white-pixel black-pixel drawable
			 color-p ink-table)
      medium
    (or (gethash ink ink-table)
	(let ((drawable (or drawable
			    (tk::display-root-window (port-display (port sheet)))))
	      (new-gc (make-instance 'tk::gcontext :drawable drawable)))
	  (cond (color-p
		 (setf (tk::gcontext-foreground new-gc)
		   (decode-color medium ink)))
		(t
		 (multiple-value-bind (r g b) (color-rgb ink)
		   ;; The luminance formula isn't really right.  XXX
		   (let* ((luminance (color-luminosity r g b))
			  (color (decode-luminance luminance t)))
		     (cond ((eq color 1)
			    (setf (tk::gcontext-fill-style new-gc) :solid
				  (tk::gcontext-foreground new-gc) black-pixel))
			   ((eq color 0)
			    (setf (tk::gcontext-fill-style new-gc) :solid
				  (tk::gcontext-foreground new-gc) white-pixel))
			   (t		; color is an image
			    (setf (tk::gcontext-fill-style new-gc) :tiled)
			    (let ((pixmap (make-instance 'tk::pixmap
					    :drawable drawable
					    :width (tk::image-width color)
					    :height (tk::image-height color)
					    :depth (tk::drawable-depth drawable))))
			      (tk::put-image pixmap tile-gcontext color)
			      (setf (tk::gcontext-tile new-gc) pixmap))))))))
	  (setf (gethash ink ink-table) new-gc)))))

(defmethod decode-ink ((ink (eql +nowhere+)) medium)
  (decode-ink-opacity ink medium))

(defmethod decode-ink ((ink standard-opacity) medium)
  (decode-ink-opacity ink medium))

(defmethod decode-ink-opacity (opacity medium)
  (with-slots (ink-table sheet tile-gcontext white-pixel black-pixel drawable
			 foreground-gcontext color-p ink-table)
      medium
    (let ((key (cons opacity foreground-gcontext)))
      (or (gethash key ink-table)
	  (unless (eq :solid (tk::gcontext-fill-style foreground-gcontext))
	    (warn "opacities may only be use with solid colors ~
and on color servers, unless using white or black")
            (return-from decode-ink-opacity foreground-gcontext))
	  (let* ((port (port sheet))
		 (display (port-display port))
		 (drawable (or drawable (tk::display-root-window display)))
		 (new-gc (make-instance 'tk::gcontext :drawable drawable)))
  
	    (x11:xcopygc display foreground-gcontext -1 new-gc)
	    (setf (tk::gcontext-stipple new-gc) (decode-opacity opacity port)
		  (tk::gcontext-fill-style new-gc) :stippled)
	    (setf (gethash key ink-table) new-gc))))))
  

(defmethod decode-ink ((ink contrasting-ink) stream)
  (decode-ink (make-color-for-contrasting-ink ink) stream))

(defmethod decode-ink ((ink rectangular-tile) medium)
  (multiple-value-bind (pattern width height)
      (decode-rectangular-tile ink)
    (xt-decode-pattern pattern medium width height t)))

(defmethod decode-ink ((ink pattern) medium)
  (xt-decode-pattern ink medium))
    
(defmethod decode-color ((medium xt-medium) (x (eql +foreground-ink+)))
  (with-slots (foreground-gcontext) medium
    (tk::gcontext-foreground foreground-gcontext)))

(defmethod decode-color ((medium xt-medium) (x (eql +background-ink+)))
  (with-slots (background-gcontext) medium
    (tk::gcontext-foreground background-gcontext)))

(defmethod decode-color ((stream xt-medium) (ink standard-opacity))
  (if (> (slot-value ink 'clim-utils::value) 0.5)
      (decode-color stream +foreground-ink+)
      (decode-color stream +background-ink+)))

(defmethod decode-color ((medium xt-medium) (ink color))
  (with-slots (color-p white-pixel black-pixel) medium
    (or (gethash ink (port-color-cache (port medium)))
	(setf (gethash ink (port-color-cache (port medium)))
	  (cond (color-p
		 (multiple-value-bind (red green blue)
		     (color-rgb ink)
		   (tk::allocate-color
		    (tk::default-colormap (port-display (port
							 (medium-sheet medium))))
		    (let ((x #.(1- (ash 1 16))))
		      (make-instance 'tk::color
				    :red (truncate (* x red))
				    :green (truncate (* x green))
				    :blue (truncate (* x blue)))))))
		(t
		 (multiple-value-bind (r g b) (color-rgb ink)
		   ;; The luminance formula isn't really right.  XXX
		   (let* ((luminance (/ (+ (* r r) (* g g) (* b b)) 3)))
		     (if (> luminance .5)
			 white-pixel
			 black-pixel)))))))))


(defmethod adjust-ink ((medium xt-medium) gc ink line-style x-origin y-origin)
  (declare (ignore ink))
  (let* ((dashes (line-style-dashes line-style))
	 (gc-line-style
	  (etypecase dashes
	    ((member nil t) :solid)
	    (sequence :dash))))
	
    (tk::set-line-attributes 
     gc
     (let ((thickness (line-style-thickness line-style)))
       (ecase (line-style-unit line-style)
	 (:normal)
	 (:point
	  (setq thickness (* (graft-pixels-per-point (graft medium))
			     thickness))))
       (when (< thickness 2)
	 (setq thickness 0))
       (round thickness))
     gc-line-style
     (ecase (line-style-cap-shape line-style)
       (:butt :butt)
       (:square :projecting)
       (:round :round)
       (:no-end-point :not-last))
     (ecase (line-style-joint-shape line-style)
       ((:miter :none) :miter)
       (:bevel :bevel)
       (:round :round)))
    
    (when (eq gc-line-style :dash)
      (setf (tk::gcontext-dashes gc) dashes))
    
    (let* ((sheet (medium-sheet medium))
	   (dr (sheet-device-region sheet))
	   (mcr (medium-clipping-region medium)))
      (unless (eq mcr +everywhere+)
	(setq mcr (transform-region (sheet-device-transformation sheet) mcr))
	(setq dr (region-intersection dr (bounding-rectangle mcr))))
      (cond ((eq dr +everywhere+)
	     (setf (tk::gcontext-clip-mask gc) :none))
	    ((eq dr +nowhere+)
	     (setf (tk::gcontext-clip-mask gc) :nowhere))
	    (t
	     (with-bounding-rectangle* (a b c d) dr
	       (with-stack-list (x (fix-coordinate a) (fix-coordinate b) 
				   (fix-coordinate (- c a)) (fix-coordinate (- d b)))
		 (setf (tk::gcontext-clip-mask gc) x))))))
    
    (when (member (tk::gcontext-fill-style gc) '(:tiled :stippled) :test #'eq)
      (setf (tk::gcontext-ts-x-origin gc) x-origin
	    (tk::gcontext-ts-y-origin gc) y-origin))
    gc))

#+ignore
(defmethod decode-ink :around ((ink t) (medium xt-medium))
  (let ((gc (call-next-method)))
    gc))

(defmethod xt-decode-pattern ((pattern pattern) medium &optional width height tiled-p)    
  (let* ((ink-table (slot-value medium 'ink-table))
	 (drawable (or (slot-value medium 'drawable)
		       (tk::display-root-window
			(port-display (port (medium-sheet medium))))))
	 (depth (tk::drawable-depth drawable)))
    (or (gethash pattern ink-table)
	(setf (gethash pattern ink-table)
	      (multiple-value-bind (array designs)
		  (decode-pattern pattern)
		(let ((image-data (make-array (array-dimensions array)))
		      (design-pixels (make-array (length designs))))
		  (declare (simple-vector design-pixels))
		  ;; Cache the decoded designs from the pattern
		  (do* ((num-designs (length designs))
			(n 0 (1+ n))
			design)
		       ((eq n num-designs))
		    (setq design (elt designs n))
		    (setf (svref design-pixels n) (decode-color medium design)))
		  (dotimes (w (array-dimension array 1))
		    (dotimes (h (array-dimension array 0))
		      (setf (aref image-data h w)
			    (svref design-pixels (aref array h w)))))
		  (let* ((pattern-height (array-dimension array 0))
			 (pattern-width (array-dimension array 1))
			 (image (make-instance 'tk::image
					       :width pattern-width
					       :height pattern-height
					       :data image-data
					       :depth depth))
			 (gc 
			   (make-instance 'tk::gcontext :drawable drawable))
			 (pixmap 
			   (make-instance 'tk::pixmap
					  :drawable drawable
					  :width pattern-width
					  :height pattern-height
					  :depth depth)))
		    (tk::put-image pixmap gc image)
		    (setf (tk::gcontext-tile gc) pixmap
			  (tk::gcontext-fill-style gc) :tiled)
		    gc)))))))


(defmethod port-draw-point* ((port xt-port) sheet medium x y)
  (let ((transform (sheet-device-transformation sheet)))
    (convert-to-device-coordinates transform x y))
  (when (medium-drawable medium)
    (let ((thickness (line-style-thickness (medium-line-style medium))))
      (if (< thickness 2)
	  (tk::draw-point
	   (medium-drawable medium)
	   (adjust-ink medium
		       (decode-ink (medium-ink medium) medium)
		       (medium-ink medium)
		       (medium-line-style medium)
		       x y)
	   x y)
	(let ((thickness (round thickness)))
	  (tk::draw-ellipse (medium-drawable medium) 
			    (adjust-ink medium
					(decode-ink (medium-ink medium) medium)
					(medium-ink medium)
					(medium-line-style medium)
					(- x thickness)
					(- y thickness))
			    x y 
			    0
			    thickness 
			    0
			    thickness 0 2pi
			    t))))))

(defmethod port-draw-line* ((port xt-port) sheet medium
			    x1 y1 x2 y2)
  (let ((transform (sheet-device-transformation sheet)))
    (convert-to-device-coordinates transform
      x1 y1 x2 y2))
  (when (medium-drawable medium)
    (tk::draw-line
      (medium-drawable medium)
      (adjust-ink medium
		  (decode-ink (medium-ink medium) medium)
		  (medium-ink medium)
		  (medium-line-style medium)
		  (min x1 x2) (min y1 y2))
      x1 y1 x2 y2)))

(defmethod port-draw-rectangle* ((port xt-port) sheet medium
				 x1 y1 x2 y2 filled)
  (let ((transform (sheet-device-transformation sheet)))
    (if (rectilinear-transformation-p transform)
	(progn
	  (convert-to-device-coordinates transform
	    x1 y1 x2 y2) 
	  (when (medium-drawable medium)
	    (tk::draw-rectangle
	      (medium-drawable medium)
	      (adjust-ink medium
			  (decode-ink (medium-ink medium) medium)
			  (medium-ink medium)
			  (medium-line-style medium)
			  (min x1 x2) (min y1 y2))
	      (min x1 x2) (min y1 y2)
	      (abs (- x2 x1)) (abs (- y2 y1))
	      filled)))
      (port-draw-transformed-rectangle*
	port sheet medium x1 y1 x2 y2 filled))))

(defmethod port-draw-polygon* ((port xt-port) sheet medium
			       list-of-x-and-ys
			       closed filled)
  (let* ((transform (sheet-device-transformation sheet))
	 (npoints (/ (length list-of-x-and-ys) 2))
	 (points (xt::make-xpoint-array :number (cond ((and closed (not filled))
						       (incf npoints))
						      (t npoints))))
	 (window (medium-drawable medium))
	 ;; These really are fixnums, since we're fixing coordinates below
	 (minx most-positive-fixnum)
	 (miny most-positive-fixnum)
	 ink)
    (do ((ps list-of-x-and-ys (cddr ps))
	 (i 0 (1+ i))
	 r)
	((null ps)
	 (setq list-of-x-and-ys (nreverse r)))
      (let ((x (first ps))
	    (y (second ps)))
	(convert-to-device-coordinates transform x y)
	(minf minx x)
	(minf miny y)
	(setf (tk::xpoint-array-x points i) x
	      (tk::xpoint-array-y points i) y)))
    (when (and closed (not filled))
      (setf (tk::xpoint-array-x points (- npoints 1)) (tk::xpoint-array-x points 0)
	    (tk::xpoint-array-y points (- npoints 1)) (tk::xpoint-array-y points 0)))
    (setq ink 
      (adjust-ink medium (decode-ink (medium-ink medium) medium)
		  (medium-ink medium)
		  (medium-line-style medium)
		  minx miny))
    (when (medium-drawable medium)
      (if filled
	  (x11:xfillpolygon
	   (tk::object-display window)
	   window
	   ink
	   points
	   npoints
	   x11:complex
	   x11:coordmodeorigin)
	(x11:xdrawlines
	  (tk::object-display window)
	  window
	  ink
	  points
	  npoints
	  x11:coordmodeorigin)))))

(defmethod port-draw-ellipse* ((port xt-port) sheet medium
			       center-x center-y
			       radius-1-dx radius-1-dy radius-2-dx radius-2-dy 
			       start-angle end-angle filled)
  (let ((transform (sheet-device-transformation sheet)))
    (convert-to-device-coordinates transform center-x center-y)
    (convert-to-device-distances transform 
      radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
    (when (medium-drawable medium)
      (tk::draw-ellipse
	(medium-drawable medium)
	(adjust-ink medium
		    (decode-ink (medium-ink medium) medium)
		    (medium-ink medium)
		    (medium-line-style medium)
		    (- center-x 
		       (if (zerop radius-1-dx) radius-2-dx radius-1-dx))
		    (- center-y 
		       (if (zerop radius-1-dy) radius-2-dy radius-1-dy)))
	center-x center-y
	radius-1-dx radius-1-dy radius-2-dx radius-2-dy 
	start-angle end-angle 
	filled)))) 

(defmethod port-draw-text* ((port xt-port) sheet medium
			    string-or-char x y start end
			    align-x align-y
			    towards-x towards-y transform-glyphs
			    )
  (let* ((transform (sheet-device-transformation sheet))
	 (font (text-style-mapping port (medium-text-style medium)))
	 (ascent (tk::font-ascent font)))
    (convert-to-device-coordinates transform x y)
    (when towards-x
      (convert-to-device-coordinates transform towards-x towards-y))
    (when (typep string-or-char 'character)
      (setq string-or-char (string string-or-char)))
    (ecase align-x
      (:center 
       (let ((dx (floor (text-size sheet string-or-char
				   :text-style (medium-text-style medium)
				   :start start :end end) 2)))
	 (when towards-x (decf towards-x dx))
	 (decf x dx)))
      (:left nil))
    (ecase align-y
      (:center 
       (let ((dy (- (text-style-descent (medium-text-style medium) port)
		  (floor (text-style-height (medium-text-style medium) port) 2))))
	 (decf y dy)
	 (when towards-y (decf towards-y dy))))
      (:baseline nil)
      (:top
       (when towards-y (incf towards-y ascent))
       (incf y ascent))) 
    (when (medium-drawable medium)
      (let ((gc (adjust-ink
		 medium
		 (decode-ink (medium-ink medium) medium)
		 (medium-ink medium)
		 (medium-line-style medium)
		 x 
		 (- y ascent)))
	    (drawable (medium-drawable medium)))
	(setf (tk::gcontext-font gc) font)
	(if (and towards-x towards-y)
	    (port-draw-rotated-text
	     port
	     drawable
	     gc
	     x y
	     string-or-char 
	     start 
	     end
	     font
	     towards-x towards-y transform-glyphs)
	  (tk::draw-string
	   drawable
	   gc
	   x y
	   string-or-char start end))))))

(defmethod clim-internals::port-text-bounding-box ((port xt-port)
						   stream
						   string x y start end align-x
						   align-y text-style
						   towards-x
						   towards-y
						   transform-glyphs)
  (declare (ignore string start end transform-glyphs))
  (multiple-value-bind
      (left top right bottom) (call-next-method)
    (if (and towards-y towards-x)
	(flet ((compute-rotation (x y towards-x towards-y)
		 (decf towards-x x)
		 (decf towards-y y)
		 (mod (round (atan towards-y towards-x) (/ pi 2.0))
		      4)))
	  (let ((ascent (text-style-ascent text-style (port stream))))
	    (ecase align-y
	      (:top (incf y ascent)
		    (incf towards-y ascent))
	      ;;-- which
	      ((:baseline :base-line)))
	    (ecase align-x
	      (:left))
	    (let ((transformation
		   (make-rotation-transformation 
		    (* (compute-rotation x y towards-x
						towards-y)
		       (/ pi 2.0))
		    (make-point x y))))
	      (multiple-value-setq (left top)
		(transform-position transformation left top))
	      (multiple-value-setq (right bottom)
		(transform-position transformation right bottom))
	      (values (min left right) (min top bottom)
		      (max left right) (max top bottom)))))
      (values left top right bottom))))

(defun port-draw-rotated-text (port
			       drawable
			       gcontext
			       x
			       y
			       string 
			       start 
			       end
			       font
			       towards-x 
			       towards-y 
			       transform-glyphs)
  (declare (ignore transform-glyphs))
  ;; When we are transforming glyphs:
  ;;-- What we need to do is to create a 2^n depth 1 pixmap containing all
  ;;-- the characters in the font and rotate it by the required amount
  ;; Then use the bitmap  as a clip-mask for drawing when drawing each
  ;; rectangle character
  (flet ((compute-rotation (x y towards-x towards-y)
	   (decf towards-x x)
	   (decf towards-y y)
	   (mod (round (atan towards-y towards-x) (/ pi 2.0)) 4)))
    (let* ((rotation (compute-rotation x y towards-x towards-y))
	   (min-char (xt::font-range font))
	   (ascent (xt::font-ascent font))
	   (descent (xt::font-descent font)))
    (multiple-value-bind
	(pixmap columns 
	 width 
	 leftover-width 
	 rows height
	 leftover-height)
	(find-rotated-text-pixmap
	 port font rotation)
      (flet ((compute-next-x-and-y (char-width x y)
	       (let ()
		 (case rotation
		   (0 (values (+ x char-width) y))
		   (3 (values x (- y char-width)))
		   (2 (values (- x char-width) y))
		   (1 (values x (+ y char-width))))))
	     (compute-clip-x-and-clip-y (char char-width x y)
	       ;;-- This has to get to the right character in the pixmap
	       (multiple-value-bind
		(row column)
		(truncate (- char min-char) columns)
		;; Now we have to take into account the rotation
		(let ((columns columns)
		      (rows rows))
		  #+debug
		  (format t "Was Row ~D of ~D, Column ~D of ~D~%" 
					row rows column columns)
		  (dotimes (i rotation)
		    (psetf column (- (1- rows) row) row column)
		    (rotatef rows columns)))
		#+debug
		(format t "Now Row ~D of ~D, Column ~D of ~D~%" 
			row rows column columns)
		(let ((px (* column (if (oddp rotation) height width)))
		      (py (* row (if (oddp rotation) width height)))
		      (w (- width char-width)))
		  (ecase rotation
		    (0)
		    (3 (incf py leftover-width)
		       (incf py w))
		    (2 (incf px leftover-width)
		       (incf px w)
		       (incf py leftover-height))
		    (1 (incf px leftover-height)))
		  #+debug
		  (format t "x ~D y ~D px ~D py ~D~%" x y px py)
		  (values (- x px) (- y py))))))
	(let  ((ox x)
	       (oy y))
	  (declare (ignore oy)
		   (ignore ox))
	  (setf (tk::gcontext-clip-mask gcontext) pixmap)
	  (unless start (setq start 0))
	  (unless end (setq end (length string)))
	  (dotimes (i (- end start))
	    (let* ((char (char-int (aref string (+ start i))))
		   (char-width (xt::char-width font char))
		   (cx x)
		   (cy y))
	      
	      (ecase rotation
		(0 (decf cy ascent))
		(3 (decf cx ascent)
		   (decf cy char-width))
		(2 (decf cy descent)
		   (decf cx char-width))
		(1 (decf cx descent)))
	      
	      (multiple-value-bind
		  (clip-x clip-y)
		  (compute-clip-x-and-clip-y char char-width cx cy)
		#+debug
		(setf (tk::gcontext-clip-x-origin gcontext) 0
		      (tk::gcontext-clip-y-origin gcontext) 70
		      )
		#+debug
		(tk::draw-rectangle drawable gcontext 
				    0 70 256 256 
				    t)
		#+debug
		(format t "CLip-x, clip-y ~D,~D~%" clip-x clip-y)
		(setf (tk::gcontext-clip-x-origin gcontext) clip-x
		      (tk::gcontext-clip-y-origin gcontext) clip-y
		      ))
	      ;;-- We have lost the clipping region at this point!
	      ;;--- We should draw in the background color also
	      (tk::draw-rectangle drawable gcontext 
				  cx cy 
				  (if (oddp rotation) height char-width)
				  (if (oddp rotation) char-width height)
				  t)
	      ;;
	      (multiple-value-setq
		  (x y) (compute-next-x-and-y char-width x y))))))))))

(defun find-rotated-text-pixmap (port font rotation)
  (let ((x (assoc (list font rotation) (port-rotated-font-cache port) 
		  :test #'equal)))
    (when x
      (return-from find-rotated-text-pixmap (values-list (cdr x))))
    (multiple-value-bind
	(min-char max-char) (xt::font-range font)
      (let ((width (xt::font-width font))
	    (height (xt::font-height font))
	    (ascent (xt::font-ascent font))
	    (nchars (- max-char min-char)))
	(flet ((find-pixmap-size ()
		 ;;-- Is there a better way???
		 (do ((power 2 (* power 2)))
		     (nil)
		   (let ((columns (truncate (/ power width)))
			 (rows (truncate (/ power height))))
		     (when  (>= (* columns rows) nchars)
		       (return (values power columns rows)))))))
	  (multiple-value-bind
	      (n columns rows)
	      (find-pixmap-size)
	    (let* ((pixmap (make-instance 'tk::pixmap
					  :drawable
					  (xt::display-root-window
					   (port-display port))
					  :depth 1
					  :width n
					  :height n))
		   (gc (make-instance 'tk::gcontext
				      :drawable pixmap
				      :foreground 1
				      :background 0)))
	      ;; Draw all the characters
	      (setf (tk::gcontext-foreground gc) 0)
	      (tk::draw-rectangle pixmap gc 0 0 n n t)
	      (setf (tk::gcontext-foreground gc) 1)
	      (do ((row 0)
		   (col 0)
		   (string (make-string 1))
		   (char min-char (1+ char)))
		  ((> char max-char))
		(when (= col columns)
		  (setq col 0 row (1+ row)))
		(setf (schar string 0) 
		  (cltl1:int-char char))
		(tk::draw-string pixmap gc 
				 (* col width) 
				 (+ (* row height) ascent)
				 string)
		(incf col))
	      (rotate-pixmap pixmap rotation)
	      (values-list
	       (cdr (car
		     (push (list* (list font rotation)
				  pixmap
				  (list columns width (- n (* columns width))
					rows height (- n (* rows height))))
			   (port-rotated-font-cache port))))))))))))

(defun rotate-pixmap  (source-pixmap n)
  ;; original code was in smalltalk from april 1981 byte magazine.
  ;; this code is mostly from a symbolics lispm hack:
  ;;   created 11/24/81 by cmb
  ;;   modified, 1/9/82 by dlw
  ;;   converted to clim, 19mar92 by nlc.
  ;; the bit array must be square and a power of two bits on a side.
  (let* ((array-size (xt::pixmap-width source-pixmap))
	 (mask-pixmap (make-instance 'tk::pixmap
		       :drawable source-pixmap
		       :width array-size
		       :height array-size
		       :depth 1))
	 (temp-pixmap (make-instance 'tk::pixmap
		       :drawable source-pixmap
		       :width array-size
		       :height array-size
		       :depth 1))
	 (gc (make-instance 'tk::gcontext :drawable source-pixmap)))
    (dotimes (i n)
      (macrolet ((copy-all-to (from xoffset yoffset to alu)
		   `(stream-bitblt-support gc
					   ,from  0 0
					   ,to ,xoffset ,yoffset
					   (- array-size ,xoffset) (- array-size ,yoffset)
					   :function ,alu))
		 (copy-all-from (to xoffset yoffset from alu)
		   `(stream-bitblt-support gc
					   ,from ,xoffset ,yoffset
					   ,to 0 0
					   (- array-size ,xoffset) (- array-size ,yoffset)
					   :function ,alu)))
	(copy-all-to mask-pixmap 0 0 mask-pixmap boole-clr)
	(copy-all-from mask-pixmap (/ array-size 2) (/ array-size 2)
		       mask-pixmap boole-set)
	(do ((quad (/ array-size 2) (/ quad 2)))
	    ((< quad 1))
	  (copy-all-to mask-pixmap 0 0 temp-pixmap boole-1) ; 1        
	  (copy-all-to mask-pixmap 0 quad temp-pixmap boole-ior) ; 2
	  (copy-all-to source-pixmap 0 0 temp-pixmap boole-and) ; 3
	  (copy-all-to temp-pixmap 0 0 source-pixmap boole-xor) ; 4
	  (copy-all-from temp-pixmap quad 0 source-pixmap boole-xor) ; 5
	  (copy-all-from source-pixmap quad 0 source-pixmap boole-ior) ; 6
	  (copy-all-to temp-pixmap quad 0 source-pixmap boole-xor) ; 7
	  (copy-all-to source-pixmap 0 0 temp-pixmap boole-1) ; 8
	  (copy-all-from temp-pixmap quad quad source-pixmap boole-xor) ; 9
	  (copy-all-to mask-pixmap 0 0 temp-pixmap boole-and) ; 10
	  (copy-all-to temp-pixmap 0 0 source-pixmap boole-xor) ; 11
	  (copy-all-to temp-pixmap quad quad source-pixmap boole-xor) ; 12
	  (copy-all-from mask-pixmap (floor quad 2) (floor quad 2)
			 mask-pixmap boole-and) ;13
	  (copy-all-to mask-pixmap quad 0 mask-pixmap boole-ior) ; 14
	  (copy-all-to mask-pixmap 0 quad mask-pixmap boole-ior) ; 15
	  )))))

(defun stream-bitblt-support (gc
			      from from-left from-top
			      to to-left to-top
			      width height &key function)
  (setf (tk::gcontext-function gc) 
    (or function boole-1))
  (tk::copy-area from
		 gc
		 from-left from-top
		 width height
		 to
		 to-left to-top))



;;--- Is this used any more?
(defmethod port-write-string-1 ((port xt-port) medium
				glyph-buffer start end 
				x-font color x y)
  (break "is this used anymore")
  (unless (= start end)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (window (medium-drawable medium))
	   (font x-font))
      ;; At one point we checked to see whether the widget is unrealized
      ;; or not.  Can we draw on disabled sheets?
      (convert-to-device-coordinates transform x y)
      (incf y (tk::font-ascent font))
      (when (medium-drawable medium)
	(let ((gc (decode-ink (medium-ink medium) medium)))
	  (setf (tk::gcontext-font gc) font)
	  (etypecase glyph-buffer
	    ((simple-array (unsigned-byte 16))
	     (x11::xdrawstring16
	       (tk::object-display window)
	       window
	       gc
	       x y
	       glyph-buffer (- end start)))))))))


(defmethod port-beep ((port xt-port) (sheet t))
  (x11:xbell (port-display port) 100))

   

;;--- FROM-SHEET and TO-SHEET should be mediums, not sheets
;;--- This needs to be able to copy to/from pixmaps, too
;;--- arglist s/b FROM-MEDIUM FROM-X FROM-Y TO-MEDIUM TO-X TO-Y WIDTH HEIGHT
(defmethod port-copy-area ((port xt-port)
			   from-sheet to-sheet
			   from-left from-top from-right from-bottom
			   to-left to-top)
  ;; coords in "host" coordinate system
  (let ((transform (sheet-native-transformation from-sheet)))
    (convert-to-device-coordinates transform
       from-left from-top from-right from-bottom to-left to-top)
    (with-sheet-medium (from-medium from-sheet)
      (with-sheet-medium (to-medium to-sheet)
	(let* ((from-drawable (medium-drawable from-medium))
	       (to-drawable (medium-drawable to-medium))
	       (width (- from-right from-left))
	       (height (- from-bottom from-top))
	       (copy-gc (port-copy-gc port)))
	  (when (and from-drawable to-drawable)
	    (with-port-event-lock (port)
	      (let ((seq-no 0))
		(without-interrupts
		  (setq seq-no (x11:xnextrequest (port-display port)))
		  (tk::copy-area from-drawable copy-gc from-left from-top
				 width height to-drawable to-left to-top))
		(let ((event
		       (tk::get-event-matching-sequence-and-types
			to-drawable seq-no
			'(:graphics-expose :no-expose))))
		  (case (tk::event-type event)
		    (:no-expose
		     nil)
		    (:graphics-expose
		     (loop
		       (let* ((minx (x11::xexposeevent-x event))
			      (miny (x11::xexposeevent-y event))
			      (width (x11::xexposeevent-width event))
			      (height (x11::xexposeevent-height event))
			      (maxx (+ minx width))
			      (maxy (+ miny height)))
			 (dispatch-repaint
			  to-sheet
			  (make-instance 'window-repaint-event
			    :native-region (make-bounding-rectangle minx miny maxx maxy)
			    :region (untransform-region
				     (sheet-native-transformation to-sheet)
				     (make-bounding-rectangle minx miny maxx maxy))
			    :sheet to-sheet)))
		       (setq event (tk::get-event-matching-sequence-and-types
				    to-drawable seq-no '(:graphics-expose) 
				    :block nil))
		       (unless event
			 (return))))))))))))))
