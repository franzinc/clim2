;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: genera-medium.lisp,v 1.4 92/05/07 13:13:26 cer Exp $

(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defclass genera-medium (medium)
    ((window :initform nil)
     (ink-cache :initform (make-ink-cache 16))))

#||
(defclass basic-genera-display-medium (basic-genera-medium display-medium)
    ((window-system-clipping-region :initform (list 0 0 0 0))))

(defclass basic-genera-pixmap-medium (basic-genera-medium pixmap-medium)
    ((associated-medium :initarg :associated-medium)))
||#

(defmethod make-medium ((port genera-port) sheet)
  (make-instance 'genera-medium
		 :port port
		 :sheet sheet))

(defmethod deallocate-medium :after (port (medium genera-medium))
  (declare (ignore port))
  (with-slots (window) medium
    (setf window nil)))

(defmethod engraft-medium :after ((medium genera-medium) (port genera-port) sheet)
  (with-slots (window) medium
    (unless window
      (setq window (sheet-mirror sheet)))
    (scl:send window :set-char-aluf
	      (genera-decode-color (medium-foreground medium) medium nil))
    (scl:send window :set-erase-aluf
	      (genera-decode-color (medium-background medium) medium nil))))

(defmethod degraft-medium :after ((medium genera-medium) (port genera-port) sheet)
  (declare (ignore sheet))
  ;;--- Deallocate the stuff in the ink cache
  )

(defmethod (setf medium-foreground) :after (ink (medium genera-medium))
  (with-slots (window) medium	   
    (scl:send window :set-char-aluf (genera-decode-color ink medium nil))
    (scl:send window :refresh)))

(defmethod (setf medium-background) :after (ink (medium genera-medium))
  (with-slots (window) medium	   
    (scl:send window :set-erase-aluf (genera-decode-color ink medium nil))
    (scl:send window :refresh)))


;;; Translation between CLIM and Genera graphics model

;;; We cache the translation of the most recently used inks in a small array,
;;; using a property-list masquerade for quick lookup.  When a cache element
;;; is replaced, any temporary resources used by the translation are returned
;;; (temporary rasters are currently the only examples).
(defun make-ink-cache (size)
  (let ((array (make-array size :fill-pointer t)))
    (setf (sys:%p-cdr-code (scl:locf (aref array (1- size)))) sys:cdr-nil)
    (values array)))

(defun-inline ink-cache-lookup (cache ink)
  (getf (sys:%make-pointer sys:dtp-list (scl:locf (aref cache 0))) ink))

(defun ink-cache-replace (cache ink value)
  (let ((index (fill-pointer cache)))
    (when (<= (array-total-size cache) index)
      (setq index 0))
    ;; Deallocate the previous temporary rasters
    (let ((value (aref cache (1+ index))))
      (when (listp value)
	(dolist (element value)
	  (when (sixth element)
	    (tv:deallocate-temporary-sheet-bit-raster (first element))))))
    (setf (aref cache index) ink)
    (setf (aref cache (1+ index)) value)
    (setf (fill-pointer cache) (+ index 2))
    value))

(defun-inline follow-indirect-ink (ink medium)
  (cond ((eq ink +foreground-ink+) (medium-foreground medium))
	((eq ink +background-ink+) (medium-background medium))
	((eq ink +everywhere+) (medium-foreground medium))
	(t ink)))

(defmethod genera-decode-color (ink medium &optional (stipple-p t))
  (declare (ignore stipple-p medium))
  (error "Expected ~S to be a color.  This may be due to an implementation limitation." ink))

(defun genera-decode-luminance (luminance stipple-p)
  (macrolet ((stipple (height width patterns)
	       `(quote ((,(graphics::make-stipple height width patterns)
			 ,boole-set ,boole-clr)))))
    (if (not stipple-p)
	(if (< luminance 0.5) boole-set boole-clr)
      (sys:selector luminance <
	(0.05 boole-set)
	(0.1 (stipple 8 16 #b(0111111111111111
			      1111110111111111
			      1111111111110111
			      1101111111111111
			      1111111101111111
			      1111111111111101
			      1111011111111111
			      1111111111011111)))
	(0.2 (stipple 8 8 #b(01111111
			     11101111
			     11111101
			     10111111
			     11110111
			     11111110
			     11011111
			     11111011)))
	(0.3 (stipple 4 4 #b(0111 1101 1011 1110)))
	(0.4 (stipple 3 3 #b(011 101 110)))
	(0.6 (stipple 2 2 #b(01 10)))
	(0.7 (stipple 3 3 #b(100 010 001)))
	(0.8 (stipple 4 4 #b(1000 0010 0100 0001)))
	(0.9 (stipple 8 8 #b(10000000
			     00010000
			     00000010
			     01000000
			     00001000
			     00000001
			     00100000
			     00000100)))
	(0.95 (stipple 8 16 #b(1000000000000000
			       0000001000000000
			       0000000000001000
			       0010000000000000
			       0000000010000000
			       0000000000000010
			       0000100000000000
			       0000000000100000)))
	(otherwise boole-clr)))))

(defvar *genera-opacity-stipples*
	(mapcar #'(lambda (entry)
		    (cons (first entry) (apply #'graphics::make-stipple (second entry))))
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

(defun genera-decode-opacity (opacity)
  (let ((opacities *genera-opacity-stipples*))
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

(defmethod genera-decode-color ((ink gray-color) medium &optional (stipple-p t))
  (let ((window (slot-value medium 'window))
	(color-p (slot-value (port medium) 'color-p)))
    (let ((luminance (slot-value ink 'clim-utils::luminance))
	  (invert-p (not (funcall (tv:sheet-screen window) :bow-mode))))
      (cond ((= luminance 0.0) (if invert-p boole-clr boole-set))
	    ((= luminance 1.0) (if invert-p boole-set boole-clr))
	    ((not color-p)
	     (genera-decode-luminance (if invert-p (- 1.0 luminance) luminance) stipple-p))
	    (t
	     (funcall (tv:sheet-screen window) :compute-rgb-alu boole-1
		      luminance luminance luminance (not invert-p)))))))

(defmethod genera-decode-color ((ink color) medium &optional (stipple-p t))
  (let ((window (slot-value medium 'window))
	(color-p (slot-value (port medium) 'color-p)))
    (multiple-value-bind (r g b)
	(color-rgb ink)
      (let ((invert-p (not (funcall (tv:sheet-screen window) :bow-mode))))
	(if (not color-p)
	    (let ((luminance (color-luminosity r g b)))
	      (genera-decode-luminance (if invert-p (- 1.0 luminance) luminance) stipple-p))
	  (funcall (tv:sheet-screen window) :compute-rgb-alu boole-1
		   r g b (not invert-p)))))))

(defmethod genera-decode-color ((ink flipping-ink) medium &optional (stipple-p t))
  (declare (ignore stipple-p))
  (let ((window (slot-value medium 'window))
	(color-p (slot-value (port medium) 'color-p)))
    (if (not color-p)
	(values boole-xor)
      (multiple-value-bind (design1 design2)
	  (decode-flipping-ink ink)
	(let ((ink1 (follow-indirect-ink design1 medium))
	      (ink2 (follow-indirect-ink design2 medium)))
	  ;; Genera substrate can get confused about this...
	  (if (or (and (eq ink1 +black+) (eq ink2 +white+))
		  (and (eq ink1 +white+) (eq ink2 +black+)))
	      (values boole-xor)
	    (values
	      (funcall (tv:sheet-screen window) :exchange-two-colors-aluf
		       (genera-decode-color (follow-indirect-ink design1 medium) medium)
		       (genera-decode-color (follow-indirect-ink design2 medium) medium)))))))))

(defmethod genera-decode-color ((ink standard-opacity) medium &optional (stipple-p t))
  (declare (ignore medium stipple-p))
  (if (> (slot-value ink 'clim-utils::value) 0.5) boole-1 boole-2))

(defun genera-decode-raster (raster &optional width height key tiled-p)
  (multiple-value-bind (swidth sheight)
      (sys:decode-raster-array raster)
    (let ((dwidth (if tiled-p (lcm width 32) (* (ceiling (1- width) 32) 32))))
      (if (and (= swidth dwidth)
	       (= (sys:array-type-field raster) sys:art-1b))
	  (values raster)
	(let ((source raster)
	      (destination (tv:make-temporary-sheet-bit-raster #*1010 dwidth height)))
	  (declare (sys:array-register-1d source destination))
	  ;; If the tiling is going to leave gaps, we need to zero the destination first
	  (when (or (/= width swidth) (/= height sheight))
	    (si:fill-array destination nil 0))
	  (do ((i 0 (1+ i))
	       (si 0 (+ si swidth))
	       (di 0 (+ di dwidth)))
	      ((= i (min height sheight)))
	    (do ((offset 0 (+ offset width)))
		((>= offset dwidth))
	      (let* ((s si)
		     (d (+ di offset))
		     (w (min (- dwidth offset) swidth)))
		(cond ((null key)
		       (sys:copy-array-portion source s (+ s w)
					       destination d (+ d w)))
		      ((integerp key)
		       (repeat w
			 (setf (aref destination d) (if (= (aref source s) key) 1 0))
			 (incf s)
			 (incf d)))))))
	  (values destination))))))

(defmethod genera-decode-pattern ((ink pattern) medium &optional width height tiled-p)
  (multiple-value-bind (array designs)
      (decode-pattern ink)
    (let ((width (or width (array-dimension array 1)))
	  (height (or height (array-dimension array 0))))
      (if (= (length designs) 2)
	  (let ((raster (genera-decode-raster array width height nil tiled-p)))
	    (list
	      (list raster
		    (genera-decode-color
		      (follow-indirect-ink (aref designs 1) medium) medium nil)
		    (genera-decode-color
		      (follow-indirect-ink (aref designs 0) medium) medium nil)
		    (and (not tiled-p) width)
		    (and (not tiled-p) height)
		    (not (eq raster array)))))
	(do* ((result (make-list (length designs)))
	      (i 0 (1+ i))
	      (l result (cdr l)))
	     ((null l) result)
	  (setf (car l) (list (genera-decode-raster array width height i tiled-p)
			      (genera-decode-color
				(follow-indirect-ink (aref designs i) medium) medium nil)
			      (if (zerop i) boole-clr boole-2)
			      (and (not tiled-p) width)
			      (and (not tiled-p) height)
			      t)))))))

(defmethod genera-decode-pattern ((ink design) medium &optional width height tiled-p)
  (declare (ignore medium width height tiled-p))
  (error "Arbitrary patterned designs are not supported in this CLIM implementation"))

(defmethod genera-decode-pattern ((ink color) medium &optional width height tiled-p)
  (declare (ignore medium width height tiled-p))
  (error "A pattern must be a bounded design, ~S isn't" ink))

(defmethod genera-decode-ink ((ink color) medium)
  (genera-decode-color ink medium))

(defmethod genera-decode-ink ((ink flipping-ink) medium)
  (genera-decode-color ink medium))

(defmethod genera-decode-ink ((ink contrasting-ink) medium)
  (if (slot-value (port medium) 'color-p)
      (genera-decode-color (make-color-for-contrasting-ink ink) medium)
    ;; More efficient than (GENERA-DECODE-COLOR (MAKE-GRAY-COLOR-FOR-CONTRASTING-INK INK))
    (let ((luminance (with-slots (clim-utils::which-one clim-utils::how-many) ink
		       (/ clim-utils::which-one clim-utils::how-many)))
	  (invert-p (not (funcall (tv:sheet-screen (slot-value medium 'window)) :bow-mode))))
      (cond ((= luminance 0.0) (if invert-p boole-clr boole-set))
	    ((= luminance 1.0) (if invert-p boole-set boole-clr))
	    (t
	     (genera-decode-luminance (if invert-p (- 1.0 luminance) luminance) t))))))

(defmethod genera-decode-ink ((ink pattern) medium)
  (genera-decode-pattern ink medium))

(defmethod genera-decode-ink ((ink rectangular-tile) medium)
  (multiple-value-bind (pattern width height)
      (decode-rectangular-tile ink)
    (genera-decode-pattern pattern medium width height t)))

(defmethod genera-decode-ink ((ink stencil) medium)
  (declare (ignore medium))
  (error "Stencils and opacities are not supported in this CLIM implementation"))

(defmethod genera-decode-ink ((ink composite-in) medium)
  (declare (ignore medium))
  (error "Compositing is not supported in this CLIM implementation"))

(defmethod genera-decode-ink ((ink composite-out) medium)
  (declare (ignore medium))
  (error "Compositing is not supported in this CLIM implementation"))
      

(flavor:defgeneric with-clim-drawing-state (drawing-state ones-alu zeros-alu
					    thickness dashes line-style pattern
					    continuation &rest arguments)
  (:inline-methods :recursive)
  (:optimize speed))

(flavor:defmethod (with-clim-drawing-state graphics::raster-drawing-state :before)
		  (ones-alu zeros-alu thickness dashes line-style pattern
		   continuation &rest arguments)
  (declare (ignore thickness dashes line-style continuation arguments))
  (setq graphics::source-pattern pattern)
  (setq graphics::ones-alu ones-alu)
  (setq graphics::zeros-alu zeros-alu))

(flavor:defmethod (with-clim-drawing-state graphics::drawing-state)
		  (ones-alu zeros-alu thickness dashes line-style pattern
		   continuation &rest arguments)
  (declare (ignore zeros-alu))
  (setq graphics::alu ones-alu)
  (setq graphics::thickness thickness)
  (setq graphics::scale-thickness nil)
  (when (not (null line-style))
    (setq graphics::dashed (not (null dashes)))
    (setq graphics::dash-pattern dashes)
    (setq graphics::line-end-shape (line-style-cap-shape line-style))
    (setq graphics::line-joint-shape (line-style-joint-shape line-style)))
  (setq graphics::stipple pattern)
  (setq graphics::scan-conversion-flags
	(dpb 1 graphics::%%scan-conversion-host-allowed
	     (dpb 1 graphics::%%scan-conversion-round-coordinates 0)))
  #+IMach
  ;; MacIvories are wierd
  (when (sys:system-case (:macivory t) (otherwise nil))
    (setq graphics::color nil graphics::gray-level 1)
    (when (not (null pattern))
      (if (let ((width (array-dimension pattern 1))
		(height (array-dimension pattern 0)))
	    (and (<= height 8)
		 (or (= width 32) (<= width 8))))
	  (setq graphics::stipple pattern)
	(setf graphics::scan-conversion-flags
	      (dpb 0 graphics::%%scan-conversion-host-allowed
		   (dpb 1 graphics::%%scan-conversion-round-coordinates 0))))))
  (setf (ldb graphics::%%drawing-state-new-parameters graphics::flags) 0)
  (apply continuation arguments))

;;; The easy continuation is called for simple drawing operations without patterning
;;; or line style options unsupported by the old Genera window system operations.
(defmethod with-appropriate-drawing-state
	   ((medium genera-medium) ink line-style easy hard)
  (let ((alu (with-slots (ink-cache) medium
	       (let ((ink (follow-indirect-ink ink medium)))
		 (or (ink-cache-lookup ink-cache ink)
		     (ink-cache-replace ink-cache ink (genera-decode-ink ink medium)))))))
    (multiple-value-bind (thickness dashes)
	(if (null line-style)
	    (values 0 nil)
	  (values (or (line-style-thickness line-style) 0)
		  (let ((dashes (line-style-dashes line-style)))
		    (if (eq dashes t) #(10 10) dashes))))
      ;; There's no way to get the ALU canonicalization done in a whopper or wrapper
      ;; without incurring extraordinary performance penalties, so...
      (macrolet ((with-drawing-state-kludge (drawing-state ones-alu zeros-alu
					     thickness dashes line-style raster
					     continuation window)
		   `(multiple-value-bind (ones-alu zeros-alu)
			(if (not (and (integerp ,ones-alu) (integerp ,zeros-alu)))
			    (values ,ones-alu ,zeros-alu)
			  ;; This is boolean magic to construct a new alu code which
			  ;; combines the effects of doing a DRAW-xx with ONES-ALU
			  ;; everywhere there is a 1 in the source and that of doing a
			  ;; DRAW-xx with ZEROS-ALU everywhere there is a 0 in the
			  ;; source.
			  (values (logior (logand ,ones-alu #b0101)
					  (scl:rot (logand ,zeros-alu #b0101) 1))
				  ,zeros-alu))
		      (with-clim-drawing-state ,drawing-state ones-alu zeros-alu
					       ,thickness ,dashes ,line-style ,raster
					       ,continuation ,window))))
	(labels ((kernel (window)
		   (declare (sys:downward-function))
		   (if (listp alu)
		       (dolist (element alu)
			 (sys:destructuring-bind (raster ones-alu zeros-alu) element
			   (with-drawing-state-kludge (graphics::get-drawing-state window)
						      ones-alu zeros-alu
						      thickness dashes line-style raster
						      hard window)))
		     (if (and easy
			      (not (slot-value (port medium) 'embedded-p))
			      (null dashes)
			      (<= thickness 1))
			 (funcall easy window alu)
		       (with-drawing-state-kludge (graphics::get-drawing-state window)
						  alu boole-2
						  thickness dashes line-style nil
						  hard window)))))
	  (with-slots (window) medium
	    (let* ((sheet (medium-sheet medium))
		   (region (sheet-device-region sheet))
		   (medium-region (medium-clipping-region medium)))
	      (unless (eq medium-region +everywhere+)
		(setq region (region-intersection
			       region
			       (transform-region 
				 (sheet-device-transformation sheet) medium-region))))
	      (if (or (eq region +everywhere+)
		      (eq region +nowhere+))	;---yow! howcum?
		  (kernel window)
		(with-bounding-rectangle* (left top right bottom) region
		  (fix-coordinates left top right bottom)
		  (with-stack-list (cr (+ (tv:sheet-left-margin-size window) left)
				       (+ (tv:sheet-top-margin-size window) top)
				       (+ (tv:sheet-right-margin-size window) right)
				       (+ (tv:sheet-bottom-margin-size window) bottom))
		    (scl:letf (((tv:sheet-clipping-region window) cr))
		      (kernel window)))))))))))) 

(defmethod medium-drawing-possible ((medium genera-medium))
  (let ((window (slot-value medium 'window)))
    ;; TV:SHEET-OUTPUT-HELD-P, except that temp-locking only delays drawing,
    ;; it doesn't make it impossible, so only look at the output-hold flag
    (or (zerop (tv:sheet-output-hold-flag window))
	(tv:sheet-screen-array window))))

(defmethod port-draw-point* ((port genera-port) sheet medium x y)
  (when (medium-drawing-possible medium)
    (let ((transform (sheet-device-transformation sheet))
	  (ink (medium-ink medium))
	  (line-style (medium-line-style medium)))
      (convert-to-device-coordinates transform x y)
      (with-appropriate-drawing-state medium ink line-style
	#'(lambda (window alu)
	    (declare (sys:downward-function))
	    (funcall window :draw-point x y alu))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (funcall (flavor:generic graphics:draw-point) window x y))))))

(defmethod port-draw-line* ((port genera-port) sheet medium x1 y1 x2 y2)
  (when (medium-drawing-possible medium)
    (let ((transform (sheet-device-transformation sheet))
	  (ink (medium-ink medium))
	  (line-style (medium-line-style medium)))
      (convert-to-device-coordinates transform
	x1 y1 x2 y2)
      (with-appropriate-drawing-state medium ink line-style
        #'(lambda (window alu)
	    (declare (sys:downward-function))
	    (funcall window :draw-line x1 y1 x2 y2 alu t))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (funcall (flavor:generic graphics:draw-line)
		     window x1 y1 x2 y2 ))))))

(defmethod port-draw-rectangle* ((port genera-port) sheet medium 
				 left top right bottom filled)
  (when (medium-drawing-possible medium)
    (let ((transform (sheet-device-transformation sheet))
	  (ink (medium-ink medium))
	  (line-style (medium-line-style medium)))
      (convert-to-device-coordinates transform
	left top right bottom)
      (if (and filled
	       (typep ink 'pattern))
	  ;; Looks like this is DRAW-ICON*
	  (draw-icon-internal medium left top right bottom ink)
	  (with-appropriate-drawing-state medium ink line-style
	    #'(lambda (window alu)
		(declare (sys:downward-function))
		(if filled
		    (let ((width (abs (- right left)))
			  (height (abs (- bottom top))))
		      (funcall window :draw-rectangle width height left top alu))
		    (scl:stack-let ((lines (vector right top left top
						   left top left bottom
						   left bottom right bottom
						   right bottom right top)))
		      (funcall window :draw-multiple-lines lines alu nil))))
	    #'(lambda (window)
		(declare (sys:downward-function))
		(funcall (flavor:generic graphics:draw-rectangle) window left top right bottom
			 :filled filled)))))))

;; Fall back to DRAW-IMAGE.  INK will be a pattern.
;;--- We have to resort to this because Genera always tiles.  Sigh.
(defmethod draw-icon-internal ((medium genera-medium) left top right bottom ink)
  (with-slots (window ink-cache) medium
    (let* ((width  (- right left))
	   (height (- bottom top))
	   (alus (or (ink-cache-lookup ink-cache ink)
		     (ink-cache-replace ink-cache ink (genera-decode-ink ink medium)))))
      (assert (= (length alus) 1) ()
	      "CLIM only supports bitmap icons under Genera sheets")
      (dolist (alu alus)
	(let ((image (pop alu)))
	  (graphics:draw-image image left top
			       :image-right width :image-bottom height
			       :stream window))))))

(defmethod port-draw-polygon* ((port genera-port) sheet medium points closed filled)
  (when (medium-drawing-possible medium)
    (let ((transform (sheet-device-transformation sheet))
	  (ink (medium-ink medium))
	  (line-style (medium-line-style medium))
	  (npoints (length points)))
      (with-appropriate-drawing-state medium ink line-style
	#'(lambda (window alu)
	    (declare (sys:downward-function))
	    (if filled
		(case npoints
		  (6 (let* ((p points)
			    (x1 (pop p)) (y1 (pop p))
			    (x2 (pop p)) (y2 (pop p))
			    (x3 (pop p)) (y3 (pop p)))
		       (convert-to-device-coordinates transform
			 x1 y1 x2 y2 x3 y3)
		       (funcall window :draw-triangle x1 y1 x2 y2 x3 y3 alu)))
		  (8 (let* ((p points)
			    (x1 (pop p)) (y1 (pop p))
			    (x2 (pop p)) (y2 (pop p))
			    (x3 (pop p)) (y3 (pop p))
			    (x4 (pop p)) (y4 (pop p)))
		       (convert-to-device-coordinates transform
			 x1 y1 x2 y2 x3 y3 x4 y4)
		       (funcall window :draw-triangle x1 y1 x2 y2 x3 y3 alu)
		       (funcall window :draw-triangle x3 y3 x4 y4 x1 y1 alu)))
		  (otherwise
		    (with-stack-array (translated-points npoints)
		      (do ((i 0)
			   (p points))
			  ((null p))
			(let* ((x (pop p))
			       (y (pop p)))
			  (convert-to-device-coordinates transform x y)
			  (setf (aref translated-points (shiftf i (1+ i))) x)
			  (setf (aref translated-points (shiftf i (1+ i))) y)))
		      (graphics::triangulate-polygon
			#'(lambda (x1 y1 x2 y2 x3 y3)
			    (funcall window :draw-triangle x1 y1 x2 y2 x3 y3 alu))
			translated-points))))
		(with-stack-array (lines (* (+ npoints (if closed 0 -2)) 2))
		  (let* ((p points)
			 (i 0)
			 (initial-x (pop p))
			 (initial-y (pop p))
			 (x initial-x)
			 (y initial-y))
		    (convert-to-device-coordinates transform 
		      initial-x initial-y x y)
		    (loop
		      (setf (aref lines (shiftf i (1+ i))) x)
		      (setf (aref lines (shiftf i (1+ i))) y)
		      (setq x (pop p)
			    y (pop p))
		      (convert-to-device-coordinates transform x y)
		      (setf (aref lines (shiftf i (1+ i))) x)
		      (setf (aref lines (shiftf i (1+ i))) y)
		      (when (null p)
			(when closed
			  (setf (aref lines (shiftf i (1+ i))) x)
			  (setf (aref lines (shiftf i (1+ i))) y)
			  (setf (aref lines (shiftf i (1+ i))) initial-x)
			  (setf (aref lines (shiftf i (1+ i))) initial-y))
			(return))))
		  (funcall window :draw-multiple-lines lines alu nil))))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (with-stack-array (translated-points npoints)
	      (do ((i 0)
		   (p points))
		  ((null p))
		(let* ((x (pop p))
		       (y (pop p)))
		  (convert-to-device-coordinates transform x y)
		  (setf (aref translated-points (shiftf i (1+ i))) x)
		  (setf (aref translated-points (shiftf i (1+ i))) y)))
	      (if (null line-style)
		  (funcall (flavor:generic graphics:draw-polygon) window translated-points
			   :filled t)
		  (funcall (flavor:generic graphics:draw-lines) window translated-points
			   :closed closed))))))))

(defmethod port-draw-ellipse* ((port genera-port) sheet medium
			       center-x center-y
			       radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			       start-angle end-angle filled)
  (when (medium-drawing-possible medium)
    (let ((transform (sheet-device-transformation sheet))
	  (ink (medium-ink medium))
	  (line-style (medium-line-style medium)))
      (convert-to-device-coordinates transform center-x center-y)
      (convert-to-device-distances transform 
	radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
      (when (and start-angle
		 (= start-angle 0.0)
		 (= end-angle 2pi))
	;; GRAPHICS:DRAW-CIRCLE draws nothing when given single-precision 2PI
	(setq end-angle graphics:2pi))
      (multiple-value-bind (pre-stretch-angle x-radius y-radius axis-rotate-angle)
	  (2x2-singular-value-decomposition radius-1-dx radius-2-dx
					    radius-1-dy radius-2-dy)
	(declare (ignore pre-stretch-angle))
	(setq x-radius (abs x-radius) y-radius (abs y-radius))
	(if (and (= x-radius y-radius) (null start-angle))
	    ;; It's a complete circle
	    (with-appropriate-drawing-state medium ink line-style
	      #'(lambda (window alu)
		  (declare (sys:downward-function))
		  (if filled
		      (funcall window :draw-filled-in-circle center-x center-y x-radius alu)
		      (funcall window :draw-circle center-x center-y x-radius alu)))
	      #'(lambda (window)
		  (declare (sys:downward-function))
		  (funcall (flavor:generic graphics:draw-ellipse) window
			   center-x center-y x-radius y-radius
			   :filled filled)))
	  ;; For general ellipse, let Genera do all the work
	  (with-appropriate-drawing-state medium ink line-style
	    nil
	    #'(lambda (window)
		(declare (sys:downward-function))
		(graphics:saving-graphics-transform (window)
		  (graphics:graphics-translate center-x center-y :stream window)
		  (when (/= axis-rotate-angle 0)
		    (graphics:graphics-rotate axis-rotate-angle :stream window))
		  (funcall (flavor:generic graphics:draw-ellipse) window
			   0 0 x-radius y-radius
			   :start-angle (or start-angle 0)
			   :end-angle (or end-angle graphics:2pi)
			   :filled filled)))))))))

(defmethod port-draw-string* ((port genera-port) sheet medium
			      string x y start end align-x align-y
			      towards-x towards-y transform-glyphs)
  (when (medium-drawing-possible medium)
    (let ((transform (sheet-device-transformation sheet))
	  (ink (medium-ink medium))
	  (text-style (medium-merged-text-style medium)))
      (convert-to-device-coordinates transform x y)
      (when towards-x
	(convert-to-device-coordinates transform towards-x towards-y))
      (unless start
	(setq start 0))
      (unless end
	(setq end (length string)))
      ;; ---OK to use this in the Genera implementation
      (sys:stack-let ((substring (make-string (- end start))))
	(replace substring string :start2 start :end2 end)
	(let* ((font (text-style-mapping 
		       port text-style *standard-character-set*
		       (slot-value medium 'window)))
	       (height (sys:font-char-height font))
	       (descent (- height (sys:font-baseline font)))
	       (ascent (- height descent)))
	  (let ((x-adjust (clim-internals::compute-text-x-adjustment 
			    align-x medium string text-style start end))
		(y-adjust (clim-internals::compute-text-y-adjustment
			    align-y descent ascent height)))
	    (incf x x-adjust)
	    (incf y y-adjust)
	    (when towards-x
	      (incf towards-x x-adjust)
	      (incf towards-y y-adjust)))
	  (with-stack-list (style :device-font (tv:font-name font) :normal)
	    (with-appropriate-drawing-state medium ink nil
	      #'(lambda (window alu)
		  (declare (sys:downward-function))
		  (funcall window :draw-string substring x y (1+ x) y nil style alu))
	      #'(lambda (window)
		  (declare (sys:downward-function))
		  (funcall (flavor:generic graphics:draw-string) window
			   substring x y :character-style style)))))))))

(defmethod port-draw-character* ((port genera-port) sheet medium
				 character x y align-x align-y
				 towards-x towards-y transform-glyphs)
  (when (medium-drawing-possible medium)
    (let ((transform (sheet-device-transformation sheet))
	  (ink (medium-ink medium))
	  (text-style (medium-merged-text-style medium)))
      (convert-to-device-coordinates transform x y)
      (when towards-x
	(convert-to-device-coordinates transform towards-x towards-y))
      (let* ((font (text-style-mapping 
		     port text-style *standard-character-set*
		     (slot-value medium 'window)))
	     (height (sys:font-char-height font))
	     (descent (- height (sys:font-baseline font)))
	     (ascent (- height descent)))
	(let ((x-adjust (clim-internals::compute-text-x-adjustment 
			  align-x medium character text-style))
	      (y-adjust (- (clim-internals::compute-text-y-adjustment
			     align-y descent ascent height)
			   ascent)))
	  (incf x x-adjust)
	  (incf y y-adjust)
	  (when towards-x
	    (incf towards-x x-adjust)
	    (incf towards-y y-adjust)))
	(with-appropriate-drawing-state medium ink nil
	  #'(lambda (window alu)
	      (declare (sys:downward-function))
	      (funcall window :draw-glyph (char-code character) font x y alu))
	  #'(lambda (window)
	      (declare (sys:downward-function))
	      (funcall (flavor:generic graphics:draw-glyph) window
		       (char-code character) font x y))))))) 

(defmethod port-draw-text* ((port genera-port) sheet medium
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

(defmethod port-beep ((port genera-port) sheet)
  (scl:beep sheet))


;;--- FROM-SHEET and TO-SHEET should be mediums, not sheets
;;--- This needs to be able to copy to/from pixmaps, too
;;--- arglist s/b FROM-MEDIUM FROM-X FROM-Y TO-MEDIUM TO-X TO-Y WIDTH HEIGHT
(defmethod port-copy-area ((port genera-port)
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
	(let ((window (slot-value medium 'window)))
	  (scl:send window :bitblt-within-sheet
			   tv:alu-seta width height
			   from-left from-top
			   to-left to-top))))))


#||
;;; Copy from a pixmap into a medium.
(defmethod copy-area ((medium clg-medium) x y (source pixmap) left bottom width height
		      &optional (boole boole-1))
  (with-slots (drawable port device-transformation) medium
    (multiple-value-bind (min-x min-y max-x max-y)
	(transform-rectangle* device-transformation x y (+ x width) (+ y height))
      (scl:send drawable :bitblt boole (- max-x min-x) (- max-y min-y)
		(realize-pixmap port source) left bottom
		min-x min-y))))

;;; Copy from a medium into another medium.
;;; --- Should there be a COPY-AREA from ON-GENERA::CLG-DISPLAY-MEDIUM to ON-X:???
(defmethod copy-area ((target clg-display-medium) target-x target-y
		      (source clg-medium) source-x source-y
		      width height
		      &optional (boole boole-1))
  ;; width and height are assumed to be unchanged under the various
  ;; transformations.  We use them, however, to realize a rectangle and
  ;; transform the rectangle to get it's anchor, which may be different
  ;; than transforming the anchor point.
  (let* ((target-drawable (slot-value target 'drawable))
	 (source-drawable (slot-value source 'drawable))
	 (target-device-transformation (insured-device-transformation target))
	 ;; --- kludge.  Should probably implement a method for 
	 ;; insured-device-transformation on pixmap mediums.
	 (source-device-transformation
	   (if (typep source 'display-medium)
	       (insured-device-transformation source)
	       (device-transformation source))))
    (multiple-value-setq (target-x target-y)
      (transform-rectangle*
	target-device-transformation
	target-x target-y (+ target-x width) (+ target-y height)))
    (multiple-value-setq (source-x source-y)
      (transform-rectangle*
	source-device-transformation
	source-x source-y (+ source-x width) (+ source-y height)))
    ;; make width and/or height be negative to interact with BITBLT
    ;; correctly in the event these are the same drawables.
    (let ((width  (if (<= target-x source-x) (+ width)  (- width)))
	  (height (if (<= target-y source-y) (+ height) (- height))))
      (tv:bitblt-from-sheet-to-sheet
	boole (round width) (round height)	;--- round???
	source-drawable (fix-coordinate source-x) (fix-coordinate source-y)
	target-drawable (fix-coordinate target-x) (fix-coordinate target-y)))))

;;; X also has a way to copy from a medium into a pixmap.  Should this be allowed here?
(defmethod copy-area ((pixmap pixmap) dst-x dst-y 
		      (medium clg-display-medium) src-x src-y width height
		      &optional (boole boole-1))
  ;;; This all only works for pixels coordinate systems.
  (with-slots (drawable gcontext port device-transformation) medium
    (multiple-value-bind (device-min-x device-min-y device-lim-x device-lim-y)
	(transform-rectangle* device-transformation
			      src-x src-y (+ src-x width)
			      (+ src-y height))
      (let* ((device-width (- device-lim-x device-min-x))
	     (device-height (- device-lim-y device-min-y))
	     (raster (realize-pixmap port pixmap)))
	(scl:send drawable :bitblt-from-sheet boole
		  (round device-width) (round device-height)	;--- round???
		  (fix-coordinate device-min-x) (fix-coordinate device-min-y)
		  raster 
		  (fix-coordinate dst-x) (fix-coordinate dst-y))))))
||#
