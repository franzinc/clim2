;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defclass genera-medium (basic-medium)
    ((window :initform nil :reader medium-drawable)
     (ink-cache :initform (make-ink-cache 16))))

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
  ;;--- Deallocate the stuff in the ink cache?  Maybe not...
  )

(defmethod (setf medium-foreground) :after (ink (medium genera-medium))
  (let ((window (medium-drawable medium)))
    (scl:send window :set-char-aluf (genera-decode-color ink medium nil))
    (when (medium-drawing-possible medium)
      (scl:send window :refresh))))

(defmethod (setf medium-background) :after (ink (medium genera-medium))
  (let ((window (medium-drawable medium)))
    (scl:send window :set-erase-aluf (genera-decode-color ink medium nil))
    (when (medium-drawing-possible medium)
      (scl:send window :refresh))))

(defmethod medium-real-screen ((medium genera-medium))
  (tv:sheet-screen (medium-drawable medium)))


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

(defun genera-decode-luminosity (luminosity stipple-p)
  (macrolet ((stipple (height width patterns)
	       `(quote ((,(graphics::make-stipple height width patterns)
			 ,boole-set ,boole-clr)))))
    (if (not stipple-p)
	(if (< luminosity 0.5) boole-set boole-clr)
      (sys:selector luminosity <
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
		    (list (first entry)
			  (list (apply #'graphics::make-stipple (second entry))
				boole-1 boole-2)))
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
		  (1.0 (1 1 (#b0)))
		  (+everywhere+ (1 1 (#b1))))))

(defun genera-decode-opacity (opacity)
  (let ((opacities *genera-opacity-stipples*))
    (cond ((eq opacity +nowhere+)
	   (rest (first opacities)))
	  ((eq opacity +everywhere+)
	   (rest (first (last opacities))))
	  (t
	   (let ((last-op (cdr (first opacities)))
		 (value (opacity-value opacity)))
	     (dolist (op (cdr opacities) last-op)
	       (when (< value (car op))
		 (return last-op))
	       (setq last-op (cdr op))))))))

(defmethod genera-decode-color ((ink gray-color) medium &optional (stipple-p t))
  (let ((screen (medium-real-screen medium))
	(color-p (slot-value (port medium) 'color-p)))
    (let ((luminosity (gray-color-luminosity ink))
	  (invert-p (not (scl:send screen :bow-mode))))
      (cond (color-p
	     (scl:send screen :compute-rgb-alu boole-1
		       luminosity luminosity luminosity (not invert-p)))
	    ((= luminosity 0.0) (if invert-p boole-clr boole-set))
	    ((= luminosity 1.0) (if invert-p boole-set boole-clr))
	    (t
	     (genera-decode-luminosity
	       (if invert-p (- 1.0 luminosity) luminosity) stipple-p))))))

(defmethod genera-decode-color ((ink color) medium &optional (stipple-p t))
  (let ((screen (medium-real-screen medium))
	(color-p (slot-value (port medium) 'color-p)))
    (multiple-value-bind (r g b)
	(color-rgb ink)
      (let ((invert-p (not (scl:send screen :bow-mode))))
	(if (not color-p)
	    (let ((luminosity (color-luminosity r g b)))
	      (genera-decode-luminosity
		(if invert-p (- 1.0 luminosity) luminosity) stipple-p))
	    (scl:send screen :compute-rgb-alu boole-1
		      r g b (not invert-p)))))))

(defmethod genera-decode-color ((ink flipping-ink) medium &optional (stipple-p t))
  (declare (ignore stipple-p))
  (let ((screen (medium-real-screen medium))
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
	      (scl:send screen :exchange-two-colors-aluf
			(genera-decode-color (follow-indirect-ink design1 medium) medium)
			(genera-decode-color (follow-indirect-ink design2 medium) medium)))))))))

(defmethod genera-decode-color ((ink standard-opacity) medium &optional (stipple-p t))
  (declare (ignore medium stipple-p))
  (if (> (opacity-value ink) 0.5) boole-1 boole-2))

(defmethod genera-decode-color ((ink nowhere) medium &optional (stipple-p t))
  (declare (ignore medium stipple-p))
  boole-2)

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
    (let ((luminosity (with-slots (clim-utils::which-one clim-utils::how-many) ink
			(/ clim-utils::which-one clim-utils::how-many)))
	  (invert-p (not (scl:send (medium-real-screen medium) :bow-mode))))
      (cond ((= luminosity 0.0) (if invert-p boole-clr boole-set))
	    ((= luminosity 1.0) (if invert-p boole-set boole-clr))
	    (t
	     (genera-decode-luminosity
	       (if invert-p (- 1.0 luminosity) luminosity) t))))))

(defmethod genera-decode-ink ((ink nowhere) medium)
  (declare (ignore medium))
  boole-2)

(defmethod genera-decode-ink ((ink standard-opacity) medium)
  (declare (ignore medium))
  (genera-decode-opacity ink))

(defmethod genera-decode-ink ((ink pattern) medium)
  (genera-decode-pattern ink medium))

(defmethod genera-decode-ink ((ink rectangular-tile) medium)
  (multiple-value-bind (pattern width height)
      (decode-rectangular-tile ink)
    (genera-decode-pattern pattern medium width height t)))

(defmethod genera-decode-ink ((ink stencil) medium)
  (declare (ignore medium))
  (error "Stencils are not supported in this CLIM implementation"))

(defmethod genera-decode-ink ((ink composite-in) medium)
  (let* ((designs (slot-value ink 'clim-utils::designs)))
    (when (= (length designs) 2)
      (let ((color (follow-indirect-ink (aref designs 0) medium))
	    (opacity (aref designs 1)))
	(when (and (colorp color)
		   (opacityp opacity))
	  (return-from genera-decode-ink
	    ;;--- Use the alpha channel if the hardware supports it
	    (genera-decode-ink (make-pattern (caar (genera-decode-opacity opacity))
					     (list +transparent-ink+ color))
			       medium))))))
  (error "This type of compositing is not supported in this CLIM implementation"))

(defmethod genera-decode-ink ((ink composite-out) medium)
  (declare (ignore medium))
  (error "This type of compositing is not supported in this CLIM implementation"))
      

(flavor:defgeneric invoke-with-clim-drawing-state
		   (drawing-state ones-alu zeros-alu
		    thickness dashes line-style pattern
		    continuation window &rest arguments)
  (:inline-methods :recursive)
  (:optimize speed))

(flavor:defmethod (invoke-with-clim-drawing-state graphics::raster-drawing-state :before)
		  (ones-alu zeros-alu thickness dashes line-style pattern
		   continuation window &rest arguments)
  (declare (ignore thickness dashes line-style continuation window arguments))
  (setq graphics::source-pattern pattern)
  (setq graphics::ones-alu ones-alu)
  (setq graphics::zeros-alu zeros-alu))

(flavor:defmethod (invoke-with-clim-drawing-state graphics::drawing-state)
		  (ones-alu zeros-alu thickness dashes line-style pattern
		   continuation window &rest arguments)
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
  (unless (tv:sheet-output-held-p window)
    (apply continuation window arguments)))

(defmacro with-genera-clipping-region ((medium drawable &optional cleft ctop cright cbottom)
				       &body body)
  (let ((sheet '#:sheet)
	(region '#:region)
	(medium-region '#:medium-region)
	(clipping-region '#:clipping-region)
	(valid '#:valid)
	(cleft (or cleft '#:cleft))
	(ctop (or ctop '#:ctop))
	(cright (or cright '#:cright))
	(cbottom (or cbottom '#:cbottom))
	(mleft '#:mleft)
	(mtop '#:mtop)
	(mright '#:mright)
	(mbottom '#:mbottom))
    `(let* ((,sheet (medium-sheet ,medium))
	    (,region (sheet-device-region ,sheet))
	    (,medium-region (medium-clipping-region ,medium))
	    (,valid t))
       (unless (eq ,region +nowhere+)
	 (with-bounding-rectangle* (,cleft ,ctop ,cright ,cbottom) ,region
	   (unless (eq ,medium-region +everywhere+)
	     (with-bounding-rectangle* (,mleft ,mtop ,mright ,mbottom) ,medium-region
	       (multiple-value-setq (,valid ,cleft ,ctop ,cright ,cbottom)
		 (multiple-value-call #'ltrb-overlaps-ltrb-p
		   ,cleft ,ctop ,cright ,cbottom
		   (transform-rectangle* 
		     (sheet-device-transformation ,sheet)
		     ,mleft ,mtop ,mright ,mbottom)))))
	   (when ,valid
	     (fix-coordinates ,cleft ,ctop ,cright ,cbottom)
	     (incf ,cleft (tv:sheet-left-margin-size ,drawable))
	     (incf ,ctop  (tv:sheet-top-margin-size ,drawable))
	     (incf ,cright  (tv:sheet-right-margin-size ,drawable))
	     (incf ,cbottom (tv:sheet-bottom-margin-size ,drawable))
	     (with-stack-list (,clipping-region ,cleft ,ctop ,cright ,cbottom)
	       (scl:letf (((tv:sheet-clipping-region ,drawable) ,clipping-region))
		 ,@body))))))))

;;; The easy continuation is called for simple drawing operations without patterning
;;; or line style options unsupported by the old Genera window system operations.
(zwei:defindentation (invoke-with-appropriate-drawing-state 3 1))
(defmethod invoke-with-appropriate-drawing-state
	   ((medium genera-medium) ink line-style easy hard)
  (let ((alu (let* ((ink-cache (slot-value medium 'ink-cache))
		    (ink (follow-indirect-ink ink medium)))
	       (or (ink-cache-lookup ink-cache ink)
		   (ink-cache-replace ink-cache ink (genera-decode-ink ink medium))))))
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
		      (invoke-with-clim-drawing-state 
			,drawing-state ones-alu zeros-alu
			,thickness ,dashes ,line-style ,raster
			,continuation ,window))))
	(let ((window (medium-drawable medium)))
	  (with-genera-clipping-region (medium window)
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
		    (unless (tv:sheet-output-held-p window)
		      (funcall easy window alu))
		    (with-drawing-state-kludge (graphics::get-drawing-state window)
		      alu boole-2
		      thickness dashes line-style nil
		      hard window)))))))))

(defmethod medium-drawing-possible ((medium genera-medium))
  (let ((window (medium-drawable medium)))
    ;; TV:SHEET-OUTPUT-HELD-P, except that temp-locking only delays drawing,
    ;; it doesn't make it impossible, so only look at the output-hold flag
    (and window				;guard against ungrafted mediums
	 (or (zerop (tv:sheet-output-hold-flag window))
	     (tv:sheet-screen-array window)))))

(defmethod stream-scan-string-for-writing 
	   ((stream clim-internals::output-protocol-mixin) (medium genera-medium)
	    string start end style cursor-x max-x &optional glyph-buffer)
  (with-genera-glyph-for-character 
    (stream-scan-string-for-writing-1
      stream medium string start end style cursor-x max-x glyph-buffer)))


(defparameter *use-macivory-for-drawing* t)
(defmacro with-drawing-model-adjustments 
	  ((window &optional (host-allowed '*use-macivory-for-drawing*))
	   &body body)
  #-IMach (declare (ignore window host-allowed))
  #-IMach `(progn ,@body)
  #+IMach `(sys:system-case
	     (:macivory
	       (graphics:with-scan-conversion-mode (,window :host-allowed ,host-allowed)
		 (progn ,@body)))
	     (otherwise ,@body)))

(defmethod medium-draw-point* ((medium genera-medium) x y)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (medium-line-style medium)))
      (convert-to-device-coordinates transform x y)
      (invoke-with-appropriate-drawing-state medium ink line-style
	#'(lambda (window alu)
	    (declare (sys:downward-function))
	    (scl:send window :draw-point x y alu))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (funcall (flavor:generic graphics:draw-point) window x y))))))

(defmethod medium-draw-points* ((medium genera-medium) position-seq)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (medium-line-style medium)))
      (invoke-with-appropriate-drawing-state medium ink line-style
        #'(lambda (window alu)
	    (declare (sys:downward-function))
	    (map-position-sequence
	      #'(lambda (x y)
		  (convert-to-device-coordinates transform x y)
		  (scl:send window :draw-point x y alu))
	      position-seq))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (map-position-sequence
	      #'(lambda (x y)
		  (convert-to-device-coordinates transform x y)
		  (funcall (flavor:generic graphics:draw-point) window x y))
	      position-seq))))))


(defmethod medium-draw-line* ((medium genera-medium) x1 y1 x2 y2)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (medium-line-style medium))) 
      (convert-to-device-coordinates transform x1 y1 x2 y2)
      (invoke-with-appropriate-drawing-state medium ink line-style
        #'(lambda (window alu)
	    (declare (sys:downward-function))
	    (with-drawing-model-adjustments (window)
	      (scl:send window :draw-line x1 y1 x2 y2 alu t)))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (with-drawing-model-adjustments (window)
	      (funcall (flavor:generic graphics:draw-line)
		       window x1 y1 x2 y2 )))))))

(defmethod medium-draw-lines* ((medium genera-medium) position-seq)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (medium-line-style medium)))
      (invoke-with-appropriate-drawing-state medium ink line-style
        #'(lambda (window alu)
	    (declare (sys:downward-function))
	    (with-drawing-model-adjustments (window)
	      (let ((length (length position-seq)))
		(with-stack-array (points length :initial-contents position-seq)
		  (declare (type simple-vector points))
		  (do ((i 0 (+ i 4)))
		      ((>= i length))
		    (let ((x1 (svref points i))
			  (y1 (svref points (+ i 1)))
			  (x2 (svref points (+ i 2)))
			  (y2 (svref points (+ i 3))))
		      (convert-to-device-coordinates transform x1 y1 x2 y2)
		      (setf (svref points i) x1)
		      (setf (svref points (+ i 1)) y1)
		      (setf (svref points (+ i 2)) x2)
		      (setf (svref points (+ i 3)) y2)))
		  (scl:send window :draw-multiple-lines points alu t)))))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (with-drawing-model-adjustments (window)
	      (map-endpoint-sequence
		#'(lambda (x1 y1 x2 y2)
		    (convert-to-device-coordinates transform x1 y1 x2 y2)
		    (funcall (flavor:generic graphics:draw-line)
			     window x1 y1 x2 y2 ))
		position-seq)))))))

(defmethod medium-draw-rectangle* ((medium genera-medium) 
				   left top right bottom filled)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (medium-line-style medium)))
      (convert-to-device-coordinates transform left top right bottom)
      (when (< right left) (rotatef right left))
      (when (< bottom top) (rotatef bottom top))
      (if (and filled
	       (typep ink 'pattern))
	  ;; Looks like this is DRAW-PATTERN*
	  (medium-draw-pattern* medium left top right bottom ink)
	  (invoke-with-appropriate-drawing-state medium ink line-style
	    #'(lambda (window alu)
		(declare (sys:downward-function))
		(if filled
		    (let ((width (- right left))
			  (height (- bottom top)))
		      (with-drawing-model-adjustments (window)
			(scl:send window :draw-rectangle width height left top alu)))
		    (scl:stack-let ((lines (vector right top left top
						   left top left bottom
						   left bottom right bottom
						   right bottom right top)))
		      (with-drawing-model-adjustments (window)
			(scl:send window :draw-multiple-lines lines alu nil)))))
	    #'(lambda (window)
		(declare (sys:downward-function))
		(with-drawing-model-adjustments (window)
		  (funcall (flavor:generic graphics:draw-rectangle) 
			   window left top right bottom
			   :filled filled))))))))

(defmethod medium-draw-rectangles* ((medium genera-medium) position-seq filled)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (medium-line-style medium)))
      (invoke-with-appropriate-drawing-state medium ink line-style
	#'(lambda (window alu)
	    (declare (sys:downward-function))
	    (with-drawing-model-adjustments (window)
	      (if filled
		  (let ((length (length position-seq)))
		    (with-stack-array (points length :initial-contents position-seq)
		      (declare (type simple-vector points))
		      (do ((i 0 (+ i 4)))
			  ((>= i length))
			(let ((left (svref points i))
			      (top  (svref points (+ i 1)))
			      (right  (svref points (+ i 2)))
			      (bottom (svref points (+ i 3))))
			  (convert-to-device-coordinates transform left top right bottom)
			  (when (< right left) (rotatef right left))
			  (when (< bottom top) (rotatef bottom top))
			  (setf (svref points i) left)
			  (setf (svref points (+ i 1)) top)
			  (setf (svref points (+ i 2)) right)
			  (setf (svref points (+ i 3)) bottom)))
		      (scl:send window :draw-multiple-rectangles points alu)))
		  (map-endpoint-sequence
		    #'(lambda (left top right bottom)
			(convert-to-device-coordinates transform left top right bottom)
			(scl:stack-let ((lines (vector right top left top
						       left top left bottom
						       left bottom right bottom
						       right bottom right top)))
			  (scl:send window :draw-multiple-lines lines alu nil)))
		    position-seq))))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (with-drawing-model-adjustments (window)
	      (map-endpoint-sequence
		#'(lambda (left top right bottom)
		    (convert-to-device-coordinates transform left top right bottom)
		    (when (< right left) (rotatef right left))
		    (when (< bottom top) (rotatef bottom top))
		    (funcall (flavor:generic graphics:draw-rectangle)
			     window left top right bottom
			     :filled filled))
		position-seq)))))))

;; Fall back to DRAW-IMAGE.  INK will be a pattern.
;; We have to resort to this because Genera always tiles.  Sigh.
(defmethod medium-draw-pattern* ((medium genera-medium) left top right bottom ink)
  (let* ((window (medium-drawable medium))
	 (ink-cache (slot-value medium 'ink-cache))
	 (width  (- right left))
	 (height (- bottom top))
	 (alus (or (ink-cache-lookup ink-cache ink)
		   (ink-cache-replace ink-cache ink (genera-decode-ink ink medium))))
	 (thickness 0)
	 (dashes nil))
    (assert (= (length alus) 1) ()
	    "CLIM only supports bitmap patterns under Genera sheets")
    (dolist (alu alus)
      (let* ((image (pop alu))
	     (ones-alu (pop alu))
	     (zeros-alu (pop alu)))
	(multiple-value-bind (ones-alu zeros-alu)
	    (if (not (and (integerp ones-alu)
			  (integerp zeros-alu)))
		(values ones-alu zeros-alu)
		(values (logior (logand ones-alu 5) (scl:rot (logand zeros-alu 5) 1)) 
			zeros-alu))
	  (with-genera-clipping-region (medium window)
	    (invoke-with-clim-drawing-state
	      (graphics::get-drawing-state window) ones-alu zeros-alu
	      thickness dashes nil image
	      #'(lambda (window)
		  (declare (sys:downward-function))
		  (with-drawing-model-adjustments (window)
		    (graphics:draw-image image left top
					 :image-right width :image-bottom height
					 :opaque (if (eq zeros-alu boole-2) nil t)
					 :stream window)))
	      window)))))))

(defmethod medium-draw-polygon* ((medium genera-medium) position-seq closed filled)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (line-style (medium-line-style medium))
	   (npoints (length position-seq)))
      (invoke-with-appropriate-drawing-state medium ink line-style
        #'(lambda (window alu)
	    (declare (sys:downward-function))
	    (if filled
		(case npoints
		  (6 (multiple-value-bind (x1 y1 x2 y2 x3 y3)
			 (if (listp position-seq)
			     (let ((p position-seq))
			       (values (pop p) (pop p)
				       (pop p) (pop p)
				       (pop p) (pop p)))
			     (let ((p position-seq))
			       (declare (type vector p))
			       (values (aref p 0) (aref p 1)
				       (aref p 2) (aref p 3)
				       (aref p 4) (aref p 5))))
		       (convert-to-device-coordinates transform
			 x1 y1 x2 y2 x3 y3)
		       (with-drawing-model-adjustments (window)
			 (scl:send window :draw-triangle x1 y1 x2 y2 x3 y3 alu))))
		  (8 (multiple-value-bind (x1 y1 x2 y2 x3 y3 x4 y4)
			 (if (listp position-seq)
			     (let ((p position-seq))
			       (values (pop p) (pop p)
				       (pop p) (pop p)
				       (pop p) (pop p)
				       (pop p) (pop p)))
			     (let ((p position-seq))
			       (declare (type vector p))
			       (values (aref p 0) (aref p 1)
				       (aref p 2) (aref p 3)
				       (aref p 4) (aref p 5)
				       (aref p 6) (aref p 7))))
		       (convert-to-device-coordinates transform
			 x1 y1 x2 y2 x3 y3 x4 y4)
		       (with-drawing-model-adjustments (window)
			 (scl:send window :draw-triangle x1 y1 x2 y2 x3 y3 alu)
			 (scl:send window :draw-triangle x3 y3 x4 y4 x1 y1 alu))))
		  (otherwise
		    (with-stack-array (points npoints)
		      (let ((i 0))
			(map-position-sequence
			  #'(lambda (x y)
			      (convert-to-device-coordinates transform x y)
			      (setf (aref points (shiftf i (1+ i))) x)
			      (setf (aref points (shiftf i (1+ i))) y))
			  position-seq))
		      (with-drawing-model-adjustments (window)
			(graphics::triangulate-polygon
			  #'(lambda (x1 y1 x2 y2 x3 y3)
			      (scl:send window :draw-triangle x1 y1 x2 y2 x3 y3 alu))
			  points)))))
		(with-stack-array (lines (* (+ npoints (if closed 0 -2)) 2))
		  (let* ((i 0)
			 (j 0)
			 (initial-x (elt position-seq (shiftf j (1+ j))))
			 (initial-y (elt position-seq (shiftf j (1+ j))))
			 (x initial-x)
			 (y initial-y))
		    (convert-to-device-coordinates transform 
		      initial-x initial-y x y)
		    (loop
		      (setf (aref lines (shiftf i (1+ i))) x)
		      (setf (aref lines (shiftf i (1+ i))) y)
		      (setq x (elt position-seq (shiftf j (1+ j)))
			    y (elt position-seq (shiftf j (1+ j))))
		      (convert-to-device-coordinates transform x y)
		      (setf (aref lines (shiftf i (1+ i))) x)
		      (setf (aref lines (shiftf i (1+ i))) y)
		      (when (= j npoints)
			(when closed
			  (setf (aref lines (shiftf i (1+ i))) x)
			  (setf (aref lines (shiftf i (1+ i))) y)
			  (setf (aref lines (shiftf i (1+ i))) initial-x)
			  (setf (aref lines (shiftf i (1+ i))) initial-y))
			(return))))
		  (with-drawing-model-adjustments (window)
		    (scl:send window :draw-multiple-lines lines alu nil)))))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (with-stack-array (points npoints)
	      (let ((i 0))
		(map-position-sequence
		  #'(lambda (x y)
		      (convert-to-device-coordinates transform x y)
		      (setf (aref points (shiftf i (1+ i))) x)
		      (setf (aref points (shiftf i (1+ i))) y))
		  position-seq))
	      (with-drawing-model-adjustments (window)
		(if filled
		    (funcall (flavor:generic graphics:draw-polygon) window points
			     :filled t)
		    (funcall (flavor:generic graphics:draw-lines) window points
			     :closed closed)))))))))

(defmethod medium-draw-ellipse* ((medium genera-medium)
				 center-x center-y 
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
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
	(setq x-radius (fix-coordinate (abs x-radius))
	      y-radius (fix-coordinate (abs y-radius)))
	(if (and (= x-radius y-radius) (null start-angle))
	    ;; It's a complete circle
	    (invoke-with-appropriate-drawing-state medium ink line-style
	      #'(lambda (window alu)
		  (declare (sys:downward-function))
		  (with-drawing-model-adjustments (window)
		    (if filled
			(scl:send window :draw-filled-in-circle center-x center-y x-radius alu)
			(scl:send window :draw-circle center-x center-y x-radius alu))))
	      #'(lambda (window)
		  (declare (sys:downward-function))
		  (with-drawing-model-adjustments (window)
		    (funcall (flavor:generic graphics:draw-ellipse) window
			     center-x center-y x-radius y-radius
			     :filled filled))))
	  ;; For general ellipse, let Genera do all the work
	  (invoke-with-appropriate-drawing-state medium ink line-style
	    nil
	    #'(lambda (window)
		(declare (sys:downward-function))
		(graphics:saving-graphics-transform (window)
		  (graphics:graphics-translate center-x center-y :stream window)
		  (when (/= axis-rotate-angle 0)
		    (graphics:graphics-rotate axis-rotate-angle :stream window))
		  (with-drawing-model-adjustments (window)
		    (funcall (flavor:generic graphics:draw-ellipse) window
			     0 0 x-radius y-radius
			     :start-angle (or start-angle 0)
			     :end-angle (or end-angle graphics:2pi)
			     :filled filled))))))))))

(defmethod medium-draw-string* ((medium genera-medium)
				string x y start end align-x align-y
				towards-x towards-y transform-glyphs)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (text-style (medium-merged-text-style medium)))
      (convert-to-device-coordinates transform x y)
      (when towards-x
	(convert-to-device-coordinates transform towards-x towards-y))
      (unless start
	(setq start 0))
      (unless end
	(setq end (length string)))
      (when (< start end)
	(let* ((font (text-style-mapping 
		       (port medium) text-style *standard-character-set*
		       (medium-drawable medium)))
	       (height (sys:font-char-height font))
	       (baseline (sys:font-baseline font))
	       (descent (- height baseline))
	       (ascent (- height descent)))
	  (let ((x-adjust 
		  (compute-text-x-adjustment align-x medium string text-style start end))
		(y-adjust 
		  (compute-text-y-adjustment align-y descent ascent height)))
	    (incf x x-adjust)
	    (incf y y-adjust)
	    (when towards-x
	      (incf towards-x x-adjust)
	      (incf towards-y y-adjust)))
	  (with-stack-list (style :device-font (tv:font-name font) :normal)
	    (invoke-with-appropriate-drawing-state medium ink nil
	      #'(lambda (window alu)
		  (declare (sys:downward-function))
		  (tv:prepare-sheet (window)
		    (let ((x (+ x (tv:sheet-left-margin-size window)))
			  (y (+ y (tv:sheet-top-margin-size window))))
		      (tv:sheet-draw-string window alu
					    x (- y baseline) string font
					    start end (tv:sheet-width window)))))
	      #'(lambda (window)
		  (declare (sys:downward-function))
		  (with-stack-array (substring (- end start) :element-type 'character)
		    (replace substring string :start2 start :end2 end)
		    (funcall (flavor:generic graphics:draw-string) window
			     substring x y :character-style style))))))))))

(defmethod medium-draw-character* ((medium genera-medium)
				   character x y align-x align-y
				   towards-x towards-y transform-glyphs)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet))
	   (ink (medium-ink medium))
	   (text-style (medium-merged-text-style medium)))
      (convert-to-device-coordinates transform x y)
      (when towards-x
	(convert-to-device-coordinates transform towards-x towards-y))
      (let* ((font (text-style-mapping 
		     (port medium) text-style *standard-character-set*
		     (medium-drawable medium)))
	     (height (sys:font-char-height font))
	     (descent (- height (sys:font-baseline font)))
	     (ascent (- height descent)))
	(let ((x-adjust 
		(compute-text-x-adjustment align-x medium character text-style))
	      (y-adjust 
		(- (compute-text-y-adjustment align-y descent ascent height) ascent)))
	  (incf x x-adjust)
	  (incf y y-adjust)
	  (when towards-x
	    (incf towards-x x-adjust)
	    (incf towards-y y-adjust)))
	(macrolet ((with-diacritic ((character font) &body body)
		     ;; I am dumbfounded.  It seems that we need to bind the
		     ;; fill pointer of the font structure beyond its usual
		     ;; length if we want to output a diacritic character.
		     `(if (diacritic-char-p ,character)
			  (letf-globally (((tv:font-fill-pointer ,font) #o320))
			    ,@body)
			  ,@body)))
	  (invoke-with-appropriate-drawing-state medium ink nil
	    #'(lambda (window alu)
		(declare (sys:downward-function))
		(tv:prepare-sheet (window)
		  (let ((x (+ x (tv:sheet-left-margin-size window)))
			(y (+ y (tv:sheet-top-margin-size window))))
		    (with-diacritic (character font)
		      (tv:sheet-draw-glyph (char-code character) font x y alu window)))))
	    #'(lambda (window)
		(declare (sys:downward-function))
		(with-diacritic (character font)
		  (funcall (flavor:generic graphics:draw-glyph) window
			   (char-code character) font x y)))))))))

(defmethod medium-draw-text* ((medium genera-medium)
			      string-or-char x y start end
			      align-x align-y
			      towards-x towards-y transform-glyphs)
  (if (characterp string-or-char)
      (medium-draw-character* medium string-or-char x y 
			      align-x align-y towards-x towards-y transform-glyphs)
      (medium-draw-string* medium string-or-char x y start end 
			   align-x align-y towards-x towards-y transform-glyphs)))


(defmethod medium-clear-area ((medium genera-medium) left top right bottom)
  (when (medium-drawing-possible medium)
    (let* ((sheet (medium-sheet medium))
	   (transform (sheet-device-transformation sheet)))
      (convert-to-device-coordinates transform left top right bottom)
      (when (< right left) (rotatef right left))
      (when (< bottom top) (rotatef bottom top))
      (invoke-with-appropriate-drawing-state medium +background-ink+ nil
	#'(lambda (window alu)
	    (declare (sys:downward-function))
	    (let ((width (- right left))
		  (height (- bottom top)))
	      (with-drawing-model-adjustments (window)
		(scl:send window :draw-rectangle width height left top alu))))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (with-drawing-model-adjustments (window)
	      (funcall (flavor:generic graphics:draw-rectangle) 
		       window left top right bottom
		       :filled t)))))))


(defmethod text-style-width ((text-style standard-text-style) (medium genera-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (sys:font-char-width font)))

(defmethod text-style-height ((text-style standard-text-style) (medium genera-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (sys:font-char-height font)))

(defmethod text-style-ascent ((text-style standard-text-style) (medium genera-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (let* ((height (sys:font-char-height font))
	   (descent (- height (sys:font-baseline font)))
	   (ascent (- height descent)))
      ascent)))

(defmethod text-style-descent ((text-style standard-text-style) (medium genera-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (let* ((height (sys:font-char-height font))
	   (descent (- height (sys:font-baseline font))))
      descent)))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (medium genera-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
      (null (sys:font-char-width-table font))))


(defmethod medium-force-output ((medium genera-medium))
  (scl:send (medium-drawable medium) :force-output))

(defmethod medium-finish-output ((medium genera-medium))
  (scl:send (medium-drawable medium) :force-output))

(defmethod medium-beep ((medium genera-medium))
  (scl:beep nil (medium-drawable medium)))


