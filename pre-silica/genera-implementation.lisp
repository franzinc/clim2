;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Defined below
(eval-when (compile load eval)
  (proclaim '(special *sheet-device*))
  #+IMach (proclaim '(special *small-sheet-device*)))

(defconstant *alu-noop* boole-2)

(defclass sheet-implementation-mixin
	  ()
     ((window :initarg :window)
      (color-p :initform nil)
      (embedded-p :initform nil)
      (ink-cache :initform (make-ink-cache 16))))

;;; Will these SAGE variables be present in all Genera worlds?
;;; I don't want to have to figure out how many microns/pixel there are
;;; for each screen type.
(defmethod implementation-pixels-per-point ((stream sheet-implementation-mixin))
  (/ sage::*microns-per-point* sage:*microns-per-screen-pixel*))

(defmethod window-erase-viewport ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (when (window-drawing-possible stream)
      (scl:send window :clear-window))))

(defmethod window-clear-area ((stream sheet-implementation-mixin) left top right bottom)
  ;; In sheet window coordinates, basically
  (with-slots (window) stream
    (when (window-drawing-possible stream)
      (scl:send window :draw-rectangle (- right left) (- bottom top) left top
		(scl:send window :erase-aluf)))))

(defmethod window-visibility ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (scl:send window :exposed-p)))

(defmethod (setf window-visibility) (visibility (stream sheet-implementation-mixin))
  (with-slots (window) stream
    ;; visibility is a boolean
    (if visibility
	;; this isn't quite right, because activating it means it's now buried??
	(scl:send window :expose)
	(scl:send window :deactivate))
    visibility))

(defmethod wait-for-window-exposed ((window sheet-implementation-mixin))
  (unless (window-visibility window)
    (scl:process-wait "Await exposure" #'window-visibility window)))

(defmethod window-drawing-possible ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    ;; tv:sheet-output-held-p, except that temp-locking only delays drawing,
    ;; it doesn't make it impossible, so only look at the output-hold flag
    (or (zerop (tv:sheet-output-hold-flag window))
	(tv:sheet-screen-array window))))

(defmethod window-stack-on-top ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (unless (scl:send window :exposed-p)
      (scl:send window :expose))
    (let ((screen (tv:sheet-screen window))
	  (console (tv:sheet-console window))
	  (mouse (tv:sheet-mouse window)))
      ;; SCREEN-MANAGE-AUTOEXPOSE-INFERIORS doesn't reliably select a window
      ;; that fills the entire screen
      (let ((sw (tv:console-selected-window console)))
	(unless (and sw (atom (tv:sheet-lock sw)))
	  (scl:send window :select)))
      (scl:send screen :screen-select)
      (unless (eq screen (tv:sheet-screen (tv:mouse-sheet mouse)))
	(tv:mouse-set-sheet screen)))))

(defmethod window-stack-on-bottom ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (scl:send window :bury)))

(defmethod close ((stream sheet-implementation-mixin) &key abort)
  (declare (ignore abort))
  (with-slots (window) stream
    (if (eq window (scl:send window :screen)) ; if this is a screen.
	(mapc #'close (window-children stream))
	(scl:send window :kill))))

(defmethod bounding-rectangle-set-edges :after
	   ((stream sheet-implementation-mixin) left top right bottom)
  (with-slots (window) stream
    (let ((superior (tv:sheet-superior window)))
      (when t ; (null (tv:sheet-superior superior))	;root-p
	(multiple-value-bind (wl wt wr wb)
	    (scl:send superior :inside-edges)
	  (declare (ignore wr wb))
	  (translate-positions wl wt left top right bottom))))
    (scl:send window :set-edges left top right bottom)))

(defmethod bounding-rectangle-set-position :after
	   ((stream sheet-implementation-mixin) new-left new-top)
  (with-slots (window) stream
    (let ((superior (tv:sheet-superior window)))
      (when t ; (null (tv:sheet-superior superior))	;root-p
	(multiple-value-bind (wl wt)
	    (scl:send superior :inside-edges)
	  (translate-positions wl wt new-left new-top))))
    (scl:send window :set-position new-left new-top)))

(defmethod bounding-rectangle-set-size :after
	   ((stream sheet-implementation-mixin) width height)
  (with-slots (window) stream
    (multiple-value-bind (wl wt wr wb)
	(scl:send window :inside-edges)
      (setq wr (+ wl width)
	    wb (+ wt height))
      (scl:send window :set-edges wl wt wr wb))))

(defmethod stream-force-output ((stream sheet-implementation-mixin))
  ;; yikes!!  Don't charge the screen-manager overhead to the caller!
  #+Ignore
  (with-slots (window) stream
    (tv:screen-manage-window-area window)))

(defmethod stream-clear-output ((stream sheet-implementation-mixin))
  )

(defmethod stream-finish-output ((stream sheet-implementation-mixin))
  ;; kludge after kludge
  (stream-force-output stream))

(defmethod redisplay-decorations ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (when (window-drawing-possible stream)
      (scl:send window :refresh-margins))))

(defmethod window-refresh ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (scl:send window :refresh)))

(defun clim-label->genera-label (label-spec stream)
  (when label-spec
    (let (label label-text-style character-style)
      (setq label (cond ((stringp label-spec) label-spec)
			(t (first label-spec))))

      (setq label-text-style
	    (cond ((stringp label-spec)
		   (medium-merged-text-style stream))
		  ((getf (rest label-spec) :text-style))
		  (t (medium-merged-text-style stream))))

      (setq character-style
	    (si:intern-character-style :device-font
				       (tv:font-name 
					 (text-style-mapping
					   (slot-value stream 'display-device-type)
					   label-text-style
					   *standard-character-set*
					   (slot-value stream 'window)))
				       :normal))
      `(:string ,label :character-style ,character-style))))

;;; A label is either a string or a list of the form (string :text-style style).
(defmethod (setf window-label) :after (new-label (stream sheet-implementation-mixin))
  (with-slots (window) stream
    (multiple-value-bind (old-width old-height)
	(scl:send window :inside-size)
      (let ((genera-label (clim-label->genera-label new-label stream)))
	(scl:send window :set-label genera-label))
      (ignore-errors
	(scl:send window :set-inside-size old-width old-height)))))

(defmethod window-label-size ((stream sheet-implementation-mixin)
			      &optional (label (window-label stream)))
  (cond ((null label)
	 (values (coordinate 0) (coordinate 0)))
	(t (dw::margin-label-size (clim-label->genera-label label stream)
				  (slot-value stream 'window)))))

(defmethod window-margins ((stream sheet-implementation-mixin))
  stream
  (values (coordinate 0) (coordinate 0) 
	  (coordinate 0) (coordinate 0)))

(defmethod host-window-margins ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (scl:send window :margins)))

(defmethod window-beep ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (scl:send window :beep)))

(defmethod window-flush-update-region ((stream sheet-implementation-mixin))
  (with-slots (update-region) stream
    (setf update-region nil)))

(defmethod window-process-update-region ((stream sheet-implementation-mixin))
  (with-slots (update-region) stream
    (when update-region
      (multiple-value-bind (vx vy)
	  (window-viewport-position stream)
	(dolist (rectangle update-region)
	  (with-bounding-rectangle* (left top right bottom) rectangle
	    (translate-positions (- vx) (- vy) left top right bottom)
	    (window-clear-area stream left top right bottom)))))))

(defmethod window-shift-visible-region :after ((stream sheet-implementation-mixin)
					       old-left old-top old-right old-bottom
					       new-left new-top new-right new-bottom)
  (let ((rectangles (ltrb-difference new-left new-top new-right new-bottom
				     old-left old-top old-right old-bottom)))
    (setf (slot-value stream 'update-region) rectangles)))

;; Does this want to be an :after method?  Will the primary method on
;; window-stream ever want to do anything else?
(defmethod stream-set-input-focus ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (prog1 (sys:console-selected-window (tv:sheet-console window))
	   ;; :select-relative ?
	   (scl:send window :select))))

(defmethod stream-restore-input-focus ((stream sheet-implementation-mixin)
				       old-focus)
  (with-slots (window) stream
    (unless (eq old-focus window)
      (if (eq (scl:send old-focus :alias-for-selected-windows)
	      (scl:send window :alias-for-selected-windows))
	  (scl:send old-focus :select-relative)
	  (scl:send window :deselect t)))))

(defmethod copy-area ((stream sheet-implementation-mixin)
		      from-left from-top from-right from-bottom
		      to-left to-top)
  (with-slots (window) stream
    (when (window-drawing-possible stream)
      (fix-points from-left from-top from-right from-bottom to-left to-top)
      ;; coords in "host" coordinate system
      (let ((width (- from-right from-left))
	    (height (- from-bottom from-top)))
	(when (>= to-left from-left)
	  ;; shifting to the right
	  (setq width (- (abs width))))
	(when (>= to-top from-top)
	  (setq height (- (abs height))))
	(scl:send window :bitblt-within-sheet
			 tv:alu-seta width height
			 from-left from-top
			 to-left to-top)))))

(defmethod notify-user-1 ((stream sheet-implementation-mixin)
			  frame format-string &rest format-args)
  (declare (ignore frame) (dynamic-extent format-args))
  (apply #'tv:notify (slot-value stream 'window) format-string format-args))


;;;; Translation between CLIM and Genera graphics model

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

(defun-inline follow-indirect-ink (ink stream)
  (cond ((eq ink +foreground-ink+) (medium-foreground stream))
	((eq ink +background-ink+) (medium-background stream))
	(t ink)))

(defmethod sheet-decode-color (ink stream &optional (stipple-p t))
  (declare (ignore stipple-p stream))
  (error "Expected ~S to be a color.  This may be due to an implementation limitation." ink))

(defun sheet-decode-luminance (luminance stipple-p)
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

(defmethod sheet-decode-color ((ink gray-color) stream &optional (stipple-p t))
  (with-slots (color-p window) stream
    (let ((luminance (slot-value ink 'clim-utils::luminance))
	  (invert-p (not (funcall (tv:sheet-screen window) :bow-mode))))
      (cond ((= luminance 0.0) (if invert-p boole-clr boole-set))
	    ((= luminance 1.0) (if invert-p boole-set boole-clr))
	    ((not color-p)
	     (sheet-decode-luminance (if invert-p (- 1.0 luminance) luminance) stipple-p))
	    (t
	     (funcall (tv:sheet-screen window) :compute-rgb-alu boole-1
		      luminance luminance luminance (not invert-p)))))))

(defmethod sheet-decode-color ((ink color) stream &optional (stipple-p t))
  (with-slots (color-p window) stream
    (multiple-value-bind (r g b)
	(color-rgb ink)
      (let ((invert-p (not (funcall (tv:sheet-screen window) :bow-mode))))
	(if (not color-p)
	    (let ((luminance (color-luminosity r g b)))
	      (sheet-decode-luminance (if invert-p (- 1.0 luminance) luminance) stipple-p))
	  (funcall (tv:sheet-screen window) :compute-rgb-alu boole-1
		   r g b (not invert-p)))))))

(defmethod sheet-decode-color ((ink flipping-ink) stream &optional (stipple-p t))
  (declare (ignore stipple-p))
  (with-slots (color-p window) stream
    (if (not color-p)
	(values boole-xor)
      (multiple-value-bind (design1 design2)
	  (decode-flipping-ink ink)
	(let ((ink1 (follow-indirect-ink design1 stream))
	      (ink2 (follow-indirect-ink design2 stream)))
	  ;; Genera substrate can get confused about this...
	  (if (or (and (eq ink1 +black+) (eq ink2 +white+))
		  (and (eq ink1 +white+) (eq ink2 +black+)))
	      (values boole-xor)
	    (values
	      (funcall (tv:sheet-screen window) :exchange-two-colors-aluf
		       (sheet-decode-color (follow-indirect-ink design1 stream) stream)
		       (sheet-decode-color (follow-indirect-ink design2 stream) stream)))))))))

(defmethod sheet-decode-color ((ink standard-opacity) stream &optional (stipple-p t))
  (declare (ignore stream stipple-p))
  (if (> (slot-value ink 'clim-utils::value) 0.5) boole-1 boole-2))

(defun sheet-decode-raster (raster &optional width height key tiled-p)
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

(defmethod sheet-decode-pattern ((ink pattern) stream &optional width height tiled-p)
  (multiple-value-bind (array designs)
      (decode-pattern ink)
    (let ((width (or width (array-dimension array 1)))
	  (height (or height (array-dimension array 0))))
      (if (= (length designs) 2)
	  (let ((raster (sheet-decode-raster array width height nil tiled-p)))
	    (list
	      (list raster
		    (sheet-decode-color
		      (follow-indirect-ink (aref designs 1) stream) stream nil)
		    (sheet-decode-color
		      (follow-indirect-ink (aref designs 0) stream) stream nil)
		    (and (not tiled-p) width)
		    (and (not tiled-p) height)
		    (not (eq raster array)))))
	(do* ((result (make-list (length designs)))
	      (i 0 (1+ i))
	      (l result (cdr l)))
	     ((null l) result)
	  (setf (car l) (list (sheet-decode-raster array width height i tiled-p)
			      (sheet-decode-color
				(follow-indirect-ink (aref designs i) stream) stream nil)
			      (if (zerop i) boole-clr boole-2)
			      (and (not tiled-p) width)
			      (and (not tiled-p) height)
			      t)))))))

(defmethod sheet-decode-pattern ((ink design) stream &optional width height tiled-p)
  (declare (ignore stream width height tiled-p))
  (error "Arbitrary patterned designs are not supported in this CLIM implementation"))

(defmethod sheet-decode-pattern ((ink color) stream &optional width height tiled-p)
  (declare (ignore stream width height tiled-p))
  (error "A pattern must be a bounded design, ~S isn't" ink))

(defmethod sheet-decode-ink ((ink color) stream)
  (sheet-decode-color ink stream))

(defmethod sheet-decode-ink ((ink flipping-ink) stream)
  (sheet-decode-color ink stream))

(defmethod sheet-decode-ink ((ink contrasting-ink) stream)
  (if (slot-value stream 'color-p)
      (sheet-decode-color (make-color-for-contrasting-ink ink) stream)
    ;; More efficient than (SHEET-DECODE-COLOR (MAKE-GRAY-COLOR-FOR-CONTRASTING-INK INK))
    (let ((luminance (with-slots (clim-utils::which-one clim-utils::how-many) ink
		       (/ clim-utils::which-one clim-utils::how-many)))
	  (invert-p (not (funcall (tv:sheet-screen (slot-value stream 'window)) :bow-mode))))
      (cond ((= luminance 0.0) (if invert-p boole-clr boole-set))
	    ((= luminance 1.0) (if invert-p boole-set boole-clr))
	    (t
	     (sheet-decode-luminance (if invert-p (- 1.0 luminance) luminance) t))))))

(defmethod sheet-decode-ink ((ink pattern) stream)
  (sheet-decode-pattern ink stream))

(defmethod sheet-decode-ink ((ink rectangular-tile) stream)
  (multiple-value-bind (pattern width height)
      (decode-rectangular-tile ink)
    (sheet-decode-pattern pattern stream width height t)))

(defmethod sheet-decode-ink ((ink stencil) stream)
  (declare (ignore stream))
  (error "Stencils and opacities are not supported in this CLIM implementation"))

(defmethod sheet-decode-ink ((ink composite-in) stream)
  (declare (ignore stream))
  (error "Compositing is not supported in this CLIM implementation"))

(defmethod sheet-decode-ink ((ink composite-out) stream)
  (declare (ignore stream))
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
	   ((stream sheet-implementation-mixin) ink line-style easy hard)
  (let ((alu (with-slots (ink-cache) stream
	       (let ((ink (follow-indirect-ink ink stream)))
		 (or (ink-cache-lookup ink-cache ink)
		     (ink-cache-replace ink-cache ink (sheet-decode-ink ink stream)))))))
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
			      (not (slot-value stream 'embedded-p))
			      (null dashes)
			      (<= thickness 1))
			 (funcall easy window alu)
		       (with-drawing-state-kludge (graphics::get-drawing-state window)
						  alu boole-2
						  thickness dashes line-style nil
						  hard window)))))
	  (with-slots (window) stream
	    (let ((clipping-region (slot-value stream 'transformed-clipping-region)))
	      (if (eq clipping-region +everywhere+)
		  (kernel window)
		(with-bounding-rectangle* (left top right bottom) clipping-region
		  (with-stack-list (cr (+ (tv:sheet-left-margin-size window) left)
				       (+ (tv:sheet-top-margin-size window) top)
				       (+ (tv:sheet-right-margin-size window) right)
				       (+ (tv:sheet-bottom-margin-size window) bottom))
		    (scl:letf (((tv:sheet-clipping-region window) cr))
		      (kernel window))))))))))))


;;;; Drawing primitives

(defmacro round-points (&rest numbers)
  `(progn ,@(do* ((numbers numbers (cdr numbers))
		  (number (first numbers) (first numbers))
		  (forms nil))
		 ((null numbers) (nreverse forms))
	      (push `(unless (integerp ,number)
		       (setf ,number (round ,number)))
		    forms))
	  (values)))

(defmethod draw-point-internal ((stream sheet-implementation-mixin) x-offset y-offset
				x y ink line-style)
  (when (window-drawing-possible stream)
    (round-points x y)
    (translate-positions x-offset y-offset x y)
    (with-appropriate-drawing-state stream ink line-style
      #'(lambda (window alu)
	  (declare (sys:downward-function))
	  (funcall window :draw-point x y alu))
      #'(lambda (window)
	  (declare (sys:downward-function))
	  (funcall (flavor:generic graphics:draw-point) window x y)))))

(defmethod draw-line-internal ((stream sheet-implementation-mixin) x-offset y-offset
			       start-x start-y end-x end-y ink line-style)
  (when (window-drawing-possible stream)
    (round-points start-x start-y end-x end-y)
    (translate-positions x-offset y-offset start-x start-y end-x end-y)
    (with-appropriate-drawing-state stream ink line-style
      #'(lambda (window alu)
	  (declare (sys:downward-function))
	  (funcall window :draw-line start-x start-y end-x end-y alu t))
      #'(lambda (window)
	  (declare (sys:downward-function))
	  (funcall (flavor:generic graphics:draw-line) window start-x start-y end-x end-y)))))

(defmethod draw-rectangle-internal ((stream sheet-implementation-mixin) x-offset y-offset
				    left top right bottom ink line-style)
  (when (window-drawing-possible stream)
    (round-points left top right bottom)
    (translate-positions x-offset y-offset left top right bottom)
    (if (and (null line-style)
	     (typep ink 'pattern))
	;; Looks like this is DRAW-ICON*
	(draw-icon-internal stream left top right bottom ink)
	(with-appropriate-drawing-state stream ink line-style
	  #'(lambda (window alu)
	      (declare (sys:downward-function))
	      (if (null line-style)
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
		       :filled (null line-style)))))))

;; Fall back to DRAW-IMAGE.  INK will be a pattern.
;;--- We have to resort to this because Genera always tiles.  Sigh.
(defmethod draw-icon-internal ((stream sheet-implementation-mixin) left top right bottom ink)
  (with-slots (window ink-cache) stream
    (let* ((width  (- right left))
	   (height (- bottom top))
	   (alus (or (ink-cache-lookup ink-cache ink)
		     (ink-cache-replace ink-cache ink (sheet-decode-ink ink stream)))))
      (assert (= (length alus) 1) ()
	      "CLIM only supports bitmap icons under Genera sheets")
      (dolist (alu alus)
	(let ((image (pop alu)))
	  (graphics:draw-image image left top
			       :image-right width :image-bottom height
			       :stream window))))))

(defmethod draw-polygon-internal ((stream sheet-implementation-mixin) x-offset y-offset
				  points closed ink line-style)
  (macrolet ((pop-x (p)
	       `(let ((x (pop ,p)))
		  (+ (if (integerp x) x (round x)) x-offset)))
	     (pop-y (p)
	       `(let ((y (pop ,p)))
		  (+ (if (integerp y) y (round y)) y-offset))))
    (when (window-drawing-possible stream)
      (let ((npoints (length points)))
	(with-appropriate-drawing-state stream ink line-style
	  #'(lambda (window alu)
	      (declare (sys:downward-function))
	      (if (null line-style)
		  (case npoints
		    (6 (let* ((p points)
			      (x1 (pop-x p)) (y1 (pop-y p))
			      (x2 (pop-x p)) (y2 (pop-y p))
			      (x3 (pop-x p)) (y3 (pop-y p)))
			 (funcall window :draw-triangle x1 y1 x2 y2 x3 y3 alu)))
		    (8 (let* ((p points)
			      (x1 (pop-x p)) (y1 (pop-y p))
			      (x2 (pop-x p)) (y2 (pop-y p))
			      (x3 (pop-x p)) (y3 (pop-y p))
			      (x4 (pop-x p)) (y4 (pop-y p)))
			 (funcall window :draw-triangle x1 y1 x2 y2 x3 y3 alu)
			 (funcall window :draw-triangle x3 y3 x4 y4 x1 y1 alu)))
		    (otherwise
		      (with-stack-array (translated-points npoints)
			(do ((i 0)
			     (p points))
			    ((null p))
			  (setf (aref translated-points (shiftf i (1+ i))) (pop-x p))
			  (setf (aref translated-points (shiftf i (1+ i))) (pop-y p)))
			(graphics::triangulate-polygon
			  #'(lambda (x1 y1 x2 y2 x3 y3)
			      (funcall window :draw-triangle x1 y1 x2 y2 x3 y3 alu))
			  translated-points))))
		  (with-stack-array (lines (* (+ npoints (if closed 0 -2)) 2))
		    (let* ((p points)
			   (i 0)
			   (initial-x (pop-x p))
			   (initial-y (pop-y p))
			   (x initial-x)
			   (y initial-y))
		      (loop
			(setf (aref lines (shiftf i (1+ i))) x)
			(setf (aref lines (shiftf i (1+ i))) y)
			(setf (aref lines (shiftf i (1+ i))) (setq x (pop-x p)))
			(setf (aref lines (shiftf i (1+ i))) (setq y (pop-y p)))
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
		  (setf (aref translated-points (shiftf i (1+ i))) (pop-x p))
		  (setf (aref translated-points (shiftf i (1+ i))) (pop-y p)))
		(if (null line-style)
		    (funcall (flavor:generic graphics:draw-polygon) window translated-points
			     :filled t)
		    (funcall (flavor:generic graphics:draw-lines) window translated-points
			     :closed closed)))))))))

(defmethod draw-ellipse-internal ((stream sheet-implementation-mixin) x-offset y-offset
				  center-x center-y
				  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				  start-angle end-angle ink line-style)
  (when (window-drawing-possible stream)
    (round-points center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
    (translate-positions x-offset y-offset center-x center-y)
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
	  (with-appropriate-drawing-state stream ink line-style
	    #'(lambda (window alu)
		(declare (sys:downward-function))
		(if (null line-style)
		    (funcall window :draw-filled-in-circle center-x center-y x-radius alu)
		    (funcall window :draw-circle center-x center-y x-radius alu)))
	    #'(lambda (window)
		(declare (sys:downward-function))
		(funcall (flavor:generic graphics:draw-ellipse) window
			 center-x center-y x-radius y-radius
			 :filled (null line-style))))
	;; For general ellipse, let Genera do all the work
	(with-appropriate-drawing-state stream ink line-style
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
			 :filled (null line-style)))))))))

(defmethod draw-string-internal ((stream sheet-implementation-mixin) x-offset y-offset
				 string x y start end align-x align-y text-style ink)
  (when (window-drawing-possible stream)
    (round-points x y)
    (translate-positions x-offset y-offset x y)
    (unless end
      (setq end (length string)))
    ;; ---OK to use this in the Genera implementation
    (sys:stack-let ((substring (make-string (- end start))))
      (replace substring string :start2 start :end2 end)
      (let* ((font (text-style-mapping (slot-value stream 'display-device-type)
				       text-style *standard-character-set*
				       (slot-value stream 'window)))
	     (height (sys:font-char-height font))
	     (descent (- height (sys:font-baseline font)))
	     (ascent (- height descent)))
	(incf x (clim-internals::compute-text-x-adjustment 
		  align-x stream string text-style start end))
	(incf y (clim-internals::compute-text-y-adjustment
		  align-y descent ascent height))
	(with-stack-list (style :device-font (tv:font-name font) :normal)
	  (with-appropriate-drawing-state stream ink nil
	    #'(lambda (window alu)
		(declare (sys:downward-function))
		(funcall window :draw-string substring x y (1+ x) y nil style alu))
	    #'(lambda (window)
		(declare (sys:downward-function))
		(funcall (flavor:generic graphics:draw-string) window
			 substring x y :character-style style))))))))

(defmethod draw-character-internal ((stream sheet-implementation-mixin) x-offset y-offset
				    character x y align-x align-y text-style ink)
  (when (window-drawing-possible stream)
    (round-points x y)
    (translate-positions x-offset y-offset x y)
    (let* ((font (text-style-mapping (slot-value stream 'display-device-type)
				     text-style *standard-character-set*
				     (slot-value stream 'window)))
	   (height (sys:font-char-height font))
	   (descent (- height (sys:font-baseline font)))
	   (ascent (- height descent)))
      (incf x (clim-internals::compute-text-x-adjustment 
		align-x stream character text-style))
      (incf y (clim-internals::compute-text-y-adjustment
		align-y descent ascent height))
      (with-appropriate-drawing-state stream ink nil
	#'(lambda (window alu)
	    (declare (sys:downward-function))
	    (funcall window :draw-glyph (char-code character) font x y alu))
	#'(lambda (window)
	    (declare (sys:downward-function))
	    (funcall (flavor:generic graphics:draw-glyph) window
		     (char-code character) font x y))))))


;;; 13 November 1989: Optimization to make stream-glyph-for-character faster in the case where
;;; we are calling it a lot, in stream-scan-string-for-writing.  We make this function be a
;;; macro.

;;; This algorithm is a paraphrase of the apparent contents of TV:%DRAW-STRING-CLIPPED-INTERNAL
(defmacro with-sheet-stream-glyph-for-character (&body body)
  `(macrolet ((stream-glyph-for-character (stream character style &optional our-font)
		`(with-slots (display-device-type) ,stream
		   (multiple-value-bind (character-set index)
		       (char-character-set-and-index ,character)
		     ;; For now we are asserting that each string passed to WRITE-STRING will
		     ;; have no style changes within it.  This is what our-font is all
		     ;; about.
		     (let* ((font (or our-font (text-style-mapping
						 display-device-type ,style
						 character-set nil)))
			    (FIT (sys:font-indexing-table font))
			    (LKT (sys:font-left-kern-table font))
			    (CWT (sys:font-char-width-table font))
			    (escapement-x (if (diacritic-char-p ,character) 0
					      (if CWT (aref CWT index)
						  (sys:font-char-width font))))
			    (escapement-y 0)
			    (origin-x (if LKT (aref LKT index) 0))
			    (origin-y (sys:font-baseline font))
			    (bb-x (if FIT (- (aref FIT (1+ index)) (aref FIT index))
				      (sys:font-char-width font)))
			    (bb-y (sys:font-char-height font)))
		       (values index font escapement-x escapement-y origin-x origin-y
			       bb-x bb-y (not (or FIT LKT CWT))))))))
     ,@body))

(defmethod stream-glyph-for-character ((stream sheet-implementation-mixin)
				       character style &optional our-font)
  (declare (values index font escapement-x escapement-y origin-x origin-y bb-x bb-y
		   fixed-width-font-p))
  (with-sheet-stream-glyph-for-character
    (stream-glyph-for-character stream character style our-font)))

(defmethod stream-scan-string-for-writing ((stream sheet-implementation-mixin) string
					   start end style cursor-x max-x
					   &optional glyph-buffer)
  (with-sheet-stream-glyph-for-character 
    (stream-scan-string-for-writing-body)))

(defmethod stream-write-char-1 ((stream sheet-implementation-mixin) index font color x y)
  (let ((window (slot-value stream 'window)))
    (when (window-drawing-possible stream)
      (tv:prepare-sheet (window)
	(let ((x (+ x (tv:sheet-left-margin-size window)))
	      (y (+ y (tv:sheet-top-margin-size window))))
	  ;; There is apparently no way to draw a glyph exactly where you want
	  ;; it on a sheet!  Draw it on the screen instead...
	  (tv:sheet-draw-glyph index font x y
			       (if (eq color (medium-foreground stream))
				   (tv:sheet-char-aluf window)
				 (sheet-decode-color (follow-indirect-ink color stream)
						     stream nil))
			       window))))))

(defmethod stream-write-string-1 ((stream sheet-implementation-mixin)
				  glyph-buffer start end font color x y)
  (when (<= end start) (return-from stream-write-string-1))
  (let ((window (slot-value stream 'window)))
    (when (window-drawing-possible stream)
      (tv:prepare-sheet (window)
	(let ((glyph-buffer glyph-buffer)
	      (index start)
	      (index8 0)
	      (x (+ x (tv:sheet-left-margin-size window)))
	      (y (+ y (tv:sheet-top-margin-size window))))
	  (declare (sys:array-register glyph-buffer))
	  (sys:with-stack-array (glyphs8 (- end start) :element-type 'string-char)
	    (declare (sys:array-register glyphs8))
	    (loop
	      (when (= index end) (return))
	      (setf (aref glyphs8 index8) (code-char (aref glyph-buffer index)))
	      (incf index)
	      (incf index8))
	    (tv:sheet-draw-string window
				  (if (eq color (medium-foreground stream))
				      (tv:sheet-char-aluf window)
				    (sheet-decode-color (follow-indirect-ink color stream)
							stream nil))
				  x y glyphs8 font 0 index8
				  (tv:sheet-width window))))))))


(defmethod text-style-height ((text-style standard-text-style)
			      (stream sheet-implementation-mixin))
  (with-slots (display-device-type window) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      (sys:font-char-height font))))

(defmethod text-style-width ((text-style standard-text-style)
			     (stream sheet-implementation-mixin))
  (with-slots (display-device-type window) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      (sys:font-char-width font))))

(defmethod text-style-ascent ((text-style standard-text-style)
			      (stream sheet-implementation-mixin))
  (with-slots (display-device-type window) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      (let* ((height (sys:font-char-height font))
	     (descent (- height (sys:font-baseline font)))
	     (ascent (- height descent)))
	ascent))))

(defmethod text-style-descent ((text-style standard-text-style)
			       (stream sheet-implementation-mixin))
  (with-slots (display-device-type window) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      (let* ((height (sys:font-char-height font))
	     (descent (- height (sys:font-baseline font))))
	descent))))

(defmethod text-style-fixed-width-p ((text-style standard-text-style)
				     (stream sheet-implementation-mixin))
  (with-slots (display-device-type window) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      (null (sys:font-char-width-table font)))))


;;; This whole file is #+Genera, right?

(scl:defflavor clim-sheet
	((selected-inferior nil))
	(dw::margin-mixin			;scroll bars, labels, borders
	 tv:no-screen-managing-mixin		;don't auto-select a pane
	 tv:window)
  (:default-init-plist :borders nil
		       :save-bits nil		;adjusted later if top-level
		       :deexposed-typeout-action :permit
		       :blinker-p nil
		       :label nil))

(scl:defflavor temp-clim-sheet
	()
	(tv:temporary-window-mixin clim-sheet))

;; All the sheet windows in a given activity (top level window) share the same io-buffer, and
;; CLIM decides which CLIM window to give the input to.
;; A top-level window should have a bit save array if we want the screen manager
;; to do what users expect.
(scl:defmethod (:init clim-sheet :before) (args)
  (declare (ignore args))
  (let ((top-level (not (typep tv:superior 'clim-sheet))))
    (unless tv:io-buffer
      (setq tv:io-buffer (if top-level
			     (tv:make-io-buffer 100)
			   (scl:send tv:superior :io-buffer))))
    (when top-level
      ;; This is like :save-bits :delayed
      (setf (tv:sheet-save-bits) 1))))

(scl:defmethod (:init temp-clim-sheet :after) (args)
  (declare (ignore args))
  (setf (tv:sheet-save-bits) 0))

;;; Interface to Genera scroll bars.
(scl:defmethod (:y-scroll-position clim-sheet) ()
  (declare (values top-displayed height-displayed minimum-y maximum-y))
  (let* ((stream (sheet-for-genera-window scl:self :error-if-no-match nil))
	 (history (and stream (stream-output-history stream)))
	 (viewport (and stream (window-viewport stream))))
    (cond ((and viewport history)
	   (with-bounding-rectangle* (vleft vtop vright vbottom) viewport
	     (declare (ignore vleft vright))
	     (with-bounding-rectangle* (hleft htop hright hbottom) history
	       (declare (ignore hleft hright))
	       (values vtop (- vbottom vtop) htop hbottom))))
	  (t (values 0 0 0 0)))))

(scl:defmethod (:x-scroll-position clim-sheet) ()
  (let* ((stream (sheet-for-genera-window scl:self :error-if-no-match nil))
	 (history (and stream (stream-output-history stream)))
	 (viewport (and stream (window-viewport stream))))
    (cond ((and viewport history)
	   (with-bounding-rectangle* (vleft vtop vright vbottom) viewport
	     (declare (ignore vtop vbottom))
	     (with-bounding-rectangle* (hleft htop hright hbottom) history
	       (declare (ignore htop hbottom))
	       (values vleft (- vright vleft) hleft hright))))
	  (t (values 0 0 0 0)))))

(defun calculate-new-viewport-position (stream x-or-y type position
					&optional (context-nlines 1))
  (declare (ignore context-nlines))
  (declare (values viewport-left viewport-top))
  (let ((viewport (window-viewport stream)))
    (ecase x-or-y
      (:y
	(ecase type
	  (:relative-jump
	    (values (bounding-rectangle-left viewport)
		    (+ (bounding-rectangle-top viewport)
		       (* position (stream-line-height stream))))
	    #+Ignore
	    (unless (zerop position)
	      (let ((index viewport-displayed-strings-start-index)
		    (max-index (fill-pointer displayed-strings))
		    (y (box-top cursor-viewport))
		    (inc (signum position)))
		(labels ((index-ok-p (index)
			   (and ( index 0)
				(< index max-index))))
		  (loop repeat (abs position)
			do (loop doing
			     (let ((new-index (+ index inc)))
			       (unless (index-ok-p new-index)
				 (loop-finish))
			       (setq index new-index))
			     (let ((new-top (box-top (presentation-displayed-box
						       (aref displayed-strings index)))))
			       (when (-control-z-char- new-top y)
				 (setq y new-top)
				 (loop-finish))))
			while (index-ok-p index))
		  (when (index-ok-p index)
		    (let ((new-top (box-top (presentation-displayed-box
					      (aref displayed-strings index)))))
		      (values (box-left cursor-viewport) new-top)))))))
	  (:relative 
	    (values (bounding-rectangle-left viewport)
		    (+ position (bounding-rectangle-top viewport))))
	  (:absolute
	    (values (bounding-rectangle-left viewport)
		    position))
	  (:screenful
	    (with-bounding-rectangle* (vleft vtop vright vbottom) viewport
	      (declare (ignore vright))
	      (let ((new-top (+ vtop (* position (- vbottom vtop)))))
		(setq new-top (min new-top (- vbottom vtop)))
		(values vleft new-top))))
	  ))
      (:x
	(ecase type
	  (:relative-jump
	    (calculate-new-viewport-position
	      stream :x :relative
	      (* position (stream-character-width stream #\Space))))
	  (:relative
	    (values (+ (bounding-rectangle-left viewport) position)
		    (bounding-rectangle-top viewport)))
	  (:absolute
	    (values position
		    (bounding-rectangle-top viewport))))))))

(scl:defmethod (:y-scroll-to clim-sheet) (position type)
  (let* ((stream (sheet-for-genera-window scl:self))
	 (history (stream-output-history stream))
	 (left (bounding-rectangle-left (window-viewport stream))))
    (multiple-value-bind (ignore top)
	(calculate-new-viewport-position stream :y type position)
      (declare (ignore ignore))
      (when history
	(with-bounding-rectangle* (hleft htop hright hbottom) history
	  (declare (ignore hleft hright))
	  (setq top (min (max htop top) hbottom))))
      (window-set-viewport-position stream left top))))

(scl:defmethod (:x-scroll-to clim-sheet) (position type)
  (let* ((stream (sheet-for-genera-window scl:self))
	 (history (stream-output-history stream))
	 (top (bounding-rectangle-top (window-viewport stream))))
    (let ((left (calculate-new-viewport-position stream :x type position)))
      (when history
	(with-bounding-rectangle* (hleft htop hright hbottom) history
	  (declare (ignore htop hbottom))
	  (setq left (min (max left hleft) hright))))
      (window-set-viewport-position stream left top))))

;;; The window manager should manipulate the highest "CLIM sheet" when invoked
;;; on an inferior.
(scl:defmethod (:alias-for-selected-windows clim-sheet) ()
  (if (typep tv:superior 'clim-sheet)
      (scl:send tv:superior :alias-for-selected-windows)
      scl:self))

(scl:defmethod (:name-for-selection clim-sheet) ()
  (or (let ((frame (window-stream-to-frame (sheet-for-genera-window scl:self))))
	(and frame (frame-pretty-name frame)))
      (let ((label (scl:send scl:self :label)))
	(and label (scl:string-search-not-char #\sp label) label))
      tv:name))

;;; A combination of the methods of pane-mixin and basic-frame
(scl:defwhopper (:select clim-sheet) (&rest args)
  (when (scl:send tv:superior :inferior-select scl:self)
    (cond ((and selected-inferior
		(scl:send selected-inferior :exposed-p))
	   (scl:lexpr-send selected-inferior :select args))
	  ((and (null selected-inferior)
		tv:inferiors
		(let ((frame (window-stream-to-frame (sheet-for-genera-window scl:self))))
		  (when frame
		    ;; This is a frame whose input focus has not yet been directed to any pane
		    ;; Choose the interactor pane by default
		    (let ((stream (frame-query-io frame)))
		      (when stream
			(scl:lexpr-send (slot-value stream 'window) :select args)
			t))))))
	  (t
	   (scl:lexpr-continue-whopper args)))))

;;; As for basic-frame
(scl:defmethod (:inferior-select clim-sheet) (pane)
  (setq selected-inferior pane)
  (scl:send tv:superior :inferior-select scl:self))

;;; As for basic-frame
(scl:defwhopper (:select-relative clim-sheet) ()
  (if (and selected-inferior
	   (scl:send selected-inferior :exposed-p))
      (scl:send selected-inferior :select-relative)
      (scl:continue-whopper)))

;;; As for pane-mixin
;;; "When selecting a pane with the mouse, pass the selection request to the frame."
(scl:defwhopper (:mouse-select clim-sheet) (&rest args)
  (if (typep tv:superior 'clim-sheet)
      (scl:lexpr-send tv:superior ':mouse-select args)
      (scl:lexpr-continue-whopper args)))

(scl:defmethod (:expose clim-sheet :after) (&rest args)
  (declare (ignore args))
  (scl:send tv:superior :inferior-expose scl:self))

(scl:defmethod (:inferior-expose clim-sheet) (inferior)
  (when (eq inferior selected-inferior)
    (scl:send inferior :select-relative)))

(scl:defmethod (tv:refresh-rectangle clim-sheet) (left top right bottom)
  (let ((window (sheet-for-genera-window scl:self)))
    (setf (slot-value window 'highlighted-presentation) nil)
    (multiple-value-bind (viewport-x viewport-y)
	(bounding-rectangle-position (window-viewport window))
      (frame-replay *application-frame*
		    window (make-bounding-rectangle (+ left viewport-x)
						    (+ top viewport-y)
						    (+ right viewport-x)
						    (+ bottom viewport-y))))
    (tv:refresh-margins-rectangle scl:self left top right bottom)))

;;; This is a hideous kludge that attempts to address the problems with moving windows
;;; within their parents.
(scl:defwhopper (:set-position clim-sheet) (new-x new-y &optional verify)
  (ecase verify
    ((:verify)
     (scl:continue-whopper new-x new-y verify))
    ((nil)
     (let ((old-x tv:x-offset) (old-y tv:y-offset))
       (multiple-value-prog1 (scl:continue-whopper new-x new-y verify)
	 (unless (or *synchronous-window-operation-being-processed*
		     (and (= old-x tv:x-offset) (= old-y tv:y-offset)))
	   (scl:send scl:self :force-kbd-input
		     (make-window-size-or-position-change-event
		       (sheet-for-genera-window scl:self)
		       nil nil nil nil))))))))

(scl:defmethod (:refresh clim-sheet :before) (&optional (type :complete-redisplay))
  (declare (ignore type))
  (let ((stream (sheet-for-genera-window scl:self)))
    ;; Invalidate all cached inks, so they'll be redecoded taking into account any
    ;; changed parameters such as reversed video.
    (let ((cache (slot-value stream 'ink-cache)))
      (dotimes (i (array-total-size cache))
	(setf (aref cache i) nil)))
    (let ((foreground (sheet-decode-color (medium-foreground stream) stream nil))
	  (background (sheet-decode-color (medium-background stream) stream nil)))
      (funcall scl:self :set-char-aluf foreground)
      (funcall scl:self :set-erase-aluf background))))

(scl:defmethod (:refresh clim-sheet :after) (&optional (type :complete-redisplay))
  (declare (ignore type))
  (let ((window (sheet-for-genera-window scl:self)))
    (setf (slot-value window 'highlighted-presentation) nil)
    (unless *sizing-application-frame*		;avoid replaying twice
      (frame-replay *application-frame* window (window-viewport window)))))

(scl:defwhopper (:change-of-size-or-margins clim-sheet) (&rest args)
  (let ((old-x tv:x-offset) (old-y tv:y-offset)
	(old-width tv:width) (old-height tv:height))
    (multiple-value-prog1 (scl:lexpr-continue-whopper args)
      (unless (or *synchronous-window-operation-being-processed*
		  (and (= old-x tv:x-offset) (= old-y tv:y-offset)
		       (= old-width tv:width) (= old-height tv:height)))
	(scl:send scl:self :force-kbd-input
		  (make-window-size-or-position-change-event
		    (sheet-for-genera-window scl:self)
		    nil nil nil nil))))))

(scl:defwhopper (:set-reverse-video-p clim-sheet) (reverse-video-p)
  (let ((old-reverse-video-p (scl:send scl:self :reverse-video-p)))
    (multiple-value-prog1
      (scl:continue-whopper reverse-video-p)
      (unless (eq reverse-video-p old-reverse-video-p)
	(labels ((switch-inks (window)
		   ;; Inhibit refreshing until we're done
		   (rotatef (slot-value window 'foreground)
			    (slot-value window 'background))
		   (dolist (window (window-children window))
		     (switch-inks window))))
	  (declare (dynamic-extent #'switch-inks))
	  (switch-inks (sheet-for-genera-window scl:self))
	  (scl:send scl:self :refresh))))))

(scl:defmethod (:convert-mouse-coords clim-sheet) (x y in-or-out)
  (case in-or-out
    (:in (values (- x tv:left-margin-size)
		 (- y tv:top-margin-size)))
    (:out (values (+ x tv:left-margin-size)
		  (+ y tv:top-margin-size)))))

(scl:defmethod (with-clim-sheet-clipping-region clim-sheet)
	       (left top right bottom continuation stream)
  (tv:with-sheet-clipping-region ((+ left tv:left-margin-size) (+ top tv:top-margin-size)
				  (+ right tv:left-margin-size) (+ bottom tv:top-margin-size))
    (declare (sys:downward-function))		;yecch
    (funcall continuation stream)))

(scl:compile-flavor-methods clim-sheet temp-clim-sheet)


;;;

(defclass sheet-window-stream
	  (sheet-implementation-mixin window-stream)
    ()
  (:default-initargs :display-device-type nil
		     :text-cursor (make-instance 'sheet-text-cursor)))

(defun create-sheet-root-window (&key (screen tv:main-screen))
  (make-instance 'sheet-window-stream
		 :window screen
		 :label nil
		 :scroll-bars nil
		 :display-device-type
		   (let (#+IMach (device (scl:send screen :display-device-type)))
		     (cond
		       #+IMach
		       ((and (find-package 'mtb)
			     (typep device
				    (intern 
				      (symbol-name 
					'small-screen-genera-fonts-mac-display-device)
				      :mtb)))
			*small-sheet-device*)
		       (t *sheet-device*)))))


;; :SHEET and :GENERA are the same thing...
(define-implementation :sheet  'create-sheet-root-window)
(define-implementation :genera 'create-sheet-root-window)

(defmethod initialize-instance :after ((stream sheet-window-stream)
				       &key parent left top right bottom label
					    (scroll-bars ':both) (borders t)
					    window save-under)
  ;; --- can we do this error checking more generally?
  (cond ((null parent)
	 ;; :PARENT NIL means open the root window.
	 (unless window
	   (error "You must supply a parent window stream or explicitly initilize the WINDOW slot."))
	 ;; parent nil means 
	 (when (or left top right bottom)
	   (error "You cannot specify the size of the root window (i.e. when :PARENT is NIL)."))
	 (with-slots (left top right bottom) stream
	   (multiple-value-bind (w h) (scl:send window :size)
	     (setf left (tv:sheet-x window)
		   top (tv:sheet-y window)
		   right (+ (tv:sheet-x window) w)
		   bottom (+ (tv:sheet-y window) h))))
	 (setf (stream-pointers stream)
	       (list (make-instance 'standard-pointer :root stream))))
	(t
	 (with-slots (display-device-type) stream
	   (unless display-device-type
	     (setf display-device-type (slot-value parent 'display-device-type))))
	 ;; When parent is supplied, open an inferior
	 (unless (and left top right bottom)
	   (error "You must supply :left :top :right and :bottom when creating a window"))
	 (let ((parent-window (slot-value parent 'window)))
	   (when parent-window
	     (multiple-value-bind (wl wt)
		 (scl:send parent-window :inside-edges)
	       (translate-positions wl wt left top right bottom)))
	   (let ((sheet (tv:make-window (if save-under 'temp-clim-sheet 'clim-sheet)
					:superior parent-window
					:left left	;see above
					:top top
					:right right
					:bottom bottom
					;; :edges (list left top right bottom)
					:margin-components (clim-sheet-margin-components
							     label scroll-bars borders))))
	     (setf (slot-value stream 'window) sheet)
	     (setf (slot-value stream 'color-p) (color:color-stream-p sheet))
	     ;; EMBEDDED-P indicates that the screen implementation has fast, specific
	     ;; methods for implementing the Genera graphics substrate.  I'm not sure
	     ;; if the use of basic-remote-screen is the right test, but it's quick...
	     (setf (slot-value stream 'embedded-p)
		   (typep (tv:sheet-screen sheet) 'tv:basic-remote-screen))
	     (scl:send sheet :set-char-aluf
		       (sheet-decode-color (medium-foreground stream) stream nil))
	     (scl:send sheet :set-erase-aluf
		       (sheet-decode-color (medium-background stream) stream nil))))))
  (associate-sheet-with-genera-window (slot-value stream 'window) stream))

;;; The margin stuff seems to have changed.
(defmethod initialize-instance :around ((stream sheet-window-stream) &key label)
  (call-next-method)
  (when (slot-value stream 'parent)
    (scl:send (slot-value stream 'window) :set-label " ")
    (scl:send (slot-value stream 'window)
	      :set-label (clim-label->genera-label label stream))))

(defmethod (setf medium-foreground) :after (ink (stream sheet-window-stream))
  (with-slots (window) stream	   
    (scl:send window :set-char-aluf (sheet-decode-color ink stream nil))
    (scl:send window :refresh)))

(defmethod (setf medium-background) :after (ink (stream sheet-window-stream))
  (with-slots (window) stream	   
    (scl:send window :set-erase-aluf (sheet-decode-color ink stream nil))
    (scl:send window :refresh)))

(defun clim-sheet-margin-components (label-p scroll-bar borders-p)
  `(,@(and borders-p
	   '((dw::margin-borders )))
    ,@(and (member scroll-bar '(:vertical :both))
	   '((dw::margin-scroll-bar :history-noun "history")))
    ,@(and (member scroll-bar '(:horizontal :both))
	   '((dw::margin-scroll-bar :margin :bottom :history-noun "history")))
    ,@(and borders-p
	   '((dw::margin-white-borders :thickness 2)))
    ,@(and label-p
	   '((dw::margin-label )))))

;;; Since sheets have no concept of a "region", we'll just use a Y rectangle
;;; to represent one.

;;--- This should clip to real regions, not just to bounding-rectangles
(defmethod invoke-with-clipping-region
	   ((stream sheet-implementation-mixin) continuation (region standard-bounding-rectangle))
  (let ((window (slot-value stream 'window)))
    (multiple-value-bind (vx vy) (window-viewport-position stream)
      (declare (type coordinate vx vy))
      (multiple-value-bind (ml mt) (window-margins stream)
	(declare (type coordinate ml mt))
	(with-bounding-rectangle* (left top right bottom) region
	  ;;--- where should this FIXing be done?
	  (fix-points left top right bottom vx vy ml mt)
	  (with-clim-sheet-clipping-region window
					   (+ (- left vx) ml) (+ (- top vy) mt)
					   (+ (- right vx) ml) (+ (- bottom vy) mt)
					   continuation stream))))))


;;; Input side

(defmethod stream-clear-input :after ((stream sheet-window-stream))
  (with-slots (window) stream
    (scl:send window :clear-input)))

(defun mouse-char-bits->modifier-state (bits)
  (let ((mask 0))
    (macrolet ((do-shift (shift)
		 `(when (si:bit-test (si:name-bit ,shift) bits)
		    (let ((bit (clim-internals::modifier-key-index ,shift)))
		      (setf mask (dpb 1 (byte 1 bit) mask))))))
      ;; why is SHIFT different from sys:%kbd-shifts-shift
      (when (ldb-test (byte 1 4) bits)
	(let ((bit (clim-internals::modifier-key-index :shift)))
	  (setf mask (dpb 1 (byte 1 bit) mask))))
      (do-shift :control)
      (do-shift :meta)
      (do-shift :super)
      (do-shift :hyper))
    mask))

(defmethod window-modifier-state ((stream sheet-implementation-mixin))
  (let* ((window (slot-value stream 'window))
	 (mouse (tv:sheet-mouse window)))
    (let ((shifts (tv:mouse-chord-shifts mouse)))
      (mouse-char-bits->modifier-state shifts))))

(defmethod set-pointer-window-and-location ((stream sheet-window-stream) pointer)
  (let ((mouse (tv:sheet-mouse (slot-value stream 'window))))
    (setf (pointer-window pointer)
	  (sheet-for-genera-window (tv:window-under-mouse-internal mouse)
				       :error-if-no-match nil))
    (pointer-set-position pointer (tv:mouse-x mouse) (tv:mouse-y mouse))))

(defmethod stream-event-handler ((stream sheet-window-stream)
				 &key (timeout nil) (input-wait-test nil))
  ;;--- Have to establish correspondence between pointing devices and pointer objects
  (with-slots (window) stream
    (let* ((sys:rubout-handler nil)
	   (mouse (tv:sheet-mouse window))
	   (pointer (stream-primary-pointer stream))
	   (old-buttons (tv:mouse-last-buttons mouse))
	   (old-x (pointer-x-position pointer))
	   (old-y (pointer-y-position pointer))
	   ;;--- Why doesn't the pointer object remember these?
	   (old-shifts (tv:mouse-chord-shifts mouse))
	   (old-window (pointer-window pointer))
	   (what-happened (if timeout :timeout :mouse-motion))
	   ;; If we're inside an encapsulating stream, such as the input editor, and a window
	   ;; resizing event is received, don't get confused while handling the event
	   (*original-stream* nil))
      (labels ((mouse-motion-pending ()
		 (or (/= (tv:mouse-chord-shifts mouse) old-shifts)
		     (/= old-buttons (tv:mouse-last-buttons mouse))
		     (/= old-x (mouse-x))
		     (/= old-y (mouse-y))
		     (not (eq old-window (sheet-for-genera-window
					   (tv:window-under-mouse-internal mouse)
					   :error-if-no-match nil)))))
	       (something-pending (sheet)
		 (cond ((not (tv:sheet-console window)) nil)	;wait for console
		       ((not (tv:mouse-sheet mouse)) nil)	;..
		       ((and input-wait-test (funcall input-wait-test stream))
			(setq what-happened :input-wait-test))
		       ((mouse-motion-pending)
			(setq what-happened :mouse-motion)
			T)
		       ((scl:send-if-handles sheet :listen)
			(setq what-happened :listen)
			T)))
	       (mouse-x () (tv:mouse-x mouse))
	       (mouse-y () (tv:mouse-y mouse)))
	(declare (dynamic-extent #'mouse-motion-pending #'something-pending
				 #'mouse-x #'mouse-y))
	(cond ((eq timeout 0)			;avoid consing bignums
	       (something-pending window))
	      (timeout
	       (scl:process-wait-with-timeout si:*whostate-awaiting-user-input* (* timeout 60)
		 #'something-pending window))
	      (t
	       (scl:process-wait si:*whostate-awaiting-user-input*
		 #'something-pending window)))
	(ecase what-happened
	  (:listen
	    (let ((character (scl:send window :any-tyi)))
	      (cond ((typep character 'window-size-or-position-change-event)
		     (let* ((window (event-sheet character))
			    (window-win (slot-value window 'window))
			    (parent (window-parent window))
			    (parent-win (slot-value parent 'window)))
		       (multiple-value-bind (left top right bottom)
			   (scl:send window-win :edges)
			 (when (null (tv:sheet-superior parent-win))
			   (multiple-value-bind (wl wt)
			       (scl:send parent-win :inside-edges)
			     (translate-positions (- wl) (- wt) left top right bottom)))
			 (window-note-size-or-position-change
			   window left top right bottom))))
		    ((listp character)
		     (case (first character)
		       (:mouse-button
			 (scl:destructuring-bind (blip-type char mouse-window x y) character
			   (declare (ignore blip-type))
			   (setf (pointer-button-state pointer)
				 (tv:mouse-last-buttons mouse))
			   ;; we don't need to translate x and y by the root-offsets
			   ;; because they're already in window coordinates...
			   (let ((pw (sheet-for-genera-window mouse-window)))
			     (setf (pointer-window pointer) pw)
			     (multiple-value-bind (xm ym) (scl:send mouse-window :margins)
			       (stream-note-pointer-button-press
				 pw pointer (ash 1 (si:mouse-char-button char))
				 (mouse-char-bits->modifier-state (si:mouse-char-bits char))
				 (- x xm) (- y ym))))))))
		    (t (queue-put (stream-input-buffer stream) character)))))
	  (:mouse-motion
	    ;; ---
	    (pointer-set-position pointer (mouse-x) (mouse-y))
	    (let ((window (sheet-for-genera-window (tv:window-under-mouse-internal mouse)
						       :error-if-no-match nil)))
	      (setf (pointer-window pointer) window)
	      (when window
		(setf (pointer-motion-pending window pointer) T))))
	  (:input-wait-test  :input-wait-test)
	  (:timeout  :timeout))
	what-happened))))

(defmethod set-stream-pointer-in-screen-coordinates ((stream sheet-window-stream) pointer x y)
  (declare (ignore pointer))			;Sigh.
  (let ((mouse (tv:sheet-mouse (slot-value stream 'window))))
    (tv:mouse-warp x y mouse)))

(defmethod stream-pointer-input-rectangle* ((stream sheet-window-stream) pointer
					    &key left top right bottom)
  (declare (ignore pointer))
  (multiple-value-bind (dx dy) (window-viewport-position stream)
    (setq left (if (typep left 'real) (round left) dx))
    (setq top  (if (typep top  'real) (round top) dy))
    (setq right  (if (typep right  'real) (round right) (+ left 50)))
    (setq bottom (if (typep bottom 'real) (round bottom) (+ top 50)))
    ;; TV:MOUSE-RESHAPE-RECTANGLE deals with values in TV:MAIN-SCREEN coordinates.
    (multiple-value-bind (lm tm) (window-offset stream)
      (decf dx lm)
      (decf dy tm))
    (multiple-value-bind (bleft btop bright bbottom)
	(dw::box-edges 
	  (tv:mouse-reshape-rectangle
	    :initial-box (dw:make-box (- left dx) (- top dy) (- right dx) (- bottom dy))
	    :sheet (slot-value stream 'window)))
      (values (+ bleft dx)
	      (+ btop dy)
	      (+ bright dx)
	      (+ bbottom dy)))))

(defmethod mouse-documentation-window ((window sheet-implementation-mixin))
  (let ((console (tv:sheet-console (slot-value window 'window))))
    (if (eq console sys:*main-console*)
	tv:who-line-documentation-window
	(let ((who-screen (tv:console-who-line-screen console)))
	  (and who-screen
	       (tv:get-who-line-field :mouse-documentation who-screen))))))


;;; Text styles for sheets

(defclass sheet-device (display-device) ())

(defmethod text-style-mapping :around ((display-device sheet-device) style
				       &optional (character-set *standard-character-set*)
						 window)
  (declare (ignore window))
  (let* ((font-symbol (call-next-method))
	 (font (if (symbolp font-symbol) (symbol-value font-symbol) font-symbol)))
    (unless (typep font 'sys:font)
      (error "Mapping for ~A characters in ~A character set is not a font"
	     style character-set))
    font))

(defparameter *sheet-logical-size-alist*
	      '((:tiny       6)
		(:very-small 8)
		(:small	     10)
		(:normal     12)
		(:large	     14)
		(:very-large 20)
		(:huge	     24)))

(defmethod standardize-text-style ((display-device sheet-device) style
				   &optional (character-set *standard-character-set*))
  (standardize-text-style-1
    display-device style character-set *sheet-logical-size-alist*))

(define-display-device *sheet-device* sheet-device
  :font-for-undefined-style fonts:boxfont)

(define-text-style-mappings *sheet-device* *standard-character-set*
  (:family :fix (:face :roman (:size 20 fonts:bigfnt
				     14 fonts:medfnt
				     12 fonts:cptfont
				     10 fonts:tvfont
				      8 fonts:einy7
				      6 fonts:tiny)
		       :italic (:size 20 fonts:bigfnti
				      14 fonts:medfnti
				      12 fonts:cptfonti
				      10 fonts:tvfonti
				       8 fonts:einy7
				       6 fonts:tiny)
		       :bold (:size 20 fonts:bigfntb
				    14 fonts:medfntb
				    12 fonts:cptfontcb
				    10 fonts:tvfontb
				     8 fonts:einy7
				     6 fonts:tiny)
		       (:bold :italic) (:size 20 fonts:bigfntbi
					      14 fonts:medfntbi
					      12 fonts:cptfontbi
					      10 fonts:tvfontbi
					       8 fonts:einy7
					       6 fonts:tiny)
		       (:bold :extended) (:size 12 fonts:cptfontb
						10 fonts:tvfontb)
		       :condensed (:size 12 fonts:cptfontc)
		       (:extra :condensed) (:size 12 fonts:cptfontcc
						  10 fonts:tvfont))))

(define-text-style-mappings *sheet-device* *standard-character-set*
  (:family :serif (:face :roman (:size 20 fonts:dutch20
				       14 fonts:dutch14
				       12 fonts:tr12
				       10 fonts:tr10
				        8 fonts:tr8)
			 :italic (:size 20 fonts:dutch20I
					14 fonts:dutch14I
					12 fonts:tr12I
					10 fonts:tr10I
				 	 8 fonts:tr8I)
			 :bold (:size 20 fonts:dutch20B
				      14 fonts:dutch14B
				      12 fonts:tr12B
				      10 fonts:tr10B
				       8 fonts:tr8B)
			 (:bold :italic) (:size 20 fonts:dutch20BI
						14 fonts:dutch14BI
						12 fonts:tr12BI
						10 fonts:tr10BI
						 8 fonts:tr8BI))))

(define-text-style-mappings *sheet-device* *standard-character-set*
  (:family :sans-serif (:face :roman (:size 20 fonts:swiss20
					    14	fonts:hl14
					    12	fonts:hl12
					    10	fonts:hl10
					     8 fonts:hl8)
			      :italic (:size 20 fonts:swiss20I
					     14 fonts:hl14I
					     12 fonts:hl12I
					     10 fonts:hl10I
					      8 fonts:hl8I)
			      :bold (:size 20 fonts:swiss20B
					   14 fonts:hl14B
					   12 fonts:hl12B
					   10 fonts:hl10B
					    8 fonts:hl8B)
			      (:bold :italic) (:size 20 fonts:swiss20BI
						     14 fonts:hl14BI
						     12 fonts:hl12BI
						     10 fonts:hl10BI
						      8 fonts:hl8BI))))

#+IMach
(defclass small-sheet-device (sheet-device) ())

#+IMach
(defparameter *small-sheet-logical-size-alist*
	      '((:tiny       6)
		(:very-small 8)
		(:small	     10)
		(:normal     12)
		(:large	     14)
		(:very-large 20)
		(:huge	     24)))

#+IMach
(defmethod standardize-text-style ((display-device small-sheet-device) style
				   &optional (character-set *standard-character-set*))
  (standardize-text-style-1
    display-device style character-set *small-sheet-logical-size-alist*))

#+IMach
(define-display-device *small-sheet-device* small-sheet-device
  :font-for-undefined-style fonts:boxfont)

#+IMach
(define-text-style-mappings *small-sheet-device* *standard-character-set*
  (:family :fix (:face :roman (:size 20 fonts:medfnt
				     14 fonts:cptfont
				     12 fonts:tvfont
				     10 fonts:einy7
				      8 fonts:einy7
				      6 fonts:einy7)
		       :italic (:size 20 fonts:medfnti
				      14 fonts:cptfonti
				      12 fonts:tvfonti 
				      10 fonts:einy7
				       8 fonts:einy7
				       6 fonts:einy7)
		       :bold (:size 20 fonts:medfntb
				    14 fonts:cptfontcb
				    12 fonts:tvfontb 
				    10 fonts:einy7
				     8 fonts:einy7
				     6 fonts:einy7)
		       (:bold :italic) (:size 20 fonts:medfntbi
					      14 fonts:cptfontbi
					      12 fonts:tvfontbi
					      10 fonts:einy7
					       8 fonts:einy7
					       6 fonts:einy7)
		       (:bold :extended) (:size 12 fonts:tvfontb
						10 fonts:cptfontb)
		       :condensed (:size 12 fonts:cptfontc)
		       (:extra :condensed) (:size 12 fonts:cptfontcc
						  10 fonts:tvfont))))

#+IMach
(define-text-style-mappings *small-sheet-device* *standard-character-set*
  (:family :serif (:face :roman (:size 20 fonts:dutch14
				       14 fonts:tr12
				       12 fonts:tr10
				       10 fonts:tr8
				        8 fonts:tr8)
			 :italic (:size 20 fonts:dutch14I
					14 fonts:tr12I
					12 fonts:tr10I
					10 fonts:tr8I
					 8 fonts:tr8I)
			 :bold (:size 20 fonts:dutch14B
				      14 fonts:tr12B
				      12 fonts:tr10B
				      10 fonts:tr8B
				       8 fonts:tr8B)
			 (:bold :italic) (:size 20 fonts:dutch14BI
						14 fonts:tr12BI
						12 fonts:tr10BI
						10 fonts:tr8BI
						 8 fonts:tr8BI))))

#+IMach
(define-text-style-mappings *small-sheet-device* *standard-character-set*
  (:family :sans-serif (:face :roman (:size 20 fonts:hl14
					    14 fonts:hl12
					    12 fonts:hl10
					    10 fonts:hl8
					     8 fonts:hl8)
			      :italic (:size 20 fonts:hl14I
					     14 fonts:hl12I
					     12 fonts:hl10I
					     10 fonts:hl8I
					      8 fonts:hl8I)
			      :bold (:size 20 fonts:hl14B
					   14 fonts:hl12B
					   12 fonts:hl10B
					   10 fonts:hl8B
					    8 fonts:hl8B)
			      (:bold :italic) (:size 20 fonts:hl14BI
						     14 fonts:hl12BI
						     12 fonts:hl10BI
						     10 fonts:hl8BI
						      8 fonts:hl8BI))))


;;; Text Cursors for sheets

(defclass sheet-text-cursor
	  (cursor)
    ((x :initarg :x :initform 0)		;needed if cursor is scrolled off viewport
     (y :initarg :y :initform 0)
     (stream :initarg :stream)
     (active :initform nil)			;false if visibility is :inactive
     (blinker :initform nil)))			;Genera window system blinker to be used

(defmethod cursor-active-p ((cursor sheet-text-cursor))
  (not (null (slot-value cursor 'active))))

(defmethod (setf cursor-stream) (new-value (cursor sheet-text-cursor))
  (setf (slot-value cursor 'stream) new-value)
  (let ((blinker (slot-value cursor 'blinker)))
    (when blinker
      (tv:blinker-set-sheet blinker (slot-value new-value 'window))))
  new-value)

(defmethod cursor-position ((cursor sheet-text-cursor))
  (values (slot-value cursor 'x) (slot-value cursor 'y)))

(defmethod cursor-set-position ((cursor sheet-text-cursor) x y)
  (setf (slot-value cursor 'x) x
	(slot-value cursor 'y) y)
  (let ((blinker (slot-value cursor 'blinker)))
    (when blinker
      (multiple-value-bind (x y)
	  (drawing-surface-to-viewport-coordinates (slot-value cursor 'stream) x y)
	(tv:blinker-set-cursorpos blinker x y)))))

(defmethod cursor-visibility ((cursor sheet-text-cursor))
  (let ((active (slot-value cursor 'active))
	(blinker (slot-value cursor 'blinker)))
    (cond ((not active) :inactive)
	  ((not blinker) :off)
	  ((member (tv:blinker-visibility blinker) '(:blink :on t)) :on)
	  (t :off))))

(defmethod (setf cursor-visibility) (new-visibility (cursor sheet-text-cursor))
  (setf (slot-value cursor 'active) (not (eq new-visibility :inactive)))
  (let ((stream (slot-value cursor 'stream))
	(blinker (slot-value cursor 'blinker)))
    (when (and (null blinker) (eq new-visibility :on))
      ;; It isn't safe to create the blinker any earlier than this, because of the
      ;; order of initializations while creating a sheet-window-stream
      (setq blinker 
	    (setf (slot-value cursor 'blinker)
		  (flavor:make-instance 'tv:rectangular-blinker
					:sheet (slot-value stream 'window)
					:visibility nil
					:deselected-visibility nil
					:follow-p nil)))
      (multiple-value-bind (x y)
	  (drawing-surface-to-viewport-coordinates stream (slot-value cursor 'x)
							  (slot-value cursor 'y))
	(tv:blinker-set-cursorpos blinker x y)))
    (when blinker
      (tv:blinker-set-visibility blinker (if (eq new-visibility :on)
					     (if (tv:sheet-selected-p
						   (slot-value stream 'window))
						 :blink
						 :on)
					     nil))
      (scl:send blinker :set-deselected-visibility (if (eq new-visibility :on) :on nil))))
  new-visibility)

;; The window system draws it, so we don't have to
(defmethod draw-cursor ((cursor sheet-text-cursor) on-p)
  on-p)
