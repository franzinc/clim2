;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements the CLIM drawing medium, with support for rectangles *
*  points, lines, text and patterns                                          *
*                                                                            *
*                                                                            *
****************************************************************************|#


(in-package :acl-clim)

(defvar null 0)

(defclass acl-medium (basic-medium)
  ((background-dc-image)
   (foreground-dc-image)))

(defmethod engraft-medium :after ((medium acl-medium) port sheet)
  (declare (ignore port sheet))
  ;; In another engraft-medium method, the medium background/foreground
  ;; is initialized from the pane background/foreground.
  (with-slots (background-dc-image foreground-dc-image) medium
    (let ((bink 
	   (dc-image-for-ink medium (medium-background medium)))
	  (fink 
	   (dc-image-for-ink medium (medium-foreground medium))))
      (setf background-dc-image bink)
      (setf foreground-dc-image fink))))

(defmethod (setf medium-background) :after (new-background (medium acl-medium))
  (with-slots (background-dc-image) medium
    (let ((bink (dc-image-for-ink medium new-background)))
      (setf background-dc-image bink))))

(defmethod (setf medium-foreground) :after (new-foreground (medium acl-medium))
  (with-slots (foreground-dc-image) medium
    (let ((fink (dc-image-for-ink medium new-foreground)))
      (setf foreground-dc-image fink))))

(defclass acl-window-medium (acl-medium)
    ((window :initform nil :reader medium-drawable)))

(defmethod make-medium ((port acl-port) sheet)
  (let ((m (make-instance 'acl-window-medium
	     :port port
	     :sheet sheet)))
    (setf (medium-background m) silica:*default-pane-background*)
    m))

(defmethod deallocate-medium ((port acl-port) medium)
  (setf (medium-sheet medium) nil)
  (with-slots (window) medium (setf window nil))
  (push medium (silica::port-medium-cache port)))


(defmethod engraft-medium :after ((medium acl-window-medium) port sheet)
  (declare (ignore port))
  (with-slots (window) medium
    (unless window
      (setq window (sheet-mirror sheet)))))

(defmethod degraft-medium :after ((medium acl-window-medium) port sheet)
  (declare (ignore port sheet))
  (with-slots (window) medium
    (setf window nil))
  nil)

;; changed the select-acl-dc method to specialize on acl-medium rather
;; than acl-window-medium (sdj 9/27/96)

(defmethod select-acl-dc ((medium acl-medium) window dc)
  (declare (ignore window))
  (let* ((sheet (medium-sheet medium))
	 (region (sheet-device-region sheet))
	 (medium-region (medium-clipping-region medium))
	 cleft ctop cright cbottom
         winrgn
	 (valid nil))
    (unless (eq region +nowhere+)
      (multiple-value-setq (cleft ctop cright cbottom) (bounding-rectangle* region))
      (setf valid t)
      (unless (or (eq medium-region +everywhere+) (eq medium-region +nowhere+))
	(with-bounding-rectangle* (mleft mtop mright mbottom) medium-region
	  (multiple-value-setq (valid cleft ctop cright cbottom)
	    (multiple-value-call #'ltrb-overlaps-ltrb-p
	      cleft ctop cright cbottom
	      (transform-rectangle* 
	       (sheet-device-transformation sheet)
	       mleft mtop mright mbottom))))))
    (when valid
      (fix-coordinates cleft ctop cright cbottom)
      (setq winrgn (win:createRectRgn cleft ctop cright cbottom))
      (setf *created-region* winrgn)
      (win:selectObject dc winrgn))))

(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +ltgray+)))
  ;; This is not supposed to be just ANY light gray.
  ;; This one is supposed to match exactly the color
  ;; used by Windows for all the other gadgets, such as
  ;; buttons and scroll bars.  MFC calls this "light
  ;; gray in the current color scheme"
  *ltgray-image*)

(defmethod color-rgb ((color (eql +ltgray+)))
  (color-rgb (wincolor->color (win:getSysColor win:COLOR_BTNFACE))))

(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +foreground-ink+)))
  (slot-value medium 'foreground-dc-image))

(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +background-ink+)))
  (slot-value medium 'background-dc-image))

#+obsolete
(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +black+)))
  *black-image*)

#+obsolete
(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +white+)))
  *white-image*)

;; changing the conversion as follows makes CLIM gray colors 1/4, 1/2
;; and 3/4 gray colors map to the corresponding windows solid system
;; colors avoiding need for windows to stipple. (cim 10/11/96)

(defun color->wincolor (ink &optional medium)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq ink +background-ink+)
	 (setq ink (medium-background medium)))
	((eq ink +foreground-ink+)
	 (setq ink (medium-foreground medium)))
	((eq ink +transparent-ink+)
	 (return-from color->wincolor -1)
	 ))
  (flet ((convert (x)
	   (declare (short-float x))
	   (setq x (float x))		; needed?
	   (if (< x 1.0)
	       (values (the fixnum (floor (the short-float (* x #x100)))))
	     #xff)))
    (multiple-value-bind (red green blue)
	(color-rgb ink)
      (let ((color (dpb (convert blue) (byte 8 16)
			(dpb (convert green) (byte 8 8)
			     (convert red)))))
	color))))

(defun wincolor->color (color)
  (flet ((convert (x)
	   (if (eql x #xff)
	       1.0
	     (float (/ x 256)))))
    (let ((red (ldb (byte 8 0) color))
	  (green (ldb (byte 8 8) color))
	  (blue (ldb (byte 8 16) color)))
      (make-rgb-color (convert red)
		      (convert green)
		      (convert blue)))))

(defmethod dc-image-for-ink ((medium acl-medium) (ink color))
  (let ((cache (port-dc-cache (port medium))))
    (or (gethash ink cache)
	(setf (gethash ink cache)
	  (let ((color (color->wincolor ink medium)))
	    (declare (fixnum color))
	    (let ((pen (win:createPen win:ps_solid 1 color))
		  (brush (win:createSolidBrush color)))
	      (make-dc-image :solid-1-pen pen
			     :brush brush
			     :text-color color 
			     :background-color nil
			     :rop2 win:r2_copypen
			     )))))))

;;; ink for opacities, regions, etc

(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +transparent-ink+)))
  *blank-image*)

(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +everywhere+)))
  (dc-image-for-ink medium +foreground-ink+))

(defmethod dc-image-for-ink ((medium acl-medium) (ink standard-opacity))
  #+ignore
  (cerror "Return Opacity 0" "Can't handle Opacities other than 0 and 1")
  (if (>= (opacity-value ink) 0.5)
    (dc-image-for-ink medium +foreground-ink+)
    *blank-image*))

(defmethod dc-image-for-ink ((medium acl-medium) (ink region))
  (dc-image-for-ink medium +foreground-ink+))

;;; ink for patterns, tiles, etc

(defconstant bmdim 32)
(defvar *bitmap-array* nil)		; probably not needed
(defvar *the-dc* nil)			; new just for now, later 
					; rationalize passing dc

;;;  just for now, later rationalize passing dc
(defun dc-image-for-multi-color-pattern (medium array designs)
  (let* ((dc-image (copy-dc-image
		    (slot-value medium 'foreground-dc-image)))
	 (tink (aref designs 1))
	 (bink (aref designs 0))
	 (tcolor (color->wincolor tink medium))
	 (bcolor (color->wincolor bink medium))
	 (width (array-dimension array 1))
	 (height (array-dimension array 0))
	 (into (make-pixel-map width height 256)))
    (dotimes (i height)
      (dotimes (j width)
	(setf (aref into i j) (aref array i j))))
    (setf *bitmap-array* into)
    (let ((bitmap (get-texture *the-dc* into designs medium)))	
      (setf (dc-image-bitmap dc-image) bitmap
	    *created-bitmap* bitmap)
      (setf (dc-image-background-color dc-image) bcolor)
      (setf (dc-image-text-color dc-image) tcolor))
    dc-image))

(defun dc-image-for-two-color-pattern (medium array designs)
  #+broken
  (let* ((dc-image (copy-dc-image
		    (slot-value medium 'foreground-dc-image)))
	 (bink (aref designs 0))
	 (tink (aref designs 1))
	 (tcolor (color->wincolor tink medium))
	 (bcolor (color->wincolor bink medium))
	 (width (array-dimension array 1))
	 (height (array-dimension array 0))
	 (into (byte-align-bits array)))
    (setf *bitmap-array* into)
    (let ((bitmap (win:createBitmap width height 1 1 into)))
      (setf (dc-image-bitmap dc-image) bitmap)
      (setf (dc-image-background-color dc-image) bcolor)
      (setf (dc-image-text-color dc-image) tcolor))
    dc-image)

  (let* ((dc-image (copy-dc-image
		    (slot-value medium 'foreground-dc-image)))
	 (tink (aref designs 1))
	 (bink (aref designs 0))
	 (tcolor (color->wincolor tink medium))
	 (bcolor (color->wincolor bink medium))
	 (width (array-dimension array 1))
	 (height (array-dimension array 0))
	 (into (make-pixel-map width height 256)))
    (dotimes (i height)
      (dotimes (j width)
	(setf (aref into i j) (aref array i j))))
    (setf *bitmap-array* into)
    (let ((bitmap (get-texture *the-dc* into designs medium)))	
      (setf (dc-image-bitmap dc-image) bitmap
	    *created-bitmap* bitmap)
      (setf (dc-image-background-color dc-image) bcolor)
      (setf (dc-image-text-color dc-image) tcolor))
    dc-image))

(defun byte-align-pixmap (a)
  ;; Pad the pixmap so that the y dimension is a multiple of 8.
  ;; This copies the array, so you oughta cache the result.
  (let* ((x-dim (array-dimension a 0))
         (y-dim (array-dimension a 1))
         (new-y-dim (* 8 (floor (/ (+ y-dim 7) 8))))
         (b nil))
    (cond ((= y-dim new-y-dim) a)	; already aligned
	  (t
	   (setq b (make-array (list x-dim new-y-dim) :initial-element 0))
	   (dotimes (y new-y-dim)
	     (dotimes (x x-dim)
	       (setf (aref b x y)
		 (if (>= y y-dim) 0 (aref a x y)))))
	   b))))

(defun byte-align-bits (a)
  ;; Pad the pixmap so that the y dimension is a multiple of 8.
  ;; This copies the array, so you oughta cache the result.
  (let* ((x-dim (array-dimension a 0))
         (y-dim (array-dimension a 1))
         (new-y-dim (* 8 (floor (/ (+ y-dim 7) 8))))
         (b (make-array (list x-dim new-y-dim) 
			:element-type 'bit
			:initial-element 0)))
    (dotimes (col new-y-dim)
      (dotimes (row x-dim)
	(setf (aref b row col)
	  (if (>= col y-dim) 0 (if (zerop (aref a row col)) 0 1)))))
    b))

(defmethod dc-image-for-ink ((medium acl-medium) (ink pattern))
  (multiple-value-bind (array designs) (decode-pattern ink)
    (cond ((= (length designs) 2)
	   (setq array (byte-align-pixmap array))
	   (dc-image-for-two-color-pattern
	    medium array designs))
	  ((find +transparent-ink+ designs)
	   (error 
	    "Multicolor patterns must not use +transparent-ink+"))
	  (t
	   (setq array (byte-align-pixmap array))
	   (dc-image-for-multi-color-pattern
	    medium array designs)))))

(defmethod dc-image-for-ink ((medium acl-medium) (ink rectangular-tile))
  ;; The only case we handle right now is stipples
  (multiple-value-bind (array width height)
      (decode-tile-as-stipple ink)
    (unless array
      (error 
       "Rectangular tiles other than stipples are not supported yet."))
    ;; I don't know why width and height are wrong, but they are. JPM 8/25/97
    (setq width (array-dimension array 1))
    (setq height (array-dimension array 0))
    (let* ((into (make-array `(,bmdim ,bmdim) :element-type 'bit
			     :initial-element 0))
	   (pattern (decode-rectangular-tile ink))
	   (designs (pattern-designs pattern))
	   dc-image background-dc foreground-dc)
      (unless (= (length designs) 2)
	(error "Only 2-color stipples are currently supported."))
      (setq background-dc (dc-image-for-ink medium (svref designs 0)))
      (setq foreground-dc (dc-image-for-ink medium (svref designs 1)))
      (setq dc-image (copy-dc-image foreground-dc))
      (dotimes (i bmdim)
	(dotimes (j bmdim)
	  (setf (aref into i j)
	    (logand 1 (lognot (aref array (mod i height)
				    (mod j width)))))))
      (let* ((bitmap (win:createBitmap bmdim bmdim 1 1 into))
	     (brush (win:createPatternBrush bitmap)))
	(setf (dc-image-brush dc-image) brush)
	;; Setting ROP2 helps when +transparent-ink+ is the background ink.
	(setf (dc-image-rop2 dc-image) (dc-image-rop2 background-dc))
	(setf (dc-image-background-color dc-image)
	  (dc-image-text-color background-dc))
	(setf (dc-image-text-color dc-image) 
	  (dc-image-text-color foreground-dc))
	dc-image))))

(defun nyi ()
  (error "This NT CLIM operation is NYI (Not Yet Implemented)."))

(defmethod dc-image-for-ink ((medium acl-medium) (ink flipping-ink))
  (let ((cache (port-dc-cache (port medium))))
    (or (gethash ink cache)
	(setf (gethash ink cache)
	  (multiple-value-bind (ink1 ink2)
	      (decode-flipping-ink ink)
	    (let* ((image1 (dc-image-for-ink medium ink1))
		   (image2 (dc-image-for-ink medium ink2))
		   (color (logxor (dc-image-text-color image1)
				  (dc-image-text-color image2))))
	      (unless (and (eql (dc-image-rop2 image1) win:r2_copypen)
			   (eql (dc-image-rop2 image2) win:r2_copypen)
			   (null (dc-image-bitmap image1))
			   (null (dc-image-bitmap image2)))
		(nyi))
	      (let ((pen (win:createPen win:ps_solid 1 color))
		    (brush (win:createSolidBrush color)))
		(make-dc-image :solid-1-pen pen
			       :brush brush
			       :rop2 win:r2_xorpen
			       :text-color color 
			       :background-color nil))))))))

(defmethod dc-image-for-ink ((medium acl-medium) (ink contrasting-ink))
  (dc-image-for-ink medium (make-color-for-contrasting-ink ink)))

(defmethod dc-image-for-ink ((medium acl-medium) (ink composite-out))
  (error "Compositing is not supported."))

(defmethod dc-image-for-ink ((medium acl-medium) (ink composite-in))
  (error "Compositing is not supported."))

(defmethod medium-draw-point* ((medium acl-medium) x y)
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc))
      (when old
        (let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (medium-line-style medium)))
          (convert-to-device-coordinates transform x y)
          (set-dc-for-ink dc medium ink line-style)
	  ;;(win:rectangle dc x y (+ x 1) (+ y 1))  pity it doesn't work
	  (win:moveToEx dc x y null)
	  (win:lineTo dc (+ x 1) (+ y 1))
	  (win:selectobject dc old)
	  )))))

(defmethod medium-draw-points* ((medium acl-medium) position-seq)
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc))
      (when old
        (let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (medium-line-style medium)))
          (set-dc-for-ink dc medium ink line-style)
	  (do ((i 0 (+ i 2))
               (j 1 (+ j 2)))
            ((>= i (length position-seq)))
            (let ((x (elt position-seq i))
	          (y (elt position-seq j)))
	      (convert-to-device-coordinates transform x y)
              ;(win:rectangle dc x y x y)
	      (win:moveToEx dc x y null)
	      (win:lineTo dc (+ x 1) (+ y 1))))
	  (win:selectobject dc old))))))

(defmethod medium-draw-line* ((medium acl-medium) x1 y1 x2 y2)
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc))
      (when old
        (let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (medium-line-style medium)))
          (convert-to-device-coordinates transform x1 y1 x2 y2)
          (set-dc-for-ink dc medium ink line-style)
          (win:moveToEx dc x1 y1 null)
          (win:lineTo dc x2 y2)
	  (win:selectobject dc old))))))

(defmethod medium-draw-lines* ((medium acl-medium) position-seq)
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc))
      (when old
        (let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (medium-line-style medium)))
          (set-dc-for-ink dc medium ink line-style)
	  (do ((i 0 (+ i 4))
               (j 1 (+ j 4))
	       (k 2 (+ k 4))
	       (l 3 (+ l 4)))
            ((>= i (length position-seq)))
            (let ((x1 (elt position-seq i))
	          (y1 (elt position-seq j))
		  (x2 (elt position-seq k))
	          (y2 (elt position-seq l)))
	      (convert-to-device-coordinates transform x1 y1 x2 y2)
              (win:moveToEx dc x1 y1 null)
              (win:lineTo dc x2 y2)))
	  (win:selectobject dc old))))))

(defmethod medium-draw-rectangle* ((medium acl-medium)
				   left top right bottom filled)
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc)) 
      (when old
        (let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (and (not filled) (medium-line-style medium))))
          (convert-to-device-coordinates transform
					 left top right bottom)
          (when (< right left) (rotatef right left))
          (when (< bottom top) (rotatef bottom top))
          (cond ((typep ink 'pattern)
		 ;; DRAW-PATTERN* ultimately does its drawing here.
		 ;; BUG: Transparent bits aren't transparent.
		 (with-compatible-dc (dc cdc)
		   (let ((width (- right left))
			 (height (- bottom top))
			 (*the-dc* dc))
		     (set-cdc-for-pattern cdc medium ink nil)
		     (win:bitblt dc left top width height
				 cdc 0 0 win:srccopy))))
		(t
		 (let ((*the-dc* dc))
		   (set-dc-for-ink dc medium ink line-style))
		 #+ignore
		 (if (typep ink 'rectangular-tile)
		     (cerror "Go" "Stop ~S" ink))
		 (if filled
		     (win:rectangle dc left top (1+ right) (1+ bottom))
		   (win:rectangle dc left top right bottom))))
	  (win:selectobject dc old))))))

(defmethod medium-draw-rectangles* ((medium acl-medium) position-seq filled)
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc))
      (when old
        (let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (and (not filled) (medium-line-style medium)))
	       (numrects (floor (length position-seq) 4))
	       (rect -1))
	  (set-dc-for-ink dc medium ink line-style)
          (dotimes (i numrects)
            (let ((left (elt position-seq (incf rect)))
		  (top (elt position-seq (incf rect)))
		  (right (elt position-seq (incf rect)))
		  (bottom (elt position-seq (incf rect))))
	      (convert-to-device-coordinates transform
					     left top right bottom)
	      (when (< right left) (rotatef right left))
	      (when (< bottom top) (rotatef bottom top))
	      (if filled
		  (win:rectangle dc left top (1+ right) (1+ bottom))
		(win:rectangle dc left top right bottom))))
	  (win:selectobject dc old))))))

(defmethod medium-draw-polygon* ((medium acl-medium)
				 position-seq closed filled)
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc))
      (when old
	(let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (and (not filled) (medium-line-style medium)))
	       (length (length position-seq))
	       (numpoints (floor length 2))
	       (extra (and closed line-style))
	       #-acl86win32 (point-vector (ct:ccallocate (win:point 256)))
	       #+acl86win32 (point-vector (ct:callocate (:long *) :size 512))) ;  limit?
	  ;; These really are fixnums, since we're fixing coordinates below
	  ;; (declare (type fixnum minx miny))
	  (with-stack-array (points (if extra (+ length 2) length))
	    (declare (type simple-vector points))
	    (replace points position-seq) ;set up the initial contents
	    (do ((i 0 (+ i 2))
		 (j 0 (+ j 1)))
		((>= i length))
	      (let ((x (svref points i))
		    (y (svref points (1+ i))))
		(convert-to-device-coordinates transform x y)
		(ct:cset (:long 512) point-vector ((fixnum (* j 2))) x)
		(ct:cset (:long 512) point-vector ((fixnum (+ 1 (* j 2)))) y)
		(when (and (= j 0) extra)
		  (ct:cset (:long 512) point-vector ((fixnum (* numpoints 2))) x)
		  (ct:cset (:long 512) point-vector ((fixnum (+ 1 (* numpoints 2)))) y)
		  ))))
	  (if (or (typep ink 'pattern) (typep ink 'rectangular-tile))
	      (if (null line-style)
		  (let ((*the-dc* dc)
			(nink ink
			      #+ignore
			      (if (typep ink 'pattern)
				  (make-rectangular-tile ink  8 8)
				ink)))
		    (set-dc-for-ink dc medium nink line-style))
		(set-dc-for-ink dc medium +foreground-ink+ line-style))	    
	    (set-dc-for-ink dc medium ink line-style))
	  (if (null line-style)
	      (win:polygon dc point-vector numpoints)
	    (win:polyline dc
			  point-vector
			  (if (and closed line-style)
			      (+ numpoints 1)
			    numpoints)))
	  (win:selectobject dc old))))))

(defconstant *ft* 0.0001)
(defun-inline fl-= (x y) (< (abs (- x y)) *ft*))

(defmethod medium-draw-ellipse* ((medium acl-medium)
				 center-x center-y
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc))
      (when old
	(let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (and (not filled) (medium-line-style medium))))
	  (convert-to-device-coordinates transform center-x center-y)
	  (convert-to-device-distances
	   transform radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
	  (when (null start-angle)
	    (setq start-angle 0.0
		  end-angle 2pi))
	  (if (or (typep ink 'pattern) (typep ink 'rectangular-tile))
	      (if (null line-style)
		  (let ((*the-dc* dc)
			(nink ink
			      #+ignore
			      (if (typep ink 'pattern)
				  (make-rectangular-tile ink  8 8)
				ink)))
		    (set-dc-for-ink dc medium nink line-style))
		(set-dc-for-ink dc medium +foreground-ink+ line-style))	    
	    (set-dc-for-ink dc medium ink line-style))
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
		     (win:ellipse dc left top right bottom))
		    ((null line-style)
		     ;; drawing a pie slice
		     (win:pie
		      dc left top right bottom
		      (round (1- (+ center-x (* (cos start-angle) x-radius))))
		      (round (1- (- center-y (* (sin start-angle) y-radius))))
		      (round (1- (+ center-x (* (cos end-angle) x-radius))))
		      (round (1- (- center-y (* (sin end-angle) y-radius))))))
		    (t
		     ;; drawing an arc
		     (win:arc
		      dc left top right bottom
		      (round (1- (+ center-x (* (cos start-angle) x-radius))))
		      (round (1- (- center-y (* (sin start-angle) y-radius))))
		      (round (1- (+ center-x (* (cos end-angle) x-radius))))
		      (round (1- (- center-y (* (sin end-angle) y-radius)))))))
	      ))
	  (win:selectobject dc old))))))

(defmethod medium-draw-string* ((medium acl-medium)
				string x y start end align-x align-y
				towards-x towards-y transform-glyphs)
  (declare (ignore transform-glyphs))
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc))
      (when old
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
		   (height (acl-font-height font))
		   (descent (acl-font-descent font))
		   (ascent (acl-font-ascent font)))
	      (let ((x-adjust 
		     (compute-text-x-adjustment align-x medium string text-style start end))
		    (y-adjust
		     (compute-text-y-adjustment align-y descent ascent height)))
		(incf x x-adjust)
		(incf y y-adjust)
		(when towards-x
		  (incf towards-x x-adjust)
		  (incf towards-y y-adjust)))
	      (decf y ascent)		;text is positioned by its top left on acl
	      (set-dc-for-text dc medium ink (acl-font-index font))
	      (win:textOut dc x y 
			   substring
			   (length substring))))
	  (win:selectobject dc old))))))

(defmethod medium-draw-character* ((medium acl-medium)
				   char x y align-x align-y
				   towards-x towards-y transform-glyphs)
  (declare (ignore transform-glyphs))
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc))
      (when old
	(let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (text-style (medium-merged-text-style medium)))
	  (convert-to-device-coordinates transform x y)
	  (when towards-x
	    (convert-to-device-coordinates transform towards-x towards-y))
	  (let* ((font (text-style-mapping (port medium) text-style))
		 (height (acl-font-height font))
		 (descent (acl-font-descent font))
		 (ascent (acl-font-ascent font)))
	    (let ((x-adjust 
		   (compute-text-x-adjustment align-x medium char text-style))
		  (y-adjust
		   (compute-text-y-adjustment align-y descent ascent height)))
	      (incf x x-adjust)
	      (incf y y-adjust)
	      (when towards-x
		(incf towards-x x-adjust)
		(incf towards-y y-adjust)))
	    (decf y ascent)		;text is positioned by its top left on acl
	    (set-dc-for-text dc medium ink (acl-font-index font))
	    (let ((cstr (ct:callocate (:char *) :size 2)))
	      (ct:cset (:char 2) cstr 0 (char-int char))
	      (win:textOut dc x y cstr 1))))
	(win:selectobject dc old)))))

(defmethod medium-draw-text* ((medium acl-medium)
			      string-or-char x y start end
			      align-x align-y
			      towards-x towards-y transform-glyphs)
  (if (characterp string-or-char)
      (medium-draw-character* medium string-or-char x y align-x align-y
			      towards-x towards-y transform-glyphs)
    (medium-draw-string* medium string-or-char x y start end align-x align-y
			 towards-x towards-y transform-glyphs)))

(defmethod medium-clear-area ((medium acl-medium) left top right bottom)
  #+old
  (let ((window (medium-drawable medium))
	(old nil))
    (with-dc (window dc)
      (setq old (select-acl-dc medium window dc))
      (when old
	(with-slots (background-dc-image) medium
	  (let* ((sheet (medium-sheet medium))
		 (transform (sheet-device-transformation sheet)))
	    (convert-to-device-coordinates transform
					   left top right bottom)
	    (when (< right left) (rotatef right left))
	    (when (< bottom top) (rotatef bottom top))
	    (set-dc-for-filling dc background-dc-image)
	    (win:rectangle dc left top (1+ right) (1+ bottom))))
	(win:selectobject dc old))))
  (with-drawing-options (medium :ink (medium-background medium))
    (medium-draw-rectangle* medium left top right bottom t)))

(defmethod text-style-width (text-style (medium acl-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (acl-font-average-character-width font)))

(defmethod text-style-height (text-style (medium acl-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (acl-font-height font)))

(defmethod text-style-ascent (text-style (medium acl-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (acl-font-ascent font)))

(defmethod text-style-descent (text-style (medium acl-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (acl-font-descent font)))

(defmethod text-style-fixed-width-p (text-style
				     (medium acl-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (= (acl-font-average-character-width font)
       (acl-font-maximum-character-width font))))

(defmethod text-style-width ((text-style standard-text-style) (medium acl-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (acl-font-average-character-width font)))

(defmethod text-style-height ((text-style standard-text-style) (medium acl-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (acl-font-height font)))

(defmethod text-style-ascent ((text-style standard-text-style) (medium acl-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (acl-font-ascent font)))

(defmethod text-style-descent ((text-style standard-text-style) (medium acl-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (acl-font-descent font)))

(defmethod text-style-fixed-width-p ((text-style standard-text-style)
				     (medium acl-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    (= (acl-font-average-character-width font)
       (acl-font-maximum-character-width font))))

(defmethod medium-beep ((medium acl-medium))
  (win:messageBeep win:MB_OK))

(defmethod medium-force-output ((medium acl-medium))
  )

(defmethod medium-clear-output ((medium acl-medium))
  )

(defmethod medium-finish-output ((medium acl-medium))
  )


;;; reading patterns from bitmap files

(defun read-bitmap-file (path &key (format :bitmap) (port (find-port)))
  (declare (ignore format port))
  (load-pixmap-1 path 0))

(defun load-pixmap-1 (filename index) 
  (format *trace-output* "~&Loading pixmap from file ~a ..." filename)
  (multiple-value-prog1 
      (with-open-file (s filename :element-type '(unsigned-byte 8))
	(read-pixmap s index)) 
    (format *trace-output* "LOADED pixmap~%")))

(defstruct (texture-info #+ig (:include faslable-structure) (:constructor make-texture-info))
  (width nil :type (or null fixnum))
  (height nil :type (or null fixnum))
  (bits-per-pixel 1 :type fixnum)
  (x-device-units-per-m nil :type (or null integer))
  (y-device-units-per-m nil :type (or null integer))
  (colors nil)
  (invert-p nil))

(defun bm-read-word (stream)
   (+ (read-byte stream)
      (ash (read-byte stream) 8)))

(defun bm-read-long (stream)
     (+ (bm-read-word stream)
         (ash (bm-read-word stream) 16)))

(defun make-pixel-map (x y &optional (palette-size 2)
		       &key (initial-element 0))
  (when (> (cond
		((< 0 palette-size 3) (truncate (* x y) 8))
		((< 2 palette-size 257) (* x y))
		((< 256 palette-size #x10001) (* x y 2))
		(t (* x y 4))) 
	   #x10000)
    (error "pixel-map bigger than 64k."))
  (make-array (list y x)
	      :element-type `(mod ,palette-size)
	      :initial-element initial-element))

(defun pixel-map-set (pm x y v) (setf (aref pm x y) v))

(defstruct (rgb ;;(:include faslable-structure)   ;; <5>
	    (:copier nil)
	    ;;(:constructor make-rgb)
	    ;;(:constructor build-rgb (red green blue))
	    )
  (red  0 :type byte)
  (green  0 :type byte)
  (blue  0 :type byte))

(defun read-pixmap (stream &optional (index 0)) ;; <12>
  (let* ((file-type nil)
	 (number-of-resources 1)
	 char1 char2 word2 file-size bytes-into-file
	 image-offset image-size info-header-size
	 width file-width height
	 hotspot-x hotspot-y bits-per-pixel
	 (compression 0)		; 0 = no compression
	 (x-pixels-per-meter nil)
	 (y-pixels-per-meter nil)
	 (number-of-colors-used 0)
         (previous-time 0) ;; <16>
         this-time ;; <16>
	 number-of-colors-in-file rgb
	 pixmap mask palette-array texture-info
	 pixel-byte byte-num real-y pixels-per-longword
	 )
    ;; Determine the type of file from the header (.bmp, .ico, or .cur)
    (setq char1 (code-char (read-byte stream))
	  char2 (code-char (read-byte stream)))
    (cond ((and (eql char1 #\B)
		(eql char2 #\M))
	   (setq file-type :pixmap))
	  ((and (zerop (char-int char1))
		(zerop (char-int char2)))
	   (setq word2 (bm-read-word stream))
	   (cond ((eql word2 1)
		  (setq file-type :icon))
		 ((eql word2 2)
		  (setq file-type :cursor)))))
    (unless file-type
      (error "Object being read is neither a ~
device-independent bitmap, an icon, nor a cursor."))
    (case file-type
      (:pixmap
       (setq file-size (bm-read-long stream))
       (bm-read-word stream)		; reserved1 = 0
       (bm-read-word stream)		; reserved2 = 0
       (setq image-offset (bm-read-long stream))
       )
      ((:icon :cursor)
       (setq number-of-resources (bm-read-word stream))
       (setq bytes-into-file 6)
       ;; skip the directory entries up to the requested one.
       (dotimes (j index)
	 (dotimes (k 16)		; each dir entry is 16 bytes
	   (read-byte stream)
	   (incf bytes-into-file)))
       ;; read the requested directory entry
       ;; most of these will be read again in the bitmapinfo header
       (setq width (read-byte stream)
             height (read-byte stream))
       (setq number-of-colors-used (read-byte stream))
       (read-byte stream)	; reserved
       (setq hotspot-x (bm-read-word stream)) ; bit-planes for icon
       (setq hotspot-y (bm-read-word stream)) ; bits-per-pixel for icon
       (setq image-size (bm-read-long stream))
       (setq image-offset (bm-read-long stream))
       (incf bytes-into-file 16)
       ;; skip the rest of the directory and the first n resources
       ;; to get to the actual resource to read
       (dotimes (j (- image-offset bytes-into-file))
	 (read-byte stream))
       ))
      
    ;; end of file-header; start of bitmap-info-header
    (setq info-header-size (bm-read-long stream))
    (setq width (bm-read-long stream)
	  height (bm-read-long stream))
    ;; for icons & cursors height is pixmap height + mask height
    (unless (eq file-type :pixmap)
      (setq height (floor height 2)))
    (bm-read-word stream) ;; bit-planes = 1
    (setq bits-per-pixel (bm-read-word stream))
    (setq pixels-per-longword (floor 32 bits-per-pixel))
    (setq file-width (* pixels-per-longword
			(ceiling width pixels-per-longword)))
    (when (>= info-header-size 20)
      (setq compression (bm-read-long stream)))
    (when (plusp compression)
      (error "Reading of compressed bitmaps is not implemented."))
    (when (>= info-header-size 24)
      (bm-read-long stream)) ;; size of all bits (needed with compression)
    (when (>= info-header-size 28)
      (setq x-pixels-per-meter (bm-read-long stream)))
    (when (>= info-header-size 32)
      (setq y-pixels-per-meter (bm-read-long stream)))
    (when (>= info-header-size 36)
      (setq number-of-colors-used (bm-read-long stream)))
    (when (>= info-header-size 40)
      (bm-read-long stream))		;; number of "important colors" used
    (dotimes (j (- info-header-size 40)) ;; should be none
      (read-byte stream))	;; but throw away extra bytes if any
    ;; end of bitmap-info-header; start of color table if any
    (setq number-of-colors-in-file
      (cond ((plusp number-of-colors-used)
	     number-of-colors-used)
	    (t
	     (case bits-per-pixel
	       (1 2)
	       (4 16)
	       (8 256)
	       (24 0) ;; each pixel value is a direct rgb
	       (t (error "~a is not a valid bits-per-pixel"
			 bits-per-pixel))))))
    
    (when (plusp number-of-colors-in-file)
      (setq palette-array (make-array number-of-colors-in-file))
         
      (dotimes (j number-of-colors-in-file)
	(if (setq rgb (aref palette-array j))
	    (setf (rgb-blue rgb) (read-byte stream)
		  (rgb-green rgb) (read-byte stream)
		  (rgb-red rgb) (read-byte stream))
	  (setf (aref palette-array j)
	    (make-rgb :blue (read-byte stream)
			  :green (read-byte stream)
			  :red (read-byte stream))))
	(read-byte stream))	; reserved = 0
      )					; end reading color table

    (setq texture-info
      (make-texture-info :width width ;; <10>
			 :height height
			 :bits-per-pixel bits-per-pixel
			 :colors palette-array
			 :invert-p nil)) ;; <18>
      
    ;; Read pixel values.
    (setq pixmap (make-pixel-map
		  file-width	;; multiple of 32 bits wide
		  height
		  (expt 2 bits-per-pixel))) ;; "palette size"
    (dotimes (y height)
      #-runtime-system
      (when (and (> width 200) ;; <16>
		 (> (setq this-time (get-internal-real-time))
		    (+ previous-time
		       internal-time-units-per-second)))
	(setq previous-time this-time)
	(format *trace-output* "Reading pixmap row ~a of ~a"
		(1+ y) height))
      (setq real-y (- (1- height) y))
      (case bits-per-pixel
	(1				; monochrome
	 (dotimes (x (* 4 (ceiling width 32)))
	   (setq pixel-byte (read-byte stream))
	   (setq byte-num (* x 8))
	   (dotimes (bit-num 8)
	     (pixel-map-set pixmap (+ byte-num bit-num) real-y
			    (if (logbitp (- 7 bit-num) pixel-byte)
				1 0)))))
	(4
	 ;; rows are padded to word boundary even in the file apparently
	 ;; so read 8 nibbles from each 4-byte section of the row
	 (dotimes (x (* 4 (ceiling width 8)))
	   (setq pixel-byte (read-byte stream))
	   (setq byte-num (* x 2))
	   (pixel-map-set pixmap byte-num real-y
			  (ash pixel-byte -4)) ; high nibble
	   (pixel-map-set pixmap (1+ byte-num) real-y
			  (logand pixel-byte 15)))) ; low nibble
	(8
	 (dotimes (x (* 4 (ceiling width 4)))
	   (pixel-map-set pixmap x real-y
			  (read-byte stream))))
	(24 (error "24-bit (direct-color) pixmaps are not implemented."))))
    ;; Return the objects that were read.
    hotspot-x hotspot-y image-size file-size number-of-resources
    x-pixels-per-meter y-pixels-per-meter
    (case file-type
      (:pixmap
       (values pixmap texture-info nil width
	       ))
      ((:icon :cursor)
       (setq file-width (* 4 (ceiling width 32))) ;; <15>
       (setq mask (make-pixel-map
		   (* 8 file-width) ;; <15>
		   height
		   1)) ;; <4>
       (dotimes (y height)
	 (setq real-y (- (1- height) y))
	 (dotimes (x file-width) ;; <15>
	   (setq pixel-byte (read-byte stream))
	   (setq byte-num (* x 8))
	   (dotimes (bit-num 8)
	     (pixel-map-set mask (+ byte-num bit-num) real-y
			    (if (logbitp (- 7 bit-num) pixel-byte)
				1 0)))))
       (values pixmap texture-info mask width
	       )))))

(defun rotate-and-mirror-bitmap (a)
  (let* ((x-dim (array-dimension a 0))
         (y-dim (array-dimension a 1))
         (b (make-array (list y-dim x-dim) :initial-element 0)))
    (dotimes (x x-dim)
      (dotimes (y y-dim)
	(setf (aref b y x) (aref a x y))))
    b))

(defun make-pattern-from-bitmap-file
    (path &key designs (format :bitmap) (port (find-port)))
  (if (member format '(:xbm :xpm))
      (multiple-value-bind (array designs)
	  (read-xbitmap-file path :format format :port port)
	(make-pattern array designs))
    (multiple-value-bind (array texture-info)
	(read-bitmap-file path :format format :port port)
      (setq array (rotate-and-mirror-bitmap array))
      (let (colors numcolors climcolors color)
	(unless designs
	  (setf colors (texture-info-colors texture-info))
	  (setf numcolors (length colors))
	  (setf climcolors (make-array numcolors))
	  (dotimes (i numcolors)
	    (setf color (aref colors i))
	    (setf (aref climcolors i)
	      (make-rgb-color
	       (/ (rgb-red color) 256.0)
	       (/ (rgb-green color) 256.0)
	       (/ (rgb-blue color) 256.0)))))
	(make-pattern array (or designs climcolors))))))

;;;*******************
;;;
;;; Support for X bitmaps (xbm and xpm). [spr17488]
;;; Mostly just copied this stuff from the unix code...jpm 5/98.
;;;

(defun get-bitmap-file-properties (fstream)
  (let ((line "")
	(properties nil)
	(name nil)
	(name-end nil))
    (loop
      (setq line (read-line fstream))
      (when  (> (length line) 0)
	(when (char= (aref line 0) #\#)
	  (return))))

    (loop
      (when  (> (length line) 0)
	(unless (char= (aref line 0) #\#)
	  (return))
	(flet ((read-keyword (line start end)
		 (cdr (find-if #'(lambda (pair)
				   (string= (car pair)
					    line :start2 start
					    :end2 end))
			       '(("image" . :image)
				 ("width" . :width)
				 ("height". :height)
				 ("depth" . :depth)
				 ("format" . :format)
				 ;;---
				 ("pixel" . :chars-per-pixel)
				 ("left_pad" . :left-pad))))))
	  ;; Get the name of the bitmaps
	  ;; #define THENANME_some_attribute
	  (when (null name)
	    (setq name-end (position #\_ line :test #'char= :from-end t)
		  name (read-keyword line 8 name-end))
	    (unless (eq name :image)
	      (setf (getf properties :name) name)))
	  ;; Get the name of the attribute.
	  (let* ((ind-start (1+ name-end))
		 (ind-end (position #\Space line :test #'char=
				    :start ind-start))
		 (ind (read-keyword line ind-start ind-end))
		 (val-start (1+ ind-end))
		 (val (parse-integer line :start val-start)))
	    (when ind
	      (setf (getf properties ind) val)))))
      (setq line (read-line fstream)))

    (flet ((extract-property (ind &rest default)
	     (prog1 (apply #'getf properties ind default)
	       (remf properties ind))))
      (values (extract-property :width)
	      (extract-property :height)
	      (extract-property :depth 1)
	      (extract-property :left-pad 0)
	      (extract-property :format nil)
	      (extract-property :chars-per-pixel nil)
	      line))))

(defun read-x11-bitmap-file (fstream)
  (multiple-value-bind (width height depth left-pad format chars-per-pixel line)
      (get-bitmap-file-properties fstream)
    (declare (ignore format  chars-per-pixel line left-pad))
    (unless (and width height) (error "Not a BITMAP file"))
    (let* ((bits-per-pixel
	    (cond ((> depth 24) 32)
		  ((> depth 16) 24)
		  ((> depth 8)  16)
		  ((> depth 4)   8)
		  ((> depth 2)   4)
		  ((> depth 1)   2)
		  (t 1)))
	   (data (make-array (list height width)))
	   (bits-per-line (* width bits-per-pixel))
	   (bytes-per-line (ceiling bits-per-line 8)))

      (labels ((read-a-byte ()
		 (peek-char #\x fstream)
		 (read-char fstream)
		 (+ (* 16 (digit-char-p (read-char fstream) 16))
		    (digit-char-p (read-char fstream) 16))))

	(dotimes (i height)
	  (dotimes (j bytes-per-line)
	    (let* ((byte (read-a-byte))
		   (bit-index (* j 8)))
	      (multiple-value-bind (pixel-offset offset-in-pixel)
		  (truncate bit-index bits-per-pixel)
		(if (<= bits-per-pixel 8)
		    (dotimes (bit (/ 8 bits-per-pixel))
		      (let ((zz (+ pixel-offset bit)))
			(when (< zz width)
			  (setf (aref data i zz)
			    (ldb (byte bits-per-pixel (* bits-per-pixel bit)) 
				 byte)))))
		  (progn
		    (assert (zerop (mod bits-per-pixel 8)))
		    (when (< pixel-offset width)
		      (setf (aref data i pixel-offset)
			(dpb byte
			     (byte 8 offset-in-pixel)
			     (aref data i pixel-offset)))))))))))
      (values data (list +black+ +white+)))))

(defun read-xpm-file (fstream palette)
  (multiple-value-bind (width height depth left-pad format
			chars-per-pixel line)
      (get-bitmap-file-properties fstream)
    (declare (ignore line depth left-pad))
    (assert (= format 1))
    (flet ((read-strings ()
	     (let ((strings nil))
	       (loop
		 (peek-char #\" fstream)
		 (push (read fstream) strings)
		 (let ((next (peek-char t fstream)))
		   (if (eq next #\,) (read-char fstream)
		     (return (nreverse strings)))))))
	   (convert-color (x)
	     (if palette
		 (find-named-color x palette)
	       x)))
      (let* ((codes (make-array (expt char-code-limit chars-per-pixel)))
	     (designs
	      (do ((color-specs (read-strings) (cddr color-specs))
		   (designs nil)
		   (index 0 (1+ index)))
		  ((null color-specs) (nreverse designs))
		(let ((code (first color-specs))
		      (color (convert-color (second color-specs)))
		      (key 0))
		  (dotimes (i chars-per-pixel)
		    (setq key (+ (char-code (schar code i))
				 (* key char-code-limit))))
		  (setf (svref codes key) index)
		  (push color designs))))
	     (array (make-array (list height width)))
	     (pixels (read-strings))
	     (i 0))
	(declare (type (simple-array t (* *)) array)
		 (string row))
	(excl::fast
	 (dolist (row pixels)
	   (let ((index 0))
	     (dotimes (j width)
	       (let ((key 0))
		 (dotimes (i chars-per-pixel)
		   (setq key (+ (char-code (schar row index))
				(* key char-code-limit)))
		   (incf index))
		 (setf (aref array i j) (svref codes key)))))
	   (incf i)))
	(values array designs)))))

(defmethod read-image-file ((format (eql :xbm)) pathname palette)
  (declare (ignore palette))
  (if (streamp pathname)
      (read-x11-bitmap-file pathname)
    (with-open-file (fstream pathname :direction :input)
      (read-x11-bitmap-file fstream))))

(defmethod read-image-file ((format (eql :xpm)) pathname palette)
  (if (streamp pathname)
      (read-xpm-file pathname palette)
    (with-open-file (fstream pathname :direction :input)
      (read-xpm-file fstream palette))))

(defun read-xbitmap-file (pathname &key (format :xbm) (port (find-port)))
  (declare (type (or pathname string stream) pathname))
  (let ((palette (and port (port-default-palette port))))
    (read-image-file format pathname palette)))

 ;;;*******************

(defparameter silica::*clim-icon-pattern* nil)
(defparameter *clim-icon-pathname* (pathname "sys:code;clim.ico"))
(defparameter *clim-icon* nil)

(defun get-clim-icon (&optional (path *clim-icon-pathname*))
  (load-icon-from-file path))

(defun clim-icon ()
  (or *clim-icon* (setq *clim-icon* (get-clim-icon))))

(defun load-icon-from-file (path)
  (when (probe-file path)
    (multiple-value-bind (array texture mask-array)
	(read-bitmap-file path)
      (create-icon array texture mask-array))))

(defun icon-width ()
   (win:GetSystemMetrics win:SM_CXICON))

(defun icon-height ()
   (win:GetSystemMetrics win:SM_CYICON))

(defun create-icon (pixmap texture-info mask-bitmap) ;; <7>
  (win:CreateIcon *hinst*
		  (icon-width)
		  (icon-height)
		  1 ;; planes (texture-info-bits-per-pixel texture-info)
		  (texture-info-bits-per-pixel texture-info)
		  mask-bitmap
		  pixmap))
		

(in-package :clim-utils)

(setf (gethash "white" clim-utils::*default-named-color-table*)
      clim:+white+)

(setf (gethash "black" clim-utils::*default-named-color-table*)
      clim:+black+)

(setf (gethash "red" clim-utils::*default-named-color-table*)
      clim:+red+)

(setf (gethash "green" clim-utils::*default-named-color-table*)
      clim:+green+)

(setf (gethash "blue" clim-utils::*default-named-color-table*)
      clim:+blue+)

(setf (gethash "cyan" clim-utils::*default-named-color-table*)
      clim:+cyan+)

(setf (gethash "yellow" clim-utils::*default-named-color-table*)
      clim:+yellow+)

(setf (gethash "magenta" clim-utils::*default-named-color-table*)
      clim:+magenta+)




