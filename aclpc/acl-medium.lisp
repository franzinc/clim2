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

#+acl86win32 (defvar null 0)

(defclass acl-medium (basic-medium)
    ((background-dc-image)
     (foreground-dc-image)))

(defmethod engraft-medium :after ((medium acl-medium) port sheet)
  (declare (ignore port))
  (with-slots (background-dc-image foreground-dc-image) medium
    (setf background-dc-image (dc-image-for-ink medium (medium-background medium)))
    (setf foreground-dc-image (dc-image-for-ink medium (medium-foreground medium)))))

(defmethod (setf medium-background) :after (new-background (medium acl-medium))
  (with-slots (background-dc-image) medium
    (setf background-dc-image (dc-image-for-ink medium new-background))))

(defmethod (setf medium-foreground) :after (new-foreground (medium acl-medium))
  (with-slots (foreground-dc-image) medium
    (setf foreground-dc-image (dc-image-for-ink medium new-foreground))))

(defclass acl-window-medium (acl-medium)
    ((window :initform nil :reader medium-drawable)))

(defmethod make-medium ((port acl-port) sheet)
  (make-instance 'acl-window-medium
    :port port
    :sheet sheet))

#| ;;; method for basic port is sufficient 
(defmethod allocate-medium ((port acl-port) sheet)
  (let ((medium (or (pop (silica::port-medium-cache port))
		    (make-medium port sheet))))
    (with-slots (window) medium
      (setf window (sheet-mirror sheet)))
    medium))
|#

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
      (setq winrgn (win::createRectRgn cleft ctop cright cbottom))
      (win::selectObject dc winrgn)
      (setf *created-region* winrgn)) 
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

;; changing the conversion as follows makes CLIM gray colors 1/4, 1/2
;; and 3/4 gray colors map to the corresponding windows solid system
;; colors avoiding need for windows to stipple. (cim 10/11/96)

(defun color->wincolor (ink)
  (flet ((convert (x)
	   (if (< x 1.0)
	       (floor (* x #x100))
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

    


(defmethod dc-image-for-ink (medium (ink color))
  (declare (ignore medium))
    (let ((color (color->wincolor ink)))
      (declare (fixnum color))
      (case color
	((#x000000) *black-image*)
	((#xffffff) *white-image*)
	(otherwise
	  (let ((pen (note-created 'pen (win::createPen win::ps_solid 1 color)))
		(brush (note-created 'brush (win::createSolidBrush color))))
		  (when *created-pen*
			;(note-destroyed *created-pen*)
			;(win::selectObject dc *white-pen*)
			;(win::deleteObject dc *created-pen*)
			(push *created-pen* *extra-objects*))
	    (setf *created-pen* pen *created-brush* brush)
	    (make-dc-image :solid-1-pen pen
			   :brush brush
			   :text-color color :background-color nil))))))

;;; ink for opacities, regions, etc

(defmethod dc-image-for-ink (medium (ink (eql +transparent-ink+)))
  (declare (ignore medium))
  *blank-image*)

(defmethod dc-image-for-ink (medium (ink (eql +everywhere+)))
  (dc-image-for-ink medium +foreground-ink+))

(defmethod dc-image-for-ink (medium (ink standard-opacity))
  #+ignore (declare (ignore medium))
  #+ignore
  (cerror "Return Opacity 0" "Can't handle Opacities other than 0 and 1")
  (if (>= (opacity-value ink) 0.5)
    (dc-image-for-ink medium +foreground-ink+)
    *blank-image*))

(defmethod dc-image-for-ink (medium (ink region))
  #+ignore (declare (ignore medium))
  (dc-image-for-ink medium +foreground-ink+))


;;; ink for patterns, tiles, etc

(defconstant bmdim 32)

(defvar *bitmap-array* nil)  ;;; probably not needed

;;; new just for now, later rationalize passing dc
(defvar *the-dc* nil)

;;;  just for now, later rationalize passing dc
(defun dc-image-for-multi-color-pattern (medium array designs)
  (let* ((dc-image (copy-dc-image
		     (slot-value medium 'foreground-dc-image)))
	 (nocolor (and (eq (aref designs 0) +background-ink+)
		       (eq (aref designs 1) +foreground-ink+)))
	 (tcolor (unless nocolor
		   (color->wincolor (aref designs 1))))
	 (bcolor (unless nocolor 
		   (color->wincolor (aref designs 0))))
	 (width (array-dimension array 1))
	 (height (array-dimension array 0))
	 (into (pc::make-pixel-map width height 256))
	 color
	 )
    (dotimes (i (length designs))
      (setf color (aref (the vector designs) i))
      #+ignore (format *terminal-io* "~%~S" color)
      (cond
	((eql color clim:+foreground-ink+)
	 (setf (aref (the vector designs) i) (medium-foreground medium)))
	((eql color clim:+background-ink+)
	 (setf (aref (the vector designs) i) (medium-background medium)))))      
    (dotimes (i height)
      (dotimes (j width)
	#+ignore (pc::pixel-map-set into i j (aref array i j))
	(setf (aref into i j) (aref array i j))))
    (setf *bitmap-array* into)
    (let ((bitmap (acl-clim::get-texture
		    *the-dc*
		    ; (pc::null-handle win::hdc)
		    into designs)))	
      (setf (dc-image-bitmap dc-image) bitmap
	    *created-bitmap* bitmap)
      (setf (dc-image-background-color dc-image)
	    (if nocolor
	      (dc-image-text-color
		(slot-value medium 'background-dc-image))
	      bcolor))
      (if tcolor (setf (dc-image-text-color dc-image) tcolor))
      )
    dc-image))

;; Paul, this hack was done by sdj for the BBN code because of byte
;; ordering bugs in the pattern decoding code. There are a number of
;; other bugs to look into in the pattern code and when these are
;; looked at this hack should be removed (cim 10/14/96)

(defun re-order-hack (a)
  (let* ((x-dim (array-dimension a 0))
         (y-dim (array-dimension a 1))
         (y-dim-floor (* 8 (floor (/ y-dim 8))))
         (new-y-dim (* 8 (floor (/ (+ y-dim 7) 8))))
         (offset (- new-y-dim y-dim))
         (b (make-array (list x-dim new-y-dim) :initial-element 0)))
    (dotimes (i x-dim b)
      (dotimes (j (/ y-dim-floor 8))
        (dotimes (k 4)
          (setf (aref b i (+ (* j 8) k))
                (aref a i (- (* (1+ j) 8) (1+ k))))
          (setf (aref b i (- (* (1+ j) 8) (1+ k)))
                (aref a i (+ (* j 8) k)))))
      (when (> offset 0)
        (dotimes (j 4)
          (when (< (+ j offset) 8)
            (setf (aref b i (+ y-dim-floor offset j))
                  (aref a i (- y-dim (+ j 1)))))
          (when (< j (- 4 offset))
            (setf (aref b i (- new-y-dim (+ j 1)))
                  (aref a i (+ y-dim-floor j)))))))))  

(defmethod dc-image-for-ink (medium (ink pattern))
  (multiple-value-bind (array designs)
    (decode-pattern ink)
	(setq array (re-order-hack array))
	(unless (= (length designs) 2)
	  (return-from dc-image-for-ink
				   (dc-image-for-multi-color-pattern
					 medium array designs)))
	(let* ((dc-image (copy-dc-image
					   (slot-value medium 'foreground-dc-image)))
		   ;; let's no try and use the color stuff yet because it
		   ;; shows up bugs requiring hacks in the bbn code (cim 9/13/96)
		   (nocolor t
					#+notyet
					(and (eq (aref designs 0) +background-ink+)
						 (eq (aref designs 1) +foreground-ink+)))
		   (tcolor (unless nocolor
					 (color->wincolor (aref designs 1))))
		   (bcolor (unless nocolor 
					 (color->wincolor (aref designs 0))))
		   (width (array-dimension array 1))
		   (height (array-dimension array 0))
		   (dim1 (* (ceiling width 32) 32)) 
		   (into
			 (make-array (list height dim1)
						 :element-type 'bit
						 :initial-element 0)))
	  (dotimes (i height)
		(dotimes (j width)
		  (setf (aref into i j)  ;;; vector: (+ (* i dim1) j)
				(if (zerop (aref array i j))
					0
					1))))
	  (setf *bitmap-array* into)
	  (let ((bitmap
			  (note-created 'bitmap (win::createBitmap dim1 height 1 1
								 #+aclpc (acl::%get-pointer into 4 0)
								 #+acl86win32 into
								 ;;;  %kernel-arhvec = 4
								 ))))	
		(setf (dc-image-bitmap dc-image) bitmap
			  *created-bitmap* bitmap)
		(setf (dc-image-background-color dc-image)
			  (if nocolor
				  (dc-image-text-color
					(slot-value medium 'background-dc-image))
				  bcolor))
		(if tcolor (setf (dc-image-text-color dc-image) tcolor))
		)
	  dc-image)))

(defmethod dc-image-for-ink (medium (ink rectangular-tile))
  ;; The only case we handle right now is stipples
  (multiple-value-bind (array width height)
    (decode-tile-as-stipple ink)
    (unless array
      (error "Rectangular tiles other than ~~
stipples are not supported yet."))
    (let ((into (make-array `(,bmdim ,bmdim) :element-type 'bit
			    :initial-element 0))
	  (dc-image (copy-dc-image
		      (slot-value medium 'foreground-dc-image)))
	  (width (array-dimension array 1))
	  (height (array-dimension array 0)))
      (dotimes (i bmdim)
	(dotimes (j bmdim)
	  (setf (aref into i j)
		(logand 1 (lognot (aref array (mod i height)
			      (mod j width)))))))
      (let* ((bitmap (note-created 'bitmap (win::createBitmap bmdim bmdim 1 1
				       #+acl86win32 into
                                       #+aclpc (acl::%get-pointer into 4 0))))
	     (brush (note-created 'brush (win::createPatternBrush bitmap))))
	(setf (dc-image-brush dc-image) brush
	      *created-brush* brush)
	(setf *created-tile* bitmap)
	(setf (dc-image-background-color dc-image)
	      (dc-image-text-color
		(slot-value medium 'background-dc-image))))
      dc-image)))

(defmethod dc-image-for-ink (medium (ink flipping-ink))
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
      (let ((pen (note-created 'pen (win::createPen win::ps_solid 1 color)))
	    (brush (note-created 'brush (win::createSolidBrush color))))
	   (when *created-pen*
		 ;(note-destroyed *created-pen*)
		 ;(win::selectObject dc *white-pen*)
		 ;(win::deleteObject dc *created-pen*)
		 (push *created-pen* *extra-objects*))
	(setf *created-pen* pen *created-brush* brush)
	(make-dc-image :solid-1-pen pen
		       :brush brush
		       :rop2 win::r2_xorpen
		       :text-color color :background-color nil)))))

(defmethod dc-image-for-ink (medium (ink contrasting-ink))
  (dc-image-for-ink medium (make-color-for-contrasting-ink ink)))

(defmethod medium-draw-point* ((medium acl-medium) x y)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
      (when (select-acl-dc medium window dc)
        (let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (medium-line-style medium)))
          (convert-to-device-coordinates transform x y)
          (set-dc-for-ink dc medium ink line-style)
          ;(win::rectangle dc x y (+ x 1) (+ y 1))  pity it doesn't work
	  (win::moveToEx dc x y null)
	  (win::lineTo dc (+ x 1) (+ y 1))
	  )))))

(defmethod medium-draw-points* ((medium acl-medium) position-seq)
 (let ((window (medium-drawable medium)))
    (with-dc (window dc)
      (when (select-acl-dc medium window dc)
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
              ;(win::rectangle dc x y x y)
	      (win::moveToEx dc x y null)
	      (win::lineTo dc (+ x 1) (+ y 1)))))))))

(defmethod medium-draw-line* ((medium acl-medium) x1 y1 x2 y2)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
      (when (select-acl-dc medium window dc)
        (let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (medium-line-style medium)))
          (convert-to-device-coordinates transform x1 y1 x2 y2)
          (set-dc-for-ink dc medium ink line-style)
          (win::moveToEx dc x1 y1 null)
          (win::lineTo dc x2 y2))))))

(defmethod medium-draw-lines* ((medium acl-medium) position-seq)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
      (when (select-acl-dc medium window dc)
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
              (win::moveToEx dc x1 y1 null)
              (win::lineTo dc x2 y2))))))))

(defmethod medium-draw-rectangle* ((medium acl-medium)
				   left top right bottom filled)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
      (when (select-acl-dc medium window dc)
        (let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (and (not filled) (medium-line-style medium))))
          (convert-to-device-coordinates transform
					 left top right bottom)
          (when (< right left) (rotatef right left))
          (when (< bottom top) (rotatef bottom top))
          (if (typep ink 'pattern)
	    (with-compatible-dc (dc cdc)
	      (let ((width (- right left))
		    (height (- bottom top))
		    (*the-dc* dc)
		    pat)
                (set-dc-for-ink dc medium +foreground-ink+ nil) ; avoid pattern
	        (setf pat (set-cdc-for-pattern cdc medium ink nil))
	        (win::bitblt dc left top width height
			    cdc 0 0 pat) ; cause 0 is foreground
	      ))
	    (progn
	      (let ((*the-dc* dc))
		(set-dc-for-ink dc medium ink line-style))
	      #+ignore
	      (if (typep ink 'rectangular-tile)
		(cerror "Go" "Stop ~S" ink))
	      (if filled
		(win::rectangle dc left top (1+ right) (1+ bottom))
		(win::rectangle dc left top right bottom)))))))))

(defmethod medium-draw-rectangles* ((medium acl-medium) position-seq filled)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
      (when (select-acl-dc medium window dc)
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
	      (win::rectangle dc left top (1+ right) (1+ bottom))
	      (win::rectangle dc left top right bottom)))))))))

(defmethod medium-draw-polygon* ((medium acl-medium)
				 position-seq closed filled)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
      (when (select-acl-dc medium window dc)
	(let* ((sheet (medium-sheet medium))
	       (transform (sheet-device-transformation sheet))
	       (ink (medium-ink medium))
	       (line-style (and (not filled) (medium-line-style medium)))
	       (length (length position-seq))
	       (numpoints (floor length 2))
	       (extra (and closed line-style))
	       #-acl86win32 (point-vector (ct::ccallocate (win::point 256)))
           #+acl86win32 (point-vector (ct::callocate (:long *) :size 512))) ;  limit?
	  ;; These really are fixnums, since we're fixing coordinates below
	  ; (declare (type fixnum minx miny))
	  (with-stack-array (points (if extra (+ length 2) length))
	    (declare (type simple-vector points))
	    (replace points position-seq)	;set up the initial contents
	    (do ((i 0 (+ i 2))
		 (j 0 (+ j 1)))
		((>= i length))
	      (let ((x (svref points i))
		    (y (svref points (1+ i)))
		    #-acl86win32 (pstruct (ct::callocate win::point)))
		(convert-to-device-coordinates transform x y)
		#-acl86win32 (ct::csets win::point pstruct win::x x win::y y)
		#-acl86win32 (ct::cset (win::point 256)
			 point-vector ((fixnum j)) pstruct)
        #+acl86win32 
        (ct::cset (:long 512) point-vector ((fixnum (* j 2))) x)
        #+acl86win32 
        (ct::cset (:long 512) point-vector ((fixnum (+ 1 (* j 2)))) y)
		(when (and (= j 0) extra)
		  #-acl86win32 (ct::cset (win::point 256) 
			   point-vector ((fixnum numpoints)) pstruct)
          #+acl86win32 
          (ct::cset (:long 512) point-vector ((fixnum (* numpoints 2))) x)
          #+acl86win32 
          (ct::cset (:long 512) point-vector ((fixnum (+ 1 (* numpoints 2)))) y)
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
	    (win::polygon dc point-vector numpoints)
	    (win::polyline dc
			  point-vector
			  (if (and closed line-style)
			    (+ numpoints 1)
			    numpoints))))))))

(defconstant *ft* 0.0001)
(defun-inline fl-= (x y) (< (abs (- x y)) *ft*))

(defmethod medium-draw-ellipse* ((medium acl-medium)
				 center-x center-y
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
      (when (select-acl-dc medium window dc)
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
		     (win::ellipse dc left top right bottom))
		    ((null line-style)
		     ;; drawing a pie slice
		     (win::pie
		       dc left top right bottom
		       (round (+ (* (cos end-angle) x-radius) center-x))
		       (round (+ (* (sin end-angle) y-radius) center-y))
		       (round (+ (* (cos start-angle) x-radius) center-x))
		       (round (+ (* (sin start-angle) y-radius) center-y))))
		    (t
		      ;; drawing an arc
		      (win::arc
			dc left top right bottom
			(round (+ (* (cos end-angle) x-radius) center-x))
			(round (+ (* (sin end-angle) y-radius) center-y))
			(round (+ (* (cos start-angle) x-radius) center-x))
			(round (+ (* (sin start-angle) y-radius) center-y)))))
	      )))))))

(defmethod medium-draw-string* ((medium acl-medium)
				string x y start end align-x align-y
				towards-x towards-y transform-glyphs)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
  (when (select-acl-dc medium window dc)
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
	  (decf y ascent)	;text is positioned by its top left on acl
	  (set-dc-for-text dc medium ink (acl-font-index font))
          (let ((cstr (ct::callocate (:char *) :size 256))
		(subsize (length substring)))
	    (dotimes (i subsize)
	      (ct::cset (:char 256) cstr ((fixnum i))
		       (char-int (char substring i))))
	    (win::textOut dc x y cstr (length substring))))))))))

(defmethod medium-draw-character* ((medium acl-medium)
				   char x y align-x align-y
				   towards-x towards-y transform-glyphs)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
  (when (select-acl-dc medium window dc)
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
	(decf y ascent)				;text is positioned by its top left on acl
	(set-dc-for-text dc medium ink (acl-font-index font))
        (let ((cstr (ct::callocate (:char *) :size 2)))
	  (ct::cset (:char 2) cstr 0 (char-int char))
	  (win::textOut dc x y cstr 1))))))))

(defmethod medium-draw-text* ((medium acl-medium)
			      string-or-char x y start end
			      align-x align-y
			      towards-x towards-y transform-glyphs)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
  (if (characterp string-or-char)
      (medium-draw-character* medium string-or-char x y align-x align-y
			      towards-x towards-y transform-glyphs)
      (medium-draw-string* medium string-or-char x y start end align-x align-y
			   towards-x towards-y transform-glyphs)))))

(defmethod medium-clear-area ((medium acl-medium) left top right bottom)
  (let ((window (medium-drawable medium)))
    (with-dc (window dc)
  (when (select-acl-dc medium window dc)
    (with-slots (background-dc-image) medium
      (let* ((sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(convert-to-device-coordinates transform
	  left top right bottom)
	(when (< right left) (rotatef right left))
	(when (< bottom top) (rotatef bottom top))
	(set-dc-for-filling dc background-dc-image)
	(win::rectangle dc left top (1+ right) (1+ bottom))))))))

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
  (win::messageBeep win::MB_OK))

(defmethod medium-force-output ((medium acl-medium))
  )

(defmethod medium-clear-output ((medium acl-medium))
  )

(defmethod medium-finish-output ((medium acl-medium))
  )


;;; reading patterns from bitmap files

(defun read-bitmap-file (path &key (format :bitmap) (port (find-port)))
  (multiple-value-bind (array texture ignore width)
    (pc::load-pixmap-1 path 0 nil nil nil nil)
    (values array texture)))

(defun make-pattern-from-bitmap-file
       (path &key designs (format :bitmap) (port (find-port)))
  (multiple-value-bind (array texture-info)
      (read-bitmap-file path :port port)
    (let (colors numcolors climcolors color)
      (unless designs
	(setf colors (cg::texture-info-colors texture-info))
	(setf numcolors (length colors))
	(setf climcolors (make-array numcolors))
	(dotimes (i numcolors)
	  (setf color (aref colors i))
	  (setf (aref climcolors i)
		(make-rgb-color
		  (/ (cg::rgb-red color) 256.0)
		  (/ (cg::rgb-green color) 256.0)
		  (/ (cg::rgb-blue color) 256.0)))))
      (make-pattern array (or designs climcolors)))))

(defparameter silica::*clim-icon-pattern* nil)

(defun get-clim-icon ()
  (let ((path (merge-pathnames "clim.ico" pc::*application-directory*)))
    (when (probe-file path)
      (setf silica::*clim-icon-pattern* 
	    (make-pattern-from-bitmap-file "clim.ico" :port *acl-port*))
      #+ignore (extract-icon-from-file "clim.ico"))))

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




