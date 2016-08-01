;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

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
   (foreground-dc-image)
   ;; DC is cached value of GetDC().  This is safe as long
   ;; as we use CS_OWNDC.
   (dc :initform nil :accessor medium-dc)
   ))

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
      (setf background-dc-image bink)))
  ;; [spr 30867] (adapted from tk-silica/xt-graphics.lisp)
  ;; (alemmens, 2005-11-30)
  (repaint-sheet (medium-sheet medium) +everywhere+))

(defmethod (setf medium-foreground) :after (new-foreground (medium acl-medium))
  (with-slots (foreground-dc-image) medium
    (let ((fink (dc-image-for-ink medium new-foreground)))
      (setf foreground-dc-image fink)))
  ;; [spr 30867] (adapted from tk-silica/xt-graphics.lisp)
  ;; (alemmens, 2005-11-30)
  (repaint-sheet (medium-sheet medium) +everywhere+))


(defclass acl-window-medium (acl-medium)
    ((window :initform nil :reader medium-drawable)))

(defmethod make-medium ((port acl-port) sheet)
  (let ((m (make-instance 'acl-window-medium
	     :port port
	     :sheet sheet)))
    ;; [spr 30867]
    (setf (slot-value m 'silica::background) silica:*default-pane-background*)
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

(defmethod select-acl-dc-1 ((medium acl-medium) window dc)
  (declare (ignore window))
  (cond ((not (valid-handle dc))
	 nil)
	(t
	 (let* ((sheet (medium-sheet medium))
		(region (sheet-device-region sheet))
		(medium-region (medium-clipping-region medium))
		cleft ctop cright cbottom
		winrgn
		(valid nil))
	   (unless (eq region +nowhere+)
	     (multiple-value-setq (cleft ctop cright cbottom) 
	       (bounding-rectangle* region))
	     (setf valid t)
	     (unless (or (eq medium-region +everywhere+) 
			 (eq medium-region +nowhere+))
	       (with-bounding-rectangle* (mleft mtop mright mbottom) 
		   medium-region
		 (multiple-value-setq (valid cleft ctop cright cbottom)
		   (multiple-value-call #'ltrb-overlaps-ltrb-p
		     cleft ctop cright cbottom
		     (transform-rectangle* 
		      (sheet-device-transformation sheet)
		      mleft mtop mright mbottom))))))
	   (when valid
	     (fix-coordinates cleft ctop cright cbottom)
	     (setq winrgn (CreateRectRgn cleft ctop cright cbottom))
	     (let ((val1 (and (valid-handle winrgn)
			      (SelectObject dc winrgn))))
	       (values val1
		       winrgn)))))))

(defmacro with-selected-acl-dc ((var) (medium window dc) &rest body)
  `(multiple-value-bind (,var ..winrgn..)
       (select-acl-dc-1 ,medium ,window ,dc)
     (when ,var
       (unwind-protect
	   (progn
	     ,@body) 
	 (when (valid-handle ..winrgn..)
	   (or (win:DeleteObject ..winrgn..)
	       (error "with-selected-acl-dc: DeleteObject")))))))

(defmacro with-dc-image-for-ink ((image-var) (medium ink) &body body)
  `(let ((,image-var nil)
	 (..generated-bitmap.. nil)
	 (..generated-mask-bitmap.. nil))
     (unwind-protect 
	 (progn 
	   (multiple-value-setq (,image-var ..generated-bitmap.. ..generated-mask-bitmap..)
	     (dc-image-for-ink ,medium ,ink))
	   ,@body)
       (when (and (valid-handle ..generated-bitmap..))
	 (or (win:DeleteObject ..generated-bitmap..)
	     (error "with-dc-image-for-ink: DeleteObject: bitmap")))
       (when (and (valid-handle ..generated-mask-bitmap..))
	 (or (win:DeleteObject ..generated-mask-bitmap..)
	     (error "with-dc-image-for-ink: DeleteObject: mask-bitmap"))))))

(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +ltgray+)))
  ;; This is not supposed to be just ANY light gray.
  ;; This one is supposed to match exactly the color
  ;; used by Windows for all the other gadgets, such as
  ;; buttons and scroll bars.  MFC calls this "light
  ;; gray in the current color scheme"
  (ltgray-image *acl-port*))

(defmethod color-rgb ((color (eql +ltgray+)))
  (color-rgb (wincolor->color (win:GetSysColor win:COLOR_BTNFACE))))

(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +foreground-ink+)))
  (declare (values image created-bitmap created-mask-bitmap))
  (dc-image-for-ink medium (medium-foreground medium)))

(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +background-ink+)))
  (declare (values image created-bitmap created-mask-bitmap))
  (dc-image-for-ink medium (medium-background medium)))

(defmethod dc-image-for-ink ((medium acl-medium) (ink t))
  (declare (values image created-bitmap created-mask-bitmap))
  ;; Don't blow out if somebody tries to use a fancy ink like compose-in.
  (dc-image-for-ink medium (medium-foreground medium)))

(defun color->wincolor (ink &optional medium)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq ink +background-ink+)
	 (setq ink (medium-background medium)))
	((eq ink +foreground-ink+)
	 (setq ink (medium-foreground medium)))
	((transparent-ink-p ink)
	 (return-from color->wincolor -1)
	 ))
  (flet ((convert (x)
	   (declare (short-float x))
	   ;; the most common cases should be fast
	   (let ((v 255.0))
	     (declare (short-float v))
	     (cond ((= x 0.0) 0)
		   ((= x 1.0) #xff)
		   (t 
		    (setq v (the short-float (* x v)))
		    (values (the fixnum (round v))))))))
    (multiple-value-bind (red green blue)
	(color-rgb ink)
      (let ((color (dpb (convert (float blue)) (byte 8 16)
			(dpb (convert (float green)) (byte 8 8)
			     (convert (float red))))))
	color))))

(defun wincolor->color (color)
  (if (= color -1)
      +transparent-ink+
    (flet ((convert (x)
	     ;; the most common cases should be fast
	     (cond ((= x 0) 0.0)
		   ((= x #xff) 1.0)
		   (t (/ x 255.0)))))
      (let ((red (ldb (byte 8 0) color))
	    (green (ldb (byte 8 8) color))
	    (blue (ldb (byte 8 16) color)))
	(make-rgb-color (convert red)
			(convert green)
			(convert blue))))))

(defmethod dc-image-for-ink ((medium acl-medium) (ink color))
  (declare (values image created-bitmap created-mask-bitmap))
  (let ((cache (port-dc-cache (port medium))))
    (or (gethash ink cache)
	(setf (gethash ink cache)
	  (let ((color (color->wincolor ink medium)))
	    (declare (fixnum color))
	    (let ((pen (win:CreatePen win:PS_SOLID 1 color))
		  (brush (win:CreateSolidBrush color)))
	      (make-dc-image :solid-1-pen pen
			     :brush brush
			     :text-color color 
			     :background-color nil
			     :rop2 win:R2_COPYPEN
			     )))))))

;;; ink for opacities, regions, etc
(defmethod transparent-ink-p ((ink t)) (eq ink +nowhere+))
(defmethod transparent-ink-p ((ink standard-opacity))
  (let ((value (opacity-value ink)))
    (< value 0.5)))

;;; +transparent-ink+ is also known as +nowhere+.
(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +transparent-ink+)))
  *blank-image*)

(defmethod dc-image-for-ink ((medium acl-medium) (ink (eql +everywhere+)))
  (declare (values image created-bitmap created-mask-bitmap))
  (dc-image-for-ink medium +foreground-ink+))

(defmethod dc-image-for-ink ((medium acl-medium) (ink standard-opacity))
  (declare (values image created-bitmap created-mask-bitmap))
  (if (>= (opacity-value ink) 0.5)
    (dc-image-for-ink medium +foreground-ink+)
    *blank-image*))

(defmethod dc-image-for-ink ((medium acl-medium) (ink region))
  (declare (values image created-bitmap created-mask-bitmap))
  (dc-image-for-ink medium +foreground-ink+))

;;; ink for patterns, tiles, etc

(defconstant bmdim 32)
(defvar *bitmap-array* nil)		; probably not needed
(defvar *the-dc* nil)			; new just for now, later 
					; rationalize passing dc

(defun get-bitmapinfo (medium dc-image pixel-map colors)
  (or (dc-image-bitmapinfo dc-image)
      (setf (dc-image-bitmapinfo dc-image)
	(let* ((width (array-dimension pixel-map 1))
	       (height (array-dimension pixel-map 0)))
	  (acl-bitmapinfo colors width height medium)))))

;;;  just for now, later rationalize passing dc
(defun dc-image-for-multi-color-pattern (medium ink array designs)
  (declare (ignore ink)
	   (values (dc-image-list created-bitmap)))
  (let* ((dc-image (copy-dc-image
		    (slot-value medium 'foreground-dc-image)))
	 ;; We need to deal with the case where the pattern has only a
	 ;; single colour.  I think this is not a good way of doing
	 ;; it, instead it would be better to special case it in the
	 ;; caller, and not do this whole rigmarole
	 (single-color-p (= (length designs) 1))
	 (tink (aref designs (if single-color-p 0 1)))
	 (bink (if single-color-p +black+ (aref designs 0)))
	 (tcolor (color->wincolor tink medium))
	 (bcolor (color->wincolor bink medium))
	 (width (array-dimension array 1))
	 (height (array-dimension array 0))
	 (bits-per-pixel 8)
	 (into (make-pixel-map width height (expt 2 bits-per-pixel)))
	 (created-bitmap nil))
    (dotimes (i height)
      (dotimes (j width)
	(setf (aref into i j) (aref array i j))))
    (setf *bitmap-array* into)
    (let* ((bitmapinfo (get-bitmapinfo medium dc-image into designs))
	   (dc (GetDC 0))
	   (bitmap (unless (zerop dc)
		     (get-texture dc into bitmapinfo))))
      (when bitmap
	;; To Do: replace BITMAP with INTO and just use 
	;; device-independent bitmap operations.
	(setf (dc-image-bitmap dc-image) bitmap
	      created-bitmap bitmap)
	(setf (dc-image-background-color dc-image) bcolor)
	(setf (dc-image-text-color dc-image) tcolor)
	(win:ReleaseDC 0 dc)))
    (values dc-image
	    created-bitmap)))

;;; On Windows machines, patterns containing transparent inks are
;;; implemented as a pair of images A main "picture" image and a
;;; second "mask" image The "image" is drawn and then overlayed with
;;; the "mask".  (For an example, see the method
;;; medium-draw-rectangle*.)
;;;
;;; However, because of the way Windows does things, these bitmaps
;;; cannot be reused (or cached) so they need to be created and then
;;; destroyed.
;;;
;;; In short, if one of these patterns is used, the caller is
;;; responsible for cleaning up the structures afterwards.  Again,
;;; see the example in medium-draw-rectangle*.
(defun dc-image-for-transparent-pattern (medium ink array designs)
  (declare (ignore ink)
	   (values (dc-image-list created-bitmap created-mask-bitmap)))
  (let* ((dc-image (copy-dc-image
		    (slot-value medium 'foreground-dc-image)))
	 (dc-image-mask (copy-dc-image
			 (slot-value medium 'foreground-dc-image)))
	 (ink0 (aref designs 0))
	 (ink1 (aref designs 1))
	 (color0 (color->wincolor ink0 medium))
	 (color1 (color->wincolor ink1 medium))
	 (width (array-dimension array 1))
	 (height (array-dimension array 0))
	 (bits-per-pixel 8)
	 (copy-arr (make-pixel-map width height (expt 2 bits-per-pixel)))
	 (copy-arr-mask (make-pixel-map width height (expt 2 bits-per-pixel)))
	 (transindex 0)
	 (maincolor color1)
	 (mainink ink1) 
	 (created-bitmap nil)
	 (created-mask-bitmap nil)
	 )
    (when (not (transparent-ink-p ink0))
      (setq transindex 1
	    maincolor color0
	    mainink ink0))
    (dotimes (i height)
      (dotimes (j width)
	(cond ((= (aref array i j) transindex)
	       (setf (aref copy-arr i j) 0)
	       (setf (aref copy-arr-mask i j) 0))
	      (t
	       (setf (aref copy-arr i j) 1)
	       (setf (aref copy-arr-mask i j) 1)))
	))
    (setf *bitmap-array* copy-arr)
    (let* ((bitmapinfo (get-bitmapinfo medium dc-image copy-arr 
				       (make-array 2 :initial-contents 
						   (list clim:+white+ 
							 mainink))
				       ))
	   (dc (GetDC 0))
	   (bitmap (get-texture dc copy-arr bitmapinfo)))
      ;; To Do: replace BITMAP with COPY-ARR and just use 
      ;; device-independent bitmap operations.
      (when bitmap
	(setf (dc-image-bitmap dc-image) bitmap
	      created-bitmap bitmap)
	(setf (dc-image-background-color dc-image) 
	  (color->wincolor clim:+white+ medium))
	(setf (dc-image-text-color dc-image) maincolor)
	(let* ((bitmapinfo-mask 
		(get-bitmapinfo medium dc-image-mask copy-arr-mask
				(make-array 2 :initial-contents 
					    (list clim:+black+ 
						  mainink))
				))
	       (dc-mask dc)
	       (bitmap-mask (get-texture dc-mask copy-arr-mask 
					 bitmapinfo-mask)))
	  ;; To Do: replace BITMAP with COPY-ARR and just use 
	  ;; device-independent bitmap operations.
	  (setf (dc-image-bitmap dc-image-mask) bitmap-mask)

	  (setf created-mask-bitmap bitmap-mask)

	  (setf (dc-image-background-color dc-image-mask) 
	    (color->wincolor clim:+black+ medium))
	  (setf (dc-image-text-color dc-image-mask) maincolor)
	  ;; I guess we don't want to release the device context
	  ;; until we destroy the rest of the image.
	  ;;(unless (zerop dc) (win:ReleaseDC 0 dc))
	  ;;(unless (zerop dc-mask) (win:ReleaseDC 0 dc-mask))
	  ;;
	  ;; Windows does not support transparent ink directly.  We have to
	  ;; create transparency with two images.
	  (win:ReleaseDC 0 dc)
	  (values
	   (list dc-image
		 dc-image-mask)
	   created-bitmap
	   created-mask-bitmap)
	  )))))

(defun byte-align-pixmap (a)
  ;; Pad the pixmap so that the y dimension is a multiple of 4.
  ;; This copies the array, so you oughta cache the result.
  (let* ((x-dim (array-dimension a 0))
         (y-dim (array-dimension a 1))
         (new-y-dim (* 4 (ceiling y-dim 4)))
         (b nil))
    (cond ((= y-dim new-y-dim) a)	; already aligned
	  (t
	   (setq b (make-array (list x-dim new-y-dim) :initial-element 0))
	   (dotimes (y new-y-dim)
	     (dotimes (x x-dim)
	       (setf (aref b x y)
		 (if (>= y y-dim) 0 (aref a x y)))))
	   b))))

(defun pattern-to-hatchbrush (pattern)
  (multiple-value-bind (array designs) (decode-pattern pattern)
    (let* ((tcolor (position-if 
		    #'(lambda (ink) (not (transparent-ink-p ink)))
		    designs))
	   (style nil))
      (unless tcolor (return-from pattern-to-hatchbrush nil))
      (when (and (> (array-dimension array 0) 1)
		 (> (array-dimension array 1) 1))
	(let ((a11 (= (aref array 0 0) tcolor))
	      (a12 (= (aref array 0 1) tcolor))
	      (a21 (= (aref array 1 0) tcolor))
	      (a22 (= (aref array 1 1) tcolor)))
	  (setq style 
	    (cond ((and a12 a21 a22) win:HS_CROSS)
		  ((and a12 a21) win:HS_FDIAGONAL)
		  ((and a11 a22) win:HS_BDIAGONAL)
		  ((and a11 a12) win:HS_HORIZONTAL)
		  ((and a11 a21) win:HS_VERTICAL)))))
      (unless style (setq style win:HS_FDIAGONAL))
      (win:CreateHatchBrush
       style
       (color->wincolor (elt designs tcolor))))))

(defmethod dc-image-for-ink ((medium acl-medium) (ink pattern))
  (declare (values image created-bitmap created-mask-bitmap))
  ;; The "pattern" part of the ink is put into the brush.
  ;; This will return a list of two DCs if one of the inks is transparent.
  (let ((cache (port-dc-cache (port medium))))
    (or (gethash ink cache)
	(multiple-value-bind (array designs) (decode-pattern ink)
	  (cond ((find-if #'transparent-ink-p designs)
		 ;; This returns two images, which
		 ;; are used as masks to support how 
		 ;; windows draws patterns containing
		 ;; transparent inks.
		 ;;
		 ;; NOTE: Not cached in hashtable.
		 (setq array (byte-align-pixmap array))
		 (let ((dc-image nil)
		       (created-bitmap nil)
		       (created-mask-bitmap nil))
		   (multiple-value-setq (dc-image created-bitmap created-mask-bitmap)
		     (dc-image-for-transparent-pattern medium ink array designs))
		   (loop for dci in dc-image
		       do (setf (dc-image-brush dci) 
			    (win:CreatePatternBrush (dc-image-bitmap dci))))
		   (values dc-image
			   created-bitmap
			   created-mask-bitmap)))
		(t
		 (setq array (byte-align-pixmap array))
		 (let ((dc-image (dc-image-for-multi-color-pattern medium ink array designs)))
		   (setf (dc-image-brush dc-image)
		     (win:CreatePatternBrush (dc-image-bitmap dc-image)))
		   ;; Cache it now.  There is a bug that caching a pattern that
		   ;; depends on foreground-ink or background-ink will cause
		   ;; those settings to get captured permanently.
		   (setf (gethash ink cache) dc-image)
		   )))))))

#|
does anyone know how to make the bitmaps have transparent
backgrounds or how to show gif files? i need to make pictures so that
whats under it will show, but need code in vb3!!! thanks

Joe,

You can do this by creating two pictures, 
one with white in the transparent area (The Picture)
and the other with black in the transparent area. (The Mask)

Then use the bitblt API to blit the picture with
the SRCAND (&h8800c6) flag.
Then blit the mask in the same location with
the SRCOR (&hee0086) flag.

This will give you the transparent effect.

This works because ANDing any value with 1 and XORing it with 0
leaves the original value unchanged. To draw a masked image
on-screen, Windows first logically ANDs the values in the AND mask
with the pixel values in the destination DC. This creates a "hole" for
the image composed of pixels with color values of 0. Then Windows
XORs the values in the XOR mask to the destination DC, filling in the
hole left by the AND mask and setting all the 0 pixels to the image
colors. This is standard stuff described in any entry-level graphics
programming course. It's also the technique that Windows uses to
draw icons and mouse cursors on the screen. 
|#

(defmethod dc-image-for-ink ((medium acl-medium) (ink rectangular-tile))
  (declare (values image created-bitmap created-mask-bitmap))
  ;; The only case we handle right now is stipples
  (let ((cache (port-dc-cache (port medium))))
    (or (gethash ink cache)
	(let ((pattern (decode-rectangular-tile ink)))
	  (multiple-value-bind (image created-bitmap created-mask-bitmap)
	      (dc-image-for-ink medium pattern)
	    ;; The brush of PATTERN is used as the tile.
	    (cond ((and (atom image)
			(null created-bitmap)
			(null created-mask-bitmap))
		   ;; It is not cached if it is a cons, to be consistent with
		   ;; the non-caching behavior of patterns with transparent
		   ;; inks.
		   ;; Also, don't cache if it generated a bitmap or mask.
		   (setf (gethash ink cache) image))
		  ((consp image)
		   ;; It is a transparent rectangular tile.  These don't
		   ;; work right now, so lets use a Windows hatchbrush
		   ;; for now.  We can cache a hatchbrush I think.
		   (dolist (dci image)
		     (let ((old (dc-image-brush dci)))
		       (when (valid-handle old) (win:DeleteObject old)))
		     (setf (dc-image-background-color dci) -1) ; transparent
		     (setf (dc-image-brush dci) 
		       (pattern-to-hatchbrush pattern)))
		   (setf (gethash ink cache) image)
		   (setq created-bitmap nil)
		   (setq created-mask-bitmap nil)))
	    (values image created-bitmap created-mask-bitmap))))))

(defun nyi ()
  (error "This NT CLIM operation is NYI (Not Yet Implemented)."))

(defmethod dc-image-for-ink ((medium acl-medium) (ink flipping-ink))
  (declare (values image created-bitmap created-mask-bitmap))
  (let ((cache (port-dc-cache (port medium))))
    (or (gethash ink cache)
	(setf (gethash ink cache)
	  (multiple-value-bind (ink1 ink2)
	      (decode-flipping-ink ink)
	    (let* ((image1 (dc-image-for-ink medium ink1))
		   (image2 (dc-image-for-ink medium ink2))
		   (color (logxor (dc-image-text-color image1)
				  (dc-image-text-color image2))))
	      (unless (and (eq (dc-image-rop2 image1) win:R2_COPYPEN)
			   (eq (dc-image-rop2 image2) win:R2_COPYPEN)
			   (null (dc-image-bitmap image1))
			   (null (dc-image-bitmap image2)))
		(nyi))
	      (let ((pen (win:CreatePen win:PS_SOLID 1 color))
		    (brush (win:CreateSolidBrush color)))
		(make-dc-image :solid-1-pen pen
			       :brush brush
			       :rop2 win:R2_XORPEN
			       :text-color color 
			       :background-color nil))))))))

(defmethod dc-image-for-ink ((medium acl-medium) (ink contrasting-ink))
  (declare (values image created-bitmap created-mask-bitmap))
  (dc-image-for-ink medium (make-color-for-contrasting-ink ink)))

(defmethod dc-image-for-ink ((medium acl-medium) (ink composite-out))
  (error "Compositing is not supported."))

(defmethod dc-image-for-ink ((medium acl-medium) (ink composite-in))
  (error "Compositing is not supported."))

(defmethod medium-draw-point* ((medium acl-medium) x y)
  (without-scheduling
    (let ((window (medium-drawable medium)))
      (with-medium-dc (medium dc)
	(with-selected-acl-dc (old) 
	  (medium window dc)
	  (when old
	    (let* ((sheet (medium-sheet medium))
		   (transform (sheet-device-transformation sheet))
		   (ink (medium-ink medium))
		   (line-style (medium-line-style medium)))
	      (if (identity-transformation-p transform)
		  (fix-coordinates x y)
		(convert-to-device-coordinates transform x y))
	      (with-set-dc-for-ink (dc medium ink line-style)
		;;(win:rectangle dc x y (+ x 1) (+ y 1))  pity it doesn't work
		(win:MoveToEx dc x y null)
		(win:LineTo dc (+ x 1) (+ y 1))
		(when (valid-handle old) (SelectObject dc old))
		))))))))

(defmethod medium-draw-points* ((medium acl-medium) position-seq)
  (without-scheduling
    (let ((window (medium-drawable medium)))
      (with-medium-dc (medium dc)
	(with-selected-acl-dc (old) 
	  (medium window dc)
	  (when old
	    (let* ((sheet (medium-sheet medium))
		   (transform (sheet-device-transformation sheet))
		   (ink (medium-ink medium))
		   (line-style (medium-line-style medium))
		   (L (length position-seq)))
	      (with-set-dc-for-ink (dc medium ink line-style)
		(do ((i 0 (+ i 2))
		     (j 1 (+ j 2)))
		    ((>= i L))
		  (let ((x (elt position-seq i))
			(y (elt position-seq j)))
		    (convert-to-device-coordinates transform x y)
					;(win:rectangle dc x y x y)
		    (win:MoveToEx dc x y null)
		    (win:LineTo dc (+ x 1) (+ y 1))))
		(when (valid-handle old) (SelectObject dc old))))))))))

(defmethod medium-draw-line* ((medium acl-medium) x1 y1 x2 y2)
  (without-scheduling
    (let ((window (medium-drawable medium)))
      (with-medium-dc (medium dc)
	(with-selected-acl-dc (old) 
	  (medium window dc)
	  (when old
	    (let* ((sheet (medium-sheet medium))
		   (transform (sheet-device-transformation sheet))
		   (ink (medium-ink medium))
		   (line-style (medium-line-style medium)))
	      (convert-to-device-coordinates transform x1 y1 x2 y2)
	      (with-set-dc-for-ink (dc medium ink line-style)
		(win:MoveToEx dc x1 y1 null)
		(win:LineTo dc x2 y2)
		(when (valid-handle old) (SelectObject dc old))))))))))

(defmethod medium-draw-lines* ((medium acl-medium) position-seq)
  #+slow-but-sure
  (let (oldx oldy drawp)
    (silica:map-position-sequence
     #'(lambda (x y)
	 (when (and oldx drawp)
	   (clim:medium-draw-line* medium oldx oldy x y))
	 (setq oldx x oldy y
	       drawp (not drawp)))
     position-seq))
  (without-scheduling
    (let ((window (medium-drawable medium)))
      (with-medium-dc (medium dc)
	(with-selected-acl-dc (old) 
	  (medium window dc)
	  (when old
	    (let* ((sheet (medium-sheet medium))
		   (transform (sheet-device-transformation sheet))
		   (ink (medium-ink medium))
		   (line-style (medium-line-style medium)))
	      (with-set-dc-for-ink (dc medium ink line-style)
		;; Do we really need all this gratuitous LABELS stuff?
		;; -- perhaps the compiler generates better code?
		(let (drawp)
		  (declare (optimize (speed 3) (safety 0)))
		  (labels ((visit1 (x y)
			     (fix-coordinates x y)
			     (if drawp
				 (win:LineTo dc x y)
				 (win:MoveToEx dc x y null))
			     (setf drawp (not drawp)))
			   (visit2 (x y)
			     (convert-to-device-coordinates transform x y)
			     (if drawp
				 (win:LineTo dc x y)
				 (win:MoveToEx dc x y null))
			     (setf drawp (not drawp))))
		    (declare (dynamic-extent #'visit1 #'visit2))
		    (if (identity-transformation-p transform)
			(silica:map-position-sequence #'visit1 position-seq)
		      (silica:map-position-sequence #'visit2 position-seq))))
		(when (valid-handle old) (SelectObject dc old))))))))))

(defmethod medium-draw-rectangle* ((medium acl-medium)
				   left top right bottom filled)
  (declare (fixnum left top right bottom)
	   (optimize (speed 3) (safety 0)))
  (without-scheduling
    (let ((window (medium-drawable medium)))
      (with-medium-dc (medium dc) 
	(with-selected-acl-dc (old) 
	  (medium window dc)
	  (when old
	    (let* ((sheet (medium-sheet medium))
		   (transform (sheet-device-transformation sheet))
		   (ink (medium-ink medium))
		   (line-style (and (not filled) (medium-line-style medium))))
	      (convert-to-device-coordinates transform
					     left top right bottom)
	      (when (< right left) (rotatef right left))
	      (when (< bottom top) (rotatef bottom top))
	      (cond ((isa-pattern ink)
		     ;; DRAW-PATTERN* ends up here.  In principle
		     ;; we could skip this case and rely on the "brush"
		   ;; to correctly paint the rectangle.  In practice,
		     ;; this case is needed to correctly render patterns
		     ;; larger than 8x8, due to limitations of CreatePatternBrush.
		     (let ((cdc nil))
		       (with-dc-image-for-ink (dc-image) 
			 (medium ink)
			
			 ;; Create compatable memory dc
			 (setq cdc (win:CreateCompatibleDC dc))
			 (assert (valid-handle cdc))	
			 (cond ((listp dc-image)
				;; This is the case of a pattern that contains transparent ink.
				;; Windows does not support transparent ink directly.
				;; You can do this by creating two pictures, 
				;; one with white in the transparent area (The Picture)
				;; and the other with black in the transparent area. (The Mask)
				;; Then use the bitblt API to blit the picture with
				;; the SRCAND (&h8800c6) flag.  Then blit the mask in the 
				;; same location with the SRCOR (&hee0086) flag.
				(let* ((dci-pict (first dc-image))
				       (dci-mask (second dc-image))
				       (pictbm (dc-image-bitmap dci-pict))
				       (maskbm (dc-image-bitmap dci-mask)))
				  ;; select a (Device-Dependent) bitmap into the dc
				  (when (valid-handle pictbm) (SelectObject cdc pictbm))
				  ;; Copy bitmap from memory dc to screen dc
				  (win:BitBlt dc left top (- right left) (- bottom top)
					      cdc 0 0 
					      win:SRCAND)
			 
				  (when (valid-handle maskbm) (SelectObject cdc maskbm))
				  (win:BitBlt dc left top (- right left) (- bottom top)
					      cdc 0 0 
					      acl-clim::SRCOR)
				  
				  (destroy-dc-image dci-pict :destroy-bitmap nil)
				  (destroy-dc-image dci-mask :destroy-bitmap nil)
				  ))
			       (t
				;; select a (Device-Dependent) bitmap into the dc
				(let ((bm (dc-image-bitmap dc-image)))
				  (when (valid-handle bm) (SelectObject cdc bm)))
				;; Copy bitmap from memory dc to screen dc
				(win:BitBlt dc left top (- right left) (- bottom top)
					    cdc 0 0 win:SRCCOPY)))
			 ;; Delete memory dc
			 (win:DeleteDC cdc)
			 (when (valid-handle old) (SelectObject dc old))))
		     t)
		    (t
		     (let ((*the-dc* dc))
		       (with-set-dc-for-ink (dc medium ink
						(if filled nil line-style)
						left top)
			 (if filled
			     (win:Rectangle dc left top (1+ right) (1+ bottom))
			   ;; right and bottom need to be incremented
			   ;; for consistent behavior with unix.
			   (win:Rectangle dc left top (1+ right) (1+ bottom)))
			 (when (valid-handle old) (SelectObject dc old))
			 t
			 )))))))))))

(defmethod medium-draw-rectangles* ((medium acl-medium) position-seq filled)
  (without-scheduling
    (let ((window (medium-drawable medium)))
      (with-medium-dc (medium dc)
	(with-selected-acl-dc (old) 
	  (medium window dc)
	  (when old
	    (let* ((sheet (medium-sheet medium))
		   (transform (sheet-device-transformation sheet))
		   (ink (medium-ink medium))
		   (line-style (and (not filled) (medium-line-style medium)))
		   (numrects (floor (length position-seq) 4))
		   (rect -1))
	      (with-set-dc-for-ink (dc medium ink line-style)
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
			(win:Rectangle dc left top (1+ right) (1+ bottom))
		      (win:Rectangle dc left top right bottom))))
		(when (valid-handle old) (SelectObject dc old))))))))))

(defparameter *point-vector* 
    (ff:allocate-fobject `(:array :long 100) :foreign-static-gc))

(defparameter *point-vector-type* 
    (ff:compile-foreign-type `(:array :long 100)))

(defun set-point (vector i x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum i))
  (setf (ff:fslot-value-typed *point-vector-type* :foreign
			      vector i)
    x)
  nil)

(defun fill-point-vector (vector transform position-seq closed)
  (declare (optimize (speed 3) (safety 0)))
  (let ((i 0))
    (labels ((visit1 (x y)
	       (fix-coordinates x y)
	       (set-point vector i x)
	       (incf i)
	       (set-point vector i y)
	       (incf i))
	     (visit2 (x y)
	       (convert-to-device-coordinates transform x y)
	       (set-point vector i x)
	       (incf i)
	       (set-point vector i y)
	       (incf i)))
      (declare (dynamic-extent #'visit1 #'visit2))
      (if (identity-transformation-p transform)
	  (silica:map-position-sequence #'visit1 position-seq)
	(silica:map-position-sequence #'visit2 position-seq))
      (when closed
	;; Make the first point be the last.
	(set-point vector i 
		   (ff:fslot-value-typed *point-vector-type* :foreign
					 vector 0))
	(incf i)
	(set-point vector i 
		   (ff:fslot-value-typed *point-vector-type* :foreign
					 vector 1))
	(incf i))
      i)))

(defun draw-polygon-1 (medium position-seq dc filled closed old)
  (let* ((sheet (medium-sheet medium))
	 (transform (sheet-device-transformation sheet))
	 (ink (medium-ink medium))
	 (line-style (and (not filled) (medium-line-style medium)))
	 (length (length position-seq))
	 (numpoints (floor length 2))
	 (point-vector nil))
    (when (and closed (not filled))
      (incf length 2)
      (incf numpoints))
    (setq point-vector *point-vector*)
    (when (< (length point-vector) length)
      (setq point-vector (ff:allocate-fobject `(:array :long ,length)))
      (setq *point-vector* point-vector))
    (fill-point-vector point-vector transform position-seq
			       (and closed (not filled)))
    (with-set-dc-for-ink (dc medium ink line-style)
      (if filled
	  (win:Polygon dc point-vector numpoints)
	(win:Polyline dc point-vector numpoints))
      (when (valid-handle old) (SelectObject dc old))
      t)))

(defmethod medium-draw-polygon* ((medium acl-medium) position-seq closed filled)
  (without-scheduling
    (let ((window (medium-drawable medium))
	  (length (length position-seq)))
      (assert (evenp length))
      (with-medium-dc (medium dc)
	(with-selected-acl-dc (old) 
	  (medium window dc)
	  (when old
	    (draw-polygon-1 medium position-seq dc filled closed old)))))))

(defconstant *ft* 0.0001)
(defun-inline fl-= (x y) (< (abs (- x y)) *ft*))

(defmethod medium-draw-ellipse* ((medium acl-medium)
				 center-x center-y
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (without-scheduling
    (let ((window (medium-drawable medium)))
      (with-medium-dc (medium dc)
	(with-selected-acl-dc (old) 
	  (medium window dc)
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
	      (with-set-dc-for-ink (dc medium ink line-style)
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
			   #+ignore
			   (and (= start-angle 0)
				(= end-angle 2pi))
			   ;; drawing a full ellipse
			   (win:Ellipse dc left top right bottom))
			  ((null line-style)
			   ;; drawing a pie slice
			   (win:Pie
			    dc left top right bottom
			    (round (1- (+ center-x (* (cos start-angle) x-radius))))
			    (round (1- (- center-y (* (sin start-angle) y-radius))))
			    (round (1- (+ center-x (* (cos end-angle) x-radius))))
			    (round (1- (- center-y (* (sin end-angle) y-radius))))))
			  (t
			   ;; drawing an arc
			   (win:Arc
			    dc left top right bottom
			    (round (1- (+ center-x (* (cos start-angle) x-radius))))
			    (round (1- (- center-y (* (sin start-angle) y-radius))))
			    (round (1- (+ center-x (* (cos end-angle) x-radius))))
			    (round (1- (- center-y (* (sin end-angle) y-radius)))))))
		    ))
		(when (valid-handle old) (SelectObject dc old))))))))))

(defmethod flipping-ink-p ((object t)) nil)
(defmethod flipping-ink-p ((object clim-utils:flipping-ink)) t)

(defmethod medium-draw-string* ((medium acl-medium)
				string x y start end align-x align-y
				towards-x towards-y transform-glyphs)
  ;; This is not supposed to try to draw multiline text.
  ;; For that, use WRITE-STRING.
  (declare (ignore transform-glyphs))
  (without-scheduling
    (unless end (setq end (length string)))
    (let* ((transform (sheet-device-transformation (medium-sheet medium))))
      (convert-to-device-coordinates transform x y)
      (when towards-x
	(convert-to-device-coordinates transform towards-x towards-y)))
    (let* ((port (port medium))
	   (text-style (medium-merged-text-style medium))
	   (font (text-style-mapping port text-style))
	   (window (medium-drawable medium))
	   (ink (medium-ink medium))
	   (font-angle (pos-to-font-angle x y towards-x towards-y))
	   (x-adjust 
	    (compute-text-x-adjustment align-x medium 
				       string text-style start end))
	   (y-adjust
	    (compute-text-y-adjustment align-y (acl-font-descent font) 
				       (acl-font-ascent font) (acl-font-height font))))
      (incf x x-adjust)
      (incf y y-adjust)
      (when towards-x
	(incf towards-x x-adjust)
	(incf towards-y y-adjust))
      (cond ((zerop font-angle) 
	     (decf y (acl-font-ascent font))) ;text is positioned by its top left 
	    (t
	     (setq font
	       (port-find-rotated-font port font font-angle))))
      (if (flipping-ink-p ink)
	  (medium-draw-inverted-string* medium string x y start end font text-style)
	(with-medium-dc (medium dc)
	  (with-selected-acl-dc (old) 
	    (medium window dc)
	    (when old
	      (with-temporary-substring (substring string start end)
		(with-set-dc-for-text (dc medium ink (acl-font-index font))
		  (multiple-value-bind (cstr len)
		      (silica::xlat-newline-return substring)
		    (or (excl:with-native-string (cstr cstr)
			  (win:TextOut dc x y cstr len))
			(check-last-error "TextOut" :action :warn)))
		  (when (valid-handle old) (SelectObject dc old)))))))))))

(defmethod medium-draw-inverted-string* ((medium acl-medium)
					 string x y start end font text-style)
  ;; This function should be used when the ink is flipping-ink.
  ;; Strings cannot be drawn by calling (SetROP2 dc R2_XORPEN).
  ;; This has no effect on TextOut.  To get the same effect,
  ;; draw the string into a bitmap and draw the bitmap onto the screen.
  (without-scheduling
    (let ((window (medium-drawable medium))
	  (width 100)
	  (height 100))
      (multiple-value-setq (width height) 
	(text-size medium string :text-style text-style
		   :start start :end end))
      (with-medium-dc (medium dc)
	(with-selected-acl-dc (old) 
	  (medium window dc)
	  (when old
	    (with-temporary-substring (substring string start end)
	      (let* ((cdc (win:CreateCompatibleDC dc))
		     (hbm (win:CreateCompatibleBitmap cdc width height))
		     (oldbm nil))
		(when (valid-handle hbm) (setq oldbm (SelectObject cdc hbm)))
		;; Set the bitmap to black, then draw the text in white.
		(with-set-dc-for-text (cdc medium +black+ (acl-font-index font))
		  (win:Rectangle cdc 0 0 width height)
		  (win:SetTextColor dc 0) ; white
		  (with-set-dc-for-text (cdc medium +white+ (acl-font-index font))
		    (multiple-value-bind (cstr len)
			(silica::xlat-newline-return substring)
		      (or (excl:with-native-string (cstr cstr)
			    (win:TextOut cdc 0 0 cstr len))
			  (check-last-error "TextOut" :action :warn))
		      )
		    ;; Copy bitmap from memory dc to screen dc
		    (win:BitBlt dc x y width height
				cdc 0 0 win:SRCINVERT)
		    ;; Restore the old bitmap
		    (when (valid-handle oldbm) (SelectObject cdc oldbm))
		    ;; Delete the bitmap
		    (or (win:DeleteObject hbm) (error "DeleteObject"))
		    ;; Delete memory dc
		    (win:DeleteDC cdc)
		    (when (valid-handle old) (SelectObject dc old)))))))))
      nil)))

;;; Enhance the bounding-box calculation for rotated text.
(defmethod clim-internals::medium-text-bounding-box :around
	   ((medium acl-window-medium) string x y
	    start end align-x align-y text-style
	    towards-x towards-y transform-glyphs transformation)
  align-x align-y transform-glyphs transformation
  (let ((font-angle nil))
    (cond ((and (or towards-x towards-y)
		(not (= (setq font-angle
			  (pos-to-font-angle x y 
					     (or towards-x x)
					     (or towards-y y)))
			0)))
	   ;;; This is sort of expecnsive, but it
	   ;;; should only happen when the text is 
	   ;;; drawn the first time.
	   (setq towards-x (or towards-x x))
	   (setq towards-y (or towards-y y))
	   ;;; Recall, font-angle is in 1/10 dgs.
	   (let* ((font-angle-rad (* font-angle #.(/ pi (* -180 10))))
		  (width (stream-string-width (medium-sheet medium) string
					      :start start :end end
					      :text-style text-style))
		  (ascent (text-style-ascent text-style medium))
		  (descent (text-style-descent text-style medium))
		  (height (+ ascent descent))
		  (transf (make-rotation-transformation* font-angle-rad x y)))
	     (multiple-value-bind (xtr ytr)	;;; top-right corner
		 (transform-position transf (+ x width) y)
	       (multiple-value-bind (xbr ybr) ;;; bottom-right corner	     
		   (transform-position transf (+ x width) (+ y height))
		 (multiple-value-bind (xbl ybl)	;;; bottom-left corner	     
		     (transform-position transf x (+ y height))
		   (values (coordinate (min x xtr xbr xbl))
			   (coordinate (min y ytr ybr ybl))
			   (coordinate (max x xtr xbr xbl))
			   (coordinate (max y ytr ybr ybl))
			   ;;; None of the following seems to be
			   ;;; used, but...
			   (coordinate x) (coordinate y)
			   towards-x towards-y))))))
	  (t 
	   (call-next-method)))))

(defmethod medium-draw-character* ((medium acl-medium)
				   char x y align-x align-y
				   towards-x towards-y transform-glyphs)
  (medium-draw-string* medium (make-string 1 :initial-element char)
		       x y 0 1 align-x align-y towards-x towards-y
		       transform-glyphs))

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
  (win:MessageBeep win:MB_OK))

(defmethod medium-force-output ((medium acl-medium))
  )

(defmethod medium-clear-output ((medium acl-medium))
  )

(defmethod medium-finish-output ((medium acl-medium))
  )


;;; reading patterns from bitmap files

(defun read-bitmap-file (path &key (format :bitmap) (port (find-port)))
  (declare (ignore port))
  (assert (member format '(:ico :bmp :cur))); give caller reasonable error msg
  (load-pixmap-1 path 0))

(defun load-pixmap-1 (filename index) 
  (format *trace-output* "~&Loading pixmap from file ~a ..." filename)
  (multiple-value-prog1 
      (with-open-file (s filename :element-type '(unsigned-byte 8))
	(read-pixmap s index)) 
    (format *trace-output* "LOADED pixmap~%")))

(defstruct (texture-info (:constructor make-texture-info))
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
	   (setq file-type :bmp))
	  ((and (zerop (char-int char1))
		(zerop (char-int char2)))
	   (setq word2 (bm-read-word stream))
	   (cond ((eql word2 1)
		  (setq file-type :ico))
		 ((eql word2 2)
		  (setq file-type :cur)))))
    (unless file-type
      (error "Object being read is neither a ~
device-independent bitmap, an icon, nor a cursor."))
    (case file-type
      (:bmp
       (setq file-size (bm-read-long stream))
       (bm-read-word stream)		; reserved1 = 0
       (bm-read-word stream)		; reserved2 = 0
       (setq image-offset (bm-read-long stream))
       )
      ((:ico :cur)
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
       (read-byte stream)		; reserved
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
    (unless (eq file-type :bmp)
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
	(read-byte stream))		; reserved = 0
      )					; end reading color table

    (setq texture-info
      (make-texture-info :width width ;; <10>
			 :height height
			 :bits-per-pixel bits-per-pixel
			 :colors palette-array
			 :invert-p nil)) ;; <18>
    (setq pixmap (make-pixel-map
		  height
		  width	
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
	     ;; If this location is outside the pixmap,
	     ;; drop it on the floor (after having read the
	     ;; the PIXEL-BYTE from the stream).
	     (let ((real-x (+ byte-num bit-num)))
	       (if (< real-x width)
		   (pixel-map-set pixmap real-x real-y
				  (if (logbitp (- 7 bit-num) pixel-byte)
				      1 0))))
	     )))
	(4
	 ;; rows are padded to word boundary even in the file apparently
	 ;; so read 8 nibbles from each 4-byte section of the row
	 (dotimes (x (* 4 (ceiling width 8)))
	   (setq pixel-byte (read-byte stream))
	   (setq byte-num (* x 2))
	   ;; If this location is outside the pixmap,
	   ;; drop it on the floor (after having read the
	   ;; the PIXEL-BYTE from the stream).
	   (if (< byte-num width)
	       (pixel-map-set pixmap byte-num real-y
			      (ash pixel-byte -4))) ; high nibble
	   (if (< (1+ byte-num) width)
	       (pixel-map-set pixmap (1+ byte-num) real-y
			      (logand pixel-byte 15))) ; low nibble
	   ))
	(8
	 (dotimes (x (* 4 (ceiling width 4)))
	   ;; If this location is outside the pixmap,
	   ;; drop it on the floor (after having read the
	   ;; the PIXEL-BYTE from the stream).
	   (let ((REAL-BYTE (read-byte stream)))
	     (if (< x width)
		 (pixel-map-set pixmap x real-y
				REAL-BYTE)))
	   ))
	(24 (error "24-bit (direct-color) pixmaps are not implemented."))))
    ;; Return the objects that were read.
    hotspot-x hotspot-y image-size file-size number-of-resources
    x-pixels-per-meter y-pixels-per-meter
    (case file-type
      (:bmp
       (values pixmap texture-info nil width
	       ))
      ((:ico :cur)
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
  ;; give reasonable error msg:
  (assert (member format '(:xbm :xpm :xpm3 :ico :bmp :cur)))
  (if (member format '(:xbm :xpm :xpm3))
      (multiple-value-bind (array designs-from-file)
	  (read-xbitmap-file path :format format :port port)
	(make-pattern array (or designs designs-from-file)))
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
  (multiple-value-bind (width height depth left-pad format 
			chars-per-pixel line)
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
      (values data (list +background-ink+ +foreground-ink+)))))

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
	(declare (type (simple-array t (* *)) array))
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

(defun read-xpm-v3-file (stream palette)
  (labels ((ensure-next-char (c)
	     (assert (eql c (skip-whitespace)) () "Expected ~S" c)
	     (read-char stream))
	   (read-a-token (predicate &optional (stream stream))
	     (skip-whitespace)
	     (let ((chars (make-array 0 :fill-pointer 0
				      :adjustable t
				      :element-type 'character)))
	       (loop
		 (let ((c (peek-char nil stream nil nil)))
		   (unless (and c (funcall predicate c))
		     (return (coerce chars 'simple-string)))
		   (vector-push-extend c chars))
		 (read-char stream))))
	   (skip-comment ()
	     (when (eql #\/ (skip-whitespace))
	       (read-char stream)	; /
	       (read-char stream)	; *
	       (loop
		 (peek-char #\* stream)
		 (read-char stream)
		 (when (eql #\/ (read-char stream))
		   (return)))))
	   (skip-trailing-crap ()
	     (loop
	       (case (skip-whitespace)
		 (#\, (read-char stream))
		 (#\/ (skip-comment))
		 (t (return)))))
	   (read-a-string ()
	     (read stream))
	   (skip-whitespace ()
	     (let (c)
	       (loop
		 (unless  (eql (setq c (peek-char t stream)) #\newline)
		   (return c))))))

    (let (width height ncolors pixels colors cpp)

      (assert (eql #\/ (skip-whitespace)) () "File must begin with a comment")

      (skip-comment)
      (assert (string= (read-a-token #'alpha-char-p) "static")
	  () "Expected static keyword")
      (assert (string= (read-a-token #'alpha-char-p) "char")
	  () "Expected char keyword" )
      (ensure-next-char #\*)

      (read-a-token #'(lambda (c) (or (alphanumericp c) (eql c #\_))))

      (ensure-next-char #\[)
      (ensure-next-char #\])
      (ensure-next-char #\=)
      (ensure-next-char #\{)

      (skip-comment)

      (let ((values (read-a-string)))
	(with-input-from-string (s values)
	  (setq width (read s)
		height (read s)
		ncolors (read s)
		cpp (read s))))

      (skip-trailing-crap)

      (let ((array (make-array (list height width))))

	(dotimes (i ncolors)
	  (let* ((string (prog1 (read-a-string) (skip-trailing-crap)))
		 (chars (subseq string 0 cpp))
		 (values nil))
	    (with-input-from-string (s string :start cpp)
	      (loop
		(let ((key (read s nil nil)))
		  (when (eq key nil) (return))
		  (assert (member key '(m s g4 g c) :test #'string-equal)
		      () "Expected either m, s, g4, g or . Got ~S" key)
		  (push (cons key
			      (case (peek-char t s)
				(#\#	; rgb
				 (read-char s)
				 (let* ((number (read-a-token #'(lambda (c) (digit-char-p c 16)) s))
					(color-string-length (length number)))
				   (assert (or (= color-string-length 12)
					       (= color-string-length 6)) ()
				     "Expected 6 or 12 character hex string. Got ~S" number)
				   (let ((comp-length (/ color-string-length 3)))
				     (flet ((get-integer (i)
					      (/ (parse-integer number
								:start (* i comp-length)
								:end (* (1+ i) comp-length)
								:radix 16)
						 (if (= comp-length 2) 255 65535))))
				       (make-rgb-color
					(get-integer 0)
					(get-integer 1)
					(get-integer 2))))))
				(#\%	; hsv
				 (read-char s)
				 (error "HSV color spec not implemented")
				 )
				(t	; color-name
				 (read s))))
			values))))
	    (assert values () "Expected  key,color for ~S" chars)
	    (push (cons chars values) colors)))


	(setq pixels (nreverse pixels))

	(dotimes (i height)
	  (let ((string (read-a-string)))
	    (skip-trailing-crap)
	    (dotimes (j width)
	      (setf (aref array i j)
		(position
		 (let ((index (* cpp j)))
		   (subseq string index (+ index cpp)))
		 colors
		 :key #'car
		 :test #'string=)))))

	(values array
		(mapcar #'(lambda (name-and-versions)
			    (let ((color (or (cdr (assoc "c" (cdr name-and-versions)
							 :test #'string-equal))
					     (cdr (car (cdr name-and-versions))))))
			      (etypecase color
				(color color)
				(symbol (cond ((string-equal color 'none)
					       +transparent-ink+)
					      (palette
					       (find-named-color (string color) palette))
					      (t color))))))
			colors))))))

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

(defmethod read-image-file ((format (eql :xpm3)) pathname palette)
  (if (streamp pathname)
      (read-xpm-v3-file pathname palette)
    (with-open-file (fstream pathname :direction :input)
      (read-xpm-v3-file fstream palette))))

(defun read-xbitmap-file (pathname &key (format :xbm) (port (find-port)))
  (declare (type (or pathname string stream) pathname))
  (assert (member format '(:xbm :xpm :xpm3)))	; give caller reasonable error msg
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

(defun icon-width  () (win:GetSystemMetrics win:SM_CXICON))
(defun icon-height () (win:GetSystemMetrics win:SM_CYICON))

(defun create-icon (pixmap texture-info mask-bitmap) ;; <7>
  (win:CreateIcon (hinst *acl-port*)
		  (icon-width)
		  (icon-height)
		  1 ;; planes (texture-info-bits-per-pixel texture-info)
		  (texture-info-bits-per-pixel texture-info)
		  mask-bitmap
		  pixmap))

(defun initialize-named-colors ()
  (let ((table clim-utils::*default-named-color-table*))
    (setf (gethash "white"   table) +white+)
    (setf (gethash "black"   table) +black+)
    (setf (gethash "red"     table) +red+)
    (setf (gethash "green"   table) +green+)
    (setf (gethash "blue"    table) +blue+)
    (setf (gethash "cyan"    table) +cyan+)
    (setf (gethash "yellow"  table) +yellow+)
    (setf (gethash "magenta" table) +magenta+)
    table))

(eval-when (load eval)
  (initialize-named-colors))

(defun clim-internals::kana-process-gesture (istream gesture type)
  (declare (ignore istream gesture type))
  (error "Not yet implemented for this platform."))
