;; -*- mode: common-lisp; package: tk-silica -*-
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
;; $fiHeader: xt-graphics.lisp,v 1.44 92/10/04 14:16:49 cer Exp Locker: cer $

(in-package :tk-silica)

(defclass fast-gcontext (tk::gcontext)
  ((last-medium-clip-mask :initform nil :fixed-index 1)
   (last-line-style :initform nil :fixed-index 2)))

(defmacro fast-gcontext-last-line-style (gcontext)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (excl::slot-value-using-class-name 'fast-gcontext ,gcontext
					'last-line-style)))
(defsetf fast-gcontext-last-line-style (gcontext) (value)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (setf (excl::slot-value-using-class-name 'fast-gcontext ,gcontext
					      'last-line-style)
       ,value)))

(defmacro fast-gcontext-last-medium-clip-mask (gcontext)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (excl::slot-value-using-class-name 'fast-gcontext ,gcontext
					'last-medium-clip-mask)))
(defsetf fast-gcontext-last-medium-clip-mask (gcontext) (value)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (setf (excl::slot-value-using-class-name 'fast-gcontext ,gcontext
					      'last-medium-clip-mask)
       ,value)))


(defclass xt-medium (basic-medium)
  ((foreground-gcontext :reader medium-foreground-gcontext :initform nil)
   (background-gcontext :reader medium-background-gcontext :initform nil)
   (flipping-gcontext :reader medium-flipping-gcontext :initform nil)
   (drawable :initform nil)
   (ink-table :initform (make-hash-table :test #'equal))
   (clip-mask :initform nil)		; A cache.
   (indirect-inks :initform nil)
   (tile-gcontext :initform nil)))


;;; Palette handling stuff

(defclass xt-palette (basic-palette)
  ((colormap :reader palette-colormap :initarg :colormap)
   (xcolor-cache :initform (make-hash-table) :reader palette-xcolor-cache)
   (named-color-cache :initform (make-hash-table :test #'equalp) 
		      :reader palette-named-color-cache)
   (white-pixel :initarg :white-pixel)
   (black-pixel :initarg :black-pixel)))

(defun make-xt-palette (port colormap)
  (let* ((display (port-display port))
	 (screen (tk::display-screen-number display)))
    (make-instance 'xt-palette
		   :port port
		   :colormap colormap
		   :mutable-p (and 
			       (member (port-visual-class port)
				       '(:gray-scale :pseudo-color :direct-color))
			       t)
		   :color-p (port-color-p port)
		   :white-pixel (x11:xwhitepixel display screen)
		   :black-pixel (x11:xblackpixel display screen))))

(defmethod make-palette ((port xt-port) &key)
  (make-xt-palette port (tk::create-colormap (port-display port))))

#+ignore
(defmethod (setf frame-manager-palette) :after 
	   ((palette xt-palette) (framem standard-frame-manager))
  (let ((colormap (palette-colormap palette)))
    (dolist (frame (frame-manager-frames framem))
      (let ((window (tk::widget-window 
		     (sheet-direct-mirror (frame-top-level-sheet
					   frame)))))
	(setf (tk::window-colormap window) colormap)))))

(defun get-xcolor (color palette)
  (let ((xcolor-cache (palette-xcolor-cache palette)))
    (or (gethash color xcolor-cache)
	(setf (gethash color xcolor-cache)
	  (multiple-value-bind (red green blue)
	      (color-rgb color)
	    (let* ((x #.(1- (ash 1 16))))
	      (make-instance 'tk::color
			     :red (truncate (* x red))
			     :green (truncate (* x green))
			     :blue (truncate (* x blue)))))))))

(defmethod find-named-color (name (palette xt-palette))
  (let ((named-color-cache (palette-named-color-cache palette)))
    (or (gethash name named-color-cache)
	(setf (gethash name named-color-cache)
	  (let ((xcolor (tk::lookup-color (palette-colormap palette) name))
		(x #.(1- (ash 1 16))))
	    (make-rgb-color (/ (x11:xcolor-red xcolor) x)
			    (/ (x11:xcolor-green xcolor) x)
			    (/ (x11:xcolor-blue xcolor) x)))))))

(defmethod update-palette-entry ((palette xt-palette) pixel color)
  (let ((xcolor (get-xcolor color palette)))
    (setf (x11:xcolor-pixel xcolor) pixel)
    (with-slots (colormap) palette
      (tk::store-color
       colormap
       xcolor))))

(defmethod update-palette-entries ((palette xt-palette) updates)
  (with-slots (colormap) palette
    (let* ((n (ash (length updates) -1))
	   (xcolors (tk::get-xcolor-array n))
	   (j 0))
      (dotimes (i n)
	(let ((pixel (aref updates j))
	      (color (aref updates (1+ j))))
	  (let ((xcolor (get-xcolor color palette)))
	    (declare (ignore ignore))
	    (setf (x11:xcolor-array-red xcolors i) (x11:xcolor-red xcolor)
		  (x11:xcolor-array-green xcolors i) (x11:xcolor-green xcolor)
		  (x11:xcolor-array-blue xcolors i) (x11:xcolor-blue xcolor)
		  (x11:xcolor-array-pixel xcolors i) pixel
		  (x11:xcolor-array-flags xcolors i)
		  #.(logior x11:dored x11:dogreen x11:doblue))))
	(incf j 2))
      (tk::store-colors colormap xcolors n))))

;;;

(defmethod medium-drawable ((medium xt-medium))
  (with-slots (drawable sheet) medium
    (or drawable
	(setf drawable (fetch-medium-drawable 
			sheet
			(sheet-mirror sheet))))))


;;
;; Clip mask is invalidated when:
;;   1. medium's sheet's device region changes
;;   2. medium's clipping region changes
;;   3. medium's sheet's device transformation changes
(defmethod medium-clip-mask ((medium xt-medium))
  (with-slots (sheet clip-mask) medium
    (or clip-mask
	(setf clip-mask
	  (let* ((dr (sheet-device-region sheet))
		 (mcr (medium-clipping-region medium)))
	    (unless (eq mcr +everywhere+)
	      (setq mcr (transform-region (sheet-device-transformation sheet) mcr))
	      (setq dr (region-intersection dr (bounding-rectangle mcr))))
	    (cond ((eq dr +everywhere+)
		   :none)
		  ((eq dr +nowhere+)
		   :nowhere)
		  (t
		   (with-bounding-rectangle* (left top right bottom) dr
		     (list (fix-coordinate left) (fix-coordinate top) 
			   (fix-coordinate (- right left))
			   (fix-coordinate (- bottom top)))))))))))

(defmethod (setf medium-clipping-region) :after (cr (medium xt-medium))
  (declare (ignore cr))
  (with-slots (clip-mask) medium
    (setf clip-mask nil)))

(defmethod invalidate-cached-regions :after ((medium xt-medium))
  (with-slots (clip-mask) medium (setf clip-mask nil)))

(defmethod invalidate-cached-transformations :after ((medium xt-medium))
  (with-slots (clip-mask) medium (setf clip-mask nil)))



(defmethod deallocate-medium :after (port (medium xt-medium))
  (declare (ignore port))
  (with-slots (drawable) medium
    (setf drawable nil)))

(defmethod fetch-medium-drawable (sheet (mirror tk::xt-root-class))
  (declare (ignore sheet))
  (tk::widget-window mirror nil))

(defmethod engraft-medium :after ((medium xt-medium) (port xt-port) sheet)
  (with-slots (foreground-gcontext background-gcontext
				   flipping-gcontext
				   indirect-inks
				   drawable tile-gcontext clip-mask)
      medium
    (let ((palette (medium-palette medium)))
      (with-slots (white-pixel black-pixel)
	  palette
	(setf indirect-inks nil)
	(setf clip-mask nil)
	(setf (medium-sheet medium) sheet)
	(when (and drawable
		   (not (eq (port-display port)
			    (tk::object-display drawable))))
	  (error "drawable and display do not match"))
	(let* ((display (port-display port))
	       (drawable (or drawable
			     (tk::display-root-window display))))
	  (unless foreground-gcontext
	    (setf foreground-gcontext (tk::make-instance 'fast-gcontext
							 :drawable drawable)))
	  (unless background-gcontext
	    (setf background-gcontext (tk::make-instance 'fast-gcontext
							 :drawable drawable)))
	  (unless flipping-gcontext
	    (setf flipping-gcontext
	      (tk::make-instance 'fast-gcontext 
				 :drawable drawable
				 :function boole-xor)))
	  (unless tile-gcontext
	    (setf tile-gcontext (make-instance 'fast-gcontext
					       :drawable drawable
					       :foreground black-pixel
					       :background white-pixel)))
	  (recompute-gcs medium))))))



(defmethod degraft-medium :after ((medium xt-medium) (port xt-port) sheet)
  (declare (ignore sheet))
  (with-slots 
       (foreground-gcontext background-gcontext flipping-gcontext tile-gcontext
			    ink-table drawable)
      medium
    (setf drawable nil
	  (medium-sheet medium) nil)
    (macrolet ((loose-gc (gc)
		 `(when ,gc
		    (tk::free-gcontext ,gc)
		    (setf ,gc nil))))
      (maphash #'(lambda (ink gc) 
		   (declare (ignore ink))
		   (tk::free-gcontext gc))
	       ink-table)
      (clrhash ink-table)
      (loose-gc foreground-gcontext)
      (loose-gc background-gcontext)
      (loose-gc flipping-gcontext)
      (loose-gc tile-gcontext))))

(defun recompute-gcs (medium)
  (with-slots 
      (foreground-gcontext background-gcontext flipping-gcontext
			   ink-table indirect-inks)
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
	      (logxor foreground-pixel background-pixel))
	(dolist (ink indirect-inks)
	  (tk::free-gcontext (gethash ink ink-table))
	  (remhash ink ink-table))
	(setf indirect-inks nil)))))

(defmethod (setf medium-background) :after (ink (medium xt-medium))
  (declare (ignore ink))
  (recompute-gcs medium)
  ;;--- This should be a in silica method
  ;;--- This breaks the default-from-mirror-resource code when you
  ;;--- change the layout of a sheet
  ;;--- Perhaps that should just setf slot-value instead
  #+ignore
  (repaint-sheet (medium-sheet medium) medium +everywhere+))



(defmethod (setf medium-foreground) :after (ink (medium xt-medium))
  (declare (ignore ink))
  (recompute-gcs medium)
  )


(defmethod (setf medium-ink) :after (ink (medium xt-medium))
  (declare (ignore ink))
  (recompute-gcs medium))


;;; Colors and their monochrome imposters
;;; Much of this is taken from CLX-IMPLEMENTATION

(defvar *luminosity-stipples*
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
(defun decode-luminosity (luminosity stipple-p)
  (if (not stipple-p)
      (if (< luminosity 0.5) 1 0)	; Questionable.  XX
      (if (< luminosity 0.05)
	  1
	  (dolist (entry *luminosity-stipples* 0)
	    (let ((l (car entry))
		  (stipple (cdr entry)))
	      (when (< luminosity l)
		(return-from decode-luminosity stipple)))))))


;;; (indirect-ink-p ink) returns t if ink is an indirect ink or
;;; derived from an indirect ink

;;; not sure if these should be here 

(defgeneric indirect-ink-p (ink))

(defmethod indirect-ink-p (ink)
  (declare (ignore ink))
  nil)

(defmethod indirect-ink-p ((ink (eql +foreground-ink+)))
  t)

(defmethod indirect-ink-p ((ink (eql +background-ink+)))
  t)

(defmethod indirect-ink-p ((ink flipping-ink))
  (multiple-value-bind (ink1 ink2)
      (clim-utils:decode-flipping-ink ink)
    (or (indirect-ink-p ink1)
	(indirect-ink-p ink2))))
    
(defgeneric decode-ink (ink medium))

;;; the default decode-ink method. If it's in the table get gc from
;;; there otherwise make a gc using decode-color to get the foreground
;;; pixel value

;;--- This is really on color, mutable-color, and group-color.
;;--- Perhaps there should be a class like basic-color?

(defmethod decode-ink ((ink design) (medium xt-medium))
  (with-slots (ink-table sheet drawable) medium
    (or (gethash ink ink-table)
	(let* ((drawable (or drawable
			     (tk::display-root-window (port-display (port sheet)))))
	       (new-gc (make-instance 'fast-gcontext :drawable drawable)))
	  (multiple-value-bind (pixel mask)
	      (decode-color medium ink)
	    (setf (tk::gcontext-foreground new-gc) pixel)
	    (when mask
	      (setf (tk::gcontext-plane-mask new-gc) mask)))
	  (setf (gethash ink ink-table) new-gc)))))

(defmethod decode-ink ((ink (eql +everywhere+)) medium)
  (slot-value medium 'foreground-gcontext))

(defmethod decode-ink ((ink (eql +foreground-ink+)) medium)
  (slot-value medium 'foreground-gcontext))

(defmethod decode-ink ((ink (eql +background-ink+)) medium)
  (slot-value medium 'background-gcontext))

(defmethod decode-ink ((ink (eql +flipping-ink+)) stream)
  (slot-value stream 'flipping-gcontext))

(defmethod decode-ink ((ink flipping-ink) (medium xt-medium))
  (let ((palette (medium-palette medium)))
    (with-slots (color-p) palette
      (with-slots (ink-table sheet tile-gcontext drawable indirect-inks)
	  medium
	(or (gethash ink ink-table)
	    (let* ((drawable (or drawable
				 (tk::display-root-window (port-display (port sheet)))))
		   (new-gc (make-instance 'fast-gcontext 
					  :drawable drawable
					  :function boole-xor)))
	      (multiple-value-bind (color1 color2)
		  (clim-utils:decode-flipping-ink ink)
		(cond (color-p
		       (setf (tk::gcontext-foreground new-gc)
			 (logxor (decode-color medium color1)
				 (decode-color medium color2))))
		      ;;-- support gray-scale here
		      (t
		       ;; in a monochrome context there is only one
		       ;; flipping ink availiable ie white <-> black
		       (slot-value medium 'flipping-gcontext)))
		(when (indirect-ink-p ink)
		  (push ink indirect-inks))
		(setf (gethash ink ink-table) new-gc))))))))

		 
(defmethod decode-ink ((ink color) (medium xt-medium))
  (let ((palette (medium-palette medium)))
    (with-slots (white-pixel black-pixel color-p) palette
      (with-slots (ink-table sheet tile-gcontext drawable)
	  medium
	(or (gethash ink ink-table)
	    (let* ((drawable (or drawable
				 (tk::display-root-window (port-display (port sheet)))))
		   (new-gc (make-instance 'fast-gcontext :drawable drawable)))
	      (cond (color-p
		     (setf (tk::gcontext-foreground new-gc)
		       (decode-color medium ink)))
		    ;;-- support gray-scale here
		    (t
		     (multiple-value-bind (r g b) (color-rgb ink)
		       (let* ((luminosity (color-luminosity r g b))
			      (color (decode-luminosity luminosity t)))
			 (cond ((eq color 1)
				(setf (tk::gcontext-fill-style new-gc) :solid
				      (tk::gcontext-foreground new-gc) black-pixel))
			       ((eq color 0)
				(setf (tk::gcontext-fill-style new-gc) :solid
				      (tk::gcontext-foreground new-gc) white-pixel))
			       (t	; color is an image
				(setf (tk::gcontext-fill-style new-gc) :tiled)
				(let ((pixmap (make-instance 'tk::pixmap
							     :drawable drawable
							     :width (tk::image-width color)
							     :height (tk::image-height color)
							     :depth (tk::drawable-depth drawable))))
				  (tk::put-image pixmap tile-gcontext color)
				  (setf (tk::gcontext-tile new-gc) pixmap))))))))
	      (setf (gethash ink ink-table) new-gc)))))))


(defmethod decode-ink ((ink (eql +nowhere+)) medium)
  (decode-ink-opacity ink medium))

(defmethod decode-ink ((ink standard-opacity) medium)
  (decode-ink-opacity ink medium))

(defmethod decode-ink-opacity (opacity medium)
  (with-slots (ink-table sheet tile-gcontext drawable
			 foreground-gcontext ink-table)
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
		 (new-gc (make-instance 'fast-gcontext :drawable drawable)))
  
	    (x11:xcopygc display foreground-gcontext -1 new-gc)
	    (setf (tk::gcontext-stipple new-gc) (decode-opacity opacity port)
		  (tk::gcontext-fill-style new-gc) :stippled)
	    (setf (gethash key ink-table) new-gc))))))
  
(defmethod decode-ink ((ink contrasting-ink) medium)
  (let ((palette (medium-palette medium)))
    (with-slots (color-p) palette
      (decode-ink (if color-p
		      (make-color-for-contrasting-ink ink)
		    (make-gray-color-for-contrasting-ink ink))
		  medium))))

(defmethod decode-ink ((ink rectangular-tile) medium)
  (multiple-value-bind (pattern width height)
      (decode-rectangular-tile ink)
    (xt-decode-pattern pattern medium width height t)))

(defmethod decode-ink ((ink pattern) medium)
  (xt-decode-pattern ink medium))

(defmethod decode-color ((medium xt-medium) (design design))
  (error "Drawing with design: ~A not yet implemented" design))

(defmethod decode-color ((medium xt-medium) (x (eql +foreground-ink+)))
  (with-slots (foreground-gcontext) medium
    (tk::gcontext-foreground foreground-gcontext)))

(defmethod decode-color ((medium xt-medium) (x (eql +background-ink+)))
  (with-slots (background-gcontext) medium
    (tk::gcontext-foreground background-gcontext)))

(defmethod decode-color ((stream xt-medium) (ink standard-opacity))
  (if (> (opacity-value ink) 0.5)
      (decode-color stream +foreground-ink+)
      (decode-color stream +background-ink+)))

(defmethod decode-color ((medium xt-medium) (color color))
  (let* ((palette (medium-palette medium))
	 (color-cache (palette-color-cache palette)))
    (or (gethash color color-cache)
	(setf (gethash color color-cache)
	  (with-slots (color-p white-pixel black-pixel) palette
	    (cond (color-p
		   (tk::allocate-color
		    (palette-colormap palette) (get-xcolor color palette)))
		  ;;-- support gray-scale here
		  (t
		   (multiple-value-bind (r g b) (color-rgb color)
		     (let ((luminosity (color-luminosity r g b)))
		       (if (> luminosity .5) white-pixel black-pixel))))))))))

(defmethod decode-color ((medium xt-medium) (color mutable-color))
  (let* ((palette (medium-palette medium))
	 (mutable-color-cache (palette-mutable-color-cache palette)))
    (or (gethash color mutable-color-cache)
	(let ((pixel (aref (tk::alloc-color-cells
			     (palette-colormap palette) 1 0)
			   0)))
	  (update-palette-entry palette pixel (mutable-color-color color))
	  (push palette (mutable-color-palettes color))
	  (setf (gethash color mutable-color-cache) pixel)))))

(defmethod decode-color ((medium xt-medium) (color group-color))
  (let* ((palette (medium-palette medium))
	 (color-group-cache (palette-color-group-cache palette)))
    (values-list
     (or (gethash color color-group-cache)
	 (setf (gethash color color-group-cache)
	   (let* ((group (group-color-group color))
		  (pixel-planes (gethash group (palette-color-group-cache palette)))) 
	     (unless pixel-planes
	       (setq pixel-planes (decode-color-group group palette)))
	     (multiple-value-list 
		 (decode-group-color (group-color-layers color) pixel-planes))))))))

(defun decode-group-color (layers pixel-planes)
  (let ((pixel (car pixel-planes))
	(planes (cdr pixel-planes))
	(mask 0)) 
    (dolist (layer layers)
      (let ((plane-masks (pop planes)))
	(if layer
	    (dolist (plane-mask plane-masks)
	      (when (zerop layer)
		(return))
	      (when (oddp layer)
		(setq pixel (logior pixel plane-mask)))
	      (setq layer (ash layer -1)))
	  (dolist (plane-mask plane-masks)
	    (setq mask (logior mask plane-mask))))))
    (values pixel (lognot mask))))

(defun decode-color-group (group palette)
  (let ((color-group-cache (palette-color-group-cache palette))
	(mutable-color-cache (palette-mutable-color-cache palette))
	(layers (color-group-layers group))
	(mutable-array (color-group-mutable-array group))
	layer-nplanes
	(total-nplanes 0))
    (dolist (layer layers)
      (let ((nplanes (floor (log layer 2))))
	(push nplanes layer-nplanes)
	(incf total-nplanes nplanes)))
    (multiple-value-bind (pixels masks)
	(tk::alloc-color-cells
	 (palette-colormap palette) 1 total-nplanes)
      (let ((pixel (aref pixels 0))
	    (count 0)
	    (planes nil))
	(dolist (nplanes layer-nplanes)
	  (let ((plane-masks nil))
	    (dotimes (i nplanes)
	      (push (aref masks count) plane-masks)
	      (incf count))
	    (push plane-masks planes)))
	(let ((pixel-planes (cons pixel planes)))
	  (map-over-group-colors 
	   #'(lambda (dimensions)
	       (let ((mutable-color (apply #'aref mutable-array dimensions))
		     (pixel (decode-group-color dimensions
						pixel-planes)))
		 (setf (gethash mutable-color mutable-color-cache) pixel)
		 (update-palette-entry palette pixel (mutable-color-color mutable-color))
		 (push palette (mutable-color-palettes mutable-color))))
	   group)
	  (setf (gethash group color-group-cache) pixel-planes))))))
	    
(defmethod decode-color ((medium xt-medium) (ink contrasting-ink))
  (let ((palette (medium-palette medium)))
    (with-slots (color-p) palette
      (decode-color medium
		    (if color-p
			(make-color-for-contrasting-ink ink)
		      (make-gray-color-for-contrasting-ink ink))))))

(defvar *default-dashes* '(4 4))

(defun adjust-ink (gc medium line-style x-origin y-origin)
  (unless (eq (fast-gcontext-last-line-style gc) line-style)
    (let* ((dashes (line-style-dashes line-style))
	   (gc-line-style (if dashes :dash :solid)))
	
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
	 (setq thickness (the fixnum (round thickness)))
	 thickness)
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
	(setf (tk::gcontext-dashes gc)
	  (if (excl::sequencep dashes) dashes *default-dashes*))
	;; The following isn't really right, but it's better than nothing.
	(setf (tk::gcontext-dash-offset gc) (1- y-origin))))
    (setf (fast-gcontext-last-line-style gc) line-style))

  (let ((mcm (medium-clip-mask medium)))
    (unless (eq (fast-gcontext-last-medium-clip-mask gc) mcm)
      (setf (tk::gcontext-clip-mask gc) (medium-clip-mask medium))
      (setf (fast-gcontext-last-medium-clip-mask gc) mcm)))
    
  (when (member (tk::gcontext-fill-style gc) '(:tiled :stippled) :test #'eq)
    (setf (tk::gcontext-ts-x-origin gc) x-origin
	  (tk::gcontext-ts-y-origin gc) y-origin))
  gc)

#+ignore
(defmethod decode-ink :around ((ink t) (medium xt-medium))
  (let ((gc (call-next-method)))
    gc))

(defmethod xt-decode-pattern ((pattern pattern) medium &optional width height tiled-p)    
  (let ((ink-table (slot-value medium 'ink-table)))
    (or (gethash pattern ink-table)
	(let* ((drawable (or (slot-value medium 'drawable)
			     (tk::display-root-window
			      (port-display (port (medium-sheet medium))))))
	       (depth (tk::drawable-depth drawable)))
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
			(make-instance 'fast-gcontext :drawable drawable))
		       (pixmap 
			(make-instance 'tk::pixmap
			  :drawable drawable
			  :width pattern-width
			  :height pattern-height
			  :depth depth)))
		  (tk::put-image pixmap gc image)
		  (setf (tk::gcontext-tile gc) pixmap
			(tk::gcontext-fill-style gc) :tiled)
		  gc))))))))



;; Line (& maybe eventually non-rectangular polygon) clipping

;; We use 32000 instead of the more correct 32768 because some buggy X servers
;; barf on numbers very close to 32768.

(defmacro valid-point-p (x y)
  `(and (excl:fixnump ,x) (excl:fixnump ,y)
	(< (the fixnum ,x) 32000) (> (the fixnum ,x) -32000)
	(< (the fixnum ,y) 32000) (> (the fixnum ,y) -32000)))

;;
;; Here we implement a simple version of the Cohen-Sutherland clipping
;; algorithm.  Specifically, we only try to clip the line so that it will
;; be legal to pass to X, we don't try to clip to window boundaries.
;; In port-draw-line* we special case trivial accepts for speed.
;;
(defun clipper (x1 y1 x2 y2
		&optional (xmin -32000) (xmax 32000) (ymin -32000) (ymax 32000))
  (declare (optimize (speed 3) (safety 0))
	   #+ignore
	   (:explain :calls :types)
	   (fixnum x1 y1 x2 y2 xmin xmax ymin ymax))
  (macrolet ((outcodes (x y)
	       `(+ (ash (if (minusp (- ymax ,y)) 1 0) 3)
		   (ash (if (minusp (- ,y ymin)) 1 0) 2)
		   (ash (if (minusp (- xmax ,x)) 1 0) 1)
		   (if (minusp (- ,x xmin)) 1 0)))
	     (rejectp (oc1 oc2) `(plusp (logand ,oc1 ,oc2)))
	     (acceptp (oc1 oc2)
	       `(and (zerop ,oc1) (zerop ,oc2))))
    (let ((outcode1 0) (outcode2 0))
      (declare (type (unsigned-byte 4) outcode1 outcode2))
      (loop
	(setq outcode1 (outcodes x1 y1)
	      outcode2 (outcodes x2 y2))
	#+ignore
	(format excl:*initial-terminal-io* "oc1 = ~d, oc2 = ~d~%" outcode1 outcode2)
	(if (rejectp outcode1 outcode2)
	    (return-from clipper nil))
	(if* (acceptp outcode1 outcode2)
	   then (return)
	   else ;; If P1 is inside window, exchange P1 and P2 to guarantee
		;; that P1 is outside window.
		(if (zerop outcode1)
		    (psetq x1 x2
			   y1 y2
			   x2 x1
			   y2 y1
			   outcode1 outcode2
			   outcode2 outcode1))
		;; Now perform a subdivision; move P1 to the intersection point.
		;; Use the formulas y=y1+slope*(x-x1), x=x1+(1/slope)*(y-y1).
		(if* (logbitp 3 outcode1)
		   then ;; Divide line at top of window.
			(setq x1 (round (+ x1 (* (- x2 x1)
						 (/ (- ymax y1) (- y2 y1))))))
			(setq y1 ymax)
		 elseif (logbitp 2 outcode1)
		   then ;; Divide line at bottom of window.
			(setq x1 (round (+ x1 (* (- x2 x1)
						 (/ (- ymin y1) (- y2 y1))))))
			(setq y1 ymin)
		 elseif (logbitp 1 outcode1)
		   then ;; Divide line at right of window.
			(setq x1 xmax)
			(setq y1 (round (+ y1 (* (- y2 y1)
						 (/ (- xmax x1) (- x2 x1))))))
		 elseif (logbitp 0 outcode1)
		   then ;; Divide line at left of window.
			(setq x1 xmin)
			(setq y1 (round (+ y1 (* (- y2 y1)
						 (/ (- xmin x1) (- x2 x1)))))))
		))))
  (values x1 y1 x2 y2))

(defmacro clip-invalidate-line (x1 y1 x2 y2)
  `(if (or (not (valid-point-p ,x1 ,y1))
	   (not (valid-point-p ,x2 ,y2)))
       (multiple-value-setq (,x1 ,y1 ,x2 ,y2)
	 (clipper ,x1 ,y1 ,x2 ,y2))))

;; Discarding of other illegal graphics

#|

(defmacro int16ify (int &optional check-type-string)
  "Make int (a fixnum) into an xlib:int16; int must be a
   place.  Call check-type if it isn't in bounds."
  ;; These are ordered in expected frequency of usage
  `(if* (and (>= ,int -32768) (< ,int 32768))
      then
	   nil
      else
	   (setf ,int (make-into-int16 ,int ,check-type-string))))

(defmacro card16ify (int &optional check-type-string)
  "Make int (a fixnum) into an xlib:card16; int must be a
   place.  Call check-type if it isn't in bounds."
  ;; These are ordered in expected frequency of usage
  `(if* (and (>= ,int 0) (< ,int 65536))
      then
	   nil
      else
	   (setf ,int (make-into-card16 ,int ,check-type-string))))

(defun make-into-int16 (object string)
  ;; Now it must be too big to fit in an xlib:int16.
  (cond (cw::*fix-up-illegal-graphics-p*
	 (max -32767 (min 32766 object)))
	(t
	 (check-type object xlib:int16 string)
	 object)))

(defun make-into-card16 (object string)
  ;; Now it must be too big to fit in an xlib:int16.
  (cond (cw::*fix-up-illegal-graphics-p*
	 (max 0 (min 65534 object)))
	(t
	 (check-type object xlib:card16 string)
	 object)))

|#

(defvar *discard-illegal-graphics* t)

(defmacro discard-illegal-coordinates (function-name &rest positions)
  (assert (evenp (length positions)) (positions)
    "There must be an even number of elements in ~S" positions)
  (let ((forms nil))
    (do ()
	((null positions))
      (let* ((x (pop positions))
	     (y (pop positions)))
	(push `(valid-point-p ,x ,y) forms)))
    `(unless (and ,@forms)
       (if* *discard-illegal-graphics*
	  then (return-from ,function-name)
	  else (error "Coordinate(s) out of (signed-byte 16) range")))))


 
(defmethod medium-draw-point* ((medium xt-medium) x y)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((ink (medium-ink medium))
	     (line-style (medium-line-style medium))
	     (thickness (line-style-thickness line-style))
	     (sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(convert-to-device-coordinates transform x y)
	(discard-illegal-coordinates medium-draw-point* x y)
	(cond ((< thickness 1.5)
	       (tk::draw-point
		drawable
		(adjust-ink (decode-ink ink medium)
			    medium
			    line-style
			    x y)
		x y))
	      (t
	       (setq thickness (round thickness))
	       (tk::draw-ellipse
		drawable
		(adjust-ink (decode-ink ink medium)
			    medium
			    line-style
			    (the fixnum (- (the fixnum x) (the fixnum thickness)))
			    (the fixnum (- (the fixnum y) (the fixnum thickness))))
		x y 
		0
		thickness 
		0
		thickness 0 2pi
		t)))))))

(defmethod medium-draw-points* ((medium xt-medium) position-seq)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((ink (medium-ink medium))
	     (line-style (medium-line-style medium))
	     (sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
       ))))

(defmethod medium-draw-line* ((medium xt-medium) x1 y1 x2 y2)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((ink (medium-ink medium))
	     (sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(convert-to-device-coordinates transform x1 y1 x2 y2)
	(clip-invalidate-line x1 y1 x2 y2)
	(tk::draw-line
	 drawable
	 (adjust-ink (decode-ink ink medium)
		     medium
		     (medium-line-style medium)
		     (the fixnum (min (the fixnum x1) (the fixnum x2)))
		     (the fixnum (min (the fixnum y1) (the fixnum y2))))
	 x1 y1 x2 y2)))))


(defmethod medium-draw-lines* ((medium xt-medium) position-seq)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet))
	     (len (length position-seq))
	     (npoints (/ len 4))
	     (points (tk::make-xsegment-array :number npoints))
	     ;; These really are fixnums, since we're fixing coordinates below
	     (minx most-positive-fixnum)
	     (miny most-positive-fixnum)
	     ink)
	(macrolet ((stuff-line-guts (x1 y1 x2 y2)
		     `(let ((x1 ,x1)
			    (y1 ,y1)
			    (x2 ,x2)
			    (y2 ,y2))
			(convert-to-device-coordinates transform x1 y1)
			(convert-to-device-coordinates transform x2 y2)
			(clip-invalidate-line x1 y1 x2 y2)
			(minf minx x1)
			(minf miny y1)
			(minf minx x2)
			(minf miny y2)
			(setf (tk::xsegment-array-x1 points i) x1
			      (tk::xsegment-array-y1 points i) y1
			      (tk::xsegment-array-x2 points i) x2
			      (tk::xsegment-array-y2 points i) y2))))
	  (if (listp position-seq)
	      (do ((ps position-seq)
		   (i 0 (1+ i)))
		  ((null ps))
		(declare (fixnum i))
		(stuff-line-guts (pop ps) (pop ps) (pop ps) (pop ps)))
	    (do ((ps position-seq)
		 (j 0 (+ j 4))
		 (i 0 (1+ i)))
		((= j len))
	      (declare (fixnum i j))
	      (stuff-line-guts (aref ps j) (aref ps (1+ j))
			       (aref ps (+ 2 j)) (aref ps (+ 3 j)))))
	  (setq ink 
	    (adjust-ink (decode-ink (medium-ink medium) medium) medium
			(medium-line-style medium)
			minx miny))
	  (x11:xdrawsegments
	   (tk::object-display drawable)
	   drawable
	   ink
	   points
	   npoints))))))

(defmethod medium-draw-rectangle* ((medium xt-medium) x1 y1 x2 y2 filled)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((ink (medium-ink medium))
	     (sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(cond ((rectilinear-transformation-p transform)
	       (convert-to-device-coordinates transform
					      x1 y1 x2 y2)
	       ;; Clipping a rectangle is ridiculously easy.
	       (unless (valid-point-p x1 y1)
		 (setq x1 (min (max -32000 (the fixnum x1)) 32000))
		 (setq y1 (min (max -32000 (the fixnum y1)) 32000)))
	       (unless (valid-point-p x2 y2)
		 (setq x2 (min (max -32000 (the fixnum x2)) 32000))
		 (setq y2 (min (max -32000 (the fixnum y2)) 32000)))
	       (let ((min-x (min (the fixnum x1) (the fixnum x2)))
		     (min-y (min (the fixnum y1) (the fixnum y2))))
		 (declare (fixnum min-x min-y))
		 (tk::draw-rectangle
		  drawable
		  (adjust-ink (decode-ink ink medium)
			      medium
			      (medium-line-style medium)
			      min-x min-y)
		  min-x min-y
		  (fast-abs (the fixnum (- (the fixnum x2) (the fixnum x1))))
		  (fast-abs (the fixnum (- (the fixnum y2) (the fixnum y1))))
		  filled)))
	      (t
	       (port-draw-transformed-rectangle*
		(port sheet) sheet medium x1 y1 x2 y2 filled)))))))

(defmethod medium-draw-polygon* ((medium xt-medium) position-seq closed filled)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet))
	     (len (length position-seq))
	     (npoints (/ len 2))
	     (points (xt::make-xpoint-array :number (cond ((and closed (not filled))
							   (incf npoints))
							  (t npoints))))
	     ;; These really are fixnums, since we're fixing coordinates below
	     (minx most-positive-fixnum)
	     (miny most-positive-fixnum)
	     ink)
	(if (listp position-seq)
	    (do ((ps position-seq (cddr ps))
		 (i 0 (1+ i)))
		((null ps))
	      (let ((x (first ps))
		    (y (second ps)))
		(convert-to-device-coordinates transform x y)
		(discard-illegal-coordinates medium-draw-polygon* x y)
		(minf minx x)
		(minf miny y)
		(setf (tk::xpoint-array-x points i) x
		      (tk::xpoint-array-y points i) y)))
	  (do ((ps position-seq)
	       (j 0 (+ 2 j))
	       (i 0 (1+ i)))
	      ((= j len))
	    (let ((x (aref ps j))
		  (y (aref ps (1+ j))))
	      (convert-to-device-coordinates transform x y)
	      (discard-illegal-coordinates medium-draw-polygon* x y)
	      (minf minx x)
	      (minf miny y)
	      (setf (tk::xpoint-array-x points i) x
		    (tk::xpoint-array-y points i) y))))
	(when (and closed (not filled))
	  (setf (tk::xpoint-array-x points (- npoints 1)) (tk::xpoint-array-x points 0)
		(tk::xpoint-array-y points (- npoints 1)) (tk::xpoint-array-y points 0)))
	(setq ink 
	  (adjust-ink (decode-ink (medium-ink medium) medium) medium
		      (medium-line-style medium)
		      minx miny))
	(if filled
	    (x11:xfillpolygon
	     (tk::object-display drawable)
	     drawable
	     ink
	     points
	     npoints
	     x11:complex
	     x11:coordmodeorigin)
	  (x11:xdrawlines
	    (tk::object-display drawable)
	    drawable
	    ink
	    points
	    npoints
	    x11:coordmodeorigin))))))

(defmethod medium-draw-ellipse* ((medium xt-medium)
				 center-x center-y
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy 
				 start-angle end-angle filled)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(convert-to-device-coordinates transform center-x center-y)
	(discard-illegal-coordinates medium-draw-ellipse* center-x center-y)
	(convert-to-device-distances transform 
	  radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
	;;--- This is some magic that means that things get drawn
	;;--- correctly.
	(setq start-angle (- 2pi start-angle)
	      end-angle (- 2pi end-angle))
	(rotatef start-angle end-angle)
	(when (< end-angle start-angle)
	  (setq end-angle (+ end-angle 2pi)))
	(tk::draw-ellipse
	  drawable
	  (adjust-ink (decode-ink (medium-ink medium) medium)
		      medium
		      (medium-line-style medium)
		      (- center-x 
			 (if (zerop radius-1-dx) radius-2-dx radius-1-dx))
		      (- center-y 
			 (if (zerop radius-1-dy) radius-2-dy radius-1-dy)))
	  center-x center-y
	  radius-1-dx radius-1-dy radius-2-dx radius-2-dy 
	  start-angle end-angle 
	  filled)))))

(defmethod medium-draw-text* ((medium xt-medium)
			      string-or-char x y start end
			      align-x align-y
			      towards-x towards-y transform-glyphs)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((port (port medium))
	     (sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet))
	     (font (text-style-mapping port (medium-text-style medium)))
	     (ascent (tk::font-ascent font)))
	(convert-to-device-coordinates transform x y)
	(discard-illegal-coordinates medium-draw-text* x y)
	(when towards-x
	  (convert-to-device-coordinates transform towards-x towards-y))
	(when (typep string-or-char 'character)
	  (setq string-or-char (string string-or-char)))
	(ecase align-x
	  (:right
	   (let ((dx (text-size sheet string-or-char
				:text-style (medium-text-style medium)
				:start start :end end)))
	     (when towards-x (decf towards-x dx))
	     (decf x dx)))
	  (:center 
	   (let ((dx (floor (text-size sheet string-or-char
				       :text-style (medium-text-style medium)
				       :start start :end end) 2)))
	     (when towards-x (decf towards-x dx))
	     (decf x dx)))
	  (:left nil))
	(ecase align-y
	  (:bottom
	   (let ((dy (text-style-descent (medium-text-style medium) medium)))
	     (decf y dy)
	     (when towards-y (decf towards-y dy))))
	  (:center 
	   (let ((dy (- (text-style-descent (medium-text-style medium) medium)
		      (floor (text-style-height (medium-text-style medium) medium) 2))))
	     (decf y dy)
	     (when towards-y (decf towards-y dy))))
	  (:baseline nil)
	  (:top
	   (when towards-y (incf towards-y ascent))
	   (incf y ascent))) 
        (let ((gc (adjust-ink
		   (decode-ink (medium-ink medium) medium)
		   medium
		   (medium-line-style medium)
		   x 
		   (- y ascent))))
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
	     string-or-char start end)))))))

(defmethod medium-text-bounding-box ((medium xt-medium)
				     string x y start end align-x
				     align-y text-style
				     towards-x towards-y
				     transform-glyphs transformation)
  (declare (ignore string start end transform-glyphs transformation))
  (multiple-value-bind
      (left top right bottom) (call-next-method)
    (if (and towards-y towards-x)
	(flet ((compute-rotation (x y towards-x towards-y)
		 (decf towards-x x)
		 (decf towards-y y)
		 (mod (round (atan towards-y towards-x) (/ pi 2.0))
		      4)))
	  (let ((ascent (text-style-ascent text-style medium)))
	    (ecase align-y
	      (:top (incf y ascent)
		    (incf towards-y ascent))
	      (:baseline))
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
	  (setf (fast-gcontext-last-medium-clip-mask gcontext) nil)
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
		   (gc (make-instance 'fast-gcontext
				      :drawable pixmap
				      :font font
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
	 (gc (make-instance 'fast-gcontext :drawable source-pixmap)))
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


(defmethod text-style-width ((text-style standard-text-style) (medium xt-medium))
  (tk::font-width (text-style-mapping (port medium) text-style)))

(defmethod text-style-height ((text-style standard-text-style) (medium xt-medium))
  ;; Optimization
  (tk::font-height (text-style-mapping (port medium) text-style)))

(defmethod text-style-ascent ((text-style standard-text-style) (medium xt-medium))
  (tk::font-ascent (text-style-mapping (port medium) text-style)))
					
(defmethod text-style-descent ((text-style standard-text-style) (medium xt-medium))
  (tk::font-descent (text-style-mapping (port medium) text-style)))
					

(defmethod medium-beep ((medium xt-medium))
  (x11:xbell (port-display (port medium)) 100))

(defmethod medium-force-output ((medium xt-medium))
  (x11:xflush (port-display (port medium))))

(defmethod medium-finish-output ((medium xt-medium))
  (x11:xsync (port-display (port medium)) 0))

