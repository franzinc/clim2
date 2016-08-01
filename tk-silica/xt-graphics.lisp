;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk-silica)

(defclass ink-gcontext (tk::gcontext)
  ((last-clip-region-tick :initform nil excl::fixed-index 2)
   (last-line-style :initform nil excl::fixed-index 3)
   (shift-tile-origin :initform nil excl::fixed-index 4)
   (ink-clip-region :initform nil excl::fixed-index 5)))

(defmacro ink-gcontext-last-clip-region-tick (gcontext)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (excl::slot-value-using-class-name 'ink-gcontext ,gcontext
					'last-clip-region-tick)))
(defsetf ink-gcontext-last-clip-region-tick (gcontext) (value)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (setf (excl::slot-value-using-class-name 'ink-gcontext ,gcontext
					      'last-clip-region-tick)
       ,value)))

(defmacro ink-gcontext-last-line-style (gcontext)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (excl::slot-value-using-class-name 'ink-gcontext ,gcontext
					'last-line-style)))
(defsetf ink-gcontext-last-line-style (gcontext) (value)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (setf (excl::slot-value-using-class-name 'ink-gcontext ,gcontext
					      'last-line-style)
       ,value)))

(defmacro ink-gcontext-shift-tile-origin (gcontext)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (excl::slot-value-using-class-name 'ink-gcontext ,gcontext
					'shift-tile-origin)))
(defsetf ink-gcontext-shift-tile-origin (gcontext) (value)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (setf (excl::slot-value-using-class-name 'ink-gcontext ,gcontext
					      'shift-tile-origin)
       ,value)))

(defmacro ink-gcontext-ink-clip-region (gcontext)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (excl::slot-value-using-class-name 'ink-gcontext ,gcontext
					'ink-clip-region)))
(defsetf ink-gcontext-ink-clip-region (gcontext) (value)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (setf (excl::slot-value-using-class-name 'ink-gcontext ,gcontext
					      'ink-clip-region)
       ,value)))

(defclass xt-medium (basic-medium)
  ((drawable :initform nil)
   (palette :initform nil)
   ;; alist of (palette . ink-table) for non-current palettes
   (ink-tables :initform nil)
   (ink-table :initform nil)
   (indirect-inks :initform nil)
   (device-clip-region :initform nil)
   (device-clip-region-tick :initform 0)))

(defmethod make-medium ((port xt-port) sheet)
  (make-instance 'xt-medium
		 :port port
		 :sheet sheet))

(defmethod medium-drawable ((medium xt-medium))
  (with-slots (drawable sheet) medium
    (or drawable
	(setf drawable (fetch-medium-drawable
			sheet
			(sheet-mirror sheet))))))

(defmethod medium-palette ((medium xt-medium))
  (with-slots (palette sheet) medium
    (or palette
	(setf palette
	  (let ((frame (pane-frame sheet)))
	    (if frame
		(frame-palette frame)
	      (port-default-palette (port medium))))))))



;;; Palette handling

(defclass xt-palette (basic-palette)
  ((colormap :reader palette-colormap :initarg :colormap)
   (xcolor-cache :initform (make-hash-table) :reader palette-xcolor-cache)
   (device-color-cache :initform (make-hash-table)
		       :reader palette-device-color-cache)
   (named-color-cache :initform (make-hash-table :test #'equalp)
		      :reader palette-named-color-cache)
   (white-pixel :initarg :white-pixel)
   (black-pixel :initarg :black-pixel)))


(defmethod make-palette ((port xt-port) &key colormap)
  (let* ((display (port-display port))
	 (screen (tk::display-screen-number display)))
    (make-instance 'xt-palette
      :port port
      :colormap (or colormap
		    (tk::create-colormap (port-display port)))
      :dynamic-p (and
		  (member (port-visual-class port)
			  '(:gray-scale :pseudo-color :direct-color))
		  t)
      :color-p (port-color-p port)
      :white-pixel (x11:xwhitepixel display screen)
      :black-pixel (x11:xblackpixel display screen))))

(defmethod update-medium-ink-table ((medium xt-medium))
  (let ((palette (medium-palette medium)))
    (with-slots (ink-table ink-tables) medium
      (when ink-table
	(invalidate-indirect-inks medium))
      (setf ink-table
	(or (cdr (assoc palette ink-tables))
	    (let ((new-ink-table (make-hash-table :test #'equal)))
	      (push (cons palette new-ink-table) ink-tables)
	      new-ink-table))))))

(defmethod (setf frame-manager-palette) :after
	   ((palette xt-palette) (framem standard-frame-manager))
  (let ((colormap (palette-colormap palette)))
    (dolist (frame (frame-manager-frames framem))
      (tk::set-values (frame-shell frame) :colormap colormap)
      (map-over-sheets
       #'(lambda (sheet)
	   (let ((medium (and (typep sheet
				     'silica::sheet-with-medium-mixin)
			      (sheet-medium sheet))))
	     (when medium
	       ;; invalidate the medium's palette cache
	       (setf (slot-value medium 'palette) nil)
	       (update-medium-ink-table medium)
	       (repaint-sheet sheet +everywhere+))))
       (frame-top-level-sheet frame)))))

(defun get-xcolor (color palette)
  (let ((xcolor-cache (palette-xcolor-cache palette)))
    (or (gethash color xcolor-cache)
	(setf (gethash color xcolor-cache)
	  (multiple-value-bind (red green blue)
	      (color-rgb color)
	    (let* ((x #.(1- (ash 1 16))))
	      (make-instance 'tk::color
		:in-foreign-space t
		:red (truncate (* x red))
		:green (truncate (* x green))
		:blue (truncate (* x blue)))))))))

(defmethod find-named-color (name (palette xt-palette) &key (errorp t))
  (let ((named-color-cache (palette-named-color-cache palette)))
    ;;-- What is the correct thing to do?
    ;;-- CLIM has names containing dashes instead of spaces
    (setq name (substitute #\space #\- (string name)))
    (or (gethash name named-color-cache)
	(setf (gethash name named-color-cache)

	  ;; put this back from lookup-color to parse-color because
	  ;; openlook version still uses R4 (cim) 1/13/94

	  (let ((xcolor (tk::parse-color (palette-colormap palette) name))
		(x #.(1- (ash 1 16))))
	    (if xcolor
		(make-rgb-color (/ (x11:xcolor-red xcolor) x)
				(/ (x11:xcolor-green xcolor) x)
				(/ (x11:xcolor-blue xcolor) x))
	      (if errorp
		  (error 'color-not-found :color name)
		(return-from find-named-color nil))))))))

(defmethod update-palette-entry ((palette xt-palette) pixel color)
  (let ((xcolor (get-xcolor color palette)))
    (setf (x11:xcolor-pixel xcolor) pixel)
    (with-slots (colormap) palette
      (tk::store-color
       colormap
       xcolor))))

(defmethod update-palette-entries ((palette xt-palette) updates)
  (with-slots (colormap) palette
    (let ((n (ash (length updates) -1))
	  (j 0))
      (tk::with-xcolor-array (xcolors n)
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
	(tk::store-colors colormap xcolors n)))))

;;; things we can install in a palette
;;; 1 - color (rgb etc)
;;; 2 - dynamic-color
;;; 3 - layered-color or it's layered-color-set

(defmethod allocate-color ((color color) (palette xt-palette))
  (unless (gethash color (palette-color-cache palette))
    (decode-color-in-palette color palette)))

(defmethod allocate-color ((color dynamic-color) (palette xt-palette))
  (unless (gethash color (palette-dynamic-color-cache palette))
    (decode-color-in-palette color palette)))

(defmethod allocate-color ((set layered-color-set) (palette xt-palette))
  (unless (gethash set (palette-layered-color-cache palette))
    (decode-layered-color-set set palette)))

(defmethod allocate-color ((color layered-color) (palette xt-palette))
  (allocate-color (layered-color-set color) palette))

(defmethod deallocate-color ((color color) (palette xt-palette))
  (let* ((color-cache (palette-color-cache palette))
	 (pixel (gethash color color-cache)))
    (when pixel
      (tk::free-color-cells (palette-colormap palette) pixel 0)
      (remhash color color-cache))))

(defmethod deallocate-color ((color dynamic-color) (palette xt-palette))
  (let* ((dynamic-color-cache (palette-dynamic-color-cache palette))
	 (pixel (gethash color dynamic-color-cache)))
    (when pixel
      (tk::free-color-cells (palette-colormap palette) pixel 0)
      (remhash color dynamic-color-cache))))

(defmethod deallocate-color ((color layered-color) (palette xt-palette))
  (deallocate-color (layered-color-set color) palette))

;;--- maybe some more of this could be made more port independent
(defmethod deallocate-color ((set layered-color-set) (palette xt-palette))
  (let* ((layered-color-cache (palette-layered-color-cache palette))
	 (pixel-planes (gethash set layered-color-cache)))
    (when pixel-planes
      (let ((dynamic-color-cache (palette-dynamic-color-cache palette))
	    (dynamic-array (layered-color-set-dynamic-array set))
	    (planes 0))
	(dolist (plane-masks (cdr pixel-planes))
	  (dolist (plane-mask plane-masks)
	    (setf planes (logior planes plane-mask))))
	(tk::free-color-cells (palette-colormap palette)
			      (car pixel-planes)
			      planes)
	(map-over-layered-colors
	 #'(lambda (dimensions)
	     (let ((dynamic-color (apply #'aref dynamic-array dimensions)))
	       (remhash dynamic-color dynamic-color-cache)
	       (setf (dynamic-color-palettes dynamic-color)
		 (delete palette (dynamic-color-palettes dynamic-color)))))
	 set)
	(maphash #'(lambda (layered-color pixel-mask)
		     (declare (ignore pixel-mask))
		     (when (and (typep layered-color 'layered-color)
				(eq (layered-color-set layered-color) set))
		       (remhash layered-color layered-color-cache)))
		 layered-color-cache)
	(remhash set layered-color-cache)))))

;;; Device colors

(defclass xt-device-color (device-color)
  ((color-cache :initform nil :accessor device-color-color-cache)))

(defmethod make-device-color ((palette xt-palette) pixel)
  (let ((device-color-cache (palette-device-color-cache palette)))
    (or (gethash pixel device-color-cache)
	(setf (gethash pixel device-color-cache)
	  (make-instance 'xt-device-color
	    :palette palette
	    :pixel pixel)))))

(defmethod device-color-color ((color xt-device-color))
  (let ((pixel (device-color-pixel color))
	(palette (device-color-palette color))
	(cache (device-color-color-cache color)))
    (multiple-value-bind (r g b)
	(tk::query-color (palette-colormap palette) pixel)
      (let* ((x #.(1- (ash 1 16)))
	     (red (/ r x))
	     (green (/ g x))
	     (blue (/ b x)))
	(if (and cache
		 (multiple-value-bind (cached-red cached-green cached-blue)
		     (color-rgb cache)
		   (and (= red cached-red)
			(= green cached-green)
			(= blue cached-blue))))
	    cache
	  (setf (device-color-color-cache color)
	    (make-rgb-color red green blue)))))))


;;
;; medium's device clip region is invalidated when:
;;   1. medium's sheet's device region changes
;;   2. medium's clipping region changes
;;   3. medium's sheet's device transformation changes

(defmethod medium-device-clip-region ((medium xt-medium))
  (let ((sheet (medium-sheet medium))
	(device-clip-region (slot-value medium 'device-clip-region))
	(device-clip-region-tick (slot-value medium 'device-clip-region-tick)))
    (if device-clip-region
	(values device-clip-region device-clip-region-tick)
      (values (setf (slot-value medium 'device-clip-region)
		(let ((dr (sheet-device-region sheet))
		      (mcr (medium-clipping-region medium)))
		  (region-intersection
		   dr
		   (transform-region (sheet-device-transformation sheet) mcr))))
	      (setf (slot-value medium 'device-clip-region-tick)
		(1+ device-clip-region-tick))))))

(defmethod (setf medium-clipping-region) :after (cr (medium xt-medium))
  (declare (ignore cr))
  (with-slots (device-clip-region) medium
    (setf device-clip-region nil)))

(defmethod invalidate-cached-regions :after ((medium xt-medium))
  (with-slots (device-clip-region) medium
    (setf device-clip-region nil)))

(defmethod invalidate-cached-transformations :after ((medium xt-medium))
  (with-slots (device-clip-region) medium
    (setf device-clip-region nil)))



(defmethod deallocate-medium :after (port (medium xt-medium))
  (declare (ignore port))
  (with-slots (drawable palette) medium
    (setf drawable nil palette nil)))

(defmethod fetch-medium-drawable (sheet (mirror tk::xt-root-class))
  (declare (ignore sheet))
  (tk::widget-window mirror nil))

(defmethod engraft-medium :after ((medium xt-medium) (port xt-port) sheet)
  (with-slots (drawable device-clip-region)
      medium
    (setf device-clip-region nil
	  (medium-sheet medium) sheet)
    (update-medium-ink-table medium)
    (when (and drawable
	       (not (eq (port-display port)
			(tk::object-display drawable))))
      (error "drawable and display do not match"))))

(defmethod degraft-medium :after ((medium xt-medium) (port xt-port) sheet)
  (declare (ignore sheet))
  (with-slots (ink-table ink-tables indirect-inks drawable palette)
      medium
    (setf drawable nil
	  palette nil
	  indirect-inks nil
	  (medium-sheet medium) nil)
    (dolist (entry ink-tables)
      (setf ink-table (cdr entry))
      (maphash #'(lambda (ink gc)
		     (declare (ignore gc))
		     (deallocate-ink ink medium))
	       ink-table)
      (clrhash ink-table))
    (setf ink-table nil)))

(defun invalidate-indirect-inks (medium)
  (with-slots (ink-table indirect-inks)
      medium
    (dolist (ink indirect-inks)
      (deallocate-ink ink medium)
      (remhash ink ink-table))
    (setf indirect-inks nil)))

(defmethod deallocate-ink ((ink design) (medium xt-medium))
  (with-slots (ink-table) medium
    (let ((gc (gethash ink ink-table)))
      (tk::free-gcontext gc))))

(defmethod (setf medium-background) :after (ink (medium xt-medium))
  (declare (ignore ink))
  (invalidate-indirect-inks medium)
  ;;--- This should be a in Silica method
  ;;--- This breaks the default-from-mirror-resource code when you
  ;;--- change the layout of a sheet
  ;;--- Perhaps that should just setf slot-value instead
  (repaint-sheet (medium-sheet medium) +everywhere+))

(defmethod (setf medium-foreground) :after (ink (medium xt-medium))
  (declare (ignore ink))
  (invalidate-indirect-inks medium)
  (repaint-sheet (medium-sheet medium) +everywhere+))


;;; Colors and their monochrome imposters
;;; Much of this is taken from CLX-IMPLEMENTATION

;;; (indirect-ink-p ink) returns t if ink is an indirect ink or
;;; derived from an indirect ink

;;;--- not sure if these should be here - they should probably be in designs

(defgeneric indirect-ink-p (ink))

(defmethod indirect-ink-p ((ink design)) nil)

(defmethod indirect-ink-p ((ink (eql +foreground-ink+))) t)
(defmethod indirect-ink-p ((ink (eql +background-ink+))) t)
(defmethod indirect-ink-p ((ink standard-opacity)) t)

(defmethod indirect-ink-p ((ink flipping-ink))
  (multiple-value-bind (ink1 ink2)
      (decode-flipping-ink ink)
    (or (indirect-ink-p ink1)
	(indirect-ink-p ink2))))

(defmethod indirect-ink-p ((ink pattern))
  (multiple-value-bind (array designs)
      (decode-pattern ink)
    (declare (ignore array))
    (some #'indirect-ink-p designs)))

(defmethod indirect-ink-p ((ink rectangular-tile))
  (indirect-ink-p (decode-rectangular-tile ink)))

;;; adjust ink

(defvar *default-dashes* '(4 4))

(defun adjust-ink (gc medium x-origin y-origin)
  (declare (optimize (speed 3) (safety 0)))
  ;; line style
  (let ((line-style (medium-line-style medium)))
    (unless (eq (ink-gcontext-last-line-style gc)
		line-style)
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
	   (if (< thickness 2)
	       0
	     (the fixnum (round thickness))))
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
      (setf (ink-gcontext-last-line-style gc) line-style)))

  ;; clip mask
  (let ((ink-clip-region (ink-gcontext-ink-clip-region gc)))
    (multiple-value-bind (device-clip-region device-clip-region-tick)
	(medium-device-clip-region medium)
      (when ink-clip-region
	(setq device-clip-region
	  (region-intersection (transform-region
				(make-translation-transformation x-origin y-origin)
				ink-clip-region)
			       device-clip-region)))
      (unless (and (eq (ink-gcontext-last-clip-region-tick gc)
		       device-clip-region-tick)
		   (null ink-clip-region))
	(setf (tk::gcontext-clip-mask gc)
	  (etypecase device-clip-region
	    (everywhere :none)
	    (nowhere nil)
	    (standard-rectangle-set device-clip-region)
	    (standard-bounding-rectangle device-clip-region)))
	(setf (ink-gcontext-last-clip-region-tick gc) device-clip-region-tick))))

  ;; tile and stipple origin
  (when (ink-gcontext-shift-tile-origin gc)
    (setf (tk::gcontext-ts-x-origin gc) x-origin
	  (tk::gcontext-ts-y-origin gc) y-origin))
  gc)


;;; decode ink

(defgeneric decode-ink (ink medium))

(defmethod decode-ink ((ink design) (medium xt-medium))
  (with-slots (ink-table indirect-inks) medium
    (or (gethash ink ink-table)
	(let ((gc (decode-ink-1 ink medium)))
	  (when (indirect-ink-p ink)
	    (push ink indirect-inks))
	  (setf (gethash ink ink-table) gc)
	  gc))))

;;; the default decode-ink-1 method. Make a gc using
;;; decode-color-in-palette to get the pixel value

;;--- This is really only makes sense for color, dynamic-color, and
;;--- layered-color. Perhaps there should be a class like basic-color?

(defmethod decode-ink-1 ((ink design) (medium xt-medium))
  (with-slots (sheet drawable) medium
    (let* ((drawable (or drawable
			 (tk::display-root-window (port-display (port sheet)))))
	   (new-gc (make-instance 'ink-gcontext :drawable drawable)))
      (multiple-value-bind (pixel mask)
	  (decode-color ink medium)
	(setf (tk::gcontext-foreground new-gc) pixel)
	(when mask
	  (setf (tk::gcontext-plane-mask new-gc) mask)))
      new-gc)))

#+ignore
(defmethod decode-ink-1 ((x (eql +foreground-ink+)) (medium xt-medium))
  (decode-ink-1 (medium-foreground medium) medium))

#+ignore
(defmethod decode-ink-1 ((x (eql +background-ink+)) (medium xt-medium))
  (decode-ink-1 (medium-background medium) medium))

(defmethod decode-ink-1 ((ink flipping-ink) (medium xt-medium))
  (with-slots (sheet drawable)
      medium
    (let* ((drawable (or drawable
			 (tk::display-root-window (port-display (port sheet)))))
	   (new-gc (make-instance 'ink-gcontext
				  :drawable drawable
				  :function boole-xor)))
      (multiple-value-bind (color1 color2)
	  (decode-flipping-ink ink)
	(setf (tk::gcontext-foreground new-gc)
	  (logxor (decode-color color1 medium)
		  (decode-color color2 medium))))
      new-gc)))

(defmethod decode-ink-1 ((ink color) (medium xt-medium))
  (let ((palette (medium-palette medium)))
    (with-slots (white-pixel black-pixel) palette
      (with-slots (sheet drawable) medium
	(let* ((port (port sheet))
	       (drawable
		(or drawable
		    (tk::display-root-window (port-display port))))
	       (new-gc (make-instance 'ink-gcontext :drawable drawable)))
	  (cond ((palette-color-p palette)
		 (setf (tk::gcontext-foreground new-gc)
		   (decode-color-in-palette ink palette)))
		;;-- support gray-scale here
		(t
		 (multiple-value-bind (r g b) (color-rgb ink)
		   (let ((luminosity (color-luminosity r g b)))
		     (cond
		      ((<= luminosity 0.05)
		       (setf (tk::gcontext-foreground new-gc) black-pixel))
		      ((< 0.95 luminosity)
		       (setf (tk::gcontext-foreground new-gc) white-pixel))
		      (t
		       (setf (tk::gcontext-fill-style new-gc) :opaque-stippled
			     (tk::gcontext-foreground new-gc) white-pixel
			     (tk::gcontext-background new-gc) black-pixel
			     (tk::gcontext-stipple new-gc)
			     (decode-stipple luminosity port))))))))
	  new-gc)))))

(defmethod decode-ink-1 ((ink (eql +nowhere+)) (medium xt-medium))
  (decode-ink-opacity ink medium))

(defmethod decode-ink-1 ((ink standard-opacity) (medium xt-medium))
  (decode-ink-opacity ink medium))

(defmethod decode-ink-1 ((ink composite-in) (medium xt-medium))
  (let ((color +foreground-ink+)
	(opacity nil)
	(pattern nil))
    (map nil
      #'(lambda (ink)
	  (typecase ink
	    (opacity (setq opacity ink))
	    (pattern (setq pattern ink))
	    (t (setq color ink))))
      (slot-value ink 'clim-utils::designs))
    (cond
     ;; compose-in of patterns we treat as stencils
     (pattern
      (multiple-value-bind (pixmap format pixels)
	  (pixmap-from-pattern pattern medium :bitmap)
	(declare (ignore format pixels))
	(let* ((drawable (or (slot-value medium 'drawable)
			     (tk::display-root-window
			      (port-display (port (medium-sheet medium))))))
	       (gc (make-instance 'ink-gcontext :drawable drawable)))
	  (setf (tk::gcontext-stipple gc) pixmap
		(tk::gcontext-fill-style gc) :stippled
		(tk::gcontext-foreground gc) (decode-color color medium)
		(tk::gcontext-background gc) 0)
	  gc)))
     (opacity
      (decode-ink-opacity opacity medium color))
     (t
      (decode-ink-1 color medium)))))

(defmethod decode-ink-opacity (opacity (medium xt-medium)
			       &optional (color +foreground-ink+))
  (with-slots (sheet drawable)
      medium
    (let* ((port (port sheet))
	   (display (port-display port))
	   (drawable (or drawable (tk::display-root-window display)))
	   (new-gc (make-instance 'ink-gcontext :drawable drawable)))
      (setf (tk::gcontext-foreground new-gc) (decode-color color medium)
	    (tk::gcontext-stipple new-gc) (decode-stipple (opacity-value opacity) port)
	    (tk::gcontext-fill-style new-gc) :stippled)
      new-gc)))

(defmethod decode-contrasting-ink ((ink contrasting-ink) (medium xt-medium))
  (if (palette-color-p (medium-palette medium))
      (make-color-for-contrasting-ink ink)
    (make-gray-color-for-contrasting-ink ink)))

(defmethod decode-ink-1 ((ink contrasting-ink) (medium xt-medium))
  (decode-ink-1 (decode-contrasting-ink ink medium) medium))

(defmethod decode-ink-1 ((pattern pattern) (medium xt-medium))
  (decode-pattern-ink pattern medium))

(defmethod decode-ink-1 ((ink rectangular-tile) (medium xt-medium))
  (multiple-value-bind (pattern width height)
      (decode-rectangular-tile ink)
    (declare (ignore width height))
    (decode-pattern-ink pattern medium t)))

(defmethod deallocate-ink :before ((ink pattern) (medium xt-medium))
  (deallocate-pattern-ink ink medium))

(defmethod deallocate-ink :before ((ink rectangular-tile) (medium xt-medium))
  (deallocate-pattern-ink ink medium))

(defun deallocate-pattern-ink (ink medium)
  (with-slots (ink-table) medium
    (let* ((gc (gethash ink ink-table))
	   (pixmap (or (tk::gcontext-stipple gc)
                       (tk::gcontext-tile gc))))
      (when pixmap
	(tk::destroy-pixmap pixmap)))))

(defun get-simple-vector-reader (vector)
  (declare (optimize (safety 0) (speed 3)))
  (typecase vector
    ((simple-array (unsigned-byte 1) (*))
     #'(lambda (vector index)
	 (aref (the (simple-array (unsigned-byte 1) (*)) vector) index)))
    ((simple-array (unsigned-byte 8) (*))
     #'(lambda (vector index)
	 (aref (the (simple-array (unsigned-byte 8) (*)) vector) index)))
    ((simple-array t (*))
     #'(lambda (vector index)
	 (aref (the (simple-array t (*)) vector) index)))
    (t #'aref)))

(defun pixmap-from-pattern (pattern medium &optional format)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (array designs)
      (decode-pattern pattern)
    (declare (simple-vector designs))
    (let* ((n-designs (length designs))
	   (height (array-dimension array 0))
	   (width (array-dimension array 1))
	   (pixels (make-array n-designs)))
      (declare (simple-vector pixels))

      ;; we only use the bitmap format (a) when we have to ie for
      ;; transparent designs or (b) when we're using background and
      ;; foreground inks - this is for creating 1 bit deep icon
      ;; bitmaps
      (unless format
	(setq format (if (and (eql n-designs 2)
			      (or (find +nowhere+ designs)
				  (and (find +background-ink+ designs)
				       (find +foreground-ink+ designs))))
			 :bitmap
		       :pixmap)))

      (case format
	(:bitmap
	 (unless (eql n-designs 2)
	   (error "Can't make a bitmap from pattern with more than two designs")))
	(:pixmap
	 (when (find +nowhere+ designs)
	   (error "Can't make a pixmap from a transparent pattern"))))

      (dotimes (n n-designs)
	(declare (fixnum n))
	(let ((design (elt designs n)))
	  (setf (svref pixels n)
	    (and (not (eql design +nowhere+))
		 (decode-color design medium)))))

      (let* ((port (port (medium-sheet medium)))
	     (display (port-display port))
	     (drawable (or (slot-value medium 'drawable)
			   (tk::display-root-window display)))
	     (depth (if (eq format :bitmap)
			1
		      (tk::drawable-depth drawable)))
	     (image (make-instance 'tk::image
		      :display display
		      :width width
		      :height height
		      :depth depth))
	     (pixmap (make-instance 'tk::pixmap
		       :drawable drawable
		       :width width
		       :height height
		       :depth depth)))

	(if (eq format :bitmap)
	    (let ((bg (and (eq format :bitmap)
			   (or (position +nowhere+ designs)
			       (position +background-ink+ designs)
			       0))))
	      (excl::with-underlying-simple-vector (array vector displacement)
		(let ((reader (get-simple-vector-reader vector))
		      (i displacement))
		  (dotimes (h height)
		    (declare (fixnum h))
		    (dotimes (w width)
		      (declare (fixnum w))
		      (x11:xputpixel image w h
				     (if (eq (funcall reader vector i) bg) 0 1))
		      (incf i)))))
	      (unless (eq bg 0)
		(rotatef (svref pixels 0) (svref pixels 1))))
	  (excl::with-underlying-simple-vector (array vector displacement)
	    (let ((reader (get-simple-vector-reader vector))
		  (i displacement))
	      (declare (fixnum i))
	      (dotimes (h height)
		(declare (fixnum h))
		(dotimes (w width)
		  (declare (fixnum w))
		  (x11:xputpixel image w h
				 (svref pixels (funcall reader vector i)))
		  (incf i))))))

	(tk::put-image pixmap
		       (if (eq format :bitmap)
			   (port-copy-gc-depth-1 port)
			 (port-copy-gc port))
		       image)
	(tk::destroy-image image)
	(values pixmap format pixels)))))


(defun decode-pattern-ink (pattern medium &optional tile-p)
  (multiple-value-bind (pixmap format pixels)
      (pixmap-from-pattern pattern medium)
    (let* ((drawable (or (slot-value medium 'drawable)
			 (tk::display-root-window
			  (port-display (port (medium-sheet medium))))))
	   (gc (make-instance 'ink-gcontext :drawable drawable)))
      (if (eq format :bitmap)
	  (let ((bg (svref pixels 0))
		(fg (svref pixels 1)))
	    (setf (tk::gcontext-stipple gc) pixmap
		  (tk::gcontext-fill-style gc) (if bg
						   :opaque-stippled
						 :stippled)
		  (tk::gcontext-foreground gc) (or fg 0)
		  (tk::gcontext-background gc) (or bg 0)))
      (setf (tk::gcontext-tile gc) pixmap
	    (tk::gcontext-fill-style gc) :tiled))
      (unless tile-p
	(setf (ink-gcontext-shift-tile-origin gc) t
	      (ink-gcontext-ink-clip-region gc)
	      (make-bounding-rectangle 0 0
				       (pattern-width pattern)
				       (pattern-height pattern))))
      gc)))

;;; decode color

(defgeneric decode-color (design medium))

(defmethod decode-color ((design design) (medium xt-medium))
  (decode-color-in-palette design (medium-palette medium)))

(defmethod decode-color ((x (eql +everywhere+)) (medium xt-medium))
  (decode-color-in-palette (medium-foreground medium) (medium-palette medium)))

(defmethod decode-color ((x (eql +foreground-ink+)) (medium xt-medium))
  (decode-color-in-palette (medium-foreground medium) (medium-palette medium)))

(defmethod decode-color ((x (eql +background-ink+)) (medium xt-medium))
  (decode-color-in-palette (medium-background medium) (medium-palette medium)))

(defmethod decode-color ((ink standard-opacity) (medium xt-medium))
  (if (> (opacity-value ink) 0.5)
      (decode-color +foreground-ink+ medium)
    (decode-color +background-ink+ medium)))

(defmethod decode-color ((ink contrasting-ink) (medium xt-medium))
  (decode-color (decode-contrasting-ink ink medium) medium))

(defmethod decode-color-in-palette ((design design) (palette xt-palette))
  (error "Drawing with design: ~A not yet implemented" design))



(defmethod decode-color-in-palette ((color color) (palette xt-palette))
  (let ((color-cache (palette-color-cache palette)))
    (or (gethash color color-cache)
	(setf (gethash color color-cache)
	  (if (palette-color-p palette)
	      (restart-case
		  (handler-case
		      (tk::allocate-color (palette-colormap palette)
					  (get-xcolor color palette))
		    (tk::x-colormap-full ()
		      (palette-full-error palette color)))
		(use-other-color (other)
		    :report (lambda (s)
			      (format s "Use another color"))
		    :interactive (lambda ()
				   (list (excl::prompt-for-input 'color t
				           "Enter another color to use: ")))
		  (decode-color-in-palette other palette)))
	    ;;-- support gray-scale here
	    (with-slots (white-pixel black-pixel) palette
	      (multiple-value-bind (r g b) (color-rgb color)
		(let ((luminosity (color-luminosity r g b)))
		  (if (< luminosity 0.5) black-pixel white-pixel)))))))))

(defmethod decode-color-in-palette ((color dynamic-color) (palette xt-palette))
  (let ((dynamic-color-cache (palette-dynamic-color-cache palette)))
    (or (gethash color dynamic-color-cache)
	(let ((pixel (tk::unsigned-long-array
		      (handler-case
			  (tk::alloc-color-cells
			   (palette-colormap palette) 1 0)
			(tk::x-colormap-full ()
			  (palette-full-error palette color)))
		      0)))
	  (update-palette-entry palette pixel (dynamic-color-color color))
	  (push palette (dynamic-color-palettes color))
	  (setf (gethash color dynamic-color-cache) pixel)))))

(defmethod decode-color-in-palette ((color device-color) (palette xt-palette))
  (device-color-pixel color))

(defmethod decode-color-in-palette ((color layered-color) (palette xt-palette))
  (let ((layered-color-cache (palette-layered-color-cache palette)))
    (values-list
     (or (gethash color layered-color-cache)
	 (setf (gethash color layered-color-cache)
	   (let* ((set (layered-color-set color))
		  (pixel-planes (gethash set layered-color-cache)))
	     (unless pixel-planes
	       (setq pixel-planes (decode-layered-color-set set palette)))
	     (multiple-value-list
		 (decode-layered-color (layered-color-layers color) pixel-planes))))))))

(defun decode-layered-color (layers pixel-planes)
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

(defun decode-layered-color-set (set palette)
  (let ((layered-color-cache (palette-layered-color-cache palette))
	(dynamic-color-cache (palette-dynamic-color-cache palette))
	(layers (layered-color-set-layers set))
	(dynamic-array (layered-color-set-dynamic-array set))
	layer-nplanes
	(total-nplanes 0))
    (dolist (layer layers)
      (let ((nplanes (ceiling (log layer 2))))
	(push nplanes layer-nplanes)
	(incf total-nplanes nplanes)))
    (multiple-value-bind (pixels masks)
	(handler-case
	    (tk::alloc-color-cells
	     (palette-colormap palette) 1 total-nplanes)
	  (tk::x-colormap-full ()
	    (palette-full-error palette)))
      (let ((pixel (tk::unsigned-long-array pixels 0))
	    (count 0)
	    (planes nil))
	(dolist (nplanes layer-nplanes)
	  (let ((plane-masks nil))
	    (dotimes (i nplanes)
	      (push (tk::unsigned-long-array masks count) plane-masks)
	      (incf count))
	    (push plane-masks planes)))
	(let ((pixel-planes (cons pixel planes)))
	  (map-over-layered-colors
	   #'(lambda (dimensions)
	       (let ((dynamic-color (apply #'aref dynamic-array dimensions))
		     (pixel (decode-layered-color dimensions
						  pixel-planes)))
		 (setf (gethash dynamic-color dynamic-color-cache) pixel)
		 (update-palette-entry palette pixel (dynamic-color-color dynamic-color))
		 (push palette (dynamic-color-palettes dynamic-color))))
	   set)
	  (setf (gethash set layered-color-cache) pixel-planes))))))


;; Line (& maybe eventually non-rectangular polygon) clipping

;; We use 32000 instead of the more correct 32768 because some buggy X servers
;; barf on numbers very close to 32768.

(defmacro valid-point-p (x y)
  `(and (excl:fixnump ,x) (excl:fixnump ,y)
	(< (the fixnum ,x) 32000) (> (the fixnum ,x) -32000)
	(< (the fixnum ,y) 32000) (> (the fixnum ,y) -32000)))

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
	       `(+ (ash (if (< ymax ,y) 1 0) 3)
		   (ash (if (< ,y ymin) 1 0) 2)
		   (ash (if (< xmax ,x) 1 0) 1)
		   (if (< ,x xmin) 1 0)))
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
	    ;; This means that both Ys or both Xs are out of the window
	    ;; which means that there is nothing to draw
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
	 (clipper ,x1 ,y1 ,x2 ,y2))
     t))

;; Discarding of other illegal graphics

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

(defmacro with-single-floats (variable-bindings &body body)
  (flet ((binding-var (x) (if (atom x) x (car x)))
	 (binding-form (x) (if (atom x) x (second x))))
    `(let ,(mapcar #'(lambda (x)
		       `(,(binding-var x) (float ,(binding-form x) 0s0)))
	    variable-bindings)
       (declare (single-float ,@(mapcar #'binding-var variable-bindings)))
       ,@body)))

(defmacro with-fixnums (variable-bindings &body body)
  (flet ((binding-var (x) (if (atom x) x (car x)))
	 (binding-form (x) (if (atom x) x (second x))))
    `(let ,(mapcar #'(lambda (x)
		       `(,(binding-var x) (fix-coordinate ,(binding-form x))))
	    variable-bindings)
       (declare (fixnum ,@(mapcar #'binding-var variable-bindings)))
       ,@body)))

(defmacro with-mins-and-maxes (variables &body body)
  `(let ,(mapcan #'(lambda (binding)
 		     (destructuring-bind (min-var max-var form1 form2) binding
 		       `((,min-var ,form1)
 			 (,max-var ,form2))))
 	  variables)
     (declare (fixnum ,@(mapcar #'first variables) ,@(mapcar #'second variables)))
     #+ignore (print-variables min-x min-y max-x max-y)
     ,@(mapcar #'(lambda (binding)
 		   (destructuring-bind (min-var max-var form1 form2) binding
 		     (declare (ignore form1 form2))
 		     `(when (> ,min-var ,max-var)
 			(rotatef ,min-var ,max-var))))
 	       variables)
     ,@body))

(defmacro with-clipped-fixnum-rectangle ((x1 y1 x2 y2) &body body)
  `(with-fixnums ((x1 ,x1) (y1 ,y1) (x2 ,x2) (y2 ,y2))
     (unless (valid-point-p x1 y1)
       (setq x1 (min (max -32000 x1) 32000))
       (setq y1 (min (max -32000 y1) 32000)))
     (unless (valid-point-p x2 y2)
       (setq x2 (min (max -32000 x2) 32000))
       (setq y2 (min (max -32000 y2) 32000)))
     (with-mins-and-maxes ((min-x max-x x1 x2)
 			   (min-y max-y y1 y2))
       ,@body)))


(defmethod medium-draw-point* ((medium xt-medium) x y)
  (declare (optimize (speed 3) (safety 0)))
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((ink (medium-ink medium))
	     (line-style (medium-line-style medium))
	     (thickness (line-style-thickness line-style))
	     (sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(transform-positions transform x y)
	(cond ((< thickness 1.5)
	       (fix-coordinates x y)
	       (discard-illegal-coordinates medium-draw-point* x y)
	       (tk::draw-point
		drawable
		(adjust-ink (decode-ink ink medium)
			    medium
			    x y)
		x y))
	      (t
	       ;;--rounding
	       ;;--what is the most efficient way of doing this?

	       (clim-internals::with-half-thickness-1 (lthickness rthickness) thickness
		 (declare (ignore rthickness))
		 (with-fixnums ((min-x (- x lthickness))
				(min-y (- y lthickness))
				(size thickness))
		   (discard-illegal-coordinates medium-draw-point* min-x min-y)
		   (tk::draw-ellipse-1
		    drawable
		    (adjust-ink (decode-ink ink medium)
				medium
				min-x
				min-y)
		    min-x min-y
		    size size 0 #.(* 360 64)
		    t)))))))))

(defmethod medium-draw-points* ((medium xt-medium) position-seq)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((ink (medium-ink medium))
	     (line-style (medium-line-style medium))
	     (thickness (line-style-thickness line-style))
	     (sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(let* ((len (length position-seq))
	       (xpoints (/ len 2))
	       (j 0)
	       (minx #.(ash 1 15))
	       (miny #.(ash 1 15)))
	  (declare (fixnum len j xpoints minx miny))
	  (macrolet
	      ((process-positions ()
		 `(if (listp position-seq)
		      (loop
			(when (null position-seq) (return nil))
			(process-position (pop position-seq)
					  (pop position-seq)))
		    (do ((i 0 (+ i 2)))
			((= i len))
		      (declare (fixnum i))
		      (process-position (aref position-seq i)
					(aref position-seq (1+ i)))))))
	    (if (< thickness 1.5)
		(tk::with-xpoint-array (points xpoints)
		  (macrolet
		      ((process-position (x y)
			 `(let ((x ,x)
				(y ,y))
			    (convert-to-device-coordinates transform x y)
			    (when (valid-point-p x y)
			      (minf minx x)
			      (minf miny y)
			      (setf (x11:xpoint-array-x points j) x
				    (x11:xpoint-array-y points j) y)
			      (incf j)))))
		    (process-positions))
		  (x11:xdrawpoints (tk::object-display drawable)
				   drawable
				   (adjust-ink (decode-ink ink medium)
					       medium minx miny)
				   points
				   j
				   x11:coordmodeorigin))
	      (clim-internals::with-half-thickness-1 (lthickness rthickness) thickness
	        (declare (ignore rthickness))
		(setq thickness (round thickness))
		(tk::with-xarc-array (points xpoints)
		  (macrolet
		      ((process-position (x y)
			 `(let ((x ,x)
				(y ,y))
			    (transform-positions transform x y)
			    (with-fixnums ((x (- x lthickness))
					   (y (- y lthickness)))
			      (when (valid-point-p x y)
				(minf minx x)
				(minf miny y)
				(setf (x11:xarc-array-x points j) x
				      (x11:xarc-array-y points j) y
				      (x11:xarc-array-width points j) thickness
				      (x11:xarc-array-height points j) thickness
				      (x11:xarc-array-angle1 points j) 0
				      (x11:xarc-array-angle2 points j) #.(* 360 64))
				(incf j))))))
		    (process-positions))
		  (x11::xfillarcs (tk::object-display drawable)
				  drawable
				  (adjust-ink (decode-ink ink medium)
					      medium minx miny)
				  points
				  j))))))))))


(defmethod medium-draw-line* ((medium xt-medium) x1 y1 x2 y2)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((ink (medium-ink medium))
	     (sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(convert-to-device-coordinates transform x1 y1 x2 y2)
	(when (clip-invalidate-line x1 y1 x2 y2)
	  (tk::draw-line
	   drawable
	   (adjust-ink (decode-ink ink medium)
		       medium
		       ;;-- This is not right in the case of thin lines
		       (the fixnum (min (the fixnum x1) (the fixnum x2)))
		       (the fixnum (min (the fixnum y1) (the fixnum y2))))
	   x1 y1 x2 y2))))))

(defmethod medium-draw-lines* ((medium xt-medium) position-seq)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet))
	     (len (length position-seq))
	     (npoints (/ len 4))
	     ;; These really are fixnums, since we're fixing coordinates below
	     (minx most-positive-fixnum)
	     (miny most-positive-fixnum)
	     ink)
	(tk::with-xsegment-array (points npoints)
	  (macrolet ((stuff-line-guts (x1 y1 x2 y2)
		       `(let ((x1 ,x1)
			      (y1 ,y1)
			      (x2 ,x2)
			      (y2 ,y2))
			  (convert-to-device-coordinates transform x1 y1)
			  (convert-to-device-coordinates transform x2 y2)
			  (when (clip-invalidate-line x1 y1 x2 y2)
			    (minf minx x1)
			    (minf miny y1)
			    (minf minx x2)
			    (minf miny y2)
			    (setf (x11:xsegment-array-x1 points i) x1
				  (x11:xsegment-array-y1 points i) y1
				  (x11:xsegment-array-x2 points i) x2
				  (x11:xsegment-array-y2 points i) y2)))))
	    (let ((i 0))
	      (declare (fixnum i))
	      (if (listp position-seq)
		  (do ((ps position-seq))
		      ((null ps))
		    (when (stuff-line-guts (pop ps) (pop ps) (pop ps) (pop ps))
		      (incf i)))
		(do ((ps position-seq)
		     (j 0 (+ j 4)))
		    ((= j len))
		  (declare (fixnum i))
		  (when (stuff-line-guts (aref ps j) (aref ps (1+ j))
					 (aref ps (+ 2 j)) (aref ps (+ 3 j)))
		    (incf i))))
	      (unless (zerop i)
		(setq ink
		  (adjust-ink (decode-ink (medium-ink medium) medium) medium
			      minx miny))
		(x11:xdrawsegments
		 (tk::object-display drawable)
		 drawable
		 ink
		 points
		 i)))))))))

#+debugging
(defmacro print-variables (&rest variables)
  `(format excl::*initial-terminal-io*
	   ,(with-output-to-string (s)
	     (dolist (var variables)
	       (format s "~S = " var)
	       (write-string "~S, " s))
	     (write-string "~%" s))
	   ,@variables))

(defmethod medium-draw-rectangle* ((medium xt-medium) ox1 oy1 ox2 oy2 filled)
  (declare (optimize (safety 0) (speed 3)))
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((ink (medium-ink medium))
	     (sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(assert (rectilinear-transformation-p transform))
	(convert-to-device-coordinates transform ox1 oy1 ox2 oy2)
	(with-clipped-fixnum-rectangle (ox1 oy1 ox2 oy2)
	  (tk::draw-rectangle
	   drawable
	   (adjust-ink (decode-ink ink medium)
		       medium
		       min-x min-y)
	   min-x min-y
	   (- max-x min-x)
	   (- max-y min-y)
	   filled))))))

(defmethod medium-draw-rectangles* ((medium xt-medium) rectangles filled)
  (declare (optimize (speed 3) (safety 0)))
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((ink (medium-ink medium))
	     (sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet))
	     (len (length rectangles))
	     (nrects (/ len 4))
	     (overall-min-x #.(1- (ash 1 16)))
	     (overall-min-y #.(1- (ash 1 16))))
	(declare (fixnum len nrects overall-min-x overall-min-y))
	(assert (zerop (mod len 4)) () "Must be a multiple of 4")
	(assert (rectilinear-transformation-p transform))
	(tk::with-xrectangle-array (rects nrects)
	  (macrolet ((guts (x1 y1 x2 y2)
		       `(let ((x1 ,x1) (y1 ,y1) (x2 ,x2) (y2 ,y2))
			  (convert-to-device-coordinates transform x1 y1 x2 y2)
			  (with-clipped-fixnum-rectangle (x1 y1 x2 y2)
			    (minf overall-min-x min-x)
			    (minf overall-min-y min-y)
			    (setf (x11:xrectangle-array-x rects i) min-x
				  (x11:xrectangle-array-y rects i) min-y
				  (x11:xrectangle-array-width rects i)
				  (- max-x min-x)
				  (x11:xrectangle-array-height rects i)
				  (- max-y min-y))
			    (incf i)))))
	    (let ((i 0))
	      (declare (fixnum i))
	      (if (listp rectangles)
		  (loop
		    (when (null rectangles)
		      (return nil))
		    (guts (pop rectangles) (pop rectangles)
			  (pop rectangles) (pop rectangles)))
		(do ((j 0 (+ j 4)))
		    ((= j len))
		  (declare (fixnum j))
		  (guts (aref rectangles j)
			(aref rectangles (+ 1 j))
			(aref rectangles (+ 2 j))
			(aref rectangles (+ 3 j))))))
	    (tk::draw-rectangles
	     drawable
	     (adjust-ink (decode-ink ink medium)
			 medium
			 overall-min-x overall-min-y)
	     rects
	     nrects
	     filled)))))))


(defmethod medium-draw-polygon* ((medium xt-medium) position-seq closed filled)
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet))
	     (len (length position-seq))
	     (npoints (/ len 2))
	     ;; These really are fixnums, since we're fixing coordinates below
	     (minx most-positive-fixnum)
	     (miny most-positive-fixnum)
	     ink)
	(when (and closed (not filled))
	  (incf npoints))
	(tk::with-xpoint-array (points npoints)
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
		  (setf (x11:xpoint-array-x points i) x
			(x11:xpoint-array-y points i) y)))
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
		(setf (x11:xpoint-array-x points i) x
		      (x11:xpoint-array-y points i) y))))
	  (when (and closed (not filled))
	    (setf (x11:xpoint-array-x points (- npoints 1)) (x11:xpoint-array-x points 0)
		  (x11:xpoint-array-y points (- npoints 1)) (x11:xpoint-array-y points 0)))
	  (setq ink
	    (adjust-ink (decode-ink (medium-ink medium) medium) medium
			minx miny))
	  (if filled
	      (x11:xfillpolygon
	       (tk::object-display drawable)
	       drawable
	       ink
	       points
	       npoints
	       x11:x11-complex
	       x11:coordmodeorigin)
	    (x11:xdrawlines
	     (tk::object-display drawable)
	     drawable
	     ink
	     points
	     npoints
	     x11:coordmodeorigin)))))))

(defmethod medium-draw-ellipse* ((medium xt-medium)
				 center-x center-y
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (declare (optimize (speed 3) (safety 0)))
  (let ((drawable (medium-drawable medium)))
    (when drawable
      (let* ((sheet (medium-sheet medium))
	     (transform (sheet-device-transformation sheet)))
	(transform-positions transform center-x center-y)
	(transform-distances transform radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
	;;--- This is some magic that means that things get drawn
	;;--- correctly.
	(with-single-floats (center-x center-y radius-1-dx radius-1-dy
				      radius-2-dx radius-2-dy
				      start-angle end-angle)
	  (setq start-angle (mod start-angle 2pi)
		end-angle (mod end-angle 2pi))
	  (unless (> end-angle start-angle)
	    (incf end-angle 2pi))
	  ;;--rounding in draw-ellipse
	  (with-single-floats ((x-radius 0)
			       (y-radius 0))

	    (cond ((and (= radius-1-dx 0) (= radius-2-dy 0))
		   (setq x-radius (abs radius-2-dx)
			 y-radius (abs radius-1-dy)))
		  ((and (= radius-2-dx 0) (= radius-1-dy 0))
		   (setq x-radius (abs radius-1-dx)
			 y-radius (abs radius-2-dy)))
		  (t
		   (let ((s-1 (+ (* radius-1-dx radius-1-dx)
				 (* radius-1-dy radius-1-dy)))
			 (s-2 (+ (* radius-2-dx radius-2-dx)
				 (* radius-2-dy radius-2-dy))))
		     (if (= s-1 s-2)
			 (let ((r (sqrt s-1)))
			   (setq x-radius r y-radius r))
                         ;; Degrade to drawing a rectilinear ellipse
                         (setq x-radius (sqrt s-1)
                               y-radius (sqrt s-2))))))

	    (setq start-angle (* start-angle #.(float (/ (* 360 64) (* 2 pi)) 0s0)))
	    (setq end-angle (* end-angle #.(float (/ (* 360 64) (* 2 pi)) 0s0)))

	    ;;--rounding
	    (with-fixnums ((x-min (- center-x x-radius))
			   (y-min (- center-y y-radius))
			   (width (* 2 x-radius))
			   (height (* 2 y-radius))
			   end-angle
			   start-angle)
	      (discard-illegal-coordinates medium-draw-ellipse* x-min y-min)
              (tk::draw-ellipse-1 drawable
				  (adjust-ink (decode-ink (medium-ink medium) medium)
					      medium
					      x-min
					      y-min)
				  x-min
				  y-min
				  width
				  height
				  start-angle
				  (- end-angle start-angle)
				  filled))))))))

(defun compute-text-alignment-delta (width height baseline align-x align-y)
  (values (ecase align-x
            (:right width)
            (:center (floor width 2))
            (:left 0))
          (ecase align-y
            (:bottom (- baseline height))
            (:center (- baseline (floor height 2)))
            (:baseline 0)
            (:top baseline))))

(defmethod medium-draw-text* ((medium xt-medium)
			      string-or-char x y start end
			      align-x align-y
			      towards-x towards-y transform-glyphs)
  (declare (ignorable transform-glyphs))
  (let* ((port (port medium))
	 (sheet (medium-sheet medium))
	 (transform (sheet-device-transformation sheet))
	 (text-style (medium-merged-text-style medium))
	 (string (string string-or-char)))
    ;;--rounding errors?
    (transform-positions transform x y)
    (when (and towards-x towards-y)
      (transform-positions transform towards-x towards-y))

    (multiple-value-bind (width height last-x last-y baseline)
	(text-size sheet string :text-style text-style :start start :end end)
      (declare (ignore last-x))
      (unless (zerop last-y)
	(error "Attempting to draw multi-line using ~S" 'medium-draw-text*))

      ;; These two are used to correct the alignment after text has
      ;; been rotated:
      (multiple-value-bind (left up)
          (compute-text-alignment-delta width height baseline align-x align-y) 
        (let ((y-factor 0)
              (x-factor 1))
          (if (and towards-x towards-y)
              (let ((xxx (- towards-x x))
                    (yyy (- towards-y y)))
                (cond ((zerop yyy)          ; horizontal (UNTESTED)
                       (setq y-factor 0)
                       (setq x-factor (if (minusp xxx) -1 1))
                       (setf x (- x (* x-factor left))
                             towards-x (- towards-x (* x-factor left))
                             y (+ y (* x-factor up))
                             towards-y (+ towards-y (* x-factor up))))
                      ((zerop xxx)          ; vertical
                       (setq x-factor 0)
                       (setq y-factor (if (minusp yyy) -1 1))
                       (setf y (- y (* y-factor left))
                             towards-y (- towards-y (* y-factor left))
                             x (- x (* y-factor up))
                             towards-x (- towards-x (* y-factor up))))
                      (t
                       ;; this branch is rather expensive.
                       ;; avoid these calls if the calculation is trivial. jpm.
                       (let ((alpha (atan yyy xxx)))
                         (setq y-factor (sin alpha))
                         (setq x-factor (cos alpha))))))
              (setf x (- x left)
                    y (+ y up)))
          (let ((drawable (medium-drawable medium))
                (font (excl:ics-target-case
                        (:-ics (text-style-mapping port text-style
                                                   *all-character-sets*))
                        (:+ics (text-style-font-set port text-style)))))
            (when drawable
              (fix-coordinates x y)
              (discard-illegal-coordinates medium-draw-text* x y)
              (let ((gc (adjust-ink
                         (decode-ink (medium-ink medium) medium)
                         medium
                         x
                         (- y height))))
                (if (and towards-x towards-y)
                    (excl:ics-target-case
                      (:+ics
                       (port-draw-rotated-multibyte-text port drawable gc x y string start end
                                                         font
                                                         towards-x towards-y transform-glyphs))
                      (:-ics
                       (setf (tk::gcontext-font gc) (text-style-mapping port text-style nil))
                       (port-draw-rotated-text port drawable gc x y string start end
                                               font towards-x towards-y transform-glyphs)))
                    (excl:ics-target-case
                      (:+ics
                       (tk::draw-multibyte-string drawable font
                                                  gc x y string start end))
                      (:-ics
                       (setf (tk::gcontext-font gc) (text-style-mapping port text-style nil))
                       (tk::draw-string drawable gc x y string start end))))))
            (let ((dx (* width x-factor))
                  (dy (* width y-factor)))
              (incf x dx)
              (incf y dy)
              (when towards-x (incf towards-x dx))
              (when towards-y (incf towards-y dy)))))))))

(defmethod medium-text-bounding-box ((medium xt-medium)
                                     string x y start end align-x
                                     align-y text-style
                                     towards-x towards-y
                                     transform-glyphs transformation)
  (declare (ignore towards-x towards-y transform-glyphs transformation x y))
  (multiple-value-bind
      (left top right bottom cx cy towards-x towards-y) (call-next-method)
    (if (and towards-y towards-x)
        (flet ((compute-rotation (cx cy towards-x towards-y)
                 (decf towards-x cx)
                 (decf towards-y cy)
                 (mod (round (atan towards-y towards-x) (/ pi 2.0)) 4)))
          (let ((transformation
                 (make-rotation-transformation
                  (* (compute-rotation cx cy towards-x
                                       towards-y)
                     (/ pi 2.0))
                  (make-point cx cy))))
            (multiple-value-bind (width height last-x last-y baseline)
                (text-size medium string :text-style text-style :start start :end end)
              (declare (ignore last-x last-y))
              (multiple-value-bind (adjust-left adjust-up)
                  (compute-text-alignment-delta width height baseline align-x align-y)
                (multiple-value-setq (left top)
                    (transform-position transformation
                                        (- left adjust-left) (+ top adjust-up)))
                (multiple-value-setq (right bottom)
                    (transform-position transformation
                                        (- right adjust-left) (+ bottom adjust-up)))))
            (values (min left right) (min top bottom)
                    (max left right) (max top bottom))))
        (values left top right bottom))))

(defun compute-rotation (x y towards-x towards-y)
  (decf towards-x x)
  (decf towards-y y)
  #+OLD ; ARE YOU KIDDING? THIS SURE IS THE LONG WAY AROUND THE BARN...JPM
  (MOD (ROUND (ATAN TOWARDS-Y TOWARDS-X) (/ PI 2.0)) 4)
  (cond ((> towards-x 0)
         (cond ((> towards-y 0) 1)
               ((= towards-y 0) 0)
               (t 3)))
        ((= towards-x 0)
         (cond ((>= towards-y 0) 1)
               (t 3)))
        (t 2)))

(defun font-set-ascent/descent (font-set)
  (loop for font in (tk::fonts-of-font-set font-set)
        maximize (xt::font-ascent font) into ascent
        maximize (xt::font-descent font) into descent
        finally (return (values ascent descent))))

(defun port-draw-rotated-multibyte-text (port
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
  (let ((rotation (compute-rotation x y towards-x towards-y)))
    (flet ((compute-string-dimensions ()
             (multiple-value-bind (character font-set escapement-x escapement-y=0 origin-x=0 origin-y
                                             bbox-width bbox-height)
                 (port-glyph-for-character-from-font-set port string font)
               (declare (ignore character font-set escapement-x escapement-y=0 origin-x=0 origin-y))
               (multiple-value-bind (ascent descent) (font-set-ascent/descent font)
                 (values bbox-width bbox-height ascent descent)))))
      (multiple-value-bind (text-width text-height ascent descent) (compute-string-dimensions)
        (multiple-value-bind (pixmap-width pixmap-height)
	    (ecase rotation
	      ((0 2) (values text-width text-height))
	      ((1 3) (values text-height text-width)))
          (let* ((side-length (max text-width text-height))
                 (square-size (expt 2 (integer-length side-length)))
                 (pixmap (make-instance 'tk::pixmap
                            :drawable
                            (xt::display-root-window
                             (port-display port))
                            :depth 1
                            :width square-size
                            :height square-size))
                 (rotation-gc (make-instance 'tk::gcontext :drawable pixmap)))
            ;; first we draw into a square pixmap:
            (setf (tk::gcontext-foreground rotation-gc) 0)
            (tk::draw-rectangle pixmap rotation-gc 0 0
                                square-size square-size t)
            (setf (tk::gcontext-foreground rotation-gc) 1)
            (tk::draw-multibyte-string pixmap font rotation-gc
                                       0 text-height
                                       string start end)
            
            
            ;; then, rotate that square:
            (rotate-pixmap pixmap rotation)

            #+debug
            (setf (tk::gcontext-clip-mask gcontext) pixmap)
            #+debug
            (tk::draw-rectangle drawable gcontext
                                0 0 square-size square-size t)
            
            ;; then we copy the text rectangle from the rotated square
            ;; into the drawable:
            (multiple-value-bind (src-x src-y x-add y-add dst-x dst-y)
                (ecase rotation
                  (0 (values 0 0
                             0 descent
                             x (- y ascent)))
                  (1 (values (- square-size pixmap-width descent) 0
                             0 0
                             (- x descent) y))
                  (2 (values (- square-size text-width)
                             (- square-size text-height descent)
                             0 descent
                             (- x pixmap-width) (- y descent)))
                  (3 (values 0 (- square-size text-width)
                             descent 0
                             (- x ascent) (- y pixmap-height))))
              (let ((rotated-pixmap (make-instance 'tk::pixmap
                                    :drawable
                                    (xt::display-root-window
                                     (port-display port))
                                    :depth 1
                                    :width (+ x-add pixmap-width)
                                    :height (+ y-add pixmap-height))))
                (tk::copy-area pixmap rotation-gc
                               src-x src-y
                               (+ x-add pixmap-width) (+ y-add pixmap-height)
                               rotated-pixmap 0 0)
                (decf (ink-gcontext-last-clip-region-tick gcontext))
                (setf (tk::gcontext-clip-mask gcontext) rotated-pixmap)
                (setf (tk::gcontext-clip-x-origin gcontext) dst-x
                      (tk::gcontext-clip-y-origin gcontext) dst-y)
                (tk::draw-rectangle drawable gcontext
                                    dst-x dst-y
                                    (+ x-add pixmap-width)
                                    (+ y-add pixmap-height) t)))))))))

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
      #+ignore (setf (tk::gcontext-clip-mask gcontext) pixmap)
      #+ignore (decf (ink-gcontext-last-clip-region-tick gcontext))

      (unless start (setq start 0))
      (unless end (setq end (length string)))

      #+debug
      (tk::copy-area pixmap gcontext 0 0
                     (tk::pixmap-width pixmap)
                     (tk::pixmap-height pixmap)
                     drawable 0 0)
      #+debug
      (progn
        (setf (tk::gcontext-clip-mask gcontext) pixmap)
        (setf (tk::gcontext-clip-x-origin gcontext) 0
              (tk::gcontext-clip-y-origin gcontext) 70)
        (tk::draw-rectangle drawable gcontext 0 70 256 256 t))

      ;;-- This should be drawing onto an appropriately size bitmap
      ;;-- that should be cached
      ;;-- Width/height is the width of string
      ;;-- Height/width is the height of the font

      (multiple-value-bind (string-pixmap pixmap-width pixmap-height)
          (find-or-cache-string-pixmap port
                                       string start end font
                                       ascent descent height rotation pixmap
                                       min-char columns rows width
                                       leftover-width leftover-height)
        ;; Now its time to
        (multiple-value-bind (dst-x dst-y)
            (ecase rotation
              (0 (values x (- y ascent)))
              (1 (values (- x descent) y))
              (2 (values (- x pixmap-width) (- y descent)))
              (3 (values (- x ascent) (- y pixmap-height))))

          (decf (ink-gcontext-last-clip-region-tick gcontext))
          (setf (tk::gcontext-clip-mask gcontext) string-pixmap)
          (setf (tk::gcontext-clip-x-origin gcontext) dst-x
                (tk::gcontext-clip-y-origin gcontext) dst-y)
          (tk::draw-rectangle drawable gcontext
                              dst-x dst-y pixmap-width
                              pixmap-height t))))))


#+ignore
(defun clear-rotation-caches (&optional (port (find-port)))
  (setf (tk-silica::port-rotated-font-cache port) nil
	(xm-silica::port-rotated-string-cache port) nil))
#+ignore
(clear-rotation-caches)

(defun find-or-cache-string-pixmap (port string start end font ascent
				    descent height rotation pixmap
				    min-char columns rows width
				    leftover-width leftover-height)
  (psetq string (subseq string start end)
	 start 0
	 end (- end start))
  (let ((strings-for-font (assoc (cons rotation font)
				 (port-rotated-string-cache port)
				 :test #'equal)))
    (let ((res (assoc string (cdr strings-for-font) :test #'string=)))
      (when res (return-from find-or-cache-string-pixmap
		  (values-list (cdr res)))))
    (flet ((compute-string-dimensions ()
	     (values (do ((r 0)
			  (i start (1+ i)))
			 ((= i end) r)
		       (incf r
			     (xt::char-width font (char-int (aref string i)))))
		     (+ ascent descent)))
	   (compute-clip-x-and-clip-y (char char-width)
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
		 (values px py))))
	   (compute-next-x-and-y (char-width x y)
	     (let ()
	       (case rotation
		 ;; Normal
		 (0 (values (+ x char-width) y))
		 ;; -ve y coordinates (up)
		 (3 (values x (- y char-width)))
		 ;; -ve x (back)
		 (2 (values (- x char-width) y))
		 ;; +ve y (down)
		 (1 (values x (+ y char-width)))))))
      (multiple-value-bind (string-width string-height)
	  (compute-string-dimensions)
	(multiple-value-bind (pixmap-width pixmap-height)
	    (ecase rotation
	      ((0 2) (values string-width string-height))
	      ((1 3) (values string-height string-width)))
	  (let ((string-pixmap (make-instance 'tk::pixmap
				 :depth 1
				 :drawable pixmap
				 :width pixmap-width
				 :height
				 pixmap-height)))
	    #+ignore
	    (tk::draw-rectangle string-pixmap clear-gc
				0 0
				pixmap-width pixmap-height
				t)
	    (multiple-value-bind (x y)
		(ecase rotation
		  (0 (values 0 ascent))
		  (1 (values descent 0))
		  (2 (values pixmap-width descent))
		  (3 (values ascent pixmap-height)))
	      (let ((copy-gc (port-copy-gc-depth-1 port)
			     #+ignore (make-instance 'tk::gcontext
					    :drawable pixmap)))
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
			(compute-clip-x-and-clip-y char char-width)
		      #+debug
		      (format excl:*initial-terminal-io* "CLip-x, clip-y ~D,~D~%"
			      clip-x clip-y)
		      #+ignore
		      (setf (tk::gcontext-clip-x-origin gcontext) clip-x
			    (tk::gcontext-clip-y-origin gcontext) clip-y
			    )
		      ;;-- We have lost the clipping region at this point!
		      ;;--- We should draw in the background color also
		      (tk::copy-area pixmap
				     copy-gc
				     clip-x clip-y
				     (if (oddp rotation) height char-width)
				     (if (oddp rotation) char-width height)
				     string-pixmap cx cy ))
		    ;;
		    (multiple-value-setq
			(x y) (compute-next-x-and-y char-width x y))))))
	    (unless strings-for-font
	      (push (setq strings-for-font (list (cons rotation font)))
		    (port-rotated-string-cache port)))
	    (push (list string string-pixmap pixmap-width pixmap-height)
		  (cdr strings-for-font))
	    (values string-pixmap pixmap-width pixmap-height)))))))

(defun find-rotated-text-pixmap (port font rotation)
  (let ((x nil))
    ;; Don't cons a hash key...JPM.
    (dolist (item (port-rotated-font-cache port))
      (let ((key (car item)))
	(when (and (equal (car key) font) (equal (second key) rotation))
	  (setq x item))))
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
		   ;;--- Perhaps we should reuse this gc instead
		   (gc (make-instance 'ink-gcontext
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
		  (code-char char))
		(tk::draw-string pixmap gc
				 (* col width)
				 (+ (* row height) ascent)
				 string)
		(incf col))
	      (rotate-pixmap pixmap rotation)
	      (xt::free-gcontext gc)
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
	 ;;-- Perhaps we should be clever and reuse these if the next
	 ;;-- time through the map is the same
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
	 ;;-- Perhaps we should reuse this bitmap instead of
	 ;;-- destroying it
	 (gc (make-instance 'ink-gcontext :drawable source-pixmap)))
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
	  )))
    (xt::destroy-pixmap mask-pixmap)
    (xt::destroy-pixmap temp-pixmap)
    (xt::free-gcontext gc)))

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


(defmethod text-style-width ((text-style text-style) (medium xt-medium))
  (tk::font-width (text-style-mapping (port medium) text-style)))

(defmethod text-style-height ((text-style text-style) (medium xt-medium))
  ;; Optimization
  (tk::font-height (text-style-mapping (port medium) text-style)))

(defmethod text-style-ascent ((text-style text-style) (medium xt-medium))
  (tk::font-ascent (text-style-mapping (port medium) text-style)))

(defmethod text-style-descent ((text-style text-style) (medium xt-medium))
  (tk::font-descent (text-style-mapping (port medium) text-style)))

(defmethod text-style-fixed-width-p ((text-style text-style) (medium xt-medium))
  (let ((font (text-style-mapping (port medium) text-style)))
    ;;-??
    (= (x11::xfontstruct-min-bounds-width font)
       (x11::xfontstruct-max-bounds-width font))))

(defmethod medium-beep ((medium xt-medium))
  (x11:xbell (port-display (port medium)) 0))

(defmethod medium-force-output ((medium xt-medium))
  (x11:xflush (port-display (port medium))))

(defmethod medium-finish-output ((medium xt-medium))
  (x11:xsync (port-display (port medium)) 0))

(defmethod port-finish-output ((port xt-port))
  (x11:xsync (port-display port) 0))

(defmacro with-medium-state-cached ((medium) &body body)
  `(progn ,medium ,@body))



(defmethod medium-draw-pixmap* ((medium xt-medium) pixmap x y
				function)
  ;;-- Perhaps the copy area functions need to observe the clipping region?
  (let* ((w (pixmap-width pixmap))
	 (h (pixmap-height pixmap))
	 (r (region-intersection (make-bounding-rectangle x y (+ x w) (+ y h))
				 (untransform-region
				  (sheet-device-transformation
				   (medium-sheet medium))
				 (medium-device-clip-region medium)))))
    (unless (eq r +nowhere+)
      (with-bounding-rectangle* (left top right bottom) r
	(copy-from-pixmap pixmap
			  (- left x) (- top y) (- right left ) (- bottom top)
			  medium
			  left top
			  function)))))
