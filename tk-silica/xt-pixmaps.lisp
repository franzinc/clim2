;; See the file LICENSE for the full license governing this code.
;;

(in-package :xm-silica)

(defclass xt-pixmap (tk::pixmap pixmap)
  ())

(defmethod port-allocate-pixmap ((port xt-port) medium width height)
  (declare (ignore medium))
  (fix-coordinates width height)
  (let ((root (tk::display-root-window (port-display port))))
    (make-instance 'xt-pixmap
      :drawable root
      :width width
      :height height
      :depth (tk::drawable-depth root)
      :port port)))

(defmethod port-deallocate-pixmap ((port xt-port) (pixmap xt-pixmap))
  (x11:xfreepixmap (port-display port) pixmap))

(defmethod pixmap-width ((pixmap xt-pixmap))
  (tk::pixmap-width pixmap))

(defmethod pixmap-height ((pixmap xt-pixmap))
  (tk::pixmap-height pixmap))

(defmethod pixmap-depth ((pixmap xt-pixmap))
  (tk::pixmap-depth pixmap))


(defclass xt-pixmap-medium (xt-medium basic-pixmap-medium) ())

(defmethod make-pixmap-medium ((port xt-port) sheet &key width height)
  (let* ((pixmap (port-allocate-pixmap port sheet width height))
	 (medium (make-instance 'xt-pixmap-medium
		   :port port
		   :sheet sheet
		   :pixmap pixmap)))
    (setf (slot-value medium 'drawable) pixmap)
    (update-medium-ink-table medium)
    medium))

(defmethod medium-copy-area
    ((from-medium xt-medium) from-x from-y width height
     (to-medium xt-medium) to-x to-y
     &optional (function boole-1))
  (let ((from-transform (sheet-device-transformation (medium-sheet from-medium)))
	(to-transform (sheet-device-transformation (medium-sheet to-medium))))
    (convert-to-device-coordinates from-transform from-x from-y)
    (convert-to-device-coordinates to-transform to-x to-y)
    (convert-to-device-distances from-transform width height)
    (let* ((from-drawable (medium-drawable from-medium))
	   (to-drawable (medium-drawable to-medium))
	   (port (port from-medium))
	   (copy-gc (port-copy-gc port)))
      (when (and from-drawable to-drawable)
	(when (port-safe-backing-store port)
	  ;; Don't bother with no-expose events.
	  (return-from medium-copy-area
	    (clim-sys:without-scheduling
	      (letf-globally (((tk::gcontext-function copy-gc) function))
		(tk::copy-area from-drawable copy-gc from-x from-y
			       width height
			       to-drawable to-x to-y)))))
	(with-port-event-lock (port)
	  ;; Do this to lock access to the copy-gc
	  (clim-sys:without-scheduling
	    (letf-globally (((tk::gcontext-function copy-gc) function))
	      (x11:_xflushgccache (port-display port) copy-gc)
	      (let ((seq-no 0))
		(setq seq-no (x11:xnextrequest (port-display port)))
		(tk::copy-area from-drawable copy-gc from-x from-y
			       width height to-drawable to-x to-y)
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
			 (let ((sheet (medium-sheet to-medium)))
			   (queue-repaint
			    sheet
			    (allocate-event 'window-repaint-event
					    :native-region (make-bounding-rectangle minx miny maxx maxy)
					    :region (untransform-region
						     (sheet-device-transformation sheet)
						     (make-bounding-rectangle minx miny maxx maxy))
					    :sheet sheet))))
		       (setq event (tk::get-event-matching-sequence-and-types
				    to-drawable seq-no '(:graphics-expose)
				    :block nil))
		       (unless event
			 (return))))))))))))))


(defmethod medium-copy-area
    ((pixmap xt-pixmap) from-x from-y width height
     (to-medium xt-medium) to-x to-y
     &optional (function boole-1))
  ;;-- What about the graphics exposure event problem
  (let ((transform (sheet-device-transformation (medium-sheet to-medium))))
    (convert-to-device-coordinates transform to-x to-y)
    (convert-to-device-distances transform width height)
    (fix-coordinates from-x from-y)
    (let* ((window (medium-drawable to-medium))
	   (copy-gc (port-copy-gc (port to-medium))))
      (clim-sys:without-scheduling
	(letf-globally (((tk::gcontext-function copy-gc) function))
	  (tk::copy-area
	   pixmap copy-gc from-x from-y width height
	   window to-x to-y))))))

(defmethod medium-copy-area
    ((from-medium xt-medium) from-x from-y width height
     (pixmap xt-pixmap) to-x to-y
     &optional (function boole-1))
  ;;-- What about the graphics exposure event problem
  (let ((transform (sheet-device-transformation (medium-sheet from-medium))))
    (convert-to-device-coordinates transform from-x from-y)
    (convert-to-device-distances transform width height)
    (fix-coordinates to-x to-y)
    (let* ((window (medium-drawable from-medium))
	   (copy-gc (port-copy-gc (port from-medium))))
      (clim-sys:without-scheduling
	(letf-globally (((tk::gcontext-function copy-gc) function))
	  (tk::copy-area
	   window copy-gc from-x from-y width height
	   pixmap to-x to-y))))))

(defmethod medium-copy-area
    ((from-pixmap xt-pixmap) from-x from-y width height
     (to-pixmap xt-pixmap) to-x to-y
     &optional (function boole-1))
  ;;-- What about the graphics exposure event problem
  (fix-coordinates from-x from-y to-x to-y)
  (let ((copy-gc (port-copy-gc (port from-pixmap))))
    (clim-sys:without-scheduling
      (letf-globally (((tk::gcontext-function copy-gc) function))
	(tk::copy-area
	 from-pixmap copy-gc from-x from-y width height
	 to-pixmap to-x to-y)))))

(defmethod make-pattern-from-pixmap ((pixmap xt-pixmap)
				     &key
				     (x 0)
				     (y 0)
				     (width (pixmap-width pixmap))
				     (height (pixmap-height pixmap)))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((image (tk::get-image pixmap :x x :y y :width width :height height))
	 (hicolor-p (> (tk::image-depth image) 8))
	 (pattern-data (make-array (list height width)))
	 ;; warning this assumes a max depth of 8. For larger depths
	 ;; we should use a sparse array
	 (pixels (or hicolor-p (make-array 256)))
	 (colors nil)
	 (color-count -1)
	 (palette (port-default-palette (port pixmap))))
    (declare (simple-vector pixels)
	     (type (simple-array t (* *)) pattern-data))
    #+ignore
    (assert (not (> (tk::image-depth image) 8)) ()
      "~s doesn't support images of depth greater than 8" 'make-pattern-from-pixmap)
    (dotimes (w width)
      (dotimes (h height)
	(let ((pixel (x11:xgetpixel image w h)))
	  (setf (aref pattern-data h w)
	    (if hicolor-p
		;; slow but better than nothing
		(let* ((color (device-color-color
			       (make-device-color palette pixel)))
		       (index (position color colors)))
		  (if index
		      (- color-count index)
		    (progn
		      (push color colors)
		      (incf color-count))))
	      ;; 8-bit image
	      (or (svref pixels pixel)
		  (let ((color (device-color-color
				(make-device-color palette pixel))))
		    (push color colors)
		    (setf (svref pixels pixel)
		      (incf color-count)))))))))
    (tk::destroy-image image)
    (make-pattern pattern-data
		  (nreverse colors))))
