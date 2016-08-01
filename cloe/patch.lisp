(in-package :win)


(DEFCONSTANT WM_SYSKEYDOWN #x0104)
(DEFCONSTANT WM_SYSKEYUP #x0105)

(in-package :cloe-clim)

;;;

(defmethod initialize-instance :after ((port cloe-port) &key)
  (initialize-dc)
  (setf (slot-value port 'logpixelsy) (win::get-device-caps *dc* 90))
  (setf (slot-value port 'pyrex::default-palette) nil)
  nil)

;;;

(defmethod enable-mirror ((port cloe-port) sheet)
  (win::show-window (sheet-mirror sheet) :type :show))

(defmethod disable-mirror ((port cloe-port) sheet)
  (win::show-window (sheet-mirror sheet) :type :hide))

(defmethod set-sheet-mirror-edges* ((port cloe-port) sheet left top right bottom)
  (fix-coordinates left top right bottom)
  (win::set-window-position (sheet-mirror sheet) 0
			    left top (- right left) (- bottom top)
			    (logior win::swp_noactivate win::swp_nozorder)))

(defmethod event-handler ((port cloe-port) args)
  (with-slots (event-queue pointer-sheet pointer-x pointer-y) port
    (let ((sheet (mirror->sheet port (win::get-16bit args 0)))
	  (message (win::get-16bit args 2)))
      (declare (fixnum message))
      (when sheet
	(cond ((eql message win::wm_mousemove)
	       (note-pointer-motion port sheet (win::get-16bit args 6) (win::get-16bit args 8)))

	      ((eql message win::wm_paint)
	       (let ((ileft (win::get-16bit args 4))
		     (itop (win::get-16bit args 6))
		     (iright (win::get-16bit args 8))
		     (ibottom (win::get-16bit args 10)))
		 (unless (or (= ileft iright) (= itop ibottom))	;seems to happen alot
		   (queue-put event-queue
			      (allocate-event 'window-repaint-event
				;;--- Should this be (MIRROR-REGION PORT SHEET), as
				;;--- it is in the :CONFIGURE-NOTIFY case?
				:native-region (sheet-native-region sheet)
				:region (make-bounding-rectangle ileft itop iright ibottom)
				:sheet sheet)))))

	      #||
	      ;; scrolling
	      ((or (eql message win::wm_hscroll) (eql message win::wm_vscroll))
	       (let ((type (win::get-16bit args 4))
		     (position (win::get-16bit args 6))
		     (message (cond ((eql message win::wm_hscroll) :x)
				    ((eql message win::wm_vscroll) :y))))
		 (declare (fixnum type position))
		 (multiple-value-bind (type position)
		     (cond ((eql type win::sb_lineup)
			    (values :relative-jump -1))
			   ((eql type win::sb_linedown)
			    (values :relative-jump +1))
			   ((eql type win::sb_pageup)
			    (values :screenful -1))
			   ((eql type win::sb_pagedown)
			    (values :screenful +1))
			   ((eql type win::sb_thumbposition)
			    (values :percentage position))
			   ((eql type win::sb_top)
			    (values :percentage 0))
			   ((eql type win::sb_bottom)
			    (values :percentage 100)))
		   (when type
		     (queue-put pending-scrolls (list message type position))
		     (pushnew stream *cloe-windows-with-deferred-events*)))))
	      ||#

	      ;; resizing
	      ((or (eql message win::wm_move)
		   (eql message win::wm_size))
	       (queue-put event-queue
			  (allocate-event 'window-configuration-event
			    :sheet sheet)))

	      ;; character typed
	      ((eql message win::wm_char)
	       (let ((char (aref args 4)))
		 (flush-pointer-motion port)
		 (queue-put event-queue
			    (allocate-event 'key-press-event
			      :key-name nil
			      :character char
			      :modifier-state (port-modifier-state port)
			      :sheet sheet))))

	      #||
	      ((or (eql message win::wm_keydown)
		   (eql message win::wm_keyup)
		   (eql message win::wm_syskeyup)
		   (eql message win::wm_syskeydown))
	       (let ((vk (char-code (aref args 4))))
		 (flush-pointer-motion port)
		 (queue-put event-queue
			    (allocate-event
			      (cond ((or (eql message win::wm_keydown)
					 (eql message win::wm_syskeydown))
				     'key-press-event)
				    ((or (eql message win::wm_keyup)
					 (eql message win::wm_syskeyup))
				     'key-release-event))
			      :key-name (vk->keysym vk)
			      :character nil
			      :modifier-state (port-modifier-state port)
			      :sheet sheet))))
	      ||#

	      ;; button press or release
	      ((or (eql message win::wm_lbuttondown)
		   (eql message win::wm_rbuttondown)
		   (eql message win::wm_mbuttondown)
		   (eql message win::wm_lbuttonup)
		   (eql message win::wm_rbuttonup)
		   (eql message win::wm_mbuttonup))
	       (let ((modifier-state (windows-mask->modifier-state (win::get-16bit args 4)))
		     (pointer (port-pointer port)))
		 (when pointer
		   (flush-pointer-motion port)
		   (setf (port-modifier-state port) modifier-state)
		   (multiple-value-bind (key button)
		       (cond ((eql message win::wm_lbuttondown)
			      (values 'pointer-button-press-event +pointer-left-button+))
			     ((eql message win::wm_mbuttondown)
			      (values 'pointer-button-press-event +pointer-middle-button+))
			     ((eql message win::wm_rbuttondown)
			      (values 'pointer-button-press-event +pointer-right-button+))
			     ((eql message win::wm_lbuttonup)
			      (values 'pointer-button-release-event +pointer-left-button+))
			     ((eql message win::wm_mbuttonup)
			      (values 'pointer-button-release-event +pointer-middle-button+))
			     ((eql message win::wm_rbuttonup)
			      (values 'pointer-button-release-event +pointer-right-button+)))
		     (queue-put event-queue
				(allocate-event key
				  :native-x (win::get-16bit args 6)
				  :native-y (win::get-16bit args 8)
				  :button button
				  :modifier-state modifier-state
				  :pointer pointer
				  :sheet sheet))))))
	      )))))


(defmethod distribute-event :around ((port cloe-port) (event window-configuration-event))
  (let* ((sheet (event-sheet event))
	 (mirror (sheet-mirror sheet)))
    (unless (win::is-iconic mirror)
      (let ((native-region (mirror-region port sheet)))
	(setf (slot-value event 'pyrex::native-region) native-region)
	(setf (slot-value event 'pyrex::region)
	      (untransform-region (sheet-native-transformation sheet) 
				  native-region)))
      (call-next-method))))

;;;

(defmethod engraft-medium :after ((medium cloe-medium) (port cloe-port) sheet)
  (with-slots (window background-dc-image foreground-dc-image) medium
    (unless window
      (setq window (sheet-mirror sheet)))
    (setf background-dc-image (dc-image-for-ink medium (medium-background medium)))
    (setf foreground-dc-image (dc-image-for-ink medium (medium-foreground medium)))))

(defmethod medium-draw-polygon* ((medium cloe-medium) position-seq closed filled)
  (let* ((sheet (medium-sheet medium))
	 (transform (sheet-device-transformation sheet))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (drawable (medium-drawable medium))
	 (minx most-positive-fixnum)
	 (miny most-positive-fixnum)
	 (length (length position-seq)))
    ;; These really are fixnums, since we're fixing coordinates below
    (declare (type fixnum minx miny))
    (with-stack-array (points (if (and closed line-style) (+ length 2) length))
      (declare (type simple-vector points))
      (replace points position-seq)		;set up the initial contents
      (do ((i 0 (+ i 2)))
	  ((>= i length))
	(let ((x (svref points i))
	      (y (svref points (1+ i))))
	  (convert-to-device-coordinates transform x y)
	  (setf (svref points i) x)
	  (setf (svref points (1+ i)) y)
	  (minf minx x)
	  (minf miny y)))
      (when (and closed line-style)		;kludge
	(setf (svref points length) (svref points 0))
	(setf (svref points (+ length 1)) (svref points 1)))
      (set-dc-for-ink medium ink line-style)
      (if (null line-style)
	  (win::polygon drawable (coerce points 'list))
	  (win::polyline drawable (coerce points 'list) closed)))))

(defmethod medium-draw-character* ((medium cloe-medium)
				   character x y align-x align-y
				   towards-x towards-y transform-glyphs)
  (let* ((sheet (medium-sheet medium))
	 (transform (sheet-device-transformation sheet))
	 (ink (medium-ink medium))
	 (text-style (medium-merged-text-style medium))
	 (drawable (medium-drawable medium)))
    (convert-to-device-coordinates transform x y)
    (when towards-x
      (convert-to-device-coordinates transform towards-x towards-y))
    (let* ((font (text-style-mapping (port medium) text-style))
	   (height (cloe-font-height font))
	   (descent (cloe-font-descent font))
	   (ascent (cloe-font-ascent font)))
      (let ((x-adjust 
	      (compute-text-x-adjustment align-x medium character text-style))
	    (y-adjust
	      (compute-text-y-adjustment align-y descent ascent height)))
	(incf x x-adjust)
	(incf y y-adjust)
	(when towards-x
	  (incf towards-x x-adjust)
	  (incf towards-y y-adjust)))
      (decf y ascent)		;text is positioned by its top left on CLOE
      (set-dc-for-text medium ink (cloe-font-index font))
      (with-temporary-string (string :length 1)
	(vector-push character string)
	(win::text-out drawable x y string)))))

(defmethod dc-image-for-ink (medium (ink pattern))
  (or (gethash ink *ink-to-image*)
      (setf (gethash ink *ink-to-image*)
	    ;; This is broken, but it's hard to do right.
	    (multiple-value-bind (array designs)
		(decode-pattern ink)
	      (unless (and (= (length designs) 2)
			   (eq (aref designs 0) +background-ink+)
			   (eq (aref designs 1) +foreground-ink+))
		(error "Can't handle this pattern."))
	      (let ((into (make-array 8 :element-type '(unsigned-byte 16) :initial-element 0))
		    (dc-image (copy-dc-image (slot-value medium 'foreground-dc-image)))
		    (width (array-dimension array 1))
		    (height (array-dimension array 0)))
		(macrolet ((collect-byte (row-no)
			     `(let ((result 0))
				(dotimes (j (min 8 width))
				  (setf (ldb (byte 1 (- 7 j)) result)
					(aref array ,row-no j)))
				(logxor result #2r11111111))))
		  (dotimes (i (min 8 height))
		    (setf (aref into i) (collect-byte i))))
		(let ((bitmap (win::create-bitmap into)))
		  (setf (dc-image-bitmap dc-image) bitmap)
		  (setf (dc-image-brush dc-image) (win::create-pattern-brush bitmap))
		  (setf (dc-image-background-color dc-image)
			(dc-image-text-color (slot-value medium 'background-dc-image))))
		dc-image)))))



(defmethod medium-copy-area ((from-medium cloe-medium) from-x from-y width height
			     (to-medium cloe-medium) to-x to-y)
  (unless (eq from-medium to-medium)
    (error "Can't copy."))
  (let ((transform (sheet-device-transformation (medium-sheet from-medium))))
    (convert-to-device-coordinates transform from-x from-y to-x to-y)
    (convert-to-device-distances transform width height)
    (let ((x-delta (- to-x from-x))		;can be negative
	  (y-delta (- to-y from-y)))		;can be negative
      (win::scroll-dc (medium-drawable from-medium)
		      x-delta y-delta
		      from-x from-y (+ from-x width) (+ from-y height) 
		      0 0 width height))))
