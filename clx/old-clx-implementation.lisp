;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: old-clx-implementation.lisp,v 1.2 92/02/24 13:23:56 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 Franz Inc.  All rights reserved."

(defvar *window-manager-border-size* 2)


;;;--- What should this really be returning?
(defmethod host-window-margins ((stream clx-root-window))
  (values 0 0 0 0))

(defmethod window-stream-class-name ((stream clx-root-window))
  'clx-window)

(defmethod clx-stream-root ((stream clx-root-window))
  stream)

(defmethod close ((stream clx-root-window) &key abort)
  (declare (ignore abort))
  (let ((frame (window-stream-to-frame stream)))
    (if frame
        (frame-exit frame)
	(with-slots (window) stream
	  (when window
	    (xlib:destroy-window (shiftf window nil)))))))

(defmethod stream-event-handler ((stream clx-root-window)
				 &key (timeout nil)
				      (input-wait-test nil)
				      (original-stream stream))
  ;; No hang-p argument, use :timeout 0.
  (with-slots (display (root-window window) modifier-state) stream
    (let ((pointer (stream-primary-pointer stream))
	  ;; If we're inside an encapsulating stream, such as the input editor, and a window
	  ;; resizing event is received, don't get confused while handling the event
	  (*original-stream* nil)
	  ws)
      ;; Return after processing each event or after the timeout.
      (block event-loop
	(loop
	  ;; The assumption is that the input-wait-test function can only change
	  ;; its value when an event is received and processed.  The only
	  ;; commonly-desired exception is a timeout, which we provide directly.
	  (when (and input-wait-test (funcall input-wait-test original-stream))
	    (return :input-wait-test))
	  (xlib:display-force-output display)
	  (when (not (or (xlib:event-listen display)
			 (not (xlib::buffer-input-wait display timeout))))
	    (return :timeout))
	  ;; The idea below is that EVENT-COND will return NIL on timeout,
	  ;; return T for "boring" events, and will call (RETURN :foo) for
	  ;; input events, which will return from the loop above.
	  (if (xlib:event-cond (display :discard-p t :timeout timeout)
		((:motion-notify) (window)
		 (setq ws (getf (xlib:window-plist window) 'stream))	;test form
		 ;; We use the pointer-motion-hint strategy to avoid event overload.
		 (multiple-value-bind (x y same-screen-p child mask root-x root-y root)
		     (xlib:query-pointer root-window)
		   (declare (ignore x y same-screen-p child mask root))
		   (pointer-set-position* pointer root-x root-y)
		   (setf (pointer-window pointer) ws)
		   (setf (pointer-motion-pending ws pointer) t))
		 (return-from event-loop :mouse-motion))
		((:button-press :button-release) (window event-key code x y)
		 (setq ws (getf (xlib:window-plist window) 'stream))	;test form
		 (let ((old-state (pointer-button-state pointer))
		       ;; X enumerates buttons, we need a mask...
		       ;;--- Why isn't this just (ASH 1 CODE)?
		       (new-state (dpb 1 (byte 1 (1- code)) 0)))
		   (setf (pointer-button-state pointer)
			 (ecase event-key
			   (:button-press (logior new-state old-state))
			   (:button-release (logandc1 new-state old-state))))
		   (setf (pointer-window pointer) ws)
		   ;; Resolve the selected-window issue.  We currently just put the
		   ;; button-press "blip" in the clicked-on window's input buffer.
		   ;; X and Y are presumed to be fixnums here
		   (case event-key
		     (:button-press
		       (stream-note-pointer-button-press
			 ws pointer new-state modifier-state x y))
		     (:button-release
		       (when *generate-button-release-events*
			 (stream-note-pointer-button-release
			   ws pointer new-state modifier-state x y)))))
		 (return-from event-loop :input-buffer))
		((:key-press :key-release) (window code event-key state)
		 (setq ws (getf (xlib:window-plist window) 'stream))	;test form
		 (let ((ch (xlib:keycode->character display code state)))
		   (cond ((characterp ch)
			  (when (and ws (eql event-key :key-press))
			    (when (eql ch #\Return)
			      ;; Unix boxes have #\Return, not #\Newline.
			      (setq ch #\Newline))
			    (queue-put (stream-input-buffer ws) ch))
			  ;; Store the shift mask in a canonical place.
			  (setf modifier-state (state-mask->modifier-state state display)))
			 (t
			  ;; save away the current set of shifts.
			  (let ((state-modifier-state
				  (state-mask->modifier-state state display))
				(shift-modifier-state
				  (keycode->modifier-state code display)))
			    (case event-key
			      (:key-press 
				(setf modifier-state
				      (boole boole-ior
					state-modifier-state shift-modifier-state)))
			      (:key-release
				(setf modifier-state
				      (boole boole-andc2
				        state-modifier-state shift-modifier-state))))))))
		 (return-from event-loop :input-buffer))
		((:mapping-notify) (request start count)
		 t				;test form
		 (xlib:mapping-notify display request start count)
		 (when (eql request :modifier)
		   (fill-keycode->modifier-state display)
		   (fill-clim-modifier-key-index->x-state display))
		 t)
		((:reparent-notify) (window parent x y)
		 (setq ws (getf (xlib:window-plist window) 'stream))	;test form
		 ;; We're interested in the relationship between our window and our
		 ;; parent, so if that's what this event is about, take notice.  If
		 ;; the event is about any other windows (e.g. intermediaries inserted
		 ;; by the window manger), then it's talking about something beyond
		 ;; our sphere of influence and we ignore it.
		 (cond ((eql parent (clx-stream-window (window-parent ws)))
			(with-slots (left top right bottom) ws
			  (let ((border-width
				  ;;--- This is technically more correct, but it doesn't
				  ;;--- account for the title bar, and generally messes
				  ;;--- CLIM up.
				  #+++ignore *window-manager-border-size*
				  #---ignore 0))
			    (unless (and (= x left) (= y top))
			      (window-note-size-or-position-change
				ws (- x border-width) (- y border-width)
				(+ x (- right left) border-width)
				(+ y (- bottom top) border-width)))))
			(setf (slot-value ws 'reparented-p) nil))
		       (t (setf (slot-value ws 'reparented-p) t)))
		 t)
		((:configure-notify) (window width height x y send-event-p border-width)
		 (setq ws (getf (xlib:window-plist window) 'stream))	;test form
		 ;; We have to discard the current event now because we'll
		 ;; likely THROW out and we don't want to process it again.
		 ;;--- This may be a problem in R4.4 CLX.  -- jdi
		 (xlib:discard-current-event display)
		 (let ((reparented-p (slot-value ws 'reparented-p)))
		   (cond (send-event-p
			  ;; hack to get around bug in older twm
			  (when (not (eql border-width 0))
			    (incf x border-width)
			    (incf y border-width)))
			 (reparented-p
			  (multiple-value-setq (x y)
			    (xlib:translate-coordinates
			      window 0 0 (clx-stream-window (window-parent ws))))))
		   (when reparented-p
		     ;;--- This is technically more correct, but it doesn't account
		     ;;--- for the title bar, and generally messes CLIM up.
		     #+++ignore
		     (setq border-width *window-manager-border-size*)))
		 (window-note-size-or-position-change
		   ws (- x border-width) (- y border-width)
		   (+ x width border-width) (+ y height border-width))
		 (setf (slot-value ws 'border-width) border-width)
		 t)
		((:map-notify) (window)
		 (setq ws (getf (xlib:drawable-plist window) 'stream))	;test form
		 (setf (slot-value ws 'mapped-p) t)
		 t)
		((:unmap-notify) (window)
		 (setq ws (getf (xlib:drawable-plist window) 'stream))	;test form
		 (setf (slot-value ws 'mapped-p) nil)
		 t)
		((:graphics-exposure) (drawable width height x y count)
		 (setq ws (getf (xlib:drawable-plist drawable) 'stream))	;test form
		 ;;--- we're actually losing here because we have to forcibly
		 ;;--- read out and process the graphics expose event at
		 ;;--- COPY-AREA time to ensure the consistency of the display.
		 (with-slots (update-region) ws
		   (multiple-value-bind (dsx dsy)
		       ;; Transform these coords into history coords,
		       ;; adding viewport, substracting margin
		       (viewport-to-drawing-surface-coordinates ws x y)
		     (push (make-bounding-rectangle dsx dsy 
						    (+ dsx width) (+ dsy height))
			   update-region)))
		 (when (zerop count)
		   ;; If no expose events are to come, repaint the stream.
		   (window-process-update-region ws))
		 t)
		(:exposure (window width height x y count)
		 (setq ws (getf (xlib:window-plist window) 'stream))	;test form
		 (with-slots (update-region) ws
		   (multiple-value-bind (dsx dsy)
		       ;; Transform these coords into history coords,
		       ;; adding viewport, substracting margin
		       (viewport-to-drawing-surface-coordinates ws x y)
		     (push (make-bounding-rectangle dsx dsy 
						    (+ dsx width) (+ dsy height))
			   update-region)))
		 (when (zerop count)
		   ;; If no expose events are to come, repaint the stream.
		   (window-process-update-region ws))
		 t)
		(:client-message (format window type data)
		 (setq ws (getf (xlib:window-plist window) 'stream))	;test form
		 (when (and (eql format 32)
			    (eql type :WM_PROTOCOLS)
			    (eql (xlib:atom-name
				   display 
				   (aref (the (simple-array (unsigned-byte 32) (5)) data)
					 0))
				:WM_DELETE_WINDOW))
		   (window-manager-close ws))
		 t))
	      ;; If we got a non-input event, just run the loop again, else
	      ;; it is a timeout
	      nil
	      (return :timeout)))))))


(defclass clx-window
	  (window-stream)
    ((screen :reader clx-stream-screen)
     (window :reader clx-stream-window)
     (root :reader clx-stream-root)
     (border-width :initform nil :initarg :border-width)
     ;; These two are used on monochrome screens
     (white-pixel)
     (black-pixel)
     (clip-mask :initform :none)
     (reparented-p :initform nil)	; true when wm reparents us
     (nuked-p :initform nil)		; true when wm nukes us
     (mapped-p :initform nil :reader window-visibility)
     (points-to-pixels)))

(defparameter *clx-use-color* t)	;for debugging monochrome...
(defmethod color-stream-p ((stream clx-window))
  (and *clx-use-color*
       (> (xlib:screen-root-depth (clx-stream-screen stream)) 2)))

(defmethod initialize-instance :before ((stream clx-window) &key parent)
  (setf (slot-value stream 'display-device-type)
	(slot-value parent 'display-device-type)))  

(defmethod initialize-instance :after
	   ((stream clx-window) &key left top right bottom
				     save-under label
				     (borders t))
  (with-slots (window root screen copy-gc points-to-pixels
	       color-p border-width black-pixel white-pixel) stream
    (let* ((parent (window-parent stream))
	   (top-level (null (window-parent parent)))
	   (x-width 0) (x-height 0))
      (cond (borders
	     ;;--- This is technically more correct, but it doesn't account for
	     ;;--- the title bar, and generally messes CLIM up.
	     #+++ignore
	     (when (not border-width)
	       (setq border-width *window-manager-border-size*))
	     #---ignore
	     (when (not border-width)
	       (if top-level
		   (setq border-width 0)
		   (setq border-width *window-manager-border-size*))))
	    (t (setq border-width 0)))
      (setq x-width (- right left (* 2 border-width))
	    x-height (- bottom top (* 2 border-width)))
      (setf root (clx-stream-root parent))
      (setf screen (clx-stream-screen parent))
      (setf color-p (color-stream-p stream))
      (setf white-pixel (xlib:screen-white-pixel screen))
      (setf black-pixel (xlib:screen-black-pixel screen))
      (setf points-to-pixels (* (sqrt (* (/ (xlib:screen-width screen)
					    (xlib:screen-width-in-millimeters screen))
					 (/ (xlib:screen-height screen)
					    (xlib:screen-height-in-millimeters screen))))
				(/ 25.4s0 72)))
      (let ((rounded (round points-to-pixels)))
	(when (< (abs (- points-to-pixels rounded)) .1s0)
	  (setf points-to-pixels rounded)))
      (assert (plusp x-width) (x-width)
        "Width ~D of window ~S is too small -- check layout" x-width stream)
      (assert (plusp x-height) (x-height)
	"Height ~D of window ~S is too small -- check layout" x-height stream)
      (cond (top-level
	     (setf window (xlib:create-window
			    :parent (clx-stream-window parent)
			    :x left :y top :width x-width :height x-height
			    ;; putting in bit-gravity reduces the number of repaint
			    ;; events we have to process when we resize the window.
			    :bit-gravity :north-west
			    :save-under (if save-under :on nil)
			    ;;--- this is a good idea, but is a problem on many
			    ;;--- buggy servers.
			    ;;--- :backing-store :when-mapped
			    :event-mask '(:key-press :button-press 
					  :key-release :button-release
					  :pointer-motion :pointer-motion-hint
					  :structure-notify
					  :exposure)
			    :border-width border-width))
	     (setf (window-label stream) label)
	     (xlib:set-wm-properties window :input :on
					    :initial-state :normal
					    :program-specified-position-p t
					    :program-specified-size-p t)
	     ;;--- Not quite sure why JDI chose to do it this way...
	     #+Allegro (xlib:change-property
			 window :WM_PROTOCOLS
			 (list (xlib:intern-atom
				 (xlib::window-display window)
				 "WM_DELETE_WINDOW"))
			 :atom 32)
	     #+Allegro (xlib:change-property
			 window :WM_CLIENT_MACHINE (short-site-name)
			 :string 8)
	     ;;--- ...but it works better for us like this
	     #-Allegro (setf (xlib:wm-protocols window) '(:WM_DELETE_WINDOW))
	     #-Allegro (setf (xlib:wm-client-machine window) (short-site-name)))
	    (t
	     (setf window (xlib:create-window
			    :parent (clx-stream-window parent)
			    :x left :y top :width x-width :height x-height
			    ;;--- this is a good idea, but is a problem on many
			    ;;--- buggy servers.
			    ;;--- :backing-store :when-mapped
			    :event-mask '(:button-press :button-release
					  :pointer-motion :pointer-motion-hint
					  :exposure)
			    ;; putting in bit-gravity reduces the number of repaint
			    ;; events we have to process when we resize the window.
			    :bit-gravity :north-west
			    :border-width border-width))
	     (setf (stream-input-buffer stream) (stream-input-buffer parent)))))
    (setf copy-gc (xlib:create-gcontext :drawable window :exposures :off))
    (clx-recompute-gcs stream)
    (setf (getf (xlib:window-plist window) 'stream) stream)))




(defmethod host-window-margins ((stream clx-window))
  (let* ((parent (window-parent stream)))
    ;; Assume there is a reparenting window manager running, so that
    ;; top-level windows will be forced to have a border-width of *w-m-b-s*.
    ;; -- jdi 1/16/91
    (cond ((not (window-parent parent))
	   ;;--- This is technically more correct, but it doesn't account for
	   ;;--- the title bar, and generally messes CLIM up.
	   #+++ignore
	   (values *window-manager-border-size* *window-manager-border-size*
		   *window-manager-border-size* *window-manager-border-size*)
	   #---ignore
	   (values 0 0 0 0))
	  (t
	   (with-slots (border-width) stream
	     (values border-width border-width border-width border-width))))))

(defmethod window-margins ((stream clx-window))
  (values 0 0 0 0))


(defmethod window-modifier-state ((window clx-window))
  (window-modifier-state (clx-stream-root window)))

(defmethod notify-user-1 ((stream clx-window) frame format-string &rest format-args)
  (declare (ignore frame) (dynamic-extent format-args))
  (with-menu (window stream)
    (with-end-of-line-action (:window allow)
      (apply #'format window format-string format-args)
      (fresh-line window)
      (terpri window)
      (with-text-size (:window smaller)
	(write-string "Hit any key to remove this window" window))
      (force-output window)
      (size-menu-appropriately window)
      (position-window-near-carefully window 10 10)	;whatever...
      (window-expose window)
      (read-gesture :stream window))))

(defmethod implementation-pixels-per-point ((stream clx-window))
  (with-slots (screen) stream
    (/ (xlib:screen-width screen) (* (/ 72 25.4) (xlib:screen-width-in-millimeters screen)))))

(defmethod window-label-size ((window clx-window) &optional (label (window-label window)))
  (let* ((text-style (if (listp label)
			 (getf (cdr label) ':text-style)
			 (medium-default-text-style window)))
	 (label (if (listp label) (car label) label)))
    (with-text-style (text-style window)
      (values (stream-string-width window label)
	      (stream-line-height window)))))

(defmethod (setf window-label) :after (label (stream clx-window))
  (with-slots (window) stream
    (multiple-value-bind (name icon-name)
	(etypecase label
	  (null (values "CLIM Window" "CLIM"))
	  (string (values label label))
	  (cons (values (first label) (first label))))
      (setf (xlib:wm-name window) name)
      (setf (xlib:wm-icon-name window) icon-name))
    label))


(defmethod window-shift-visible-region :after ((stream clx-window)
					       old-left old-top old-right old-bottom
					       new-left new-top new-right new-bottom)
  (let ((rectangles (ltrb-difference new-left new-top new-right new-bottom
				     old-left old-top old-right old-bottom)))
    ;; Use the rectangle-list as the internal representation for a CLX-implementation region
    (setf (slot-value stream 'update-region) rectangles)))

(defmethod window-clear-area ((stream clx-window) left top right bottom)
  (with-slots (window background-gc background-pixel) stream
    (if (integerp background-pixel)
	(xlib:clear-area window :x left :y top :width (- right left) :height (- bottom top))
	(xlib:draw-rectangle window background-gc left top (- right left) (- bottom top) t)))
  (clx-force-output-if-necessary stream))

;;--- This should clip to real regions, not just to bounding-rectangles
(defmethod invoke-with-clipping-region
	   ((stream clx-window) continuation (region standard-bounding-rectangle))
  (with-slots (window clip-mask) stream
    (multiple-value-bind (x y) (window-viewport-position* stream)
      (declare (type coordinate x y))
      (multiple-value-bind (ml mt) (window-margins stream)
	(declare (type coordinate ml mt))
      	(with-bounding-rectangle* (left top right bottom) region
	  (translate-fixnum-positions (- ml x) (- mt y)
	    left top right bottom)
	  ;;--- DEVICIZE-POINTS or something...
	  (fix-points left top right bottom)
	  ;;--- How not to cons? (this list and the rectangles)
	  (let ((old-clip-mask clip-mask))
	    (unwind-protect
		(progn
		  (setf clip-mask (list left top
					(- right left) (- bottom top)))
		  (funcall continuation stream))
	      (setf clip-mask old-clip-mask))))))))



(defmethod stream-clear-output ((stream clx-window))
  ;;;--- Not yet implemented.
  )

;;; Required
(defmethod stream-set-input-focus ((stream clx-window))
  ;;--- should there be a "default" method on input-protocol-mixin that
  ;;--- does this??
  )

(defmethod stream-restore-input-focus ((stream clx-window) old-focus)
  (declare (ignore old-focus))
  ;;--- should there be a "default" method on input-protocol-mixin that
  ;;--- does this??
  )

(defmethod window-erase-viewport ((stream clx-window))
  (with-slots (window) stream
    (xlib:clear-area window))
  (clx-force-output-if-necessary stream))

(defmethod (setf window-visibility) (visibility (stream clx-window))
  (with-slots (window screen) stream
    ;; Have to wait for map to happen or subsequent output is lost
    ;;--- How do we deal with state :UNVIEWABLE?  It means that window is
    ;;--- mapped but some superior isn't.  Do we propagate visibility up?
    ;;--- I think not.
    (if visibility				; visibility is a boolean
	;; This isn't quite right, because activating it means it's now buried??
	;; Fine: that's what WINDOW-EXPOSE is for.  -- jdi
	(xlib:map-window window)
        ;;--- WITHDRAW-WINDOW isn't in R3 CLX.
        (if (fboundp 'xlib::withdraw-window)
	    (xlib::withdraw-window window screen)
	    (xlib:unmap-window window))))
  (clx-force-output-if-necessary stream)
  visibility)

(defmethod wait-for-window-exposed ((stream clx-window))
  (unless (window-visibility stream)
    (stream-event-handler stream :input-wait-test #'window-visibility)))

(defmethod window-drawing-possible ((stream clx-window))
  ;;--- This isn't really right for a window that has backing store.
  (window-visibility stream))

(defmethod close ((stream clx-window) &key abort)
  (declare (ignore abort))
  (let ((frame (window-stream-to-frame stream))
	(parent (window-parent stream)))
    (if frame
        (frame-exit frame)
	(with-slots (window) stream
	  (when window
	    (remf (xlib:window-plist window) 'stream)
	    (xlib:destroy-window (shiftf window nil)))
	  (clx-force-output-if-necessary parent)))))

(defmethod window-manager-close ((stream clx-window))
  (close stream))

(defmethod open-stream-p ((stream clx-window))
  (with-slots (window) stream
    window))

(defmethod bounding-rectangle-set-edges :after ((stream clx-window) left top right bottom)
  (let ((width (- right left))
	(height (- bottom top)))
    (multiple-value-bind (lom tom rom bom) (host-window-margins stream)
      (declare (fixnum lom tom rom bom))
      (let ((x-width (- width lom rom))
	    (x-height (- height tom bom)))
	(assert (plusp x-width) (x-width)
	  "Width ~D of window ~S is too small -- check layout" x-width stream)
	(assert (plusp x-height) (x-height)
	  "Height ~D of window ~S is too small -- check layout" x-height stream)
	(with-slots (window) stream
	  (xlib:with-state (window)
	    (setf (xlib:drawable-x window) left
		  (xlib:drawable-y window) top
		  (xlib:drawable-width window) x-width
		  (xlib:drawable-height window) x-height))))))
  (clx-force-output-if-necessary stream t))

(defmethod bounding-rectangle-set-size :after ((stream clx-window) width height)
  (multiple-value-bind (lom tom rom bom) (host-window-margins stream)
    (declare (fixnum lom tom rom bom))
    (with-slots (window) stream
      (let ((x-width (- width lom rom))
	    (x-height (- height tom bom)))
	(assert (plusp x-width) (x-width)
	  "Width ~D of window ~S is too small -- check layout" x-width stream)
	(assert (plusp x-height) (x-height)
	  "Height ~D of window ~S is too small -- check layout" x-height stream)
	(xlib:with-state (window)
	  (setf (xlib:drawable-width window) x-width
		(xlib:drawable-height window) x-height)))))
  (clx-force-output-if-necessary stream t))

(defmethod bounding-rectangle-set-position* :after ((stream clx-window) new-left new-top)
  (with-slots (window) stream
    (xlib:with-state (window)
      (setf (xlib:drawable-x window) new-left)
      (setf (xlib:drawable-y window) new-top)))
  (clx-force-output-if-necessary stream t))



;;; Input side

(defmethod set-pointer-window-and-location ((stream clx-window) pointer)
  ;;--- What should this do?
  )

;;; The root stream-event-handler will do the appropriate keyword validation
(defmethod stream-event-handler ((stream clx-window) &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (with-slots (root) stream
    (apply #'stream-event-handler root :original-stream stream args)))

(defmethod set-stream-pointer-in-screen-coordinates
	   ((stream clx-window) pointer x y)
  (declare (ignore pointer))			;Sigh.
  (xlib:warp-pointer (clx-stream-window (clx-stream-root stream)) x y))

(defmethod stream-pointer-input-rectangle*
	   ((stream clx-window) pointer &key left top right bottom)
  (declare (ignore left top right bottom))
  (portable-pointer-input-rectangle* stream pointer))


(defclass clx-menu-window (clx-window) ())

;;; We use this hack to keep borders on menus, which are top level but sometimes
;;; not reparented.
(defmethod initialize-instance :around ((stream clx-menu-window) &rest args)
  (declare (dynamic-extent args))
  (setq stream (apply #'call-next-method stream :border-width 2 :label nil args)))

(defmethod menu-class-name ((stream clx-root-window))
  'clx-menu-window)

;;; This is called by the menu facility.  Under X, we simply set the transient-for
;;; slot to tell the window manager not to interfere with this window.  For pop
;;; up menus, this doesn't harm anything since no properties matter anyway.
;;; Also see INVOKE-WITH-MENU-AS-POPUP below.
(defmethod initialize-menu :after ((stream clx-window) associated-window)
  (let ((window (slot-value stream 'window)))
    (if associated-window
	(setf (xlib:transient-for window)
	      (slot-value associated-window 'window))
	(xlib:delete-property window :WM_TRANSIENT_FOR))))

(defvar *inside-mouse-grab* nil)

;;; This should take CLIM's idea of event-mask, not CLX's idea.  XXX -- jdi
;;;
;;; Make sure the window is mapped before we do the grab --
;;; if it isn't the grab will fail.
(defmethod invoke-with-mouse-grabbed-in-window
	   ((stream clx-window) continuation
	    &key mouse-cursor
	    (event-mask #.(xlib:make-event-mask :button-press :button-release
						:pointer-motion :pointer-motion-hint))
	    owner-p confine-to)
  (let ((sleep-time #+Allegro 0.25 #-Allegro 1))
    (flet ((grab-the-pointer (window event-mask &key owner-p confine-to cursor)
	     (do ((done nil))
		 (done)
	       (if (eql (xlib:grab-pointer window event-mask
					   :owner-p owner-p
					   :confine-to confine-to
					   :cursor cursor) 
			:success)
		   (setf done t)
		   (sleep sleep-time)))))
      (declare (dynamic-extent #'grab-the-pointer))
      (with-slots (window black-pixel white-pixel root) stream
	(dotimes (i (floor 10 sleep-time)	;Maximum of 10 seconds to map window
		    (error "Cannot grab mouse to unmapped window"))
	  #-(or excl Minima) (declare (ignore i))
	  (if (eql (xlib:window-map-state window) :unmapped)
	      (sleep sleep-time)
	      (return)))
	(unless mouse-cursor (setq mouse-cursor (default-grab-cursor root)))
	(let ((display (xlib:window-display window)))
	  (unwind-protect
	      (progn
		(without-scheduling
		  (grab-the-pointer window event-mask
				    :owner-p owner-p
				    :confine-to confine-to
				    :cursor mouse-cursor)
		  (push (list window event-mask owner-p confine-to mouse-cursor)
			*inside-mouse-grab*)
		  (funcall continuation)))
	    (without-scheduling
	      (pop *inside-mouse-grab*)
	      (cond (*inside-mouse-grab*
		     (let ((inside (car *inside-mouse-grab*)))
		       (grab-the-pointer (first inside) (second inside)
					 :owner-p (third inside)
					 :confine-to (fourth inside)
					 :cursor (fifth inside))))
		    (t
		     (xlib:ungrab-pointer display)
		     (xlib:display-force-output display))))))))))

;;; If the menu has a label, make it a window-manager window.  Otherwise
;;; make it an :OVERRIDE-REDIRECT window.
(defmethod invoke-with-menu-as-popup ((menu clx-window) continuation)
  (let ((window (slot-value menu 'window))
	(label (window-label menu)))
    (cond (label
	   (let ((size-hints (xlib:wm-normal-hints window)))
	     (setf (xlib:wm-size-hints-user-specified-position-p size-hints) t)
	     (setf (xlib:wm-size-hints-user-specified-size-p size-hints) t)
	     (setf (xlib:wm-normal-hints window) size-hints))
	   (funcall continuation))
	  (t
	   (setf (xlib:window-override-redirect window) :on)
	   (unwind-protect 
	       (funcall continuation)
	     (setf (xlib:window-override-redirect window) :off))))))
