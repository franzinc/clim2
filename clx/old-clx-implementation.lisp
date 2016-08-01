;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

;; See the file LICENSE for the full license governing this code.
;;

;;--- (1) Why does the new CLX port flush WHITE- and BLACK-PIXEL?
;;--- (2) What to do about reparenting?
(defclass clx-window
	  (window-stream)
    ((white-pixel)
     (black-pixel)
     (mapped-p :initform nil :reader window-visibility)
     (reparented-p :initform nil)))	; true when wm reparents us

;;--- New CLX port needs to handle this reparenting stuff
(defmethod stream-event-handler ((stream clx-root-window)
				 &key (timeout nil)
				      (input-wait-test nil)
				      (original-stream stream))
  ;; No hang-p argument, use :timeout 0.
  (loop
    ;(xlib:display-force-output display)
    ;; The idea below is that EVENT-COND will return NIL on timeout,
    ;; return T for "boring" events, and will call (RETURN :foo) for
    ;; input events, which will return from the loop above.
    (if (xlib:event-cond (display :discard-p t :timeout timeout)
	  ((:reparent-notify) (window parent x y)
	   (setq ws (getf (xlib:window-plist window) 'stream))	;test form
	   ;; We're interested in the relationship between our window and our
	   ;; parent, so if that's what this event is about, take notice.  If
	   ;; the event is about any other windows (e.g. intermediaries inserted
	   ;; by the window manger), then it's talking about something beyond
	   ;; our sphere of influence and we ignore it.
	   (with-slots (left top right bottom) ws
	     (cond ((eql parent (clx-stream-window (window-parent ws)))
		    (unless (and (= x left) (= y top))
		      (window-note-size-or-position-change
			ws x y
			(+ x (- right left)) (+ y (- bottom top))))
		    (setf (slot-value ws 'reparented-p) nil))
		   (t
		    (multiple-value-setq (x y)
		      (xlib:translate-coordinates
			window 0 0 (clx-stream-window (window-parent ws))))
		    (window-note-size-or-position-change
		      ws x y
		      (+ x (- right left)) (+ y (- bottom top)))
		    (setf (slot-value ws 'reparented-p) t))))
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
			window 0 0 (clx-stream-window (window-parent ws)))))))
	   (window-note-size-or-position-change
	     ws (- x border-width) (- y border-width)
	     (+ x width border-width) (+ y height border-width))
	   (setf (slot-value ws 'border-width) border-width)
	   t))
	;; If we got a non-input event, just run the loop again, else
	;; it is a timeout
	nil
	(return :timeout))))

;;--- Whatever became of this??
(defmethod with-clipping-region-1
	   ((stream clx-window) (region bounding-rectangle) continuation)
  (with-slots (window clip-mask) stream
    (multiple-value-bind (x y) (window-viewport-position* stream)
      (declare (fixnum x y))
      (multiple-value-bind (ml mt) (window-margins stream)
	(declare (fixnum ml mt))
      	(with-bounding-rectangle* (left top right bottom) region
	  (translate-fixnum-positions (the fixnum (- ml x)) (the fixnum (- mt y))
	    left top right bottom)
	  (fix-points left top right bottom)
	  ;;--- How not to cons? (this list and the rectangles)
	  (let ((old-clip-mask clip-mask))
	    (unwind-protect
		(progn
		  (setf clip-mask (list left top
					(the fixnum (- right left))
					(the fixnum (- bottom top))))
		  (funcall continuation stream))
	      (setf clip-mask old-clip-mask))))))))


;;--- New CLX port needs to hack these "menu windows" when the :MENU-FRAME
;;--- frame property is set.  Hack REALIZE-MIRROR appropriately.

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
;;; Also see WITH-MENU-AS-POPUP-INTERNAL below.
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
(defmethod with-mouse-grabbed-in-window-internal
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
(defmethod with-menu-as-popup-internal ((menu clx-window) continuation)
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

