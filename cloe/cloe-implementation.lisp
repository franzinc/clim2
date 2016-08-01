;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


;;; Support for the shared class DC.

(defclass cloe-window-stream
	  (window-stream)
    ((window :initarg :window)
     (move-pending :initform nil)
     (event-width) (event-height)
     (event-left) (event-top)
     (margin-width) (margin-height)
     (h-scroll-pos :initform nil)
     (v-scroll-pos :initform nil)
     (pending-scrolls :initform (make-queue))
     (background-dc-image)
     (foreground-dc-image)
     (modifier-state :initform 0 :reader window-modifier-state)))

(defmethod initialize-instance :before ((stream cloe-window-stream) &key parent)
  (setf (slot-value stream 'display-device-type)
	(slot-value parent 'display-device-type)))

(defmethod initialize-instance :after ((stream cloe-window-stream) &key parent
				       label (scroll-bars :both) (borders t) save-under)
  (with-slots (window left top right bottom margin-width margin-height
		      event-left event-top event-width event-height
		      foreground-dc-image background-dc-image)
	      stream
    (let ((top-level (not (slot-exists-p parent 'window))))
      (setf window (win::create-window
		     "Vanilla" (or label "CLIM")
		     (logior  win::ws_clipchildren	;new, helps refresh
			      (if top-level
				  (if save-under
				      (logior win::ws_popup
					      win::ws_border)
				      (logior win::ws_overlapped
					      win::ws_caption
					      win::ws_thickframe
					      win::ws_sysmenu
					      win::ws_minimizebox
					      win::ws_maximizebox))
				  (logior win::ws_child
					  win::ws_clipsiblings
					  (if borders win::ws_border 0)))
			      (if (member scroll-bars '(:horizontal :both))
			          (progn (setf h-scroll-pos 0)
				  	 win::ws_hscroll)
				  0)
			      (if (member scroll-bars '(:vertical :both))
			          (progn (setf v-scroll-pos 0)
				  	 win::ws_vscroll)
				  0)
			      )
		     left top (- right left) (- bottom top)
		     (if top-level 0 (slot-value parent 'window)) 0 0 "arg")))
    (associate-clim-window-with-host-window window stream)
    (setf foreground-dc-image (dc-image-for-ink stream (medium-foreground stream)))
    (setf background-dc-image (dc-image-for-ink stream (medium-background stream)))
    (multiple-value-bind (cleft ctop cright cbottom)
	(win::get-client-rectangle window)
      (multiple-value-bind (wleft wtop wright wbottom)
	  (win::get-window-rectangle window)
	(setf margin-width (- (- wright wleft) (- cright cleft)))
	(setf margin-height (- (- wbottom wtop) (- cbottom ctop))))
      (setf event-left (+ left cleft))
      (setf event-top (+ top ctop))
      (setf event-width (- cright cleft))
      (setf event-height (- cbottom ctop))
      (setf right (+ left event-width margin-width))
      (setf bottom (+ top event-height margin-height)))
    nil))

(defmethod implementation-pixels-per-point ((stream cloe-window-stream))
  1)

(defmethod close ((stream cloe-window-stream) &key abort)
  (declare (ignore abort))
  (with-slots (window) stream
    (win::destroy-window window)))

(defmethod (setf window-visibility) :after (visibility (stream cloe-window-stream))
  (labels ((window-set-visibility-1 (stream visibility)
	     (win::show-window (slot-value stream 'window)
			       :type (if visibility :show-no-activate :hide))
	     (dolist (child (window-children stream))
	       (when (window-visibility child)
		 (window-set-visibility-1 child visibility)))))
    (window-set-visibility-1 stream visibility))
  (handle-generated-paints)
  visibility)

(defmethod wait-for-window-exposed ((window cloe-window-stream))
  (loop
    (when (window-visibility window)
      (return))
    (process-yield)))

(defmethod window-stack-on-top ((stream cloe-window-stream))
  (with-slots (window) stream
    (win::set-window-position window 0 0 0 0 0
			      (logior win::swp_noactivate
				      win::swp_nomove
				      win::swp_nosize)))
  (handle-generated-paints))

(defmethod window-stack-on-bottom ((stream cloe-window-stream))
  (with-slots (window) stream
    (win::set-window-position window 1 0 0 0 0
			      (logior win::swp_noactivate
				      win::swp_nomove
				      win::swp_nosize)))
  (handle-generated-paints))

(defmethod bounding-rectangle-set-edges :after
	   ((stream cloe-window-stream) left top right bottom)
  (with-slots (window) stream
    (win::move-window window left top (- right left) (- bottom top) t))
  (handle-generated-paints))
  
(defmethod bounding-rectangle-set-position* :after
	   ((stream cloe-window-stream) new-left new-top)
  (multiple-value-bind (width height) (bounding-rectangle-size stream)
    (with-slots (window) stream
      (win::move-window window new-left new-top width height t)))
  (handle-generated-paints))

(defmethod bounding-rectangle-set-size :after
	   ((stream cloe-window-stream) width height)
  (multiple-value-bind (left top) (bounding-rectangle-position* stream)
    (with-slots (window) stream
      (win::move-window window left top width height t)))
  (handle-generated-paints))

;;;changed to fix the scrolling problem
(defmethod window-process-update-region ((stream cloe-window-stream))
  (with-slots (window update-region background-dc-image) stream
    (when update-region
      (set-dc-for-filling background-dc-image)
      (multiple-value-bind (dx dy)
	  (window-viewport-position* stream)
	(declare (type coordinate dx dy))
	(dolist (region update-region)
	  (with-bounding-rectangle* (left top right bottom) region
	    (win::rectangle window (- left dx) (- top dy)
				   (- right dx) (- bottom dy))))))))

;;; Postpone better update region stuff.
(defmethod window-shift-visible-region :after ((stream cloe-window-stream)
					       old-left old-top old-right old-bottom
					       new-left new-top new-right new-bottom)

  (setf (slot-value stream 'update-region)
	(ltrb-difference new-left new-top new-right new-bottom
			 old-left old-top old-right old-bottom)))

;; Does this want to be an :after method?  Will the primary method on
;; window-stream ever want to do anything else?
(defmethod stream-set-input-focus ((stream cloe-window-stream))
  (with-slots (window) stream
    (win::set-focus window)))

(defmethod stream-restore-input-focus ((stream cloe-window-stream) old-focus)
  (declare (ignore stream))
  (win::set-focus old-focus))

(defmethod stream-has-input-focus ((stream cloe-window-stream))
  (with-slots (window) stream
    (= window  (win::get-focus))))

(defmethod notify-user-1 ((stream cloe-window-stream) frame format-string &rest format-args)
  (declare (ignore frame) (dynamic-extent format-args))
  (let ((formatted-string (apply #'format nil format-string format-args)))
    (clos:with-slots (window) stream
      (win::notify-user window formatted-string))))

;;; Scrolling support.
(defmethod cloe-recompute-scroll-bars ((stream cloe-window-stream))
  ;; don't update the scroll bars if they didn't change.
  (multiple-value-bind (new-h-pos new-v-pos)
      (let ((history (and (output-recording-stream-p stream)
			  (stream-output-history stream))))
	(when history
	  (multiple-value-bind (width height) (bounding-rectangle-size history)
	    (with-bounding-rectangle* (vleft vtop vright vbottom) (window-viewport stream)
	      (values (and (< (- vright vleft) width)
			   (round (* vleft 100)
				  (- width (- vright vleft))))
		      (and (< (- vbottom vtop) height)
			   (round (* vtop 100)
				  (- height (- vbottom vtop)))))))))
    (with-slots (window h-scroll-pos v-scroll-pos) stream
      (unless (eql new-h-pos h-scroll-pos)
	(cond ((null new-h-pos)
	       ;;(win::show-scroll-bar window win::sb_horz nil)
	       (win::set-scroll-position window win::sb_horz new-h-pos))
	      (t
	       (win::set-scroll-position window win::sb_horz new-h-pos)
	       #||(when (null h-scroll-pos)
		 (win::show-scroll-bar window win::sb_horz t))||#))
	(setf h-scroll-pos new-h-pos))
      (unless (eql new-v-pos v-scroll-pos)
	(cond ((null new-v-pos)
	       ;;(win::show-scroll-bar window win::sb_vert nil)
	       (win::set-scroll-position window win::sb_vert new-v-pos))
	      (t
	       (win::set-scroll-position window win::sb_vert new-v-pos)
	       #||(when (null v-scroll-pos)
		 (win::show-scroll-bar window win::sb_vert t))||#))
	(setf v-scroll-pos new-v-pos)))))

(defmethod window-set-viewport-position* :after ((stream cloe-window-stream) new-x new-y)
  (cloe-recompute-scroll-bars stream))
