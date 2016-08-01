;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

(defvar *cloe-windows-with-deferred-events* nil)
(defvar *input-queued* nil)
(defvar *pointer-moved* nil)
(defvar *pointer-window*)
(defvar *pointer-x*)
(defvar *pointer-y*)

(defun cloe-note-pointer-motion (stream x y)
  (setf *pointer-window* stream)
  (setf *pointer-x* x)
  (setf *pointer-y* y)
  (setf *pointer-moved* t))

(defun-inline sign-extend-16 (n)
  (dpb n (byte 15 0) (- (ldb (byte 1 15) n))))

(defmethod cloe-vanilla-event ((stream cloe-window-stream) message args)
  (declare (fixnum message))
  (with-slots (update-region pending-scrolls move-pending input-buffer
	       event-left event-top event-width event-height) stream
    (cond ((= message win::wm_paint)
	   (let ((ileft (win::get-16bit args 4))
		 (itop (win::get-16bit args 6))
		 (iright (win::get-16bit args 8))
		 (ibottom (win::get-16bit args 10)))
	     (unless (or (= ileft iright) (= itop ibottom))	;seems to happen alot
	       (multiple-value-bind (dsx dsy)
		   (viewport-to-drawing-surface-coordinates stream ileft itop)
		 (push (make-bounding-rectangle
			 dsx dsy
			 (+ dsx (- iright ileft)) (+ dsy (- ibottom itop)))
		       update-region)
		 (pushnew stream *cloe-windows-with-deferred-events*)))))
	  ;; scrolling
	  ((or (= message win::wm_hscroll) (= message win::wm_vscroll))
	   (let ((type (win::get-16bit args 4))
		 (position (win::get-16bit args 6))
		 (message (cond ((= message win::wm_hscroll) :x)
				((= message win::wm_vscroll) :y))))
	     (declare (fixnum type position))
	     (multiple-value-bind (type position)
		 (cond ((= type win::sb_lineup)
			(values :relative-jump -1))
		       ((= type win::sb_linedown)
			(values :relative-jump +1))
		       ((= type win::sb_pageup)
			(values :screenful -1))
		       ((= type win::sb_pagedown)
			(values :screenful +1))
		       ((= type win::sb_thumbposition)
			(values :percentage position))
		       ((= type win::sb_top)
			(values :percentage 0))
		       ((= type win::sb_bottom)
			(values :percentage 100)))
	       (when type
		 (queue-put pending-scrolls (list message type position))
		 (pushnew stream *cloe-windows-with-deferred-events*)))))
	  ;; resizing
	  ((= message win::wm_move)
	   (setf event-left (sign-extend-16 (win::get-16bit args 6)))
	   (setf event-top (sign-extend-16 (win::get-16bit args 8)))
	   (pushnew stream *cloe-windows-with-deferred-events*)
	   (setf move-pending t))
	  ((= message win::wm_size)
	   (unless (= (the fixnum (win::get-16bit args 4)) win::sizeiconic)
	     (setf event-width (win::get-16bit args 6))
	     (setf event-height (win::get-16bit args 8))
	     (pushnew stream *cloe-windows-with-deferred-events*)
	     (setf move-pending t)))
	  ;; character typed
	  ((= message win::wm_char)
	   (let ((char (aref args 4)))
             (when (char= char #\return)
	       (setq char #\newline))
	     (queue-put input-buffer char))
	   (setf *input-queued* t))
	  ;; meta character typed
	  ((= message win::wm_syschar) ;meta key
	   (let ((char (set-char-bit 
			 (char-upcase (aref args 4)) :meta t))) 
	   (queue-put input-buffer char))
	   (setf *input-queued* t))
	  ;; button press or release
	  ((or (= message win::wm_lbuttondown)
	       (= message win::wm_rbuttondown)
	       (= message win::wm_mbuttondown)
	       (= message win::wm_lbuttonup)
	       (= message win::wm_rbuttonup)
	       (= message win::wm_mbuttonup))
	   (let ((mask (win::get-16bit args 4)) 
		 (x (win::get-16bit args 6))
		 (y (win::get-16bit args 8))
		 (pointer (stream-primary-pointer stream)))
	     (cloe-note-pointer-motion stream x y)
	     (when pointer
	       (cond ((or (= message win::wm_lbuttondown)
			  (= message win::wm_rbuttondown)
			  (= message win::wm_mbuttondown))
		      (stream-note-pointer-button-press 
			stream pointer (case message
					 (#.win::wm_lbuttondown 1)
					 (#.win::wm_mbuttondown 2)
					 (#.win::wm_rbuttondown 4))
			(windows-mask->modifier-state mask) x y)
		      (setf *input-queued* t))
		     (*generate-button-release-events*
		      (stream-note-pointer-button-release 
			stream pointer (case message
					 (#.win::wm_lbuttonup 1)
					 (#.win::wm_mbuttonup 2)
					 (#.win::wm_rbuttonup 4))
			(windows-mask->modifier-state mask) x y)
		      (setf *input-queued* t)))))))))

(defun win::vanilla-event (code length args)
  (declare (ignore length))			;because I don't know what they are!
  (let ((stream (clim-window-for-host-window
			 (win::get-16bit args 0) :error-if-no-match nil))
	(message (win::get-16bit args 2)))
    (declare (fixnum message))
    (when stream
      (cond ((= message win::wm_mousemove)
	     (let ((x (win::get-16bit args 6))
		   (y (win::get-16bit args 8)))
	       (cloe-note-pointer-motion stream x y)))
	    (t
	     (cloe-vanilla-event stream message args))))))

;;; Convert a MS Windows shift mask into a CLIM modifier-state
(defun windows-mask->modifier-state (mask)
  (let ((modifier-state 0))
    (macrolet ((do-shift (shift)
		 `(when (logtest ,(intern (format nil "~A_~A" 'mk (symbol-name shift))
					  (find-package :win))
				 mask)
		    (let ((bit (modifier-key-index ,shift)))
		      (setf modifier-state (dpb 1 (byte 1 bit) modifier-state))))))
      (do-shift :shift)
      (do-shift :control)
      ;; no meta super or hyper bits here!
      modifier-state
      )))

(defmethod set-pointer-window-and-location ((stream cloe-window-stream) pointer)
  ;;--- What should this do?
  )

(defmethod stream-event-handler ((stream cloe-window-stream)
			         &key (timeout nil) (input-wait-test nil))
  (cloe-event-handler timeout input-wait-test stream))

(defmethod stream-event-handler ((stream cloe-root-window)
			         &key (timeout nil) (input-wait-test nil))
  (cloe-event-handler timeout input-wait-test stream))

(defun cloe-event-handler (timeout input-wait-test original-stream)
  (cloe-process-all-deferred-events)
  (let ((end-time (and timeout (+ (get-internal-real-time)
				  (* internal-time-units-per-second timeout))))
	;; If we're inside an encapsulating stream, such as the input editor, and a window
	;; resizing event is received, don't get confused while handling the event
	(*original-stream* nil))
    (loop
      ;; The assumption is that the input-wait-test function can only change
      ;; its value when an event is received and processed.  The only
      ;; commonly-desired exception is a timeout, which we provide directly.
      (when (and input-wait-test (funcall input-wait-test original-stream))
	(return-from cloe-event-handler :input-wait-test))
      (win::await-response -1 t t)
      ;; we do this here, rather than in vanilla event
      ;; to provide for better synchronization.
      (cloe-process-all-deferred-events)
      (when *pointer-moved*
	(setf *pointer-moved* nil)
	(let ((window *pointer-window*))
	  (setf (pointer-window *cloe-pointer*) window)
	  (when window
	    (multiple-value-bind (wx wy)
		(window-offset window)
	      (pointer-set-position* *cloe-pointer* (+ *pointer-x* wx) (+ *pointer-y* wy)))
	    (setf (pointer-motion-pending window *cloe-pointer*) t)))
	(return :mouse-motion))
      (when *input-queued*
	(setf *input-queued* nil)
	(return :input-buffer))
      (when (and end-time (> (get-internal-real-time) end-time))
	(return :timeout)))))

;;; Called after paint events have likely been generated.  First, make sure
;;; they get read from the WINFE side.  Then, process all queued paint events
;;; now.
(defun handle-generated-paints () 
  (win::await-response -1 nil nil)
  (cloe-process-all-deferred-events))

(defmethod set-stream-pointer-in-screen-coordinates ((stream cloe-window-stream)
						     pointer x y)
  (declare (ignore pointer))
  (win::set-pointer-position x y))

(defmethod stream-pointer-input-rectangle*
    ((stream cloe-window-stream)
     pointer &key left top right bottom)
 (declare (ignore left top right bottom))
 (portable-pointer-input-rectangle* stream pointer))

(defun cloe-process-all-deferred-events ()
  (loop
    (when (null *cloe-windows-with-deferred-events*) (return))
    (cloe-process-deferred-events (pop *cloe-windows-with-deferred-events*))))

(defmethod cloe-process-deferred-events ((stream cloe-window-stream))
  (with-slots (window left top right bottom margin-width margin-height
  		      event-left event-top event-width event-height
		      move-pending update-region pending-scrolls) stream
    (declare (fixnum left top right bottom event-left event-top event-width event-height))
    (when move-pending
      (setf move-pending nil)
      (unless (win::is-iconic window)
	(multiple-value-bind (new-left new-top new-right new-bottom)
	    (multiple-value-bind (width height)
		(multiple-value-bind (wleft wtop wright wbottom)
		    (win::get-window-rectangle window)
		  (declare (fixnum wleft wtop wright wbottom))
		  (values (- wright wleft) (- wbottom wtop)))
	      (declare (fixnum width height))
	      (setf margin-width (- width event-width))
	      (setf margin-height (- height event-height))
	      (values event-left event-top (+ event-left width) (+ event-top height)))
	  (declare (fixnum new-left new-top new-right new-bottom))
	  (unless (and (= left new-left) (= top new-top)
		       (= right new-right) (= bottom new-bottom))
	    (window-note-size-or-position-change
	      stream new-left new-top new-right new-bottom)))))
    (loop
      (let ((entry (queue-get pending-scrolls)))
	(when (null entry) (return))
	(apply #'cloe-process-deferred-scroll stream entry)))
    (when update-region
      (window-process-update-region stream))
    nil))

(defmethod cloe-process-deferred-scroll ((stream cloe-window-stream) message type position)
  (multiple-value-bind (new-left new-top)
      (with-bounding-rectangle* (vleft vtop vright vbottom) (window-viewport stream)
	(multiple-value-bind (hleft htop hright hbottom)
	    (let ((history (and (output-recording-stream-p stream)
				(stream-output-history stream))))
	      (if history
		  (bounding-rectangle* history)
		  (values 0 0 1000 100)))
	  (ecase message
	    (:y
	      (values vleft
		      (min (max htop
				(ecase type
				  (:relative-jump
				    (+ vtop (* position (stream-line-height stream))))
				  (:screenful
				    (+ vtop (* position (- vbottom vtop))))
				  (:percentage
				    (+ htop
				       (round (* position (- (- hbottom htop) (- vbottom vtop)))
					      100)))))
			   hbottom)))
	    (:x
	      (values (min (max hleft
				(ecase type
				  (:relative-jump
				    (+ vleft
				       (* position (stream-character-width stream #\space))))
				  (:screenful
				    (+ vleft (* position (- vright vleft))))
				  (:percentage
				    (+ hleft
				       (round (* position (- (- hright hleft) (- vright vleft)))
					      100)))))
			   hright)
		      vtop))
	    )))
    (window-set-viewport-position* stream new-left new-top)))
