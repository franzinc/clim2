;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: old-clx-implementation.lisp,v 1.1 92/02/24 13:21:58 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 Franz Inc.  All rights reserved."

(defclass clx-display-device (display-device)
    ((display :initarg :display)
     (screen :initarg :screen))
  (:default-initargs :allow-loose-text-style-size-mapping t
		     :mapping-table (make-hash-table :test 'equal)))

(defvar *window-manager-border-size* 2)

(defvar *clx-white-color* (xlib:make-color :red 1 :blue 1 :green 1))
(defvar *clx-black-color* (xlib:make-color :red 0 :blue 0 :green 0))
(defvar *cursor-font* nil)


(defclass clx-root-window
	  (window-stream)
    ((display)
     (screen :reader clx-stream-screen)
     (window :reader clx-stream-window)
     (modifier-state :initform (make-modifier-state)
		     :reader window-modifier-state)
     ;; Used to create "color" stipples on monochrome screens
     (stipple-gc)
     (default-grab-cursor :reader default-grab-cursor))
  (:default-initargs :input-buffer nil))

(defun disassemble-display-spec (display &optional (default-display 0) (default-screen 0))
  (declare (values host display-number nscreen))
  (let ((host-n (position #\: display)))
    (unless host-n
      (return-from disassemble-display-spec 
	(values display default-display default-screen)))
    (let* ((host (subseq display 0 host-n))
	   (screen-n (position #\. display :start (1+ host-n)))
	   (display-number (if screen-n
			       (parse-integer display :start (1+ host-n) :end screen-n
					      :junk-allowed t)
			       (parse-integer display :start (1+ host-n)
					      :junk-allowed t)))
	   (nscreen (if screen-n
			(parse-integer display :start (1+ screen-n)
				       :junk-allowed t)
		        default-screen)))
      (values host display-number nscreen))))

;; Code to hack X Authority mechanism
#+Allegro
(defmethod open-display-with-auth (host &key (display-number 0)
					     (xauthority-filename
					       (or (sys:getenv "XAUTHORITY")
						   "~/.Xauthority")))
  (do ((try-count 0 (1+ try-count))
       (auth-data (xau-get-auth-by-addr
		    host display-number xauthority-filename))
       no-error-p display)
      ((or no-error-p (eql try-count 3))
       (and no-error-p display))
    (multiple-value-setq (no-error-p display)
      (if (eql try-count 2)
	  ;; Let error be signalled last time through.
	  (xlib:open-display
	    host :display display-number
	    :authorization-name (first auth-data)
	    :authorization-data (second auth-data))
	  (excl:errorset (xlib:open-display
			   host :display display-number
			   :authorization-name (first auth-data)
			   :authorization-data (second auth-data)))))))

;; See the man page for "Xau" for the format of the authorization
;; file read here.  This function emulates the C function
;; XauGetAuthByAddr.  Perhaps I should've used a foreigh function
;; interface to that, but would need to return two values.   --- cheetham
#+Allegro
(defmethod xau-get-auth-by-addr (host &optional (display-number 0) filename)
  (unless filename
    (setq filename (or (sys:getenv "XAUTHORITY") "~/.Xauthority")))
  (if (probe-file filename)
      (with-open-file (s filename :element-type 'unsigned-byte)
	;; Read each record of the user's .Xauthority file (or other file).
	(do ((first (read-byte s nil nil)
		    (read-byte s nil nil))
	     host1 display-number1 name data
	     host-length display-number-length name-length data-length)
	    ((null first) nil)
	  ;; The first short integer is the "family" field which we
	  ;; ignore (what's it for?), so skip by the second byte of it.
	  (read-byte s)
	  ;; Read the server name.
	  (setq host-length (read-short s))
	  (setq host1 (make-string host-length))
	  (dotimes (j host-length)
	    (setf (aref host1 j) (code-char (read-byte s))))
	  ;; Read the display number.
	  (setq display-number-length (read-short s))
	  (setq display-number1 (make-string display-number-length))
	  (dotimes (j display-number-length)
	    (setf (aref display-number1 j) (code-char (read-byte s))))
	  ;; Read the name of the encoding protocol ("MIT-MAGIC-COOKIE-1")
	  (setq name-length (read-short s))
	  (setq name (make-string name-length))
	  (dotimes (j name-length)
	    (setf (aref name j) (code-char (read-byte s))))
	  ;; Read the actual authorization code.
	  (setq data-length (read-short s))
	  (setq data (make-string data-length))
	  (dotimes (j data-length)
	    (setf (aref data j) (coerce (read-byte s) 'character)))
	  ;; If this is the user's record for the requested server and
	  ;; display, then return the data for this record.
	  (when (and (string= host host1)
		     (= display-number
			(multiple-value-bind (no-error-p value)
			    (excl:errorset
			      (read-from-string display-number1))
			  (if no-error-p
			      value
			      -1))))
	    (return (list name data)))))
      (progn
	(unless (string= filename "~/.Xauthority")
	  (warn "X authorization file ~a doesn't exist." filename))
	nil)))

#+Allegro
(defun read-short (stream)
  #+big-endian
  (+ (* 256 (read-byte stream)) (read-byte stream))
  #+little-endian
  (+ (read-byte stream) (* 256 (read-byte stream))))

(defmethod initialize-instance :before
	   ((stream clx-root-window)
	    &key (host #+Genera  (scl:send neti:*local-host* :name)
		       #+Minima  (machine-instance)
		       #+Allegro (or (system:getenv "DISPLAY") (short-site-name))
		       #+Lucid   (or (lcl:environment-variable "DISPLAY")
				     (machine-instance)))
		 ((:display display-number) 0)
		 ((:screen nscreen) 0))
  (multiple-value-bind (host display-number nscreen)
      (disassemble-display-spec host display-number nscreen)
    (with-slots (display screen window display-device-type
		 left top right bottom stipple-gc default-grab-cursor) stream
      (setf display #-Allegro (xlib:open-display host :display display-number)
		    #+Allegro (open-display-with-auth host :display-number display-number))
      (fill-keycode->modifier-state display)
      (fill-clim-modifier-key-index->x-state display)
      (setf screen (nth nscreen (xlib:display-roots display)))
      (setf window (xlib:screen-root screen))
      (setf left 0)
      (setf top 0)
      (setf right (xlib:screen-width screen))
      (setf bottom (xlib:screen-height screen))
      (setf display-device-type
	    (make-instance 'clx-display-device
	      :display display :screen screen
	      :name (format nil "~A" display)))
      (setf stipple-gc (xlib:create-gcontext
			 :drawable window
			 :foreground (xlib:screen-black-pixel screen)
			 :background (xlib:screen-white-pixel screen)))
      (setf default-grab-cursor
	    (xlib:create-glyph-cursor :source-font *cursor-font*
				      :mask-font *cursor-font*
				      ;; Northeast arrow
				      :source-char 2
				      :mask-char 3
				      :foreground *clx-black-color*
				      :background *clx-white-color*)))
    (setf (stream-pointers stream)
	  (list (setf (stream-primary-pointer stream)
		      (make-instance 'standard-pointer :root stream))))))

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

;;; Convert an X keycode into a CLIM modifier state that can be IORed with
;;; another modifier state.
;;; We'll just use an array that's DISPLAY-MAX-KEYCODEs long and index each keycode
;;; to a modifier state.
(defun keycode->modifier-state (keycode display)
  (let ((array (getf (xlib:display-plist display) 'keycode->modifier-state)))
    (aref array keycode)))

(defun fill-keycode->modifier-state (display)
  (multiple-value-bind (shift lock control mod1 mod2 mod3 mod4 mod5)
      (xlib:modifier-mapping display)
    (declare (ignore lock))
    (declare (ignore mod4 mod5))
    (let ((array (getf (xlib:display-plist display) 'keycode->modifier-state))
	  (n-keycodes (1+ (xlib:display-max-keycode display))))
      (cond ((or (null array)
		 (< (length array)
		    n-keycodes))
	     (setq array (make-array n-keycodes :element-type '(unsigned-byte 8)
						:initial-element 0))
	     (setf (getf (xlib:display-plist display) 'keycode->modifier-state)
		   array))
	    (t (fill array 0)))
      (flet ((x-name->clim-name (name)
	       ;;; for now, 1 = meta, 2 = super and 3 = hyper.
	       (ecase name
		 ((:shift :control) name)
		 (:mod1 :meta)
		 (:mod2 :super)
		 (:mod3 :hyper))))
	(macrolet ((do-shift (x-name)
		     `(let* ((clim-name (x-name->clim-name
					  ,(intern (symbol-name x-name) *keyword-package*)))
			     (modifier-state (if clim-name
						 (make-modifier-state clim-name)
						 0)))
			(dolist (code ,x-name)
			  (setf (aref array code) modifier-state)))))
	  (do-shift shift)
	  (do-shift control)
	  (do-shift mod1)
	  (do-shift mod2)
	  (do-shift mod3))))))

(defun fill-clim-modifier-key-index->x-state (display)
  (multiple-value-bind (shift lock control mod-1 mod-2 mod-3 mod-4 mod-5)
      (xlib:modifier-mapping display)
    (declare (ignore shift lock control))
    (let ((array (getf (xlib:display-plist display) 'clim-modifier-key-index->x-state))
	  (length (length *modifier-keys*)))
      (cond ((or (null array)
		 (< (length array) length))
	     (setq array (make-array length :element-type '(unsigned-byte 8)
					    :initial-element 0))
	     (setf (getf (xlib:display-plist display) 'clim-modifier-key-index->x-state)
		   array))
	    (t (fill array 0)))
      ;; Maybe we can speed this up by special casing :SHIFT and :CONTROL
      ;; somewhere else.
      (setf (aref array (modifier-key-index :shift)) (xlib:make-state-mask :shift))
      (setf (aref array (modifier-key-index :control)) (xlib:make-state-mask :control))
      (flet ((test-keysym (keysym state-mask)
	       (cond ((or (= keysym xlib::left-meta-keysym)
			  (= keysym xlib::left-alt-keysym)
			  (= keysym xlib::right-meta-keysym)
			  (= keysym xlib::right-alt-keysym))
		      (setf (aref array (modifier-key-index :meta)) state-mask))
		     ((or (= keysym xlib::left-super-keysym)
			  (= keysym xlib::right-super-keysym))
		      (setf (aref array (modifier-key-index :super)) state-mask))
		     ((or (= keysym xlib::left-hyper-keysym)
			  (= keysym xlib::right-hyper-keysym))
		      (setf (aref array (modifier-key-index :hyper)) state-mask)))))
	(macrolet ((do-mod (mod)
		     `(let ((codes ,mod))
			(dolist (keycode codes)
			  (let ((keysym (xlib:keycode->keysym display keycode 0)))
			    (test-keysym
			      keysym (xlib:make-state-mask
				       ,(intern (symbol-name mod) *keyword-package*))))))))
	  (do-mod mod-1)
	  (do-mod mod-2)
	  (do-mod mod-3)
	  (do-mod mod-4)
	  (do-mod mod-5))))))

;;; Don't want to have to always go through the plist.
;;; We can store this info in slots in the root window
;;; and pass the arrays in as args.
(defun clim-modifier-key-index->x-state (modifier-key-index display)
  (let ((array (getf (xlib:display-plist display)
		     'clim-modifier-key-index->x-state)))
    (aref array modifier-key-index)))

(defun state-mask->modifier-state (state-mask display)
  (let ((mask 0))
    (macrolet ((do-shift (clim-shift)
		 `(let* ((bit (modifier-key-index ,clim-shift))
			 (x-state (clim-modifier-key-index->x-state bit display)))
		    (unless (zerop (boole boole-and x-state state-mask))
		      (setf mask (dpb 1 (byte 1 bit) mask))))))
      (do-shift :shift)
      (do-shift :control)
      (do-shift :meta)
      (do-shift :super)
      (do-shift :hyper))
    mask))

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
     (color-p :initform nil)
     ;; These two are used on monochrome screens
     (white-pixel)
     (black-pixel)
     (copy-gc)
     (foreground-gc)
     (foreground-pixel)
     (background-gc)
     (background-pixel)
     (flipping-gc)
     (ink-table :initform (make-hash-table :test #'equal))
     (clip-mask :initform :none)
     (reparented-p :initform nil)	; true when wm reparents us
     (nuked-p :initform nil)		; true when wm nukes us
     (points-to-pixels)
     (vertical-scroll-bar :initform nil :reader window-vertical-scroll-bar)
     (horizontal-scroll-bar :initform nil :reader window-horizontal-scroll-bar)))

(defparameter *clx-use-color* t)	;for debugging monochrome...
(defmethod color-stream-p ((stream clx-window))
  (and *clx-use-color*
       (> (xlib:screen-root-depth (clx-stream-screen stream)) 2)))

(defparameter *clx-scrollbar-border-width* 0)
(defparameter *clx-scrollbar-width* 14)

;;--- Scrollbars have an inner margin
;;--- Perhaps we should use window-margin!
(defvar *clx-scrollbar-inner-margin* 2)

(defclass clx-scrollbar (clx-window) 
    ((scroll-bar-values-1 :initform nil
			  :accessor scroll-bar-values-1)
     (scroll-bar-values-2 :initform nil
			  :accessor scroll-bar-values-2)))

(defclass clx-h-scrollbar (clx-scrollbar) ())
(defclass clx-v-scrollbar (clx-scrollbar) ())

(defmethod initialize-instance :before ((stream clx-window) &key parent)
  (setf (slot-value stream 'display-device-type)
	(slot-value parent 'display-device-type)))  

(defmethod initialize-instance :after
	   ((stream clx-window) &key left top right bottom
				     (scroll-bars :both) save-under label
				     (borders t) border-width)
  (with-slots (window root screen copy-gc points-to-pixels
	       color-p black-pixel white-pixel) stream
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
	     (if (and top-level (not border-width))
		 (setq border-width 0)
		 (when (not border-width)
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
    (setf (getf (xlib:window-plist window) 'stream) stream))
  (when scroll-bars
    ;; Adjust right and bottom for the border of the window
    (multiple-value-bind (lom tom rom bom) (host-window-margins stream)
      (declare (fixnum rom bom) (ignore lom tom))
      (decf right rom)
      (decf bottom bom))
    (with-slots (horizontal-scroll-bar vertical-scroll-bar) stream
      (let ((hp (member scroll-bars '(:both :horizontal)))
	    (vp (member scroll-bars '(:both :vertical))))
	(when hp
	  (setf horizontal-scroll-bar 
		(open-window-stream :parent stream 
				    :window-class 'clx-h-scrollbar
				    :borders nil
				    :left (if vp *clx-scrollbar-width* 0) 
				    :right right
				    :top (- bottom top *clx-scrollbar-width*) 
				    :bottom (- bottom top)
				    :scroll-bars nil))
	  (setf (stream-recording-p horizontal-scroll-bar) nil)
	  (window-expose horizontal-scroll-bar))
	(when vp
	  (setf vertical-scroll-bar
		(open-window-stream :parent stream 
				    :borders nil
				    :window-class 'clx-v-scrollbar
				    :left 0 
				    :right *clx-scrollbar-width*
				    :top 0
				    :bottom (- bottom top (if hp *clx-scrollbar-width* 0))
				    :scroll-bars nil))
	  (setf (stream-recording-p vertical-scroll-bar) nil)
	  (window-expose vertical-scroll-bar))))))


;;; Scroll bars

(defmethod stream-replay ((stream clx-scrollbar) &optional region)
  (declare (ignore region))
  (draw-scrollbar stream t))

(defmethod stream-note-pointer-button-press ((bar clx-v-scrollbar)
					     pointer button modifier-state x y)
  (declare (ignore pointer modifier-state x))
  (let* ((stream (window-parent bar))
	 (history (stream-output-history stream)))
    (when history
      (with-bounding-rectangle* (hleft htop hright hbottom) history
	(declare (ignore hleft hright))
	(with-bounding-rectangle* (v-left v-top v-right v-bottom) (window-viewport stream)
	  (declare (ignore v-right))
	  (window-set-viewport-position* stream 
	    v-left
	    (compute-scroll-to-value
	      y (bounding-rectangle-width bar) (bounding-rectangle-height bar)
	      htop hbottom v-top v-bottom button))
	  (draw-scrollbar bar))))))

(defmethod stream-note-pointer-button-press ((bar clx-h-scrollbar)
					     pointer button modifier-state x y)
  (declare (ignore pointer modifier-state y))
  (let* ((stream (window-parent bar))
	 (history (stream-output-history stream)))
    (when history
      (with-bounding-rectangle* (hleft htop hright hbottom) history
	(declare (ignore htop hbottom))
	(with-bounding-rectangle* (v-left v-top v-right v-bottom) (window-viewport stream)
	  (declare (ignore v-bottom))
	  (window-set-viewport-position* stream 
	    (compute-scroll-to-value
	      x (bounding-rectangle-height bar) (bounding-rectangle-width bar)
	      hleft hright v-left v-right button)
	    v-top)
	  (draw-scrollbar bar))))))

(defun compute-scroll-to-value (y bar-width bar-height
				history-top history-bottom viewport-top viewport-bottom
				button)
  (cond 
   ;; top
   ((if (= button 1) (< y bar-width) (> y (- bar-height bar-width)))
    (max history-top 
	 (- viewport-top (truncate (* 0.9 (- viewport-bottom viewport-top))))))
   ;; bottom
   ((if (= button 1) (> y (- bar-height bar-width)) (< y bar-width))
    (min (- history-bottom (- viewport-bottom viewport-top))
	 (+ viewport-top (truncate (* 0.9 (- viewport-bottom viewport-top))))))
   ;; elsewhere
   (t
    (let* ((ratio (float (/ (max 0 (- y bar-width)) (- bar-height (* 2 bar-width))))))
      (min (- history-bottom (- viewport-bottom viewport-top))
	   (truncate (+ history-top (* ratio (- history-bottom history-top)))))))))

(defmacro with-scroll-bar-update (&body body)
  `(unless (and (not force-p)
		(eql slug-start (scroll-bar-values-1 stream))
		(eql slug-length (scroll-bar-values-2 stream)))
     (let ((old-slug-start (scroll-bar-values-1 stream))
	   (old-slug-length (scroll-bar-values-2 stream)))
       (setf  (scroll-bar-values-1 stream) slug-start
	      (scroll-bar-values-2 stream) slug-length)
       ,@body)))

(defmethod draw-scrollbar ((stream clx-v-scrollbar) &optional force-p)
  (with-bounding-rectangle* (left top right bottom) stream
    (let ((width (- right left))
	  (height (- bottom top))
	  (win (window-parent stream)))
      (with-bounding-rectangle* (extent-left extent-top extent-right extent-bottom)
				(stream-output-history win)
	(declare (ignore extent-left extent-right))
	(with-bounding-rectangle* (viewport-left viewport-top viewport-right viewport-bottom)
				  (window-viewport win)
	  (declare (ignore viewport-left viewport-right))
	  (multiple-value-bind (y1 y2 y3 y4 x1 x2 slug-start slug-length)
	      (compute-scrollbar-values
		height width *clx-scrollbar-inner-margin*
		extent-top extent-bottom viewport-top viewport-bottom)
	    (with-scroll-bar-update 
	      (if (or force-p (null old-slug-start))
		  (progn
		    (draw-rectangle* stream
		      0 0
		      (bounding-rectangle-width stream) (bounding-rectangle-height stream)
		      :ink +background-ink+)
		    ;; The -1 adjusts for X unfilled rectangles being one pixel too big.  jdi
		    (draw-rectangle* stream x1 y1 (1- x2) (1- y2) :filled nil :ink +black+)
		    (draw-rectangle* stream x1 y3 (1- x2) (1- y4) :filled nil :ink +black+)
		    (draw-rectangle* stream x1 y2 (1- x2) (1- y3) :filled nil :ink +black+))
		  ;; Clear the slug
		  (draw-rectangle* stream 
		    (1+ x1) (min slug-start old-slug-start)
		    (1- x2) (max (+ slug-start slug-length) (+ old-slug-start old-slug-length))
		    :ink +background-ink+))
	      ;; Redraw the slug
	      (draw-rectangle* stream 
		(1+ x1) slug-start (1- x2) (+ slug-start slug-length)
		:ink +light-gray+))))))))

(defmethod draw-scrollbar ((stream clx-h-scrollbar) &optional force-p)
  (with-bounding-rectangle* (left top right bottom) stream
    ;; The -1 adjusts for X unfilled rectangles being one pixel too big.  jdi
    (let ((width (- right left 1))
	  (height (- bottom top 1))
	  (win (window-parent stream)))
      (with-bounding-rectangle* (extent-left extent-top extent-right extent-bottom)
				(stream-output-history win)
	(declare (ignore extent-top extent-bottom))
	(with-bounding-rectangle* (viewport-left viewport-top viewport-right viewport-bottom)
				  (window-viewport win)
	  (declare (ignore viewport-top viewport-bottom))
	  (multiple-value-bind (x1 x2 x3 x4 y1 y2 slug-start slug-length)
	      (compute-scrollbar-values
		width height *clx-scrollbar-inner-margin*
		extent-left extent-right viewport-left viewport-right)
	    (with-scroll-bar-update 
	      (if (or force-p (null old-slug-start))
		  (progn
		    (draw-rectangle* stream
		      0 0
		      (bounding-rectangle-width stream) (bounding-rectangle-height stream)
		      :ink +background-ink+)
		    ;; The -1 adjusts for X unfilled rectangles being one pixel too big.  jdi
		    (draw-rectangle* stream x1 y1 (1- x2) (1- y2) :filled nil :ink +black+)
		    (draw-rectangle* stream x3 y1 (1- x4) (1- y2) :filled nil :ink +black+)
		    (draw-rectangle* stream x2 y1 (1- x3) (1- y2) :filled nil :ink +black+))
		;; Clear the slug
		(draw-rectangle* stream 
		  (min slug-start old-slug-start) (1+ y1)
		  (max (+ slug-start slug-length) (+ old-slug-start old-slug-length)) (1- y2)
		  :ink +background-ink+))
	      ;; Redraw the slug
	      (draw-rectangle* stream 
		slug-start (1+ y1) (+ slug-start slug-length) (1- y2)
		:ink +light-gray+))))))))
 
(defun compute-scrollbar-values (long short margin
				 extent-min extent-max viewport-min viewport-max)
  (let* ((square (- short (* 2 margin)))
	 (x1 margin)
	 (x2 (+ x1 square))
	 (x4 (- long margin))
	 (x3 (- x4 square))
	 (y1 margin)
	 (y2 (- short margin))
	 (esize (- (max viewport-max extent-max) (min viewport-min extent-min)))
	 (vsize (- viewport-max viewport-min))
	 (ratio (float (/ (max 1 vsize) (max vsize 1 esize))))
	 (available-size (- x3 x2 2))
	 (slug-length (* available-size ratio))
	 (left (- available-size slug-length))
	 (slug-start (+ x2 (* left (/ (- viewport-min extent-min)
				      (max 1 (- esize vsize)))) 1)))
      (values x1 x2 x3 x4 y1 y2 (floor slug-start) (floor slug-length))))

;; Update the scroll-bars when their parent window changes
(defun update-scrollbars (stream width height)
  ;; Adjust width and height for the border of the window
  (multiple-value-bind (lom tom rom bom) (host-window-margins stream)
    (declare (fixnum lom tom rom bom))
    (decf width (+ lom rom))
    (decf height (+ tom bom)))
  (with-slots (horizontal-scroll-bar vertical-scroll-bar) stream
    (when vertical-scroll-bar
      (let ((new-height
	     (if horizontal-scroll-bar (- height *clx-scrollbar-width*) height)))
	(when (/= new-height (bounding-rectangle-height vertical-scroll-bar))
	  (bounding-rectangle-set-size 
	   vertical-scroll-bar
	   (bounding-rectangle-width vertical-scroll-bar)
	   new-height)
	  (draw-scrollbar vertical-scroll-bar t))))
    (when horizontal-scroll-bar
      (bounding-rectangle-set-edges
	horizontal-scroll-bar
	(if vertical-scroll-bar *clx-scrollbar-width* 0)
	(- height *clx-scrollbar-width*)
	width
	height)
      (draw-scrollbar horizontal-scroll-bar))))

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
	   (with-slots (window) stream
	     (let ((bw (xlib:drawable-border-width window)))
	       (values bw bw bw bw)))))))

(defmethod window-margins ((stream clx-window))
  (let ((h (window-horizontal-scroll-bar stream))
	(v (window-vertical-scroll-bar stream)))
    (values (if v 
		(+ (* 2 *clx-scrollbar-border-width*) (bounding-rectangle-width v))
	        0)
	    0 
	    0
	    (if h
		(+ (* 2 *clx-scrollbar-border-width*) (bounding-rectangle-height h))
	        0))))

(defmethod window-note-size-or-position-change :after
	   ((stream clx-window) new-left new-top new-right new-bottom)
  (update-scrollbars stream (- new-right new-left) (- new-bottom new-top)))


(defmethod (setf medium-foreground) :after (ink (stream clx-window))
  (declare (ignore ink))
  (clx-recompute-gcs stream))

(defmethod (setf medium-background) :after (ink (stream clx-window))
  (declare (ignore ink))
  (clx-recompute-gcs stream))

(defmethod window-modifier-state ((window clx-window))
  (window-modifier-state (clx-stream-root window)))

(defmethod clx-recompute-gcs ((stream clx-window))
  (with-slots (foreground-gc foreground-pixel background-gc background-pixel
	       window flipping-gc) stream
    (unless (slot-boundp stream 'foreground-gc)
      (setf foreground-gc (xlib:create-gcontext :drawable window)))
    (unless (slot-boundp stream 'background-gc)
      (setf background-gc (xlib:create-gcontext :drawable window)))
    (unless (slot-boundp stream 'flipping-gc)
      (setf flipping-gc (xlib:create-gcontext :drawable window :function boole-xor)))
    (flet ((do-ground (ink gc)
	     (setf (xlib:gcontext-fill-style gc) :solid)
	     (setf (xlib:gcontext-foreground gc) (clx-decode-color stream ink))))
      (setf foreground-pixel (do-ground (medium-foreground stream) foreground-gc))
      (setf background-pixel (do-ground (medium-background stream) background-gc)))
    (cond ((and (integerp foreground-pixel) (integerp background-pixel))
	   (setf (xlib:gcontext-background foreground-gc) background-pixel)
	   (setf (xlib:gcontext-fill-style flipping-gc) :solid)
	   (setf (xlib:gcontext-foreground flipping-gc)
		 (logxor foreground-pixel background-pixel)))
	  (t (nyi)))
    (setf (xlib:window-background window) background-pixel)))

(defmethod window-beep ((stream clx-window))
  (with-slots (window) stream
    (let ((display (xlib:window-display window)))
      (xlib:bell display)
      (xlib:display-force-output display))))

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

;;; Debugging aid
(defparameter *clx-force-output* nil)

(defun clx-force-output-if-necessary (stream &optional force-p)
  (when (or force-p *clx-force-output*)
    (xlib:display-force-output (xlib:window-display (slot-value stream 'window)))))

(defmethod window-flush-update-region ((stream clx-window))
  (with-slots (update-region) stream
    (setf update-region nil)))

(defmethod window-process-update-region ((stream clx-window))
  (with-slots (update-region) stream
    (when update-region
      (dolist (rectangle update-region)
	(with-bounding-rectangle* (left top right bottom) rectangle
	  (adjust-for-viewport-and-margins stream left top right bottom)
	  (window-clear-area stream left top right bottom))))))

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

(defmethod copy-area ((stream clx-window)
		      from-left from-top from-right from-bottom
		      to-left to-top)
  ;; coords in "host" coordinate system
  (fix-points from-left from-top from-right from-bottom to-left to-top)
  (with-slots (window copy-gc) stream
    (let ((width (- from-right from-left))
	  (height (- from-bottom from-top)))
      (xlib:copy-area window copy-gc from-left from-top width height window to-left to-top)))
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



(defmethod stream-force-output ((stream clx-window))
  (xlib:display-force-output (xlib:window-display (slot-value stream 'window))))

(defmethod stream-finish-output ((stream clx-window))
  (xlib:display-finish-output (xlib:window-display (slot-value stream 'window))))

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
  (when (window-drawing-possible stream)
    (with-slots (window) stream
      (xlib:clear-area window))
    (clx-force-output-if-necessary stream)))

(defmethod window-visibility ((stream clx-window))
  (with-slots (window) stream
    (not (eql (xlib:window-map-state window) :unmapped))))

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
  (clx-force-output-if-necessary stream))

(defmethod wait-for-window-exposed ((window clx-window))
  ;;--- This is expensive if the window manager doesn't map the window "soon"
  (loop
    (when (window-visibility window)
      (return))
    (process-yield)))

(defmethod window-drawing-possible ((stream clx-window))
  ;;--- This isn't really right for a window that has backing store.
  (with-slots (window) stream
    (not (eql (xlib:window-map-state window) :unmapped))))

(defmethod window-stack-on-top ((stream clx-window))
  (with-slots (window) stream
    (setf (xlib:window-priority window) :above))
  (clx-force-output-if-necessary stream))

(defmethod window-stack-on-bottom ((stream clx-window))
  (with-slots (window) stream
    (setf (xlib:window-priority window) :below))
  (clx-force-output-if-necessary stream))

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
		  (xlib:drawable-height window) x-height)))))
    (update-scrollbars stream width height))
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
  (update-scrollbars stream width height)
  (clx-force-output-if-necessary stream t))

(defmethod bounding-rectangle-set-position* :after ((stream clx-window) new-left new-top)
  (with-slots (window) stream
    (xlib:with-state (window)
      (setf (xlib:drawable-x window) new-left)
      (setf (xlib:drawable-y window) new-top)))
  (clx-force-output-if-necessary stream t))

(defmethod redisplay-decorations ((stream clx-window))
  (with-slots (horizontal-scroll-bar vertical-scroll-bar) stream
    (when vertical-scroll-bar
      (draw-scrollbar vertical-scroll-bar))
    (when horizontal-scroll-bar
      (draw-scrollbar horizontal-scroll-bar))))



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
  (xlib:warp-pointer (xlib:drawable-root (clx-stream-window stream)) x y))

(defmethod stream-pointer-input-rectangle*
	   ((stream clx-window) pointer &key left top right bottom)
  (declare (ignore left top right bottom))
  (portable-pointer-input-rectangle* stream pointer))


;;; Character-drawing methods for CLX implementation

;; TEXT-STYLE  must be a fully merged text style
(defmethod stream-glyph-for-character
	   ((stream clx-window) character text-style &optional our-font)
  (declare (values index font escapement-x escapement-y origin-x origin-y bb-x bb-y))
  (with-slots (window display-device-type) stream
    (multiple-value-bind (character-set index)
	(char-character-set-and-index character)
      (when (eql character-set *standard-character-set*)
	(setf index (xlib:char->card8 character)))	;A little gross, but right, I think.
      (let* ((x-font (or our-font
			 (text-style-mapping
			   display-device-type text-style character-set window)))
	     (escapement-x (or (xlib:char-width x-font index)
			       ;;--- Sometimes the above can return NIL
			       (xlib:char-width x-font (xlib:char->card8 #\A))))
	     (escapement-y 0)
	     (origin-x 0)
	     (origin-y (xlib:font-ascent x-font))
	     (bb-x escapement-x)
	     (bb-y (+ origin-y (xlib:font-descent x-font))))
	(values index x-font escapement-x escapement-y origin-x origin-y bb-x bb-y)))))

(defmethod stream-write-char-1 ((stream clx-window) index x-font ink x y)
  (with-slots (window) stream
    (let ((gc (clx-decode-ink ink stream)))
      (setf (xlib:gcontext-font gc) x-font)
      ;; Move line down so that the top of the character (not the baseline) is at Y.
      (xlib:draw-glyph window gc (round x) (+ (round y) (xlib:font-ascent x-font)) index)))
  (clx-force-output-if-necessary stream))

(defmethod stream-write-string-1 ((stream clx-window)
				  glyph-buffer start end x-font ink x y)
  ;;--- For some reason we can get into a state when start = end meaning
  ;;--- there is nothing to draw and font is NIL so lets skip doing
  ;;--- anything - cer
  (when (> end start)
    (with-slots (window) stream
      (let ((size (if (find-if #'(lambda (x) (> x 255.)) glyph-buffer :start start :end end)
		      16 :default))
	    (gc (clx-decode-ink ink stream)))
	(setf (xlib:gcontext-font gc) x-font)
	;; Move line down so that the top of the characters (not the baseline) is at Y.
	(xlib:draw-glyphs window gc (round x) (+ (round y) (xlib:font-ascent x-font))
			  glyph-buffer
			  :start start :end end :translate #'noop-translate-function
			  :size size)))
    (clx-force-output-if-necessary stream)))

;;; Too bad we have to copy the glyphs again, but that's life...
(defun noop-translate-function (src src-start src-end font dst dst-start)
  (declare (values ending-index horizontal-motion width))
  (declare (ignore font))
  (replace dst src :start1 dst-start :start2 src-start :end2 src-end)
  (values src-end nil nil))


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



(defun create-clx-root-window (&rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'clx-root-window args))

(define-implementation :clx 'create-clx-root-window)

