;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; See the file LICENSE for the full license governing this code.

(in-package :clim-internals)

(defclass clx-display-device (display-device)
    ((display :initarg :display)
     (screen :initarg :screen))
  (:default-initargs :allow-loose-text-style-size-mapping t
		     :mapping-table (make-hash-table :test 'equal)))

(defvar *window-manager-border-size* 2)

(defvar *clx-font-families*
	'((:fix "-*-courier-*")
	  (:sans-serif "-*-helvetica-*")
	  (:serif "-*-charter-*" "-*-new century schoolbook-*" "-*-times-*")))

(defvar *clx-white-color* (xlib:make-color :red 1 :blue 1 :green 1))
(defvar *clx-black-color* (xlib:make-color :red 0 :blue 0 :green 0))
(defvar *cursor-font* nil)

(defun disassemble-x-font-name (name)
  (let ((cpos 0)
	(tokens nil))
    (loop
      (let ((dpos (position #\- name :start cpos)))
	(when (null dpos)
	  (push (subseq name cpos) tokens)
	  (return))
	(push (if (= cpos dpos)
		  nil
		  (subseq name cpos dpos))
	      tokens)
	(setf cpos (1+ dpos))))
    (reverse tokens)))

(defvar *clx-fallback-font* "8x13"
  "When non NIL and nothing better exists use this as the fallback font")

(defmethod initialize-instance :after ((display-device clx-display-device) 
				       &key display screen)
  (let* ((screen-height (xlib:screen-height screen))
	 ;; Use a float value so formula below will be accurate.
	 (screen-pixels-per-inch
	   (* 25.4 (/ screen-height
		      (xlib:screen-height-in-millimeters screen)))))
    (flet ((font->text-style (font family)
	     (let* ((tokens (disassemble-x-font-name font))
		    (italic (member (fifth tokens) '("i" "o") :test #'equalp))
		    (bold (equalp (fourth tokens) "Bold"))
		    (face (if italic
			      (if bold '(:bold :italic) :italic)
			      (if bold :bold :roman)))
		    (designed-point-size (parse-integer (ninth tokens)))
		    (designed-y-resolution (parse-integer (nth 10 tokens)))
		    (point-size (* (float designed-point-size)
				   (/ designed-y-resolution
				      screen-pixels-per-inch)))
		    (size (/ point-size 10)))
	       (make-text-style family face size))))
      (dolist (family-stuff *clx-font-families*)
	(let ((family (car family-stuff)))
	  (dolist (font-pattern (cdr family-stuff))
	    (dolist (xfont (xlib:list-font-names display font-pattern))
	      (let ((text-style (font->text-style xfont family)))
		;; prefer first font satisfying this text style, so
		;; don't override if we've already defined one.
		(unless (text-style-mapping-exists-p
			  display-device text-style *standard-character-set* t)
		  (setf (text-style-mapping display-device text-style) xfont)))))
	  ;; Now build the logical size alist for the family
	  ))))
  (setq *cursor-font* (xlib:open-font display "cursor"))
  ;; We now need to find to establish the
  ;; *undefined-text-style* -> some abstract font mapping -> some real font
  ;; doing something horrible if that fails.
  (let (temp)
    (cond ((setq temp
		 (dolist (family *clx-font-families*)
		   (when (text-style-mapping-exists-p 
			   display-device `(,(car family) :roman 10))
		     (return (make-text-style (car family) :roman 10)))))
	   (setf (text-style-mapping
		   display-device (port-undefined-text-style display-device))
		 temp))
	  ;; Perhaps we should look for some other conveniently sized
	  ;; fonts.
	  (*clx-fallback-font*
	   (setf (text-style-mapping
		   display-device (port-undefined-text-style display-device))
		 (xlib:open-font display
				 *clx-fallback-font*)))
	  ;; Perhaps we should just grab the first font we can find.
	  (t
	   (error "Unable to determine default font")))))

(defparameter *clx-logical-size-alist*
	      '((:tiny       6)
		(:very-small 8)
		(:small	     10)
		(:normal     12)
		(:large	     14)
		(:very-large 18)
		(:huge	     24)))

(defmethod standardize-text-style ((display-device clx-display-device) style 
				   &optional (character-set *standard-character-set*))
  (standardize-text-style-1
    display-device style character-set *clx-logical-size-alist*))


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
      ((or no-error-p (eq try-count 3))
       (and no-error-p display))
    (multiple-value-setq (no-error-p display)
      (if (eq try-count 2)
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
  (values (coordinate 0) (coordinate 0) (coordinate 0) (coordinate 0)))

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
      (setf (aref array (clim-internals::modifier-key-index :shift))
	    (xlib:make-state-mask :shift))
      (setf (aref array (clim-internals::modifier-key-index :control))
	    (xlib:make-state-mask :control))
      (flet ((test-keysym (keysym state-mask)
	       (cond ((or (= keysym xlib::left-meta-keysym)
			  (= keysym xlib::left-alt-keysym)
			  (= keysym xlib::right-meta-keysym)
			  (= keysym xlib::right-alt-keysym))
		      (setf (aref array (clim-internals::modifier-key-index :meta))
			    state-mask))
		     ((or (= keysym xlib::left-super-keysym)
			  (= keysym xlib::right-super-keysym))
		      (setf (aref array (clim-internals::modifier-key-index :super))
			    state-mask))
		     ((or (= keysym xlib::left-hyper-keysym)
			  (= keysym xlib::right-hyper-keysym))
		      (setf (aref array (clim-internals::modifier-key-index :hyper))
			    state-mask)))))
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
		 `(let* ((bit (clim-internals::modifier-key-index ,clim-shift))
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
		   (pointer-set-position pointer root-x root-y)
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
			  (when (and ws (eq event-key :key-press))
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
		 (when (eq request :modifier)
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
		 (with-slots (left top right bottom) ws
		   (cond ((eq parent (clx-stream-window (window-parent ws)))
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
				(+ y (- bottom top) border-width))))
			  (setf (slot-value ws 'reparented-p) nil))
			 (t			  
			  (multiple-value-setq (x y)
			    (xlib:translate-coordinates
			      window 0 0 (clx-stream-window (window-parent ws))))
			  (let ((border-width 0))
			    (window-note-size-or-position-change
			      ws
			      (- x border-width) (- y border-width)
			      (+ x (- right left) border-width)
			      (+ y (- bottom top) border-width)))
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
			  (when (not (eq border-width 0))
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
		 (when (and (eq format 32)
			    (eq type :wm_protocols)
			    (eq (xlib:atom-name
				  display 
				  (aref (the (simple-array (unsigned-byte 32) (5)) data)
					0))
				:wm_delete_window))
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
     (border-width :initform nil :initarg :border-width)
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
     (mapped-p :initform nil :reader window-visibility)
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
					    :program-specified-size-p t
					    ;; pre-X11R5 doesn't hack the above
					    :allow-other-keys t)
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
      (declare (type coordinate rom bom) (ignore lom tom))
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
	  (window-set-viewport-position stream 
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
	  (window-set-viewport-position stream 
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
		(eq slug-start (scroll-bar-values-1 stream))
		(eq slug-length (scroll-bar-values-2 stream)))
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
    (declare (type coordinate lom tom rom bom))
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
	   (values (coordinate 0) (coordinate 0)
		   (coordinate 0) (coordinate 0)))
	  (t
	   (with-slots (border-width) stream
	     (values border-width border-width border-width border-width))))))

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
    (multiple-value-bind (x y) (window-viewport-position stream)
      (declare (type coordinate x y))
      (multiple-value-bind (ml mt) (window-margins stream)
	(declare (type coordinate ml mt))
      	(with-bounding-rectangle* (left top right bottom) region
	  (translate-coordinates (- ml x) (- mt y)
	    left top right bottom)
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
      (declare (type coordinate lom tom rom bom))
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
    (declare (type coordinate lom tom rom bom))
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

(defmethod bounding-rectangle-set-position :after ((stream clx-window) new-left new-top)
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
  (xlib:warp-pointer (clx-stream-window (clx-stream-root stream)) x y))


(defmethod stream-pointer-input-rectangle*
	   ((stream clx-window) pointer &key left top right bottom)
  (declare (ignore left top right bottom))
  (portable-pointer-input-rectangle* stream pointer))


;;; Colors and their monochrome imposters

(defun make-stipple-image (height width patterns)
  (xlib:create-image :width width :height height
		     :data (clim-internals::make-stipple-array height width patterns)
		     :bits-per-pixel 1))

(defvar *clx-luminance-stipples*
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
		
;; The xlib:image objects are created at load time to save run time & space.
;; Here a '0' means white, '1' black.
(defun clx-decode-luminance (luminance stipple-p)
  (if (not stipple-p)
      (if (< luminance 0.5) 1 0)
      (if (< luminance 0.05)
	  1
	  (dolist (entry *clx-luminance-stipples* 0)
	    (let ((l (car entry))
		  (stipple (cdr entry)))
	      (when (< luminance l)
		(return-from clx-decode-luminance stipple)))))))

;; This should only be called on a color screen.
(defmethod clx-decode-color ((stream clx-window) (ink color))
  (with-slots (screen color-p black-pixel) stream
    (multiple-value-bind (red green blue) (color-rgb ink)
      ;;--- Should probably use COLOR-P here.  Otherwise if *CLX-USE-COLOR* is NIL,
      ;;--- colors can still be used for foreground, background, patterns.
      (handler-case
	  (xlib:alloc-color (xlib:screen-default-colormap screen)
			    ;; Either someone has to ensure the the color values in a color
			    ;; object are floats, or we have to here.
			    (xlib:make-color :red (float red)
					     :green (float green)
					     :blue (float blue)))
	;;--- Have to handle resource exhaustion better here.  -- jdi
	(xlib:alloc-error (condition)
	  (declare (ignore condition))
	  (warn "No more colors available for ~S, using black instead." ink)
	  black-pixel)))))

(defmethod clx-decode-color ((stream clx-window) (ink (eql +foreground-ink+)))
  (slot-value stream 'foreground-pixel))

(defmethod clx-decode-color ((stream clx-window) (ink (eql +background-ink+)))
  (slot-value stream 'background-pixel))

;;--- We can surely do better than this
(defmethod clx-decode-color ((stream clx-window) (ink standard-opacity))
  (if (> (slot-value ink 'clim-utils::value) 0.5)
      (slot-value stream 'foreground-pixel)
      (slot-value stream 'background-pixel)))

(defgeneric clx-decode-ink (ink stream))

(defmethod clx-decode-ink ((ink (eql +foreground-ink+)) stream)
  (slot-value stream 'foreground-gc))

(defmethod clx-decode-ink ((ink (eql +background-ink+)) stream)
  (slot-value stream 'background-gc))

(defmethod clx-decode-ink ((ink (eql +flipping-ink+)) stream)
  (slot-value stream 'flipping-gc))

(defmethod clx-decode-ink ((ink flipping-ink) stream)
  (multiple-value-bind (design1 design2)
      (decode-flipping-ink ink)
    (cond ((or (and (eq design1 +foreground-ink+) (eq design2 +background-ink+))
	       (and (eq design1 +background-ink+) (eq design2 +foreground-ink+)))
	   (slot-value stream 'flipping-gc))
	  (t (nyi)))))

(defmethod clx-decode-ink ((ink color) stream)
  (with-slots (foreground-gc color-p black-pixel white-pixel ink-table
	       window screen root) stream
    (or (gethash ink ink-table)
	(let ((gc (xlib:create-gcontext :drawable window)))
	  (xlib:copy-gcontext foreground-gc gc)
	  (cond (color-p
		 (setf (xlib:gcontext-fill-style gc) :solid
		       (xlib:gcontext-foreground gc) (clx-decode-color stream ink)))
		(t
		 (multiple-value-bind (r g b) (color-rgb ink)
		   (let* ((luminance (color-luminosity r g b))
			  (color (clx-decode-luminance luminance t)))
		     (cond ((eq color 1)
			    (setf (xlib:gcontext-fill-style gc) :solid
				  (xlib:gcontext-foreground gc) black-pixel))
			   ((eq color 0)
			    (setf (xlib:gcontext-fill-style gc) :solid
				  (xlib:gcontext-foreground gc) white-pixel))
			   (t			; color is an image
			    (setf (xlib:gcontext-fill-style gc) :tiled)
			    (let ((pixmap (xlib:create-pixmap
					    :drawable window
					    :width (xlib:image-width color)
					    :height (xlib:image-height color)
					    ;;--- is this right?
					    :depth (xlib:screen-root-depth screen))))
			      (xlib:put-image pixmap
					      (slot-value root 'stipple-gc)
					      color :x 0 :y 0 :bitmap-p t)
			      (setf (xlib:gcontext-tile gc) pixmap))))))))
	  (setf (gethash ink ink-table) gc)))))

(defmethod clx-decode-ink ((ink contrasting-ink) stream)
  (clx-decode-ink (make-color-for-contrasting-ink ink) stream))

(defmethod clx-decode-ink ((ink rectangular-tile) stream)
  (with-slots (ink-table) stream
    (or (gethash ink ink-table)
	(multiple-value-bind (pattern width height)
	    (decode-rectangular-tile ink)
	  (clx-decode-pattern pattern stream width height t)))))

(defmethod clx-decode-ink ((ink pattern) stream)
  (clx-decode-pattern ink stream))

(defmethod clx-decode-ink ((ink stencil) stream)
  (declare (ignore stream))
  (error "Stencils and opacities are not supported in this CLIM implementation"))

(defmethod clx-decode-ink ((ink composite-in) stream)
  (declare (ignore stream))
  (error "Compositing is not supported in this CLIM implementation"))

(defmethod clx-decode-ink ((ink composite-out) stream)
  (declare (ignore stream))
  (error "Compositing is not supported in this CLIM implementation"))

(defmethod clx-decode-pattern ((ink design) stream &optional width height tiled-p)
  (declare (ignore stream width height tiled-p))
  (error "Arbitrary patterned designs are not supported in this CLIM implementation"))

(defmethod clx-decode-pattern ((ink color) stream &optional width height tiled-p)
  (declare (ignore stream width height tiled-p))
  (error "A pattern must be a bounded design, ~S isn't" ink))

(defmethod clx-decode-pattern ((ink pattern) stream &optional width height tiled-p)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (foreground-gc foreground-pixel background-gc background-pixel
	       window ink-table screen) stream
    (or (gethash ink ink-table)
	(multiple-value-bind (array designs)
	    (decode-pattern ink)
	  (let ((pattern-height (array-dimension array 0))
		(pattern-width  (array-dimension array 1)))
	    (declare (type xlib::array-index pattern-width pattern-height))
	    (unless width
	      (setq width pattern-width))
	    (unless height
	      (setq height pattern-height))
	    #+++ignore
	    (do ((i 0 (1+ i))
		 (design))
		((eq i (length designs)))
	      (setq design (elt designs i))
	      (when (and (not (colorp design))
			 (not (eq design +foreground-ink+))
			 (not (eq design +background-ink+)))
		(error "Pattern designs other than colors are not supported yet.")))
	    (let* ((design-pixels (make-array (length designs)))
		   (depth (xlib:screen-root-depth screen))
		   (image-data 
		     (make-array (list height width)
				 :element-type
				   (cond ((= depth 1) 'xlib::pixarray-1-element-type)
					 ((<= depth 4) 'xlib::pixarray-4-element-type)
					 ((<= depth 8) 'xlib::pixarray-8-element-type)
					 ((<= depth 16) 'xlib::pixarray-16-element-type)
					 ((<= depth 24) 'xlib::pixarray-24-element-type)
					 (t 'xlib::pixarray-32-element-type)))))
	      (declare #+Genera (sys:array-register design-pixels)
		       (simple-vector design-pixels))
	      ;; Cache the decoded designs from the pattern
	      (do* ((num-designs (length designs))
		    (n 0 (1+ n))
		    design)
		   ((eq n num-designs))
		(setq design (elt designs n))
		(setf (svref design-pixels n) (clx-decode-color stream design)))
	      (do ((y 0 (1+ y)))
		  ((eq y (the fixnum height)))
		(declare (type xlib::array-index y))
		(do ((x 0 (1+ x)))
		    ((eq x (the fixnum width)))
		  (declare (type xlib::array-index x))
		  (setf (aref image-data y x)
			(if (or (>= y pattern-height) (>= x pattern-width))
			    background-pixel
			    (svref design-pixels (aref array y x))))))
	      (let ((gc (xlib:create-gcontext :drawable window)))
		(xlib:copy-gcontext foreground-gc gc)
		(setf (xlib:gcontext-fill-style gc) :tiled)
		(setf (xlib:gcontext-tile gc)
		      (let ((image
			      (xlib:create-image :depth depth
						 :data image-data
						 :width width :height height))
			    (pixmap
			      (xlib:create-pixmap :width width :height height
						  :drawable window
						  :depth depth)))
			(xlib:put-image pixmap gc image :x 0 :y 0)
			pixmap))
		(when (not tiled-p)
		  ;;--- This doesn't work because the clip mask is set below.
		  ;;--- Anyway, the clip-mask applies to the destination, so
		  ;;--- the x and y must be set correctly.  -- jdi
		  (setf (xlib:gcontext-clip-mask gc)
			(list 0 0 pattern-width pattern-height)))
		(setf (gethash ink ink-table) gc))))))))

;;--- It would be nice if we did not have to cons up a new list each
;;--- time.  Perhaps we could test if it had changed.  Perhaps the global
;;--- value of the clipping rectangle could be determined in advance.
(defun compute-gcontext-clip-mask (stream clip-mask)
  (multiple-value-bind 
      (left top right bottom)
      (window-margins stream)
    (let ((s-width (bounding-rectangle-width stream))
	  (s-height (bounding-rectangle-height stream)))
      (if (eq clip-mask :none)
	  (list left top (- s-width right) (- s-height bottom))
	(let* ((x (pop clip-mask))
	       (y (pop clip-mask))
	       (width  (pop clip-mask))
	       (height (pop clip-mask)))
	  (let ((new-clip-left (max left x))
		(new-clip-top (max top y)))
	    (list new-clip-left new-clip-top
		  (- (min (+ left (- s-width right))	;clip-right
			  (+ x width))			;another clip-right
		     new-clip-left)
		   (- (min (+ top (- s-height bottom))	;clip-bottom
			   (+ y height))		;another clip-bottom
		      new-clip-top))))))))

;; This is necessary because the GC used for drawing doesn't depend only
;; on the ink used, unfortunately.  We have to adjust the clip-mask for
;; the stream it's used on.
(defmethod clx-decode-ink :around (ink stream)
  (setq ink (call-next-method))
  (with-slots (clip-mask) stream
    (setf (xlib:gcontext-clip-mask ink)
      #---ignore clip-mask
      #+++ignore (compute-gcontext-clip-mask stream clip-mask)))
  ink)
    
;; This only needs to be called for shapes, not points or characters.
;; Basically, we want to do things in here that operate on cached inks
;; (graphics contexts).
(defmethod clx-adjust-ink (ink stream line-style x-origin y-origin)
  (with-slots (points-to-pixels) stream
    (when (eq (xlib:gcontext-fill-style ink) :tiled)
      #---ignore
      (setf (xlib:gcontext-ts-x ink) x-origin
	    (xlib:gcontext-ts-y ink) y-origin)
      #+++ignore
      (with-bounding-rectangle* (left top right bottom) (window-viewport stream)
	(declare (ignore right bottom))
	(let ((size (xlib:drawable-width (xlib:gcontext-tile ink))))
	  (setf (xlib:gcontext-ts-x ink) (mod left size)	;--- 16 is a magic
		(xlib:gcontext-ts-y ink) (mod top size)))))
    (when line-style
      (let ((thickness (line-style-thickness line-style)))
	(ecase (line-style-unit line-style)
	  (:normal)
	  (:point
	   (setf thickness (* thickness points-to-pixels))))
	(setf (xlib:gcontext-line-width ink) 
	  (if (< thickness 2) 0 (round thickness)))
	(let ((dashes (line-style-dashes line-style)))
	  (cond (dashes
		 (setf (xlib:gcontext-line-style ink) :dash)
		 (setf (xlib:gcontext-dashes ink) 
		       #-Allegro (cond ((eq dashes t) #(4 4))
				       ((listp dashes) (coerce dashes 'vector))
				       (t dashes))
		       #+Allegro (cond ((eq dashes t) '(4 4))
				       ((vectorp dashes) (coerce dashes 'list))
				       (t dashes))))
		(t
		 (setf (xlib:gcontext-line-style ink) :solid))))
	(setf (xlib:gcontext-cap-style ink)
	      (ecase (line-style-cap-shape line-style)
		(:butt :butt)
		(:square :projecting)
		(:round :round)
		(:no-end-point :not-last)))
	(setf (xlib:gcontext-join-style ink)
	      (ecase (line-style-joint-shape line-style)
		((:miter :none) :miter)
		(:bevel :bevel)
		(:round :round)))))
    ink))


;;; Drawing functions

(defmethod draw-point-internal ((stream clx-window) x-offset y-offset x y ink line-style)
  (fix-points x y)
  (translate-positions x-offset y-offset x y)
  (with-slots (window points-to-pixels) stream
    (let ((thickness (line-style-thickness line-style))
	  (gc (clx-decode-ink ink stream)))
      (ecase (line-style-unit line-style)
	((:normal :point)
	 (setf thickness (* thickness points-to-pixels))))
      (if (< thickness 2)
	  (xlib:draw-point window gc x y)
	  (let ((thickness (round thickness)))
	    (xlib:draw-arc window gc x y thickness thickness 0 2pi t)))))
  (clx-force-output-if-necessary stream))

(defmethod draw-line-internal ((stream clx-window) x-offset y-offset
			       start-x start-y end-x end-y ink line-style)
  (fix-points start-x end-x start-y end-y)
  (translate-positions x-offset y-offset start-x start-y end-x end-y)
  (with-slots (window) stream
    (xlib:draw-line window
		    (clx-adjust-ink (clx-decode-ink ink stream) stream line-style
				    (min start-x end-x) (min start-y end-y))
		    start-x start-y end-x end-y))
  (clx-force-output-if-necessary stream))

(defmethod draw-rectangle-internal ((stream clx-window) x-offset y-offset
				    left top right bottom ink line-style)
  (fix-points left top right bottom)
  (translate-positions x-offset y-offset left top right bottom)
  (with-slots (window) stream
    (xlib:draw-rectangle window 
			 (clx-adjust-ink (clx-decode-ink ink stream) stream line-style
					 left top)
			 left top (- right left) (- bottom top) (null line-style)))
  (clx-force-output-if-necessary stream))

(defmethod draw-polygon-internal ((stream clx-window) x-offset y-offset
				  list-of-x-and-ys closed ink line-style)
  (setq list-of-x-and-ys
	(mapcar #'round 
		(translate-position-sequence x-offset y-offset list-of-x-and-ys)))
  (let ((minx most-positive-fixnum)
	(miny most-positive-fixnum)
	(points list-of-x-and-ys))
    (declare (type fixnum minx miny))
    (do* ((points list-of-x-and-ys (cddr points))
	  (x (car points) (car points))
	  (y (cadr points) (cadr points)))
	((null points))
      (if (< x minx) (setq minx x))
      (if (< y miny) (setq miny y)))
    (when (and closed line-style)		;kludge
      (setq points (append points `(,(first points) ,(second points)))))
    (with-slots (window) stream
      (xlib:draw-lines window 
		       (clx-adjust-ink (clx-decode-ink ink stream) stream line-style
				       minx miny)
		       points :fill-p (null line-style))))
  (clx-force-output-if-necessary stream))

(defmethod draw-ellipse-internal ((stream clx-window) x-offset y-offset
				  center-x center-y
				  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				  start-angle end-angle ink line-style)
  (fix-points center-x center-y)
  (translate-positions x-offset y-offset center-x center-y)
  (when (null start-angle)
    (setq start-angle 0.0
	  end-angle 2pi))
  ;; Awaiting implementation of real graphics model
  ;; CLX takes a start angle and a delta-angle relative to it, but measures angles
  ;; such that pi/2 points straight up even though Y increases downwards.  Flip that.
  (setq start-angle (- 2pi start-angle)
	end-angle (- 2pi end-angle))
  ;; We also have to flip the sense of clockwise.
  (rotatef start-angle end-angle)
  ;; The caller has already coerced start- and end-angle to be in the range 0 <= angle < 2pi
  (when (< end-angle start-angle)
    (setq end-angle (+ end-angle 2pi)))
  (setf radius-1-dx (round radius-1-dx))
  (setf radius-1-dy (round radius-1-dy))
  (setf radius-2-dx (round radius-2-dx))
  (setf radius-2-dy (round radius-2-dy))
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
    (with-slots (window) stream
      (let ((angle-size (- end-angle start-angle)))
	(xlib:draw-arc window 
		       (clx-adjust-ink (clx-decode-ink ink stream) stream
				       line-style
				       (- center-x 
					  (if (zerop radius-1-dx)
					      radius-2-dx
					    radius-1-dx))
				       (- center-y 
					  (if (zerop radius-1-dy)
					      radius-2-dy
					    radius-1-dy)))
		       (- center-x x-radius) (- center-y y-radius)
		       (* x-radius 2) (* y-radius 2)
		       ;; CLX measures the second angle relative to the first
		       start-angle angle-size (null line-style)))))
  (clx-force-output-if-necessary stream))


;;; Character-drawing methods for CLX implementation

;; TEXT-STYLE  must be a fully merged text style
(defmethod stream-glyph-for-character
	   ((stream clx-window) character text-style &optional our-font)
  (declare (values index font escapement-x escapement-y origin-x origin-y bb-x bb-y))
  (with-slots (window display-device-type) stream
    (multiple-value-bind (character-set index)
	(char-character-set-and-index character)
      (when (eq character-set *standard-character-set*)
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

(defmethod text-style-mapping :around ((device clx-display-device) text-style
				       &optional (character-set *standard-character-set*) etc)
  (declare (ignore etc))
  (let ((font (call-next-method)))
    (when (or (stringp font) (symbolp font))
      (let* ((font-name (string font)))
	(with-slots (display) device
	  (setf font (xlib:open-font display font-name)))
	(setf (text-style-mapping
		device (parse-text-style text-style) character-set)
	      font)))
    font))

(defmethod clx-get-default-font ((stream clx-window))
  (with-slots (window display-device-type) stream
    (text-style-mapping
      display-device-type (medium-merged-text-style stream) *standard-character-set* window)))

(defmethod draw-string-internal ((stream clx-window) x-offset y-offset
				 string x y start end align-x align-y text-style ink)
  (fix-points x y)
  (translate-positions x-offset y-offset x y)
  (with-slots (window display-device-type) stream
    (let* ((font (if text-style
		     (text-style-mapping
		       display-device-type text-style *standard-character-set* window)
		     (clx-get-default-font stream)))
	   (ascent (xlib:font-ascent font))
	   (descent (xlib:font-descent font))
	   (height (+ ascent descent))
	   (gc (clx-decode-ink ink stream)))
      (incf x (clim-internals::compute-text-x-adjustment 
		align-x stream string text-style start end))
      (incf y (clim-internals::compute-text-y-adjustment
		align-y descent ascent height))
      (setf (xlib:gcontext-font gc) font)
      (xlib:draw-glyphs window gc x y string :start start :end end)))
  (clx-force-output-if-necessary stream))

(defmethod draw-character-internal ((stream clx-window) x-offset y-offset
				    character x y align-x align-y text-style ink)
  (fix-points x y)
  (translate-positions x-offset y-offset x y)
  (with-slots (window display-device-type) stream
    (let* ((font (if text-style
		     (text-style-mapping
		       display-device-type text-style *standard-character-set* window)
		     (clx-get-default-font stream)))
	   (ascent (xlib:font-ascent font))
	   (descent (xlib:font-descent font))
	   (height (+ ascent descent))
	   (gc (clx-decode-ink ink stream)))
      (incf x (clim-internals::compute-text-x-adjustment 
		align-x stream character text-style))
      (incf y (clim-internals::compute-text-y-adjustment
		align-y descent ascent height))
      (setf (xlib:gcontext-font gc) font)
      (xlib:draw-glyph window gc x y character)))
  (clx-force-output-if-necessary stream))


(defmethod text-style-height ((text-style standard-text-style) (stream clx-window))
  (with-slots (window display-device-type) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      (let* ((ascent (xlib:font-ascent font))
	     (descent (xlib:font-descent font))
	     (height (+ ascent descent)))
	height))))

(defmethod text-style-width ((text-style standard-text-style) (stream clx-window))
  (with-slots (window display-device-type) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      ;; Disgusting, but probably OK
      (xlib:char-width font (xlib:char->card8 #\A)))))

(defmethod text-style-ascent ((text-style standard-text-style) (stream clx-window))
  (with-slots (window display-device-type) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      (xlib:font-ascent font))))

(defmethod text-style-descent ((text-style standard-text-style) (stream clx-window))
  (with-slots (window display-device-type) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      (xlib:font-descent font))))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (stream clx-window))
  (with-slots (window display-device-type) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      ;; Really disgusting, but probably OK
      (= (xlib:char-width font (xlib:char->card8 #\.))
	 (xlib:char-width font (xlib:char->card8 #\M))))))


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
	       (if (eq (xlib:grab-pointer window event-mask
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
	  #-(or Genera Minima Allegro) (declare (ignore i))
	  (if (eq (xlib:window-map-state window) :unmapped)
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



(defun create-clx-root-window (&rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'clx-root-window args))

(define-implementation :clx 'create-clx-root-window)

