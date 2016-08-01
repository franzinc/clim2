;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


(defparameter *vk->keysym*
	      `(
		;; The semi-standard characters
		(#x0d #\newline :enter :newline #\return :return)
		(#x20 #\space :space)
		(#x09 #\tab :tab)
		(#x2e :delete)
		(#x08 #\backspace :backspace :rubout)
		;;(???? :page)
		;;(???? :linefeed)
		(#x1b #\escape :escape :abort)
		;; The shifts
		(#x10 :left-shift)
		(#x11 :left-control)
		(#x14 :caps-lock)
		(#x12 :left-meta)
		(#x90 :num-lock)
		;; Non-standard keys
		(#x03 :cancel)
		(#x0c :clear :clear-input)
		(#x13 :pause)
		(#x21 :page-up)
		(#x22 :page-down :scroll)
		(#x23 :end)
		(#x24 :home)
		(#x25 :left)
		(#x26 :up)
		(#x27 :right)
		(#x28 :down)
		(#x29 :select)
		(#x2b :execute)
		(#x2c :print-screen)
		(#x2d :insert)
		(#x2f :help)
		(#x60 :keypad-0)
		(#x61 :keypad-1)
		(#x62 :keypad-2)
		(#x63 :keypad-3)
		(#x64 :keypad-4)
		(#x65 :keypad-5)
		(#x66 :keypad-6)
		(#x67 :keypad-7)
		(#x68 :keypad-8)
		(#x69 :keypad-9)
		(#x6a :keypad-multiply)
		(#x6b :keypad-add)
		(#x6c :keypad-separator)
		(#x6d :keypad-subtract)
		(#x6e :keypad-decimal)
		(#x6f :keypad-divide)
		(#x70 :f1)
		(#x71 :f2)
		(#x72 :f3)
		(#x73 :f4)
		(#x74 :f5)
		(#x75 :f6)
		(#x76 :f7)
		(#x77 :f8)
		(#x78 :f9)
		(#x79 :f10)
		(#x7a :f11)
		(#x7b :f12)
		(#x7c :f13)
		(#x7d :f14)
		(#x7e :f15)
		(#x7f :f16)
		(#x80 :f17)
		(#x81 :f18)
		(#x82 :f19)
		(#x83 :f20)
		(#x84 :f21)
		(#x85 :f22)
		(#x86 :f23)
		(#x87 :f24)
		(#x91 :scroll-lock)
		;;(???? :complete)
		;;(???? :refresh)
		))

(defparameter *char->keysym*
	      (let ((array (make-array 256 :initial-element nil)))
		(dolist (char '(#\newline #\escape #\backspace #\tab #\space #\return))
		  (setf (svref array (char-code char))
			(intern (string-upcase (char-name char)) "KEYWORD")))
		(loop for code from (char-code #\!) to (char-code #\~)
		      do (setf (svref array code)
			       (intern (string (char-upcase (code-char code))) "KEYWORD")))
		array))



(defvar *cloe-port* nil)

(defclass cloe-port (basic-port)
    ((text-style->cloe-font-mapping :initform (make-hash-table))
     (font-cache-style :initform nil)
     (font-cache-font :initform nil)
     (vk->keysym :initform (make-hash-table))
     (keysym->keysym :initform (make-hash-table))
     logpixelsy
     (event-queue :initform (make-queue))
     (pointer-sheet :initform nil)
     pointer-x
     pointer-y))

(defmethod find-port-type ((type (eql ':cloe)))
  'cloe-port)

(defmethod port-type ((port cloe-port))
  ':cloe)


;;--- Eventually do better than this
(defclass cloe-palette (basic-palette) ())

(defmethod make-palette ((port cloe-port) &key color-p dynamic-p)
  (make-instance 'cloe-palette
    :port port 
    :color-p color-p
    :dynamic-p dynamic-p))


(defmethod initialize-instance :before ((port cloe-port) &key)
  (unless (null *cloe-port*)
    (error "There can only be one Cloe port."))
  (setf *cloe-port* port))

(defmethod initialize-instance :after ((port cloe-port) &key)
  (with-slots (logpixelsy silica::default-palette vk->keysym) port
    (initialize-dc)
    (setf logpixelsy (win::get-device-caps *dc* win::logpixelsy))
    (setf silica::default-palette (make-palette port :color-p t))
    (loop for (vk . keysym) in *vk->keysym* do
      (setf (gethash vk vk->keysym) keysym))
    (loop for code from (char-code #\!) to (char-code #\~)
	  for char = (code-char code)
	  do (push char (gethash (win::get-keymap char) vk->keysym)))
    )
  nil)

(defmethod destroy-port :before ((port cloe-port))
  (when (eq port *cloe-port*)
    (setf *cloe-port* nil)))



(defstruct (cloe-font)
  index
  height
  ascent
  descent
  internal-leading
  external-leading
  average-character-width
  maximum-character-width
  weight
  italic
  underlined
  struckout
  first-character
  last-character
  default-character
  break-character
  character-set
  overhang
  pitch
  family
  font-width-table)

(defparameter *cloe-logical-size-alist*
	      '((:tiny       6)
		(:very-small 7)
		(:small	     8)
		(:normal     10)
		(:large	     12)
		(:very-large 18)))

(defmethod text-style-mapping
	   ((port cloe-port) (style text-style)
	    &optional (character-set *standard-character-set*) etc)
  (declare (ignore character-set))
  (with-slots (text-style->cloe-font-mapping font-cache-style font-cache-font) port
    (if (eq style font-cache-style)
	font-cache-font
	(progn 
	  (setf font-cache-style style)
	  (setf font-cache-font
		(or (gethash style text-style->cloe-font-mapping)
		    (setf (gethash style text-style->cloe-font-mapping)
			  (multiple-value-bind (weight italic)
			      (let ((face (text-style-face style)))
				(typecase face
				  (cons
				    (values (if (member :bold face) 700 400)
					    (member :italic face)))
				  (otherwise
				    (case face
				      (:roman (values 400 nil))
				      (:bold (values 700 nil))
				      (:italic (values 400 t))
				      (otherwise (values 400 nil))))))
			    (multiple-value-bind (family face-name)
				(case (text-style-family style)
				  (:fix (values #x35 nil))
				  (:serif (values #x16 nil))
				  (:sans-serif (values #x26 nil))
				  (otherwise (values 0 nil)))
			      (let ((point-size
				      (let ((size (text-style-size style)))
					(typecase size
					  (number
					    size)
					  (otherwise
					    (or (second (assoc size *cloe-logical-size-alist*)) 12))))))
				(make-windows-font 
				  (- (round (* point-size (slot-value port 'logpixelsy))
					    72))
				  :weight weight :italic italic
				  :pitch-and-family family :face face-name)))))))))))

(defmethod text-style-mapping
	   ((device cloe-port) (style silica::device-font)
	    &optional (character-set *standard-character-set*) etc)
  (declare (ignore character-set))
  (unless (eql device (silica::device-font-display-device style))
    (error "An attempt was made to map device font ~S on device ~S, ~@
	    but it is defined for device ~S"
	   style device (silica::device-font-display-device style)))
  (with-slots (text-style->cloe-font-mapping font-cache-style font-cache-font) port
    (if (eq style font-cache-style)
	font-cache-font
	(progn 
	  (setf font-cache-style style)
	  (setf font-cache-font
		(or (gethash style text-style->cloe-font-mapping)
		    (setf (gethash style text-style->cloe-font-mapping)
			  (let ((args (silica::device-font-name style)))
			    (apply #'make-windows-font
				   (- (round (* (pop args) (slot-value device 'logpixelsy))
					     72))
				   args)))))))))

(defun make-windows-font
       (height &key (width 0) (escapement 0) (orientation 0)
	       (weight 400) (italic nil) (underline nil) (strikeout nil)
	       (charset 0) (output-precision 0) (clip-precision 0)
	       (quality 2) (pitch-and-family 0) (face nil))
  (let ((win-font
	  (win::create-font height width escapement orientation weight
			    (if italic 1 0) (if underline 1 0)
			    (if strikeout 1 0) charset
			    output-precision clip-precision quality
			    pitch-and-family (or face ""))))
    (select-font win-font)
    (multiple-value-bind (height ascent descent
			  internal-leading external-leading
			  average-character-width maximum-character-width
			  weight italic underlined struckout
			  first-character last-character default-character
			  break-character p&f character-set overhang
			  aspect-x aspect-y)
	(win::get-text-metrics *dc*)
      (declare (ignore p&f aspect-x aspect-y))
      (make-cloe-font
	:index win-font :height height :ascent ascent :descent descent
	:internal-leading internal-leading :external-leading external-leading
	:average-character-width average-character-width
	:maximum-character-width maximum-character-width
	:weight weight :italic italic 
	:underlined underlined :struckout struckout
	:first-character first-character :last-character last-character
	:default-character default-character :break-character break-character
	:overhang overhang
	:font-width-table (and (/= average-character-width
				   maximum-character-width)
			       (let ((array (make-array (1+ last-character))))
				 (with-temporary-string (string :length 1)
				   (setf (fill-pointer string) 1)
				   (loop for i from first-character to last-character do
				     (setf (aref string 0) (code-char i))
				     (multiple-value-bind (width height)
					 (win::get-text-extent *dc* string)
				       (declare (ignore height))
				       (setf (aref array i) width))))
				 array))))))

(defmethod port-glyph-for-character ((port cloe-port) character style &optional our-font)
  (multiple-value-bind (character-set index)
      (char-character-set-and-index character)
    (let* ((cloe-font (or our-font
			  (text-style-mapping port style character-set)))
	   (origin-x 0)
	   (origin-y (cloe-font-ascent cloe-font))
	   (average-w (cloe-font-average-character-width cloe-font))
	   (max-w (cloe-font-maximum-character-width cloe-font))
	   (fixed-width-p (= max-w average-w))
	   (escapement-x (if fixed-width-p
			     average-w
			     (aref (cloe-font-font-width-table cloe-font) index)))
	   (escapement-y 0)
	   (bb-y (+ (cloe-font-height cloe-font)
		    (cloe-font-external-leading cloe-font)))
	   (bb-x escapement-x))
      (values index cloe-font escapement-x escapement-y
	      origin-x origin-y bb-x bb-y))))



(defmethod port-set-pointer-cursor ((port cloe-port) pointer cursor)
  (unless (eq (pointer-cursor pointer) cursor)
    (win::set-mouse-cursor (realize-cursor port cursor)))
  cursor)

(defmethod port-set-sheet-pointer-cursor ((port cloe-port) sheet cursor)
  (unless (eq (sheet-pointer-cursor sheet) cursor)
    (win::set-mouse-cursor (realize-cursor port cursor)))
  cursor)

(defmethod realize-cursor ((port cloe-port) (cursor integer))
  cursor)

(defmethod realize-cursor ((port cloe-port) (cursor symbol))
  win::idc_arrow)

;; X and Y are in native coordinates
(defmethod port-set-pointer-position ((port cloe-port) pointer x y)
  (win::set-pointer-position x y))



(defmethod port-canonicalize-gesture-spec 
	   ((port cloe-port) gesture-spec &optional modifier-state)
  (with-slots (vk->keysym) port
    (multiple-value-bind (keysym shifts)
	(if modifier-state
	    (values gesture-spec modifier-state)
	    (parse-gesture-spec gesture-spec))
      (let ((char (typecase keysym
		    (symbol
		      (let ((code (position keysym *char->keysym*)))
			(and code
			     (code-char code))))
		    (character
		      (shiftf keysym (svref *char->keysym* (char-code keysym)))))))
	(loop for keysyms being the hash-values of vk->keysym using (hash-key vk)
	      when (member (or char keysym) keysyms)
		do (setf keysym (first keysyms))
	           (when (characterp keysym)
		     (setf keysym (svref *char->keysym* (char-code keysym))))
		   (when (logtest #x100 vk)
		     (setf shifts (logior shifts +shift-key+)))
		   (return))
	(cons keysym shifts)))))



(defun-inline sign-extend-16 (n)
  (dpb n (byte 15 0) (- (ldb (byte 1 15) n))))

(defmethod note-pointer-motion ((port cloe-port) sheet x y)
  (with-slots (pointer-sheet pointer-x pointer-y) port
    (setf pointer-sheet sheet)
    (setf pointer-x x)
    (setf pointer-y y)))

(defmethod flush-pointer-motion ((port cloe-port))
  (with-slots (event-queue pointer-sheet pointer-x pointer-y) port
    (when pointer-sheet
      (let ((pointer (port-pointer port)))
	(when pointer
	  (queue-put event-queue
		     (allocate-event 'pointer-motion-event
		       :native-x pointer-x
		       :native-y pointer-y
		       :modifier-state (port-modifier-state port)
		       :pointer pointer
		       :sheet pointer-sheet))))
      (setf pointer-sheet nil))))

;;; Convert a MS Windows shift mask into a CLIM modifier-state
(defun windows-mask->modifier-state (mask)
  (if (logtest win::mk_shift mask)
      (if (logtest win::mk_control mask)
	  (make-modifier-state :shift :control)
	  (make-modifier-state :shift))
      (if (logtest win::mk_control mask)
	  (make-modifier-state :control)
	  (make-modifier-state))))

(defmethod event-handler ((port cloe-port) args)
  (with-slots (event-queue pointer-sheet pointer-x pointer-y vk->keysym) port
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
	      ((or (eql message win::wm_hscroll)
		   (eql message win::wm_vscroll))
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
	      ((or (eql message win::wm_keydown)
		   (eql message win::wm_syskeydown)
		   (eql message win::wm_keyup)
		   (eql message win::wm_syskeyup))
	       (flush-pointer-motion port)
	       (let ((code (win::get-16bit args 4)))
		 (let ((vk (ldb (byte 8 0) code)))
		   ;; CAPS LOCK
		   (when (and (logtest #x0800 code)
			      (<= #x41 vk #x5a))
		     (setf code (logior #x100 code)))
		   ;; NUM LOCK
		   (when (and (logtest #x1000 code)
			      (<= #x60 vk #x69))
		     (setf code (logior #x100 code))))
		 (let ((keysym (or (gethash (ldb (byte 9 0) code) vk->keysym)
				   (gethash (ldb (byte 8 0) code) vk->keysym)))
		       (char nil))
		   (when (consp keysym)
		     (setf keysym (first keysym)))
		   (when (characterp keysym)
		     (when (zerop (ldb (byte 2 9) code))
		       (setf char keysym))
		     (setf keysym (svref *char->keysym* (char-code keysym))))
		   (queue-put event-queue
			      (allocate-event 
				(cond ((or (eql message win::wm_keydown)
					   (eql message win::wm_syskeydown))
				       'key-press-event)
				      ((or (eql message win::wm_keyup)
					   (eql message win::wm_syskeyup))
				       'key-release-event))
				:key-name keysym
				:character char
				:modifier-state (setf (port-modifier-state port)
						      (ldb (byte 3 8) code))
				:sheet sheet)))))

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

(defun win::vanilla-event (code length args)
  (declare (ignore code length))		;because I don't know what they are!
  (event-handler *cloe-port* args))

;;;

(defmethod process-next-event ((port cloe-port)
			       &key (timeout nil) (wait-function nil)
			       (state "Windows Event"))
  (declare (ignore state))
  (with-slots (event-queue) port
    (let ((end-time (and timeout (+ (get-internal-real-time)
				    (* internal-time-units-per-second timeout)))))
      (win::await-response -1 nil nil)
      (loop
	(flush-pointer-motion port)
	(let ((event (queue-get event-queue)))
	  (when event
	    (distribute-event port event)
	    (return t)))
	(when (and wait-function
		   (funcall wait-function))
	  (return nil))
	(when (and end-time (>= (get-internal-real-time) end-time))
	  (return nil))
	(win::await-response -1 t t)))))

(defmethod distribute-event :around ((port cloe-port) (event window-configuration-event))
  (let* ((sheet (event-sheet event))
	 (mirror (sheet-mirror sheet)))
    (unless (win::is-iconic mirror)
      (let ((native-region (mirror-region port sheet)))
	(setf (slot-value event 'silica::native-region) native-region)
	(setf (slot-value event 'silica::region)
	      (untransform-region (sheet-native-transformation sheet) 
				  native-region)))
      (call-next-method))))
