;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements the CLIM Port protocol.  Also handles processing     *
*  events, and text and keyboard support.                                    *
*                                                                            *
*                                                                            *
****************************************************************************|#


(in-package :acl-clim)

;;; MSWindows Virtual-Key Codes Win32 PR, V2p872
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
		(#x21 :page-up :scroll-up)
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

(defparameter *keysym-alist*
	      `((#\Return . :return)
		(#\Newline . :newline)
		(#\Tab . :tab)
		(#\Rubout . :rubout)
		(#\Backspace . :backspace)
		(#\Page . :page)
		(#\Linefeed . :linefeed)
		(#\Escape . :escape)))

(defclass acl-event-queue (queue)
    ())
  
(define-constructor make-acl-event-queue queue () )

;; reinstated the text-style->acl-font-mapping table (and commented
;; out font-cache-style/font slots) because we now need to have
;; multiple logical fonts open simultaneously for the life time of any
;; controls for which a :text-style is specified. Before, only the 
;; Windows system font was being used and text-style-mapping was only
;; used to put stuff in device contexts which only lived inside a
;; with-dc - besides which the *original-font*, *created-font* logic
;; was suspect. (cim 10/14/96)

(defclass acl-port (basic-port)
    ((text-style->acl-font-mapping :initform (make-hash-table)) 
     #+ignore (font-cache-style :initform nil)
     #+ignore (font-cache-font :initform nil)
     (vk->keysym :initform (make-hash-table))
     (keysym->keysym :initform (make-hash-table))
     logpixelsy
     (event-queue :initform (make-acl-event-queue))
     (mirror-with-focus :initform nil :accessor acl-port-mirror-with-focus)
     (pointer-sheet :initform nil)
     (motion-pending :initform nil)
     (cursor-cache :initform nil)
     (grab-cursor :initform nil :accessor port-grab-cursor)
     pointer-x
     pointer-y))

(defun silica::find-port-event-queue ()
  (when *acl-port*
    (slot-value *acl-port* 'event-queue)))

(defmethod find-port-type ((type (eql ':aclpc)))
  'acl-port)

#+aclpc
(defmethod port-type ((port acl-port))
  ':aclpc)

(defmethod find-port-type ((type (eql ':aclnt)))
  'acl-port)

#+acl86win32
(defmethod port-type ((port acl-port))
  ':aclnt)

;;; some work to do better 
(defclass acl-palette (basic-palette) ())

(defmethod make-palette ((port acl-port) &key color-p dynamic-p)
  (make-instance 'acl-palette
    :port port 
    :color-p color-p
    :dynamic-p dynamic-p))

(defmethod initialize-instance :before ((port acl-port) &key)
  #+(or aclpc acl86win32) (ensure-clim-initialized)
  (unless (null *acl-port*)
    (cerror "do it anyway" "There can only be one acl port."))
  (setf *acl-port* port))

;;;  more work needed???
(defmethod initialize-instance :after ((port acl-port) &key)
  (with-slots (silica::default-palette silica::deep-mirroring vk->keysym) port
    (setf silica::default-palette (make-palette port :color-p t))
    (setf silica::deep-mirroring t)
    (register-window-class)
    ;(get-clim-icon) +++
    (let ((res (ct::callocate :long)))
    (loop for (vk . keysym) in *vk->keysym* do
      (setf (gethash vk vk->keysym) keysym))
    (loop for code from (char-code #\!) to (char-code #\~)
	  for char = (code-char code)
	  do
          (let ((scan #+aclpc (win::VkKeyscan code)
					  #+acl86win32  (cg::loword (win::VkKeyscan code))))
            (push char (gethash scan vk->keysym))))
    ))
  nil)

(defmethod destroy-port :before ((port acl-port))
  (when (eq port *acl-port*)
    (setf *acl-port* nil)))

(defstruct (acl-font)
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

(defparameter *acl-logical-size-alist*
	      '((:tiny       6)
		(:very-small 7)
		(:small	     8)
		(:normal     10)
		(:large	     12)
		(:very-large 18)))

;;--- hacked to allow text-styles to be specified as lists
;;--- there's probably a more efficient way...
(defmethod text-style-mapping ((port acl-port) (style list)
			       &optional (character-set *standard-character-set*) etc)
  (text-style-mapping port (apply #'make-text-style style)
		      character-set etc))

(defmethod text-style-mapping
	   ((port acl-port) (style text-style)
	    &optional (character-set *standard-character-set*) etc)
  (with-slots (text-style->acl-font-mapping) port
    (or (gethash style text-style->acl-font-mapping)
	(setf (gethash style text-style->acl-font-mapping)
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
		  #-ugly (:fix (values 0 "courier"))
		  #-ugly (:serif (values 0 "times new roman"))
		  #-ugly (:sans-serif (values 0 "arial"))
		  ;;--- some of these specify ugly ugly linedrawn fonts
		  #+ugly (:fix (values #x35 nil)) ;; (logior FF_MODERN FIXED_PITCH 4)
		  #+ugly (:serif (values #x16 nil)) ;; (logior FF_ROMAN VARIABLE_PITCH 4)
		  #+ugly (:sans-serif (values #x26 nil)) ;; (logior FF_SWISS VARIABLE_PITCH 4)
		  (otherwise (values 0 nil)))
	      (let ((point-size
		     (let ((size (text-style-size style)))
		       (typecase size
			 (number
			  size)
			 (otherwise
			  (or (second (assoc size *acl-logical-size-alist*)) 12))))))
		(make-windows-font 
		 (- (round (* point-size (slot-value port 'logpixelsy))
			   72))
		 :weight weight :italic italic
		 :pitch-and-family family :face face-name))))))))

(defmethod text-style-mapping
	   ((device acl-port) (style silica::device-font)
	    &optional (character-set *standard-character-set*) etc)
  (unless (eql device (silica::device-font-display-device style))
    (error "An attempt was made to map device font ~S on device ~S, ~@
	    but it is defined for device ~S"
	   style device (silica::device-font-display-device style)))
  (with-slots (text-style->acl-font-mapping) device
    (or (gethash style text-style->acl-font-mapping)
	(setf (gethash style text-style->acl-font-mapping)
	  (let ((args (silica::device-font-name style)))
	    (apply #'make-windows-font
		   (- (round (* (pop args)
				(slot-value device 'logpixelsy))
			     72))
		   args))))))

(defvar *fwt* nil)

(defun make-font-width-table (dc last-character first-character)
  (let* ((tepsize (ct:ccallocate win::size))
	 (string #+aclpc (ct:ccallocate win::lpstr :size 1) #+acl86win32 (make-string 1) )
	 (array (make-array (1+ last-character))))
    (loop for i from first-character to last-character do
      #+aclpc (ct:cset (char *) string 0 i) ;;; (code-char i)
      #+acl86win32 (setf (char string 0) (code-char i))
      (win::getTextExtentPoint dc string 1 tepsize)
      (setf (aref array i)
	    (ct::cref win::size tepsize win::cx)))
    (setq *fwt* array)))

(defun make-windows-font
       (height &key (width 0) (escapement 0) (orientation 0)
	       (weight 400) (italic nil) (underline nil) (strikeout nil)
	       (charset 0) (output-precision 0) (clip-precision 0)
	       (quality 2) ;; ie PROOF_QUALITY
	       (pitch-and-family 0) (face nil) win-font) 
  (let ((win-font 
	 (or win-font
	     (win::createFont height width escapement orientation weight
			      (if italic 1 0) (if underline 1 0)
			      (if strikeout 1 0) charset
			      output-precision clip-precision quality
			      pitch-and-family (or face ""))))
        (old-font nil)
	(cw (and *application-frame*
		 (frame-top-level-sheet *application-frame*)
		 (sheet-mirror (frame-top-level-sheet *application-frame*))))
	(tmstruct (ct::ccallocate win::textmetric)))
   (unless cw (setf cw *current-window*))
   (with-dc (cw dc)
     #+ignore
     (format *terminal-io* "~%MWF CW: ~S *CW*: ~S Sh: ~S" cw *current-window*
		(mirror->sheet *acl-port* *current-window*))
    (setf old-font (win::selectObject dc win-font))
    ; (setf *created-font* win-font *original-font* old-font)
    (win::getTextMetrics dc tmstruct)
    (let ((average-character-width
	    (ct::cref win::textmetric tmstruct win::tmavecharwidth))
	  (maximum-character-width
            (ct::cref win::textmetric tmstruct win::tmmaxcharwidth))
	  (last-character (ct::cref win::textmetric tmstruct win::tmlastchar))
	  (first-character (ct::cref win::textmetric tmstruct win::tmfirstchar))
	  (font-width-array-or-nil nil))
      (setq font-width-array-or-nil
            (and (/= average-character-width maximum-character-width)
	         (make-font-width-table dc last-character first-character)))
      (make-acl-font
        :index win-font 
	:height (ct::cref win::textmetric tmstruct win::tmheight)
	:ascent (ct::cref win::textmetric tmstruct win::tmascent)
	:descent (ct::cref win::textmetric tmstruct win::tmdescent)
	:internal-leading
	  (ct::cref win::textmetric tmstruct win::tminternalleading)
        :external-leading
	  (ct::cref win::textmetric tmstruct win::tmexternalleading)
	:average-character-width average-character-width        
	:maximum-character-width maximum-character-width 
	:weight (ct::cref win::textmetric tmstruct win::tmweight)
	:italic (ct::cref win::textmetric tmstruct win::tmitalic)
	:underlined (ct::cref win::textmetric tmstruct win::tmunderlined)
	:struckout (ct::cref win::textmetric tmstruct win::tmstruckout)
	:first-character first-character 
	:last-character last-character 
	:default-character (ct::cref win::textmetric tmstruct win::tmdefaultchar)
	:break-character (ct::cref win::textmetric tmstruct win::tmbreakchar)
	:overhang (ct::cref win::textmetric tmstruct win::tmoverhang)
	:font-width-table font-width-array-or-nil)))))

#+ignore ; old version
(defun make-windows-font
       (height &key (width 0) (escapement 0) (orientation 0)
	       (weight 400) (italic nil) (underline nil) (strikeout nil)
	       (charset 0) (output-precision 0) (clip-precision 0)
	       (quality 2) (pitch-and-family 0) (face nil))
  (let ((win-font
	  (win::createFont height width escapement orientation weight
			    (if italic 1 0) (if underline 1 0)
			    (if strikeout 1 0) charset
			    output-precision clip-precision quality
			    pitch-and-family (or face "")))
	(tmstruct (ct:ccallocate win::textmetric)))
   (with-dc (*current-window* dc) 
    (win::selectObject dc win-font)
    (win::getTextMetrics dc tmstruct)
    (let ((average-character-width
	    (ct::cref win::textmetric tmstruct win::tmavecharwidth))
	  (maximum-character-width
            (ct::cref win::textmetric tmstruct win::tmmaxcharwidth))
	  (last-character (ct::cref win::textmetric tmstruct win::tmlastchar))
	  (first-character (ct::cref win::textmetric tmstruct win::tmfirstchar))
	  (font-width-array-or-nil nil))
      (setq font-width-array-or-nil
            (and (/= average-character-width maximum-character-width)
	         (make-font-width-table dc last-character first-character)))
      (make-acl-font
        :index win-font 
	:height (ct::cref win::textmetric tmstruct win::tmheight)
	:ascent (ct::cref win::textmetric tmstruct win::tmascent)
	:descent (ct::cref win::textmetric tmstruct win::tmdescent)
	:internal-leading
	  (ct::cref win::textmetric tmstruct win::tminternalleading)
        :external-leading
	  (ct::cref win::textmetric tmstruct win::tmexternalleading)
	:average-character-width average-character-width        
	:maximum-character-width maximum-character-width 
	:weight (ct::cref win::textmetric tmstruct win::tmweight)
	:italic (ct::cref win::textmetric tmstruct win::tmitalic)
	:underlined (ct::cref win::textmetric tmstruct win::tmunderlined)
	:struckout (ct::cref win::textmetric tmstruct win::tmstruckout)
	:first-character first-character 
	:last-character last-character 
	:default-character (ct::cref win::textmetric tmstruct win::tmdefaultchar)
	:break-character (ct::cref win::textmetric tmstruct win::tmbreakchar)
	:overhang (ct::cref win::textmetric tmstruct win::tmoverhang)
	:font-width-table font-width-array-or-nil)))))

(defmethod port-glyph-for-character ((port acl-port) char style
				     &optional our-font)
  (multiple-value-bind (character-set index)
      (char-character-set-and-index char)
    (let* ((acl-font (or our-font
			 (text-style-mapping port style character-set)))
	   (origin-y (if acl-font (acl-font-ascent acl-font) 10))
	   (average-w (if acl-font (acl-font-average-character-width
				    acl-font) 30))
	   (max-w (if acl-font (acl-font-maximum-character-width acl-font) 30))
	   (fixed-width-p (= max-w average-w))
	   (italic (if acl-font (plusp (acl-font-italic acl-font))))
	   ;; jpm Aug97 this allows FIX.ROMAN.7 and others which for some
	   ;; reason were blowing up on NT... someone should look into
	   ;; what's wrong with the font data structure.
	   (table (acl-font-font-width-table acl-font))
	   (escapement-x (if (or fixed-width-p (>= index (length table)))
			     average-w
			   (or (aref table index) average-w)))
	   #+ignore
	   (escapement-x (if fixed-width-p
			     average-w
			   (aref (acl-font-font-width-table acl-font)
				 index)))
	   (overh (if acl-font (acl-font-overhang acl-font) 0))
	   (origin-x (max 0 overh))
	   (escapement-y 0)
	   (bb-y (if acl-font (+ (acl-font-height acl-font)
		    (acl-font-external-leading acl-font)) 15))
	   (bb-x escapement-x))
      (when italic (setf clim-internals::*wd40italic* origin-x)) 
      (values index acl-font escapement-x escapement-y
	      origin-x origin-y bb-x bb-y))))

(defparameter *win-cursor-type-alist*
    `((:default ,win::IDC_ARROW)
      (:position ,win::IDC_CROSS)
      (:vertical-scroll ,win::IDC_SIZENS)
      (:horizontal-scroll ,win::IDC_SIZEWE)
      #+aclpc (:move ,win::IDC_SIZE)
      #+acl86win32 (:move ,win::IDC_SIZEALL)
      (:scroll-up ,win::IDC_UPARROW)
      (:busy ,win::IDC_WAIT)))

(defmethod port-set-pointer-cursor ((port acl-port) pointer cursor)
  (unless (eq (pointer-cursor pointer) cursor)
    (win::setCursor (realize-cursor port cursor))) ; mouse cursor
  cursor)

(defmethod port-set-sheet-pointer-cursor ((port acl-port) sheet cursor)
  (unless (eq (sheet-pointer-cursor sheet) cursor)
    (win::setCursor (realize-cursor port cursor))) ; mouse cursor
  cursor)

(defmethod realize-cursor ((port acl-port) (cursor symbol))
  (let ((cursor (or (second (assoc cursor *win-cursor-type-alist*))
		    :default)))
    (realize-cursor port cursor)))

(defmethod realize-cursor ((port acl-port) (cursor number))
  (let ((hinstance (ct::null-handle win::hinst))
	    (lpcursorname #+aclpc (ct::ccallocate (char *) :initial-value 0) #+acl86win32 cursor))
    #+aclpc (setf (pc::cpointer-value lpcursorname) cursor)
    (win::LoadCursor hinstance lpcursorname)))

(defmethod realize-cursor ((port acl-port) (cursor #+aclpc acl::lhandle
												   #-aclpc t)) ;+++
  cursor)

(defmethod realize-cursor :around ((port acl-port) cursor)
  (with-slots (cursor-cache) port
    (or (getf cursor-cache cursor)
	(setf (getf cursor-cache cursor)
	  (call-next-method)))))

;; pointer-grabbing - how do you do this on windows? - the following
;; simply deals with the handling of the cursor keyword (cim 10/14/96)

(defmethod port-invoke-with-pointer-grabbed
    ((port acl-port) (sheet basic-sheet) continuation
     &key cursor &allow-other-keys)
  (clim-utils:letf-globally (((port-grab-cursor port) cursor))
    (funcall continuation)))

;; X and Y are in native coordinates
(defmethod port-set-pointer-position ((port acl-port) pointer x y)
  (fix-coordinates x y)
  (win::setCursorPos x y))

(defmethod clim-internals::port-query-pointer ((port acl-port) sheet)
  (let ((point (ct:ccallocate win::point))
	(native-x 0)(native-y 0))
    (win::getCursorPos point)
    (let ((root-x (ct::cref win::point point win::x))
	  (root-y (ct::cref win::point point win::y)))
      (multiple-value-bind (x y)
	  (untransform-position (sheet-device-transformation sheet)
			        native-x native-y)
        (values x y native-x native-y root-x root-y)))))


(defun char->keysym (char)
  (let ((entry (assoc char *keysym-alist*))
        (keysym nil))
    (cond (entry
	    (setq keysym (cdr entry)))
	  ((characterp char)
	    (setq keysym (svref *char->keysym* (char-code char)))))
    keysym))

(defmethod port-canonicalize-gesture-spec 
	   ((port acl-port) gesture-spec &optional modifier-state)
  (with-slots (vk->keysym) port
    (multiple-value-bind (keysym shifts)
	(if modifier-state
	    (values gesture-spec modifier-state)
	    (parse-gesture-spec gesture-spec))
      (let ((char (typecase keysym
		    (symbol
		      (let ((code (position keysym *char->keysym*
					    :from-end
					     (not (logtest +shift-key+
							   shifts)))))
			(and code
			     (code-char code))))
		    (character
		      (shiftf keysym (char->keysym keysym))))))
	; using maphash instead of loop below because of bug in hash iterators
        (maphash
	  #'(lambda (vk keysyms)
	      (when (member (or char keysym) keysyms)
		(setf keysym (first keysyms))
	        (when (characterp keysym)
		  (setf keysym (char->keysym keysym)))
		(when (logtest #x100 vk)
		  (setf shifts (logior shifts +shift-key+)))))
          vk->keysym)
	#|(loop for keysyms being the hash-values of vk->keysym using (hash-key vk)
	      when (member (or char keysym) keysyms)
		do (setf keysym (first keysyms))
	           (when (characterp keysym)
		     (setf keysym (svref *char->keysym* (char-code keysym))))
		   (when (logtest #x100 vk)
		     (setf shifts (logior shifts +shift-key+)))
		   (return))|#
	(cons keysym shifts)))))


(defmethod note-pointer-motion ((port acl-port) sheet x y)
  (with-slots (pointer-sheet motion-pending pointer-x pointer-y) port
    (setf pointer-sheet sheet)
    (setf pointer-x x)
    (setf pointer-y y)
    (setf motion-pending t)))

(defmethod flush-pointer-motion ((port acl-port))
  (with-slots (event-queue motion-pending pointer-sheet pointer-x pointer-y)
      port
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
      (setf pointer-sheet nil
	    motion-pending nil))))

;;; Convert a MS Windows shift mask into a CLIM modifier-state
(defun windows-mask->modifier-state (mask)
  (if (logtest win::mk_shift mask)
      (if (logtest win::mk_control mask)
	  (make-modifier-state :shift :control)
	  (make-modifier-state :shift))
      (if (logtest win::mk_control mask)
	  (make-modifier-state :control)
	  (make-modifier-state))))

(defmethod event-handler ((port acl-port) args)
  (cerror "Go on" "What isn't firing?")
  nil)

;; what's with this hacked queue-get?? (cim 9/16/96)

(defmethod queue-get ((queue acl-event-queue) &optional default)
  "return the element at the head of the queue
   deleteing it from the queue"
  (if (queue-empty-p queue)
    default
    (let ((event (queue-next queue))
          (sheet nil))
      (setf (clim-utils::queue-head queue)
            (free-cons queue (queue-head queue)))
      (if (and (or (typep event 'device-event)
		   (typep event 'window-event))
	       (or (not (setq sheet (event-sheet event)))
		   (not (slot-value sheet 'silica::mirror))))
	(progn
	  (deallocate-event event)
	  (queue-get queue))
	event))))

(defvar *l-counter* 0)
(defvar *nowait* nil)

;;--- event-processing is horribly wrong on aclwin; fix me!!!
#+aclpc
(defmethod process-next-event ((port acl-port)
			       &key (timeout nil) (wait-function nil)
			       (state "Windows Event"))
  (setq *l-counter* 0)
  ; (if (and timeout (= timeout 0)) (cerror "timeout" "zero"))
  (with-slots (event-queue) port ;  nil to disable timeout
    (let ((end-time (and timeout (+ (get-internal-real-time)
				     (* internal-time-units-per-second
				        timeout)))))
      ;; dispatch one pending message, if one is pending
      (await-response nil)
      (loop
        (incf *l-counter*)
	(flush-pointer-motion port)	; adds mouse-moved event to port's
					; queue if necessary
	(let ((event (queue-get event-queue)))
	  (when event
	    (distribute-event port event)
	    (return t)))
	(when (and wait-function
		   (funcall wait-function))
	  (return nil))
	(when (and end-time (>= (get-internal-real-time) end-time))
	  (return nil))
	(loop
	  ;; dispatches all pending messages
	  (unless (await-response nil) (return nil)))
	))))

#+acl86win32
(defmethod process-next-event ((port acl-port)
			       &key (timeout nil) (wait-function nil))
  (with-slots (event-queue motion-pending) port
    (let ((event (queue-get event-queue))
	  (reason nil))
      (unless event 
	(flet ((wait-for-event ()
		 ;(await-response t);(cg::process-pending-events);
         (sys::process-pending-events)
		 (when motion-pending
		   (flush-pointer-motion port))
		 (or (setq event (queue-get event-queue))
		     (and wait-function
			  (funcall wait-function)
			  (setq reason :wait-function)))))
	  (if timeout
	      (mp:process-wait-with-timeout "Windows Event" timeout
					    #'wait-for-event)
	    (mp:process-wait "Windows Event" #'wait-for-event))))
      (cond (event
	     (distribute-event port event)
	     t)
	    (reason :wait-function)
	    (t :timeout)))))

(defmethod distribute-event :around ((port acl-port) (event device-event))
  (let* ((sheet (event-sheet event))
	 (mirror (slot-value sheet 'silica::mirror)))
    (if (not mirror)
      (deallocate-event event)
      (call-next-method))))

(defmethod distribute-event :around ((port acl-port)
				     (event window-configuration-event))
  (let ((sheet (event-sheet event))
        (mirror nil))
    ; (if sheet (setf mirror (slot-value sheet 'silica::mirror)))
    (setf mirror (slot-value sheet 'silica::mirror))
    (if (not mirror)
      (deallocate-event event)
      (unless (win::isIconic mirror)
        (let ((native-region (mirror-region port sheet)))
	  (setf (slot-value event 'silica::native-region) native-region)
	  (setf (slot-value event 'silica::region)
	        (untransform-region (sheet-native-transformation sheet) 
				    native-region)))
        (call-next-method)))))

#+acl86win32-notyet ;; a work in progress
(eval-when (compile load eval)
  (defconstant win::VER_PLATFORM_ETC 1)
  (ff:def-foreign-type win::OSVERSIONINFO
      (:struct (dwOSVersionInfoSize dword)
	       (dwMajorVersion dword)
	       (dwMinorVersion dword)
	       (dwBuildNumber dword)
	       (dwPlatformID dword)
	       (szCSDVersion (char *))))
  (ff:def-foreign-type win::LPOSVERSIONINFO (win::OSVERSIONINFO *)))
  
#+aclpc-notyet
(eval-when (compile load eval)
  (defcstruct win::OSVERSIONINFO
    ))

;; need to update this to use GetVersionEx and return :win95 or :winnt4 in
;; addition to :winnt and :win31 -tjm Aug97
(defun get-system-version ()
  "Use win::GetVersion to determine the operating system being used."
  (let* ((v (win::GetVersion))
         (vh (pc::hiword v)))
    (if (>= vh 0) :winnt :win31)))

(defun silica::acl-mirror-with-focus ()
  (acl-port-mirror-with-focus *acl-port*))


;;; gestures for handling middle button with two button mice

#+(or aclpc acl86win32)
(define-gesture-name :describe :pointer-button (:right :control) :unique nil)
#+(or aclpc acl86win32)
(define-gesture-name :delete :pointer-button (:right :control :shift) :unique nil)

