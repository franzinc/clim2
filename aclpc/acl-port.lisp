;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: acl-port.lisp,v 1.5.8.13 1999/05/26 18:11:37 layer Exp $

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
		(#x25 :left-arrow)
		(#x26 :up-arrow)
		(#x27 :right-arrow)
		(#x28 :down-arrow)
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
	  (intern (string-upcase (char-name char))
		  (find-package :keyword))))
      (loop for code from (char-code #\!) to (char-code #\~)
	  do (setf (svref array code)
	       (intern (string (char-upcase (code-char code)))
		       (find-package :keyword))))
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

(defclass acl-port (basic-port)
  (;; Cache used by dc-image-for-ink for mapping inks to device contexts:
   (dc-cache :initform (make-hash-table) :accessor port-dc-cache)
   ;; Virtual key to keysym:
   (vk->keysym :initform (make-hash-table))
   ;; Cached value of (win:GetDeviceCaps dc win:LOGPIXELSY) :
   logpixelsy
   ;; Where window events get put:
   (event-queue :initform (make-acl-event-queue))
   ;; Most recently activated window or control:
   (mirror-with-focus :initform nil :accessor acl-port-mirror-with-focus)
   ;; Cache used by realize-cursor:
   (cursor-cache :initform nil)
   ;; Support for pointer grabbing:
   (grab-cursor :initform nil :accessor port-grab-cursor)
   ;; Default foreground, background colors:
   (resources :initform nil :accessor port-default-resources)
   ;; Four slots for cached pointer state:
   (pointer-x :initform 0)
   (pointer-y :initform 0)
   (pointer-sheet :initform nil)
   (motion-pending :initform nil)
   ;; Cached mapping of CLIM text styles to operating system fonts:
   (text-style->acl-font-mapping :initform (make-hash-table)) 
   ;; Drawing text at an angle is implemented by asking the
   ;; operating system to rotate a font to the required angle:
   (rotated-font-htable :initform (make-hash-table)
			:accessor port-rotated-font-htable)   
   ))

(defmethod restart-port ((port acl-port))
  ;; No need to devote a thread to receiving messages
  ;; if events get delivered directly to the
  ;; frame's own thread.
  nil)

(defmethod port-alive-p ((port acl-port))
  t)

(defmethod port-event-wait ((port acl-port) waiter
			    &key (wait-reason 
				  #+Genera si:*whostate-awaiting-user-input*
				  #-Genera "CLIM Input")
				 timeout)
  (process-next-event port
		      :timeout timeout
		      :wait-function waiter
		      :state wait-reason))

(defun silica::find-port-event-queue ()
  (when *acl-port*
    (slot-value *acl-port* 'event-queue)))

(defmethod find-port-type ((type (eql ':aclpc)))
  'acl-port)

(defmethod find-port-type ((type (eql ':aclnt)))
  'acl-port)

(defmethod port-type ((port acl-port))
  ':aclnt)

;;; some work to do better 
(defclass acl-palette (basic-palette) ())

(defmethod make-palette ((port acl-port) &key color-p dynamic-p)
  (make-instance 'acl-palette
    :port port 
    :color-p color-p
    :dynamic-p dynamic-p))

(defclass acl-device-color (device-color) 
  ((color :initform nil)))

(defmethod make-device-color ((palette acl-palette) pixel)
  (make-instance 'acl-device-color 
    :palette palette
    :pixel pixel))

(defmethod device-color-color ((device-color acl-device-color))
  (with-slots (color) device-color
    (or color
	(setq color (wincolor->color (device-color-pixel device-color))))))

(defmethod silica::port-set-pane-background ((port acl-port) pane medium ink)
  (declare (ignore pane))
  ;; Invoked :after (setf pane-background)
  (when (not (numberp MEDIUM))
    ;;; On acl a medium is a number.
    (setf (medium-background medium) ink)))

(defmethod silica::port-set-pane-foreground ((port acl-port) pane medium ink)
  (declare (ignore pane))
  ;; Invoked :after (setf pane-foreground)
  (when (not (numberp MEDIUM))
    ;;; On acl a medium is a number.
    (setf (medium-foreground medium) ink)))

(defmethod silica::port-set-pane-text-style ((port acl-port) pane medium style)
  (declare (ignore pane))
  (when (not (numberp MEDIUM))
    ;;; On acl a medium is a number.
    (setf (medium-text-style medium) style)))

(defmethod initialize-instance :before ((port acl-port) &key)
  (ensure-clim-initialized)
  (unless (null *acl-port*)
    (cerror "do it anyway" "There can only be one acl port."))
  (setf *acl-port* port))

(defmethod initialize-instance :after ((port acl-port) &key)
  (with-slots (silica::default-palette silica::deep-mirroring vk->keysym) port
    ;; Start the scheduler as a workaround to bug7305.  Eventually,
    ;; we probably won't need this.  JPM 10/98.
    (mp:start-scheduler)
    (setf silica::default-palette (make-palette port :color-p t))
    (setf silica::deep-mirroring t)
    (register-window-class (realize-cursor port :default))
    ;;(get-clim-icon) +++
    (let ((res (ct:callocate :long)))
      (loop for (vk . keysym) in *vk->keysym* do
	    (setf (gethash vk vk->keysym) keysym))
      (loop for code from (char-code #\!) to (char-code #\~)
	  for char = (code-char code)
	  do
	    (let ((scan (loword (win:VkKeyScan code))))
	      (push char (gethash scan vk->keysym))))
      )
    ;; Panes that have a direct mirror will get initialized from
    ;; get-sheet-resources:
    (setf (port-default-resources port)
      `(:background 
	,+ltgray+
	:foreground
	,(wincolor->color (win:GetSysColor win:COLOR_WINDOWTEXT))))
    ;; Panes that don't have a direct mirror will use this as
    ;; the default background:
    (setq silica:*default-pane-background* +ltgray+)
    ))

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
		(:very-large 18)
		(:huge       24)))

(defmethod port-find-rotated-font ((port acl-port) 
				   plain-font font-rotation-angle)
  ;; Find the rotated-font corresponding to plain-font
  ;; rotated at the appropriate angle.
  ;; If it doesn't exist, create it and cache a record
  ;; of it.
  (let ((htable (port-rotated-font-htable port)))
    (let ((base-font-list (gethash plain-font htable)))
      (let ((font-pair (and base-font-list
			    (find font-rotation-angle base-font-list 
				  :test #'=
				  :key #'first))))
	(cond (font-pair
	       (second font-pair))
	      (t
	       (let ((new-font (port-duplicate-rotated-windows-font 
				port plain-font font-rotation-angle)))
		 (push (list font-rotation-angle new-font)
		       (gethash plain-font htable))
		 new-font)))))))

(defmethod port-duplicate-rotated-windows-font 
    ((port acl-port) plain-font font-rotation-angle)
  (let ((text-style (port-acl-font->text-style port plain-font)))
    (cond (text-style 
	   (text-style-mapping-1 
	    port text-style 
	    :font-rotation-angle font-rotation-angle))
	  (t 
	   ;;; Make a best guess.
	   (let ((old-height (acl-font-height plain-font))
		 (italic-flag (acl-font-italic plain-font))
		 (underline-flag (acl-font-underlined plain-font))
		 (weight-flag (acl-font-weight plain-font)))
	     (make-windows-font 
	      old-height 
	      :escapement font-rotation-angle
	      :orientation font-rotation-angle
	      :italic (not (= italic-flag 0))
	      :underline (not (= underline-flag 0))
	      :weight weight-flag
	      ))))))

(defun pos-to-font-angle (x0 y0 x1 y1)
  (let ((dx (if (null x1) 0 (- x1 x0)))
	(dy (if (null y1) 0 (- y0 y1))))
    (cond ((= dy 0)
	   (if (< dx 0)
	       -1800
	     0))
	  ((= dx 0)
	   (if (< dy 0)
	       -900
	     900))
	  (t
	   (let* ((angfact #.(/ 180 pi))
		  (ang (* (atan dy dx) angfact)))
	     (round (* 10 ang)))))))

(defmethod port-acl-font->text-style ((port acl-port) acl-font)
  (with-slots (text-style->acl-font-mapping) port
    (when text-style->acl-font-mapping
      (catch :font-found 
	(maphash #'(lambda (KEY VAL)
		     (if (eql VAL ACL-FONT)
			 (throw :font-found KEY)))
		 text-style->acl-font-mapping)))))

(defmethod text-style-mapping-1
    ((port acl-port) (style text-style) &key font-rotation-angle)
  (multiple-value-bind (weight italic)
      (let ((face (text-style-face style)))
	(typecase face
	  (cons
	   (values (if (member :bold face) win:FW_BOLD win:FW_NORMAL)
		   (member :italic face)))
	  (otherwise
	   (case face
	     (:roman (values win:FW_NORMAL nil))
	     (:bold (values win:FW_BOLD nil))
	     (:italic (values win:FW_NORMAL t))
	     (otherwise (values win:FW_BOLD nil))))))
    (multiple-value-bind (family face-name)
	(case (text-style-family style)
	  (:fix (values (logior win:FIXED_PITCH win:FF_MODERN)
			#+ignore
			"courier"))
	  (:serif (values (logior win:VARIABLE_PITCH win:FF_ROMAN)
			  "times new roman"))
	  (:sans-serif (values (logior win:VARIABLE_PITCH win:FF_SWISS)
			       "arial"))
	  ;;--- some of these specify ugly ugly linedrawn fonts
	  (otherwise (values (logior win:DEFAULT_PITCH win:FF_DONTCARE)
			     (string (text-style-family style)))))
      (let ((size (text-style-size style))
	    (point-size nil))
	(when (numberp size)
	  (setq point-size size))
	(unless point-size
	  (setq point-size
	    (second (assoc size *acl-logical-size-alist*))))
	(unless point-size
	  (format *trace-output*
		  "~& Warning: ~S does not specify size, using 12" style)
	  (setq point-size 12))
	(if font-rotation-angle 
	    (make-windows-font 
	     (- (round (* point-size (slot-value port 'logpixelsy)) 72))
	     :weight weight 
	     :italic italic
	     :pitch-and-family family 
	     :face face-name	  
	     :escapement font-rotation-angle
	     :orientation font-rotation-angle)
	  (make-windows-font 
	   (- (round (* point-size (slot-value port 'logpixelsy)) 72))
	   :weight weight 
	   :italic italic
	   :pitch-and-family family 
	   :face face-name	  
	   ))))))

(defmethod text-style-mapping 
    ((port acl-port) (style list)
     &optional (character-set *standard-character-set*) etc)
  (text-style-mapping port (apply #'make-text-style style)
		      character-set etc))

(defmethod text-style-mapping
    ((port acl-port) (style text-style)
     &optional (character-set *standard-character-set*) etc)
  (declare (ignore character-set etc))
  (with-slots (text-style->acl-font-mapping) port
    (or (gethash style text-style->acl-font-mapping)
	(setf (gethash style text-style->acl-font-mapping)
	  (text-style-mapping-1 port style)))))

(defmethod text-style-mapping
    ((device acl-port) (style silica::device-font)
     &optional (character-set *standard-character-set*) etc)
  (declare (ignore character-set etc))
  (unless (eql device (silica::device-font-display-device style))
    (error "An attempt was made to map device font ~S on device ~S, ~@
	    but it is defined for device ~S"
	   style device (silica::device-font-display-device style)))
  (with-slots (text-style->acl-font-mapping) device
    (or (gethash style text-style->acl-font-mapping)
	(setf (gethash style text-style->acl-font-mapping)
	  (let ((name (silica::device-font-name style)))
	    (make-device-font (win:GetStockObject name)))))))

(defun make-font-width-table (dc last-character first-character default-width)
  (let* ((tepsize (ct:ccallocate win:size))
	 (string (make-string 1) )
	 (array (make-array (1+ last-character))))
    (loop for i from first-character to last-character do
	  (setf (char string 0) (code-char i))
	  (cond ((excl:with-native-string (string string)
		   (win:GetTextExtentPoint dc string 1 tepsize))
		 (setf (aref array i) (ct:cref win:size tepsize cx)))
		(t
		 ;; Why does this clause ever run?  getlasterror=10035.
		 (check-last-error "GetTextExtentPoint" :action :warn)
		 (setf (aref array i) default-width))))
    array))

(defun make-system-font ()
  (make-device-font (win:GetStockObject win:SYSTEM_FONT)))

;; This should be in the WIN package but isn't.
;; We use it to specify how Windows matches font requests to a
;; font currently installed on the user's system.  CLIM uses a
;; TrueType font when the user's system contains multiple fonts of
;; the same name.  With OUT_DEFAULT_PRECIS, :FIX fonts
;; were not always mapping to required sizes.  JPM 8/98.
(defconstant OUT_TT_PRECIS 4)

(defun make-windows-font
    (height &key (width 0) (escapement 0) (orientation 0)
		 (weight win:FW_NORMAL) 
		 (italic nil) (underline nil) (strikeout nil)
		 (charset win:ANSI_CHARSET) 
		 (output-precision OUT_TT_PRECIS)
		 (clip-precision WIN:CLIP_DEFAULT_PRECIS)
		 (quality win:PROOF_QUALITY) 
		 (pitch-and-family (logior win:DEFAULT_PITCH win:FF_DONTCARE)) 
		 (face nil) 
		 win-font) 
  (unless win-font
    (setq win-font
      (excl:with-native-string (vface (or face ""))
	(win:CreateFont height		; logical height
			width		; logical average width
			escapement	; angle of escapement (tenths of degrees)
			orientation	; normally the same as escapement
			weight		; font weight (FW_NORMAL=400, FW_BOLD=700)
			(if italic 1 0) 
			(if underline 1 0)
			(if strikeout 1 0) 
			charset		; if you want chinese or greek
			output-precision
			clip-precision
			quality
			pitch-and-family 
			vface
			))))
  (when (zerop win-font)
    (check-last-error "CreateFont"))
  (make-device-font win-font))

(defun make-device-font (win-font) 
  (let ((cw (and *application-frame*
		 (frame-top-level-sheet *application-frame*)
		 (sheet-mirror (frame-top-level-sheet *application-frame*))))
	(tmstruct (ct:ccallocate win:textmetric)))
    (unless cw (setf cw *current-window*))
    (unless (win:IsWindow cw) 
      ;; This clause is for the rare case that you are doing drawing
      ;; from a background process the first time you attempt to use
      ;; this font.  It doesn't really matter which frame you pick.
      (let* ((framem (find-frame-manager))
	     (frame (some #'(lambda (f)
			      (when (win:IsWindow
				     (sheet-mirror (frame-top-level-sheet
						    f)))
				f))
			  (when framem
			    (frame-manager-frames framem)))))
	(when frame
	  (setq cw (sheet-mirror (frame-top-level-sheet frame))))))
    (unless (win:IsWindow cw) 
      (error "No window found for calculating text font metrics."))
    (with-dc (cw dc)
      (win:SetMapMode dc win:MM_TEXT)
      (selectobject dc win-font)
      (or (win:GetTextMetrics dc tmstruct)
	  (check-last-error "GetTextMetrics"))
      (let ((average-character-width
	     (ct:cref win:textmetric tmstruct tmavecharwidth))
	    (maximum-character-width
	     (ct:cref win:textmetric tmstruct tmmaxcharwidth))
	    (last-character (ct:cref win:textmetric tmstruct tmlastchar))
	    (first-character (ct:cref win:textmetric tmstruct tmfirstchar))
	    (font-width-array-or-nil nil))
	(setq font-width-array-or-nil
	  (and (/= average-character-width maximum-character-width)
	       (make-font-width-table dc last-character first-character
				      maximum-character-width)))
	(make-acl-font
	 :index win-font 
	 :height (ct:cref win:textmetric tmstruct tmheight)
	 :ascent (ct:cref win:textmetric tmstruct tmascent)
	 :descent (ct:cref win:textmetric tmstruct tmdescent)
	 :internal-leading (ct:cref win:textmetric tmstruct tminternalleading)
	 :external-leading (ct:cref win:textmetric tmstruct tmexternalleading)
	 :average-character-width average-character-width        
	 :maximum-character-width maximum-character-width 
	 :weight (ct:cref win:textmetric tmstruct tmweight)
	 :italic (ct:cref win:textmetric tmstruct tmitalic)
	 :underlined (ct:cref win:textmetric tmstruct tmunderlined)
	 :struckout (ct:cref win:textmetric tmstruct tmstruckout)
	 :first-character first-character 
	 :last-character last-character 
	 :default-character (ct:cref win:textmetric tmstruct tmdefaultchar)
	 :break-character (ct:cref win:textmetric tmstruct tmbreakchar)
	 :overhang (ct:cref win:textmetric tmstruct tmoverhang)
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

;; The second element of each item is passed to
;; LoadCursor and SetCursor.
(defvar *win-cursor-type-alist*
    `((:appstarting ,win:IDC_APPSTARTING)
      (:default ,win:IDC_ARROW)
      (:position ,win:IDC_CROSS)
      (:ibeam ,win:IDC_IBEAM)
      #+ignore
      (:no ,win:IDC_NO)
      (:move ,win:IDC_SIZEALL)
      (:vertical-scroll ,win:IDC_SIZENS)
      (:horizontal-scroll ,win:IDC_SIZEWE)
      (:scroll-up ,win:IDC_UPARROW)
      (:busy ,win:IDC_WAIT)))

(defmethod port-set-pointer-cursor ((port acl-port) pointer cursor)
  (unless (eq (pointer-cursor pointer) cursor)
    (win:SetCursor (realize-cursor port cursor)) ; mouse cursor
    )
  cursor)

(defmethod port-set-sheet-pointer-cursor ((port acl-port) sheet cursor)
  (unless (eq (sheet-pointer-cursor sheet) cursor) (set-cursor sheet cursor))
  cursor)

(defmethod realize-cursor ((port acl-port) (cursor symbol))
  (let ((cursor (or (second (assoc cursor *win-cursor-type-alist*))
		    win:IDC_ARROW)))
    (realize-cursor port cursor)))

(defvar *loaded-cursors* nil)

(defmethod realize-cursor ((port acl-port) (cursor number))
  (let* ((result (second (assoc cursor *loaded-cursors*))))
    (unless result
      (setq result 
	(win:LoadCursor 0 cursor))
      (when (zerop result)
	;; Suppress the error for now to be compatible with
	;; previous versions of CLIM.  It would be nice to
	;; figure out what is causing this and fix it.  JPM 6/98.
	(check-last-error "LoadCursor" :action :warn))
      (push (list cursor result) *loaded-cursors*))
    result))

(defmethod realize-cursor ((port acl-port) (cursor t)) 
  cursor)

(defmethod realize-cursor :around ((port acl-port) cursor)
  (with-slots (cursor-cache) port
    (or (getf cursor-cache cursor)
	(setf (getf cursor-cache cursor)
	  (call-next-method)))))

;; pointer-grabbing - how do you do this on windows? - the following
;; simply deals with the handling of the cursor keyword (cim 10/14/96)

;; I think you do this on windows by using SetCapture
;; and ReleaseCapture.  JPM 5/98.

(defmethod port-invoke-with-pointer-grabbed
    ((port acl-port) (sheet basic-sheet) continuation
     &key cursor &allow-other-keys)
  (clim-utils:letf-globally (((port-grab-cursor port) cursor))
    (funcall continuation)))

;; X and Y are in native coordinates
(defmethod port-set-pointer-position ((port acl-port) pointer x y)
  (declare (ignore pointer))
  (fix-coordinates x y)
  (or (win:SetCursorPos x y)
      (check-last-error "SetCursorPos")))

(defmethod clim-internals::port-query-pointer ((port acl-port) sheet)
  (let ((point (ct:ccallocate win:point))
	(native-x 0)(native-y 0))
    (or (win:GetCursorPos point)
	(check-last-error "GetCursorPos"))
    (let ((root-x (ct:cref win:point point x))
	  (root-y (ct:cref win:point point y)))
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
  ;; X and Y come straight from WM_MOUSEMOVE.
  ;; Take care not to declare motion-pending in the case where there
  ;; was no motion, WM_MOUSEMOVE gives you more than you need.
  (with-slots (pointer-sheet motion-pending pointer-x pointer-y) port
    (cond ((and (eq x pointer-x)
		(eq y pointer-y))
	   nil)
	  (t
	   #+debug
	   (format *trace-output* 
		   "~% Was ~A ~A ~A IS ~A ~A ~A~%"
		   pointer-sheet pointer-x pointer-y
		   sheet x y)
	   (setf pointer-sheet sheet)
	   (setf pointer-x x)
	   (setf pointer-y y)
	   (setf motion-pending t)
	   t))))

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
(defun windows-mask->modifier-state (mask &optional double)
  (let ((state 
	 (if (logtest win:MK_SHIFT mask)
	     (if (logtest win:MK_CONTROL mask)
		 (make-modifier-state :shift :control)
	       (make-modifier-state :shift))
	   (if (logtest win:MK_CONTROL mask)
	       (make-modifier-state :control)
	     (make-modifier-state)))))
    (if double 
	(logior state (make-modifier-state :double))
      state)))

(defmethod event-handler ((port acl-port) args)
  (declare (ignore args))
  (cerror "Go on" "What isn't firing?")
  nil)

;; The fact that we need this method gives me the uneasy feeling
;; that we have addressed the symptom and not the cause.  The
;; default method does some port-trace-thing which sometimes
;; generates the wrong sheet.  There are no comments regarding
;; port-trace-thing, so there no way to know if it is broken.  
;; For example, SHEET might be a spacing pane, which is unable
;; to queue an event and therefore fails to distribute it.
;; JPM 10/98.
(defmethod silica::distribute-event-1 ((port acl-port) 
				       (event silica::window-change-event))
  (declare (optimize (speed 3)))
  (let ((sheet (or (silica::event-mswin-control event) (event-sheet event))))
    (silica::dispatch-pointer-event-to-sheet port event sheet)))

(defvar *l-counter* 0)
(defvar *nowait* nil)

(defvar *clim-pulse-rate* 1.0)		; seconds.

;; Redefine function from utils/processes.lisp.
(defun clim-utils::process-wait (state function &rest args)
  (declare (dynamic-extent function args))
  #+someday
  (apply #'mp:process-wait state function args)
  (let ((result nil))
    (loop
      ;; 5/28/98 JPM ACL 5.0.beta
      ;; There seems to be a bug in process-wait (on NT) that
      ;; it does not run the test function often enough.
      ;; Lisp will appear to hang until it receives some Windows
      ;; events, which apparently wake up the scheduler.
      ;; The workaround is to wake up every so often and
      ;; run the test function.
      (when (setq result (apply #'mp:process-wait-with-timeout 
				state *clim-pulse-rate* 
				function args))
	(return result)))))
  

(defmethod process-next-event ((port acl-port)
			       &key (timeout nil) 
				    (wait-function nil)
				    (state "Windows Event"))
  (with-slots (event-queue motion-pending) port
    (let ((event (queue-get event-queue))
	  (sheet (frame-top-level-sheet *application-frame*))
	  (reason nil))
      (unless event 
	(flet ((wait-for-event ()
		 (process-pending-messages t (and sheet (sheet-mirror sheet)))
		 ;;(sys::process-pending-events t)
		 (when motion-pending
		   (flush-pointer-motion port))
		 (or event
		     (setq event (queue-get event-queue))
		     (and wait-function
			  (funcall wait-function)
			  (setq reason :wait-function)))))
	  (if timeout
	      (mp:process-wait-with-timeout state timeout
					    #'wait-for-event)
	    (process-wait state #'wait-for-event))))
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
      (unless (win:IsIconic mirror)
        (let ((native-region (mirror-region port sheet)))
	  (setf (slot-value event 'silica::native-region) native-region)
	  (setf (slot-value event 'silica::region)
	        (untransform-region (sheet-native-transformation sheet) 
				    native-region)))
        (call-next-method)))))

(defun get-system-version ()
  "Use win:GetVersion to determine the operating system being used."
  (let* ((v (win:GetVersion))
         (vh (hiword v)))
    (if (>= vh 0) :winnt :win31)))

(defun silica::acl-mirror-with-focus ()
  (acl-port-mirror-with-focus *acl-port*))


;;; gestures for handling middle button with two button mice

(define-gesture-name :describe :pointer-button (:right :control) :unique nil)
(define-gesture-name :delete :pointer-button (:right :control :shift) :unique nil)

