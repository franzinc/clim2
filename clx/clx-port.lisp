;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
;;; $fiHeader: clx-port.lisp,v 1.3 92/03/04 16:20:55 cer Exp $
 Portions copyright (c) 1991, 1992 International Lisp Associates."


(defclass clx-port (port)
    ((display :initform nil :reader port-display)
     (screen :initform nil)
     (window :initform nil)
     (cursor-font)
     (cursor-cache :initform nil :type nil)
     (stipple-gc)
     (default-grab-cursor :reader default-grab-cursor)
     (context :reader port-context)
     (width-pixels)
     (height-pixels)
     (type :allocation :class 
	   :initform :clx :reader port-type))
  (:default-initargs :allow-loose-text-style-size-mapping t))

(defmethod find-port-type ((type (eql ':clx)))
  'clx-port)

(defvar *clx-white-color* (xlib:make-color :red 1 :blue 1 :green 1))
(defvar *clx-black-color* (xlib:make-color :red 0 :blue 0 :green 0))
(defvar *cursor-font* nil)

(defmethod initialize-instance :after ((port clx-port) &key server-path)
  (destructuring-bind (type &key host screen display-number) server-path
    (declare (ignore type))
    (when (null host)
      (setq host #+Genera  (scl:send neti:*local-host* :name)
		 #+Minima  (machine-instance)
		 #+Allegro (or (system:getenv "DISPLAY") (short-site-name))
		 #+Lucid   (or (lcl:environment-variable "DISPLAY") (machine-instance))))
    (multiple-value-bind (host display-number nscreen)
	(disassemble-display-spec host (or display-number 0) (or screen 0))
      (with-slots (display screen window height-pixels width-pixels
		   stipple-gc default-grab-cursor) port
	(setf display #-Allegro (xlib:open-display host :display display-number)
		      #+Allegro (open-display-with-auth host :display-number display-number))
	(fill-keycode->modifier-state display)
	(fill-clim-modifier-key-index->x-state display)
	(setf screen (nth nscreen (xlib:display-roots display)))
	(setf window (xlib:screen-root screen))
	(setf width-pixels (xlib:screen-width screen))
	(setf height-pixels (xlib:screen-height screen))
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
					:background *clx-white-color*))))
    (initialize-clx-port port)))

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


;;; Text styles for CLX windows

(defvar *clx-font-families*
	'((:fix "-*-courier-*")
	  (:sans-serif "-*-helvetica-*")
	  (:serif "-*-charter-*" "-*-new century schoolbook-*" "-*-times-*")))

(defvar *clx-fallback-font* "8x13"
  "When non NIL and nothing better exists use this as the fallback font")

(defmethod initialize-clx-port ((port clx-port))
  (with-slots (screen display) port
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
			    port text-style *standard-character-set* t)
		    (setf (text-style-mapping port text-style) xfont)))))
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
			     port `(,(car family) :roman 10))
		       (return (make-text-style (car family) :roman 10)))))
	     (setf (text-style-mapping
		     port (port-undefined-text-style port))
		   temp))
	    ;; Perhaps we should look for some other conveniently sized
	    ;; fonts.
	    (*clx-fallback-font*
	     (setf (text-style-mapping
		     port (port-undefined-text-style port))
		   (xlib:open-font display
				   *clx-fallback-font*)))
	    ;; Perhaps we should just grab the first font we can find.
	    (t
	     (error "Unable to determine default font"))))))

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

(defmethod text-style-mapping :around ((port clx-port) text-style
				       &optional (character-set *standard-character-set*) etc)
  (declare (ignore etc))
  (let ((font (call-next-method)))
    (when (or (stringp font) (symbolp font))
      (let* ((font-name (string font)))
	(with-slots (display) port
	  (setf font (xlib:open-font display font-name)))
	(setf (text-style-mapping
		port (parse-text-style text-style) character-set)
	      font)))
    font))

(defparameter *clx-logical-size-alist*
	      '((:tiny       6)
		(:very-small 8)
		(:small	     10)
		(:normal     12)
		(:large	     14)
		(:very-large 18)
		(:huge	     24)))

(defmethod standardize-text-style ((port clx-port) style 
				   &optional (character-set *standard-character-set*))
  (standardize-text-style-1
    port style character-set *clx-logical-size-alist*))


(defmethod port-glyph-for-character ((port clx-port)
				     character text-style &optional our-font)
  (declare (values index font escapement-x escapement-y origin-x origin-y bb-x bb-y))
  (multiple-value-bind (character-set index)
      (char-character-set-and-index character)
    (when (eq character-set *standard-character-set*)
      (setf index (xlib:char->card8 character)))	;A little gross, but right, I think.
    (let* ((x-font (or our-font
		       (text-style-mapping port text-style character-set)))
	   (escapement-x (or (xlib:char-width x-font index)
			     ;;--- Sometimes the above can return NIL
			     (xlib:char-width x-font (xlib:char->card8 #\A))))
	   (escapement-y 0)
	   (origin-x 0)
	   (origin-y (xlib:font-ascent x-font))
	   (bb-x escapement-x)
	   (bb-y (+ origin-y (xlib:font-descent x-font))))
      (values index x-font escapement-x escapement-y origin-x origin-y bb-x bb-y))))

(defmethod clx-get-default-font ((port clx-port) medium)
  (text-style-mapping port (medium-merged-text-style medium)))

(defmethod text-style-width ((text-style standard-text-style) (port clx-port))
  (let ((font (text-style-mapping port text-style)))
    ;; Disgusting, but probably OK
    (xlib:char-width font (xlib:char->card8 #\A))))

(defmethod text-style-height ((text-style standard-text-style) (port clx-port))
  (let ((font (text-style-mapping port text-style)))
    (let* ((ascent (xlib:font-ascent font))
	   (descent (xlib:font-descent font))
	   (height (+ ascent descent)))
      height)))

(defmethod text-style-ascent ((text-style standard-text-style) (port clx-port))
  (let ((font (text-style-mapping port text-style)))
    (xlib:font-ascent font)))

(defmethod text-style-descent ((text-style standard-text-style) (port clx-port))
  (let ((font (text-style-mapping port text-style)))
    (xlib:font-descent font)))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (port clx-port))
  (let ((font (text-style-mapping port text-style)))
    ;; Really disgusting, but probably OK
    (= (xlib:char-width font (xlib:char->card8 #\.))
       (xlib:char-width font (xlib:char->card8 #\M)))))


(defvar *clx-keysym->clim-keysym-table*
	(make-hash-table :test (xlib::keysym->character-map-test)))
(defvar *clim-keysym->clx-keysym-table* (make-hash-table))

(defmacro define-clx-keysym (clx-keysym clim-keysym)
  `(progn 
     (setf (gethash ,clx-keysym *clx-keysym->clim-keysym-table*) ',clim-keysym)
     (unless (gethash ,clim-keysym *clim-keysym->clx-keysym-table*)
       (setf (gethash ,clim-keysym *clim-keysym->clx-keysym-table*) ',clx-keysym))))

(defun-inline clx-keysym->keysym (clx-keysym)
  (gethash clx-keysym *clx-keysym->clim-keysym-table*))

(defun-inline keysym->clx-keysym (keysym)
  (gethash keysym *clim-keysym->clx-keysym-table*))

(defmethod port-canonical-gesture-spec (gesture-spec (port clx-port))
  ;; here, we must take the gesture spec, turn it back into
  ;; a keycode, then see what the keysyms are for that keycode
  (let* ((x-display (slot-value port 'display))
	 (keysym (if (listp gesture-spec) (first gesture-spec) gesture-spec))
	 (shifts (and (listp gesture-spec) (rest gesture-spec)))
	 (x-keysym (if (standard-char-p keysym)
		       keysym
		       (keysym->clx-keysym keysym)))
	 (x-keycode nil))
    (unless x-keysym 
      (return-from port-canonical-gesture-spec nil))
    (setq x-keycode (xlib:keysym->keycodes x-display x-keysym))
    ;; will this ever happen?
    (when (listp x-keycode)
      (setq x-keycode (first x-keycode)))
    ;; now need to figure out necessary shifts for this character
    (when x-keycode
      ;; [for now, just check shift...]
      ;; This could be written to iterate over all possible shift masks seeing which
      ;; of them are required to type this particular character.  That could also be
      ;; cached, I suppose.  We'll just do :SHIFT for now.
      (when (= (xlib:keycode->keysym 
		 x-display x-keycode
		 (xlib:default-keysym-index 
		   x-display x-keycode (make-modifier-state :shift)))
	       x-keysym)
	(setq shifts (logior shifts (make-modifier-state :shift))))
      ;; Now SHIFTS includes any shift that was necessary to type the original
      ;; keysym.  Now, we backtranslate the keysym and modifier-state we now
      ;; have into a new gesture.
      (cons
	(clx-keysym->keysym
	  (xlib:keycode->keysym 
	    x-display x-keycode
	    (xlib:default-keysym-index x-display x-keycode shifts)))
	shifts))))

;; The standard characters
(define-clx-keysym 032 :space)
(define-clx-keysym 033 :\!)
(define-clx-keysym 034 :\")
(define-clx-keysym 035 :\#)
(define-clx-keysym 036 :\$)
(define-clx-keysym 037 :\%)
(define-clx-keysym 038 :\&)
(define-clx-keysym 039 :\')
(define-clx-keysym 040 :\()
(define-clx-keysym 041 :\))
(define-clx-keysym 042 :\*)
(define-clx-keysym 043 :\+)
(define-clx-keysym 044 :\,)
(define-clx-keysym 045 :\-)
(define-clx-keysym 046 :\.)
(define-clx-keysym 047 :\/)
(define-clx-keysym 048 :\0)
(define-clx-keysym 049 :\1)
(define-clx-keysym 050 :\2)
(define-clx-keysym 051 :\3)
(define-clx-keysym 052 :\4)
(define-clx-keysym 053 :\5)
(define-clx-keysym 054 :\6)
(define-clx-keysym 055 :\7)
(define-clx-keysym 056 :\8)
(define-clx-keysym 057 :\9)
(define-clx-keysym 058 :\:)
(define-clx-keysym 059 :\;)
(define-clx-keysym 060 :\<)
(define-clx-keysym 061 :\=)
(define-clx-keysym 062 :\>)
(define-clx-keysym 063 :\?)
(define-clx-keysym 064 :\@)
(define-clx-keysym 065 :\A)
(define-clx-keysym 097 :\A)
(define-clx-keysym 066 :\B)
(define-clx-keysym 098 :\B)
(define-clx-keysym 067 :\C)
(define-clx-keysym 099 :\C)
(define-clx-keysym 068 :\D)
(define-clx-keysym 100 :\D)
(define-clx-keysym 069 :\E)
(define-clx-keysym 101 :\E)
(define-clx-keysym 070 :\F)
(define-clx-keysym 102 :\F)
(define-clx-keysym 071 :\G)
(define-clx-keysym 103 :\G)
(define-clx-keysym 072 :\H)
(define-clx-keysym 104 :\H)
(define-clx-keysym 073 :\I)
(define-clx-keysym 105 :\I)
(define-clx-keysym 074 :\J)
(define-clx-keysym 106 :\J)
(define-clx-keysym 075 :\K)
(define-clx-keysym 107 :\K)
(define-clx-keysym 076 :\L)
(define-clx-keysym 108 :\L)
(define-clx-keysym 077 :\M)
(define-clx-keysym 109 :\M)
(define-clx-keysym 078 :\N)
(define-clx-keysym 110 :\N)
(define-clx-keysym 079 :\O)
(define-clx-keysym 111 :\O)
(define-clx-keysym 080 :\P)
(define-clx-keysym 112 :\P)
(define-clx-keysym 081 :\Q)
(define-clx-keysym 113 :\Q)
(define-clx-keysym 082 :\R)
(define-clx-keysym 114 :\R)
(define-clx-keysym 083 :\S)
(define-clx-keysym 115 :\S)
(define-clx-keysym 084 :\T)
(define-clx-keysym 116 :\T)
(define-clx-keysym 085 :\U)
(define-clx-keysym 117 :\U)
(define-clx-keysym 086 :\V)
(define-clx-keysym 118 :\V)
(define-clx-keysym 087 :\W)
(define-clx-keysym 119 :\W)
(define-clx-keysym 088 :\X)
(define-clx-keysym 120 :\X)
(define-clx-keysym 089 :\Y)
(define-clx-keysym 121 :\Y)
(define-clx-keysym 090 :\Z)
(define-clx-keysym 122 :\Z)
(define-clx-keysym 091 :\[)
(define-clx-keysym 092 :\\)
(define-clx-keysym 093 :\])
(define-clx-keysym 094 :\^)
(define-clx-keysym 095 :\_)
(define-clx-keysym 096 :\`)
(define-clx-keysym 123 :\{)
(define-clx-keysym 124 :\|)
(define-clx-keysym 125 :\})
(define-clx-keysym 126 :\~)

;; The semi-standard characters
(define-clx-keysym (xlib::keysym 255 013) :return)
(define-clx-keysym (xlib::keysym 255 009) :tab)
(define-clx-keysym (xlib::keysym 255 255) :rubout)
(define-clx-keysym (xlib::keysym 255 008) :backspace)
(define-clx-keysym (xlib::keysym 009 227) :page)
(define-clx-keysym (xlib::keysym 255 010) :linefeed)

;; Other useful characters
(define-clx-keysym (xlib::keysym 255 087) :end)
(define-clx-keysym (xlib::keysym 255 105) :abort)
(define-clx-keysym (xlib::keysym 255 106) :help)
(define-clx-keysym (xlib::keysym 255 104) :complete)
(define-clx-keysym (xlib::keysym 255 086) :scroll)
(define-clx-keysym (xlib::keysym 255 097) :refresh)
(define-clx-keysym (xlib::keysym 255 011) :clear-input)

;; Finally, the shifts
(define-clx-keysym xlib::left-shift-keysym :left-shift)
(define-clx-keysym xlib::right-shift-keysym :right-shift)
(define-clx-keysym xlib::left-control-keysym :left-control)
(define-clx-keysym xlib::right-control-keysym :right-control)
(define-clx-keysym xlib::caps-lock-keysym :caps-lock)
(define-clx-keysym xlib::shift-lock-keysym :shift-lock)
(define-clx-keysym xlib::left-meta-keysym :left-meta)
(define-clx-keysym xlib::right-meta-keysym :right-meta)
(define-clx-keysym xlib::left-super-keysym :left-super)
(define-clx-keysym xlib::right-super-keysym :right-super)
(define-clx-keysym xlib::left-hyper-keysym :left-hyper)
(define-clx-keysym xlib::right-hyper-keysym :right-hyper)


(defvar *clx-cursor-type-alist*
  '((:default 132)
    (:vertical-scroll 116)
    (:scroll-up 114)
    (:scroll-down 106)
    (:horizontal-scroll 108)
    (:scroll-left 110)
    (:scroll-right 112)
    (:busy 150)
    (:upper-left 134)
    (:upper-right 136)
    (:lower-left 12)
    (:lower-right 14)
    (:vertical-thumb 112)
    (:horizontal-thumb 114)
    (:button 132)
    (:prompt 92)
    (:move 52)
    (:position 34)))

(defmethod install-port-cursor ((port clx-port) sheet cursor)
  (unless (eq cursor (port-cursor port))
    (with-slots (display) port
      (setf (port-cursor port) cursor)
      (setf (xlib:window-cursor (sheet-mirror sheet))
	    (realize-cursor port cursor))
      (xlib:display-force-output display))
    cursor))

(defmethod realize-cursor :around ((port clx-port) cursor)
  (with-slots (cursor-cache) port
    (or (getf cursor-cache cursor)
	(setf (getf cursor-cache cursor)
	      (call-next-method)))))

(defmethod realize-cursor ((port clx-port) (cursor symbol))
  (let ((cursor (or (second (assoc cursor *clx-cursor-type-alist*))
		    132)))
    (realize-cursor port cursor)))

(defmethod realize-cursor ((port clx-port) (cursor number))
  (with-slots (cursor-font screen) port
    (xlib:create-glyph-cursor
      :source-font cursor-font
      :source-char cursor
      :mask-font cursor-font
      :mask-char (1+ cursor)
      ;;--- Should query for black and white or use the b/w from screen
      :foreground (xlib:make-color :red   0.0 :green 0.0 :blue  0.0)
      :background (xlib:make-color :red   1.0 :green 1.0 :blue  1.0))))

(defmethod set-cursor-location ((port clx-port) sheet x y)
  (xlib:warp-pointer (sheet-mirror sheet)
		     (integerize-coordinate x)
		     (integerize-coordinate y)))

(defmethod port-note-cursor-change ((port clx-port) cursor stream type old new)
  (declare (ignore type old))
  ;;--- Where should this be, really?
  (setf (port-keyboard-input-focus port) (and new stream)))


;;--- Doesn't this need a sheet/stream argument?
(defmethod port-force-output ((port clx-port))
  (xlib:display-force-output (port-display port)))

(defmethod port-finish-output ((port clx-port))
  (xlib:display-finish-output (port-display port)))


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
	  (length (length clim-internals::*modifier-keys*)))
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

