;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-
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
;; $Id: clx-port.lisp,v 1.21 1998/08/06 23:16:18 layer Exp $

(in-package :clx-clim)

;;;"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1991, 1992 International Lisp Associates."


(defclass clx-port (basic-port)
    ((display :initform nil :reader port-display)
     (screen :initform nil)			;--- should be on the medium
     (window :initform nil)
     (color-p :initform nil)			;--- should be on graft
     (cursor-font)
     (cursor-cache :initform nil)
     (stipple-gc)
     (default-grab-cursor :reader default-grab-cursor)
     (context :reader port-context)
     (width-pixels)
     (height-pixels)
     (port-name))
  (:default-initargs :allow-loose-text-style-size-mapping t))

(defmethod find-port-type ((type (eql ':clx)))
  'clx-port)

(defmethod port-type ((port clx-port))
  ':clx)

(defmethod port-name ((port clx-port))
  (let ((keys (cdr (port-server-path port)))
	(port-name (slot-value port 'port-name)))
    (format nil "~A:~D ~D" 
      (or (getf keys :host) (first port-name))
      (or (getf keys :display) (second port-name))
      (or (getf keys :screen) (third port-name)))))


(defvar *clx-white-color* (xlib:make-color :red 1 :blue 1 :green 1))
(defvar *clx-black-color* (xlib:make-color :red 0 :blue 0 :green 0))

(defparameter *clx-use-color-p* t)

;;--- Eventually do better than this
(defclass clx-palette (basic-palette) ())

(defmethod make-palette ((port clx-port) &key color-p dynamic-p)
  (make-instance 'clx-palette
    :port port 
    :color-p color-p
    :dynamic-p dynamic-p))


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
		   stipple-gc default-grab-cursor color-p port-name) port
	(setf port-name (list host display-number nscreen))
	(setf display #-Allegro (xlib:open-display host :display display-number)
		      #+Allegro (open-display-with-auth host :display-number display-number))
	(fill-keycode->modifier-state display)
	(fill-clim-modifier-key-index->x-state display)
	(setf screen (nth nscreen (xlib:display-roots display)))
	(setf window (xlib:screen-root screen))
	(setf width-pixels (xlib:screen-width screen))
	(setf height-pixels (xlib:screen-height screen))
	(let ((vic (xlib:visual-info-class 
		     (xlib:window-visual-info (xlib:screen-root screen)))))
	  (setf color-p (and *clx-use-color-p*
			     (ecase vic
			       ((:static-gray :gray-scale) nil)
			       ((:static-color :true-color :pseudo-color :direct-color) t))))
	  (setf stipple-gc (xlib:create-gcontext
			     :drawable window
			     :foreground (xlib:screen-black-pixel screen)
			     :background (xlib:screen-white-pixel screen)))
	  (setf (slot-value port 'silica::default-palette) 
		(make-palette port :color-p color-p
				   :dynamic-p (and (member vic '(:gray-scale
								 :pseudo-color
								 :direct-color))
						   t))))
 	(prog1
	  (initialize-clx-port port)		;compute cursor font now
	  (let ((cursor-font (slot-value port 'cursor-font)))
	    (setf default-grab-cursor
		  (xlib:create-glyph-cursor :source-font cursor-font
					    :mask-font cursor-font
					    ;; Northeast arrow
					    :source-char 2
					    :mask-char 3
					    :foreground *clx-black-color*
					    :background *clx-white-color*))))))))

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
  (with-slots (screen display cursor-font) port
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
    (setq cursor-font (xlib:open-font display "cursor"))
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


(defvar *clx-keysym->clim-keysym-table*
	(make-hash-table :test (xlib::keysym->character-map-test)))
(defvar *clim-keysym->clx-keysym-table* (make-hash-table))

(defmacro define-clx-keysym (clx-keysym clim-keysym)
  `(progn 
     (setf (gethash ,clx-keysym *clx-keysym->clim-keysym-table*) ,clim-keysym)
     (unless (gethash ,clim-keysym *clim-keysym->clx-keysym-table*)
       (setf (gethash ,clim-keysym *clim-keysym->clx-keysym-table*) ,clx-keysym))))

(defun-inline clx-keysym->keysym (clx-keysym)
  (gethash clx-keysym *clx-keysym->clim-keysym-table*))

(defun-inline keysym->clx-keysym (keysym)
  (gethash keysym *clim-keysym->clx-keysym-table*))

;; If MODIFIER-STATE is supplied, then GESTURE-SPEC is assumed to be a keysym.
;; Otherwise, we parse GESTURE-SPEC as a gesture spec.
(defmethod port-canonicalize-gesture-spec 
	   ((port clx-port) gesture-spec &optional modifier-state)
  (multiple-value-bind (keysym shifts)
      (if modifier-state
	  (values gesture-spec modifier-state)
	  (parse-gesture-spec gesture-spec))
    ;; Here, we must take the gesture spec, turn it back into
    ;; a keycode, then see what the keysyms are for that keycode
    (let* ((x-display (slot-value port 'display))
	   (x-keysym (if (characterp keysym)
			 (keysym->clx-keysym (or (clx-keysym->keysym (char-code keysym))
						 (clx-keysym->keysym keysym)))
			 (keysym->clx-keysym keysym)))
	   (x-keycode nil))
      (unless x-keysym 
	(return-from port-canonicalize-gesture-spec nil))
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
	(when (and (not (or (<= (char-code #\A) x-keysym (char-code #\Z))
			    (<= (char-code #\a) x-keysym (char-code #\z))))
		   (not (member keysym '(:return :newline #\Return #\Newline)))
		   (= (xlib:keycode->keysym 
			x-display x-keycode
			(xlib:default-keysym-index 
			  x-display x-keycode (make-modifier-state :shift)))
		      x-keysym))
	  (setq shifts (logior shifts (make-modifier-state :shift))))
	;; Now SHIFTS includes any shift that was necessary to type the original
	;; keysym.  Backtranslate the keysym and modifier-state into a new gesture.
	(cons (clx-keysym->keysym
		(xlib:keycode->keysym 
		  x-display x-keycode
		  (xlib:default-keysym-index x-display x-keycode shifts)))
	      shifts)))))

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
(define-clx-keysym 065 :a)
(define-clx-keysym 097 :a)
(define-clx-keysym 066 :b)
(define-clx-keysym 098 :b)
(define-clx-keysym 067 :c)
(define-clx-keysym 099 :c)
(define-clx-keysym 068 :d)
(define-clx-keysym 100 :d)
(define-clx-keysym 069 :e)
(define-clx-keysym 101 :e)
(define-clx-keysym 070 :f)
(define-clx-keysym 102 :f)
(define-clx-keysym 071 :g)
(define-clx-keysym 103 :g)
(define-clx-keysym 072 :h)
(define-clx-keysym 104 :h)
(define-clx-keysym 073 :i)
(define-clx-keysym 105 :i)
(define-clx-keysym 074 :j)
(define-clx-keysym 106 :j)
(define-clx-keysym 075 :k)
(define-clx-keysym 107 :k)
(define-clx-keysym 076 :l)
(define-clx-keysym 108 :l)
(define-clx-keysym 077 :m)
(define-clx-keysym 109 :m)
(define-clx-keysym 078 :n)
(define-clx-keysym 110 :n)
(define-clx-keysym 079 :o)
(define-clx-keysym 111 :o)
(define-clx-keysym 080 :p)
(define-clx-keysym 112 :p)
(define-clx-keysym 081 :q)
(define-clx-keysym 113 :q)
(define-clx-keysym 082 :r)
(define-clx-keysym 114 :r)
(define-clx-keysym 083 :s)
(define-clx-keysym 115 :s)
(define-clx-keysym 084 :t)
(define-clx-keysym 116 :t)
(define-clx-keysym 085 :u)
(define-clx-keysym 117 :u)
(define-clx-keysym 086 :v)
(define-clx-keysym 118 :v)
(define-clx-keysym 087 :w)
(define-clx-keysym 119 :w)
(define-clx-keysym 088 :x)
(define-clx-keysym 120 :x)
(define-clx-keysym 089 :y)
(define-clx-keysym 121 :y)
(define-clx-keysym 090 :z)
(define-clx-keysym 122 :z)
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
(define-clx-keysym (xlib::keysym 255 013) :newline)
(define-clx-keysym (xlib::keysym 255 009) :tab)
(define-clx-keysym (xlib::keysym 255 255) :rubout)
(define-clx-keysym (xlib::keysym 255 008) :backspace)
(define-clx-keysym (xlib::keysym 009 227) :page)
(define-clx-keysym (xlib::keysym 255 010) :linefeed)
(define-clx-keysym (xlib::keysym 255 027) :escape)

;; Another way, just to be sure
(define-clx-keysym #\Return    :return)
(define-clx-keysym #\Newline   :newline)

;; Other useful characters
(define-clx-keysym (xlib::keysym 255 087) :end)
(define-clx-keysym (xlib::keysym 255 105) :abort)
(define-clx-keysym (xlib::keysym 255 106) :help)
(define-clx-keysym (xlib::keysym 255 104) :complete)
(define-clx-keysym (xlib::keysym 255 086) :scroll)
(define-clx-keysym (xlib::keysym 255 097) :refresh)
(define-clx-keysym (xlib::keysym 255 011) :clear-input)

;; Finally, the shifts
(define-clx-keysym xlib::left-shift-keysym    :left-shift)
(define-clx-keysym xlib::right-shift-keysym   :right-shift)
(define-clx-keysym xlib::left-control-keysym  :left-control)
(define-clx-keysym xlib::right-control-keysym :right-control)
(define-clx-keysym xlib::caps-lock-keysym     :caps-lock)
(define-clx-keysym xlib::shift-lock-keysym    :shift-lock)
(define-clx-keysym xlib::left-meta-keysym     :left-meta)
(define-clx-keysym xlib::right-meta-keysym    :right-meta)
(define-clx-keysym xlib::left-super-keysym    :left-super)
(define-clx-keysym xlib::right-super-keysym   :right-super)
(define-clx-keysym xlib::left-hyper-keysym    :left-hyper)
(define-clx-keysym xlib::right-hyper-keysym   :right-hyper)

;; Non-standard keys found on Sun keyboards

(define-clx-keysym (xlib::keysym 255 #xB3) :f1)
(define-clx-keysym (xlib::keysym 255 #xBF) :f2)
(define-clx-keysym (xlib::keysym 255 #xC0) :f3)
(define-clx-keysym (xlib::keysym 255 #xC1) :f4)
(define-clx-keysym (xlib::keysym 255 #xC2) :f5)
(define-clx-keysym (xlib::keysym 255 #xC3) :f6)
(define-clx-keysym (xlib::keysym 255 #xC4) :f7)
(define-clx-keysym (xlib::keysym 255 #xC5) :f8)
(define-clx-keysym (xlib::keysym 255 #xC6) :f9)
(define-clx-keysym (xlib::keysym 255 #xC7) :f10)

(define-clx-keysym (xlib::keysym 255 #xC8) :l1)
(define-clx-keysym (xlib::keysym 255 #xC9) :l2)
(define-clx-keysym (xlib::keysym 255 #xCA) :l3)
(define-clx-keysym (xlib::keysym 255 #xCB) :l4)
(define-clx-keysym (xlib::keysym 255 #xCC) :l5)
(define-clx-keysym (xlib::keysym 255 #xCD) :l6)
(define-clx-keysym (xlib::keysym 255 #xCE) :l7)
(define-clx-keysym (xlib::keysym 255 #xCF) :l8)
(define-clx-keysym (xlib::keysym 255 #xD0) :l9)
(define-clx-keysym (xlib::keysym 255 #xD1) :l10)

(define-clx-keysym (xlib::keysym 255 #xD2) :r1)
(define-clx-keysym (xlib::keysym 255 #xD3) :r2)
(define-clx-keysym (xlib::keysym 255 #xD4) :r3)
(define-clx-keysym (xlib::keysym 255 #xD5) :r4)
(define-clx-keysym (xlib::keysym 255 #xD6) :r5)
(define-clx-keysym (xlib::keysym 255 #xD7) :r6)
(define-clx-keysym (xlib::keysym 255 #xD8) :r7)
(define-clx-keysym (xlib::keysym 255 #xD9) :r8)
(define-clx-keysym (xlib::keysym 255 #xDA) :r9)
(define-clx-keysym (xlib::keysym 255 #xDB) :r10)


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
	  (:prompt 132)
	  (:move 52)
	  (:position 34)))

;; In X Windows, cursors are associated with a particular window, so
;; we are depending on the fact that the CLX port is not deeply mirrored
;; to get the behavior that we can "globally" change the pointer cursor.
(defmethod port-set-pointer-cursor ((port clx-port) pointer cursor)
  (unless (eq (pointer-cursor pointer) cursor)
    (with-slots (display) port
      (setf (xlib:window-cursor (sheet-mirror (pointer-sheet pointer)))
	    (realize-cursor port cursor))
      (xlib:display-force-output display)))
  cursor)

(defmethod port-set-sheet-pointer-cursor ((port clx-port) sheet cursor)
  (unless (eq (sheet-pointer-cursor sheet) cursor)
    (with-slots (display) port
      (setf (xlib:window-cursor (sheet-mirror sheet))
	    (realize-cursor port cursor))
      (xlib:display-force-output display)))
  cursor)

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

;;--- We need something like PORT-SET-POINTER-POSITION
(defmethod set-cursor-location ((port clx-port) sheet x y)
  (xlib:warp-pointer (sheet-mirror sheet)
		     (fix-coordinate x) (fix-coordinate y)))

;; X and Y are in native coordinates
(defmethod port-set-pointer-position ((port clx-port) pointer x y)
  (let* ((sheet (pointer-sheet pointer))
	 (mirror (sheet-mirror sheet)))
    (xlib:warp-pointer mirror x y)))


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
      (setf (aref array (modifier-key-index :shift))
	    (xlib:make-state-mask :shift))
      (setf (aref array (modifier-key-index :control))
	    (xlib:make-state-mask :control))
      (flet ((test-keysym (keysym state-mask)
	       (cond ((or (= keysym xlib::left-meta-keysym)
			  (= keysym xlib::left-alt-keysym)
			  (= keysym xlib::right-meta-keysym)
			  (= keysym xlib::right-alt-keysym))
		      (setf (aref array (modifier-key-index :meta))
			    state-mask))
		     ((or (= keysym xlib::left-super-keysym)
			  (= keysym xlib::right-super-keysym))
		      (setf (aref array (modifier-key-index :super))
			    state-mask))
		     ((or (= keysym xlib::left-hyper-keysym)
			  (= keysym xlib::right-hyper-keysym))
		      (setf (aref array (modifier-key-index :hyper))
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

