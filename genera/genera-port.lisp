;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: genera-port.lisp,v 1.7 92/07/01 15:47:32 cer Exp $

(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 International Lisp Associates."


(defvar *genera-default-server-path* `(:genera :host ,net:*local-host*
					       :screen ,tv:main-screen))

(defclass genera-port (basic-port)
    ((screen :initform nil :reader port-display)
     (console :initform nil)
     (color-p :initform nil)
     (embedded-p :initform nil)
     (cursor-font)
     (cursor-cache :initform nil :type nil)
     (height-pixels)
     (width-pixels))
  (:default-initargs :allow-loose-text-style-size-mapping nil))

(defmethod find-port-type ((type (eql ':genera)))
  'genera-port)

(defmethod port-type ((port genera-port))
  ':genera)

(defmethod port-name ((port genera-port))
  (let ((keys (cdr (port-server-path port))))
    (format nil "~A:~A" 
      (getf keys :host) (getf keys :screen))))


(defparameter *genera-use-color* t)		;for debugging monochrome...

(defmethod initialize-instance :after ((port genera-port)
				       &key (server-path *genera-default-server-path*))
  (destructuring-bind (type &key (host net:*local-host*)
				 (screen tv:main-screen)) server-path
    (declare (ignore type host))
    (with-slots ((this-screen screen) console color-p embedded-p
		 cursor-font height-pixels width-pixels) port
      (setf this-screen     screen 
	    console	    (tv:sheet-console screen)
	    color-p	    (and *genera-use-color* (color:color-stream-p screen))
	    embedded-p	    (typep screen 'tv:basic-remote-screen)
	    cursor-font     fonts:mouse
	    height-pixels   (tv:sheet-inside-height screen)
	    width-pixels    (tv:sheet-inside-width screen)))
    (initialize-genera-port port)))


;;; Text styles for Genera sheets

(defmethod initialize-genera-port ((port genera-port))
  (let ((screen (slot-value port 'screen)))
    (let ((device (scl:send screen :display-device-type)))
      (cond
	#+Imach
	((and (find-package 'mtb)
	      (typep device (intern 
			      (symbol-name 
				'small-screen-genera-fonts-mac-display-device)
			      :mtb)))
	 (define-small-screen-text-style-mappings port))
	(t (define-normal-screen-text-style-mappings port))))))

(defmethod text-style-mapping :around ((port genera-port) style
				       &optional (character-set *standard-character-set*) 
						 window)
  (declare (ignore window))
  (let ((font (call-next-method)))
    (etypecase font
      (null nil)
      (sys:font font)
      (symbol
	(when (not (boundp font))
	  (handler-case (fed:find-and-load-font font)
	    (fed:font-not-found ()
	      (error "Mapping ~S for ~A characters in ~A character set could not be found."
		     font style (or character-set :default)))))
	(let ((font (symbol-value font)))
	  (unless (typep font 'sys:font)
	    (error "Mapping for ~A characters in ~A character set is not a font."
		   style (or character-set :default)))
	  ;; Simple cache: hope nobody changes the font later
	  (setf (text-style-mapping port (parse-text-style style)) font)
	  font)))))

(defvar *sheet-logical-size-alist*)

(defmethod standardize-text-style ((port genera-port) style 
				   &optional (character-set *standard-character-set*))
  (standardize-text-style-1
    port style character-set *sheet-logical-size-alist*))

(defun define-normal-screen-text-style-mappings (display-device)
  (setq *sheet-logical-size-alist*
	'((:tiny        6)
	  (:very-small  8)
	  (:small      10)
	  (:normal     12)
	  (:large      14)
	  (:very-large 20)
	  (:huge       24)))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :fix (:face :roman (:size 20 fonts:bigfnt
					 14 fonts:medfnt
					 12 fonts:cptfont
					 10 fonts:tvfont
					 8  fonts:einy7
					 6  fonts:tiny)
			   :italic (:size 20 fonts:bigfnti
					  14 fonts:medfnti
					  12 fonts:cptfonti
					  10 fonts:tvfonti
					  8  fonts:einy7
					  6  fonts:tiny)
			   :bold (:size 20 fonts:bigfntb
					14 fonts:medfntb
					12 fonts:cptfontcb
					10 fonts:tvfontb
					8  fonts:einy7
					6  fonts:tiny)
			   (:bold :italic) (:size 20 fonts:bigfntbi
						  14 fonts:medfntbi
						  12 fonts:cptfontbi
						  10 fonts:tvfontbi
						  8  fonts:einy7
						  6  fonts:tiny)
			   (:bold :extended) (:size 12 fonts:cptfontb
						    10 fonts:tvfontb)
			   :condensed (:size 12 fonts:cptfontc)
			   (:extra :condensed) (:size 12 fonts:cptfontcc
						      10 fonts:tvfont)))))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :serif (:face :roman (:size 20 fonts:dutch20
					   14 fonts:dutch14
					   12 fonts:tr12
					   10 fonts:tr10
					   8  fonts:tr8)
			     :italic (:size 20 fonts:dutch20i
					    14 fonts:dutch14i
					    12 fonts:tr12i
					    10 fonts:tr10i
					    8  fonts:tr8i)
			     :bold (:size 20 fonts:dutch20b
					  14 fonts:dutch14b
					  12 fonts:tr12b
					  10 fonts:tr10b
					  8  fonts:tr8b)
			     (:bold :italic) (:size 20 fonts:dutch20bi
						    14 fonts:dutch14bi
						    12 fonts:tr12bi
						    10 fonts:tr10bi
						    8  fonts:tr8bi)))))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :sans-serif (:face :roman (:size 20 fonts:swiss20
						14 fonts:hl14
						12 fonts:hl12
						10 fonts:hl10
						8  fonts:hl8)
				  :italic (:size 20 fonts:swiss20i
						 14 fonts:hl14i
						 12 fonts:hl12i
						 10 fonts:hl10i
						 8  fonts:hl8i)
				  :bold (:size 20 fonts:swiss20b
					       14 fonts:hl14b
					       12 fonts:hl12b
					       10 fonts:hl10b
					       8  fonts:hl8b)
				  (:bold :italic) (:size 20 fonts:swiss20bi
							 14 fonts:hl14bi
							 12 fonts:hl12bi
							 10 fonts:hl10bi
							 8  fonts:hl8bi))))))

#+IMach
(defun define-small-screen-text-style-mappings (display-device)
  (setq *sheet-logical-size-alist*
	'((:tiny        6)
	  (:very-small  8)
	  (:small      10)
	  (:normal     12)
	  (:large      14)
	  (:very-large 20)
	  (:huge       24)))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :fix (:face :roman (:size 20 fonts:medfnt
					 14 fonts:cptfont
					 12 fonts:tvfont
					 10 fonts:einy7
					 8  fonts:einy7
					 6  fonts:einy7)
			   :italic (:size 20 fonts:medfnti
					  14 fonts:cptfonti
					  12 fonts:tvfonti 
					  10 fonts:einy7
					  8  fonts:einy7
					  6  fonts:einy7)
			   :bold (:size 20 fonts:medfntb
					14 fonts:cptfontcb
					12 fonts:tvfontb 
					10 fonts:einy7
					8  fonts:einy7
					6  fonts:einy7)
			   (:bold :italic) (:size 20 fonts:medfntbi
						  14 fonts:cptfontbi
						  12 fonts:tvfontbi
						  10 fonts:einy7
						  8  fonts:einy7
						  6  fonts:einy7)
			   (:bold :extended) (:size 12 fonts:tvfontb
						    10 fonts:cptfontb)
			   :condensed (:size 12 fonts:cptfontc)
			   (:extra :condensed) (:size 12 fonts:cptfontcc
						      10 fonts:tvfont)))))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :serif (:face :roman (:size 20 fonts:dutch14
					   14 fonts:tr12
					   12 fonts:tr10
					   10 fonts:tr8
					   8  fonts:tr8)
			     :italic (:size 20 fonts:dutch14i
					    14 fonts:tr12i
					    12 fonts:tr10i
					    10 fonts:tr8i
					    8  fonts:tr8i)
			     :bold (:size 20 fonts:dutch14b
					  14 fonts:tr12b
					  12 fonts:tr10b
					  10 fonts:tr8b
					  8  fonts:tr8b)
			     (:bold :italic) (:size 20 fonts:dutch14bi
						    14 fonts:tr12bi
						    12 fonts:tr10bi
						    10 fonts:tr8bi
						    8  fonts:tr8bi)))))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :sans-serif (:face :roman (:size 20 fonts:hl14
						14 fonts:hl12
						12 fonts:hl10
						10 fonts:hl8
						8  fonts:hl8)
				  :italic (:size 20 fonts:hl14i
						 14 fonts:hl12i
						 12 fonts:hl10i
						 10 fonts:hl8i
						 8  fonts:hl8i)
				  :bold (:size 20 fonts:hl14b
					       14 fonts:hl12b
					       12 fonts:hl10b
					       10 fonts:hl8b
					       8  fonts:hl8b)
				  (:bold :italic) (:size 20 fonts:hl14bi
							 14 fonts:hl12bi
							 12 fonts:hl10bi
							 10 fonts:hl8bi
							 8  fonts:hl8bi))))))


(defmacro with-genera-glyph-for-character (&body body)
  `(macrolet ((port-glyph-for-character (port character style &optional our-font)
		`(multiple-value-bind (character-set index)
		     (char-character-set-and-index ,character)
		   ;; For now we are asserting that each string passed to
		   ;; WRITE-STRING will have no style changes within it.
		   ;; This is what OUR-FONT is all about...
		   (let* ((font (or ,our-font 
				    (text-style-mapping ,port ,style character-set)))
			  (fit (sys:font-indexing-table font))
			  (lkt (sys:font-left-kern-table font))
			  (cwt (sys:font-char-width-table font))
			  (escapement-x (if (diacritic-char-p ,character)
					    0
					    (if cwt 
						(aref cwt index)
						(sys:font-char-width font))))
			  (escapement-y 0)
			  (origin-x (if lkt (aref lkt index) 0))
			  (origin-y (sys:font-baseline font))
			  (bb-x (if fit (- (aref fit (1+ index)) (aref fit index))
				    (sys:font-char-width font)))
			  (bb-y (sys:font-char-height font)))
		     (values index font escapement-x escapement-y origin-x origin-y
			     bb-x bb-y (not (or fit lkt cwt)))))))
     ,@body))

(defmethod port-glyph-for-character ((port genera-port)
				     character style &optional our-font)
  (declare (values index font escapement-x escapement-y origin-x origin-y bb-x bb-y
		   fixed-width-font-p))
  (with-genera-glyph-for-character
    (port-glyph-for-character port character style our-font)))


(defvar *genera-character->keysym-table* (make-hash-table))
(defvar *keysym->genera-character-table* (make-hash-table))

(defmacro define-genera-keysym (character keysym)
  `(progn 
     (setf (gethash ,character *genera-character->keysym-table*) ',keysym)
     (unless (gethash ,keysym *keysym->genera-character-table*)
       (setf (gethash ,keysym *keysym->genera-character-table*) ',character))))

(defun-inline genera-character->keysym (character)
  ;; Use SCL:MAKE-CHAR here to strip bits from the character.
  (gethash (char-upcase (scl:make-char character)) *genera-character->keysym-table*))

(defun-inline keysym->genera-character (keysym)
  (gethash keysym *keysym->genera-character-table*))

(defmethod port-canonicalize-gesture-spec ((port genera-port) gesture-spec)
  (multiple-value-bind (keysym shifts)
      (parse-gesture-spec gesture-spec)
    (let* ((console (slot-value port 'console))
	   (keyboard-table (si:keyboard-keyboard-table (si:console-keyboard console)))
	   (genera-char (if (and (characterp keysym) (standard-char-p keysym))
			    keysym
			    (keysym->genera-character keysym)))
	   (genera-charcode nil)
	   needed-shifts genera-keycode)
      (unless genera-char
	(return-from port-canonicalize-gesture-spec nil))
      (setq genera-charcode (char-code genera-char))
      (multiple-value-setq (needed-shifts genera-keycode)
	(block find-it
	  (dotimes (i 4)
	    (dotimes (j 128)
	      (when (= (aref keyboard-table i j) genera-charcode)
		(return-from find-it (values i j)))))))
      (unless genera-keycode 
	(return-from port-canonicalize-gesture-spec nil))
      (if (and (not (alpha-char-p genera-char))
	       (ldb-test si:%%kbd-mapping-table-index-shift needed-shifts))
	  (setq needed-shifts (make-modifier-state :shift))
	  (setq needed-shifts 0))
      (setq shifts (logior shifts needed-shifts))
      ;; OK, now we've got the genera keycode index into the keyboard table.
      ;; We find all of the shifts which match SHIFTS and look them up in the table.
      (let* ((real-charcode (aref keyboard-table
				  ;; The only modifier key that can actually
				  ;; change the keysym in this port is :SHIFT
				  (if (= (logand shifts (make-modifier-state :shift))
					 (make-modifier-state :shift))
				      (make-modifier-state :shift)
				      0)
				  genera-keycode))
	     (real-genera-char (and real-charcode (code-char real-charcode)))
	     (real-genera-keysym (and real-genera-char
				      (genera-character->keysym real-genera-char))))
	(cons real-genera-keysym shifts)))))

;; The standard characters
(define-genera-keysym #\Space :space)
(define-genera-keysym #\! :\!)
(define-genera-keysym #\" :\")
(define-genera-keysym #\# :\#)
(define-genera-keysym #\$ :\$)
(define-genera-keysym #\% :\%)
(define-genera-keysym #\& :\&)
(define-genera-keysym #\' :\')
(define-genera-keysym #\( :\()
(define-genera-keysym #\) :\))
(define-genera-keysym #\* :\*)
(define-genera-keysym #\+ :\+)
(define-genera-keysym #\, :\,)
(define-genera-keysym #\- :\-)
(define-genera-keysym #\. :\.)
(define-genera-keysym #\/ :\/)
(define-genera-keysym #\0 :\0)
(define-genera-keysym #\1 :\1)
(define-genera-keysym #\2 :\2)
(define-genera-keysym #\3 :\3)
(define-genera-keysym #\4 :\4)
(define-genera-keysym #\5 :\5)
(define-genera-keysym #\6 :\6)
(define-genera-keysym #\7 :\7)
(define-genera-keysym #\8 :\8)
(define-genera-keysym #\9 :\9)
(define-genera-keysym #\: :\:)
(define-genera-keysym #\; :\;)
(define-genera-keysym #\< :\<)
(define-genera-keysym #\= :\=)
(define-genera-keysym #\> :\>)
(define-genera-keysym #\? :\?)
(define-genera-keysym #\@ :\@)
(define-genera-keysym #\A :\A)
(define-genera-keysym #\a :\A)
(define-genera-keysym #\B :\B)
(define-genera-keysym #\b :\B)
(define-genera-keysym #\C :\C)
(define-genera-keysym #\c :\C)
(define-genera-keysym #\D :\D)
(define-genera-keysym #\d :\D)
(define-genera-keysym #\E :\E)
(define-genera-keysym #\e :\E)
(define-genera-keysym #\F :\F)
(define-genera-keysym #\f :\F)
(define-genera-keysym #\G :\G)
(define-genera-keysym #\g :\G)
(define-genera-keysym #\H :\H)
(define-genera-keysym #\h :\H)
(define-genera-keysym #\I :\I)
(define-genera-keysym #\i :\I)
(define-genera-keysym #\J :\J)
(define-genera-keysym #\j :\J)
(define-genera-keysym #\K :\K)
(define-genera-keysym #\k :\K)
(define-genera-keysym #\L :\L)
(define-genera-keysym #\l :\L)
(define-genera-keysym #\M :\M)
(define-genera-keysym #\m :\M)
(define-genera-keysym #\N :\N)
(define-genera-keysym #\n :\N)
(define-genera-keysym #\O :\O)
(define-genera-keysym #\o :\O)
(define-genera-keysym #\P :\P)
(define-genera-keysym #\p :\P)
(define-genera-keysym #\Q :\Q)
(define-genera-keysym #\q :\Q)
(define-genera-keysym #\R :\R)
(define-genera-keysym #\r :\R)
(define-genera-keysym #\S :\S)
(define-genera-keysym #\s :\S)
(define-genera-keysym #\T :\T)
(define-genera-keysym #\t :\T)
(define-genera-keysym #\U :\U)
(define-genera-keysym #\u :\U)
(define-genera-keysym #\V :\V)
(define-genera-keysym #\v :\V)
(define-genera-keysym #\W :\W)
(define-genera-keysym #\w :\W)
(define-genera-keysym #\X :\X)
(define-genera-keysym #\x :\X)
(define-genera-keysym #\Y :\Y)
(define-genera-keysym #\y :\Y)
(define-genera-keysym #\Z :\Z)
(define-genera-keysym #\z :\Z)
(define-genera-keysym #\[ :\[)
(define-genera-keysym #\\ :\\)
(define-genera-keysym #\] :\])
(define-genera-keysym #\^ :\^)
(define-genera-keysym #\_ :\_)
(define-genera-keysym #\` :\`)
(define-genera-keysym #\{ :\{)
(define-genera-keysym #\| :\|)
(define-genera-keysym #\} :\})
(define-genera-keysym #\~ :\~)

;; The semi-standard characters
(define-genera-keysym #\Return	  :return)
(define-genera-keysym #\Newline	  :newline)
(define-genera-keysym #\Tab       :tab)
(define-genera-keysym #\Rubout    :rubout)
(define-genera-keysym #\Backspace :backspace)
(define-genera-keysym #\Page	  :page)
(define-genera-keysym #\Line	  :linefeed)

;; Other useful characters
(define-genera-keysym #\End   :end)
(define-genera-keysym #\Abort :abort)
(define-genera-keysym #\Help  :help)
(define-genera-keysym #\Complete :complete)
(define-genera-keysym #\Scroll   :scroll)
(define-genera-keysym #\Refresh  :refresh)
(define-genera-keysym #\Clear-Input :clear-input)


(defmethod genera-mouse-char-for-cursor ((cursor t))
  ;; any other cursor we don't know about yet
  #\mouse:ne-arrow)

(defvar *genera-cursor-type-alist*
	'((:default #\mouse:nw-arrow)
	  (:vertical-scroll #\mouse:vertical-double-arrow)
	  (:scroll-up #\mouse:up-arrow)
	  (:scroll-down #\mouse:down-arrow)
	  (:horizontal-scroll #\mouse:horizontal-double-arrow)
	  (:scroll-left #\mouse:left-arrow)
	  (:scroll-right #\mouse:right-arrow)
	  (:busy #\mouse:hourglass)
	  (:upper-left #\mouse:nw-corner)
	  (:lower-right #\mouse:se-corner)))

(defmethod install-port-cursor ((port genera-port) sheet cursor)
  (declare (ignore sheet))
  (let ((char (or (second (assoc cursor *genera-cursor-type-alist*))
		  #\mouse:ne-arrow)))
    ;; Note that this means that CLIM requires DW for the nonce...
    ;; It would be trivial to encode this information in the above methods, though
    (let ((entry (assoc char dw::*mouse-blinker-characters*))
	  (x 0) (y 0))
      (when entry
	(scl:destructuring-bind (nil xx yy &optional nil) entry
	  (setq x xx y yy)))
      (tv:mouse-set-blinker-definition ':character x y ':on ':set-character char))))

;;--- We need something like PORT-SET-POINTER-POSITION
(defmethod set-cursor-location ((port genera-port) sheet x y)
  ;; we don't do anything about this yet.
  )

(defun ensure-blinker-matches-cursor (cursor stream)
  (let ((active (cursor-active cursor))
	(state (cursor-state cursor))
	(focus (cursor-focus cursor)))
    (let* ((mirror (sheet-mirror stream))
	   (blinker (scl:send mirror :ensure-blinker-for-cursor cursor)))
      (when blinker
	(let ((transformation (sheet-native-transformation stream)))
	  (multiple-value-bind (x y) (bounding-rectangle* cursor)
	    (multiple-value-setq (x y)
	      (transform-position transformation x y))
	    (fix-coordinates x y)
	    (cond ((and active state focus)
		   (scl:send mirror :set-cursorpos x y)
		   (scl:send blinker :set-visibility :blink))
		  ((and active state)
		   (scl:send mirror :set-cursorpos x y)
		   (scl:send blinker :set-visibility T))
		  (t (scl:send blinker :set-visibility nil)))))))))

(defmethod (setf port-keyboard-input-focus) :around (focus (port genera-port))
  (let ((old-focus (port-keyboard-input-focus port)))
    (call-next-method)
    (let ((mirror (and focus (sheet-mirror focus)))
	  (old-mirror (and old-focus (sheet-mirror old-focus))))
      (when (and mirror 
		 old-mirror
		 (not (eq focus old-focus)))
	(if (eq (scl:send mirror :alias-for-selected-windows)
		(scl:send old-mirror :alias-for-selected-windows))
	    (scl:send old-mirror :select-relative)
	    (scl:send mirror :deselect t)))))
  focus)

(defmethod port-note-cursor-change :after ((port genera-port) cursor stream type old new)
  (declare (ignore type old))
  (ensure-blinker-matches-cursor cursor stream))


;;;--- From ILA stream
#||
(defmethod destroy-port :after ((port genera-port))
  (with-slots (genera-screen) port
    ;;--- Needs to be a defined accessor (or mapping function)
    (do* ((tail (slot-value port 'mirror->sheet-table)
		;; Really a PLIST
		(cddr tail))
	  (genera-window (first tail) (first tail)))
	 ((null tail) nil)
      (scl:send genera-window :kill))))

||#
