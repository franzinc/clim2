;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: cloe-port.lisp,v 1.1 92/10/01 10:03:56 cer Exp $

(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


(defvar *cloe-port* nil)

(defclass cloe-port (basic-port)
    ((text-style->cloe-font-mapping :initform (make-hash-table))
     (font-cache-style :initform nil)
     (font-cache-font :initform nil)
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

(defmethod make-palette ((port cloe-port) &key color-p mutable-p)
  (make-instance 'cloe-palette
    :port port 
    :color-p color-p
    :mutable-p mutable-p))


(defmethod initialize-instance :before ((port cloe-port) &key)
  (unless (null *cloe-port*)
    (error "There can only be one Cloe port."))
  (setf *cloe-port* port))

(defmethod initialize-instance :after ((port cloe-port) &key)
  (initialize-dc)
  (setf (slot-value port 'logpixelsy) (win::get-device-caps *dc* 90))
  (setf (slot-value port 'silica::default-palette) 
	(make-palette port :color-p t))
  nil)



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

;;; Fill up the font width table for a variable width font
;;; by asking windows the width of each character.
;;; This doesn't quite work in the face of kerning.
(defun initialize-font-width-table (font window)
  ;; only need to run this on variable-width fonts.
  (select-font (cloe-font-index font))
  (let ((array (make-array (1+ (- 126 32)))))
    (with-temporary-string (string :length 1)
      (setf (fill-pointer string) 1)
      (loop for i from 32 to 126 do
	(setf (aref string 0) (code-char i))
	(multiple-value-bind (width height)
	    (win::get-text-extent window string)
	  (declare (ignore height ))
	  (setf (aref array (- i 32)) width))))
    (setf (cloe-font-font-width-table font) array)))

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
	   ((device cloe-port) (style pyrex::device-font)
	    &optional (character-set *standard-character-set*) etc)
  (declare (ignore character-set))
  (unless (eql device (pyrex::device-font-display-device style))
    (error "An attempt was made to map device font ~S on device ~S, ~@
	    but it is defined for device ~S"
	   style device (pyrex::device-font-display-device style)))
  (with-slots (text-style->cloe-font-mapping font-cache-style font-cache-font) port
    (if (eq style font-cache-style)
	font-cache-font
	(progn 
	  (setf font-cache-style style)
	  (setf font-cache-font
		(or (gethash style text-style->cloe-font-mapping)
		    (setf (gethash style text-style->cloe-font-mapping)
			  (let ((args (pyrex::device-font-name style)))
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



#||
(defvar *vk->keysym* (make-array 256 :initial-element nil))
(defvar *keysym->vk* (make-hash-table))

(defmacro define-vk (vk keysym)
  `(progn 
     (setf (svref *vk->keysym* vk) keysym)
     (setf (gethash ,keysym *keysym->vk*) ,vk)))

(defun-inline vk->keysym (vk)
  (svref *vk->keysym* vk))

(defun-inline keysym->vk (keysym)
  (gethash keysym *keysym->vk*))

;; The semi-standard characters
(define-vk 0x0d :return)
(define-vk ???? :newline)
(define-vk 0x09 :tab)
(define-vk 0x2e :rubout)
(define-vk 0x08 :backspace)
(define-vk ???? :page)
(define-vk ???? :linefeed)
(define-vk 0x1b :escape)

;; Other useful characters
(define-vk 0x23 :end)
(define-vk ???? :abort)
(define-vk 0x2f :help)
(define-vk ???? :complete)
(define-vk 0x22 :scroll)
(define-vk ???? :refresh)
(define-vk ???? :clear-input)

;; Finally, the shifts
(define-vk 0x19 :left-shift)
(define-vk ???? :right-shift)
(define-vk 0x11 :left-control)
(define-vk ???? :right-control)
(define-vk 0x14 :caps-lock)
(define-vk ???? :shift-lock)
(define-vk 0x12 :left-meta)
(define-vk ???? :right-meta)
(define-vk ???? :left-super)
(define-vk ???? :right-super)
(define-vk ???? :left-hyper)
(define-vk ???? :right-hyper)

;; Non-standard keys found on Sun keyboards

(define-vk 0x70 :f1)
(define-vk 0x71 :f2)
(define-vk 0x72 :f3)
(define-vk 0x73 :f4)
(define-vk 0x74 :f5)
(define-vk 0x75 :f6)
(define-vk 0x76 :f7)
(define-vk 0x77 :f8)
(define-vk 0x78 :f9)
(define-vk 0x79 :f10)
||#
