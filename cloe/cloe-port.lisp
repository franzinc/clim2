;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader$

(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defclass cloe-port (basic-port)
    ()
  (:default-initargs :allow-loose-text-style-size-mapping nil))

(defmethod find-port-type ((type (eql ':cloe)))
  'cloe-port)

(defmethod port-type ((port cloe-port))
  ':cloe)


(eval-when (compile load eval)
	   
;; Define the new key chars for Cloe CLIM.  Regular Cloe defines 0-127, we define
;; 128-139 as the F-keys (F1 thru F12), 140 for c-sh-A, and 141 as c-sh-V
(sys::define-character-name "F1" 128)
(sys::define-character-name "F2" 129)
(sys::define-character-name "F3" 130)
(sys::define-character-name "F4" 131)
(sys::define-character-name "F5" 132)
(sys::define-character-name "F6" 133)
(sys::define-character-name "F7" 134)
(sys::define-character-name "F8" 135)
(sys::define-character-name "F9" 136)
;; Note windows traps F10 as alt-space. Why?
(sys::define-character-name "F10" 137)
(sys::define-character-name "F11" 138)
(sys::define-character-name "F12" 139)
(sys::define-character-name "Arglist" 140)
(sys::define-character-name "ShowValue" 141)

)	;eval-when


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

(defclass cloe-display-device ()
    ((text-style->cloe-font-mapping :initform (make-hash-table))
     (logpixelsy :initform 72 :initarg :logpixelsy)))

(defmethod text-style-mapping
	   ((device cloe-display-device) character-set (style text-style) window)
  (declare (ignore character-set))
  (let ((hash-table (slot-value device 'text-style->cloe-font-mapping)))
    (or (gethash style hash-table)
	(setf (gethash style hash-table)
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
		      window 
		      (- (round (* point-size (slot-value device 'logpixelsy))
				72))
		      :weight weight :italic italic
		      :pitch-and-family family :face face-name))))))))

(defmethod text-style-mapping
	   ((device cloe-display-device) character-set (style device-font) window)
  (declare (ignore character-set))
  (unless (eql device (device-font-display-device style))
    (error "An attempt was made to map device font ~S on device ~S, ~@
	    but it is defined for device ~S"
	   style device (device-font-display-device style)))
  (let ((hash-table (slot-value device 'text-style->cloe-font-mapping)))
    (or (gethash style hash-table)
	(setf (gethash style hash-table)
	      (let ((args (device-font-name style)))
		(apply #'make-windows-font window 
		       (- (round (* (pop args) (slot-value device 'logpixelsy))
				 72))
		       args))))))

(defun make-windows-font
       (window height &key (width 0) (escapement 0) (orientation 0)
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
	(win::get-text-metrics window)
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
					 (win::get-text-extent window string)
				       (declare (ignore height))
				       (setf (aref array i) width))))
				 array))))))

(defmethod stream-glyph-for-character ((stream cloe-window-stream) character style
				       &optional our-font)
  (let ((display-device-type (slot-value stream 'display-device-type))
	(window (slot-value stream 'window)))
    (multiple-value-bind (character-set index)
	(char-character-set-and-index character)
      (let* ((cloe-font (or our-font
			    (text-style-mapping
			      display-device-type style character-set window)))
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
		origin-x origin-y bb-x bb-y
		fixed-width-p)))))

