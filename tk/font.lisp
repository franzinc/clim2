;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: font.lisp,v 1.4 92/01/31 14:54:27 cer Exp Locker: cer $

(in-package :tk)

(defclass font (handle-class)
  ((name :initarg :name :reader font-name)))

(defmethod print-object ((x font) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~A ~X" (font-name x) (object-handle x))))
(defmethod initialize-instance :after ((f font) &key handle display name)
  (unless handle
    (let ((x (x11:xloadqueryfont
	      (display-handle display)
	      (string-to-char* name))))
      (when (zerop x) (error "Cannot find font: ~S" name))
      (setf (slot-value f 'handle) x))
    (register-address f)))


(defun query-font (display font-id)
  (let ((h (x11:xqueryfont (display-handle display) font-id)))
    (when (zerop h)
      (error "Cannot query font: ~D" font-id))
    (make-instance 'font
		    :display display
		    :handle h)))

(def-c-type (xcharstruct-vector :in-foreign-space) 1 x11:xcharstruct)

(defmethod font-width (font)
  (x11::xfontstruct-max-bounds-width (object-handle font)))
 
(defmethod font-ascent (font)
  (x11:xfontstruct-ascent (object-handle font)))

(defmethod font-descent (font)
  (x11:xfontstruct-descent (object-handle font)))

(defmethod font-all-chars-exist-p (font)
  (x11:xfontstruct-all-chars-exist (object-handle font)))

(defmethod font-range (font)
  (let* ((h (object-handle font))
	 (min-byte-1 (x11:xfontstruct-min-byte1 h))
	 (max-byte-1 (x11:xfontstruct-max-byte1 h)))
    (cond ((and (zerop min-byte-1)
		(zerop max-byte-1))
	   (values (x11:xfontstruct-min-char-or-byte2 h)
		   (x11:xfontstruct-max-char-or-byte2 h))))))

(defmethod char-width (font index)
  (multiple-value-bind
      (min max) (font-range font)
    (if (and (<= min index)
	     (<= index max))
	(xcharstruct-vector-width
	 (x11:xfontstruct-per-char (object-handle font))
	 (- index min)))))

(def-c-type (xfontname-list :in-foreign-space) 1 * :char)

(defun list-font-names (display pattern &key (max-fonts 65535) (result-type 'list))
  (with-ref-par ((n 0))
    (let* ((names (x11:xlistfonts (display-handle display)
				  pattern
				  max-fonts
				  n))
	   (n (aref n 0))
	   (seq (make-sequence result-type n)))
      (prog1
	  (dotimes (i n seq)
	    (setf (elt seq i) (char*-to-string (xfontname-list names i))))
	(x11::xfreefontnames names)))))

(def-c-type (xfontstruct-array :in-foreign-space) 1 x11::xfontstruct)

(defun list-font-names-with-info (display pattern &key (max-fonts 65535) (result-type 'list))
  (with-ref-par ((n 0)
		 (fonts 0))
    (let* ((names (x11:xlistfontswithinfo (display-handle display)
					  pattern
					  max-fonts
					  n
					  fonts))
	   (n (aref n 0))
	   (fonts (aref fonts 0))
	   (seq (make-sequence result-type n)))
      (prog1
	  (dotimes (i n seq)
	    (setf (elt seq i) 
	      (make-instance 'font
			     :handle (xfontstruct-array fonts i)
			     :name (char*-to-string (xfontname-list
						     names i)))))
	;;--- Dont free the font info
	(x11:xfreefontnames names)))))

(defun tk::font-property (font which)
  (declare (ignore font which))
  nil)


      

      
    
