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
;; $fiHeader: font.lisp,v 1.15 1993/07/27 01:52:46 colin Exp $

(in-package :tk)

(defclass font (ff:foreign-pointer)
  ((name :initarg :name :reader font-name)))

(defmethod print-object ((x font) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~A"
	    (if (slot-boundp x 'name) (font-name x) :dunno))))

(defmethod initialize-instance :after ((f font) &key foreign-address display name)
  (unless foreign-address
    (check-type name string)
    (let ((x (x11:xloadqueryfont display
				 #+ics (fat-string-to-string8 name)
				 #-ics name)))
      (when (zerop x) (error "Cannot find font: ~S" name))
      (setf (foreign-pointer-address f) x)
      (register-address f))))


(defun query-font (display font-id)
  (let ((h (x11:xqueryfont display font-id)))
    (when (zerop h)
      (error "Cannot query font: ~D" font-id))
    (make-instance 'font
		    :display display
		    :foreign-address h)))

(defun font-width (font)
  (x11::xfontstruct-max-bounds-width font))

(defun font-height (font)
  (+ (x11:xfontstruct-ascent font)
     (x11:xfontstruct-descent font)))

(defun font-ascent (font)
  (x11:xfontstruct-ascent font))

(defun font-descent (font)
  (x11:xfontstruct-descent font))

(defun font-all-chars-exist-p (font)
  (x11:xfontstruct-all-chars-exist font))

(defun font-range (font)
  (let* ((h font)
	 (min-byte-1 (x11:xfontstruct-min-byte1 h))
	 (max-byte-1 (x11:xfontstruct-max-byte1 h)))
    (cond ((and (zerop min-byte-1)
		(zerop max-byte-1))
	   (values (x11:xfontstruct-min-char-or-byte2 h)
		   (x11:xfontstruct-max-char-or-byte2 h))))))

(defun per-char-index (font index)
  (or (let ((min-byte1 (x11:xfontstruct-min-byte1 font))
	    (max-byte1 (x11:xfontstruct-max-byte1 font))
	    (min-byte2 (x11:xfontstruct-min-char-or-byte2 font))
	    (max-byte2 (x11:xfontstruct-max-char-or-byte2 font)))
	(if (and (zerop min-byte1)
		 (zerop max-byte1))
	    (when (<= min-byte2 index max-byte2)
	      (- index min-byte2))
	  (let ((byte1 (ldb (byte 8 8) index))
		(byte2 (ldb (byte 8 0) index))
		(d (1+ (- max-byte2 min-byte2))))
	    (when (and (<= min-byte1 byte1 max-byte1)
		       (<= min-byte2 byte2 max-byte2))
	      (+ (- byte2 min-byte2)
		 (* (- byte1 min-byte1) d))))))
      (error "Charecter index ~D out of range for font ~S" index font)))

;;; char-width and char-dimenstions could be more elegent if
;;; x11:xfontstruct-min-bounds was defined. The returned XCharStruct
;;; could be treated like the XCharStruct vector using n=0 rather
;;; than the per-char-index. Need to investigate the def-c-type in
;;; xlib/xlib-defs.lisp (cim 2/9/95)

(defun char-width (font index)
  (let ((s (x11:xfontstruct-per-char font)))
    (if (zerop s)
	(x11::xfontstruct-min-bounds-width font)
      (let ((n (per-char-index font index)))
	(xcharstruct-vector-width s n)))))

(defun char-dimensions (font index)
  (let ((s (x11:xfontstruct-per-char font)))
    (if (zerop s)
	(values
	 (x11::xfontstruct-min-bounds-lbearing font)
	 (x11::xfontstruct-min-bounds-rbearing font)
	 (x11::xfontstruct-min-bounds-width font)
	 (x11::xfontstruct-min-bounds-ascent font)
	 (x11::xfontstruct-min-bounds-descent font))
      (let ((n (per-char-index font index)))
	(values
	 (xcharstruct-vector-lbearing s n)
	 (xcharstruct-vector-rbearing s n)
	 (xcharstruct-vector-width s n)
	 (xcharstruct-vector-ascent s n)
	 (xcharstruct-vector-descent s n))))))

(defun list-font-names (display pattern &key (max-fonts 65535) (result-type 'list))
  (with-ref-par ((n 0))
    (let* ((names (x11:xlistfonts display
				  #+ics (fat-string-to-string8 pattern)
				  #-ics pattern
				  max-fonts
				  n))
	   (n (aref n 0))
	   (seq (make-sequence result-type n)))
      (prog1
	  (dotimes (i n seq)
	    (setf (elt seq i) (char*-to-string (xfontname-list names i))))
	(x11::xfreefontnames names)))))

(defun list-font-names-with-info (display pattern &key (max-fonts 65535) (result-type 'list))
  (with-ref-par ((n 0)
		 (fonts 0))
    (let* ((names (x11:xlistfontswithinfo display
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
			     :foreign-address (xfontstruct-array fonts i)
			     :name (char*-to-string (xfontname-list
						     names i)))))
	;;--- Dont free the font info
	(x11:xfreefontnames names)))))

(defun tk::font-property (font which)
  (declare (ignore font which))
  nil)

;;; fontset support

#+ics
(defclass font-set (ff:foreign-pointer)
  ((base-names :initarg :base-names :reader font-set-base-names)))

#+ics
(defun create-font-set (display base-font-names)
  (with-ref-par ((missing-list-ref 0)
		 (missing-count-ref 0)
		 (def-string-ref 0))
    (let ((font-set (x11:xcreatefontset display
					(ff:string-to-char* base-font-names)
					missing-list-ref
					missing-count-ref
					def-string-ref))
	  (missing-charsets nil))
      (dotimes (i (aref missing-count-ref 0))
	(push (ff:char*-to-string
	       (sys:memref-int (aref missing-list-ref 0) 0 (* i 4) :unsigned-long))
	      missing-charsets))
      (values font-set missing-charsets
	      (unless (zerop (aref def-string-ref 0))
		(ff:char*-to-string (aref def-string-ref 0)))))))

#+ics
(defmethod initialize-instance :after ((fs font-set) &key
						     foreign-address display base-names)
  (unless foreign-address
    (multiple-value-bind (x missing)
	(create-font-set display base-names)
      (when missing
	(let ((*error-output* excl:*initial-terminal-io*))
	  (warn "Missing charsets:~{ ~A,~} creating fontset for ~A"
		missing base-names)))
      (when (zerop x) (error "Cannot create fontset for ~S" base-names))
      (setf (foreign-pointer-address fs) x)
      (register-address fs))))

#+ics
(defun fonts-of-font-set (font-set)
  (with-ref-par ((font-struct-list-return-ref 0)
		 (font-name-list-return-ref 0))
    (let ((n (x11:xfontsoffontset font-set
				  font-struct-list-return-ref
				  font-name-list-return-ref))
	  (fonts nil))
      (dotimes (i n)
	(let ((name (ff:char*-to-string
		     (sys:memref-int (aref font-name-list-return-ref 0)
				     0 (* i 4) :unsigned-long))))

	  (push (intern-object-address
		 (sys:memref-int (aref font-struct-list-return-ref 0)
				 0 (* i 4) :unsigned-long)
		 'font :name name)
		fonts)))
      (nreverse fonts))))

#+ics
(defun text-extents (font-set string)
  (let* ((euc (excl:string-to-euc string))
	 (length (1- (length euc))))
    (with-ref-par ((overall-ink-return 0)
		   (overall-logical-return 0))
      (values (x11:xmbtextextents
	       font-set
	       (ff:euc-to-char* euc)
	       length
	       overall-ink-return
	       overall-logical-return)))))
