;; -*- mode: common-lisp; package: x11 -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: load-xlib.lisp,v 1.9 92/12/14 15:05:14 cer Exp $

(in-package :x11)

(defmacro symbols-from-file (&rest files)
  (let ((r nil))
    (dolist (file files `(quote ,(nreverse r)))
      (with-open-file (s file :direction :input)
	(do ((l (read-line s nil nil) (read-line s nil nil)))
	    ((null l))
	  (push (ff:convert-to-lang l) r))))))


(defun load-undefined-symbols-from-library (file what libraries)
  (let* ((n (length what)))
    (when (> n 0)			; bug2898
      (let ((names (coerce what 'vector))
	    (entry-points (make-array n :element-type '(unsigned-byte 32))))
	(declare (type (simple-array (unsigned-byte 32) (*))))
	(when (> (ff:get-entry-points names entry-points) 0)
	  #+ignore
	  (dotimes (i n) 
	    (when (= (aref entry-points i)
		     sys::*impossible-load-address*)
	      (format t ";; ~A is undefined~%" (aref names i))))
	  (load file
		:system-libraries libraries
		:print t))))))

(defvar sys::*libx11-pathname* "/x11/R4/sun4-lib/libX11.a")

(unless (ff:get-entry-point (ff:convert-to-lang "lisp_XDrawString"))
  (load "xlibsupport.o" :system-libraries (list sys::*libx11-pathname*) :print t))

(x11::load-undefined-symbols-from-library
 "stub-x.o"
 (x11::symbols-from-file "misc/undefinedsymbols")
 (list sys::*libx11-pathname*))

