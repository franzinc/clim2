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
;; $fiHeader: macros.lisp,v 1.14 1993/07/27 01:53:11 colin Exp $

(in-package :tk)

(defvar *temp-with-ref-par* nil)
(defvar *temp-with-signed-ref-par* nil)

(defmacro with-ref-par (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
    (destructuring-bind
	((var value &optional signed) &rest more-bindings) bindings
      (let ((val (gensym)))
	`(let ((,val ,value)
	       (,var ,(if signed
			  `(or (pop *temp-with-signed-ref-par*)
			       (make-array 1 :element-type '(signed-byte 32)))
			`(or (pop *temp-with-ref-par*)
			  (make-array 1 :element-type '(unsigned-byte 32))))))
	   ,(if signed
		`(declare (type (simple-array (signed-byte 32) (1)) ,var))
	      `(declare (type (simple-array (unsigned-byte 32) (1)) ,var)))
	   (setf (aref ,var 0) ,val)
	   (multiple-value-prog1
	       (with-ref-par ,more-bindings ,@body)
	     ,(if signed
		  `(push ,var *temp-with-signed-ref-par*)
		`(push ,var *temp-with-ref-par*))))))))



(defmacro object-display (object)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (excl::slot-value-using-class-name 'display-object ,object
					'display)))

(defvar *malloced-objects*)

(defun note-malloced-object (obj &optional (free #'excl::free))
  (push (cons free obj) *malloced-objects*)
  obj)

(defmacro with-malloced-objects (&body body)
  `(let ((*malloced-objects* nil))
     (unwind-protect
	 (progn ,@body)
       (dolist (entry *malloced-objects*)
	 (funcall (car entry) (cdr entry))))))
