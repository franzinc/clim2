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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $fiHeader: macros.lisp,v 1.14 1993/07/27 01:53:11 colin Exp $

(in-package :tk)

(defmacro def-foreign-array-resource (name constructor)
  `(progn
     (clim-sys:defresource ,name (n)
       :constructor (cons n (,constructor :number n))
       :matcher (not (< (car ,name) n)))
     (defmacro ,(intern (format nil "~A-~A" 'with name))
	 ((var n) &body body)
       `(clim-sys:using-resource (,var ,',name ,n)
	  (let ((,var (cdr ,var)))
	    ,@body)))))

(defmacro define-ref-par-types (&rest types)
  (let ((forms nil))
    (dolist (type types)
      (let ((type-array (intern (format nil "~A-~A" type 'array)))
	    (make-type-array (intern (format nil "~A-~A-~A"
					     'make type 'array))))
	(setq forms
	  `(,@forms
	    (ff:def-c-type ,type-array 1 ,type)
	    (def-foreign-array-resource
		,type-array ,make-type-array)))))
    `(progn ,@forms)))

(define-ref-par-types
    :unsigned-int :int :unsigned-long :long
    *)

(defmacro with-ref-par (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
    (destructuring-bind
	((var value type) &rest more-bindings)
	bindings
      (let ((&var (intern (format nil "&~A" var)))
	    (val '#:val)
	    (with-type-array (intern (format nil "~A-~A-~A" 'with type 'array)
				     (find-package :tk)))
	    (type-array (intern (format nil "~A-~A" type 'array)
				(find-package :tk))))
	`(let ((,val ,value))
	   (,with-type-array (,&var 1)
	     (symbol-macrolet ((,var (,type-array ,&var 0)))
	       (setf ,var ,val)
	       (multiple-value-prog1
		   (with-ref-par ,more-bindings ,@body)))))))))

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
