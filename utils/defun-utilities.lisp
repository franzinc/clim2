;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader$

(in-package :clim-utils)

"Copyright (c) 1991 International Lisp Associates.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

;;; Useful proclamations, very early on

#+(or ccl-2 excl)	;Franz and Coral appear not to support these declarations
(eval-when (compile load eval)
  (proclaim '(declaration values))
  (proclaim '(declaration arglist)))

;;; Moved here from DEFUN.  DEFUN now only contains the portable implementation
;;; of the DYNAMIC-EXTENT declaration, and so is not loaded into Lisps which 
;;; implement that declaration.

;;; This file has to be loaded BEFORE DEFUN.

(defparameter *declarations-may-be-exposed-by-macro-expansion* nil)

(lisp:defun extract-declarations (body &optional environment)
  (declare (values documentation declarations body))
  (let ((declarations nil)
	(documentation nil))
    (block process-declarations
      (loop
	(when (null body) (return-from process-declarations))
	(let ((form (first body)))
	  (cond ((stringp form)
		 (setf documentation (or documentation form)
		       body (cdr body)))
		((atom form) (return-from process-declarations))
		;; X3J13 says this nonsense is not required any more:
		(*declarations-may-be-exposed-by-macro-expansion*
		 (block expand-macros
		   (loop
		     (when (eql (first form) 'declare)
		       (setf declarations (append declarations (cdr form))
			     body (cdr body))
		       (return-from expand-macros))
		     (multiple-value-bind (new-form macro-expanded-p)
			 (macroexpand-1 form environment)
		       (unless macro-expanded-p (return-from process-declarations))
		       (setf form new-form)))))
		(t (if (eql (first form) 'declare)
		       (setf declarations (append declarations (cdr form))
			     body (cdr body))
		     (return-from process-declarations)))))))
    (values documentation `((declare ,@declarations)) body)))


