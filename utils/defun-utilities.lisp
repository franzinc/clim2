;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;;; $Header: /repo/cvs.copy/clim2/utils/defun-utilities.lisp,v 1.13 1998/05/19 18:51:25 layer Exp $

(in-package :clim-utils)

"Copyright (c) 1991 International Lisp Associates.  All rights reserved."

;;; Useful proclamations, very early on

#+(or CCL-2 Allegro Minima)	;not part of ANSI CL, but they're nice to have around
(eval-when (compile load eval)
  (proclaim '(declaration values))
  (proclaim '(declaration arglist)))

#+aclpc
(eval-when (compile load eval)
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
		     (when (eq (first form) 'declare)
		       (setf declarations (append declarations (cdr form))
			     body (cdr body))
		       (return-from expand-macros))
		     (multiple-value-bind (new-form macro-expanded-p)
			 (macroexpand-1 form environment)
		       (unless macro-expanded-p (return-from process-declarations))
		       (setf form new-form)))))
		(t (if (eq (first form) 'declare)
		       (setf declarations (append declarations (cdr form))
			     body (cdr body))
		     (return-from process-declarations)))))))
    (values documentation `((declare ,@declarations)) body)))


;;; DEFINE-GROUP: defines a "group" of definitions which are related
;;; somehow.  In Genera, this causes the function-parents to be set
;;; correctly, for example, and also if you attempt to abort out of the
;;; middle you get told that something might be left inconsistent.
#+Genera 
(defmacro define-group (name type &body body)
  `(sys:multiple-definition ,name ,type ,@body))

#+(and Allegro (not acl86win32) (version>= 4 1))
(defmacro define-group (name type &body body)
  `(progn
     (excl::record-source-file ',name :type ',type)
     ,@body))

#-(or Genera (and (not acl86win32) (and Allegro (version>= 4 1))))
(defmacro define-group (name type &body body)
  (declare (ignore name type))
  `(progn ,@body))

(defmacro with-warnings-for-definition (name type &body body)
  #-Genera (declare (ignore name type)) ;-- Why?
  #+Genera `(let ((compiler:default-warning-function ,name)
		  (compiler:default-warning-definition-type ',type))
	      ,@body)
  #-Genera `(let () ,@body))

(defmacro defun-inline (name lambda-list &body body)
  `(define-group ,name defun-inline
     (eval-when (compile load eval) (proclaim '(inline ,name)))
     (defun ,name ,lambda-list
       ,@body)))

#+Genera
(progn
  (setf (get 'defun-inline 'zwei:definition-function-spec-parser)
	(zl:::scl:function (:property zl:::scl:defun zwei:definition-function-spec-parser)))
  (setf (get 'defun-inline 'zwei:definition-function-spec-type) 'zl:::scl:defun)
  (setf (get 'defun-inline 'gprint::formatter) 
	(zl:::scl:function (:property zl:::scl:defun gprint::formatter)))
  (pushnew 'defun-inline zwei:*irrelevant-functions*)
  (pushnew 'defun-inline zwei:*irrelevant-defining-forms*))

