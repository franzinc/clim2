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

;; $fiHeader: clos-patches.lisp,v 1.1 91/08/30 13:57:44 cer Exp Locker: cer $

(in-package :clim-utils)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

;;; This file contains various patches to get around current deficiencies
;;; in various CLOS implementations

#+Lucid
;; In Lucid 4.0, JonL advises that this is needed because we haven't explicitly
;; provided for anonymous classes, and thus some level of error checking will
;; gratuitously slap your hand.
(lucid-common-lisp:defadvice (clos::find-class-set allow-nil)
			     (new-value symbol &optional errorp)
  (if (null symbol) nil (lucid-common-lisp:advice-continue new-value symbol errorp)))

#+(or Lucid excl)
;; I can't figure out how to do this, so for now we will not try to keep the
;; compilation and run-time environments properly separated.
(defun compile-file-environment-p (environment)
  (declare (ignore environment))
  nil)

#+ccl-2
(defun compile-file-environment-p (environment)
  (if (eq environment 'compile-file)
      t
      (ccl::compile-file-environment-p environment)))

#+(or Lucid (and excl (not (version>= 4 1))))
(defgeneric make-load-form (object))

#+Lucid
;; Lucid 4.0 doesn't have MAKE-LOAD-FORM, so add it (with advice from JonL)
(lucid-common-lisp:defadvice (clos::standard-reconstructor-list support-make-load-form)
			     (object)
  (if (clos:compute-applicable-methods #'make-load-form (list object))
      (multiple-value-bind (form1 form2)
	  (make-load-form object)
	(when form2
	  (error "MAKE-LOAD-FORM with two values is not supported yet.~@
		  (MAKE-LOAD-FORM ~S) => ~S ~S"
		 object form1 form2))
	;; We have to return not a form, but a cons of a function and arguments
	(unless (and (consp form1)
		     (symbolp (first form1))
		     (not (special-form-p (first form1)))
		     (not (macro-function (first form1)))
		     (every #'constantp (rest form1)))
	  (error "MAKE-LOAD-FORM is too complicated.~@
		  (MAKE-LOAD-FORM ~S) => ~S ~S"
		 object form1 form2))
	(cons (first form1) (mapcar #'eval (rest form1))))
      (lucid-common-lisp:advice-continue object)))

;; this should be (not (version>= 4 1)) except of bug2008

#+excl 
;; Allegro CL doesn't have MAKE-LOAD-FORM, so add it (with advice from Foderaro)
(excl:defadvice comp::wfasl-lispobj (implement-make-load-form :before)
  (let ((object (first excl:arglist)))
    (when (typep object 'standard-object)
      (multiple-value-bind (form1 form2)
	  (make-load-form object)
	(when form2
	  (error "MAKE-LOAD-FORM with two values is not supported yet.~@
		  (MAKE-LOAD-FORM ~S) => ~S ~S"
		 object form1 form2))
	(return
 	  (apply #'comp::wfasl-lispobj
 		 (cons compiler::*eval-when-load-marker* form1)
		 (rest excl:arglist)))))))

;; this should be (not (version>= 4 1)) except of bug2008

#+excl
(excl:compile-advice 'comp::wfasl-lispobj)

#+Lucid
;; Work around a Lucid 4.0 bug in anonymous classes
;; JonL suggests that a better way might be to leave CLASS-NAME = NIL
;; and store that information in our own private slot, then we won't
;; trigger this bug by having a non-symbol in the NAME slot
(lucid-common-lisp:defadvice (lucid:class-or-class-name-p fix-list-named-class)
			     (&rest args) 
  (let ((answer (lucid-common-lisp:apply-advice-continue args)))
    (if (consp answer)
	(first args)	;return the class
	answer)))

#+PCL
;;; PCL doesn't allow using a class instead of a class-name as a
;;; parameter specializer name in DEFMETHOD.  Add the feature.
;;; Lucid CLOS and Symbolics CLOS have this feature.
(defun pcl::parse-specializers (specializers)
  (flet ((parse (spec)
	   (cond ((symbolp spec)
		  (or (find-class spec nil)
		      (error
			"~S used as a specializer,~%~
                         but is not the name of a class."
			spec)))
		 ((and (listp spec)
		       (eq (car spec) 'eql)
		       (null (cddr spec)))
		  (make-instance 'pcl::eql-specializer :object (cadr spec))	;*EQL*
;		  spec
		  )
		 ;;--- This cond clause is added by this patch
		 ((typep spec 'class)
		  spec)
		 (t (error "~S is not a legal specializer." spec)))))
    (mapcar #'parse specializers)))

#+(and excl (not (version>= 4 0)))
;;; This is needed to prevent a MAKE-LOAD-FORM form from being evaluated before
;;; an earlier top-level form, says Foderaro.  Even the forward reference allowed
;;; by load-reference-to-presentation-type-class isn't sufficient without this,
;;; because a MAKE-LOAD-FORM form for a presentatation type class could be evaluated
;;; before a superclass has been defined.
(eval-when (compile load eval)
  (setq compiler::.random-forms-max. 0))


;;; Go through this rigamarole because WITH-SLOTS doesn't accept declarations
;;; on Lucid and Franz

#+(or Lucid (and excl (or :rs6000 (not (version>= 4 1)))))
(lisp:defun slot-value-alist (body)
  (declare (values real-body alist))
  (let ((alist nil))
    (do* ((real-body body (cdr real-body))
	  (form (car real-body) (car real-body)))
	 ((or (null real-body)
	      (not (and (consp form)
			(eq (first form) 'declare))))
	  (values real-body alist))
      (dolist (spec (rest form))
	(let ((type (first spec)))
	  (dolist (var (rest spec))
	    (push (cons var type) alist)))))))

#+(or Lucid (and excl (or :rs6000 (not (version>= 4 1)))))
(defparameter *with-slots* #+PCL 'pcl::with-slots 
			   #+(and excl (or :rs6000 (not (version>= 4 1)))) 'clos::with-slots
			   #-(or (and excl (or :rs6000 (not (version>= 4 1)))) PCL) 'clos:with-slots)

#+(or Lucid (and excl (or :rs6000 (not (version>= 4 1)))))
(defparameter *slot-value* #+PCL 'pcl::slot-value
			   #+(and excl (or :rs6000 (not (version>= 4 1)))) 'clos::slot-value
			   #-(or (and excl (or :rs6000 (not (version>= 4 1)))) PCL) 'clos:slot-value)

#+(or Lucid (and excl (or :rs6000 (not (version>= 4 1)))))
(defmacro with-slots (slot-entries instance-form &body body &environment environment)
  (multiple-value-bind (real-body alist) (slot-value-alist body)
    (let ((expansion (macroexpand `(,*with-slots* ,slot-entries ,instance-form
				     ,@real-body)
				  environment)))
      (lisp:labels
	((fix-tree (tree &optional first)
	   (typecase tree
	     (cons
	       (when (and first
			  (eq (car tree) *slot-value*)
			  (consp (cdr tree))
			  (consp (cddr tree))
			  (null (cdddr tree)))
		 (let ((third (third tree)))
		   (when (and (consp third)
			      (eq (car third) 'quote)
			      (consp (cdr third))
			      (null (cddr third)))
		     (let ((slot-name (second third)))
		       (when (symbolp slot-name)
			 (let ((type (cdr (assoc slot-name alist))))
			   (when type
			     (return-from fix-tree
			       `(the ,type (,*slot-value* ,(fix-tree (second tree) t)
					    ',slot-name))))))))))
	       (cons (fix-tree (car tree) t)
		     (fix-tree (cdr tree) nil)))
	     (otherwise tree))))
	(fix-tree expansion)))))
