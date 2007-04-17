;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: clos-patches.lisp,v 2.7 2007/04/17 21:45:54 layer Exp $

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; This file contains various patches to get around current deficiencies
;;; in various CLOS implementations

#+Lucid
;; In Lucid 4.0, JonL advises that this is needed because we haven't explicitly
;; provided for anonymous classes, and thus some level of error checking will
;; gratuitously slap your hand.
(lucid-common-lisp:defadvice (clos::find-class-set allow-nil)
                             (new-value symbol &optional errorp)
  (if (null symbol) nil (lucid-common-lisp:advice-continue new-value symbol errorp)))

#+Lucid
;; I can't figure out how to do this, so for now we will not try to keep the
;; compilation and run-time environments properly separated.
(defun-inline compile-file-environment-p (environment)
  (declare (ignore environment))
  nil)

#+allegro
(defun-inline compile-file-environment-p (environment)
  #-(version>= 7 0)
  (or (eq environment 'compile-file) excl::*compiler-environment*)
  #+(version>= 7 0)
  (or (and sys::*compile-file-environment*
	   (excl::compilation-environment-p environment)
	   environment)
      sys::*compile-file-environment*))

#+(and allegro never-in-a-million-years)
(eval-when (compile)
  (warn "~S hacked for lack of environment support in 4.1" 'compile-file-environment-p))

#+CCL-2
(defun-inline compile-file-environment-p (environment)
  (if (eq environment 'compile-file)
      t
      (ccl::compile-file-environment-p environment)))

#+(and allegro (not (version>= 4 1)))
(defgeneric make-load-form (object))

#+(and allegro (not (version>= 4 1 40)))        ; 40 is arbitrary, I mean > beta. ;; Allegro CL doesn't have MAKE-LOAD-FORM, so add it (with advice from Foderaro)
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

#+(and allegro (not (version>= 4 1 40)))
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
        (first args)        ;return the class
        answer)))

#+(and allegro (not (version>= 4 0)))
;;; This is needed to prevent a MAKE-LOAD-FORM form from being evaluated before
;;; an earlier top-level form, says Foderaro.  Even the forward reference allowed
;;; by load-reference-to-presentation-type-class isn't sufficient without this,
;;; because a MAKE-LOAD-FORM form for a presentatation type class could be evaluated
;;; before a superclass has been defined.
(eval-when (compile load eval)
  (setq compiler::.random-forms-max. 0))


;;; Go through this rigamarole because WITH-SLOTS doesn't accept declarations
;;; on old versions of Lucid and Franz Allegro

#+(and allegro (not (version>= 4 1)))
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
        (let ((type (if (eq (first spec) 'type) (second spec) (first spec)))
              (vars (if (eq (first spec) 'type) (cddr spec) (cdr spec))))
          (dolist (var vars)
            (push (cons var type) alist)))))))

#+(and allegro (not (version>= 4 1)))
(defparameter *with-slots*
              #+PCL 'pcl::with-slots 
              #+(and allegro (not (version>= 4 1))) 'clos::with-slots
              #-(or (and allegro (not (version>= 4 1))) PCL) 'clos:with-slots)

#+(and allegro (not (version>= 4 1)))
(defparameter *slot-value*
              #+PCL 'pcl::slot-value
              #+(and allegro (not (version>= 4 1))) 'clos::slot-value
              #-(or (and allegro (not (version>= 4 1))) PCL) 'clos:slot-value)

#+(and allegro (not (version>= 4 1)))
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



