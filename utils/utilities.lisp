;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: utilities.lisp,v 1.13 1998/08/06 23:17:37 layer Exp $

;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved. 
;;;

(in-package :clim-utils)

;;;
;;; Various UTILITIES
;;; 

;;;
;;; COMMONLISP Extensions
;;; 

;; now we have cl:boolean we don't need this (cim 2/24/94)
;;mm: except in aclpc  28-Dec-94
#+aclpc ;; was #+(or aclpc acl86win32) -tjm 23May97
(deftype boolean nil '(member nil t))

(defmacro compile-time-warn (warn-string)
  (warn warn-string)
  nil)

(eval-when (compile load eval)
(defmacro with-collection (&body body)
  `(let (($with-collection-result$ nil)
         $with-collection-tail$)
     (macrolet
         ((collect (form)
            ;;  The FORM is evaluated first so that COLLECT nests
            ;; properly, i.e., The test to determine if this is
            ;; the first value collected should be done after the
            ;; value itself is generated in case it does
            ;; collection as well.
            `(let (($collectable$ ,form))
               (if $with-collection-tail$
                   (rplacd $with-collection-tail$
                           (setq $with-collection-tail$
                                 (list $collectable$)))
                   (setq $with-collection-result$
                         (setq $with-collection-tail$
                               (list $collectable$))))
               $with-collection-tail$)))
       ,@body $with-collection-result$))))

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(mapcar #'(lambda (symbol)
                     `(,symbol (gensymbol ',symbol)))
                 vars)
     ,@body))

(defmacro with-fast-vector-references ((&rest macros-and-arrays) &body body)
  (flet ((simple-part-accessor (array)
           ;; Allegro only allows SVREF on simple T vectors.
           #+Allegro `(let ((temp ,array))
                        (etypecase temp
                          (simple-vector temp)
                          ((vector t)
                           (let ((temp2 (excl::ah_data temp)))
                             (setq temp2 (if (consp temp2)
                                             (cdr temp2)
                                             temp2))
                             (assert (and 
                                       (zerop (the fixnum (excl::ah_displacement temp)))
                                       (typep temp2 'simple-vector))
                                     ()
                                     "Arrays passed to ~S must be non-displaced"
                                     'with-fast-vector-references)
                             temp2))))
           #+Genera array
           #-(or Allegro Genera) array)
         (internal-binding-declarations (variables)
           #+Allegro `(declare (simple-vector ,@variables))
           #+Genera `(declare (sys:array-register ,@variables))
           #-(or Allegro Genera) `(declare)))
    (let* ((aref #+(or Allegro Genera) 'svref
                 #-(or Allegro Genera) 'aref)
           (macro-names (mapcar #'first macros-and-arrays))
           (internal-variables (mapcar #'gensymbol macro-names))
           (arrays (mapcar #'second macros-and-arrays))
           (bindings (mapcar #'(lambda (variable array)
                                 `(,variable ,(simple-part-accessor array)))
                             internal-variables arrays))
           (macros (mapcar #'(lambda (macro-name variable)
                               `(,macro-name (index) 
                                  `(,',aref ,',variable (the fixnum ,index))))
                           macro-names internal-variables)))
  `(let ,bindings
     ,(internal-binding-declarations internal-variables)
     (progn ,@internal-variables nil)
     (macrolet ,macros
       ,@body)))))


;;;
;;; Generates macros for accessing a slot used as a property list.
;;;

(defmacro def-property-slot-macros (name (class) slot-name)
  (declare (ignore class))
  `(progn
     (defmacro ,name (object key)
       `(getf (slot-value ,object ',',slot-name) ,key))
     (defsetf ,name (object key) (val)
       `(setf 
         (getf (slot-value ,object ',',slot-name) ,key) 
         ,val))))

(defmacro def-property-slot-accessors (name (class) slot-name)
  `(progn
     (defmethod ,name ((object ,class) key)
       (getf (slot-value object ',slot-name) key))
     (defmethod (setf ,name) (val (object ,class) key)
       (setf (getf (slot-value object ',slot-name) key) val))))

;;;
;;; Stub Sealers 
;;;

(defun unimplemented (&optional (string "So who you gun-na call..."))
  (error "Unimplemented: ~s." string))

(defmacro define-unimplemented-protocol-method (name protocol-name lambda-list)
  (let ((variables (with-collection
                       (dolist (var lambda-list)
                         (unless (member var lambda-list-keywords)
                           (collect (if (consp var) (car var) var))))))
        (protocol-var (first (find-if #'(lambda (lambda-var)
                                          (and (consp lambda-var)
                                               (eq (second lambda-var) protocol-name)))
                                      lambda-list))))
    `(defmethod ,name ,lambda-list
                #+Genera (declare 
                          (sys:function-parent ,name
                                               define-unimplemented-protocol-method))
       (progn ,@variables)                ;Ignore these variables...
       (error "The required operation ~S is not defined for the~
               ~@[ ~S implementation of~] protocol ~S"
              ',name
              ,(when protocol-var `(class-name (class-of ,protocol-var)))
              ',protocol-name))))

(defun warn-obsolete (fn)
  (warn "Obsoleted Call: ~a" fn))


(defun safe-slot-value (instance slot-name)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)
      "Unbound"))
