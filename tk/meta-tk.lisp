;; -*- mode: common-lisp; package: tk -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: meta-tk.lisp,v 2.5 2004/01/16 19:15:44 layer Exp $

(in-package :tk)


(defclass xt-class (ff:foreign-pointer standard-class)
  ((entry-point :initarg :entry-point
		:initform nil
		:reader class-entry-point)
   ;; Resources are now looked up on the fly, and cached lazily one by one.
   ;; Note that direct resources and direct constraint resources are no
   ;; longer available, since initializing the XT widget class removes the
   ;; ability to determine them lazily.
   (cached-resources :initform (make-hash-table :test #'equal)
		     :type hash-table)
   (specially-hacked-resources :initform nil) ; See add-resource-to-class.
   (cached-constraint-resources :initform (make-hash-table :test #'equal)
		     :type hash-table)
   (get-values-cache :initform (make-hash-table :test #'equal)
		     :type hash-table
		     :reader class-get-values-cache)
   (set-values-cache :initform (make-hash-table :test #'equal)
		     :type hash-table
		     :reader class-set-values-cache)))

(defmethod describe-object :after ((class xt-class) stream)
  (with-slots (entry-point) class
    (format stream "The entry point is ~A,~X~%"
	    (and (slot-boundp class 'entry-point) entry-point)
	    (ff:foreign-pointer-address class))))

(defmethod class-handle ((class xt-class))
  (unless (clos:class-finalized-p class)
    (clos:finalize-inheritance class))
  (dolist (c (clos:class-precedence-list class)
	    (error "Cannot get handle for class" class))
    (when (and (typep c 'xt-class)
	       (not (zerop (ff:foreign-pointer-address c))))
      (return c))))

