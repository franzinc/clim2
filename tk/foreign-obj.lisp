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
;; $fiHeader: foreign-obj.lisp,v 1.7 92/03/09 17:40:41 cer Exp Locker: cer $

(in-package :tk)

;;; There should be multiple mappings:
;;; Xid -> object
;;; Address -> object

(defvar *address->object-mapping* (make-hash-table :test 'equal))
(defvar *xid->object-mapping* (make-hash-table :test 'equal))

(defun find-object-from-address (handle &optional (errorp t))
  (find-object-from-handle handle *address->object-mapping* errorp))

(defun find-object-from-xid (handle &optional (errorp t))
  (find-object-from-handle handle *xid->object-mapping* errorp))

(defun find-object-from-handle (handle table errorp)
  (cond ((gethash handle table))
	(errorp 
	 (error "Cannot find object from handle: ~S" handle))))

  
(defun register-address (object &optional (handle (foreign-pointer-address object)))
  (setf (gethash handle *address->object-mapping*) object)
  object)

(defun unregister-address (object &optional (handle (foreign-pointer-address object)))
  (remhash handle *address->object-mapping*)
  (setf (ff:foreign-pointer-address object) 0))

(defun register-xid (object &optional (handle (foreign-pointer-address object)))
  (setf (gethash handle *xid->object-mapping*) object)
  object)

(defun intern-object-address (handle class &rest initargs)
  (apply #'intern-object-1 
	 *address->object-mapping*
	 handle 
	 class 
	 initargs))

(defun unintern-object-address (handle)
  (unintern-object-1 
	 *address->object-mapping*
	 handle))


(defun intern-object-xid (handle class &rest initargs)
  (apply #'intern-object-1 
	 *xid->object-mapping*
	 handle 
	 class 
	 initargs))


(defun unintern-object-1 (table handle)
  (remhash handle table))
    
(defun intern-object-1 (table handle class &rest initargs)
  (let ((x (find-object-from-handle handle table nil)))
    (cond ((null x)
	   (values 
	    (setf (gethash handle table)
	      (apply #'make-instance 
		     class 
		     :foreign-address handle
		     initargs))
	    t))
	  ((typep x class) 
	   (values x nil))
	  (t
	   (cerror "Make a new one"
		   "~s has the wrong class: ~s"
		   x class)
	   (values 
	    (setf (gethash handle table)
	      (apply #'make-instance 
		     class 
		     :foreign-address handle
		     initargs))
	    t)))))
