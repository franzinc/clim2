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
;; $fiHeader: foreign-obj.cl,v 1.3 92/01/06 20:43:44 cer Exp Locker: cer $

(in-package :tk)

(defclass handle-class ()
	  ((handle :initarg :handle :reader object-handle)))

(defmethod invalidate-object-handle ((object handle-class) handle)
  (declare (ignore handle))
  (slot-makunbound object 'handle))


;;; There should be multiple mappings:
;;; Xid -> object
;;; Address -> object

(defvar *address->object-mapping* (make-hash-table))
(defvar *xid->object-mapping* (make-hash-table))

(defun find-object-from-address (handle &optional (errorp t))
  (find-object-from-handle handle *address->object-mapping* errorp))

(defun find-object-from-xid (handle &optional (errorp t))
  (find-object-from-handle handle *xid->object-mapping* errorp))

(defun find-object-from-handle (handle table errorp)
  (cond ((gethash handle table))
	(errorp 
	 (error "Cannot find object from handle: ~S" handle))))

(defun register-address (object &optional (handle (object-handle object)))
  (setf (gethash handle *address->object-mapping*) object)
  object)

(defun unregister-address (object &optional (handle (object-handle object)))
  (remhash handle *address->object-mapping*)
  (invalidate-object-handle object handle))

(defun register-xid (object &optional (handle (object-handle object)))
  (setf (gethash handle *xid->object-mapping*) object)
  object)

(defun intern-object-address (handle class &rest initargs)
  (apply #'intern-object-1 
	 *address->object-mapping*
	 handle 
	 class 
	 initargs))


(defun intern-object-xid (handle class &rest initargs)
  (apply #'intern-object-1 
	 *xid->object-mapping*
	 handle 
	 class 
	 initargs))


(defun intern-object-1 (table handle class &rest initargs)
  (let ((x (find-object-from-handle handle table nil)))
    (cond ((null x)
	   (setf (gethash handle table)
	     (apply #'make-instance 
		    class 
		    :handle handle
		    initargs)))
	  ((typep x class) x)
	  (t
	   (cerror "Make a new one"
		   "~s has the wrong class: ~s"
		   x class)
	   (setf (gethash handle table)
	     (apply #'make-instance 
		    class 
		    :handle handle
		    initargs))))))
