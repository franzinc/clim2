;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[Mon Sep 26 02:02:28 1994 by smh]-
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
;; $fiHeader: foreign-obj.lisp,v 1.13 1994/11/23 23:29:13 smh Exp $

(in-package :tk)

(defvar *address->object-mapping* (make-hash-table :test 'equal))

(defclass display (ff:foreign-pointer)
  ((context :initarg :context :reader display-context)
   (xid->object-mapping :initform (make-hash-table :test #'equal)
			clos::fixed-index 0)))

(defmacro display-xid->object-mapping (object)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (excl::slot-value-using-class-name 'display ,object 'xid->object-mapping)))

(defun find-object-from-xid (handle display &optional (errorp t))
  (find-object-from-handle handle (display-xid->object-mapping display) errorp))

(defun find-object-from-address (handle &optional (errorp t))
  (find-object-from-handle handle *address->object-mapping* errorp))

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

(defun register-xid (object display &optional (handle (foreign-pointer-address object)))
  (setf (gethash handle (display-xid->object-mapping display)) object)
  object)

(defun unregister-xid (object display &optional (handle (foreign-pointer-address object)))
  (remhash handle (display-xid->object-mapping display))
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

(defun intern-object-xid (handle class display &rest initargs)
  (apply #'intern-object-1
	 (display-xid->object-mapping display)
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

#+ics
(defun fat-string-to-string8 (string)
  (let* ((length (length string))
	 (string8 (make-array (1+ length) :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref string8 i)
	(logand (char-code (char string i)) #xff)))
    (setf (aref string8 length) 0)
    string8))

#+ics
(defun fat-string-to-string16 (string)
  (let* ((length (length string))
	 (string16 (make-array (1+ length) :element-type '(unsigned-byte 16))))
    (dotimes (i length)
      (setf (aref string16 i)
	(xchar-code (char string i))))
    (setf (aref string16 length) 0)
    string16))

#+ics
(defun xchar-code (char)
  (let ((code (char-code char)))
    (logand code
	    (if (logbitp 15 code)
		;; jis-x208 and gaiji
		#x7f7f
	      ;; ascii and jis-x201
	      #xff))))

(defun setlocale (&optional (category 0) locale)
  (let ((r (setlocale-1 category (or (and locale
					(ff:string-to-char* locale))
				   0))))
    (unless (zerop r)
      (ff:char*-to-string r))))
