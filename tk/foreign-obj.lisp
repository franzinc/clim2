;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defvar *address->object-mapping* (make-hash-table :test 'equal))

(defclass display (ff:foreign-pointer)
  ((context :initarg :context :reader display-context)
   (xid->object-mapping :initform (make-hash-table :test #'equal)
			excl::fixed-index 0)))

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


