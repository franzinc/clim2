(in-package :tk)

(defclass handle-class ()
	  ((handle :initarg :handle :reader object-handle)))


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
	   (error "~s has the wrong class: ~s"
		  x class)))))
