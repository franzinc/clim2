;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

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

