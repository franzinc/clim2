;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: defresource.lisp,v 1.6 91/03/26 12:47:53 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

(defstruct (resource-descriptor (:conc-name RD-)
				#+Allegro (:print-function print-resource)
				(:constructor make-resource-descriptor (name)))
  name
  (objects nil)
  constructor
  initializer
  deinitializer
  matcher
  (lock (make-lock "a resource lock")))

#+Allegro
(defun print-resource (o s d)
  (declare (ignore d))
  (format s "#<~S ~S>" (type-of o) (RD-name o)))

(defstruct (object-storage (:conc-name OS-))
  object
  use-cons
  parameters)

(defmacro with-resource-rd ((resource rd) &body body)
  (let ((resource-var (gensymbol 'resource)))
    `(let ((,resource-var ,resource)		;once-only...
	   (,rd nil))
       (cond ((symbolp ,resource-var)
	      (setq ,rd (lookup-resource-descriptor ,resource-var)))
	     ((typep ,resource-var 'resource-descriptor)
	      (setq ,rd ,resource-var)))
       (unless ,rd (error "Can't find resource ~S" ,resource))
       ,@body)))

(defun describe-resource (resource)
  (with-resource-rd (resource RD)
    (format t "~&Resource ~S:" (RD-name RD))
    (when (RD-constructor RD)
      (format t "~%Constructor: ~S" (RD-constructor RD)))
    (when (RD-initializer RD)
      (format t "~%Initializer: ~S" (RD-initializer RD)))
    (when (RD-matcher RD)
      (format t "~%Matcher: ~S" (RD-matcher RD)))
    (let ((objects (RD-objects RD)))
      (when (> (fill-pointer objects) 0)
	(format t "~%~D Object~:P:" (fill-pointer objects)))
      (doseq (object-storage objects)
	(format t "~%~S, ~:[Not in~;In~] use"
		(os-object object-storage)
		(os-use-cons object-storage))))))

(defun clear-resource (resource)
  (with-resource-rd (resource RD)
    (setf (fill-pointer (RD-objects RD)) 0))
  nil)

(defun lookup-resource-descriptor (name)
  (get name 'defresource))

(defun set-resource-descriptor (name value)
  (setf (get name 'defresource) value))

(defsetf lookup-resource-descriptor set-resource-descriptor)

(defun default-resource-matcher (object object-storage &rest parameters)
  (declare (ignore object))
  (declare (dynamic-extent parameters))
  (let ((object-parameters (os-parameters object-storage)))
    (equal object-parameters parameters)))

(defmacro defresource (name parameters &key constructor initializer deinitializer
		       matcher initial-copies)
  (unless constructor
    (error "Can't make a resource without a constructor: ~S" name))
  (let ((result ()))
    (labels ((output (form) (push form result))
	     (cleanup-lambda-list (lambda-list)
	       (mapcar #'(lambda (x) (if (listp x) (first x) x))
		       (set-difference lambda-list lambda-list-keywords)))
	     (output-function (function type &rest extra-parameters)
	       (declare (dynamic-extent extra-parameters))
	       (cond ((null function) nil)
		     ((symbolp function) `#',function)
		     (t (let ((function-name
				(make-symbol (concatenate 'string
							  (string name) "-" (string type)))))
			  (output `(defun ,function-name (,@extra-parameters ,@parameters)
				     ,@extra-parameters ,@(cleanup-lambda-list parameters)
				     ,function))
			  `#',function-name)))))
      (setf constructor (output-function constructor 'constructor (make-symbol "RD"))
	    initializer (output-function initializer 'initializer name)
	    matcher (or (output-function matcher 'matcher name (make-symbol "OBJECT-STORAGE"))
			(output-function 'default-resource-matcher 'matcher))
	    deinitializer (letf-globally ((parameters nil))
			    (output-function deinitializer 'deinitializer name)))
      (output `(defresource-load-time ',name
		 ,constructor
		 ,initializer
		 ,deinitializer
		 ,matcher
		 ',initial-copies)))
    `(define-group ,name defresource ,@(nreverse result))))

(defun defresource-load-time (name constructor initializer deinitializer matcher
			      initial-copies)
  #+Ignore
  (when (lookup-resource-descriptor name)
    (cerror "Continue to overwrite old definition of resource ~S"
	    "Attempt to redefine resource ~S" name))
  (let ((RD (or (lookup-resource-descriptor name) (make-resource-descriptor name))))
    (with-lock-held ((RD-lock RD) "Resource lock")
      (unless (RD-objects RD)
	(setf (RD-objects RD) (make-array (* 2 (or initial-copies 10))
					  :fill-pointer 0))
	(when initial-copies
	  (dotimes (i initial-copies)
	    #-(or Allegro Minima) (declare (ignore i))
	    (vector-push (cons nil (funcall constructor RD)) (RD-objects RD)))))
      (setf (RD-constructor RD) constructor)
      (setf (RD-initializer RD) initializer)
      (setf (RD-deinitializer RD) deinitializer)
      (setf (RD-matcher RD) matcher)
      (setf (lookup-resource-descriptor name) RD))))

(defun allocate-resource (name &rest parameters &aux RD object-storage)
  (declare (dynamic-extent parameters))
  (block allocate
    (setf RD (lookup-resource-descriptor name))
    (unless RD (error "Can't allocate nonexistent resource ~S" name))
    (with-lock-held ((RD-lock RD) "Resource lock")
      (let* ((array (RD-objects RD))
	     (matcher (RD-matcher RD))
	     (fill-pointer (fill-pointer array)))
	#+Genera (declare (sys:array-register array))
	#+Minima (declare (type vector array))
	(dotimes (i fill-pointer)
	  (setf object-storage (aref array i))
	  (when (and (null (cdr (os-use-cons object-storage)))
		     (if (null matcher)
			 (equal (os-parameters object-storage) parameters)
			 (apply matcher (os-object object-storage) object-storage parameters)))
	    (if (os-use-cons object-storage)
		(setf (car (os-use-cons object-storage)) RD
		      (cdr (os-use-cons object-storage)) i)
		(setf (os-use-cons object-storage) (cons RD i)))
	    (return-from allocate)))
	(let* ((new-object (apply (RD-constructor RD) RD parameters))
	       (array-size (array-dimension array 0)))
	  (setf object-storage (make-object-storage
				 :object new-object
				 :use-cons (cons RD fill-pointer)
				 :parameters (copy-list parameters)))
	  (when (<= array-size fill-pointer)
	    (let ((new-array (make-array (* 2 array-size)
					 :fill-pointer fill-pointer)))
	      (replace new-array array)
	      (setf array (setf (RD-objects RD) new-array))))
	  (vector-push object-storage array)))))
  (when (RD-initializer RD) (apply (RD-initializer RD) (os-object object-storage) parameters))
  (values (os-object object-storage) object-storage))

(defun deallocate-resource (name object &optional allocation-key
			    &aux RD object-array object-index object-storage)
    (if allocation-key
	(setf RD (car (os-use-cons allocation-key)) object-array (RD-objects RD)
	      object-index (cdr (os-use-cons allocation-key)))
	(setf RD (or (lookup-resource-descriptor name)
		     (error "Can't deallocate nonexistent resource ~S" name))
	      object-array (RD-objects RD)
	      object-index (position object object-array
				     :key #'os-object)))
    (unless object-index
      (error "Can't deallocate object ~S: not present in resource ~S" object (RD-name RD)))
    (with-lock-held ((RD-lock RD) "Resource lock")
      (setf object-storage (aref object-array object-index))
      (unless (cdr (os-use-cons object-storage))
	(error "Can't deallocate object ~S: already deallocated from resource ~S"
	       object (RD-name RD)))
      (unless (eql object (os-object object-storage))
	(error "Can't deallocate object ~S: not present in resource ~S" object (RD-name RD)))
      (when (RD-deinitializer RD)
	(funcall (RD-deinitializer RD) (os-object object-storage)))
      (setf (cdr (os-use-cons object-storage)) nil)))

(defmacro using-resource ((variable resource &rest parameters) &body body)
  (let ((allocation-key (make-symbol "ALLOCATION-KEY")))
    `(let ((,variable nil)
	   (,allocation-key nil))
       (unwind-protect
	   (progn (multiple-value-setq (,variable ,allocation-key)
		    (allocate-resource ',resource ,@parameters))
		  ,@body)
	 (when ,allocation-key
	   (deallocate-resource ',resource ,variable ,allocation-key))))))

;;; A combination of LETF-GLOBALLY and USING-RESOURCE -- written because
;;; we only want to have a single unwind-protect in a stack frame if
;;; possible.  Who knows how slow they are?
(defmacro letf-using-resource ((place resource &rest parameters) &body body)
  (let ((old-place (gensymbol 'letf-globally-temp))
	(resource-variable (gensymbol resource 'temp))
	(allocation-key (gensymbol 'allocation-key)))
    `(let ((,resource-variable nil)
	   (,allocation-key nil)
	   (,old-place ,place))
       (unwind-protect
	   (progn (multiple-value-setq (,resource-variable ,allocation-key)
		    (allocate-resource ',resource ,@parameters))
		  (setf ,place ,resource-variable)
		  ,@body)
	 (setf ,place ,old-place)
	 (when ,allocation-key (deallocate-resource ',resource ,resource-variable
						    ,allocation-key))))))
