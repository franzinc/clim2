;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;

(in-package :clim-utils)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

(defstruct (resource-descriptor (:conc-name rd-)
				(:constructor make-resource-descriptor (name)))
  name
  (objects nil)
  constructor
  initializer
  deinitializer
  matcher
  (lock (initial-lock-value)))

(defstruct (object-storage (:conc-name os-))
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
  ;; jump through a hoop to allow for useful debugging in Genera.
  (let ((format
	  #+Genera
	  (if (typep (follow-synonym-stream *standard-output*) 'dw:dynamic-window)
	      #'lisp:format
	      #'format)))
    #-Genera (declare (ignore format))
    (macrolet ((my-format (&rest stuff)
		 #{Genera    `(funcall format ,@stuff)
  		   otherwise `(format ,@stuff)
		  }
		 ))
      (with-resource-rd (resource RD)
	(my-format t "~&Resource ~S:" (rd-name RD))
	(when (rd-constructor RD)
	  (my-format t "~%Constructor: ~S" (rd-constructor RD)))
	(when (rd-initializer RD)
	  (my-format t "~%Initializer: ~S" (rd-initializer RD)))
	(when (rd-matcher RD)
	  (my-format t "~%Matcher: ~S" (rd-matcher RD)))
	(let ((objects (rd-objects RD)))
	  (when (> (fill-pointer objects) 0)
	    (my-format t "~%~D Object~:P:" (fill-pointer objects)))
	  (doseq (object-storage objects)
	    (my-format t "~%~S, ~:[Not in~;In~] use"
		       (os-object object-storage)
		       (os-use-cons object-storage))))))))

(defun clear-resource (resource)
  (with-resource-rd (resource RD)
    (setf (fill-pointer (rd-objects RD)) 0))
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
    (with-lockf ((rd-lock RD) "Resource lock")
      (unless (rd-objects RD)
	(setf (rd-objects RD) (make-array (* 2 (or initial-copies 10))
					  :fill-pointer 0))
	(when initial-copies
	  (dotimes (i initial-copies)
	    #+Genera-release-8 (declare (ignore i))
	    (vector-push (cons nil (funcall constructor RD)) (rd-objects RD)))))
      (setf (rd-constructor RD) constructor)
      (setf (rd-initializer RD) initializer)
      (setf (rd-deinitializer RD) deinitializer)
      (setf (rd-matcher RD) matcher)
      (setf (lookup-resource-descriptor name) RD))))

(defun allocate-resource (name &rest parameters &aux RD object-storage)
  (declare (non-dynamic-extent parameters))
  (block allocate
    (setf RD (lookup-resource-descriptor name))
    (unless RD (error "Can't allocate nonexistent resource ~S" name))
    (with-lockf ((rd-lock RD) "Resource lock")
      (let* ((array (rd-objects RD))
	     (matcher (rd-matcher RD))
	     (fill-pointer (fill-pointer array)))
	#+genera (declare (sys:array-register array))
	(dotimes (i fill-pointer)
	  (setf object-storage (aref array i))
	  (when (and (null (os-use-cons object-storage))
		     (or (null matcher)
			 (apply matcher (os-object object-storage) object-storage parameters)))
	    (setf (os-use-cons object-storage) (cons RD i))
	    (setf (os-parameters object-storage) parameters)
	    (return-from allocate)))
	(let* ((new-object (apply (rd-constructor RD) RD parameters))
	       (array-size (array-dimension array 0)))
	  (setf object-storage (make-object-storage
				 :object new-object
				 :use-cons (cons RD fill-pointer)
				 :parameters parameters))
	  (when (<= array-size fill-pointer)
	    (let ((new-array (make-array (* 2 array-size)
					 :fill-pointer fill-pointer)))
	      (replace new-array array)
	      (setf array (setf (rd-objects RD) new-array))))
	  (vector-push object-storage array)))))
  (when (rd-initializer RD) (apply (rd-initializer RD) (os-object object-storage) parameters))
  (values (os-object object-storage) object-storage))

(defun deallocate-resource (name object &optional allocation-key
			    &aux RD object-array object-index object-storage)
    (if allocation-key
	(setf RD (car (os-use-cons allocation-key)) object-array (rd-objects RD)
	      object-index (cdr (os-use-cons allocation-key)))
	(setf RD (or (lookup-resource-descriptor name)
		     (error "Can't deallocate nonexistent resource ~S" name))
	      object-array (rd-objects RD)
	      object-index (position object object-array
				     :key #'os-object)))
    (unless object-index
      (error "Can't deallocate object ~S: not present in resource ~S" object (rd-name RD)))
    (with-lockf ((rd-lock RD) "Resource lock")
      (setf object-storage (aref object-array object-index))
      (unless (os-use-cons object-storage)
	(error "Can't deallocate object ~S: already deallocated from resource ~S"
	       object (rd-name RD)))
      (unless (eql object (os-object object-storage))
	(error "Can't deallocate object ~S: not present in resource ~S" object (rd-name RD)))
      (when (rd-deinitializer RD)
	(funcall (rd-deinitializer RD) (os-object object-storage)))
      (setf (os-use-cons object-storage) nil)))

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
	(allocation-key (gensymbol "ALLOCATION-KEY")))
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

#|| ;;; Tests

(defresource test (&optional (n 10))
  :constructor (make-array n :fill-pointer 0)
  :matcher (>= (array-dimension test 0) n)
  :initializer (setf (fill-pointer test) 0)	;etc.
  )

(defresource test1 (x)
  :constructor (make-array 10)
  )

(defresource drawing-state (&optional old-drawing-state)
  :constructor (make-drawing-state)
  :initializer (when old-drawing-state (replace drawing-state old-drawing-state)))

(defmethod stream-drawing-state ((stream drawing-state-mixin))
  (slot-value stream 'drawing-state))

(defmethod (setf stream-drawing-state) (new-drawing-state (stream drawing-state-mixin))
  (setf (slot-value stream 'drawing-state) new-drawing-state))

(defmacro with-saved-drawing-state ((stream) &body body)
  `(letf-using-resource ((stream-drawing-state ,stream) drawing-state
			 (stream-drawing-state ,stream))
     ,@body))

(with-saved-drawing-state (*y-window*)
  (apply-translation translation))

||#
