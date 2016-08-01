;;; -*- Mode: Lisp; Package: CLIM-UTILS; Base: 10.; Syntax: Common-Lisp -*-
;; See the file LICENSE for the full license governing this code.
;;

;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved.
;;;

(in-package :clim-utils)

;;;
;;; Protocol Stuff (should be moved to a utils package)
;;;

(defclass protocol ()
    ((name :initarg :name :accessor protocol-name)
     (roles :initarg :roles :accessor protocol-roles)
     (operations :initform () :initarg :operations
		 :accessor protocol-operations)))

(defmethod print-object ((protocol protocol) stream)
  (print-unreadable-object (protocol stream)
    (format stream "Protocol: ~a" (protocol-name protocol))))

(defclass role ()
    ((name :initarg :name :accessor role-name)
     (slots :initarg :slots :accessor role-slots)))

(defmethod print-object ((role role) stream)
  (print-unreadable-object (role stream)
    (format stream "Role: ~a" (role-name role))))

(defclass operation ()
    ((name :initarg :name :accessor operation-name)
     (required-args :initarg :required-args :accessor operation-required-args)
     (specs :initarg :specs :accessor operation-specs)
     (extra-args :initarg :extra-args :accessor operation-extra-args)))

(defmethod print-object ((operation operation) stream)
  (print-unreadable-object (operation stream)
    (format stream "Operation: ~a" (operation-name operation))))

(defvar *protocols* nil)
(defmacro find-protocol (name) `(getf *protocols* ,name))
(defsetf find-protocol (name) (value) `(setf (getf *protocols* ,name) ,value))

(defvar *roles* nil)
(defmacro find-role (name) `(getf *roles* ,name))
(defsetf find-role (name) (value) `(setf (getf *roles* ,name) ,value))

(defmacro defprotocol (name supers &rest options)
  (declare (ignore supers options))
  `(eval-when (compile eval load)
     (setf (find-protocol ',name)
	     (make-instance 'protocol :name ',name))))

(defmacro defrole (class supers slots &rest options)
  (declare (ignore supers options))
  `(eval-when (compile eval load)
     #+PCL
     (pcl::do-standard-defsetf
       ,@(mapcan #'(lambda (slot)
		     (with-collection
		       (do* ((tail (cdr slot) (cddr tail))
			     (key  (car tail) (car tail))
			     (val  (cadr tail) (cadr tail)))
			    ((null tail))
			 (when (or (eq key :writer)
				   (eq key :accessor))
			   (collect val)))))
		 slots))
     (setf (find-role ',class)
	     (make-instance 'role :name ',class :slots ',slots))))

(defmacro defoperation (name protocol arg-specs &key (defgenericp t))
  (let* ((pos (position-if #'(lambda (x) (member x '(&key &optional &rest)))
			   arg-specs))
	 (required-arg-specs (if pos (subseq arg-specs 0 pos)
				 arg-specs))
	 (required-args (mapcar #'(lambda (arg-spec)
				    (if (listp arg-spec)
					(first arg-spec)
					arg-spec))
				required-arg-specs))
	 (specs (mapcar #'(lambda (arg-spec)
			    (if (listp arg-spec)
				(second arg-spec)
				t))
			required-arg-specs))
	 (extra-args (and pos (subseq arg-specs pos))))
    `(eval-when (compile eval load)
       ;; Define a group named (OPERATION PROTOCOL NAME), since we can
       ;; have multiple DEFOPERATIONs for the same operation in
       ;; different protocols (used to just be named NAME).
       (define-group (operation ,protocol ,name) defoperation
	 #-VDPCL				;PCL's defgeneric fails.
	 ,(when defgenericp
	    `(defgeneric ,name (,@required-args
				,@(mapcar #'(lambda (arg)
					      (cond ((atom arg) arg)
						    ((atom (first arg)) (first arg))
						    (t (first (first arg)))))
					  extra-args))))
	 (let* ((protocol (find-protocol ',protocol))
		(operation
		  (make-instance 'operation :name ',name
				 :required-args ',required-args
				 :specs ',specs
				 :extra-args ',extra-args)))
	   ;; Just simple now.
	   (push-unique operation (protocol-operations protocol)
			:key #'operation-name))))))

;;; Genera support for function spec (OPERATION PROTOCOL NAME):
#+Genera (progn
(sys:define-function-spec-handler operation (op spec &rest args)
  (case op
    (sys:validate-function-spec (= (length spec) 3))
    ((sys:fdefinedp sys:fdefinition) nil)
    (sys:fdefine (error "Can't define an operation this way."))
    (otherwise (apply #'si:function-spec-default-handler op spec args))))

(defun (:property defoperation zwei:definition-function-spec-parser) (start-bp)
  (let ((bp start-bp))
    (block top-level
      (flet ((read-next ()
	       (let ((next-bp (zwei:forward-sexp bp)))
		 (if next-bp
		     (prog1 (zwei:read-fspec-item-from-interval bp next-bp)
			    (setf bp next-bp))
		     (return-from top-level (values nil nil nil :more))))))
	(let* ((protocol (read-next))
	       (operation-name (read-next)))
	  (values `(operation ,protocol ,operation-name) 'defoperation
		  (zwei:string-interval start-bp bp)))))))

(scl:defprop defoperation "Generic Operation" si:definition-type-name)
) ;;; #+Genera

(defvar *outer-self* nil)
;; --- for old CLIM compatability.  Remove when we don't have to share sources anymore.
(defvar *original-stream* nil)

(defmacro define-trampoline-template
	  (protocol-name role-name role-player (player-var body-var) &rest body)
  (let ((protocol (find-protocol protocol-name))
	(macro-name (gensymbol (string-capitalize (string protocol-name))
			       "Trampoline-Generator-")))
    (unless protocol
      (warn "~S: can't find protocol named ~S" 'define-trampoline-template protocol-name))
    `(progn
       (defmacro ,macro-name (,player-var &body ,body-var) ,@body)
       ;; Don't blow up when protocol is undefined.
       ,@(when protocol
	   (mapcar
	     #'(lambda (operation)
		 (with-slots (name required-args specs extra-args) operation
		   (let* ((subst-extra-args
			    (subst role-player role-name extra-args))
			  (role-pos (position role-name specs))
			  (arg-specs (copy-list required-args))
			  (call-specs (copy-list required-args))
			  (rest-p (member '&rest subst-extra-args))
			  (extras (make-pass-on-arglist subst-extra-args)))
		     (setf (nth role-pos arg-specs) `(,player-var ,role-player))
		     (setf (nth role-pos call-specs) player-var)
		     `(defmethod ,name (,@arg-specs ,@subst-extra-args)
			,@(when rest-p
			    `((declare (dynamic-extent ,(second rest-p)))))
			(let* ((*outer-self* (or *outer-self* ,player-var))
			       (*original-stream* *outer-self*))
			  (,macro-name ,player-var
			   ,(if rest-p
				`(apply #',name ,@call-specs ,@extras)
				`(,name ,@call-specs ,@extras))))))))
	     (protocol-operations protocol))))))

(defmacro define-slot-trampoline-template
	  (protocol-name role-name role-player (player-var body-var) &rest body)
  (let ((role (find-role role-name))
	(macro-name (gensymbol (string-capitalize (string protocol-name))
			       "Trampoline-Generator-")))
    (unless role (warn "~S: can't find role ~S" 'define-slot-trampoline-template role-name))
    `(progn
       (defmacro ,macro-name (,player-var &body ,body-var) ,@body)
       ;; Don't blow up when role is undefined.
       ,@(when role
	   (mapcan
	     #'(lambda (slot)
		 (let ((writer (or (getf (cdr slot) :writer)
				   (getf (cdr slot) :accessor)))
		       (reader (or (getf (cdr slot) :reader)
				   (getf (cdr slot) :accessor)))
		       (nvalues (or (getf (cdr slot) :nvalues) 1)))
		   `(,@(when reader
			 `((defmethod ,reader ((,role-player ,role-player))
			     (let* ((*outer-self* (or *outer-self* ,role-player))
				    (*original-stream* *outer-self*))
			       (,macro-name ,player-var
				(,reader ,player-var))))))
		     ,@(when writer
			 (let ((values-vars
				 (with-collection
				   (dotimes (i nvalues)
				     (collect
				       (make-symbol
					 (format nil "~A-~D" 'new-value i)))))))
			   `((,(if (= nvalues 1) 'defmethod 'defmethod*)
			      (setf ,writer) (,@values-vars (,role-player ,role-player))
			      (let* ((*outer-self* (or *outer-self* ,role-player))
				     (*original-stream* *outer-self*))
				(,macro-name ,player-var
				 (setf (,writer ,player-var) (values ,@values-vars)))))))))))
	     (role-slots (find-role role-name)))))))

(defmacro generate-trampolines (protocol-name role-name role-player delegate-form)
  `(progn
     (define-trampoline-template ,protocol-name ,role-name ,role-player
				 (,role-player body)
				 `(let ((,',role-player ,,delegate-form))
				    ,@body))
     (define-slot-trampoline-template ,protocol-name ,role-name ,role-player
				      (,role-player body)
				      `(let ((,',role-player ,,delegate-form))
					 ,@body))))
