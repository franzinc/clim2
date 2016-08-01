;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

;;;
;;; Copyright (c) 1990 by Xerox Corporation.  All rights reserved.
;;;

(in-package :clim-utils)

;;;
;;; Automatic Constructor Lookup
;;;

(defvar *constructor-table* (make-hash-table :test #'eq))

(defmethod (setf autoconstructor) (constructor class-name)
  (when (typep class-name 'standard-class)
    (setf class-name (class-name class-name)))
  (setf (gethash class-name *constructor-table*) constructor))

#-Allegro-v4.0-constructors
(progn
(defvar %constructors-p #-PCL nil #+PCL t)

(defun autoconstructor (class)
  (flet ((default-constructor (&rest args)
	   (declare (dynamic-extent args))
	   (apply #'make-instance class args)))
    (or (and %constructors-p (gethash class *constructor-table*))
	#'default-constructor)))
)	;#-Allegro-v4.0-constructors

#+Allegro-v4.0-constructors
(progn
(defvar %constructors-p t)
(defun autoconstructor (class)
  (or
    (and %constructors-p (excl::class-autoconstructor
			   (if (symbolp class) (find-class class) class)))
    #'(lambda (&rest args)
	(declare (dynamic-extent args))
	(apply #'make-instance class args))))
)	;#+Allegro-v4.0-constructors

(defun disable-autoconstructors ()
  (setf %constructors-p nil))

(defun enable-autoconstructors ()
  (setf %constructors-p t))

(defmacro fast-make-instance (type &rest args)
  `(funcall (autoconstructor ,type) ,@args))

#+PCL
(defmacro defautoconstructor (name class &optional extra-args)
  (expand-defautoconstructor class name (copy-list (eval extra-args))))

#+Allegro-v4.0-constructors
(defmacro defautoconstructor (name class &optional extra-args)
  (when  extra-args "Extra args to autocontructor ~S,~S,~S" name class extra-args)
  `(excl::defautoconstructor ,name ,class))

#-(or PCL Allegro-v4.0-constructors)
(defmacro defautoconstructor (name class &optional extra-args)
  (declare (ignore name class extra-args))
  nil)

;;;
;;; Expand Defautoconstructor
;;;

#+PCL
(import '(pcl::class-slots
	  pcl::class-default-initargs
	  pcl::method-lambda-list
	  pcl::slotd-name
	  pcl::slotd-allocation
	  pcl::slotd-initform
	  pcl::slotd-initargs))

#+PCL
(progn

(defun expand-defautoconstructor (class-name name extra-args)
  (let ((class (find-class class-name nil)))
    (when (null class)
      (error "defconstructor form being compiled (or evaluated) before~@
              class ~S is defined."
	     class-name))
    `(define-group ,name defautoconstructor
       ;; In order to avoid undefined function warnings, we want to tell
       ;; the compile time environment that a function with this name and
       ;; this argument list has been defined.  The portable way to do this
       ;; is with defun.
       (eval-when (compile load eval)
	 (proclaim '(notinline ,name))
	 (proclaim '(function ,name)))
       #+++ignore ;; See if FUNCTION proclamation is sufficient.
       (defun ,name (&rest args)
	 (declare (dynamic-extent args)
		  (ignore args))
	 (error "Constructor ~S not loaded." ',name))

       ;; Register auto constructor name
       (setf (autoconstructor ',class-name) ',name)

       ,(pcl::make-top-level-form
	  `(defconstructor ,name) '(eval load)
	  `(pcl::load-constructor
	     ',class-name
	     ',(class-name (class-of class))
	     ',name

	     ;; No initargs in the constructor
	     ;; Established later
	     nil;;',supplied-initarg-names

	     ;; make-constructor-code-generators is called to return a list
	     ;; of constructor code generators.  The actual interpretation
	     ;; of this list is left to compute-constructor-code, but the
	     ;; general idea is that it should be an plist where the keys
	     ;; name a kind of constructor code and the values are generator
	     ;; functions which return the actual constructor code.  The
	     ;; constructor code is usually a closures over the arguments
	     ;; to the generator.
	     ,(make-automatic-constructor-code-generators
		class name extra-args))))))

(defmethod make-automatic-constructor-code-generators
	   ((class pcl::standard-class) name extra-args)
  `(list 'automatic
	 ,(automatic-constructor-code-generator-generator
	   class name extra-args)))

;;;
;;; The automatic constructor code generator generator (ccgg) invokes the
;;; general ccgg to generate a ccg of type 'automatic.
;;;
;;; Note:  Since this ccg is dependent on the state of the class at the time of
;;; its generation, the ccg must check to see if this state (i.e. the automatic
;;; arglist and supplied-initargs) are still good.  If not then the ccg resorts
;;; to reinvoking the ccgg (yes causing runtime compile) to regenerate itself.
;;;

(defvar *clim-development-flag* nil)

(defun automatic-constructor-code-generator-generator (class name extra-args)
  (unless (pcl::class-finalized-p class)
    (pcl::finalize-inheritance class))
  (multiple-value-bind (arglist supplied-initarg-names supplied-initargs)
      (automatic-generator-1 class extra-args)
    `(function
      (lambda (class .wrapper. defaults init shared)
       (let ((.extra-args. ',extra-args))
	 (funcall
	  (if ;;; the automatic arglist and hence supplied initargs have
	      ;;  not changed since the call to this generator generator,
	      (or (null *clim-development-flag*)
		  (equal ',arglist
			 (automatic-generator-1 class .extra-args. t)))
	      ;;  THEN just continue to use the general generator
	      ,(funcall (cadr
			 (assoc 'pcl::general
				pcl::*constructor-code-types*))
			class name arglist
			supplied-initarg-names supplied-initargs)
	      ;; ELSE regenerate and compile a generator, install it
	      (compile-and-install-generator
	       ',name
	       'automatic
	       (automatic-constructor-code-generator-generator
		class ',name .extra-args.)))
	  class .wrapper. defaults init shared))))))

(defun compile-and-install-generator (name type generator)
  (let ((generator (compile nil (eval generator))))
    (setf (getf (slot-value (symbol-function name) 'pcl::code-generators)
		type)
	  generator)
    generator))

(defun automatic-generator-1 (class extra-args &optional just-arg-list)
  (let* ((arg-specs (nconc (copy-list extra-args)
			   (find-constructor-args class)))
	 (supplied-initargs
	  (unless just-arg-list
	    (with-collection
		(dolist (spec arg-specs)
		  (let ((arg (if (listp spec) (car spec) spec)))
		    (collect (intern (string arg) :keyword))
		    (collect arg))))))
	 (supplied-initarg-names
	  (unless just-arg-list
	    (with-collection
	      ;; Can't use PCL's iterate tool
	      (dorest (sublist supplied-initargs cddr)
		(collect (first sublist)))
	      #+++ignore
	      (iterate ((name (pcl::*list-elements supplied-initargs
						   :by #'cddr)))
		       (collect name))))))
    (values `(&key ,@arg-specs)
	    supplied-initarg-names
	    supplied-initargs)))


;;;
;;; Groping for the data needed to generate automatic constructors.
;;; (i.e grovelling before the pcl god to be given knowledge.)
;;;


(defvar *debug-constructor-hax* nil)

(defun arg-spec-key (val) (if (listp val) (car val) val))

(defun grope-for-pcl-data (class)
  (flet ((get-keys (args)
	   (let ((keystart (position '&key args))
		 (keyend   (position '&allow-other-keys args)))
	     (if keystart
		 (if keyend
		     (subseq (the list args) (1+ keystart) keyend)
		     (subseq (the list args) (1+ keystart)))
		 ()))))
    (let* ((package (symbol-package (class-name class)))
	   (slots      (class-slots class))
	   (defaults   (class-default-initargs class)))

      (values
       (let (all-keys)
	 (dolist (m (compute-applicable-methods
		     #'initialize-instance
		     (list (class-prototype class))))

	   (let ((keys (get-keys (method-lambda-list m))))
	     (when keys
	       (setq all-keys (nconc all-keys keys)))))
	 ;; Not worrying about items that have different initforms
	 (remove-duplicates all-keys :from-end t))
       (delete-duplicates
	(with-collection
	    (dolist (default defaults)
	      (collect
	       (list (intern (string (car default)) package)
		     (caddr default)))))
	:from-end t
	:key #'arg-spec-key)

       (delete-duplicates
	(with-collection
	    (dolist (slotd slots)
	      (let* (;;(alloc (slotd-allocation slotd))
		     (name (slotd-name slotd))
		     (form (slotd-initform slotd))
		     (initargs (slotd-initargs slotd)))
		(when initargs
		  (collect
		   (list name
			 form
			 (remove-duplicates initargs :from-end t)))))))
	:from-end t
	:key #'car)
       ))))

(defun find-constructor-args (class &aux package)
  (unless (classp class)
    (setq class (find-class class)))
  (setq package (symbol-package (class-name class)))

  (multiple-value-bind (arg-specs defaults slots)
      (grope-for-pcl-data class)

    (when *debug-constructor-hax*
      (format t "~%Having Groped, discovered ... ~%")
      (pprint (list arg-specs defaults slots))
      (terpri))

    (setq
     arg-specs
     (nconc arg-specs
	    (with-collection
		(dolist (default defaults)
		  (let* ((tail (member (arg-spec-key default) arg-specs
				       :key #'arg-spec-key))
			 (arg-spec (car tail)))
		    (if arg-spec
			(if (listp arg-spec)
			    (setf (cadr arg-spec) (cadr default))
			    (setf (car tail) (list (car tail) (cadr default))))
			;; Not confirming that its a legal default-initarg
			(collect default)))))))

    (dolist (iarg-spec
	      (with-collection
		  (dolist (slot slots)
		    (let ((iargs (caddr slot))
			  (iform (cadr slot)))
		      (when (eq iform pcl::*slotd-unsupplied*)
			(setq iform 'pcl::*slot-unbound*))
		      (if (= (length iargs) 1)
			  (collect
			   (list (intern (string (car iargs)) package) iform))

			  ;;
			  (progn
			    (when *debug-constructor-hax*
			      (warn "Multiple initargs for ~s -> ~s"
				    class
				    (car slot)))
			    (dolist (iarg iargs)
			      (collect
			       (list (intern (string iarg) package)
				     iform)))))))))
      (unless (member (arg-spec-key iarg-spec) arg-specs
		      :key #'arg-spec-key)
	(setq arg-specs (nconc arg-specs (list iarg-spec)))))

    (when *debug-constructor-hax*
      (format t "~%Consolidated into: ~%")
      (pprint arg-specs)
      (terpri))

    arg-specs))


) ; end of PCL zone

