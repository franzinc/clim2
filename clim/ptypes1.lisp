;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: ptypes1.lisp,v 1.4 92/02/24 13:08:21 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; This file contains the substrate for presentation types


;;;; Utilities

;;; The only portable way to figure out what a backquote form does is to
;;; evaluate it with "dummy" arguments, which of course places some
;;; restrictions on what backquote forms are valid to use.  They can't
;;; contain conditionals nor depend on the types of their arguments.
;;; The arguments are a form and some lambda-lists that define its inputs.
;;; The values are the result of evaluating the form with dummy bindings for
;;; the variables in the lambda-lists, and two alists from variables to dummy values,
;;; corresponding to the two lambda-lists.
;;; The second and third values only contain entries for variables whose dummy value
;;; actually was incorporated into the result.
;;; We use uninterned symbols as the dummy values.
(defun meta-evaluate-form (form lambda-list &optional another-lambda-list)
  (declare (values value bindings more-bindings))
  (let ((bindings nil) (more-bindings nil))
    (labels ((do-lambda-list (lambda-list)
	       (dolist (item lambda-list)
		 (cond ((member item lambda-list-keywords))
		       ((atom item) (do-variable item))
		       (t (do-variable (if (atom (car item)) (car item) (cadar item)))
			  (when (and (consp (cdr item)) (consp (cddr item)) (third item))
			    (do-variable (third item)))))))
	     (do-variable (variable)
	       (push (cons variable (make-symbol (symbol-name variable))) bindings)))
      (do-lambda-list another-lambda-list)
      (setq more-bindings bindings
	    bindings nil)
      (do-lambda-list lambda-list))
    (let* ((x `(let ,(mapcar #'(lambda (binding) `(,(car binding) ',(cdr binding)))
			     (append bindings more-bindings))
		 ,form))
	   (result (handler-case (eval x)
		     (error (error)
		       (warn "Error while meta-evaluating the form ~S:~%~A" form error)
		       nil))))
      (labels ((prune (bindings)
		 (delete-if-not #'(lambda (binding) (tree-member (cdr binding) result))
				bindings)))
	(values result (prune bindings) (prune more-bindings))))))

;;; True if value appears anywhere in tree, which is not really a tree because
;;; we only look in cars, not in cdrs, and we look inside vectors too.
;;; But I couldn't think of a better name.
(defun tree-member (value tree)
  (or (eq value tree)
      (and (typep tree 'sequence)
	   (some #'(lambda (elt) (tree-member value elt)) tree))))

;;; Anonymous class support
;;; If the class doesn't have a proper name, use the class object
(defun class-proper-name (class &optional environment)
  #+Genera
  (unless environment
    ;; Use the speedup that is already there
    (return-from class-proper-name
      (clos-internals:class-name-for-type-of class)))
  (let ((name (class-name class)))
    (if (and name (symbolp name) 
	     (eq (find-class-that-works name nil environment) class))
	name
	#-Minima class
	;; Just let the name stand in for the clsas when cross-compiling
	#+Minima (if (and name (symbolp name)) name class))))


;;;; Conditions for Parsing Exceptions

#+(or (not ANSI-90) (not (or Genera (and Allegro (version>= 4 1)))))
(define-condition parse-error (error) ())

(define-condition simple-parse-error (parse-error)
  ((format-string :reader parse-error-format-string :initarg :format-string)
   (format-arguments :reader parse-error-format-arguments :initarg :format-arguments))
  (:report (lambda (condition stream)
	     (apply #'format stream (parse-error-format-string condition)
				    (parse-error-format-arguments condition)))))

(define-condition input-not-of-required-type (parse-error)
  ((string :reader input-not-of-required-type-string :initarg :string)
   (type :reader input-not-of-required-type-type :initarg :type))
  (:report (lambda (condition stream)
	     (let ((string (input-not-of-required-type-string condition)))
	       (format stream "The input read, ~A, was not "
		       (if (equal string "") '|""| string))
	       (describe-presentation-type (input-not-of-required-type-type condition)
					   stream 1)
	       (write-char #\. stream)))))

(defun simple-parse-error (format-string &rest format-arguments)
  (declare (dynamic-extent format-arguments))
  (signal 'simple-parse-error
	  :format-string format-string
	  :format-arguments (copy-list format-arguments))
  (error "SIMPLE-PARSE-ERROR signalled outside of ACCEPT."))

(defun input-not-of-required-type (object type)
  (signal 'input-not-of-required-type :string object :type type)
  (error "INPUT-NOT-OF-REQUIRED-TYPE signalled outside of ACCEPT."))


;;;; Operations on presentation type specifiers

;;; A presentation type specifier matches one of these three patterns:
;;;   name
;;;   (name parameters...)
;;;   ((name parameters...) options...)
;;; The name is a symbol or a class object (but not a built-in class)

(deftype presentation-type-specifier ()
  `(satisfies presentation-type-specifier-p))

;;; Decode a presentation type specifier into its name, parameters, and options
;;; Any of the three variables can be NIL meaning not to bother with that component
(defmacro with-presentation-type-decoded ((name-var &optional parameters-var options-var)
					  type &body body)
  (once-only (type)
    `(multiple-value-bind (,@(and name-var `(,name-var))
			   ,@(and parameters-var `(,parameters-var))
			   ,@(and options-var `(,options-var)))
	 (cond ((atom ,type)
		(values ,@(and name-var `(,type))
			,@(and parameters-var `(nil))
			,@(and options-var `(nil))))
	       ((atom (car ,type))
		(values ,@(and name-var `((car ,type)))
			,@(and parameters-var `((cdr ,type)))
			,@(and options-var `(nil))))
	       (t
		(values ,@(and name-var `((caar ,type)))
			,@(and parameters-var `((cdar ,type)))
			,@(and options-var `((cdr ,type))))))
       ,@body)))

(defun presentation-type-name (type)
  (with-presentation-type-decoded (name) type
    name))

;;; Bind the parameter variables of this presentation type to the
;;; parameter values in the given presentation type specifier
(defmacro with-presentation-type-parameters ((type-name type)
					     &body body &environment env)
  (let ((lambda-list (asterisk-default (presentation-type-parameters type-name env)))
	(parameters '#:parameters))
    `(with-presentation-type-decoded (nil ,parameters) ,type
       (bind-to-list ,lambda-list ,parameters
	 ,@body))))

;;; Bind the option variables of this presentation type to the
;;; option values in the given presentation type specifier
(defmacro with-presentation-type-options ((type-name type)
					  &body body &environment env)
  (let ((lambda-list (lambda-list-from-options (presentation-type-options type-name env)))
	(options '#:options))
    `(with-presentation-type-decoded (nil nil ,options) ,type
       (bind-to-list ,lambda-list ,options
	 ,@body))))

;;; Insert DEFTYPE-style defaulting of unsupplied arguments to *
(defun asterisk-default (lambda-list)
  (do* ((lambda-list lambda-list (cdr lambda-list))
	(item (first lambda-list) (first lambda-list))
	(result nil)
	(mode nil))
       ((null lambda-list) (nreverse result))
    (cond ((member item '(&optional &rest &key &aux))
	   (setq mode item))
	  ((member item lambda-list-keywords))
	  ((not (member mode '(&optional &key))))
	  ((atom item)
	   (setq item `(,item '*)))
	  ((null (cdr item))
	   (setq item `(,(car item) '*))))
    (push item result)))

;;; The form that merely reconstructs this lambda-list's argument list
(defun lambda-list-pass-through (lambda-list)
  (do* ((lambda-list lambda-list (cdr lambda-list))
	(item (first lambda-list) (first lambda-list))
	(result nil)
	(mode nil))
       ((or (null lambda-list) (eq item '&aux))
	(and result
	     (cons 'list (nreverse result))))
    (cond ((member item '(&optional &rest &key))
	   (setq mode item))
	  ((member item lambda-list-keywords))
	  ((eq mode '&rest)
	   (return (if result `(list* ,@(nreverse result) ,item) item)))
	  (t
	   (let ((keyword (and (eq mode '&key) (parameter-specifier-keyword item))))
	     ;; Parameter specifiers that were only put in to ignore standard
	     ;; presentation options don't count
	     (unless (and keyword (consp item) (consp (car item))
			  (eq (caar item) keyword)
			  (eq (cadar item) (get keyword 'standard-presentation-option)))
	       (when keyword
		 (push (if (keywordp keyword) keyword `',keyword) result))
	       (push (cond ((atom item) item)
			   ((atom (car item)) (car item))
			   (t (cadar item)))
		     result)))))))

;;; Keyed by the presentation type class, yield description or history if it
;;; was specified in the define-presentation-type rather than by a method

(defvar *presentation-type-description-table* (make-hash-table))
(defvar *presentation-type-history-table* (make-hash-table))

;;; A list of names of presentation types whose parameters are themselves
;;; presentation types, for example, OR or SEQUENCE.
(defvar *presentation-type-parameters-are-types* nil)


;;;; Standard Presentation Options

;;; This list is not in the same form as the :options option,
;;; but is just a list of keywords.  Standard presentation options default
;;; to NIL, do not create variable bindings, and are not viewspec choices.
(defparameter *standard-presentation-options* '(:description))

;;; Remove the extra stuff that isn't part of a lambda-list, 
;;; and add &KEY and &ALLOW-OTHER-KEYS to make it a lambda-list.
;;; Do not add the standard presentation options as there is no need
;;; to bind variables to them, and no need to check the option keywords
;;; at all here, since PRESENTATION-TYPE-SPECIFIER-P does that.
(defun lambda-list-from-options (options)
  `(&key ,@(mapcar #'(lambda (item)
		       (if (and (consp item) (consp (cdr item)) (consp (cddr item)))
			   (subseq item 0 (if (null (third item)) 2 3))
			   item))
		   options)
#||	 ,@(mapcan #'(lambda (keyword)
		       (unless (find keyword options :key #'parameter-specifier-keyword)
			 ;; Bind an uninterned dummy variable, all just to get keyword
			 ;; argument validity checking not to complain about the
			 ;; presence of this keyword.
			 (let ((variable (or (get keyword 'standard-presentation-option)
					     (setf (get keyword 'standard-presentation-option)
						   (make-symbol (symbol-name keyword))))))
			   `(((,keyword ,variable))))))
		   *standard-presentation-options*)
||#
	 &allow-other-keys))


;;;; Presentation Type Classes

;;; The metaclass for classes created by DEFINE-PRESENTATION-TYPE.
;;; They have their own metaclass just for CLASS-PRESENTATION-TYPE-NAME,
;;; ACCEPTABLE-PRESENTATION-TYPE-CLASS, and PRESENTATION-TYPE-CLASS-P.
(defclass presentation-type-class (standard-class #+Minima-Developer standard-object) ())

;;; JonL says that the latest CLOS spec says that you aren't allowed to mix
;;; objects of different metaclasses unless there is a VALIDATE-SUPERTYPE
;;; method that says that you can.
#+Lucid
(defmethod clos-system::validate-superclass
	   ((class presentation-type-class) (meta standard-class))
  t)

#+(and ANSI-90 Symbolics)
(defmethod clos-internals::validate-superclass
	   ((class presentation-type-class) (meta standard-class))
  t)

;;; These classes get into compiled files as method parameter specializers
(defmethod make-load-form ((object presentation-type-class))
  ;; The following would be "the right thing," but it doesn't work because some compilers
  ;; evaluate the MAKE-LOAD-FORM forms out of order with the top-level forms, so the
  ;; presentation type class might not be defined yet when this is evaluated.
  #+ignore `(find-presentation-type-class ',(class-presentation-type-name object))
  ;; So do this instead
  `(load-reference-to-presentation-type-class
     ',(class-presentation-type-name object #+(or Genera Minima) 'compile-file)
     ',(presentation-type-parameters object)
     ',(presentation-type-options object)
     ',(let ((superclasses (class-direct-superclasses object)))
	 (if (cdr superclasses)
	     (mapcar #'(lambda (class)
			 (class-presentation-type-name class
						       #+(or Genera Minima) 'compile-file))
		     (class-direct-superclasses object))
	     ;; Use an atom instead of a one-element list to work around a Lucid bug
	     ;; where it blows up if anything here is a list that gets consed freshly
	     ;; each time make-load-form is called, because make-load-form gets called
	     ;; twice and the results of evaluating the subforms have to be EQ.  Jeez!
	     (class-presentation-type-name (first superclasses)
					   #+(or Genera Minima) 'compile-file)))
     nil			;description doesn't matter
     ',(let* ((type-name (class-presentation-type-name object
						       #+(or Genera Minima) 'compile-file))
	      (history (gethash type-name *presentation-type-history-table*)))
 	 ;; The (AND HISTORY ...) is here to get around a compile/load
 	 ;; bootstrapping problem in HISTORIES.LISP
 	 (cond ((and history (typep history 'basic-history))
		(or (eql (slot-value history 'name) type-name)
		    (slot-value history 'name)))
	       ((symbolp history) history)
	       (t nil)))
     ',(not (null (member (class-name object) *presentation-type-parameters-are-types*)))
     nil			;parameter-massagers don't matter
     nil))			;options-massagers don't matter

;;; This is bound during the macro expansion of define-presentation-type to a list
;;; (name class parameters options direct-supertypes parameter-massagers options-massagers)
;;; so that information that has not yet been put into the tables can be retrieved
#-Genera
(defvar *presentation-type-being-defined* nil)
#+Genera
(sys:defvar-resettable *presentation-type-being-defined* nil)

;;; This hash table is keyed by the presentation type name and yields the class.
(defvar *presentation-type-class-table* (make-hash-table))

#+CCL-2
(defvar *presentation-class-type-table* (make-hash-table))

;;; Find the class corresponding to the presentation type named name
(defun find-presentation-type-class (name &optional (errorp t) environment)
  #+Allegro (setq environment (compile-file-environment-p environment))
  (typecase name
    (symbol
      (or (and (eq name (first *presentation-type-being-defined*))
	       (second *presentation-type-being-defined*))
	  (if (compile-file-environment-p environment)
	      (compile-time-property name 'presentation-type-class)
	      (gethash name *presentation-type-class-table*))
	  (let ((class (find-class name nil environment)))
	    (and (acceptable-presentation-type-class class)
		 class))
	  (when (compile-file-environment-p environment)
	    ;; compile-file environment inherits from the run-time environment
	    (or (gethash name *presentation-type-class-table*)
		(let ((class (find-class name nil nil)))
		  (and (acceptable-presentation-type-class class)
		       class))))
	  (and errorp (error "~S is not the name of a presentation type" name))))
    ((satisfies acceptable-presentation-type-class)
     name)
    (otherwise		;a type error should complain even if errorp is nil
     (error "~S is not the name of a presentation type" name))))

;;; Return the presentation type name corresponding to a class
;;; This is essentially the inverse of find-presentation-type-class
(defmethod class-presentation-type-name ((class presentation-type-class) &optional environment)
  (declare (ignore environment))
  (second (class-name class)))
  
(defmethod class-presentation-type-name ((class class) &optional environment)
  (class-proper-name class environment))

;;; Hide the long name when printing these
(defmethod print-object ((object presentation-type-class) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (class-presentation-type-name object) :stream stream :escape t)))

;;; Distinguish presentation type classes, which don't have "real" instances,
;;; from regular classes.  See presentation-typep.
(defmethod presentation-type-class-p ((class presentation-type-class)) t)
(defmethod presentation-type-class-p ((class class)) nil)

;;; True if this class can be used as a presentation type class
;;; which mainly means that it has a valid prototype that is a member
;;; of this class and not a member of any subclass of this class.
;;; Built-in classes aren't accepted because, for example, the prototype
;;; of RATIONAL must be a member of either INTEGER or RATIO.
;;; However, T has to be accepted even though it's technically a built-in class
;;; or method inheritance wouldn't work right -- we need the real class T
;;; that is a real superclass of every other class.
(defmethod acceptable-presentation-type-class ((class presentation-type-class)) t)
(defmethod acceptable-presentation-type-class ((class class)) t)
(defmethod acceptable-presentation-type-class ((class built-in-class)) nil)
(defmethod acceptable-presentation-type-class ((class (eql (find-class 't)))) t)
(defmethod acceptable-presentation-type-class ((class t)) nil)

;;; Abstract flavors aren't accepted since CLASS-PROTOTYPE signals an error
#+Genera
(defmethod acceptable-presentation-type-class ((class clos-internals::flavor-class))
  (not (flavor:flavor-is-abstract (clos-internals::class-instance-information class))))

;;; COMPILE-TIME-CLASSes, while the right place to hang all this information,
;;; unfortunately do NOT have CLASS-PRECEDENCE-LISTS, or any information of
;;; value other than name.
#+CCL-2
(defmethod acceptable-presentation-type-class ((class ccl::compile-time-class))
  nil)


;;;; Retrieving Information about a Presentation Type or P.T. Abbreviation

;;; This hash table is keyed by the class and yields the supertype names
;;; and the parameters and options massagers.
;;; The massagers are inlined in combined methods and map-over methods.
;;; One-element lists get condensed to save space.
(defvar *presentation-type-inheritance-table* (make-hash-table))

;;; Given a presentation type class get its internal inheritance information
(defun presentation-type-inheritance (class &optional environment)
  (declare (values direct-supertypes		;These three values are lists of
		   parameter-massagers		;equal length and parallel contents.
		   options-massagers))
  (let ((list (cond ((eq class (second *presentation-type-being-defined*))
		     (cddddr *presentation-type-being-defined*))
		    ((and (compile-file-environment-p environment)
			  (compile-time-property class 'presentation-type-inheritance)))
		    (t
		     (gethash class *presentation-type-inheritance-table*)))))
    (if (listp (car list))
	(values (first list) (second list) (third list))
	;; Expand condensed lists
	(values (list (first list)) (list (second list)) (list (third list))))))

;;; These hash tables are keyed by the presentation type class if it's a type
;;; or the name if it's an abbreviation, and yield up the indicated information.
;;; We don't bother putting an entry in the table if the information is nil.
;;; We use hash tables instead of property lists to improve virtual memory locality.
;;; This probably incurs no significant space cost.

(defvar *presentation-type-parameters-table* (make-hash-table))
(defvar *presentation-type-options-table* (make-hash-table))

;;; Find the parameters lambda-list of the presentation type or abbreviation named name
(defun presentation-type-parameters (name &optional environment)
  (let ((class (if (symbolp name) (find-presentation-type-class name nil environment) name)))
    (cond ((and *presentation-type-being-defined*
		(or (eq name (first *presentation-type-being-defined*))
		    (eq class (second *presentation-type-being-defined*))))
	   (third *presentation-type-being-defined*))
	  ((and class
		(compile-file-environment-p environment)
		(compile-time-property class 'presentation-type-inheritance))
	   (compile-time-property class 'presentation-type-parameters))
	  (class (gethash class *presentation-type-parameters-table*))
	  (t (gethash name *presentation-type-parameters-table*)))))

;;; Find the options list of the presentation type or abbreviation named name
;;; This does not include the standard options unless the define-presentation-type
;;; mentioned them explicitly
(defun presentation-type-options (name &optional environment)
  (let ((class (if (symbolp name) (find-presentation-type-class name nil environment) name)))
    (cond ((and *presentation-type-being-defined*
		(or (eq name (first *presentation-type-being-defined*))
		    (eq class (second *presentation-type-being-defined*))))
	   (fourth *presentation-type-being-defined*))
	  ((and class
		(compile-file-environment-p environment)
		(compile-time-property class 'presentation-type-inheritance))
	   (compile-time-property class 'presentation-type-options))
	  (class (gethash class *presentation-type-options-table*))
	  (t (gethash name *presentation-type-options-table*)))))


;;;; Presentation Type Abbreviations

;;; Keyed by name, yields the function that translates one type specifier into another
(defvar *presentation-type-abbreviation-table* (make-hash-table))

;;; Define a "macro" that expands one presentation type specifier into another
(defmacro define-presentation-type-abbreviation (name parameters expansion &key options
						 &environment env)
  (check-type name symbol)
  (check-type parameters list)
  (check-type options list)
  (let* ((parameters-var (and parameters '#:parameters))
	 (options-var '#:options)
	 (type-var '#:type)
	 (parameters-ll (asterisk-default parameters))
	 (options-ll (lambda-list-from-options options))
	 (function
	   `#'(lambda (,type-var)
		#+Genera (declare (sys:function-name (presentation-type-abbreviation ,name))
				  (sys:function-parent ,name define-presentation-type))
		;; Wrap the expansion in code to parse the incoming presentation type specifier
		(with-presentation-type-decoded (nil ,parameters-var ,options-var) ,type-var
		  (bind-to-list ,options-ll ,options-var
		    ,(if parameters
			 `(bind-to-list ,parameters-ll ,parameters-var ,expansion)
			 expansion))))))
    ;; If it's both an abbreviation and a class, warn
    (when (and (compile-file-environment-p env)	;Avoid duplicate warning
	       (find-class name nil #-Allegro env))
      (with-warnings-for-definition name define-presentation-type
	(warn "It is not valid to define a presentation type abbreviation with~@
	       the same name as a CLOS class.")))
    ;; Generate the expander function and pass it to load-presentation-type-abbreviation
    `(progn
       (eval-when (compile)
	 #+(or Genera Cloe-Runtime Minima CCL-2)
	   (setf (compile-time-property ',name 'presentation-type-abbreviation)
		 ,function)
	 #+(or Lucid Allegro)
	   ;; In these Lisps, compile-file-environment-p is always false, so
	   ;; expand-presentation-type-abbreviation-1 is not going to look for
	   ;; a compile-time-property.
	   (load-presentation-type-abbreviation ',name ',parameters ',options ,function))
       (load-presentation-type-abbreviation ',name ',parameters ',options ,function))))

;;; Load-time support for define-presentation-type-abbreviation
(defun load-presentation-type-abbreviation (name parameters options function)
  (when #+Genera (sys:record-source-file-name name 'define-presentation-type)
	#-Genera t
    #-Genera (setf (gethash name *presentation-type-abbreviation-table*) function)
    #+Genera (sys:fdefine `(presentation-type-abbreviation ,name) function t t)
    (if parameters
	(setf (gethash name *presentation-type-parameters-table*) parameters)
	(remhash name *presentation-type-parameters-table*))
    (if options 
	(setf (gethash name *presentation-type-options-table*) options)
	(remhash name *presentation-type-options-table*))
    ;; If it used to be a presentation type, undefine it
    (let ((class (gethash name *presentation-type-class-table*)))
      (when class
	(remhash name *presentation-type-class-table*)
	(remhash class *presentation-type-inheritance-table*)))
    ;; If it's both an abbreviation and a class, warn
    (when (find-class name nil)
      (with-warnings-for-definition name define-presentation-type
	(warn "It is not valid to define a presentation type abbreviation with~@
	       the same name as a CLOS class.")))
    name))

;;; Like macroexpand-1
(defun expand-presentation-type-abbreviation-1 (type &optional environment)
  (declare (values expansion expanded))
  (with-presentation-type-decoded (name parameters options) type
    (let ((expander (or (and (compile-file-environment-p environment)
			     (compile-time-property name 'presentation-type-abbreviation))
			(gethash name *presentation-type-abbreviation-table*))))
      (cond (expander
	     (setq type (funcall expander type))
	     ;; If it didn't pass the standard presentation options through, add them
	     (with-presentation-type-decoded (new-name new-parameters new-options) type
	       (dolist (keyword *standard-presentation-options*)
		 (do ((l options (cddr l)))
		     ((null l))
		   (when (eq (car l) keyword)
		     (unless (do ((l new-options (cddr l)))
				 ((null l) nil)
			       (when (eq (car l) keyword)
				 (return t)))
		       (setq new-options `(,keyword ,(cadr l) ,@new-options)
			     type nil)))))
	       (values (or type
			   `((,new-name ,@new-parameters) ,@new-options))
		       t)))
	    ((member name '(and or sequence sequence-enumerated))
	     (let* ((any-expanded nil)
		    (expansions
		      (mapcar #'(lambda (type)
				  (multiple-value-bind (expansion expanded)
				      (expand-presentation-type-abbreviation-1 type
									       environment)
				    (when expanded (setq any-expanded t))
				    expansion))
			      parameters)))
	       (if any-expanded
		   (values `((,name ,@expansions) ,@options) t)
		   (values type nil))))
	    (t
	     (values type nil))))))

;;; Like macroexpand
(defun expand-presentation-type-abbreviation (type &optional environment)
  (declare (values expansion expanded))
  (let ((flag nil))
    (loop
      (multiple-value-bind (expansion expanded)
	  (expand-presentation-type-abbreviation-1 type environment)
	(unless expanded
	  (unless (compile-file-environment-p environment)
	    (check-type type presentation-type-specifier))
	  (return-from expand-presentation-type-abbreviation
	    (values type flag)))
	(setq type expansion
	      flag t)))))

#+Genera
;;; Genera prefers us to define functions with fdefine rather than just doing puthash
;;; I didn't actually check whether this was important.  It allows for tracing, certainly.
(sys:define-function-spec-handler presentation-type-abbreviation
				  (operation function-spec &optional arg1 arg2)
  (case operation
    ((sys:validate-function-spec)
      (and (= (length function-spec) 2)
	   (symbolp (second function-spec))))
    ((si:definition-has-location-p) nil)
    ((sys:fdefine)
      (setf (gethash (second function-spec) *presentation-type-abbreviation-table*) arg1))
    ((sys:fdefinedp sys:fdefinition)
     (gethash (second function-spec) *presentation-type-abbreviation-table*))
    ((sys:fundefine)
     (remhash (second function-spec) *presentation-type-abbreviation-table*))
    (otherwise
      (si:function-spec-default-handler operation function-spec arg1 arg2))))


;;;; Defining Presentation Types

(defmacro define-presentation-type (name parameters
				    &key options inherit-from description history
					 parameters-are-types
				    &environment environment)
  (check-type name (or symbol (satisfies acceptable-presentation-type-class))
	      "a symbol or a non-built-in class")
  (check-type parameters list)
  (check-type options list)
  (check-type history (or symbol (satisfies acceptable-presentation-type-class))
	      "T, NIL, or the name of another presentation type")
  (check-type description (or string null))
  (check-type parameters-are-types boolean)

  (with-warnings-for-definition name define-presentation-type
    (let* ((parameters-var '#:parameters)
	   (options-var '#:options)
	   (type-var '#:type)
	   (function-var '#:function)
	   (parameters-ll (asterisk-default parameters))
	   (options-ll (lambda-list-from-options options))
	   (class (if (symbolp name) (find-class-that-works name nil environment) name))
	   (direct-superclasses (and class
				     (mapcar #'(lambda (class)
						 (class-proper-name class environment))
					     (class-direct-superclasses class))))
	   ;; Default the supertype if unsupplied or nil
	   ;; This assumes consistency of the compile-time and load-time environments.  Okay?
	   (inherit-from (cond (inherit-from)
			       ((null class) `'standard-object)
			       ((cdr direct-superclasses) `'(and ,@direct-superclasses))
			       (t `',(first direct-superclasses)))))
  
      ;; Default the :description option
      (unless description
	(setq description (substitute #\space #\- (string-downcase
						    (if (symbolp name) name
							(class-name name))))))
  
      ;; Convert :inherit-from into what we need to inherit methods and map over supertypes
      (multiple-value-bind (direct-supertypes parameter-massagers options-massagers)
	  (analyze-inherit-from inherit-from parameters-ll options-ll)
  
	;; Make sure we have a class adequate to use at macro expansion time.
	;; It has to have the right class-precedence-list, as well as serving
	;; as a key for the second position of *presentation-type-being-defined*.
	;; If not compiling to a file, it has to be EQ to the class that will
	;; be used for real when methods are invoked.  If compiling to a file, it
	;; has to be similar as a constant to the class that will be used for real.
	;;--- Might have to think this through a bit better
	;;--- In particular, merely macroexpanding should not define the p.t. class
	;;--- if it was not already defined, and also it's undesirable to change the
	;;--- direct supertypes during macroexpansion, although we have to do that
	;;--- to get the CPL right.  Maybe the CPL has to be specially kludged somehow
	;;--- in *presentation-type-being-defined* ???
	(unless (acceptable-presentation-type-class class)
	  (setq class (ensure-presentation-type name parameters options direct-supertypes
						description history
						parameters-are-types
						parameter-massagers options-massagers
						environment)))
  
	;; Establish the information needed at macro expansion time
	(let ((*presentation-type-being-defined*
		(list name class parameters options
		      direct-supertypes parameter-massagers options-massagers)))
  
	  ;; Generate the form that stores all the information and defines the
	  ;; automatically-defined presentation methods
	  `(progn
	     (eval-when (compile)
	       (ensure-presentation-type ',name ',parameters ',options ',direct-supertypes
					 ',description ',history
					 ',parameters-are-types
					 ',parameter-massagers ',options-massagers
					 ;; Can't just put the environment here, since macro
					 ;; expansion environments have dynamic extent
					 ',(and (compile-file-environment-p environment)
						'compile-file)))
	     (define-group ,name define-presentation-type
	       (ensure-presentation-type ',name ',parameters ',options ',direct-supertypes
					 ',description ',history
					 ',parameters-are-types
					 ',parameter-massagers ',options-massagers)
	       ,@(generate-map-over-presentation-type-supertypes-method-if-needed
		    name class function-var parameters-var options-var type-var environment)
	       #-CLIM-extends-CLOS
	       ,@(generate-presentation-type-inheritance-methods
		   name class parameters-var options-var environment)
	       ',name)))))))

;;; Load-time and compile-time effect of define-presentation-type.
;;; The compile-time effects are dropped after compile-file.
(defun ensure-presentation-type (name parameters options direct-supertypes
				 description history
				 parameters-are-types
				 parameter-massagers options-massagers
				 &optional environment)
  (when (or (eq direct-supertypes 't)	;In CLOS there's an intervening class
	    (null direct-supertypes))	;If the :inherit-from was erroneous
    (unless (eq name 't)		;Don't create a bootstrapping problem
      (typecase (find-class-that-works name nil environment)
	#+(or Symbolics LispWorks)	;Symbolics CLOS, that is
	(clos:structure-class
	 (setq direct-supertypes 'clos:structure-object))
	#+Allegro
	(clos::structure-class
	 (setq direct-supertypes 'common-lisp:structure-object))
	#+CCL-2
	(structure-class
	 (setq direct-supertypes 'structure-object))
	(t
	 (setq direct-supertypes 'standard-object)))))
  (with-warnings-for-definition name define-presentation-type
				(let* ((supertypes-list (if (listp direct-supertypes) 
							    direct-supertypes
							  (list direct-supertypes)))
				       (direct-superclasses (mapcar #'(lambda (name)
									(find-presentation-type-class name t environment))
								    supertypes-list))
				       old-inheritance new-inheritance
				       (clos-class (find-class-that-works name nil environment))
				       (class (find-presentation-type-class name nil environment))
				       #-CLIM-extends-CLOS
				       (old-direct-superclasses (if class
								    (class-direct-superclasses class)
								  direct-superclasses))
				       #+CCL-2
				       (registered-class-name
					(let ((keyword-package (find-package :keyword))
					      (*package* (find-package :lisp)))
					  (intern (lisp:format nil "~A ~S" 'ptype name) keyword-package))))
  
				  ;; If both a regular class and a presentation type class exist,
				  ;; get rid of the presentation type class, with a warning
				  (when (and (not (compile-file-environment-p environment))
					     class clos-class
					     (not (eq class clos-class))
					     (acceptable-presentation-type-class clos-class))
				    (warn "The presentation type ~S is being converted to use ~S.~@
	       If any presentation methods were previously defined for this type,~@
	       they will no longer work until they are recompiled."
					  name clos-class)
				    (remhash name *presentation-type-class-table*)
				    (setq class clos-class))
  
				  ;; Create a presentation-type-class if one does not already exist
				  (cond ((not class)
					 (dolist (superclass direct-superclasses)
					   (unless (or (presentation-type-class-p superclass)
						       (eq superclass
							   (find-presentation-type-class 'standard-object t environment)))
					     (error "The presentation type ~S is being defined with~@
			 the class ~S as a direct supertype.  This is invalid~@
			 because ~S cannot be used to create~@
			 a new subclass of a CLOS class.~
			 ~@[~%Use ~S of ~S first to define the class, then~@
			 use ~3:*~S to define how that class acts as a presentation type.~]"
						    name (class-proper-name superclass environment)
						    'define-presentation-type
						    (typecase superclass
						      (funcallable-standard-class 'defgeneric)
						      (standard-class 'defclass)
						      #+Symbolics ;Symbolics CLOS, that is
						      (clos:structure-class 'defstruct)
						      #+CCL-2 (structure-class 'defstruct))
						    name)))
					 (let ((class-name `(presentation-type ,name)))
					   (setq class #-Lucid (make-instance 'presentation-type-class
									      :direct-superclasses direct-superclasses
									      ;; Symbolics CLOS, that is
									      #+(or Genera Cloe-Runtime) 'clos-internals::name
									      #+(or Genera Cloe-Runtime) class-name
									      #+CCL-2 :name #+CCL-2 class-name
									      #-(or PCL CCL-2 Allegro) :slots
									      #+(or PCL CCL-2 Allegro) :direct-slots
									      nil)
						 ;; The above does not work in Lucid 4.0, so do it this way
						 ;; instead, on JonL's advice.  We can't set the name here
						 ;; because only symbols are accepted as names.
						 #+Lucid (clos-sys:common-add-named-class 
							  (class-prototype (find-class 'presentation-type-class))
							  nil ;name
							  direct-superclasses ;superclasses
							  () ;slots
							  ())) ;options
					   ;;--- Workaround for apparent MCL bug that otherwise causes
					   ;;--- BAD things to happen
					   #+CCL-2 (setf (slot-value class 'ccl::slots) (cons nil (vector)))
					   ;; If the class name couldn't be set while making the class, set it now
					   #-(or Genera Cloe-Runtime CCL-2) (setf (class-name class) class-name)))
					((not (or (equal (class-direct-superclasses class) direct-superclasses)
						  ;; The above equal would suffice if it were not for the fact that when
						  ;; a CLOS class in the compile-file environment has a superclass in the
						  ;; runtime environment, CLOS uses a forward-referenced-class instead
						  ;; of using the run-time class
						  (every #'(lambda (class type)
							     (eq (class-proper-name class environment) type))
							 (class-direct-superclasses class) supertypes-list)
						  (presentation-type-class-p class)))
					 ;; Inheritance must be consistent between CLOS and CLIM classes,
					 ;; but only when CLOS and CLIM use the same class object.
					 ;; When using different class objects, e.g. for built-in-classes, the
					 ;; inheritance will necessarily be different since the metaclasses differ.
					 (warn "The direct supertypes of presentation type ~S, ~S,~@
		    do not match the direct superclasses of ~S, ~S."
					       name supertypes-list class
					       (mapcar #'(lambda (class)
							   (class-proper-name class environment))
						       (class-direct-superclasses class)))))
  
				  ;;--- This used to be done only in the case where we were creating a class
				  ;;--- "de novo".  However, CCL-2 currently doesn't record anything about
				  ;;--- DEFCLASS at compile time, so we may write the new methods for this
				  ;;--- presentation class on this "registered" name instead of on the
				  ;;--- official class name.  The following line hooks up the class and the
				  ;;--- registered name at load time.  -- rsl & York, 4 June 1991
				  #+CCL-2 (setf (gethash class *presentation-class-type-table*) registered-class-name
						;;--- Should the following be done at compile time?  It's unclear.
						(find-class registered-class-name) class)

				  ;; Always put the class into the table, even if FIND-CLASS could find it, for better
				  ;; virtual memory locality in systems where FIND-CLASS uses the property list.
				  (when (symbolp name)
				    (if (compile-file-environment-p environment)
					(setf (compile-time-property name 'presentation-type-class) class)
				      (setf (gethash name *presentation-type-class-table*) class)))
  
				  (setq old-inheritance (gethash class *presentation-type-inheritance-table*)
					new-inheritance (list direct-supertypes parameter-massagers options-massagers))
  
				  ;; Store the information about the presentation type into the tables
				  (cond ((compile-file-environment-p environment)
					 (setf (compile-time-property class 'presentation-type-parameters) parameters))
					(parameters
					 (setf (gethash class *presentation-type-parameters-table*) parameters))
					(t (remhash class *presentation-type-parameters-table*)))
				  (cond ((compile-file-environment-p environment)
					 (setf (compile-time-property class 'presentation-type-options) options))
					(options 
					 (setf (gethash class *presentation-type-options-table*) options))
					(t (remhash class *presentation-type-options-table*)))
				  (if (compile-file-environment-p environment)
				      (setf (compile-time-property class 'presentation-type-description) description)
				    (setf (gethash class *presentation-type-description-table*) description))
				  ;; We use the type's name instead of the class for speed during presentation
				  ;; translator lookup
				  (if (compile-file-environment-p environment)
				      (setf (compile-time-property name 'parameters-are-types) parameters-are-types)
				    (if parameters-are-types
					(pushnew name *presentation-type-parameters-are-types*)
				      (setq *presentation-type-parameters-are-types*
					(delete name *presentation-type-parameters-are-types*))))
				  (cond ((compile-file-environment-p environment)
					 (setf (compile-time-property name 'presentation-type-history) history))
					((null history)
					 (remhash name *presentation-type-history-table*))
					((eql history 't)
					 (setf (gethash name *presentation-type-history-table*)
					   (make-presentation-type-history name)))
					(t
					 (setf (gethash name *presentation-type-history-table*) history)))
				  (if (compile-file-environment-p environment)
				      (setf (compile-time-property class 'presentation-type-inheritance) new-inheritance)
				    (setf (gethash class *presentation-type-inheritance-table*) new-inheritance))
  
				  ;; If it used to be an abbreviation, undefine the abbreviation
				  (cond ((compile-file-environment-p environment)
					 (setf (compile-time-property name 'presentation-type-abbreviation) nil))
					(t
					 #+Genera (sys:fundefine `(presentation-type-abbreviation ,name))
					 #-Genera (remhash name *presentation-type-abbreviation-table*)))
  
				  ;; If class already existed, make sure its inheritance is up to date.
				  ;; This cannot be done until after the information is stored into the tables,
				  ;; since this will recompute method combination, which accesses the tables.
				  ;; This must be done even if class-direct-superclasses equals
				  ;; direct-superclasses, since if there is both a defclass and a
				  ;; define-presentation-type, when changing superclasses the defclass
				  ;; will be redefined first, then when the define-presentation-type
				  ;; is redefined, we need to make sure that methods are recombined based
				  ;; on the new information in *presentation-type-inheritance-table*.
				  ;; If a CLOS implementation optimizes out recombining methods when
				  ;; reinitialize-instance doesn't appear to be changing anything, it will lose.
				  ;; If we don't do method combination based on the presentation type class inheritance
				  ;; tables, and the direct superclasses haven't changed, then don't call
				  ;; reinitialize-instance.  This gets around a bug in some CLOS implementations.
				  (unless (compile-file-environment-p environment)
				    (unless (equal new-inheritance old-inheritance)
				      (unless #+CLIM-extends-CLOS nil ;always recompute method combination
					      #-CLIM-extends-CLOS ;if not massaging the parameters/options during method inheritance
					      (equal direct-superclasses old-direct-superclasses)
					      #-Lucid (reinitialize-instance class :direct-superclasses direct-superclasses)
					      #+Lucid (clos-sys:update-class class :direct-superclasses direct-superclasses))))
  
				  class)))

;;; Called by MAKE-LOAD-FORM forms
(defun load-reference-to-presentation-type-class (name parameters options direct-supertypes
						  description history
						  parameters-are-types
						  parameters-massagers options-massagers)
  (or (find-presentation-type-class name nil)
      (ensure-presentation-type name parameters options direct-supertypes
				description history
				parameters-are-types
				parameters-massagers options-massagers)))

;; We need this during Minima cross-compilation, when the "local" class
;; precedence list and the "remote" class precedence list are not the same.
;; For example, there will be two different STANDARD-OBJECT classes, plus a
;; host of other Genera-specific junk that makes no sense for Minima.
#+Minima
(defun elide-nonessential-superclasses (class-list)
  (let ((tail (cdr (member (find-class 'standard-object) class-list))))
    (if tail
	(append (ldiff class-list tail) (last class-list))
	class-list)))

;;; Generate a map-over-presentation-type-supertypes method if there is any
;;; parameterized inheritance.  Otherwise the default method will work.
;;; If there is parameterized inheritance, compile code to do it efficiently
;;; rather than interpreting the inheritance information at run time.
(defun generate-map-over-presentation-type-supertypes-method-if-needed
       (name class function-var parameters-var options-var type-var
	&optional environment)
  (let ((superclasses
	  #-Allegro (cdr (class-precedence-list class))
	  #+Allegro ;; Work around bug in CLOS compilation environments...
	  (multiple-value-bind (no-errorp result)
	      (excl:errorset (cdr (class-precedence-list class)) nil)
	    (if no-errorp
		result
		(return-from generate-map-over-presentation-type-supertypes-method-if-needed)))))
    #+Minima (setq superclasses (elide-nonessential-superclasses superclasses))
    (multiple-value-bind (bindings alist)
	(generate-type-massagers class superclasses parameters-var options-var t environment)
      (unless (every #'(lambda (class)
			 (let ((massage-forms (cdr (assoc class alist))))
			   (and (null (first massage-forms))		;no parameters
				(null (second massage-forms)))))	;no options
		     superclasses)
	`((define-presentation-method-without-massaging
		map-over-presentation-type-supertypes
		(,parameters-var ,options-var (,type-var ,name) ,function-var)
	    ,parameters-var ,options-var	;might not be used
	    (funcall ,function-var ',name ,type-var)
	    ,@(make-stack-list-bindings bindings
	        (mapcan #'(lambda (class)
			    (let* ((massage-forms (cdr (assoc class alist)))
				   (parameters (first massage-forms))
				   (options (second massage-forms))
				   (type-name (class-presentation-type-name class)))
			      (if (or options parameters)
				  (make-stack-list-bindings
				    `((type ,(if options
						 (if (constantp parameters)
						     (if (constantp options)
							 `',(cons (cons type-name
									(eval parameters))
								  (eval options))
							 `(cons ',(cons type-name
									(eval parameters))
								,options))
						     `(cons (cons ',type-name ,parameters)
							    ,options))
						 (if (constantp parameters)
						     `',(cons type-name (eval parameters))
						     `(cons ',type-name ,parameters)))))
				    `((funcall ,function-var ',type-name type)))
				  `((funcall ,function-var ',type-name ',type-name)))))
			superclasses))
	    nil))))))

;;; Wrap some let* bindings around some forms
;;; All the objects in the bindings have dynamic extent
;;; so use WITH-STACK-LIST as much as possible
(defun make-stack-list-bindings (bindings forms)
;  ;; The easy, ANSI Common Lisp way to do it would be thus:
;  `(let* ,bindings
;     ,@(when bindings `((declare (dynamic-extent ,@(mapcar #'first bindings)))))
;     ,@forms)
  (dolist (binding (reverse bindings))
    (labels ((translate (variable form)
	       (if (and (consp form) (member (first form) '(list list* cons)))
		   (do* ((l (cdr form) (cdr l))
			 (element (car l) (car l))
			 (auxes nil)
			 (elements nil))
			((null l)
			 (setq forms `((,(if (eq (first form) 'list) 'with-stack-list
					     'with-stack-list*)
					 (,variable ,@(nreverse elements))
					,@forms)))
			 (dolist (aux auxes)
			   (translate (first aux) (second aux))))
		     (if (and (consp element) (member (first element) '(list list* cons)))
			 (let ((temp (gensym)))
			   (push (list temp element) auxes)
			   (push temp elements))
			 (push element elements)))
		   (setq forms `((let ((,variable ,form)) ,@forms))))))
      (translate (first binding) (second binding))))
  forms)

;;; Generate efficient code to massage presentation type specifiers for class
;;; into presentation type specifiers for each class in superclasses.
;;; A class can appear multiple times in superclasses if its presentation
;;; type specifier will be used more than once.
;;; If accuratep is false, the parameters/options for presentation types
;;; that don't have any will be ignored, so we can pass whatever is most
;;; convenient.  This speeds up a common case of method combination.
;;; The first value is a list of serial bindings of temporary variables.
;;; The second value is a list of elements (class parameters-form options-form)
(defun generate-type-massagers (class superclasses parameters-var options-var accuratep
				&optional environment)
  (declare (values bindings alist))
  (let ((paths nil)
	(classes-seen nil)
	(class-bindings-used nil)	;((class parameters-count options-count)...)
	(bindings nil)
	alist)
    (labels ((map-over-superclasses (function class &rest trail)
	       (declare (dynamic-extent trail))
	       (funcall function class trail)
	       (dolist (superclass (class-direct-superclasses class))
		 (apply #'map-over-superclasses function superclass class trail)))
	     (note-path (class trail)
	       (unless (assoc class paths)
		 (note-class class)
		 (dolist (subclass trail)
		   (note-class subclass))
		 (push (copy-list (cons class trail)) paths)))
	     (note-class (class)
	       (pushnew class classes-seen))
	     (note-binding-used (class type)
	       (let ((item (assoc class class-bindings-used)))
		 (unless item
		   (push (setq item (list class 0 0)) class-bindings-used))
		 (ecase type
		   (parameters (incf (second item)))
		   (options (incf (third item)))))))
      (declare (dynamic-extent #'note-path))

      ;; Find the direct inheritance path to each class
      (map-over-superclasses #'note-path class)
      (dolist (superclass superclasses)
	(unless (assoc superclass paths)
	  (error "~S is not a superclass of ~S" superclass class)))

      ;; Remember how many times the parameters and options of each terminal class
      ;; will be used
      (dolist (class superclasses)
	(note-binding-used class 'parameters)
	(note-binding-used class 'options))

      ;; Build up the alist of massaging forms
      ;; This traverses the classes in most-specific-first order
      ;; The first pass identifies common subexpressions, the second builds the real alist
      (dolist (pass '(first second))
	(setq alist `((,class ,parameters-var ,options-var)))
	(dolist (to-class classes-seen)
	  (do* ((path (reverse (assoc to-class paths)) (cdr path))
		(from-class (first path) (first path))
		(to-class (second path) (second path)))
	       ((null to-class))
	    (unless (assoc to-class alist)
	      (let ((parameters-ll (asterisk-default
				     (presentation-type-parameters from-class environment)))
		    (options-ll (lambda-list-from-options
				  (presentation-type-options from-class environment)))
		    (supertype (class-name to-class))
		    (from-data (assoc from-class alist)))
		(assert (not (null from-data)) ()
			"INTERNAL ERROR: classes-seen is out of order")
		(when (and (consp supertype) (eq (first supertype) 'presentation-type))
		  (setq supertype (second supertype)))
		(multiple-value-bind (direct-supertypes parameters-massager options-massager)
		    (presentation-type-inheritance from-class environment)
		  (when (listp direct-supertypes)
		    (do ()	;Find correct supertype when multiple inheritance
			((or (null direct-supertypes)
			     (eq (car direct-supertypes) supertype))
			 (setq parameters-massager (car parameters-massager)
			       options-massager (car options-massager)))
		      (setq direct-supertypes (cdr direct-supertypes)
			    parameters-massager (cdr parameters-massager)
			    options-massager (cdr options-massager))))
  
		  ;; Make the massaging forms that go from from-class to to-class
		  (setq parameters-massager
			(cond ((and (not accuratep) (null (presentation-type-parameters
							    to-class environment)))
			       ;; Pass inaccurate, but faster value
			       ;; since it's going to be ignored anyway
			       parameters-var)
			      ((constantp parameters-massager)
			       parameters-massager)
			      ((and (equal parameters-massager
					   (lambda-list-pass-through parameters-ll))
				    (equal (presentation-type-parameters from-class
									 environment)
					   (presentation-type-parameters to-class
									 environment)))
			       ;; The subtype's parameters are passed through to the supertype
			       (when (eq pass 'first)
				 (note-binding-used from-class 'parameters))
			       (second from-data))
			      ((lambda-list-variables-used-in-body
				 parameters-ll (list parameters-massager))
			       ;; The massager is parameterized, wrap bindings around it
			       (when (eq pass 'first)
				 (note-binding-used from-class 'parameters))
			       `(bind-to-list ,parameters-ll ,(second from-data)
				  ,parameters-massager))
			      (t parameters-massager)))
		  (setq options-massager
			(cond ((and (not accuratep)
				    (null options-massager)
				    (equal (presentation-type-options to-class environment)
					   (presentation-type-options class environment))
				    (= (length (presentation-type-options to-class
									  environment))
				       (length *standard-presentation-options*)))
			       ;; Pass inaccurate, but faster value
			       ;; since it's going to be ignored anyway
			       options-var)
			      ((constantp options-massager)
			       options-massager)
			      ((and (equal options-massager
					   (lambda-list-pass-through options-ll))
				    (equal (presentation-type-options from-class environment)
					   (presentation-type-options to-class environment)))
			       ;; The subtype's options are passed through to the supertype
			       (when (eq pass 'first)
				 (note-binding-used from-class 'options))
			       (third from-data))
			      ((lambda-list-variables-used-in-body
				 options-ll (list options-massager))
			       ;; The massager is parameterized, wrap bindings around it
			       (when (eq pass 'first)
				 (note-binding-used from-class 'options))
			       `(bind-to-list ,options-ll ,(third from-data)
				  ,options-massager))
			      (t options-massager)))

		  ;; Remove common subexpressions referenced more than once
		  (when (eq pass 'second)
		    (unless (or (constantp parameters-massager) (symbolp parameters-massager)
				(member (second (assoc to-class class-bindings-used))
					'(0 1 nil)))
		      (let ((temp (make-symbol (format nil "~A-PARAMETERS" supertype))))
			(push (list temp parameters-massager) bindings)
			(setq parameters-massager temp)))
		    (unless (or (constantp options-massager) (symbolp options-massager)
				(member (third (assoc to-class class-bindings-used))
					'(0 1 nil)))
		      (let ((temp (make-symbol (format nil "~A-OPTIONS" supertype))))
			(push (list temp options-massager) bindings)
			(setq options-massager temp))))
		  (push (list to-class parameters-massager options-massager) alist)))))))
      (values (nreverse bindings) alist))))

;;; Analyze the :inherit-from backquote form to get the lists of direct supertype
;;; names, parameter massaging forms, and option massaging forms,
;;; and to check for violation of the separation of parameters and options.
;;; This requires that the backquote form be a simple substitution of variables
;;; into positions in a fixed structure, not conditional and not involve
;;; computation on the values of the variables.
;;; The values are lists of equal length and parallel contents, except that
;;; if the length is 1, each value is just the list element, to save space in
;;; compiled files.
(defun analyze-inherit-from (backquote parameters-ll options-ll)
  (declare (values direct-supertypes parameter-massagers options-massagers))
  (multiple-value-bind (expansion parameters-bindings options-bindings)
      (meta-evaluate-form backquote parameters-ll options-ll)
    (with-presentation-type-decoded (expanded-name expanded-parameters expanded-options)
				    expansion
      ;; Generate the "massager" that translates the parameters or options of a
      ;; type into the parameters or options of a direct supertype.  It is a form that
      ;; can be evaluated inside the bindings offered by parameters-ll or options-ll.
      ;; This also checks for violation of the separation of parameters and options.
      (flet ((generate-massager (expansion bindings bad-bindings
				 good-word bad-word supertype-name)
	       (let ((bad-variables nil))
		 (labels ((backquotify (object)
			    (cond ((car (rassoc object bindings)))
				  ((rassoc object bad-bindings)
				   (pushnew (car (rassoc object bad-bindings)) bad-variables)
				   nil)
				  ((consp object)
				   (multiple-value-bind (forms tail)
				       (do ((l object (cdr l))
					    (forms nil (cons (backquotify (car l)) forms)))
					   ((atom l)
					    (when l (push (backquotify l) forms))
					    (values (nreverse forms) l)))
				     (if (every #'constantp forms)
					 (list 'quote object)
					 (cons (if tail 'list* 'list) forms))))
				  ((and (vectorp object) (not (stringp object)))
				   (let ((forms (map 'list #'backquotify object)))
				     (if (every #'constantp forms)
					 object
					 (cons 'vector forms))))
				  ((and (symbolp object) (not (keywordp object))
					(not (eq object nil)) (not (eq object t)))
				   (list 'quote object))
				  (t object))))
		   (prog1 (backquotify expansion)
			  (when bad-variables
			    (setq bad-variables (sort bad-variables #'string<))
			    (warn "The presentation type ~A~P ~{~S~^, ~} ~
				   appear~A in the ~As of the supertype ~S."
				  bad-word (length bad-variables) bad-variables
				  (if (cdr bad-variables) "" "s") good-word supertype-name))
			  )))))
	(case expanded-name
	  ((or)
	   (warn "OR is not supported in :INHERIT-FROM")
	   (values nil nil nil))
	  ((and)
	   ;; Multiple inheritance -- first get the direct supertype names
	   (let ((direct-supertypes
		   (mapcar #'(lambda (type)
			       (with-presentation-type-decoded (expanded-name) type
				 (when (member expanded-name '(and or not satisfies))
				   (warn "AND in :INHERIT-FROM only supports classes, not ~S"
					 type))
				 expanded-name))
			   expanded-parameters)))
	     (when expanded-options
	       (warn "AND in :INHERIT-FROM does not support options."))
	     ;; Generate the inheritance information for each supertype
	     (values direct-supertypes
		     (mapcar #'(lambda (supertype)
				 (with-presentation-type-decoded (name parameters) supertype
				   (generate-massager parameters parameters-bindings
						      options-bindings
						      "parameter" "option" name)))
			     expanded-parameters)
		     (mapcar #'(lambda (supertype)
				 (with-presentation-type-decoded (name nil options) supertype
				   (generate-massager options options-bindings
						      parameters-bindings
						      "option" "parameter" name)))
			     expanded-parameters))))
	  (otherwise
	    ;; Single inheritance is a bit simpler
	    (values expanded-name
		    (generate-massager expanded-parameters parameters-bindings
				       options-bindings
				       "parameter" "option" expanded-name)
		    (generate-massager expanded-options options-bindings
				       parameters-bindings
				       "option" "parameter" expanded-name))))))))


;;;; The slow implementation of presentation method inheritance
;;;; that doesn't depend on meta objects

;;--- Some of the methods here are redundant and will never be called.
;;--- Also sometimes bindings are generated that are not used.
;;--- I didn't bother figuring out how to optimize further.

#-CLIM-extends-CLOS
(defun generate-presentation-type-inheritance-methods
       (name class parameters-var options-var &optional environment)
  (let ((superclasses 
	  #-Allegro (cdr (class-precedence-list class))
	  #+Allegro ;; Work around bug in CLOS compilation environments...
	  (multiple-value-bind (no-errorp result)
	      (excl:errorset (cdr (class-precedence-list class)) nil)
	    (if no-errorp
		result
		(return-from generate-presentation-type-inheritance-methods))))
	(to-type-name-var '#:to-type-name)
	(from-type-name-var '#:from-type-name))
    #+Minima (setq superclasses (elide-nonessential-superclasses superclasses))
    (mapcan #'(lambda (superclass)
		(multiple-value-bind (bindings alist)
		    (generate-type-massagers class (list superclass)
					     parameters-var options-var t environment)
		  `(,@(when (presentation-type-parameters superclass environment)
			`((defmethod inherited-presentation-type-parameters-method
				     ((,to-type-name-var
				       (eql ',(class-presentation-type-name superclass)))
				      (,from-type-name-var (eql ',name))
				      ,parameters-var)
			    ,parameters-var	;might not be used
			    (let* ,bindings
			      ,@(when bindings
				  `((declare (dynamic-extent ,@(mapcar #'first bindings)))
				    ,@(mapcar #'first bindings)))
			      ,(second (assoc superclass alist))))))
		    ,@(when (presentation-type-options superclass environment)
			`((defmethod inherited-presentation-type-options-method
				     ((,to-type-name-var
				       (eql ',(class-presentation-type-name superclass)))
				      (,from-type-name-var (eql ',name))
				      ,options-var)
			    ,options-var	;might not be used
			    (let* ,bindings
			      ,@(when bindings
				  `((declare (dynamic-extent ,@(mapcar #'first bindings)))
				    ,@(mapcar #'first bindings)))
			      ,(third (assoc superclass alist)))))))))
	    superclasses)))

#-CLIM-extends-CLOS
(defun inherited-presentation-type-parameters (to-type-name from-type from-parameters)
  (with-presentation-type-decoded (from-type-name) from-type
    (unless (symbolp from-type-name) 
      (setq from-type-name (class-presentation-type-name from-type-name)))
    (if (eq from-type-name to-type-name)
	from-parameters
	(inherited-presentation-type-parameters-method to-type-name from-type-name
						       from-parameters))))

#-CLIM-extends-CLOS
(defun inherited-presentation-type-options (to-type-name from-type from-options)
  (with-presentation-type-decoded (from-type-name) from-type
    (unless (symbolp from-type-name) 
      (setq from-type-name (class-presentation-type-name from-type-name)))
    (if (eq from-type-name to-type-name)
	from-options
	(inherited-presentation-type-options-method to-type-name from-type-name
						    from-options))))


;;;; Presentation Generic Functions

;;; This hash table is keyed by the presentation function name and yields
;;; a list (generic-function-name . lambda-list), where the symbols type,
;;; type-key, type-class, parameters, and options are special in lambda-list.
(defvar *presentation-generic-function-table* (make-hash-table))

;;; Define a presentation generic function, for which presentation methods
;;; can be defined.
(defmacro define-presentation-generic-function (generic-function-name
						presentation-function-name
						lambda-list &rest options)
  (check-type presentation-function-name symbol)	;SETFs don't work, I'm afraid
  (unless (member 'type lambda-list)
    (error "The parameter named ~S is missing from the lambda-list for ~S, ~S"
	   'type generic-function-name lambda-list))
  ;; The first three parameters must be type-key or type-class, parameters, and options,
  ;; except that parameters and/or options can be omitted.  If the first parameter
  ;; is type-class, then presentation type methods don't inherit -- this allows not
  ;; defining a method to mean use the default (even if a supertype defines a method).
  (unless (member (first lambda-list) '(type-key type-class))
    (error "The first parameter in the lambda-list for ~S must be ~S or ~S, not ~S"
	   generic-function-name 'type-key 'type-class (first lambda-list)))
  (when (member 'parameters (cddr lambda-list))
    (error "The parameter named ~S is misplaced in the lambda-list for ~S, ~S"
	   'parameters generic-function-name lambda-list))
  (when (member 'options (nthcdr (if (eq (second lambda-list) 'parameters) 3 2) lambda-list))
    (error "The parameter named ~S is misplaced in the lambda-list for ~S, ~S"
	   'options generic-function-name lambda-list))
  `(progn
     (setf (gethash ',presentation-function-name *presentation-generic-function-table*)
	   `(,',generic-function-name ,@',lambda-list))
     (defgeneric ,generic-function-name ,lambda-list
       #-CCL-2
       (:generic-function-class presentation-generic-function)
       #+CLIM-extends-CLOS
       (:method-combination presentation-method-combination)
       ,@options)))

;;; The call side of argument massaging
;;; Note that the presentation-type-specifier is evaluated multiple times
(defmacro call-presentation-generic-function (presentation-function-name &body arguments)
  (let* ((apply (when (eq presentation-function-name 'apply)
		  (setq presentation-function-name (pop arguments))
		  t))
	 (info (or (gethash presentation-function-name *presentation-generic-function-table*)
		   (error "~S is not the name of a presentation~@
			   function that supports presentation methods."
			  presentation-function-name)))
	 (generic-function-name (car info))
	 (generic-lambda-list (cdr info))
	 (type-name-var '#:type-name)
	 (parameters-var (and (member 'parameters generic-lambda-list) '#:parameters))
	 (options-var (and (member 'options generic-lambda-list) '#:options))
	 ;; Position in arguments of the type parameter
	 (type-pos (- (position 'type generic-lambda-list)
		      (+ 1 (if parameters-var 1 0) (if options-var 1 0))))
	 (presentation-type-specifier (nth type-pos arguments)))
    `(with-presentation-type-decoded (,type-name-var ,parameters-var ,options-var)
				     ,presentation-type-specifier
       (,@(if apply
	      `(apply #',generic-function-name)
	      `(,generic-function-name))
	,(ecase (second info)
	   (type-key `(class-prototype (find-presentation-type-class ,type-name-var)))
	   (type-class `(find-presentation-type-class ,type-name-var)))
	,@(when parameters-var `(,parameters-var))
	,@(when options-var `(,options-var))
	,@arguments))))

;;; Presentation generic functions have their own class just so we can define
;;; one method that aids in implementing presentation-method-combination
#-CCL-2
(eval-when (eval load compile)
(defclass presentation-generic-function
	  (standard-generic-function #+Minima-Developer standard-object) 
  ()
  (:metaclass funcallable-standard-class))
)	;end of (eval-when (eval load compile)

#+CLIM-extends-CLOS
(defvar *presentation-method-argument-class*)

#+CLIM-extends-CLOS
(defmethod clos-internals::compute-effective-method-1 :around
	   ((generic-function presentation-generic-function)
	    methods argument-function)
  (declare (ignore methods))
  (let ((*presentation-method-argument-class*
	  (ecase (first (clos:generic-function-lambda-list generic-function))
	    (type-key (funcall argument-function
			       #-Minima-Developer 'class 
			       #+Minima-Developer 'zl:::clos-internals::class
			       0))
	    (type-class (multiple-value-bind (valid class)
			    (funcall argument-function
				     #-Minima-Developer 'eql
				     #+Minima-Developer 'zl:::clos-internals::eql
				     0)
			  (if valid
			      class
			      ;; I don't know why control sometimes gets here, but it does.
			      ;; Something to do with the class dispatch that precedes
			      ;; an EQL dispatch.  I think the value we return in this
			      ;; case never gets used anyway.
			      (funcall argument-function
				       #-Minima-Developer 'class 
				       #+Minima-Developer 'zl:::clos-internals::class
				       0)))))))
    (call-next-method)))

#+CLIM-extends-CLOS
;;; A lot like standard method combination, except for massaging of the
;;; presentation-type-parameters and -options arguments, using an
;;; extension to call-method
;;;--- If combined methods were generated during compile-file, this would need
;;;--- to get an environment argument from somewhere
(eval-when (compile load eval)
(define-method-combination presentation-method-combination ()
      ((around (:around))
       (before (:before))
       (primary () :required t)
       (after (:after)))
    (:arguments arg1 arg2 arg3)
    (:generic-function generic-function)
  (setq after (reverse after))
  (labels ((method-class (method)
	     (let ((class (first (clos:method-specializers method))))
	       (if (consp class) (second class) class))))
    (let* ((generic-function-name (clos:generic-function-name generic-function))
	   (generic-lambda-list (block nil
				  (maphash #'(lambda (key value)
					       (declare (ignore key))
					       (when (eq (car value) generic-function-name)
						 (return (cdr value))))
					   *presentation-generic-function-table*)
				  (error "INTERNAL ERROR: no generic-lambda-list found.")))
	   (parameters (and (member 'parameters generic-lambda-list) arg2))
	   (options (and (member 'options generic-lambda-list) (if parameters arg3 arg2)))
	   (superclasses (mapcar #'method-class (append around before primary after)))
	   (class *presentation-method-argument-class*))
      (multiple-value-bind (bindings alist)
	  (generate-type-massagers class superclasses parameters options nil)
	(labels ((call-method (method &optional (next-methods nil next-methods-p))
		   (setq next-methods		;massage call-next-method args also
			 (mapcar #'(lambda (method)
				     (if (atom method)
					 (let ((call (call-method method)))
					   (if (cddr call)
					       `(make-method ,call)
					       method))
					 method))
				 next-methods))
		   (let ((info (assoc (method-class method) alist)))
		     (if (and (not (eq (first info) (find-class 't)))
			      (or (and parameters (not (eq (second info) parameters)))
				  (and options (not (eq (third info) options)))))
			 `(call-method ,method
				       :arguments (,@(when parameters
						       `(1 ,(second info)))
						   ,@(when options
						       `(,(if parameters 2 1) ,(third info))))
				       ,@(when next-methods-p
					   `(:next-methods ,next-methods)))
			 `(call-method ,method ,@(when next-methods-p
						   `(,next-methods))))))
		 (call-methods (methods)
		   (mapcar #'call-method methods)))
	  (let ((form (if (or before after (rest primary))
			  `(multiple-value-prog1
			     (progn ,@(call-methods before)
				    ,(call-method (first primary) (rest primary)))
			     ,@(call-methods (reverse after)))
			  (call-method (first primary) ()))))
	    (when around
	      (setq form (call-method (first around)
				      `(,@(rest around)
					(make-method ,form)))))
	    (if bindings
		`(let* ,bindings ,form)
		form)))))))
)	;end of (eval-when (compile load eval)


;;;; Definitions of Presentation Generic Functions

(define-presentation-generic-function map-over-presentation-type-supertypes-method
				      map-over-presentation-type-supertypes
  (type-class parameters options type function))

(define-presentation-generic-function present-method
				      present
  (type-key parameters options object type stream view
	    &key &allow-other-keys)
	    ;; &key &allow-other-keys allows methods to receive only the keyword arguments
	    ;; that they are interested in.  We know the caller will never supply
	    ;; misspelled keywords since only the CLIM system calls this.
  #-CCL-2
  (declare (arglist type-key parameters options object type stream view
		    &key acceptably for-context-type)))

(define-presentation-generic-function accept-method
				      accept
  (type-key parameters options type stream view
	    &key &allow-other-keys)
	    ;; &key &allow-other-keys allows methods to receive only the keyword arguments
	    ;; that they are interested in.  We know the caller will never supply
	    ;; misspelled keywords since only the CLIM system calls this.
  #-CCL-2
  (declare (arglist type-key parameters options type stream view
		    &key default default-type)
	   (values object type)))

(define-presentation-generic-function describe-presentation-type-method
				      describe-presentation-type
  (type-key parameters options type stream plural-count))

(define-presentation-generic-function presentation-typep-method
				      presentation-typep
  (type-key parameters object type))

(define-presentation-generic-function presentation-subtypep-method
				      presentation-subtypep
  (type-key type putative-supertype)
  #-CCL-2
  (declare (values subtype-p known-p)))

(define-presentation-generic-function presentation-type-history-method
				      presentation-type-history
  (type-key parameters type))

(define-presentation-generic-function presentation-default-preprocessor-method
				      presentation-default-preprocessor
  (type-key default type &key &allow-other-keys)
	    ;; &key &allow-other-keys allows methods to receive only the keyword arguments
	    ;; that they are interested in.  We know the caller will never supply
	    ;; misspelled keywords since only the CLIM system calls this.
  #-CCL-2
  (declare (arglist type-key default type &key default-type)
	   (values default default-type)))

(define-presentation-generic-function presentation-type-specifier-p-method
				      presentation-type-specifier-p
  (type-key parameters options type))

(define-presentation-generic-function accept-present-default-method
				      accept-present-default
  (type-key parameters options type
	    stream view default default-supplied-p
	    present-p query-identifier
	    &key &allow-other-keys)
  #-CCL-2
  (declare (arglist type-key parameters options type
		    stream view default default-supplied-p
		    present-p query-identifier
		    &key (prompt t))))

(define-presentation-generic-function highlight-presentation-method
				      highlight-presentation
  (type-key parameters options type
	    record stream state))


;;;; Presentation Methods

;;; Define a presentation method, which involves translating the generic function name,
;;; translating the specializer of the type argument, and wrapping the body in
;;; lexical bindings of the presentation type parameters and options.
(defmacro define-presentation-method (presentation-function-name &rest body
				      &environment environment)
  (declare (arglist presentation-function-name [qualifiers]* specialized-lambda-list &body body))
  (define-presentation-method-1 presentation-function-name body ':massage environment))
  
;;; The next level down from define-presentation-method, this doesn't create
;;; lexical bindings of the presentation type parameters and options and hence
;;; doesn't insert those parameters into the lambda-list.  It still inserts
;;; a type-key or type-class parameter.
(defmacro define-presentation-method-without-massaging (presentation-function-name &rest body
							&environment environment)
  (declare (arglist presentation-function-name [qualifiers]* specialized-lambda-list &body body))
  (define-presentation-method-1 presentation-function-name body nil environment))

;;; Define a default method for a presentation generic function.
;;; It will be invoked if the presentation type doesn't have a specialized method.
;;; There is no parameters/options processing, this macro just exists for abstraction.
(defmacro define-default-presentation-method (presentation-function-name &rest body
					      &environment environment)
  (declare (arglist presentation-function-name [qualifiers]* specialized-lambda-list &body body))
  (define-presentation-method-1 presentation-function-name body ':default environment))

;;; Common subroutine of the three forms of define-presentation-method
(defun define-presentation-method-1 (presentation-function-name body kind environment)
  ;;--- Remodularize this some time, because presentation-function-name isn't
  ;; really right, but it's too hard to compute the actual name of this definition
  ;; until we've parsed a bunch of stuff.
  (with-warnings-for-definition presentation-function-name define-presentation-method
    (let* ((qualifiers (do ((qualifiers nil (cons (pop body) qualifiers)))
			   ((listp (car body)) (nreverse qualifiers))))
	   (specialized-lambda-list (copy-list (pop body)))
	   (info (or (gethash presentation-function-name *presentation-generic-function-table*)
		     (error "~S is not the name of a presentation~@
			     function that supports presentation methods."
			    presentation-function-name)))
	   (generic-lambda-list (cdr info))
	   (parameters (and kind (member 'parameters generic-lambda-list)))
	   (options (and kind (member 'options generic-lambda-list)))
	   ;; Position in specialized-lambda-list of the type parameter
	   (type-pos (- (position 'type generic-lambda-list)
			(+ 1 (if parameters 1 0) (if options 1 0))))
	   type-var	;name of the user's variable
	   ;; Uninterned symbols for "magic" parameters added behind the user's back
	   (type-key-var '#:type-key)
	   (type-class-var '#:type-class)
	   (parameters-var '#:parameters)
	   (options-var '#:options)
	   presentation-type-name class)
      (multiple-value-bind (documentation declarations body)
	  (extract-declarations body environment)
  
	(unless (eq kind ':default)
	  ;; Extract the presentation-type-name from the specialized-lambda-list
	  ;; and make the type parameter unspecialized, since that argument is just
	  ;; a presentation type specifier
	  (setq presentation-type-name (nth type-pos specialized-lambda-list))
	  (unless (and (consp presentation-type-name)
		       (consp (cdr presentation-type-name))
		       (null (cddr presentation-type-name))
		       (atom (second presentation-type-name)))	;symbol or class
	    (error "The TYPE parameter specifier, ~S, is invalid." presentation-type-name))
	  (setq type-var (first presentation-type-name))
	  (setq presentation-type-name (second presentation-type-name))
	  (setf (nth type-pos specialized-lambda-list) type-var)
	  (push type-var body)	;specialized parameters don't get unused variable warnings
    
	  ;; Find the presentation type class, the key to all the information we need
	  (setq class (find-presentation-type-class presentation-type-name t environment))
  
	  ;; Wrap the body in lexical bindings of the presentation type parameters and options
	  (when parameters
	    (let ((parameters-ll (asterisk-default
				   (presentation-type-parameters class environment))))
	      (when (lambda-list-variables-used-in-body parameters-ll body)
		(setq body `((bind-to-list 
			       ,parameters-ll
			       ;; If massaging the parameters during
			       ;; method inheritance
			       #+CLIM-extends-CLOS ,parameters-var
			       ;; If doing it the slow way
			       #-CLIM-extends-CLOS (inherited-presentation-type-parameters
						     ',presentation-type-name
						     ,type-var
						     ,parameters-var)
			       ,@body))))))
	  (when options
	    (let ((options-ll (lambda-list-from-options
				(presentation-type-options class environment))))
	      (when (lambda-list-variables-used-in-body options-ll body)
		(setq body `((bind-to-list
			       ,options-ll
			       ;; If massaging the options during
			       ;; method inheritance
			       #+CLIM-extends-CLOS ,options-var
			       ;; If doing it the slow way
			       #-CLIM-extends-CLOS (inherited-presentation-type-options
						     ',presentation-type-name
						     ,type-var
						     ,options-var)
			       ,@body)))))))
    
	;; Add the "magic" parameters to the front of the lambda list
	;; and specialize this method to the presentation type class
	;; It's okay not to use the "magic" parameters so add dummy uses to the body
	(when options
	  (push options-var specialized-lambda-list)
	  (push options-var body))
	(when parameters
	  (push parameters-var specialized-lambda-list)
	  (push parameters-var body))
	(push (ecase (first generic-lambda-list)
		(type-key `(,type-key-var
			    ,(cond ((eq kind ':default) `t)
				   ((eq (class-name class) presentation-type-name)
				    presentation-type-name)
				   (t 
                                    #+CCL-2 (gethash class *presentation-class-type-table*)
                                    #-CCL-2 class))))
		(type-class `(,type-class-var
			      ,(cond ((eq kind ':default) `t)
				     ((symbolp presentation-type-name)
				      `(eql (find-presentation-type-class
					      ',presentation-type-name)))
				     (t `(eql ,class))))))
	      specialized-lambda-list)
  
	#+Genera
	(when (null kind)
	  (push `(declare (sys:function-parent ,presentation-type-name define-presentation-type))
		declarations))
  
	;;--- Note that defmethod doesn't accept class objects, according to 88-002R.
	;;--- We will have to do something for that (like give them gensym names?  Ick!)
	
	;; Expand into a defmethod
	`(defmethod ,(car info) ,@qualifiers ,specialized-lambda-list
	   ,@(and documentation (list documentation))
	   ,@declarations
	   (block ,presentation-function-name
	     ,@body))))))


;;;; Basic Development System Support for Presentation Types and Methods

#+Genera 
(scl:defprop define-presentation-type "CLIM Presentation Type" si:definition-type-name)

#+Genera 
(scl:defprop define-presentation-type-abbreviation define-presentation-type
	     zwei:definition-function-spec-type)

(defun remove-presentation-type (name)
  #+Genera (sys:fundefine `(presentation-type-abbreviation ,name))
  #-Genera (remhash name *presentation-type-abbreviation-table*)
  (remhash name *presentation-type-parameters-table*)
  (remhash name *presentation-type-options-table*)
  (let ((class (find-presentation-type-class name nil)))
    (when class
      (remhash class *presentation-type-parameters-table*)
      (remhash class *presentation-type-options-table*)
      (remhash class *presentation-type-inheritance-table*)
      (remhash name *presentation-type-class-table*)
      (remhash class *presentation-type-description-table*)
      (remhash name *presentation-type-history-table*)
      (setq *presentation-type-parameters-are-types*
	    (delete name *presentation-type-parameters-are-types*))))
  ;--- maybe need to jerk the class around a bit more than this
  name)

#+Genera
(scl:defprop define-presentation-type remove-presentation-type zwei:kill-definition)

#+Genera
(scl:defprop define-presentation-generic-function zl:::scl:defun
	     zwei:definition-function-spec-type)

;---I think these don't get used, because methods are just functions
;#+Genera 
;(scl:defprop define-presentation-method "Presentation Method" si:definition-type-name)
;
;#+Genera 
;(scl:defprop define-presentation-method-without-massaging define-presentation-method
;	     zwei:definition-function-spec-type)
;
;#+Genera 
;(scl:defprop define-default-presentation-method define-presentation-method
;	     zwei:definition-function-spec-type)

#+Genera 
(defun (define-presentation-method zwei:definition-function-spec-parser) (bp)
  (presentation-method-definition-function-spec-parser bp ':massage))
  
#+Genera 
(defun (define-presentation-method-without-massaging zwei:definition-function-spec-parser) (bp)
  (presentation-method-definition-function-spec-parser bp nil))
  
#+Genera 
(defun (define-default-presentation-method zwei:definition-function-spec-parser) (bp)
  (presentation-method-definition-function-spec-parser bp ':default))
  
#+Genera 
(defun presentation-method-definition-function-spec-parser (bp kind)
  (multiple-value-bind (fspec type str error-p)
      (funcall (zl:::scl:function
		 (:property clos:defmethod zwei:definition-function-spec-parser)) bp)
    (unless error-p
      (let* ((info (or (gethash (second fspec) *presentation-generic-function-table*)
		       (return-from presentation-method-definition-function-spec-parser
			 (values nil nil nil t))))
	     (generic-lambda-list (cdr info))
	     (parameters (and kind (member 'parameters generic-lambda-list)))
	     (options (and kind (member 'options generic-lambda-list)))
	     ;; Position in specialized-lambda-list of the type parameter
	     (type-pos (- (position 'type generic-lambda-list)
			  (+ 1 (if parameters 1 0) (if options 1 0))))
	     (specializers (third fspec))
	     presentation-type-name)
	(unless (eq kind ':default)
	  ;; Extract the presentation-type-name from the specialized-lambda-list
	  ;; and make the type parameter unspecialized, since that argument is just
	  ;; a presentation type specifier
	  (setq presentation-type-name (nth type-pos specializers))
	  (unless (> (length specializers) type-pos)
	    (return-from presentation-method-definition-function-spec-parser
	      (values nil nil nil t)))
	  (setq specializers (append (subseq specializers 0 type-pos)
				     '(t)
				     (subseq specializers (1+ type-pos)))))
	;; Add the "magic" parameters to the front of the lambda list
	;; and specialize this method to the presentation type class
	(when options (push t specializers))
	(when parameters (push t specializers))
	(let ((class (find-presentation-type-class presentation-type-name nil)))
	  (push (ecase (first generic-lambda-list)
		  (type-key (cond ((eq kind ':default) `t)
				  ((or (null class)
				       (eq (class-name class) presentation-type-name))
				   presentation-type-name)
				  (t class)))
		  (type-class (cond ((eq kind ':default) `t)
				    (t `(eql ,class)))))
		specializers)
	  (setq fspec `(clos:method ,(car info) ,specializers ,@(cdddr fspec))))))
    (values fspec type str error-p)))

;;; Make c-sh-A work better
#+Genera
(progn
(dolist (x '(define-presentation-method
	     define-presentation-method-without-massaging
	     define-default-presentation-method))
  (pushnew x zwei:*irrelevant-functions*)
  (pushnew x zwei:*irrelevant-defining-forms*)
  (pushnew x zwei:*forms-that-define-things-with-names-that-are-symbols*)
  (pushnew (cons x 'define-presentation-method-function-name-hook)
	   zwei:*forms-that-define-things-with-names-that-are-symbols-translator-alist*
	   :test #'equal))

(defun define-presentation-method-function-name-hook (presentation-function-name)
  (let ((info (gethash presentation-function-name *presentation-generic-function-table*)))
    (when info
      (values (first info)
	      (+ 1
		 (if (eq (third info) 'parameters)
		     (progn (pop info) 1) 0)
		 (if (eq (third info) 'options) 1 0))))))
)	;#+Genera

;;; Warn about doing defclass and define-presentation-type in the wrong order
;;; This is not valid portable code, but I happen to know that in Genera
;;; there is not already a :AFTER method with these specializers
#+Genera
(defmethod clos-internals::ensure-class-using-class :after (class name
							    &key environment metaclass
							    &allow-other-keys)
  (declare (ignore class))	;might be NIL
  (unless (symbolp metaclass)
    (setq metaclass (class-name metaclass)))
  (unless (or (null name)
	      (member metaclass '(clos:forward-referenced-class presentation-type-class)))
    ;; Find the CLOS and Presentation Type classes within the environment, without inheritance
    ;; So don't call find-class-that-works and find-presentation-type-class
    (let ((clos-class (find-class name t environment))
	  (presentation-type-class (if (compile-file-environment-p environment)
				       (compile-time-property name 'presentation-type-class)
				       (gethash name *presentation-type-class-table*))))
      (when (and presentation-type-class
		 (not (eq presentation-type-class clos-class)))
	(let (#+Genera (compiler:default-warning-function name)
	      #+Genera (compiler:default-warning-definition-type 'define-presentation-type))
	  (warn "The presentation type ~S was defined before the ~(~A~) ~S.~@
		 When a presentation type is also a class, the class must be defined first~@
		 or presentation methods will not work properly."
		name (or metaclass 'class) name))))))

#+Genera
(defmethod clos-internals::class-name-string-for-matchp ((class presentation-type-class))
  (let ((class-name (class-name class)))
    (if (symbolp class-name)
	(string class-name)
	(string (second (class-name class))))))
