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

;; $fiHeader: defun.lisp,v 1.1 91/08/30 13:57:46 cer Exp Locker: cer $

(in-package :clim-utils)

"Copyright (c) 1989, 1990 International Lisp Associates.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

;;;
;;; DEFUN and friends which support implementation-specific ways of declaring that
;;; various storage can be allocated on the stack.  Currently handles &REST
;;; arguments, closure arguments and functions which are to be passed as arguments
;;; to functions which are known not to store their arguments.  Under Genera, this
;;; generates DYNAMIC-EXTENT and SYS:DOWNWARD-FUNCTION declarations.  If you
;;; don't declare a &REST argument to have dynamic extent, it will be copied (thus
;;; preserving CL semantics).
;;;
;;; We continue to support &DOWNWARD-FUNARG and &DOWNWARD-REST lambda-list
;;; keywords, but their use draws a warning concerning their obsolescence.
;;;
;;; Example:
;;; (defun test (ignore ignore ignore continuation &rest args)
;;;   (declare (dynamic-extent continuation args))
;;;   "This is a test of dynamic-extent"
;;;   "This is a second documentation string"
;;;   ;; Make sure declarations work before and after documentation strings:
;;;   (declare (list args))
;;;   (flet ((continuator (new-continuation &rest args)
;;;	       (declare (dynamic-extent new-continuation))
;;;	       (list* (apply new-continuation continuation) args))
;;;	     (continuate (arg1 &rest args)
;;;	       (declare (dynamic-extent args))
;;;	       (member arg1 args)))
;;;	(declare (dynamic-extent #'continuate))
;;;	(apply continuation args)))
;;;*** The following warning issued only under Genera (for now):
;;; ==> Warning: The rest argument ARGS will be copied at runtime because it is not declared 
;;;		 to have dynamic extent.
;;; (lisp:defun test (#:ignore-4 #:ignore-5 #:ignore-6 continuation &rest args)
;;;   "This is a test of dynamic-extent"   ;; [Should we preserve ALL doc strings??]
;;;   (declare (list args))
;;;   (declare (ignore #:ignore-6 #:ignore-5 #:ignore-4)
;;;	       (dynamic-extent continuation))
;;;   (lisp:flet ((continuator (new-continuation &rest args)
;;;		    (declare (dynamic-extent new-continuation))
;;;		    (setq args (copy-list args))
;;;		    (list* (apply new-continuation continuation) args))
;;;		  (continuate (arg1 &rest args)
;;;		    (member arg1 args)))
;;;     (declare (dynamic-extent #'continuator #'continuate))
;;;	(apply continuation args)))
;;;


;;; What does #' actually read as?  Because of how packages and syntaxes interact
;;; in Genera, (FUNCTION CAR) and #'CAR might not be EQUAL if the package and the
;;; syntax don't match up.  When we no longer need to support Genera 8.0 and 8.1,
;;; then we can change the syntax of all the files to ANSI-Common-Lisp and get
;;; rid of this kludge.
(defvar *function-symbol* #+Genera (first '#'car) #-Genera 'function)

;;; The heart of function body processing:
(lisp:defun decode-function (lambda-list body environment
			     &key clos-method-p function-name downward-p generic-function-p)
  (declare (values new-lambda-list new-body))
  #-Genera (declare (ignore function-name))
  (let* ((ignores nil)
	 (new-lambda-list nil)
	 (lambda-list-state '#1=#:required)
	 #+Genera (compiler:default-warning-function
		    (or function-name compiler:default-warning-function))
	 (remaining-lambda-list-keys (list* '#1# '(&optional &rest &key &aux)))
	 (rests nil) non-dynamic-rests arguments
	 (dynamic-extent-vars nil) (dynamic-extent-functions nil)
	 (non-dynamic-vars nil)
	 (new-declarations nil))
    (dolist (element lambda-list)
      (let ((where-we-are (member element remaining-lambda-list-keys)))
	(cond (where-we-are
	       (setf lambda-list-state element remaining-lambda-list-keys where-we-are))
	      ;; Handle obsolete keywords:
	      ((eql element '&downward-rest)
	       (let ((var (cadr (member element lambda-list))))
		 (push var dynamic-extent-vars) (push var rests)
		 (warn "~S is obsolete: use ~S" '&downward-rest
		       `(declare (dynamic-extent ,var))))
	       (setf lambda-list-state '&rest element '&rest))
	      ((eql element '&downward-funarg)
	       (let ((var (cadr (member element lambda-list))))
		 (push var dynamic-extent-functions)
		 (warn "~S is obsolete: use ~S" '&downward-funarg
		       `(declare (dynamic-extent ,var))))
	       (setf element '#1#))
	      ((and (member element lambda-list-keywords)
		    (not (and (eql element '&allow-other-keys)
			      (eql lambda-list-state '&key))))
	       (warn "Keyword ~A is invalid in this position in the lambda-list~%~S"
		     element lambda-list))
	      (t (let ((var-name (ecase lambda-list-state
				   (#1#		; Required argument
				    (if (and clos-method-p (listp element))
					(first element)
					element))
				   ((&optional &aux)
				    (if (listp element) (first element) element))
				   (&rest element)
				   (&key (if (listp element)
					     (if (listp (first element))
						 (second (first element))
						 (first element))
					     element)))))
		   (push var-name arguments)
		   (when (and (symbolp var-name)
			      (member var-name '(ignore ignored) :test #'string=))
		     (let ((ignored-var (gensymbol 'ignore)))
		       (setf element (subst ignored-var var-name element))
		       (push ignored-var ignores)
		       (when (eq lambda-list-state '&rest)
			 (push ignored-var dynamic-extent-vars))))
		   (when (eq lambda-list-state '&rest)
		     (push element rests))))))
      (unless (eql element '#1#) (push element new-lambda-list)))
    (multiple-value-bind (documentation declarations new-body)
	(extract-declarations body environment)
      (dolist (declare declarations)
	(dolist (dcl-form (cdr declare))
	  (case (first dcl-form)
	    (dynamic-extent
	      (macrolet ((make-dynamic (var)
			   `(let ((value ,var))
			      (if (member value arguments)
				  (pushnew value dynamic-extent-vars)
				  (warn "Variable ~S declared ~S, but it's not an argument."
					value 'dynamic-extent)))))
		(dolist (de (rest dcl-form))
		  (typecase de
		    (symbol (make-dynamic de))
		    (list (if (and (null (cddr de)) (eql (first de) *function-symbol*))
			      (progn
				(warn "You probably meant ~S intead of ~S"
				      `(declare (dynamic-extent ,(second de)))
				      `(declare (dynamic-extent ,de)))
				(make-dynamic (second de)))
			      (warn "Syntax error: ~S declaration for ~S"
				    'dynamic-extent de)))))
		(setf declare (remove dcl-form declare))))
	    (non-dynamic-extent ;; Suppress the warning for &REST NON-DYNAMIC-VAR
	      (setf non-dynamic-vars (append non-dynamic-vars (rest dcl-form)))
	      (setf declare (remove dcl-form declare)))
	    (ignore (dolist (ignored-var (rest dcl-form))
		      (when (member ignored-var rests)
			(push ignored-var dynamic-extent-vars))))))
	(unless (null (cdr declare))
	  (push declare new-declarations)))
      (setf new-lambda-list (nreverse new-lambda-list)
	    dynamic-extent-functions (append dynamic-extent-functions
					     (set-difference dynamic-extent-vars rests))
	    dynamic-extent-vars (set-difference dynamic-extent-vars
						dynamic-extent-functions)
	    non-dynamic-rests (unless generic-function-p
				(set-difference rests dynamic-extent-vars)))
      (values new-lambda-list
	      ;; New body:
	      `(,@(when documentation `(,documentation))
		,@(nreverse new-declarations)
		,@(let
		    ((decl-stuff
		       `(,@(when ignores `((ignore ,@ignores)))
			 ,@(when downward-p
			     (generate-downward-function-declarations))
			 ,@(when dynamic-extent-functions
			     (generate-downward-funarg-declarations dynamic-extent-functions))
			 ,@(when dynamic-extent-vars
			     (generate-downward-rest-declarations dynamic-extent-vars)))))
		    (when decl-stuff
		      `((declare ,@decl-stuff))))
		,@(when non-dynamic-rests
		    (generate-rest-code non-dynamic-rests non-dynamic-vars))
		,@new-body)))))

(lisp:defun generate-downward-function-declarations ()
  `(#+(or Genera CLOE-Runtime) (sys:downward-function)))

(lisp:defun generate-downward-funarg-declarations (args)
  #-(or Genera CLOE-Runtime) (declare (ignore args))
  #+(or Genera CLOE-Runtime)
  `(#+(or Genera-Release-8-0 CLOE-Runtime) (sys:downward-funarg ,@args)
    #+(and Genera (not Genera-Release-8-0)) (dynamic-extent ,@args)))

(lisp:defun generate-downward-rest-declarations (args)
  #+(or Genera Cloe-Runtime) (declare (ignore args))
  `(#+Cloe-Runtime (sys:downward-rest-argument)
    #+(or excl lucid) (dynamic-extent ,@args)))

#+Genera (defparameter *warn-about-copied-rest-args* t)

(lisp:defun generate-rest-code (args non-dynamic-vars)
  #-Genera (declare (ignore args non-dynamic-vars))
  #+Genera
  (let ((code nil)
	(warn (not (null (set-difference args non-dynamic-vars)))))
    (dolist (arg args)
      (push `(setf ,arg (copy-list ,arg)) code))
    (when (and warn *warn-about-copied-rest-args*)
      (let ((singular-p (null (cdr args))))
	(warn "The rest argument~:[s~] ~{~S~^, ~} will be copied at runtime ~
	     because ~:[they are~;it is~] not declared to have dynamic extent."
	      singular-p args singular-p)))
    (nreverse code)))

(defmacro with-new-function ((ll-var body-var)
			     (lambda-list orig-body
					  &key environment clos-method-p function-name
					  downward-p)  
			     &body body)
  #+Genera (declare (zwei:indentation 0 3 1 3 2 1))
  `(multiple-value-bind (,ll-var ,body-var)
       (decode-function ,lambda-list ,orig-body ,environment
			:clos-method-p ,clos-method-p :function-name ,function-name
			:downward-p ,downward-p)
     ,@body))

;;; DEFUN: simple use of above.

(defmacro defun (name lambda-list &body body &environment env)
  (with-new-function (ll b) (lambda-list body :environment env :function-name name)
    `(lisp:defun ,name ,ll ,@b)))

#+Genera
(progn
  (setf (get 'defun 'zwei:definition-function-spec-parser)
	#'(:property cl:defun zwei:definition-function-spec-parser))
  (setf (get 'defun 'zwei:definition-function-spec-type) 'cl:defun)
  (setf (get 'defun 'gprint::formatter) #'(:property cl:defun gprint::formatter))
  (push 'defun zwei:*irrelevant-functions*)
  (push 'defun zwei:*irrelevant-defining-forms*))


;;; FLET and LABELS.  Process (declare (dynamic-extent #'FOO)) by putting, e.g.,
;;; (declare (sys:downward-function)) inside FOO's body.
(lisp:defun construct-local-function-body (operator functions body environment)
  (multiple-value-bind (documentation declarations new-body)
      (extract-declarations body environment)
    (let ((new-functions nil)
	  (downward-functions
	    (let ((result nil))
	      (dolist (declaration declarations)
		(dolist (decl-form (cdr declaration))
		  (when (eql (first decl-form) 'dynamic-extent)
		    (dolist (de (cdr decl-form))
		      (if (and (listp de) (null (cddr de))
			       (eql (first de) *function-symbol*))
			  (let ((downward-name (second de)))
			    (if (assoc downward-name functions)
				(push downward-name result)
				(warn "Function ~S declared to have dynamic extent, but not ~
				       defined in this ~A form" downward-name operator)))
			  (warn "Syntax error in ~A: invalid ~S declaration ~S"
				operator 'dynamic-extent de)))
		    ;; Get rid of the errant declaration
		    (setf declaration (delete decl-form declaration))))
		(when (null (cdr declaration))
		  (setf declarations (delete declaration declarations))))
	      result)))
      (dolist (function functions)
	(let* ((name (pop function))
	       (downward-p (not (null (member name downward-functions))))
	       (lambda-list (pop function)))
	  (with-new-function (ll b)
	      (lambda-list function :environment environment
				    :downward-p downward-p)
	    (push `(,name ,ll ,@b) new-functions))))
      `(,operator ,(nreverse new-functions) ,@declarations
	,@(generate-dynamic-function-declarations downward-functions)
	,@documentation ,@new-body))))

(lisp:defun generate-dynamic-function-declarations (functions)
  #-Ignore (declare (ignore functions))
  #+Ignore `((declare (dynamic-extent ,@(mapcar #'(lambda (fn) `#',fn) functions)))))

(defmacro flet (functions &body body &environment env)
  (construct-local-function-body 'lisp:flet functions body env))

#+Genera
(pushnew 'flet zwei:*definition-list-functions*)

(defmacro labels (functions &body body &environment env)
  (construct-local-function-body 'lisp:labels functions body env))

#+Genera
(pushnew 'labels zwei:*definition-list-functions*)


(defparameter *defgeneric* #+PCL 'pcl::defgeneric
			   #+excl 'clos::defgeneric
			   #-(or excl PCL) 'clos:defgeneric)

(defmacro defgeneric (name lambda-list &body options &environment env)
  (multiple-value-bind (new-ll new-body)
      (decode-function lambda-list (remove 'declare options :test-not #'eq :key #'first) env
		       :function-name name :generic-function-p t)
    `(,*defgeneric* ,name ,new-ll
      ,@(remove 'declare new-body :test-not #'eq :key #'first)
      ,@(remove 'declare options :key #'first))))


;;; DEFMETHOD needs to handle the DYNAMIC-EXTENT declaration, too.
(defparameter *defmethod* #+PCL 'pcl::defmethod
			  #+excl 'clos::defmethod
			  #-(or excl PCL) 'clos:defmethod)

(defmacro defmethod (&whole form name &rest args &environment env)
  (declare (arglist name {method-qualifier}* specialized-lambda-list &body body)
	   #+Genera (zwei:indentation . #-PCL zwei:indent-for-clos-defmethod
					#+PCL pcl::indent-clos-defmethod))
  ;; How many times do we have to write parsers for DEFMETHOD?
  (let (qualifiers
	(lambda-list :invalid))
    (loop (when (null args) (return))
	  (let ((possible-qualifier (pop args)))
	    (when (listp possible-qualifier)
	      (setf lambda-list possible-qualifier
		    qualifiers (nreverse qualifiers))
	      (return))
	    (push possible-qualifier qualifiers)))
    (when (eql lambda-list :invalid)
      (warn "No valid lambda-list found in this form: ~S" form)
      ;; Don't blow up; I don't know what else to say.
      (setf lambda-list nil))
    (with-new-function (ll b)
	(lambda-list args :clos-method-p t
			  :environment env
			  :function-name (make-method-name name qualifiers lambda-list))
      `(,*defmethod* ,name ,@qualifiers ,ll ,@b))))

(lisp:defun make-method-name (function-name qualifiers lambda-list)
  (let* ((specifier-list 
	   (do ((ll lambda-list (cdr ll))
		(result nil))
	       ((null ll) (nreverse result))
	     (let* ((arg (first ll))
		    (type (if (consp arg) (second arg) 't)))
	       (when (member arg `(&downward-rest &downward-funarg ,@lambda-list-keywords))
		 (return (nreverse result)))
	       (push type result)))))
    #+PCL `(pcl::method ,function-name ,@qualifiers ,specifier-list)
    #-PCL `(clos:method ,function-name ,specifier-list ,@qualifiers)))

#+(and excl (version>= 4 1))
(eval-when (compile load eval) (cltl1::require :scm))
#+(and excl (version>= 4 1))
(scm::define-simple-parser defmethod scm::defmethod-parser)

#+Genera
(progn
  (setf (get 'defmethod 'zwei:definition-function-spec-parser)
	(get *defmethod* 'zwei:definition-function-spec-parser))
  #-PCL
  (setf (get 'defmethod 'gprint::formatter) #'(:property clos:defmethod gprint::formatter))
  (setf (get 'defmethod 'zwei:definition-function-spec-type) 'cl:defun)
  (pushnew 'defmethod zwei:*irrelevant-functions*)
  (pushnew 'defmethod zwei:*irrelevant-defining-forms*)
  #+Genera
  (pushnew 'defmethod zwei:*forms-that-define-things-with-names-that-are-symbols*))

