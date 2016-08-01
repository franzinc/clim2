;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved.
;;;
;;; SILICA CLOS Extensions - adaptations of CLOS to meet SILICA's needs.
;;;

(in-package :clim-utils)

#+Allegro-v4.0-constructors
(eval-when (compile eval load) (require :constructor))

;;; ----------------
;;; Constructors
;;;
;;; PCL supports a much more efficient mechanism for initializing
;;; instances than simply calling MAKE-INSTANCE.  We need a portable
;;; hook into that mechanism.

#+PCL
(defmacro define-constructor (name class lambda-list &body initargs)
  `(pcl::defconstructor ,name ,class ,lambda-list ,@initargs))

#+(and PCL (not VDPCL))
(pushnew 'compile pcl::*defclass-times*)

#+allegro-v4.0-constructors
(defmacro define-constructor (name class lambda-list &body initargs)
  `(excl::defconstructor ,name ,class ,lambda-list ,@initargs))

#-(or PCL allegro-v4.0-constructors)
;; NB: any &REST argument is declared to have dynamic extent!
(defmacro define-constructor (name class lambda-list &body initargs)
  (let ((rest-arg (member '&rest lambda-list)))
    `(progn
       (eval-when (compile load eval) (proclaim '(inline ,name)))
       (defun ,name ,lambda-list
	 ,@(when rest-arg
	     `((declare (dynamic-extent ,(second rest-arg)))))
	 (make-instance ',class ,@initargs)))))

#+(and Genera PCL)
(scl:defmethod (:fasd-form #-VDPCL pcl::std-instance #+VDPCL pcl::iwmc-class) ()
  (make-load-form scl:self))


;; DEFINE-CONSTRUCTOR-USING-PROTOTYPE-INSTANCE can be used when creating an
;; instance can be copied from another instance of the same type, the init
;; args exist solely to initialize slots, and any possible
;; INITIALIZE-INSTANCE or SHARED-INITIALIZE methods don't do anything that
;; would prevent a valid create by copying and setting.  This is true of
;; graphics output records and text output records.

;; The syntax is a little bizarre in order to avoid having to do FIND-CLASS
;; at compile time and depend on that working.  Each tuple consists of three
;; required components: the slot name is the first element, the initarg is
;; the second element (in a form that is evaluated, in the event it isn't a
;; keyword), and the value is the third element, again, in a form that is
;; evaluated.  The "optional" fourth argument is the value that should be used
;; in the slot in the prototype instance.  For example,
;;   (define-constructor-using-prototype-instance
;;     make-foo foo (up over)
;;     (slot-1 :slot-1 up)
;;     (slot-2 'slot-2 (1+ over)))
;; will initialize SLOT-1 with the value of UP, initializing it either using
;; SLOT-VALUE with 'SLOT-1 or MAKE-INSTANCE with :SLOT-1, and will
;; initialize SLOT-2 with value of (1+ OVER) using SLOT-VALUE with 'SLOT-2
;; or MAKE-INSTANCE with 'SLOT-2.

#+CCL-2
(defmacro define-constructor-using-prototype-instance
	  (name class args &body tuples)
  (let ((proto-var (make-symbol (format nil "*a ~A*" class)))
	(proto '#:prototype-instance)
	(inst  '#:instance))
    `(define-group ,name define-constructor-using-prototype-instance
       (defvar ,proto-var nil)
       (defun ,name (,@args)
	 (let* ((,proto (or ,proto-var
			     (setq ,proto-var (make-instance ',class))))
		(,inst (without-scheduling
			  (ccl::copy-uvector
			    (ccl::%maybe-forwarded-instance ,proto)))))
	   ,@(mapcar #'(lambda (tuple)
			 `(setf (slot-value ,inst ',(first tuple)) ,(third tuple)))
		     tuples)
	   ,inst)))))

#-CCL-2
(defmacro define-constructor-using-prototype-instance
	  (name class args &body tuples)
  `(define-group ,name define-constructor-using-prototype-instance
     (define-constructor ,name ,class ,args
       ,@(mapcan #'(lambda (tuple)
		     (list (second tuple) (third tuple)))
		 tuples))))



;;;
;;; Dynamic Class Creation
;;;

;(defvar *dynamic-classes* (make-hash-table :test #'equal))
;
;(eval-when (compile load eval) (proclaim '(inline %make-standard-class)))
;(defun %make-standard-class (name supers)
;
;  #+Lucid
;  ;; Jonl thinks this is okay, but I personally find it pretty gross. -- RR
;  (eval `(defclass ,name ,supers ()))
;
;  #-Lucid
;  ;; by which we mean PCL and Genera CLOS, at this point
;  (let ((class (make-instance 'standard-class :direct-superclasses supers)))
;    ;; Note that this does NOT make it so that you can find this
;    ;; class with (find-class name)
;    (setf (class-name class) name)
;    class))
;
;(defun find-dynamic-class (name-fn &rest supers)
;  (declare (dynamic-extent supers))
;  (when supers
;    (do ((tail supers (cdr tail)))
;	((null tail))
;      (when (not #-PCL (typep (car tail) 'standard-class)
;		 #+PCL (classp (car tail)))
;	(setf (car tail) (find-class (car tail))))))
;
;  (or (gethash supers *dynamic-classes*)
;      ;;
;      ;;  If there is no entry for a dynamic class with these supers
;      ;;  then we have to create one.  This involves creating the class,
;      ;;  setting its supers and adding the entry to *dynamic-classes*.
;      ;;
;      (let ((supers (copy-list supers)))
;	(setf (gethash supers *dynamic-classes*)
;	      (%make-standard-class
;		(intern (funcall name-fn) (find-package :silica))
;		supers)))))
;
;(defun add-mixin (object mixin-class)
;  (let ((class (class-of object)))
;    (if (member mixin-class (class-precedence-list class) :test #'eq)
;	(error "The class of ~S already includes ~S." object mixin-class)
;	(change-class object
;		      (find-dynamic-class #'(lambda () "???")
;					  mixin-class class)))))

;;;
;;; DEFGENERIC ... because it isn't there.
;;;

#+++ignore
(defmacro defgeneric (function-specifier lambda-list &rest options)
  (declare (ignore lambda-list))
  (let ((expansion nil)
	(setfp (and (consp function-specifier)
		    (eq (car function-specifier) 'setf)))
	(methods nil)
	(docstring nil))
    (when setfp
      (push `(pcl::do-standard-defsetf ,(cadr function-specifier))
	    expansion))
    (dolist (option options)
      (case (car option)
	(:documentation (setq docstring (second option)))
	(:method
	 (push `(defmethod ,function-specifier ,@(cdr option))
	       methods))))
    (when docstring
      (if setfp
	  (push `(setf (documentation ',(cadr function-specifier) 'setf)
		       ',docstring)
		expansion)
	  (push `(setf (documentation ',function-specifier 'function)
		       ',docstring)
		expansion)))
    `(progn ,@expansion ,@methods)))


#||

;;;
;;; Sundries..
;;;

(defun classes-in-package (package &optional map-on-package)
  (let ((classes nil))
    (unless (packagep package)
      (setq package (find-package package)))
    (if map-on-package
	(do-symbols (sym package)
	  (if (and (eq (symbol-package sym) package)
		   (find-class sym nil))
	      (push sym classes)))
	(maphash #'(lambda (key val)
		     (declare (ignore val))
		     (if (eq (symbol-package key)
			     package)
			 (push key classes)))
		 pcl::*find-class*))
    classes))

(defun collect-root-classes (&optional package)
  (w::with-collection
    (let ((classes (mapcar #'find-class (classes-in-package
					  (or package :silica) t))))
      (dolist (class classes)
	(if (not (intersection (pcl::class-local-supers class)
			       classes))
	    (w::collect (class-name class)))))))


||#


;;; Multiple value SETFs, aka SETF*

(defun make-setf-function-name (accessor-name)
  (values `(setf ,accessor-name) t))

(defun make-setf*-function-name (accessor-name)
  (declare (values setf-function-name defsetf-done-p))
  (let ((writer (get accessor-name 'setf-function-name))
	(old-p nil))
    (when writer
      (ignore-errors
	(multiple-value-bind (vars vals store-vars store-form access-form)
	    (lisp:get-setf-expansion `(,accessor-name foo))
	  (declare (ignore vars vals store-vars access-form))
	  (when (or (equal (first store-form) writer)
		    (and (eq (first store-form) 'funcall)
			 (eq (first (second store-form)) 'function)
			 (equal (second (second store-form)) writer)))
	    (setf old-p t))))
      (return-from make-setf*-function-name (values writer old-p)))
    (values (setf (get accessor-name 'setf-function-name)
		  (package-fintern (find-package 'clim-utils)
				   "~A ~A:~A"
				   'setf*
				   (package-name 
				    (symbol-package accessor-name))
				   accessor-name))
	    nil)))

(defun expand-defsetf-for-defmethod*
       (accessor-name accessor-arg real-arglist setf-function-name)
  `(lisp:define-setf-expander
     ,accessor-name (,accessor-arg)	;Only last one is real.
     (flet ((make-temp (name) (gensymbol name 'temp)))
       (let ((temps (list (make-temp ',accessor-arg)))
	     (store-temps (mapcar #'make-temp ',(butlast real-arglist))))
	 (values temps (list ,accessor-arg) store-temps
		 `(funcall #',',setf-function-name ,@store-temps ,@temps)
		 `(,',accessor-name ,@temps))))))

;; For example, (DEFGENERIC* (SETF CURSOR-POSITION) (X Y CURSOR))
(defmacro defgeneric* (function-spec lambda-list &body options)
  (assert (and (listp function-spec)
	       (eq (first function-spec) 'setf)
	       (null (cddr function-spec)))
	  ()
	  "Syntax error in ~S: This only works on ~S generic functions" 'defgeneric* 'setf)
  (let* ((accessor-name (second function-spec))
	 (accessor-arg (first (last lambda-list)))
	 (setf-function-name (make-setf*-function-name accessor-name)))
    `(define-group ,function-spec defgeneric*
       (defgeneric ,(make-setf*-function-name accessor-name) ,lambda-list ,@options)
       ,(expand-defsetf-for-defmethod* accessor-name accessor-arg
				       lambda-list setf-function-name))))

;; For example, (DEFMETHOD* (SETF CURSOR-POSITION) (NX NY (CURSOR T)) ...)
;; Then (SETF (CURSOR-POSITION cursor) (VALUES nx ny))
(defmacro defmethod* (name &body quals-lambda-list-and-body)
  (declare (arglist name [qualifiers]* lambda-list &body body))
  #+Genera (declare (zwei:indentation . zwei:indent-for-clos-defmethod))
  (assert (and (listp name) (eq (first name) 'setf) (null (cddr name))) ()
	  "Syntax error in ~S: This only works on ~S methods" 'defmethod* 'setf)
  (let (qualifiers real-arglist body accessor-arg
	(accessor-name (second name)))
    (multiple-value-bind (setf-function-name old-p)
	(make-setf*-function-name accessor-name)
      (do ((qllab quals-lambda-list-and-body (cdr qllab)))
	  ((not (symbolp (first qllab)))
	   (setf qualifiers (nreverse qualifiers)
		 real-arglist (first qllab)
		 accessor-arg (let ((arg (first (last real-arglist))))
				(if (listp arg) (first arg) arg))
		 body (cdr qllab)))
	(push (first qllab) qualifiers))
      `(progn ,(unless old-p			;Don't write same SETF method again.
		 (expand-defsetf-for-defmethod* accessor-name accessor-arg
						real-arglist setf-function-name))
	      (defmethod ,setf-function-name ,@qualifiers ,real-arglist ,@body)))))
