;;; -*- Mode: Lisp; Package: CLIM-UTILS; Syntax: Common-Lisp -*-
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

;; $fiHeader: clos.lisp,v 1.1 91/08/30 13:57:44 cer Exp Locker: cer $

;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved. 
;;; Copyright (c) 1991, Franz Inc. All rights reserved
;;; 
;;; SILICA CLOS Extensions - adaptations of CLOS to meet SILICA's needs.
;;; 

(in-package :clim-utils)

;;; ----------------
;;; Constructors
;;;
;;; PCL supports a much more efficient mechanism for initializing
;;; instances than simply calling make-instance.  We need a portable
;;; hook into that mechanism.

#+PCL
(defmacro define-constructor (name class lambda-list &rest initialization-arguments)
  `(pcl::defconstructor ,name ,class ,lambda-list ,@initialization-arguments))

#+(and PCL (not VDPCL))
(pushnew 'compile pcl::*defclass-times*)

#+Allegro-v4.0-constructors
(defmacro define-constructor (name class lambda-list &rest initialization-arguments)
  `(clos::defconstructor ,name ,class ,lambda-list ,@initialization-arguments))

#-(or PCL Allegro-v4.0-constructors)
(defmacro define-constructor (name class lambda-list &rest initialization-arguments)
  `(defun-inline ,name ,lambda-list
     (make-instance ',class ,@initialization-arguments)))

#+(and Genera PCL)
(scl:defmethod (:fasd-form #-VDPCL pcl::std-instance #+VDPCL pcl::iwmc-class) ()
  (make-load-form scl:self))



;;;
;;; Dynamic Class Creation
;;;

(defvar *dynamic-classes* (make-hash-table :test #'equal))

(defun-inline %make-standard-class (name supers)
  
  #+Lucid
  ;; Jonl thinks this is okay, but I personally find it pretty gross. -- RR
  (eval `(defclass ,name ,supers ()))
  
  #-Lucid
  ;; by which we mean PCL and Genera CLOS, at this point
  (let ((class (make-instance 'standard-class :direct-superclasses supers)))
    ;; Note that this does NOT make it so that you can find this
    ;; class with (find-class name)
    (setf (class-name class) name)
    class))

(defun find-dynamic-class (name-fn &rest supers)
  (declare (dynamic-extent supers))
  (when supers
    (do ((tail supers (cdr tail)))
	((null tail))
      (when (not #-PCL (typep (car tail) 'standard-class)
		 #+PCL (classp (car tail)))
	(setf (car tail) (find-class (car tail))))))
      
  (or (gethash supers *dynamic-classes*)
      ;;
      ;;  If there is no entry for a dynamic class with these supers
      ;;  then we have to create one.  This involves creating the class,
      ;;  setting its supers and adding the entry to *dynamic-classes*.
      ;;  
      (let ((supers (copy-list supers)))
	(setf (gethash supers *dynamic-classes*)
	      (%make-standard-class
		(intern (funcall name-fn) (find-package 'silica))
		supers)))))

(defun add-mixin (object mixin-class)
  (let ((class (class-of object)))
    (if (member mixin-class (class-precedence-list class) :test #'eq)
	(error "The class of ~S already includes ~S." object mixin-class)
	(change-class object
		      (find-dynamic-class #'(lambda () "???")
					  mixin-class class)))))

;;;
;;; DEFGENERIC ... because it isn't there.
;;;

#+ignore
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
					  (or package 'silica) t))))
      (dolist (class classes)
	(if (not (intersection (pcl::class-local-supers class)
			       classes))
	    (w::collect (class-name class)))))))


||#


