;;; -*- Base: 10; Mode: LISP; Syntax: Common-lisp; Package: (CLIM-LISP :USE LISP :COLON-MODE :EXTERNAL); Lowercase: Yes -*-
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

;; $fiHeader: defpackage.lisp,v 1.1 91/08/30 13:57:45 cer Exp Locker: cer $

#+excl (defpackage :clim-lisp)
#+excl (in-package :clim-lisp)

#-excl
(in-package :clim-lisp :use '(:lisp))

#+excl
(import 'common-lisp::defpackage (find-package :clim-lisp))

#+Genera
(shadowing-import 'future-common-lisp:defpackage (find-package :clim-lisp))

#-(or excl Genera)
(progn

(defmacro defpackage (name &body options)
  (flet ((get-option (option-name &optional default)
	   (dolist (option options default)
	     (if (atom option)
		 (when (eql option option-name) 
		   (warn "Option ~S standing along ignored." option-name))
		 (when (eql (car option) option-name) (return (cdr option))))))
	 (stringify (option-arg) (mapcar #'string option-arg))
	 (map-options (option-name function)
	   (let ((result nil))
	     (dolist (option options (nreverse result))
	       (if (atom option)
		   (when (eql option option-name)
		     (warn "Option ~S standing alone ignored." option-name))
		   (when (eql (car option) option-name)
		     (push (funcall function (cdr option)) result)))))))
    (let ((package-name (string name))
	  (use-list (get-option :use '(:lisp)))
	  (nicknames (get-option :nicknames))
	  #+Genera (prefix-name (car (get-option :prefix-name)))
	  (size (get-option :size))
	  (package-var (make-symbol "PACKAGE")))
      (let ((bad-option
	      (find-if-not #'(lambda (opt)
			       (lisp:member opt '(:use :nicknames :size #+Genera :prefix-name
						  :shadow :shadowing-import-from
						  :import-from :internal :export)))
			   options
			   :key #'first)))
	(when bad-option
	  (warn "Unknown ~S option: ~S" 'defpackage (first bad-option))))
      `(eval-when (compile load eval)
	 (let ((,package-var (make-package-aux
			       ',package-name :use nil
			       ,@(when nicknames `(:nicknames ',(mapcar #'string nicknames)))
			       #+Genera
			       ,@(when prefix-name `(:prefix-name ',prefix-name))
			       ,@(when size `(:size ',(car size))))))
	   ,@(map-options 
	       :shadow
	       #'(lambda (option-arg) `(shadow ',option-arg ,package-var)))
	   ,@(map-options
	       :shadowing-import-from
	       #'(lambda (option-arg)
		   `(shadowing-import-from ',(string (car option-arg))
					   ',(stringify (cdr option-arg)) ,package-var)))
	   ,@(map-options
	       :import-from
	       #'(lambda (option-arg)
		   `(import-from ',(string (car option-arg)) ',(stringify (cdr option-arg))
				 ,package-var)))
	   ,@(map-options
	       :internal
	       #'(lambda (option-arg)
		   `(progn ,@(mapcar #'(lambda (x) `(intern ',(string x) ,package-var))
				     option-arg))))
	   ,@(when use-list `((use-package ',(stringify use-list) ,package-var)))
	   ,@(map-options
	       :export
	       #'(lambda (option-arg)
		   `(export-internal ',option-arg ,package-var)
		   #+ignore
		   `(progn ,@(mapcar #'(lambda (x)
					 `(export (list (intern ',x ,package-var)) ,package-var))
				     (stringify option-arg)))))

	   ,package-var)))))

(defun make-package-aux (package-name &rest args &key use nicknames #+Genera prefix-name size)
  (let ((pkg (find-package package-name)))
      (if pkg
	  (fix-up-package pkg use nicknames size #+Genera prefix-name)
	(setf pkg (apply #'make-package package-name :use use args)))
      pkg))

(defun fix-up-package (pkg use-list nicknames size #+Genera prefix-name)
  (declare (ignore size))     ;Perhaps there is something non-standard we could do.
  (unuse-package (package-use-list pkg) pkg)
  (use-package use-list pkg)
  (rename-package pkg (package-name pkg) nicknames)	;Remove old nicknames, if any.
  #+Genera (when prefix-name (setf (si:pkg-prefix-name pkg) prefix-name))
  pkg)

;;; Only Symbolics' find-package might return the package itself.
(defun find-package-internal (pkg)
  (if (typep pkg 'package) pkg
      (find-package pkg)))

;;; Keep proper semantics of package operations: Do it to a list, and it keeps the
;;; list-ness when passing it on to the underlying package operation.  Thus, EXPORTing
;;; '(NIL), for example, will do what you expect.
(defun perform-package-operation (operation symbol-names from-package into-package)
  (flet ((intern-symbol (name)
	   (if from-package
	       (intern (string name) from-package)
	       (string name))))
    (setf symbol-names
	    (if (listp symbol-names)
	      (mapcar #'intern-symbol symbol-names)
	      (intern-symbol symbol-names))))
  (funcall operation symbol-names into-package))

(defun shadowing-import-from (from-package symbol-names &optional (into-package *package*))
  (let ((true-from-package (find-package-internal from-package))
	(true-into-package (find-package-internal into-package)))
    (when (and from-package (null true-from-package))
      (error "Package ~A does not exist; cannot import symbols from it." from-package))
    (when (and into-package (null true-into-package))
      (error "Package ~A does not exist; cannot import symbols into it." into-package))
    (perform-package-operation #'shadowing-import symbol-names
			       true-from-package true-into-package)))

(defun import-from (from-package symbol-names &optional (into-package *package*))
  (let ((true-from-package (find-package-internal from-package))
	(true-into-package (find-package-internal into-package)))
    (when (and from-package (null true-from-package))
      (error "Package ~A does not exist; cannot import symbols from it." from-package))
    (when (and into-package (null true-into-package))
      (error "Package ~A does not exist; cannot import symbols into it." into-package))
    (perform-package-operation #'import symbol-names true-from-package true-into-package)))

(defun export-internal (symbol-names &optional (from-package *package*))
  (let ((true-from-package (find-package-internal from-package)))
    (when (and from-package (null true-from-package))
      (error "Package ~A does not exist; cannot export symbols from it." from-package))
    (perform-package-operation #'export symbol-names true-from-package true-from-package)))

)
