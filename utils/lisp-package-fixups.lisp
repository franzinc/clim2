;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10 -*-

;; $fiHeader: lisp-package-fixups.lisp,v 1.3 92/02/16 20:55:12 cer Exp $

#+ANSI-90 (in-package :cl-user)

#-ANSI-90 (in-package :user)

"Copyright (c) 1991 by International Lisp Associates.  All Rights Reserved."

;;; The point of this file is to give CLIM a consistent naming scheme
;;; for the LISP and USER packages, no matter what they are actually
;;; called in any given Lisp environment.

;;; These two forms should be merged into one facility that does the
;;; right thing, but I am trying to make the minimal change since it
;;; is shortly before the MCL CLIM release.  The two forms were both
;;; loaded as part of building CLIM before I moved them into this
;;; file, so they ought to work as well together in this file as they
;;; did separately in defsystem and sysdcl.
;;;
;;; I only moved them at all so that the magic they do to packages did
;;; not rely on having the "extra-CLIM" files DEFSYSTEM and SYSDCL
;;; loaded in order to load or run CLIM.  That is, I moved the forms
;;; from files that are loaded only to compile/build CLIM into a file
;;; that is actually part of CLIM.

;;; *** A temporary workaround, easier than fixing all references to
;;; *** LISP:<foo>.  --RWK 20.Nov.90
;;; Copied here from defsystem.lisp by York, 5 June 91.
;;; The point of this is to add all the common old aliases for
;;; the ANSI-named COMMON-LISP and COMMON-LISP-USER packages.
;;; I left a copy of this form in defsystem.lisp so that defsystem
;;; can be compiled (it has lots of references to LISP: symbols).
#+(and ANSI-90 (not Allegro))
(eval-when (eval compile load)
  (flet ((fix-package (pack-name add-name)
	   (setq add-name (string add-name))
	   (let ((pack (find-package pack-name)))
	     (assert (not (null pack)) ()
		     "Attempting to add the name ~S to package ~S, which doesn't exist")
	     (when (null (find-package add-name))
	       (let (#+Allegro (excl::*enable-package-locked-errors* nil))
		 (rename-package 
		   pack (package-name pack)
		   (list* add-name (package-nicknames pack))))))))
    (fix-package "COMMON-LISP" "LISP")
    (fix-package "COMMON-LISP" "CL") ;; ??
    (fix-package "COMMON-LISP-USER" "USER")
    (fix-package "COMMON-LISP-USER" "CL-USER")) ;; ??
  (when (null (find-package :system))
    (defpackage system))
)	;eval-when

;;; Moved here from sysdcl.lisp, York, 5 June 91 The point of this one
;;; is really to add the ANSI names COMMON-LISP and COMMON-LISP-USER
;;; to the LISP and USER packages found in non-ANSI Lisps.
#-Allegro
(eval-when (compile load eval)
  (flet ((clean-up-package (old-package-name primary-name required-nicknames)
	   (let* ((package (or (find-package old-package-name)
			       (find-package primary-name)))
		  (old-name (package-name package))
		  (old-nicknames (package-nicknames package)))
	     (let (#+Allegro (excl::*enable-package-locked-errors* nil))
	       (rename-package 
		 package primary-name
		 (remove primary-name
			 (union (cons old-name old-nicknames) required-nicknames
				:test #'string-equal)
			 :test #'string-equal))))))
    (clean-up-package "LISP" "COMMON-LISP" '("CL"))
    (clean-up-package "USER" "COMMON-LISP-USER" '("CL-USER")))
  )	;eval-when
