;; -*- mode: common-lisp; package: user -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: dev-load-1.lisp,v 1.18 92/11/05 17:15:57 cer Exp $

;;;; This should not matter
;;;; (setq *ignore-package-name-case* t)

(set-case-mode :case-insensitive-lower)

(tenuring
   (let ((*load-source-file-info* t)
	 (*load-xref-info* nil))
     (let ((*enable-package-locked-errors* nil))
       (load "sys/defsystem"))
     (load "sys/sysdcl")))

(defun load-it (sys)
  (let ((*load-source-file-info* t)
	(*load-xref-info* nil)
	(excl:*global-gc-behavior* nil))
  
    (tenuring 
     (ecase sys
       (motif-clim
	(load "climg.fasl")
	(load "climxm.fasl")
	(load "clim-debug.fasl")
	(load "clim-debugxm.fasl"))
       (openlook-clim
	(load "climg.fasl")
	(load "climol.fasl")
	(load "clim-debug.fasl")
	(load "clim-debugol.fasl")))

     ;;-- What would be good is to mark the files in the system as having
     ;;-- been loaded


     (load "postscript/sysdcl")
  
     (load "climps.fasl")

     (compile-file-if-needed "test/test-suite")


     (load "test/test-suite")

     (load "demo/sysdcl")

     (clim-defsys::load-system 'clim-demo)

     (when (probe-file "/usr/tech/cer/stuff/climtoys/sysdcl.lisp")
       (load "/usr/tech/cer/stuff/climtoys/sysdcl.lisp")
       (tenuring (clim-defsys::load-system 'clim-toys)))
     (ignore-errors (require :composer))

   
     (ignore-errors
      (load (case sys
	      (motif-clim "misc/clos-preloadxm.fasl")
	      (openlook-clim "misc/clos-preloadol"))
	    :if-does-not-exist nil)))))
