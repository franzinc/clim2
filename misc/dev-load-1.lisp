;; -*- mode: common-lisp; package: user -*-
;;
;;				-[Wed Sep 15 18:09:43 1993 by duane]-
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
;; $fiHeader: dev-load-1.lisp,v 1.31 1993/07/27 01:47:51 colin Exp $

;;;; This should not matter
;;;; (setq *ignore-package-name-case* t)

(set-case-mode :case-insensitive-lower)

(tenuring
 (let ((*load-source-file-info* t)
       (*record-source-file-info* t)
       (*load-xref-info* nil))
     (let ((*enable-package-locked-errors* nil))
       (load "sys/defsystem"))
     (load "sys/sysdcl")))

(defun load-it (sys &key (load-composer t))
  (let ((excl::*update-entry-points* nil)
	(*load-source-file-info* t)
	(*record-source-file-info* t)
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

     (tenuring
      (load "climps.fasl")
      (load "climhpgl.fasl")
      (load "climdemo.fasl"))


     (load "postscript/sysdcl")
     (load "demo/sysdcl")

     (when load-composer
       (ignore-errors 
	(tenuring 
	 (require :composer))))

     (progn
       (load "test/testdcl")
       (tenuring 
	(clim-defsys::load-system 'testing)))

     (when (probe-file "climtoys/sysdcl.lisp")
       (load "climtoys/sysdcl.lisp")
       (clim-defsys::compile-system 'clim-toys)
       (tenuring 
	(clim-defsys::load-system 'clim-toys)))

     (clim-defsys::fake-load-system 
      (cons sys '(postscript-clim clim-demo)) 
      :recurse t)
     
     (ignore-errors
      (load (case sys
	      (motif-clim "climxm-preload.fasl")
	      (openlook-clim "climol-preload.fasl"))
	    :if-does-not-exist nil)))))
