;; -*- mode: common-lisp; package: user -*-
;;
;;				-[Wed Sep 15 18:09:45 1993 by duane]-
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
;; $fiHeader: compile-1.lisp,v 1.23 1993/05/25 20:41:40 cer Exp $

(in-package :user)

;;;; This should not matter
;;;; (setq *ignore-package-name-case* t)

(set-case-mode :case-insensitive-lower)

#+ignore
(progn
  (compile-file "~/stuff/misc/new-slot-opt.cl")
  (load "~/stuff/misc/new-slot-opt.fasl")
  )

#+ignore
(progn
  (setf (sys:gsgc-switch :print) t)
  (setf (sys:gsgc-switch :stats) t))

(setq comp:generate-call-count-code-switch
  (named-function |(> debug 1)| 
		  (lambda (safety size speed debug)
		    (declare (ignore safety size speed))
		    (> debug 1))))
(setq comp:declared-fixnums-remain-fixnums-switch
  (named-function |(> speed 2)|
		  (lambda (safety size speed debug)
		    (declare (ignore safety size debug))
		    (> speed 2))))

(unless (find-package 'clim-defsystem)
  (compile-file-if-needed "sys/defsystem")
  (let ((*enable-package-locked-errors* nil))
    (load "sys/defsystem")))



(defun compile-it (sys)
  (let ((excl::*update-entry-points* nil))
    (unless (ignore-errors (clim-defsys::find-system sys))
      (load "sys/sysdcl"))
    (clim-defsys::compile-system sys :propagate t)
    (tenuring
     (clim-defsys::load-system sys))

    (load "postscript/sysdcl")
    (clim-defsys::compile-system 'postscript-clim :propagate t)
    (clim-defsys::load-system 'postscript-clim)

    (compile-file-if-needed "test/test-suite")

    (load "test/test-suite")
  
    (load "demo/sysdcl")
    (clim-defsys::compile-system 'clim-demo :propagate t)
    (clim-defsys::load-system 'clim-demo)
  
    (progn
      (load "test/testdcl")
      (tenuring 
       (clim-defsys::compile-system 'testing)
       (clim-defsys::load-system 'testing)))

    (when (probe-file "climtoys/sysdcl.lisp")
      (load "climtoys/sysdcl.lisp")
      (clim-defsys::compile-system 'clim-toys))
  
    #+ignore (load "compatibility/sysdcl.lisp")
    #+ignore (clim-defsys::compile-system 'clim-compatibility :propagate t)

    (load "hpgl/sysdcl")
    (clim-defsys::compile-system 'hpgl-clim :propagate t)
    (clim-defsys::load-system 'hpgl-clim)
  
    ))




