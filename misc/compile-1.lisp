;; -*- mode: common-lisp; package: user -*-
;;
;;				-[Thu Nov 10 02:16:15 1994 by smh]-
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

;; Forgive them, lord, for they know not what they do.
(pushnew :ansi-90 *features*)

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

;(unless (find-package 'clim-defsystem)
;  (compile-file-if-needed "sys/defsystem")
;  (let ((*enable-package-locked-errors* nil))
;    (load "sys/defsystem")))

(defun compile-it (sys)
  (let ((excl::*update-entry-points* nil))
    (unless (ignore-errors (excl:find-system 'sys))
      (load "clim2:;sys;sysdcl"))
    (excl:compile-system sys :include-components t)
    (tenuring
     (excl:load-system sys))

    (load "clim2:;postscript;sysdcl")
    (excl:compile-system 'postscript-clim :include-components t)
    (excl:load-system 'postscript-clim)

    (compile-file-if-needed "clim2:;test;test-suite")

    (load "clim2:;test;test-suite")

    (load "clim2:;demo;sysdcl")
    (excl:compile-system 'clim-demo :include-components t)
    (excl:load-system 'clim-demo)

    (progn
      (load "clim2:;test;testdcl")
      (tenuring
       (excl:compile-system 'testing)
       (excl:load-system 'testing)))

    (when (probe-file "clim2:;climtoys;sysdcl.lisp")
      (load "clim2:;climtoys;sysdcl.lisp")
      (excl:compile-system 'clim-toys))

    #+ignore (load "compatibility/sysdcl.lisp")
    #+ignore (excl:compile-system 'clim-compatibility :include-components t)

    (load "clim2:;hpgl;sysdcl")
    (excl:compile-system 'hpgl-clim :include-components t)
    (excl:load-system 'hpgl-clim)

    ))




