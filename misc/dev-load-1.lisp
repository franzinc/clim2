;; -*- mode: common-lisp; package: user -*-
;;
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: dev-load-1.lisp,v 2.6 2005/12/08 21:25:45 layer Exp $

;;;; This should not matter
;;;; (setq *ignore-package-name-case* t)

;; Forgive them, lord, for they know not what they do.
(pushnew :ansi-90 *features*)

(tenuring
 (let ((*load-source-file-info* t)
       (*record-source-file-info* t)
       (*load-xref-info* nil))
   (load "clim2:;sys;sysdcl")))

(defun load-it (sys &key load-composer)
  (let ((*load-source-file-info* t)
	(*record-source-file-info* t)
	(*load-xref-info* nil)
	(excl:*global-gc-behavior* nil))

    (tenuring
     (ecase sys
       (motif-clim
	(load "clim2:;climg.fasl")
	(load "clim2:;climxm.fasl")
	(load "clim2:;clim-debug.fasl")
	(load "clim2:;clim-debugxm.fasl"))
       (openlook-clim
	(load "clim2:;climg.fasl")
	(load "clim2:;climol.fasl")
	(load "clim2:;clim-debug.fasl")
	(load "clim2:;clim-debugol.fasl")))

     #+ics
     (tenuring
      (load "clim2:;climwnn.fasl")
      (load "clim2:;clim-debugwnn.fasl"))

     (tenuring
      (load "clim2:;climps.fasl")
      (load "clim2:;climhpgl.fasl")
      (load "clim2:;climdemo.fasl"))


     (load "clim2:;postscript;sysdcl")
     (load "clim2:;demo;sysdcl")

     (when load-composer
       (ignore-errors
	(tenuring
	 (require :composer))))

     (progn
       (load "clim2:;test;testdcl")
       (tenuring
	(excl:load-system 'testing)))

     (when (probe-file "clim2:;climtoys;sysdcl.lisp")
       (load "clim2:;climtoys;sysdcl.lisp")
       (excl:compile-system 'clim-toys)
       (tenuring
	(excl:load-system 'clim-toys)))

     #+never
     (clim-defsys::fake-load-system
      (cons sys '(postscript-clim clim-demo))
      :recurse t)

     (ignore-errors
      (load (case sys
	      (motif-clim "clim2:;climxm-preload.fasl")
	      (openlook-clim "clim2:;climol-preload.fasl"))
	    :if-does-not-exist nil)))))
