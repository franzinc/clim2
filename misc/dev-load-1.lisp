;; -*- mode: common-lisp; package: user -*-
;;
;;
;; See the file LICENSE for the full license governing this code.
;;

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
