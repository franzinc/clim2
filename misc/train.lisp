; -*- mode: common-lisp; package: user -*-
;;
;;				-[Thu Sep  2 10:17:42 1993 by layer]-
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: train.lisp,v 1.19 1995/05/17 19:48:53 colin Exp $

(defun train-clim (&key (train-times 2)
			(psview nil)
			(frame-tests t)
			(errorp t)
			(hpglview nil)
			(compile t)
			(profilep t)
			(benchmarkp t)
			(report-file))

  (load "test/test.lisp")

  (room t)

  (clim-test:with-test-reporting (:file (or report-file
					    (if (excl::featurep :clim-motif)
						"notes/test-suite-reportxm.lisp"
					      "notes/test-suite-reportol.lisp")))
    (when compile
      (compile-file-if-needed "test/test-suite" :print nil :verbose t))
    (load "test/test-suite.fasl")
    (clim-test:train-clim-2 train-times)
    (when frame-tests
      (clim-test:do-frame-tests errorp))
    (when psview
      (load "test/postscript-tests.lisp")
      (clim-user::run-postscript-tests :output psview))

    (when hpglview
      (load "test/postscript-tests.lisp")
      (require :climhpgl)
      (load "test/hpgl-tests.lisp")
      (clim-user::run-hpgl-tests :output hpglview)))

  (room t)
  (gc t)
  (room t)

  #+ignore
  (format t "Counters are ~S~%"
	  tk::(list *string-counter* *font-counter* *color-counter* *widget-count*))


  #+ignore
  (progn
    (format t "Port mapping ~S~%" (silica::port-mirror->sheet-table (clim:find-port)))

    #+verbose
    (maphash #'(lambda  (x y)
		 (print y))
	     (silica::port-mirror->sheet-table (clim:find-port)))

    (format t "Port framem ~S~%" (find-frame-manager :port (clim:find-port)))
    (format t "Port framem frames ~S~%" (frame-manager-frames
					 (find-frame-manager :port (clim:find-port))))

    (format t " Address mapping ~S~%" tk::*address->object-mapping*)

    #+verbose
    (maphash #'(lambda  (x y) (print y))
	     tk::*address->object-mapping*))


  #-svr4 (excl:shell "ps vaxg")

  ;; coverage reports don't yet work with new call counting scheme
  ;; cim 10/17/95
  #+ignore
  (when (fboundp 'generate-coverage-report)
    (with-open-file (*standard-output* (if (excl::featurep :clim-motif)
					   "notes/coverage-reportxm.lisp"
					 "notes/coverage-reportol.lisp")
		     :if-exists :supersede :direction :output)
      (generate-coverage-report :files (known-clim2-files))))

  ;; We have to do this here because profiling clears the call counts.

  (when (and (fboundp 'clim-user::run-profile-clim-tests)
	     profilep)
    (clim-user::run-profile-clim-tests))

  (when benchmarkp
    (clim-test::benchmark-clim))

  ;; delete the preload fasls for the type of clim we are testing, so that
  ;; make-dist cat use the one we will make.

  (let ((xx (if* (excl::featurep :clim-motif)
	       then "xm"
	       else "ol")))
    (dolist (file-format '("clim~a-preload.fasl"
			   "clim~a-preload.fasl.Z"
			   "clim~a-preload.fasl.z"
			   "clim~a-preload.fasl.gz"))
      (handler-case (delete-file (format nil file-format xx))
	(error () nil))))


  (let ((comp::*new-closure-style* nil)
	(tpl::*zoom-print-circle* t)
	(tpl::*zoom-print-length* 3)
	(tpl::*zoom-print-level* 3))
    (compile-file (if* (probe-file "../src/clos-preload.cl")
		     thenret
		     else "misc/clos-preload.cl")
		  :output-file
		  (if (excl::featurep :clim-motif)
		      "climxm-preload.fasl"
		    "climol-preload.fasl"))))
