;; -*- mode: common-lisp; package: user -*-
;;
;;				-[Fri Aug 20 07:36:40 1993 by layer]-
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: train.lisp,v 1.10 1993/07/29 20:51:36 layer Exp $

(defun train-clim (&key (train-times 2) 
			(psview nil)
			(frame-tests t)
			(errorp t)
			(hpglview nil)
			(compile t)
			(profilep t)
			(benchmarkp t))
  
  ;;(setq *global-gc-behavior* nil)
  (load "test/test.lisp")
  (clim-user::with-test-reporting (:file (if (excl::featurep :clim-motif) 
					     "test-suite-reportxm.lisp"
					   "test-suite-reportol.lisp"))
    (when compile
      (compile-file-if-needed "test/test-suite" :print nil :verbose t))
    (load "test/test-suite.fasl")
    (clim-user::train-clim-2 train-times) 
    (when frame-tests
      (clim-user::do-frame-tests errorp))
    (when psview
      (load "test/postscript-tests.lisp")
      (clim-user::run-postscript-tests :output psview))
  
    (when hpglview
      (load "test/postscript-tests.lisp")
      (require :climhpgl)
      (load "test/hpgl-tests.lisp")
      (clim-user::run-hpgl-tests :output hpglview)))
  
  (when (fboundp 'generate-coverage-report)
    (with-open-file (*standard-output* (if (excl::featurep :clim-motif) 
					   "coverage-reportxm.lisp"
					 "coverage-reportol.lisp")
		     :if-exists :supersede :direction :output)
      (generate-coverage-report :files (known-clim2-files))))

  ;; We have to do this here because profiling clears the call counts.
  
  (when (and (fboundp 'clim-user::run-profile-clim-tests)
	     profilep)
    (clim-user::run-profile-clim-tests))

  (when benchmarkp 
    (clim-user::benchmark-clim))

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

  (compile-file "misc/clos-preload.cl" 
		:output-file 
		(if (excl::featurep :clim-motif) 
		    "climxm-preload.fasl" 
		  "climol-preload.fasl")))



