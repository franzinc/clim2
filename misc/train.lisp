;; -*- mode: common-lisp; package: user -*-
;;
;;				-[]-
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
;; $fiHeader: train.lisp,v 1.2 92/12/07 12:15:06 cer Exp $

(defun train-clim (&key (train-times 2) (psview nil) (frame-tests t) (errorp t))
  (load "test/test.lisp") 
  (when frame-tests
    (clim-user::train-clim-2 train-times) 
    (clim-user::do-frame-tests errorp))
  (when psview
    (load "test/postscript-tests.lisp")
    (clim-user::run-postscript-tests :output psview))
  (with-open-file (*standard-output* "coverage-report.lisp" 
		   :if-exists :supersede :direction :output)
    (generate-coverage-report))
  (compile-file "misc/clos-preload.cl" 
		:output-file 
		(if (excl::featurep :clim-motif) 
		    "misc/clos-preloadxm.fasl" 
		  "misc/clos-preloadol.fasl")))



