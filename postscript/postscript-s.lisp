;; -*- mode: common-lisp; package: postscript-clim -*-
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
;; $fiHeader$


(in-package :postscript-clim)

(macrolet ((def-ps-stubs (functions macros)
	       `(progn
		  ,@(mapcar #'(lambda (fn)
				`(excl::def-autoload-function ,fn "climps.fasl"))
			    functions)
		  ,@(mapcar #'(lambda (macro)
				`(excl::def-autoload-macro ,macro "climps.fasl"))
			    macros))))
  (def-ps-stubs
      ;;-- We have to do this because its not exported.
      ;;-- if it were we could make the package autoloaded too
      (invoke-with-output-to-postscript-stream)
      (with-output-to-postscript-stream)))

