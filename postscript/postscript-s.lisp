;; -*- mode: common-lisp; package: postscript-clim -*-
;;
;;
;; See the file LICENSE for the full license governing this code.
;;


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

