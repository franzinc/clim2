;; -*- mode: common-lisp; package: user -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-user)

(defvar *demo-files*
    '(
      "test-suite"
      "packages"
      "demo-driver"
      "cad-demo"
      "thinkadot"
      "graphics-demos"
      "address-book"
      "listener"
      "navfun"
      "navdata"
      "puzzle"
      "plot"
      "color-editor"
      "graphics-editor"
      "bitmap-editor"
      "ico"
      "process-browser"
      "peek-frame"
      "demo-activity"
      "custom-records"
      "browser"))

(defun compile-and-load-demos (&key forcep
				    (directory
				     (make-pathname
				      :directory (pathname-directory
						  #.(truename
						     excl::*source-pathname*)))))
  (mapcar #'(lambda (name)
	      (let ((name (merge-pathnames
			   directory
			   name)))
		(if forcep
		    (compile-file name)
		  (excl::compile-file-if-needed name))
		(load name)))
	  *demo-files*))
