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
;; $fiHeader: cload-demos.lisp,v 1.5 92/10/12 17:54:30 cer Exp $

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
      "custom-records"))

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
