;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: excl-verification.lisp,v 1.4 92/02/24 13:05:36 cer Exp $

(in-package :sys)

"Copyright (c) 1991 Franz Inc.  All rights reserved."

(assert (eq excl::*current-case-mode* :case-insensitive-lower))

#-Silica
(eval-when (compile load eval)
  (require :clx))

(provide :clim)

#+(not (version>= 4 1))
(let* ((patch-package (find-package :patch))
       (patches-symbol (and patch-package
			    (find-symbol (string '*patches*) patch-package)))
       (patches (and patches-symbol (symbol-value patches-symbol)))
       (needed-patches '(26. 27. 46. 50.))
       (dont-have (remove-if 
		   #'(lambda (pn)
		       (assoc pn patches))
		   needed-patches)))
  (when dont-have
    (error "On Allegro 4.0 CLIM requires patches ~{~S ~} ~
You do not have patches ~{~S ~}"
	   needed-patches
	   dont-have)))

(si::rcsnote
 "CLIM 2.0"
 "$fiHeader: ")
