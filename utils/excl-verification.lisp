;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: excl-verification.lisp,v 1.6 92/03/24 19:36:55 cer Exp Locker: cer $

(in-package :sys)

"Copyright (c) 1991 Franz Inc.  All rights reserved."

(assert (eq excl::*current-case-mode* :case-insensitive-lower))

#-Silica
(eval-when (compile load eval)
  (require :clx))

(provide :clim)
(provide :climg)

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
 "CLIM 2.0.alpha.0"
 "$fiHeader: ")
