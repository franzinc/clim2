;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SYS; Base: 10; Lowercase: Yes -*-
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
;; $fiHeader: excl-verification.lisp,v 1.13 92/06/23 08:19:33 cer Exp $

(in-package :sys)

(eval-when (compile)
  (assert (member excl::*current-case-mode*
		  '(:case-insensitive-lower :case-insensitive-upper))))

(eval-when (compile load eval)
  (pushnew :clim *features*)
  (pushnew :clim-2 *features*)
  (pushnew :clim-2.0 *features*)
  (pushnew :silica *features*)
  (pushnew :ansi-90 *features*))

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

(defvar *clim-version* 
    '("CLIM 2.0.alpha.0"
      "$fiHeader: excl-verification.lisp,v 1.13 92/06/23 08:19:33 cer Exp $"))

(si::rcsnote (first *clim-version*) (second *clim-version*))
