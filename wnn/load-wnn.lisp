;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $Id: load-wnn.lisp,v 1.8.36.2 2002/02/08 19:11:26 layer Exp $

(in-package :user)

;;; compare with tk/load-xm for update to new makefile style

(excl:ics-target-case
(:+ics

#+(version>= 5 0)
(unless (ff:get-entry-point (ff:convert-foreign-name "jl_open_lang"))
  (load (merge-pathnames (make-pathname
			  :type (or (car excl::*load-foreign-types*)
				    (error "Don't know foreign extension.")))
			 "clim2:;wnn")))

#+(and (not (version>= 5 0)) dlfcn)
(unless (ff:get-entry-point (ff:convert-foreign-name "jl_open_lang")
			    :note-shared-library-references
			    nil)
  (load "clim2:;wnn.so"))

#+(and (not (version>= 5 0)) (not dlfcn))
(progn
  (defvar sys::*libwnn-pathname* "wnn")

  (unless (ff:get-entry-point (ff:convert-foreign-name "jl_open_lang"))
    (load "stub-wnn.o"
	  :system-libraries (list sys::*libwnn-pathname*)
	  :print t)))

(provide :wnn)
(pushnew :wnn *features*)

)) ;; ics-target-case


