;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1993 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: load-wnn.lisp,v 1.3 1996/03/15 05:18:37 colin Exp $

(in-package :user)

;;; compare with tk/load-xm for update to new makefile style

(excl:ics-target-case
(:+ics

#-dlfcn
(progn

  (defvar sys::*libwnn-pathname* "wnn")

  (unless (ff:get-entry-point (ff:convert-to-lang "jl_open_lang"))
    (load "stub-wnn.o"
	  :system-libraries (list sys::*libwnn-pathname*)
	  :print t)))

#+dlfcn
(unless (ff:get-entry-point (ff:convert-to-lang "jl_open_lang"))
  (cerror "Continue without wnn"
	  "Can't load Wnn/jlib dynamically - need to rebuild Lisp"))

(provide :wnn)
(pushnew :wnn *features*)

)) ;; ics-target-case



