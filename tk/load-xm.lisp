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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Header: /repo/cvs.copy/clim2/tk/load-xm.lisp,v 1.43 1998/03/18 20:27:53 layer Exp $

(in-package :user)

(eval-when (compile eval load)
  (require :climg))

#+(version>= 5 0)
(unless (ff:get-entry-point (ff:convert-to-lang "XmCreateMyDrawingArea"))
  (let (
;;;; See spr12165 for the *dlopen-mode* hack explanation.
	#+(and dlfcn sun4 (not sunos4))
	(excl::*dlopen-mode* (excl:ics-target-case
			      (:+ics #x102)
			      (:-ics excl::*dlopen-mode*))))
    (load (merge-pathnames (make-pathname
			    :type (or (car excl::*load-foreign-types*)
				      (error "Don't know foreign extension.")))
			   "clim2:;climxm"))))

#+(and (not (version>= 5 0)) dlfcn)
(unless (ff:get-entry-point (ff:convert-to-lang "XmCreateMyDrawingArea")
			    :note-shared-library-references
			    nil)
  (let ((ff::*dlopen-mode* (excl:ics-target-case
			    (:+ics #x102)
			    (:-ics ff::*dlopen-mode*))))
    (load "clim2:;climxm.so")))

#+(and (not (version>= 5 0)) (not dlfcn))
(progn

(defvar sys::*libtk-pathname* "Xm")
(defvar sys::*libxt-pathname* "Xt")

(unless (ff:get-entry-point (ff:convert-to-lang "XtToolkitInitialize"))
  (load "clim2:;stub-motif.o"
	:system-libraries (list sys::*libtk-pathname*
				sys::*libxt-pathname*
				sys::*libx11-pathname*)
	:print t)
  (load "clim2:;stub-xt.o"
	:system-libraries (list sys::*libxt-pathname*
				sys::*libx11-pathname*)
	:print t))

(unless (ff:get-entry-point (ff:convert-to-lang "XmCreateMyDrawingArea"))
  (load "clim2:;xmsupport.o"
	:system-libraries (list sys::*libtk-pathname*
				sys::*libxt-pathname*
				sys::*libx11-pathname*)
	:print t))

(unless (ff:get-entry-point (ff:convert-to-lang "XtAppIntervalNextTimer"))
  (load "clim2:;xtsupport.o"
	:system-libraries (list sys::*libxt-pathname*
				sys::*libx11-pathname*)
	:print t))
)

(pushnew :clim-motif *features*)
