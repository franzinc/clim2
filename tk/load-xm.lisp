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
;; $fiHeader: load-xm.lisp,v 1.32 1995/05/15 07:17:56 duane Exp $

(in-package :user)

(require :climg)

#+dlfcn
(progn
  (defvar sys::*toolkit-shared* nil)

  (unless (ff:get-entry-point (ff:convert-to-lang "XmCreateMyDrawingArea")
			      :note-shared-library-references nil)
    (load "clim2:;climxm.so")
    (setq sys::*toolkit-shared* t)))

#-dlfcn
(progn
  (defvar sys::*libtk-pathname* "Xm")
  (defvar sys::*libxt-pathname* "Xt")

  (unless (ff:get-entry-point (ff:convert-to-lang "XtToolkitInitialize"))
    (load "stub-motif.o"
	  :system-libraries (list sys::*libtk-pathname*
				  sys::*libxt-pathname*
				  sys::*libx11-pathname*)
	  :print t)
    (load "stub-xt.o"
	  :system-libraries (list sys::*libxt-pathname*
				  sys::*libx11-pathname*)
	  :print t))

  (unless (ff:get-entry-point (ff:convert-to-lang "XmCreateMyDrawingArea"))
    (load "xmsupport.o"
	  :system-libraries (list sys::*libtk-pathname*
				  sys::*libxt-pathname*
				  sys::*libx11-pathname*)
	  :print t))

  (unless (ff:get-entry-point (ff:convert-to-lang "XtAppIntervalNextTimer"))
    (load "xtsupport.o"
	  :system-libraries (list sys::*libxt-pathname*
				  sys::*libx11-pathname*)
	  :print t)))

(pushnew :clim-motif *features*)


