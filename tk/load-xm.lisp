;; -*- mode: common-lisp; package: user -*-
;;
;;				-[Fri Apr 16 17:38:25 1993 by layer]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: load-xm.lisp,v 1.22 92/10/02 15:17:58 cer Exp $

(in-package :user)

(provide :climxm)
(require :climg)

#+svr4
(unless (ff:get-entry-point (ff:convert-to-lang "XmCreateMyDrawingArea"))
  (load "climxm.so"))

#-svr4
(progn
  (defvar sys::*libxt-pathname* "/x11/R4/sun4-lib/libXt.a")
  (defvar sys::*clim-motif-pathname*
      "clim-motif.o")

  (x11::load-undefined-symbols-from-library
   sys::*clim-motif-pathname*
   (x11::symbols-from-file "misc/undefinedsymbols.motif")
   (list sys::*libxt-pathname* sys::*libx11-pathname*))

  (unless (ff:get-entry-point (ff:convert-to-lang "XmCreateMyDrawingArea"))
    (load "MyDrawingA.o"
	  :system-libraries (list sys::*libxt-pathname*
				  sys::*libx11-pathname*)
	  :print t))

  (unless (ff:get-entry-point (ff:convert-to-lang "XtAppIntervalNextTimer"))
    (load "xtsupport.o"
	  :system-libraries (list sys::*libxt-pathname*
				  sys::*libx11-pathname*)
	  :print t)))

(pushnew :clim-motif *features*)


