;; -*- mode: common-lisp; package: system -*-
;;
;;				-[Fri Apr 16 11:55:09 1993 by layer]-
;; 
;; copyright (c) 1991 Franz Inc, Berkeley, CA  All rights reserved.
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

;; $fiHeader: instclimol.lisp,v 1.4 92/08/18 17:54:20 cer Exp Locker: cer $

;;
;; Load the OpenLook version of CLIM
;;

(in-package :system)

#-svr4
(progn
  (defvar sys::*libx11-pathname* "/x11/R4/sun4-lib/libX11.a")
  (defvar sys::*libxt-pathname* "/x11/R4/sun4-lib/libXt.a"))

(load-application (require :climol) :devel system::*devel*)

(format t "~&; Finished loading CLIM OL~%")
(force-output)
