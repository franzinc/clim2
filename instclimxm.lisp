;; -*- mode: common-lisp; package: system -*-
;;
;;				-[Thu Feb 27 12:39:07 1992 by layer]-
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

;; $fiHeader: instclimxm.lisp,v 1.1 92/03/30 22:10:04 cer Exp Locker: cer $

;;
;; Load CLIM
;;

(in-package :system)

(defvar sys::*libx11-pathname* "/x11/R4/src/mit/lib/X/libX_d.a")
(defvar sys::*libxt-pathname* "/x11/R4/src/mit/lib/Xt/libXt_d.a")
(defvar sys::*libxm-pathname* "/x11/motif-1.1/lib/Xm/libXm.a")

(load-application (progn 
		    (require :climxm)
		    (load-patches
		     "patch"
		     (namestring
		      (merge-pathnames "update-clim-2.0"
				       excl::*library-pathname*))))
		  :devel system::*devel*)

(format t "~&; Finished loading CLIM~%")
(force-output)
