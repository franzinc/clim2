;; -*- mode: common-lisp; package: system -*-
;;
;;				-[Sun Jun  5 10:03:07 1994 by duane]-
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

;; $fiHeader: instclimxm.lisp,v 1.7 93/04/28 17:19:04 layer Exp $

;;
;; Load the Motif version of CLIM
;;

(in-package :system)

#-svr4
(progn
  (defvar sys::*libx11-pathname* "c")
  (defvar sys::*libxt-pathname* "c"))

(load-application (require :climxm) :devel system::*devel*)

(format t "~&; Finished loading CLIM XM~%")
(force-output)
