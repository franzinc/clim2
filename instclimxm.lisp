;; -*- mode: common-lisp; package: system -*-
;;
;;
;; copyright (c) 1991-2002 Franz Inc, Berkeley, CA  All rights reserved.
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

;; $Id: instclimxm.lisp,v 1.15 2002/07/09 20:57:14 layer Exp $

;;
;; Load the Motif version of CLIM
;;

(in-package :system)

(load-application (require :climxm) :devel system::*devel*)

#+ics
(load-application (require :climwnn) :devel system::*devel*)

(format t "~&; Finished loading CLIM XM~%")
(force-output)
