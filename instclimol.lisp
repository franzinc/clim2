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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;

;; $Header: /repo/cvs.copy/clim2/Attic/instclimol.lisp,v 1.10 1997/02/05 01:42:20 tomj Exp $

;;
;; Load the OpenLook version of CLIM
;;

(in-package :system)

(load-application (require :climol) :devel system::*devel*)

(format t "~&; Finished loading CLIM OL~%")
(force-output)
