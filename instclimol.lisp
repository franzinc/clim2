;; -*- mode: common-lisp; package: system -*-
;;
;;				-[Mon Jul  6 15:37:15 1998 by layer]-
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

;; $Id: instclimol.lisp,v 1.11 1998/08/06 23:15:36 layer Exp $

;;
;; Load the OpenLook version of CLIM
;;

(in-package :system)

(load-application (require :climol) :devel system::*devel*)

(format t "~&; Finished loading CLIM OL~%")
(force-output)
