;; -*- mode: common-lisp; package: cl-user -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; $fiHeader: pkg.lisp,v 1.9 93/04/02 18:41:09 cer Exp $

(defpackage tk-silica
  (:nicknames xm-silica xt-silica)
  (:use clim-lisp clim-utils clim silica tk)
  (:import-from excl #:if*)
  (:export
   ;; Motif 
 
   #:motif-separator
   #:motif-scroll-bar
   #:motif-slider
   #:motif-push-button
   #:motif-label-pane
   #:motif-text-field
   #:motif-text-editor
   #:motif-toggle-button
   #:motif-menu-bar
   #:motif-radio-box
   #:motif-check-box
   #:motif-frame-pane
   #:motif-list-pane
   #:motif-option-pane

   ;; Openlook
 
   #:openlook-scroll-bar
   #:openlook-slider
   #:openlook-push-button
   #:openlook-label-pane
   #:openlook-text-field
   #:openlook-text-editor
   #:openlook-toggle-button
   #:openlook-menu-bar
   #:openlook-radio-box
   #:openlook-check-box
   #:openlook-list-pane
   #:openlook-option-pane
   
   ))

