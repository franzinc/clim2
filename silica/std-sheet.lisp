;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: std-sheet.lisp,v 1.12.22.2 1998/07/06 23:09:58 layer Exp $

(in-package :silica)

;;;"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved."


(defclass standard-sheet 
          (sheet-multiple-child-mixin
           sheet-transformation-mixin
           standard-repainting-mixin
           standard-sheet-input-mixin
           permanent-medium-sheet-output-mixin
           basic-sheet)
    ())

(defclass standard-mirrored-sheet 
          (mirrored-sheet-mixin
           standard-sheet)
    ())

(defclass simple-sheet
          (sheet-multiple-child-mixin 
           sheet-transformation-mixin
           standard-repainting-mixin
           standard-sheet-input-mixin
           temporary-medium-sheet-output-mixin
           basic-sheet)
    ())


(defclass simple-pane 
          (sheet-permanently-enabled-mixin
           sheet-mute-input-mixin
           space-requirement-mixin
           space-requirement-cache-mixin
           basic-pane)
    ())
