;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/silica/std-sheet.lisp,v 1.12.22.1 1998/05/19 01:05:04 layer Exp $

(in-package :silica)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved."


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
