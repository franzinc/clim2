;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: std-sheet.lisp,v 1.8 92/08/18 17:23:57 cer Exp $

(in-package :silica)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved."


;;--- This should probably be flushed
(defclass standard-sheet 
	  (permanent-medium-sheet-output-mixin
	   mirrored-sheet-mixin
	   sheet-multiple-child-mixin
	   sheet-transformation-mixin
	   standard-repainting-mixin
	   standard-sheet-input-mixin
	   basic-sheet)
    ())


;;--- This should probably be flushed
(defclass simple-sheet
	  (sheet-multiple-child-mixin 
	   sheet-transformation-mixin
	   standard-repainting-mixin
	   standard-sheet-input-mixin
	   temporary-medium-sheet-output-mixin
	   basic-sheet)
    ())


(defmethod handle-event (sheet event)
  (declare (ignore sheet event))
  #+++ignore (warn "Ignoring event ~S on sheet ~S" sheet event)
  nil)
