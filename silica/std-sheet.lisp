;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: std-sheet.lisp,v 1.7 92/07/20 15:59:33 cer Exp $

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
	   sheet)
    ())


;;--- This should probably be flushed
(defclass simple-sheet
	  (sheet-multiple-child-mixin 
	   sheet-transformation-mixin
	   standard-repainting-mixin
	   standard-sheet-input-mixin
	   temporary-medium-sheet-output-mixin
	   sheet)
    ())


(defmethod handle-event (sheet event)
  (declare (ignore sheet event))
  #+++ignore (warn "Ignoring event ~S on sheet ~S" sheet event)
  nil)
