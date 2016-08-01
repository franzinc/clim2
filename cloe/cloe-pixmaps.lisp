;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


(defmethod medium-copy-area ((from-medium cloe-window-medium) from-x from-y width height
			     (to-medium cloe-window-medium) to-x to-y)
  (unless (eq from-medium to-medium)
    (error "Can't copy."))
  (when (select-cloe-dc from-medium)
  (let ((transform (sheet-device-transformation (medium-sheet from-medium))))
    (convert-to-device-coordinates transform from-x from-y to-x to-y)
    (convert-to-device-distances transform width height)
    (let ((left (min from-x to-x))
	  (top (min from-y to-y))
	  (right (+ (max from-x to-x) width))
	  (bottom (+ (max from-y to-y) height)))
      (win::scroll-dc *dc*
		      (- to-x from-x) (- to-y from-y)
		      left top right bottom
		      left top right bottom)))))
