;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader$

(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defmethod copy-area ((from-medium cloe-medium) from-x from-y width height
		      (to-medium cloe-medium) to-x to-y)
  (let ((transform (sheet-device-transformation (medium-sheet from-medium))))
    (convert-to-device-coordinates transform from-x from-y to-x to-y)
    (convert-to-device-distances transform width height)
    (let ((x-delta (- to-x from-x))		;can be negative
	  (y-delta (- to-y from-y)))		;can be negative
      (with-slots (window) stream
	(win::scroll-dc window
			x-delta y-delta
			from-x from-y (+ from-x width) (+ from-y height) 
			0 0 width height)))))
