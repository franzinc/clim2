;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: window-stream.lisp,v 1.7 92/05/22 19:28:37 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(defclass console
	  ()
     ((key-table :accessor console-key-table)
      (pointer-list :initform nil :accessor console-pointer-list)))


;; A "window" is a CLIM stream pane that supports all the stream and
;; window operations.
(define-stream-protocol-class window ())

(defclass window-stream
	  (graphics-output-recording
	   window-output-recording
	   ;; This part still stands, but we need better layering so that you can
	   ;; have a window stream with no output recording mixed in.
	   output-recording-mixin
	   input-protocol-mixin
	   output-protocol-mixin
	   window)
    ())

(defmethod interactive-stream-p ((stream window-stream))
  nil)


#-Silica	;--- no such slots in Silica
(defmethod print-object ((window window-stream) stream)
  (print-unreadable-object (window stream :type t :identity t)
    (let ((left (safe-slot-value window 'left))
	  (top (safe-slot-value window 'top))
	  (right (safe-slot-value window 'right))
	  (bottom (safe-slot-value window 'bottom)))
      (format stream "/x ~D:~D y ~D:~D/" left right top bottom))))

(defmethod window-stream-class-name ((window-stream window-stream))
  (class-name (class-of window-stream)))

(defmethod window-modifier-state ((window window-stream))
  (let ((pointer (stream-primary-pointer window)))
    (pointer-button-state pointer)))


;;; Creation functions

#+CLIM-1-compatibility
(defun open-window-stream (&key parent
				left top right bottom width height
				(text-style *default-text-style*)
				(vertical-spacing 2)
				(end-of-line-action :allow)
				(end-of-page-action :allow)
				(background +white+)
				(foreground +black+)
				output-history 
				text-cursor text-margin
				label save-under 
				(scroll-bars :vertical)
				(class 'clim-stream-pane))
  (with-look-and-feel-realization ()
    (make-clim-stream-pane :type class
			   :left left :top top :right right :bottom bottom
			   :width width :height height
			   :text-style text-style :vertical-spacing vertical-spacing
			   :end-of-line-action end-of-line-action 
			   :end-of-page-action end-of-page-action
			   :background background :foreground foreground
			   :output-history output-history
			   :text-cursor text-cursor :text-margin text-margin
			   :label label :save-under save-under
			   :scroll-bars scroll-bars)))
