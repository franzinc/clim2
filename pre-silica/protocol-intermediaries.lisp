;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1989, 1990 International Lisp Associates.  All rights reserved."

;;; Methods for streams that are both windows and extended input streams

(defmethod stream-note-pointer-button-press :around
  ((window input-and-window-protocol-intermediary) pointer button modifier-state x y)
  (multiple-value-bind (nx ny) (viewport-to-drawing-surface-coordinates window x y)
    (call-next-method window pointer button modifier-state nx ny)))

(defmethod stream-note-pointer-button-release :around
  ((window input-and-window-protocol-intermediary) pointer button modifier-state x y)
  (multiple-value-bind (nx ny) (viewport-to-drawing-surface-coordinates window x y)
    (call-next-method window pointer button modifier-state nx ny)))

(defmethod stream-pointer-position :around ((window input-and-window-protocol-intermediary)
					    &rest args)
  (declare (ignore args))
  (multiple-value-bind (x y)
      (call-next-method)
    (viewport-to-drawing-surface-coordinates window x y)))

(defmethod stream-set-pointer-position :around
  ((window input-and-window-protocol-intermediary) x y &key pointer)
  (multiple-value-bind (nx ny) (viewport-to-drawing-surface-coordinates window x y)
    (call-next-method window nx ny :pointer pointer)))

(defmethod stream-set-cursor-position :after ((window input-and-window-protocol-intermediary)
					      x y)
  (let ((cursor (stream-text-cursor window)))
    (when cursor
      (cursor-set-position cursor x y))))

(defmethod stream-set-cursor-position-internal :after
	   ((window input-and-window-protocol-intermediary) x y)
  (let ((cursor (stream-text-cursor window)))
    (when cursor
      (cursor-set-position cursor x y))))


;;; Methods for streams that are both windows and extended output streams

(defmethod stream-write-string-1 :around ((window output-and-window-protocol-intermediary)
					  glyph-buffer start end font color x y)
  (multiple-value-bind (nx ny) (drawing-surface-to-viewport-coordinates window x y)
    (call-next-method window glyph-buffer start end font color nx ny)))

(defmethod stream-write-char-1 :around ((window output-and-window-protocol-intermediary)
					index font color x y)
  (multiple-value-bind (nx ny) (drawing-surface-to-viewport-coordinates window x y)
    (call-next-method window index font color nx ny)))

(defmethod window-note-size-or-position-change :around
  ((window output-and-window-protocol-intermediary)
   new-left new-top new-right new-bottom)
  ;; we have to use UNWIND-PROTECT because some method may throw...
  ;; Yecch.  Can't there be a better way?
  (unwind-protect
      (call-next-method window new-left new-top new-right new-bottom)
    (let ((width (window-inside-width window)))
      (setf (stream-default-text-margin window) width))))

#-Silica
(defmethod bounding-rectangle-set-edges :after
	   ((window output-and-window-protocol-intermediary)
	    new-left new-top new-right new-bottom)
  (declare (ignore new-left new-top new-right new-bottom))
  (let ((width (window-inside-width window)))
    (setf (stream-default-text-margin window) width)))

#-Silica
(defmethod bounding-rectangle-set-width :after
	   ((window output-and-window-protocol-intermediary) width height)
  (declare (ignore width height))
  (let ((width (window-inside-width window)))
    (setf (stream-default-text-margin window) width)))


;;; Methods for streams that are both output recording and extended input streams

(defmethod invoke-with-output-recording-options :around
           ((stream output-and-recording-protocol-intermediary) continuation record draw)
  (let ((restore-p (and record
			(not draw)
			(not (stream-recording-p stream)))))
    (multiple-value-bind (old-x old-y)
	(when restore-p (stream-cursor-position stream))
      (unwind-protect
	  (progn
	    (stream-close-text-output-record stream)
	    (call-next-method stream continuation record draw))
	(when restore-p (stream-set-cursor-position stream old-x old-y))))))

