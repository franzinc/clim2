;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader$

"Copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

(in-package :clim-internals)

(defun pop-up-text-editor (&key initial-contents prompt
				(exit-gesture '(:end))
				(width '(60 :character)) (height '(10 :line)))
  (with-menu (stream)
    ;; First set up the pop-up window the way we want to see it
    (setf (cursor-visibility (stream-text-cursor stream)) :off)
    (when prompt
      (with-text-face (stream :italic)
	(write-string prompt stream))
      (fresh-line stream))
    (let ((width (process-spacing-arg stream width 'pop-up-text-editor :width))
	  (height (process-spacing-arg stream height 'pop-up-text-editor :height)))
      (window-set-inside-size stream width height))
    (setf (stream-text-margin stream) (bounding-rectangle-width (window-viewport stream)))
    (window-expose stream)
    ;; Now edit the text
    (unwind-protect
	(do-text-editing stream 
			 :initial-contents initial-contents :exit-gesture exit-gesture)
      (setf (window-visibility stream) nil)
      (setf (cursor-visibility (stream-text-cursor stream)) nil))))

(defun do-text-editing (stream &key initial-contents (exit-gesture '(:end)))
  (with-input-editing (stream :initial-contents initial-contents)
    (with-activation-gestures (exit-gesture :override t)
      (unwind-protect
	  (read-token stream)
	;; Eat the activation character
	(read-gesture :stream stream :timeout 0)))))
