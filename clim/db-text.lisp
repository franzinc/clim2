;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: db-text.lisp,v 1.4 92/10/28 11:31:32 cer Exp $

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
    (setf (window-visibility stream) t)
    ;; Now edit the text
    (unwind-protect
	(do-text-editing stream 
			 :initial-contents initial-contents :exit-gesture exit-gesture)
      (setf (window-visibility stream) nil)
      (setf (cursor-visibility (stream-text-cursor stream)) nil))))

(defun do-text-editing (stream &key initial-contents (exit-gesture '(:end)))
  (with-input-focus (stream)
    (with-input-editing (stream :initial-contents initial-contents)
      (with-activation-gestures (exit-gesture :override t)
	(unwind-protect
	    (read-token stream)
	  ;; Eat the activation character
	  (read-gesture :stream stream :timeout 0))))))


;;; Text field and text editor gadgets

;; Requires TEXT-FIELD-PANE or TEXT-EDITOR-PANE
(defclass text-editor-mixin 
	  ;;--- It's awful that we need these to be CLIM streams
	  (clim-stream-pane) 
    ()
  (:default-initargs :draw-p t :record-p nil
		     :pointer-cursor :prompt))

(defmethod handle-repaint :around ((pane text-editor-mixin) region)
  (declare (ignore region))
  (call-next-method)
  (stream-set-cursor-position pane 0 0)
  (write-string (gadget-value pane) pane))

;;--- Grotesque kludge to subvert RECEIVE-GESTURE mechanism
(defmethod receive-gesture :around ((pane text-editor-mixin) (event event))
  (process-event-locally pane event)
  nil)

(defmethod handle-event :around ((pane text-editor-mixin) (event pointer-event))
  (when (gadget-active-p pane)
    (call-next-method))
  (deallocate-event event))

(defmethod handle-event ((pane text-editor-mixin) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane text-editor-mixin) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))


(defclass text-field-pane (text-field
			   text-editor-mixin)
    ())

(defmethod handle-event ((pane text-field-pane) (event pointer-button-press-event))
  (let ((string
	  (do-text-editing pane :initial-contents (gadget-value pane))))
    (setf (gadget-value pane) string)))


(defclass text-editor-pane (text-editor
			    text-editor-mixin)
    ())

(defmethod compose-space ((pane text-editor-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let* ((style (medium-default-text-style medium))
	   (style-width (text-style-width style medium))
	   (style-height (text-style-height style medium)))
      (make-space-requirement
	:width (* style-width (gadget-columns pane))
	:height (* style-height (gadget-lines pane))))))

(defmethod handle-event ((pane text-editor-pane) (event pointer-button-press-event))
  (when (gadget-editable-p pane)
    (let ((string
	    (do-text-editing pane :initial-contents (gadget-value pane))))
      (setf (gadget-value pane) string))))
