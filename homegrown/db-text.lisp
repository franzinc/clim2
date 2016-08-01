;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


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

(defun do-text-editing (stream &key initial-contents clear (exit-gesture '(:end)))
  (with-clim-state-reset (:all t
			  :encapsulating-streams nil)
    (when clear
      (window-clear stream))
    (with-input-focus (stream)
      (with-input-editing (stream :initial-contents initial-contents)
	(with-activation-gestures (exit-gesture :override t)
	  (unwind-protect
	      (read-token stream)
	    ;; Eat the activation character
	    (read-gesture :stream stream :timeout 0)))))))


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
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
    (when (eq (port-input-focus-selection (port pane)) :sheet-under-pointer)
      (edit-text-field pane))))

(defmethod handle-event ((pane text-editor-mixin) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defun edit-text-field (pane)
  (let ((string
	  (do-text-editing pane :initial-contents (gadget-value pane) :clear t)))
    (setf (gadget-value pane :invoke-callback t) string)))


(defclass text-field-pane (text-field
			   text-editor-mixin)
    ())

(defmethod compose-space ((pane text-field-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let* ((style (medium-default-text-style medium))
	   (style-width (text-style-width style medium))
	   (style-height (text-style-height style medium))
	   (string (gadget-value pane)))
      (multiple-value-bind (width height)
	  (if string 
	      (text-size pane string :text-style style)
	      (values (* style-width 20) style-height))
	(make-space-requirement :width width :height height)))))

(defmethod handle-event ((pane text-field-pane) (event pointer-button-press-event))
  (edit-text-field pane))


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
    (edit-text-field pane)))
