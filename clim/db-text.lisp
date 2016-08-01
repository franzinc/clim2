;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

;;;"Copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

;;; This file also exists in clim2/homegrown. The homegrown directory
;;; contains files to implement CLIM's generic gadgets. Native backends
;;; should not require this code. However, the Windows port of CLIM does
;;; seem to require this file so it is duplicated here with
;;; modifications. At some point this file and the file in homegrown should
;;; be merged and one of the two removed from cvs control (cim/tjm 2/4/97)

(in-package :clim-internals)

#+(or aclpc acl86win32)
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

#+(or aclpc acl86win32)
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
#+(or aclpc acl86win32)
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

;;;; Defer to above version per Jeff Morrill.
;;;;
;;;(defmethod handle-repaint :around ((pane text-editor-mixin) region)
;;;  (declare (ignore region))
;;;  (call-next-method)
;;;  (stream-set-cursor-position pane 0 0)
;;;  (let ((object (gadget-value pane))) 
;;;    (if (equal object "")
;;;      (with-standard-io-syntax
;;;        (write object :stream pane :escape t))
;;;      (write-string (gadget-value pane) pane))))

;;--- Grotesque kludge to subvert RECEIVE-GESTURE mechanism
#-(or aclpc acl86win32)
(defmethod receive-gesture :around ((pane text-editor-mixin) (event event))
  (process-event-locally pane event)
  nil)

#+(or aclpc acl86win32)
(defmethod receive-gesture :around ((pane text-editor-mixin) (event event))
  (if (typep event 'pointer-event)
    (progn  ; for processing random pointer events only
      (process-event-locally pane event)
      nil)
    (call-next-method)))

#+(or aclpc acl86win32)
(defmethod handle-event :around ((pane text-editor-mixin) (event pointer-event))
  (when (gadget-active-p pane)
    (call-next-method))
  (deallocate-event event))

#-(or aclpc acl86win32)
(defmethod handle-event ((pane text-editor-mixin) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
    (when (eq (port-input-focus-selection (port pane)) :sheet-under-pointer)
      (edit-text-field pane))))

#+(or aclpc acl86win32); no armed slot in instance, don't execute on pointer-enter-event
(defmethod handle-event ((pane text-editor-mixin) (event pointer-enter-event))
  nil)

#+(or aclpc acl86win32)
(defmethod handle-event ((pane text-editor-mixin) (event pointer-exit-event))
  nil)

#-(or aclpc acl86win32)
(defmethod handle-event ((pane text-editor-mixin) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

#+(or aclpc acl86win32)
(defun edit-text-field (pane)
  (let ((string
          (do-text-editing pane :initial-contents (gadget-value pane) :clear t)))
    (setf (gadget-value pane :invoke-callback t) string)))


#+(or aclpc acl86win32)
(defclass text-field-pane (text-field
                           text-editor-mixin)
    (#+(or aclpc acl86win32)
     (label :initform nil :initarg :label :accessor pane-label)))

#+(or aclpc acl86win32)
(defmethod initialize-instance :around ((pane text-field-pane) &key
                                        (background +white+))
  (call-next-method)
  (setf (slot-value pane 'silica::background) background))

#+(or aclpc acl86win32)
(defmethod compose-space ((pane text-field-pane) &key width height)
  ;(declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let* ((style (medium-default-text-style medium))
           (style-width (text-style-width style medium))
           (style-height (text-style-height style medium))
           (string (gadget-value pane)))
      (multiple-value-bind (twidth theight)
          (if string 
              (text-size pane string :text-style style)
              (values (* style-width 20) style-height))
          ;(cerror "ok" "width=~a height=~a twidth=~a theight=~a"
          ;        width height twidth theight)
          (setq width (or width twidth))
          (setq height (or height theight))
        (make-space-requirement :width 
                                #-(or aclpc acl86win32) width 
                                ;; not picking up on :width initialization
                                #+(or aclpc acl86win32) (+ width 32) 
                                :height (max height 16))))))

#+(or aclpc acl86win32)
(defmethod handle-event ((pane text-field-pane) (event pointer-button-press-event))
  (edit-text-field pane))

#+(or aclpc acl86win32)
(defclass text-editor-pane (text-editor
                            text-editor-mixin)
    ())

#+(or aclpc acl86win32)
(defmethod compose-space ((pane text-editor-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let* ((style (medium-default-text-style medium))
           (style-width (text-style-width style medium))
           (style-height (text-style-height style medium)))
      (make-space-requirement
        :width (* style-width (gadget-columns pane))
        :height (* 
                 #-(or aclpc acl86win32) style-height 
                 #+(or aclpc acl86win32) (+ style-height 2) ;; need to check this???
                 (gadget-lines pane))))))

#+(or aclpc acl86win32)
(defmethod handle-event ((pane text-editor-pane) (event pointer-button-press-event))
  (when (gadget-editable-p pane)
    (edit-text-field pane)))
