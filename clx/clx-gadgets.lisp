;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: clx-gadgets.lisp,v 1.3 92/03/04 16:20:50 cer Exp $

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."

(defmethod make-pane-class ((framem clx-frame-manager) class &rest options)
  (declare (ignore options))
  (second (assoc class '((scroll-bar scroll-bar-pane)
			 (slider slider-pane)
			 (push-button push-button-pane)
			 (text-field text-field-pane)
			 (toggle-button toggle-button-pane)
			 (menu-bar menu-bar-pane)
			 (viewport viewport)
			 (radio-box radio-box-pane)
			 (frame-pane frame-pane)
			 (top-level-sheet top-level-sheet)
			 ;; One day
			 (line-editor-pane)
			 (label-button-pane)
			 (radio-button-pane)
			 (horizontal-divider-pane)
			 (vertical-divider-pane)
			 (label-pane)
			 ;;
			 (list-pane)
			 (caption-pane)
			 (cascade-button)
			 ))))

(defmethod make-pane-1 ((framem clx-frame-manager)
			frame abstract-type &rest options)
  (let ((type (apply #'make-pane-class framem abstract-type options)))
    (if type
	(apply #'make-instance type
	       :frame frame
	       :frame-manager framem
	       (apply #'make-pane-arglist framem abstract-type options))
	(call-next-method))))

(defmethod make-pane-arglist (realizer type &rest options)
  (declare (ignore realizer type))
  options)

;;--- This isn't really right.
(defmethod sheet-shell (sheet) sheet)
