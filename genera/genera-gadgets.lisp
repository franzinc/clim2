;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader: genera-gadgets.lisp,v 1.2 92/03/04 16:22:47 cer Exp Locker: cer $

(defmethod make-pane-class ((framem genera-frame-manager) class &rest options)
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

(defmethod make-pane-1 ((framem genera-frame-manager)
			   frame abstract-type &rest options)
  (let ((type (apply #'make-pane-class framem abstract-type options)))
    (if type
	(apply #'make-instance type
	       :frame frame
	       :frame-manager framem
	       (apply #'make-pane-arglist
		      framem abstract-type options))
	(call-next-method))))

(defmethod make-pane-arglist (realizer type &rest options)
  (declare (ignore realizer type))
  options)

