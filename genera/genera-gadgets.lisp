;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader$

(defmethod realize-pane-class ((framem genera-frame-manager) class &rest options)
  (declare (ignore options))
  (second (assoc class '((scroll-bar genera-scrollbar)
			 (slider generic-slider)
			 (push-button generic-push-button)
			 (text-field generic-text-field)
			 (toggle-button generic-toggle-button)
			 (menu-bar generic-menu-bar)
			 (viewport genera-viewport)
			 (radio-box generic-radio-box)
			 (frame-pane generic-frame-pane)
			 (top-level-sheet genera-top-level-sheet)
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

(defmethod realize-pane-1 ((framem genera-frame-manager)
			   frame abstract-type &rest options)
  (let ((type (apply #'realize-pane-class framem abstract-type options)))
    (if type
	(apply #'make-instance type
	       :frame frame
	       :frame-manager framem
	       (apply #'realize-pane-arglist
		      framem abstract-type options))
	(call-next-method))))

(defmethod realize-pane-arglist (realizer type &rest options)
  (declare (ignore realizer type))
  options)



(defclass genera-top-level-sheet (top-level-sheet) ())

(defclass genera-viewport (viewport) ())

(defclass genera-scrollbar (silica::scroll-bar-pane) ())


(defclass generic-menu-bar (clim-stream-pane) 
    ((command-table :initarg :command-table)))

