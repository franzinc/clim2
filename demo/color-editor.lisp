;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: color-editor.lisp,v 1.2 92/09/08 15:19:00 cer Exp $

(in-package :clim-demo)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(define-application-frame color-chooser ()
    ((color :accessor color :initform +black+)
     red blue green
     intensity hue saturation)
  (:menu-bar nil)
  (:panes 
    (display :application
	     :scroll-bars nil
	     :display-function 'display-color
	     ;; Make sure we don't have a useless cursor blinking away...
	     :initial-cursor-visibility nil)
    (exit push-button 
	  :label "Exit"
	  :activate-callback #'(lambda (button)
				 (frame-exit (pane-frame button))))
    (rgb (with-slots (red green blue) *application-frame*
	   (outlining ()
	     (horizontally ()
	       (setq red (make-pane 'slider
			   :label "Red" :foreground +red+
			   :orientation :vertical
			   :min-value 0.0 :max-value 1.0 
			   :show-value-p t :decimal-places 3
			   :client 'color :id 'red))
	       (setq green (make-pane 'slider
			     :label "Green" :foreground +green+
			     :orientation :vertical
			     :min-value 0.0 :max-value 1.0 
			     :show-value-p t :decimal-places 3
			     :client 'color :id 'green))
	       (setq blue (make-pane 'slider
			    :label "Blue" :foreground +blue+
			    :orientation :vertical
			    :min-value 0.0 :max-value 1.0 
			    :show-value-p t :decimal-places 3
			    :client 'color :id 'blue))))))
    (ihs (with-slots (intensity hue saturation) *application-frame*
	   (outlining ()
	     (horizontally ()
	       (setq intensity (make-pane 'slider
				 :label "Intensity"
				 :orientation :vertical
				 :min-value 0.0 :max-value (sqrt 3)
				 :show-value-p t :decimal-places 3
				 :client 'color :id 'intensity))
	       (setq hue (make-pane 'slider
			   :label "Hue"
			   :orientation :vertical
			   :min-value 0.0 :max-value 1.0 
			   :show-value-p t :decimal-places 3
			   :client 'color :id 'hue))
	       (setq saturation (make-pane 'slider
				  :label "Saturation"
				  :orientation :vertical
				  :min-value 0.0 :max-value 1.0 
				  :show-value-p t :decimal-places 3
				  :client 'color :id 'saturation)))))))
  (:layouts
    (default 
      (horizontally ()
	(outlining ()
	  (vertically () display exit))
	rgb ihs))))

(defmethod display-color ((frame color-chooser) stream)
  (with-bounding-rectangle* (left top right bottom) (window-viewport stream)
    (with-output-recording-options (stream :record nil)
      (draw-rectangle* stream left top right bottom
		       :filled t :ink (color frame)))))

(defmacro define-rgb-callbacks (color)
  (check-type color (member red green blue))
  (let* ((rgb '(red green blue))
	 (new-rgb (substitute 'value color rgb)))
    `(progn
       (defmethod value-changed-callback
		  ((slider slider) (client (eql 'color)) (id (eql ',color)) value)
	 (let ((frame (pane-frame slider)))
	   (multiple-value-bind (,@rgb) (color-rgb (color frame))
	     (declare (ignore ,color))
	     (setf (color frame) (make-rgb-color ,@new-rgb)))
	   (update-ihs frame)))
       (defmethod drag-callback
		  ((slider slider) (client (eql 'color)) (id (eql ',color)) value)
	 (let ((frame (pane-frame slider)))
	   (multiple-value-bind (,@rgb) (color-rgb (color frame))
	     (declare (ignore ,color))
	     (setf (color frame) (make-rgb-color ,@new-rgb)))
	   (update-ihs frame))))))

(define-rgb-callbacks red)
(define-rgb-callbacks green)
(define-rgb-callbacks blue)

(defmethod update-ihs ((frame color-chooser))
  (with-slots (intensity hue saturation) frame
    (multiple-value-bind (ii hh ss) (color-ihs (color frame))
      (setf (gadget-value intensity :invoke-callback nil) ii)
      (setf (gadget-value hue :invoke-callback nil) hh)
      (setf (gadget-value saturation :invoke-callback nil) ss))))

(defmacro define-ihs-callbacks (color)
  (check-type color (member intensity hue saturation))
  (let* ((ihs '(intensity hue saturation))
	 (new-ihs (substitute 'value color ihs)))
    `(progn
       (defmethod value-changed-callback
		  ((slider slider) (client (eql 'color)) (id (eql ',color)) value)
	 (let ((frame (pane-frame slider)))
	   (multiple-value-bind (,@ihs) (color-ihs (color frame))
	     (declare (ignore ,color))
	     (setf (color frame) (make-ihs-color ,@new-ihs)))
	   (update-rgb frame)))
       (defmethod drag-callback
		  ((slider slider) (client (eql 'color)) (id (eql ',color)) value)
	 (let ((frame (pane-frame slider)))
	   (multiple-value-bind (,@ihs) (color-ihs (color frame))
	     (declare (ignore ,color))
	     (setf (color frame) (make-ihs-color ,@new-ihs)))
	   (update-rgb frame))))))

(define-ihs-callbacks intensity)
(define-ihs-callbacks hue)
(define-ihs-callbacks saturation)

(defmethod update-rgb ((frame color-chooser))
  (with-slots (red green blue) frame
    (multiple-value-bind (rr gg bb) (color-rgb (color frame))
      (setf (gadget-value red :invoke-callback nil) rr)
      (setf (gadget-value green :invoke-callback nil) gg)
      (setf (gadget-value blue :invoke-callback nil) bb))))

(defmethod value-changed-callback :after ((slider slider) (client (eql 'color)) id value)
  (declare (ignore id value))
  (redisplay-frame-pane (pane-frame slider) 'display))


(defvar *color-choosers* nil)

(defun do-color-chooser (&key (port (find-port)) (force nil))
  (let* ((framem (find-frame-manager :port port))
	 (frame 
	   (let* ((entry (assoc port *color-choosers*))
		  (frame (cdr entry)))
	     (when (or force (null frame))
	       (setq frame (make-application-frame 'color-chooser
						   :frame-manager framem)))
	     (if entry 
		 (setf (cdr entry) frame)
		 (push (cons port frame) *color-choosers*))
	     frame)))
    (run-frame-top-level frame)
    (color frame)))

(define-demo "Color Chooser" do-color-chooser)
