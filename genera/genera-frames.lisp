;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defparameter *use-gadget-menu-bars* nil)
(defclass genera-frame-manager (standard-frame-manager)
    ((gadget-menu-bar :initarg :gadget-menu-bar :initform *use-gadget-menu-bars*))
  (:default-initargs :dialog-view +textual-dialog-view+))

(defmethod make-frame-manager ((port genera-port) 
			       &key palette (gadget-menu-bar *use-gadget-menu-bars*)
			       &allow-other-keys)
  (make-instance 'genera-frame-manager 
    :port port :palette palette :gadget-menu-bar gadget-menu-bar))

(defmethod frame-manager-matches-options-p
	   ((framem genera-frame-manager) port 
	    &key palette (gadget-menu-bar *use-gadget-menu-bars*) &allow-other-keys)
  (declare (ignore palette))
  (and (eq (port framem) port)
       (eq (slot-value framem 'gadget-menu-bar) gadget-menu-bar)))

(defmethod frame-wrapper ((framem genera-frame-manager) 
			  (frame standard-application-frame) pane)
  (let ((menu-bar (let ((menu-bar (slot-value frame 'menu-bar)))
		    (if (and (eq menu-bar 't)
			     (clim-internals::find-frame-pane-of-type 
			       frame 'clim-internals::command-menu-pane))
			nil
			menu-bar)))
	(menu-width (let ((geometry (clim-internals::frame-geometry frame)))
		      (or (getf geometry :width)
			  (let ((left  (getf geometry :left))
				(right (getf geometry :right)))
			    (and left right (- right left)))))))
    (with-look-and-feel-realization (framem frame)
      (outlining ()
	(if menu-bar
	    (vertically ()
	      (if (slot-value framem 'gadget-menu-bar)
		  (compute-menu-bar-pane frame menu-bar)
		  (outlining ()
		    (make-pane 'command-menu-pane
		      :display-function 
		        `(display-command-menu :command-table ,menu-bar
					       :max-width ,menu-width)
		      :incremental-redisplay t
		      :default-text-style clim-internals::*command-table-menu-text-style*
		      :text-style clim-internals::*command-table-menu-text-style*
		      :width :compute :height :compute)))
	      pane)
	    pane)))))

(defmethod frame-manager-exit-box-labels 
	   ((framem genera-frame-manager) frame view)
  (declare (ignore frame view))
  '((:exit   "<End> uses these values")
    (:abort  "<Abort> aborts")))

(defmethod frame-manager-exit-box-labels
	   ((framem genera-frame-manager) frame (view gadget-dialog-view))
  (declare (ignore frame))
  '((:exit   "Exit")
    (:abort  "Cancel")))


;;; Pointer documentation and progress notes for Genera

(defmacro with-who-line-stream ((stream-var frame field) &body body)
  `(let ((,stream-var
	  (let* ((console (tv:sheet-console (sheet-mirror (graft ,frame))))
		 (who-screen (if (eq console sys:*main-console*)
				 tv:who-line-screen
				 (tv:console-who-line-screen console))))
	    (and who-screen
		 (tv:get-who-line-field ,field)))))
     ,@body))

(defmethod frame-manager-clear-progress-note 
	   ((framem genera-frame-manager) (note clim-internals::progress-note))
  (with-who-line-stream (stream (slot-value note 'clim-internals::frame) :file-state)
    (when stream
      (scl:send stream :clear-window))))

(defmethod frame-manager-display-progress-note
	   ((framem genera-frame-manager) (note clim-internals::progress-note))
  (with-who-line-stream (stream (slot-value note 'clim-internals::frame) :file-state)
    (with-slots clim-internals::(name-displayed bar-length) note
      (when stream
	(let* ((stream-width (scl:send stream :inside-width))
	       (line-height  (- (scl:send stream :inside-height) 2))
	       (new-bar-length 
		 (floor (* stream-width (slot-value note 'clim-internals::numerator))
			(slot-value note 'clim-internals::denominator))))
	  (unless clim-internals::name-displayed
	    (scl:send stream :clear-window))
	  (unless clim-internals::name-displayed
	    (scl:send stream :set-cursorpos 0 0)
	    (scl:send stream :string-out (progress-note-name note))
	    (setq clim-internals::name-displayed t))
	  (when (< new-bar-length clim-internals::bar-length)
	    (scl:send stream :draw-rectangle
			     clim-internals::bar-length 2 0 line-height :erase))
	  (scl:send stream :draw-rectangle new-bar-length 2 0 line-height :draw)
	  (setq clim-internals::bar-length new-bar-length))))))


(defvar *pointer-documentation-buffer*
	(make-array 80 :element-type 'string-char :fill-pointer 0 :adjustable t))

(defmethod frame-manager-pointer-documentation-stream 
	   ((framem genera-frame-manager) frame stream)
  (declare (ignore stream))
  (let ((console (tv:sheet-console (sheet-mirror (graft frame)))))
    (if (eq console sys:*main-console*)
	tv:who-line-documentation-window
	(let ((who-screen (tv:console-who-line-screen console)))
	  (and who-screen
	       (tv:get-who-line-field :mouse-documentation who-screen))))))

(defmethod frame-manager-display-pointer-documentation
	   ((framem genera-frame-manager)
	    frame presentation input-context window x y stream)
  (let ((stream (frame-manager-pointer-documentation-stream framem frame stream)))
    (when presentation
      (setf (fill-pointer *pointer-documentation-buffer*) 0)
      (with-output-to-string (stream *pointer-documentation-buffer*)
	(when (null (clim-internals::frame-document-highlighted-presentation-1
		      frame presentation input-context window x y stream))
	  (setq clim-internals::*last-pointer-documentation-time* 0)))
      (scl:send stream :clear-window)
      (scl:send stream :string-out *pointer-documentation-buffer*))))

(defmethod frame-manager-display-pointer-documentation-string 
	   ((framem genera-frame-manager) frame stream string)
  (let ((stream (frame-manager-pointer-documentation-stream framem frame stream)))
    (scl:send stream :clear-window)
    (when string
      (scl:send stream :string-out string))))
