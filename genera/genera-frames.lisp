;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: genera-frames.lisp,v 1.6 92/05/22 19:28:54 cer Exp $

(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defclass genera-frame-manager (standard-frame-manager)
    ())

(defmethod make-frame-manager ((port genera-port))
  (make-instance 'genera-frame-manager :port port))

(defmethod frame-wrapper ((framem genera-frame-manager) 
			  (frame standard-application-frame) pane)
  (let ((menu-bar (slot-value frame 'menu-bar)))
    (if menu-bar
	(with-look-and-feel-realization (framem frame)
	  (vertically ()
	    (outlining ()
	      ;;--- Incremental redisplay, too
	      (make-pane 'command-menu-pane
			 :display-function 
			   `(display-command-menu :command-table ,menu-bar)
			 :default-text-style clim-internals::*command-table-menu-text-style*
			 :width :compute :height :compute))
	    pane))
	pane)))

;;--- We can do better than this at some point
(defmethod frame-manager-notify-user
	   ((framem genera-frame-manager) message-string 
	    &key (style :inform)
		 (frame nil frame-p)
		 (associated-window
		   (if frame-p
		       (frame-top-level-sheet frame)
		       (graft framem)))
		 (title "Notify user")
		 documentation
		 (exit-boxes '(:exit :abort :help))
		 (name title))
  (tv:notify nil message-string))

(defmethod frame-manager-select-file 
	   ((framem genera-frame-manager) &rest options 
	    &key (frame nil frame-p)
		 (associated-window
		   (if frame-p
		       (frame-top-level-sheet frame)
		       (graft framem)))
		 (title "Select a file")
		 documentation
		 file-search-proc
		 directory-list-label
		 file-list-label
		 (exit-boxes '(:exit :abort :help))
		 (name title))
  )


;;; Pointer documentation and progress notes for Genera

(defmacro with-who-line-stream ((stream-var frame field) &body body)
  `(let ((,stream-var
	  (let* ((console (tv:sheet-console (sheet-mirror (frame-top-level-sheet ,frame))))
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

(defmethod frame-manager-display-pointer-documentation
	   ((framem genera-frame-manager)
	    frame presentation input-context window x y stream)
  (declare (ignore stream))
  (let ((stream
	  (let ((console (tv:sheet-console (sheet-mirror (frame-top-level-sheet frame)))))
	    (if (eq console sys:*main-console*)
		tv:who-line-documentation-window
		(let ((who-screen (tv:console-who-line-screen console)))
		  (and who-screen
		       (tv:get-who-line-field :mouse-documentation who-screen)))))))
    ;; The documentation should never say anything if we're not over a presentation
    (when (null presentation) 
      (scl:send stream :clear-window))
    ;; Cheap test to not do this work too often
    (let ((old-modifier-state clim-internals::*last-pointer-documentation-modifier-state*)
	  (modifier-state (clim-internals::window-modifier-state window))
	  (last-time clim-internals::*last-pointer-documentation-time*)
	  (time (get-internal-real-time)))
      (setq clim-internals::*last-pointer-documentation-modifier-state* modifier-state)
      (when (and (< time (+ last-time clim-internals::*pointer-documentation-interval*))
		 (= modifier-state old-modifier-state))
	(return-from clim-internals::frame-manager-display-pointer-documentation nil))
      (setq clim-internals::*last-pointer-documentation-time* time))
    (when presentation
      (setf (fill-pointer *pointer-documentation-buffer*) 0)
      (with-output-to-string (stream *pointer-documentation-buffer*)
	(when (null (clim-internals::frame-document-highlighted-presentation-1
		      frame presentation input-context window x y stream))
	  (setq clim-internals::*last-pointer-documentation-time* 0)))
      (scl:send stream :clear-window)
      (scl:send stream :string-out *pointer-documentation-buffer*))))
