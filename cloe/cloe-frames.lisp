;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


(defclass cloe-frame-manager (standard-frame-manager)
    ()
  (:default-initargs :dialog-view +textual-dialog-view+))

(defmethod make-frame-manager ((port cloe-port) &key palette)
  (make-instance 'cloe-frame-manager :port port :palette palette))

(defmethod frame-wrapper ((framem cloe-frame-manager) 
			  (frame standard-application-frame) pane)
  (let ((menu-bar (slot-value frame 'menu-bar)))
    (when (eq menu-bar 't)
      (setq menu-bar (frame-command-table frame)))
    (with-look-and-feel-realization (framem frame)
      (outlining ()
	(if menu-bar
	    (vertically ()
	      (compute-menu-bar-pane frame menu-bar)
	      pane)
	    pane)))))

;;--- We can do better than this
(defmethod frame-manager-notify-user
	   ((framem cloe-frame-manager) message-string 
	    &key (style :inform)
		 (frame nil frame-p)
		 (associated-window
		   (if frame-p
		       (frame-top-level-sheet frame)
		       (graft framem)))
		 (title "Notify user")
		 documentation
		 (exit-boxes '(:exit :abort :help))
		 (name title)
		 text-style)
  (declare (ignore style documentation name))
  (let ((stream associated-window))
    (accepting-values (stream :exit-boxes exit-boxes :label title
			      :own-window t)
      (with-text-style (stream text-style)
	(write-string message-string stream)))))

;;--- We can do better than this
(defmethod frame-manager-select-file 
	   ((framem cloe-frame-manager)
	    &key (default nil default-p)
		 (frame nil frame-p)
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
  (declare (ignore style documentation name
		   file-search-proc directory-list-label file-list-label))
  (let ((stream associated-window))
    (accepting-values (stream :exit-boxes exit-boxes :label title
			      :own-window t)
      (values
	(accept 'pathname :prompt "Enter a pathname"
		:stream stream
		:default default 
		:provide-default (not default-p))))))
