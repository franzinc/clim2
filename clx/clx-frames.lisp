;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: clx-frames.lisp,v 1.9 92/08/18 17:24:21 cer Exp $

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defclass clx-frame-manager (standard-frame-manager)
    ()
  (:default-initargs :dialog-view +textual-dialog-view+))

(defmethod make-frame-manager ((port clx-port) &key)
  (make-instance 'clx-frame-manager :port port))

(defmethod frame-wrapper ((framem clx-frame-manager) 
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

(defmethod frame-manager-notify-user
	   ((framem clx-frame-manager) message-string 
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
	   ((framem clx-frame-manager)
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
