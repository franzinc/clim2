;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: clx-frames.lisp,v 1.7 92/07/01 15:45:54 cer Exp $

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defclass clx-frame-manager (standard-frame-manager)
    ()
  (:default-initargs :dialog-view +textual-dialog-view+))

(defmethod make-frame-manager ((port clx-port))
  (make-instance 'clx-frame-manager :port port))

(defmethod frame-wrapper ((framem clx-frame-manager) 
			  (frame standard-application-frame) pane)
  (let ((menu-bar (slot-value frame 'menu-bar)))
    (with-look-and-feel-realization (framem frame)
      (outlining ()
	(if menu-bar
	    (vertically ()
	      (outlining ()
		;;--- Incremental redisplay, too
		(make-pane 'command-menu-pane
		  :display-function 
		    `(display-command-menu :command-table ,menu-bar)
		  :incremental-redisplay t
		  :default-text-style clim-internals::*command-table-menu-text-style*
		  :text-style clim-internals::*command-table-menu-text-style*
		  :width :compute :height :compute))
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
		 (name title))
  )

(defmethod frame-manager-select-file 
	   ((framem clx-frame-manager) &rest options 
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
