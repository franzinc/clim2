;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: genera-frames.lisp,v 1.4 92/04/15 11:47:51 cer Exp $

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
	      (make-pane 'command-menu-pane
			 :display-function 
			   `(display-command-menu :command-table ,menu-bar)
			 :width :compute :height :compute))
	    pane))
	pane)))

(defmethod port-dialog-view ((port genera-port))
  +textual-dialog-view+)
  
;;--- Should "ungray" the command button, if there is one
(defmethod note-command-enabled ((framem genera-frame-manager) frame command)
  (declare (ignore frame command)))

;;--- Should "gray" the command button, if there is one
(defmethod note-command-disabled ((framem genera-frame-manager) frame command)
  (declare (ignore frame command)))

;;--- We can do better than this at some point
(defmethod port-notify-user ((port genera-port) message-string 
			     &key (style :inform)
				  (frame nil frame-p)
				  (associated-window
				    (if frame-p
					(frame-top-level-sheet frame)
					(find-graft :port port)))
				  (title "Notify user")
				  documentation
				  (exit-boxes '(:exit :abort :help))
				  (name title))
  (tv:notify nil message-string))
