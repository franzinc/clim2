;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader: genera-frames.lisp,v 1.2 92/03/04 16:22:45 cer Exp Locker: cer $

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
