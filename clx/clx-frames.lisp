;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader: clx-frames.lisp,v 1.2 92/02/24 13:23:38 cer Exp $


(defclass clx-frame-manager (standard-frame-manager)
    ())

(defmethod make-frame-manager ((port clx-port))
  (make-instance 'clx-frame-manager :port port))

(defmethod frame-wrapper ((framem clx-frame-manager) 
			  (frame standard-application-frame) pane)
  (let ((menu-bar (slot-value frame 'menu-bar)))
    (if menu-bar
	(with-look-and-feel-realization (framem frame)
	  (vertically ()
	    (outlining ()
	      (realize-pane 'command-menu-pane
			    :display-function 
			      `(display-command-menu :command-table ,menu-bar)
			    :width :compute :height :compute))
	    pane))
	pane)))

(defmethod port-dialog-view ((port clx-port))
  +textual-dialog-view+)
  
;;--- Should "ungray" the command button, if there is one
(defmethod note-command-enabled ((framem clx-frame-manager) frame command)
  (declare (ignore frame command)))

;;--- Should "gray" the command button, if there is one
(defmethod note-command-disabled ((framem clx-frame-manager) frame command)
  (declare (ignore frame command)))

