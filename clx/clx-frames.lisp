;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: clx-frames.lisp,v 1.10 92/09/24 09:38:11 cer Exp $

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
