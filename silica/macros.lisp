;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: macros.lisp,v 1.13 92/08/18 17:23:48 cer Exp $

(in-package :silica)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defmacro with-sheet-medium ((medium sheet) &body body)
  `(flet ((with-sheet-medium-body (,medium) ,@body))
     (declare (dynamic-extent #'with-sheet-medium-body))
     (invoke-with-sheet-medium ,sheet #'with-sheet-medium-body)))

(defmacro with-sheet-medium-bound ((sheet medium) &body body)
  `(flet ((with-sheet-medium-bound-body () ,@body))
     (declare (dynamic-extent #'with-sheet-medium-bound-body))
     (invoke-with-sheet-medium-bound ,sheet ,medium 
				     #'with-sheet-medium-bound-body)))

(defmacro with-temporary-medium ((medium sheet) &body body)
  (let ((sheet-var '#:sheet))
    `(let* ((,sheet-var ,sheet)
	    (,medium (allocate-medium (port ,sheet-var) ,sheet-var)))
       (unwind-protect
	   (progn ,@body)
	 (deallocate-medium (port ,sheet-var) ,medium)))))


(defmacro with-port-locked ((port) &body body)
  `(with-lock-held ((port-lock (port ,port)))
     ,@body))

(defmacro with-graft-locked ((graft) &body body)
  `(with-lock-held ((graft-lock (graft ,graft)))
     ,@body))


(defmacro with-look-and-feel-realization ((&optional frame-manager frame) &body forms)
  (when (and (null frame) (null frame-manager))
    (setq frame-manager `(frame-manager *application-frame*))
    (setq frame `*application-frame*))
  `(flet ((make-pane (pane-class &rest pane-options)
	    (declare (dynamic-extent pane-options))
	    (apply #'make-pane-1 
		   ,frame-manager ,frame pane-class pane-options)))
     (declare (dynamic-extent #'make-pane))
     #'make-pane			;prevent spurious compiler warnings
     ,@forms))

#+Genera (zwei:defindentation (make-pane 1 1))
(defun make-pane (pane-class &rest pane-options)
  (declare (ignore pane-class pane-options))
  (warn "~S not inside a call to ~S"
	'make-pane 'with-look-and-feel-realization))

