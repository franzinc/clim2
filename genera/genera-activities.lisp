;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: genera-activities.lisp,v 1.1 92/01/31 14:27:51 cer Exp $

(in-package :genera-clim)


"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader$

(defvar *sheet-roots* nil)
(defvar *deactivated-clim-activities* nil)

(scl:defflavor clim-activity
	(frame-name
	 (frame-args nil))
	(cli::basic-activity)
  (:initable-instance-variables frame-name frame-args)
  (:required-init-keywords :frame-name))

(scl:defmethod (sys:select-activity clim-activity)
	       (&key console superior force-create beep-if-only-one-selected)
  (cli::find-frame-for-activity console superior
    #'(lambda (clim-sheet)
	(when (typep clim-sheet 'genera-window)
	  (let* ((window (zl:symeval-in-instance clim-sheet 'sheet))
		 (frame (and window (window-stream-to-frame window))))
	    (and frame (eq (frame-name frame) frame-name)))))
    #'(lambda (&rest init-keywords &key superior &allow-other-keys)
	(let ((root (find superior *sheet-roots* :key #'(lambda (root)
							  (slot-value root 'window)))))
	  (unless root
	    (setq root (open-root-window :sheet :screen superior))
	    (push root *sheet-roots*))
	  (let* ((frame (dolist (frame *deactivated-clim-activities*
				       ;; Didn't find one, so make a new one
				       (with-keywords-removed (init-keywords init-keywords
							       '(:superior))
					 (apply #'make-application-frame frame-name
						:parent root
						:pretty-name cli::description
						(append init-keywords frame-args))))
			  (when (eq (frame-name frame) frame-name)
			    (let ((window (frame-top-level-sheet frame)))
			      (when (eq (window-parent window) root)
				(setq *deactivated-clim-activities*
				      (delete frame *deactivated-clim-activities*))
				(when (zerop (tv:sheet-dead (slot-value window 'window)))
				  (return frame)))))))
		 (window (frame-top-level-sheet frame))
		 (clim-sheet (slot-value window 'window)))
	    (stream-clear-input window)		;might have an Abort left in it
	    (scl:process-run-function (frame-pretty-name frame)
	      #'(lambda (frame clim-sheet)
		  (setf (tv:io-buffer-last-output-process (scl:send clim-sheet :io-buffer))
			scl:*current-process*)
		  (unwind-protect (run-frame-top-level frame)
		    (when (zerop (tv:sheet-dead clim-sheet))
		      (push frame *deactivated-clim-activities*))))
	      frame clim-sheet)
	    clim-sheet)))
    :select t
    :force-create force-create
    :beep-if-only-one-selected beep-if-only-one-selected))

(scl:defmethod (cli::activity-frame-acceptable-p clim-activity) (clim-sheet)
  (and (typep clim-sheet 'genera-window)
       (let* ((window (zl:symeval-in-instance clim-sheet 'sheet))
	      (frame (and window (window-stream-to-frame window))))
	 (and frame
	      (eql (frame-name frame) frame-name)))))

(flavor:defmethod (x-screen::activity-acceptable-for-x-screen-p clim-activity) () t)

(scl:compile-flavor-methods clim-activity)


(defmacro define-genera-application (frame-name &rest keys
				     &key pretty-name select-key
					  ;; Default edges and size for the frame
					  left top right bottom width height)
  (declare (ignore left top right bottom width height))
  (with-keywords-removed (keys keys '(:pretty-name :select-key))
    `(define-genera-application-1
       ',frame-name ',pretty-name ',select-key ',(copy-list keys))))

(defun define-genera-application-1 (frame-name pretty-name select-key &optional frame-args)
  (when (null pretty-name)
    (setq pretty-name (command-name-from-symbol frame-name)))
  (when (cli::define-activity 'clim-activity
			      :name pretty-name
			      :description pretty-name
			      :frame-name frame-name
			      :frame-args frame-args)
    (when select-key
      (sys:set-select-key-activity select-key pretty-name))))
