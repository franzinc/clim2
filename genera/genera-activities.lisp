;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :genera-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


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
    #'(lambda (window)
	(when (typep window 'genera-window)
	  (let* ((sheet (zl:symeval-in-instance window 'sheet))
		 (frame (and sheet (pane-frame sheet))))
	    (and frame (eq (frame-name frame) frame-name)))))
    #'(lambda (&rest initargs &key superior &allow-other-keys)
	(let* ((port (if (eq superior tv:main-screen)
			 (find-port)
			 (find-port :server-path `(:genera :screen ,superior))))
	       (frame (dolist (frame *deactivated-clim-activities*
			       ;; Didn't find one, so make a new one
			       (with-keywords-removed (initargs initargs '(:superior))
				 (apply #'make-application-frame frame-name
					:frame-manager (find-frame-manager :port port)
					:pretty-name cli::description
					(append initargs frame-args))))
			(when (eq (frame-name frame) frame-name)
			  (let ((sheet (frame-top-level-sheet frame)))
			    (when (eq (port sheet) port)
			      (setq *deactivated-clim-activities*
				    (delete frame *deactivated-clim-activities*))
			      (when (zerop (tv:sheet-dead 
					     (medium-drawable (sheet-medium sheet))))
				(return frame)))))))
	       (sheet (frame-top-level-sheet frame))
	       (window (medium-drawable (sheet-medium sheet))))
	  (stream-clear-input sheet)		;might have an Abort left in it
	  (scl:process-run-function (frame-pretty-name frame)
	    #'(lambda (frame window)
		(setf (tv:io-buffer-last-output-process (scl:send window :io-buffer))
		      scl:*current-process*)
		(unwind-protect 
		    (run-frame-top-level frame)
		  (when (zerop (tv:sheet-dead window))
		    (push frame *deactivated-clim-activities*))))
	    frame window)
	  window))
    :select :unless-created
    :force-create force-create
    :beep-if-only-one-selected beep-if-only-one-selected))

(scl:defmethod (cli::activity-frame-acceptable-p clim-activity) (window)
  (and (typep window 'genera-window)
       (let* ((sheet (zl:symeval-in-instance window 'sheet))
	      (frame (and sheet (pane-frame sheet))))
	 (and frame (eq (frame-name frame) frame-name)))))

(flavor:defmethod (x-screen::activity-acceptable-for-x-screen-p clim-activity) () t)

(scl:compile-flavor-methods clim-activity)


(defmacro define-genera-application (frame-name &body keys
				     &key pretty-name select-key
					  ;; Default edges and size for the frame
					  left top right bottom 
					  (width +fill+ width-p) (height +fill+ height-p)
				     &allow-other-keys)
  (declare (ignore width height))
  (with-keywords-removed (keys keys '(:pretty-name :select-key))
    `(define-genera-application-1
       ',frame-name ',pretty-name ',select-key
       ,@(unless width-p
	   (unless (and left right)
	     `(:width +fill+)))
       ,@(unless height-p
	   (unless (and top bottom)
	     `(:height +fill+)))
       ,@(copy-list keys))))

(defun define-genera-application-1 (frame-name pretty-name select-key &rest frame-args)
  (declare (dynamic-extent frame-args))
  (when (null pretty-name)
    (setq pretty-name (command-name-from-symbol frame-name)))
  (when (cli::define-activity 'clim-activity
			      :name pretty-name
			      :description pretty-name
			      :frame-name frame-name
			      :frame-args (copy-list frame-args))
    (when select-key
      (sys:set-select-key-activity select-key pretty-name))))
