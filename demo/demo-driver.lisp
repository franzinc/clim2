;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: demo-driver.lisp,v 1.35.36.2 2002/02/08 19:11:22 layer Exp $

(in-package :clim-demo)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defvar *demos* nil)

(defclass demo ()
  ((name :reader demo-name :initarg :name)
   (class :reader demo-class :initarg :class)
   (initargs :reader demo-initargs :initarg :initargs)
   (frames :accessor demo-frames :initform nil)))

(defmacro define-demo (name class &rest initargs)
  (let ((do-name (clim-utils:fintern "~A-~A" 'do class)))
    `(let ((demo (make-instance 'demo 
		   :name ,name :class ',class :initargs ',initargs)))
       (clim-utils:push-unique demo *demos* :key #'demo-class)
       (defun ,do-name (&rest args)
	 (apply #'run-demo demo args)))))

(define-demo "Test Suite" clim-user::clim-tests :width 600 :height 420)

(define-application-frame demo-driver ()
    ()
  (:panes 
   (display :application :display-function 'display-all-demos
	    :display-time nil :scroll-bars nil
	    :text-cursor nil
	    :width :compute :height :compute
	    :end-of-line-action :allow
	    :end-of-page-action :allow))
  (:pointer-documentation t)
  (:layouts
   (default display)))

(defmethod display-all-demos ((frame demo-driver) stream &rest args)
  (declare (ignore args))
  (with-text-style (stream '(:serif :roman :large))
    (dolist (demo (sort (copy-list *demos*) #'string< :key #'demo-name))
      (let ((name (demo-name demo)))
	;; Attempt to gracefully recover from any error
	;; which occurs while printing the demos name.
	;; In particular, this can occur when listing
	;; the Japanese graphics-editor when the correct
	;; Japanese fonts are not accessible.	
	(let ((result (catch 'excl::printer-error
			(with-output-as-presentation (stream demo 'demo)
			  (format stream "~A~%" name))
			nil)))
	  (clim:with-output-recording-options (stream :draw nil :record nil)
	    (when (and result
		       (typep result 'error))
	      (format t "An error occured while displaying the demo: ~S~%(The demo will be removed from the menu.)~%~A"
		      (clim-demo::demo-class demo)
		      result)
	      nil
	      ))))))
  nil)

(define-demo-driver-command (com-exit-demo-driver :menu t :name "Exit") ()
  (frame-exit *application-frame*))

(define-demo-driver-command (com-run-demo)
    ((demo 'demo :gesture :select))
  (run-demo demo :port (port *application-frame*) :background t))

(define-gesture-name :shift-select :pointer-button (:left :shift))

(define-demo-driver-command (com-force-demo)
    ((demo 'demo :gesture :shift-select))
  (run-demo demo :port (port *application-frame*) :force t :background t))

(defun run-demo (demo &key (port (find-port)) force background)
  #+(and os-threads microsoft-32)
  (setq force t)
  (flet ((do-it ()
	   (let* ((entry (assoc port (demo-frames demo)))
		  (frame (cdr entry))
		  (activity-p (subtypep (demo-class demo) 'activity)))
	     (when (or force (null frame))
	       (setq frame (apply (if activity-p
				      #'make-instance
				    #'make-application-frame)
				  (demo-class demo)
				  :frame-manager (find-frame-manager :port port)
				  (demo-initargs demo))))
	     (if entry
		 (setf (cdr entry) frame)
	       (push (cons port frame) (demo-frames demo)))
	     (if (slot-value frame 'clim-internals::top-level-process)
		 (unless activity-p
		   (when (eq (frame-state frame) :shrunk)
		     (note-frame-deiconified (frame-manager frame) frame))
		   (raise-frame frame))
	       (run-frame-top-level frame)))))
    (if background 
	(mp:process-run-function 
	 `(:name ,(demo-name demo)
	   :initial-bindings ((*package* . ',*package*)))
	 #'do-it)
      (do-it))))
	
(defvar *demo-frame* nil)

(defun start-demo (&key (port (find-port)) 
			(background #+aclpc nil #-aclpc t) 
			force)
  (unless *demo-frame*
    (setq *demo-frame* 
      (make-instance 'demo
	:name "Demo Driver" :class 'demo-driver
	:initargs '(:left 0 :top 0))))
  (run-demo *demo-frame* :port port :background background :force force)
  #+(and os-threads microsoft-32)
  (setq *demo-frame* nil)
  )

