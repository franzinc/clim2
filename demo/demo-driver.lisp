;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: demo-driver.lisp,v 1.27 1993/09/22 21:21:15 cer Exp $

(in-package :clim-demo)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

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
       (clim-utils:push-unique demo
	 *demos* :test #'string-equal :key #'demo-name)
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
	(with-output-as-presentation (stream demo 'demo)
	  (format stream "~A~%" name))))))

(define-demo-driver-command (com-exit-demo-driver :menu t :name "Exit") ()
  (frame-exit *application-frame*))

(define-demo-driver-command (com-run-demo)
    ((demo 'demo :gesture :select))
  (mp:process-run-function (demo-name demo) 
			   #'run-demo demo
			   :port (port *application-frame*)))

(define-gesture-name :shift-select :pointer-button (:left :shift))

(define-demo-driver-command (com-force-demo)
    ((demo 'demo :gesture :shift-select))
  (mp:process-run-function (demo-name demo) 
			   #'run-demo demo
			   :port (port *application-frame*) :force t))

(defun run-demo (demo &key (port (find-port)) force)
  (let* ((entry (assoc port (demo-frames demo)))
	 (frame (cdr entry))
	 (activity-p (subtypep (demo-class demo) 'activity))
	 (*package* (find-package :clim-demo)))
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
	  (note-frame-deiconified (frame-manager frame) frame)
	  (raise-frame frame))
      (run-frame-top-level frame))))

(let ((demo (make-instance 'demo
	      :name "Demo Driver" :class 'demo-driver
	      :initargs '(:left 0 :top 0))))
  (defun start-demo (&rest args)
    (apply #'run-demo demo args)))


#+Genera
(cp:define-command (si:com-demonstrate-clim
		     :name "Demonstrate CLIM"
		     :command-table "Demonstration"
		     :provide-output-destination-keyword nil)
    ()
  (start-demo))
