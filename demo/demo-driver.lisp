;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: demo-driver.lisp,v 1.20 92/12/03 10:28:38 cer Exp $

(in-package :clim-demo)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defvar *demos* nil)

(defmacro define-demo (name start-form)
  (if (symbolp start-form)
      `(clim-utils:push-unique (cons ,name ',start-form) *demos*
			       :test #'string-equal :key #'car)
      `(clim-utils:push-unique (cons ,name #'(lambda () ,start-form)) *demos*
			       :test #'string-equal :key #'car)))

(define-demo "Exit" (exit-demo))

(defun exit-demo () (throw 'exit-demo nil))

(define-demo "Test Suite" run-the-test-suite)

(defun run-the-test-suite (&key (port (find-port)) (force nil))
  (when (fboundp 'clim-user::do-test-suite)
    (clim-user::do-test-suite :port port :force force)))

(defun start-demo (&key (port (find-port)))
  (let* ((framem (typecase port
		   (frame-manager port)
		   (t (find-frame-manager :port port))))
	 (graft (typecase port
		  (frame-manager (graft port))
		  (t (find-graft :port port))))
	 (*application-frame* 
	   (make-application-frame 'standard-application-frame
	     :frame-manager framem))
	 (demos (sort (copy-list (map 'list #'car *demos*)) #'string<)))
    (catch 'exit-demo
      (loop
	(let* ((demo-name 
		 (menu-choose demos
			      :text-style '(:serif :roman :large)
			      :label '("Clim Demonstrations"
				       :text-style (:serif :bold :normal))
			      :associated-window graft
			      :cache nil
			      :unique-id 'demo-menu :id-test #'eql
			      :cache-value *demos* :cache-test #'equal))
	       (demo-fcn
		 (cdr (assoc demo-name *demos* :test #'string-equal))))
	  (cond ((null demo-fcn))
		((functionp demo-fcn)
		 (funcall demo-fcn))
		(t
		 (funcall demo-fcn :port port))))))))

(defparameter *color-stream-p* t)
(defun color-stream-p (stream)
  #-Genera *color-stream-p*		;--- kludge
  #+Genera (if (and stream
		    (eql (port-type (port stream)) ':genera))
	       (slot-value (port stream) 'genera-clim::color-p)
	       *color-stream-p*))

#+Genera
(cp:define-command (com-demonstrate-clim
		     :name "Demonstrate CLIM"
		     :command-table "Demonstration"
		     :provide-output-destination-keyword nil)
    ()
  (start-demo))
