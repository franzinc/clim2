;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: demo-driver.lisp,v 1.23 93/01/11 15:44:58 colin Exp $

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

(defun start-demo (&key (port (find-port)) force)
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
		 (funcall demo-fcn :port port :force force))))))))

#+Genera
(cp:define-command (si:com-demonstrate-clim
		     :name "Demonstrate CLIM"
		     :command-table "Demonstration"
		     :provide-output-destination-keyword nil)
    ()
  (start-demo))
