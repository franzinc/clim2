;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: demo-driver.lisp,v 1.11 92/07/06 19:56:02 cer Exp $

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

(define-presentation-type demo-menu-item ())

(defun start-demo (&key (port (find-port)))
  (let ((*application-frame* 
	  (make-application-frame 'standard-application-frame
				  :frame-manager (find-frame-manager :port port))))
    (labels ((demo-menu-drawer (stream type &rest args)
	       (declare (dynamic-extent args))
	       (with-text-style (stream '(:serif :roman :large))
		 (apply #'draw-standard-menu stream type args)))
	     (demo-menu-choose (list)
	       (with-menu (menu (find-graft :port port))
		 (setf (window-label menu)
		       '("Clim Demonstrations" :text-style (:fix :bold :normal)))
		 (menu-choose-from-drawer
		   menu 'demo-menu-item
		   #'(lambda (stream type)
		       (demo-menu-drawer stream type list nil))
		   :cache nil
		   :unique-id 'demo-menu :id-test #'eql
		   :cache-value *demos* :cache-test #'equal))))
      (catch 'exit-demo
	(loop
	  (let* ((demo-name (demo-menu-choose (nreverse (map 'list #'car *demos*))))
		 (demo-fcn (cdr (assoc demo-name *demos* :test #'string-equal))))
	    (cond ((null demo-fcn))
		  ((functionp demo-fcn)
		   (funcall demo-fcn))
		  (t
		   (funcall demo-fcn :port port)))))))))

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


#-Symbolics
(progn

(define-application-frame demo-frame ()
    ((style :initform :graph))
  (:command-table (demo-frame :inherit-from (accept-values-pane)))
  (:panes
    (display :application
	     :width :compute :height :compute
	     :display-function 'graph-demos
	     :incremental-redisplay t
	     :scroll-bars :both)
    (options :accept-values
	     :width :compute :height :compute
	     :display-function '(accept-values-pane-displayer :displayer display-options)))
  (:menu-bar nil)
  (:layouts
    (default (vertically () display options))))

(defmethod display-options ((frame demo-frame) pane &key &allow-other-keys)
  (with-slots (style) frame
    (setf style (accept '(member :list :graph)
			:default style
			:stream pane
			:prompt "Style"))))

(defmethod graph-demos ((frame demo-frame) pane &key &allow-other-keys)
  (flet ((display-button (stream object)
	   (write-string (car object) excl::*initial-terminal-io*)
	   (write-string (car object) stream)
	   #+ignore
	   (with-output-as-gadget (stream)
	     (flet ((do-callback (gadget) 
		      (declare (ignore gadget))
		      (if (string= (car object) "Exit")
			  (frame-exit frame)
			  (clim-sys:make-process
			    (if (functionp (cdr object))
				(cdr object)
				#'(lambda () (funcall (cdr object))))
			    :name (car object)))))
	       (make-pane 'push-button 
		 :activate-callback #'do-callback
		 :label (car object))))))
    (with-slots (style) frame
      (updating-output (pane :unique-id 'foo :cache-value style)
	(ecase style
	  (:graph
	    (flet ((print-node (object stream)
		     (if (eq object :root)
			 (with-text-style (stream '(nil :bold :large))
			   (format stream "CLIM 2.0 demos"))
			 (display-button stream object)))
		   (get-children (object)
		     #+allegro (print object excl::*initial-terminal-io*)
		     (case object
		       ((:root) (copy-list *demos*))
		       (t nil))))
	      (format-graph-from-root :root #'print-node #'get-children
				      :stream pane)))
	  (:list
	    (formatting-item-list (pane :n-columns 1)
	      (dolist (demo *demos*)
		(formatting-cell (pane)
		  (write-string (car demo) pane)
		  #+ignore
		  (display-button pane demo))))))))))

(defun do-demos (&key (port (find-port)))
  (run-frame-top-level 
    (make-application-frame 'demo-frame
			    :frame-manager (find-frame-manager :port port))))

)	;#-Symbolics
