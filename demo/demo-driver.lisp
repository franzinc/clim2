;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: demo-driver.lisp,v 1.10 92/07/06 18:52:03 cer Exp Locker: cer $

(in-package :clim-demo)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."

(defvar *demos* nil)

(defmacro define-demo (name start-form)
  `(clim-utils:push-unique (cons ,name #'(lambda () ,start-form)) *demos*
			   :test #'string-equal :key #'car))

(define-demo "Exit" (exit-demo))

(define-demo "Test Suite" (run-the-test-suite))

(defun run-the-test-suite ()
  (when (fboundp 'clim-user::do-test-suite)
    (clim-user::do-test-suite)))

(defun exit-demo () (throw 'exit-demo nil))

(define-presentation-type demo-menu-item ())

(defvar *demo-root* nil)

(defun start-demo (&optional (root *demo-root*))
  (setq *demo-root* (or root (setq root (find-graft))))
  ;;--- Make a dummy frame for the time being
  (let ((*application-frame* (make-application-frame 'standard-application-frame)))
    (labels ((demo-menu-drawer (stream type &rest args)
	       (declare (dynamic-extent args))
	       (with-text-style (stream '(:serif :roman :very-large))
		 (apply #'draw-standard-menu stream type args)))
	     (demo-menu-choose (list associated-window)
	       (with-menu (menu associated-window)
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
	  (let* ((demo-name (demo-menu-choose (nreverse (map 'list #'car *demos*)) root))
		 (demo-fcn (cdr (assoc demo-name *demos* :test #'string-equal))))
	    (when demo-fcn
	      (funcall demo-fcn))))))))

(defparameter *color-stream-p* t)
(defun color-stream-p (stream)
  #-Genera *color-stream-p*		;--- kludge
  #+Genera (if (and stream
		    (eql (port-type (port stream)) ':genera))
	       (slot-value stream 'clim-internals::color-p)
	       *color-stream-p*))


(define-application-frame demo-frame ()
			  ((style :initform :graph))
  (:panes
   (display
    :application
    :incremental-redisplay t
    :width :compute
    :height :compute
    :scroll-bars :both
    :display-function 'graph-demos)
   (options
    :accept-values
    :width :compute :height :compute
    :display-function '(accept-values-pane-displayer :displayer display-options)))
  (:menu-bar nil)
  (:layouts (default (vertically () display options))))



(defmethod display-options ((frame demo-frame) pane &key &allow-other-keys)
  (with-slots (style) frame
    (setf style (accept '(member :list :graph)
			:default style
			:stream pane
			:prompt "Style"))))

(defmethod graph-demos ((frame demo-frame) pane &key &allow-other-keys)
  (flet ((display-button (stream object)
	   (with-output-as-gadget (stream)
	     (flet ((do-callback (gadget) 
		      (declare (ignore gadget))
		      (if (string= (car object) "Exit")
			  (frame-exit frame)
			(mp::process-run-function 
			 (car object)
			 (cdr object)))))
	       (make-pane 'push-button 
			  :activate-callback #'do-callback
			  :label (car object))))))
    (with-slots (style) frame
      (updating-output (pane :unique-id 'foo :cache-value style)
	  (ecase style
	    (:graph
	     (flet ((print-node (object stream)
		      (if (eq object :root)
			  (with-text-style (stream '(nil :bold :huge))
			    (format stream "CLIM 2.0 demos"))
			(display-button stream object)))
		    (get-children (object)
		      (print object excl::*initial-terminal-io*)
		      (case object
			((:root) (copy-list *demos*))
			(t nil))))
	       (format-graph-from-root :root
				       #'print-node
				       #'get-children
				       :stream pane)))
	    (:list
	     (formatting-item-list (pane :n-columns 1)
		 (dolist (demo *demos*)
		   (formatting-cell (pane)
		       (write-string (car demo) pane)
		     #+ignore
		       (display-button pane demo))))))))))


(defun do-demos ()
  (run-frame-top-level (make-application-frame 'demo-frame)))
