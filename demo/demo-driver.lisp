;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: demo-driver.lisp,v 1.8 92/06/03 18:18:45 cer Exp $

(in-package :clim-demo)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."

(defvar *demos* nil)

(defmacro define-demo (name start-form)
  `(clim-utils:push-unique (cons ,name #'(lambda () ,start-form)) *demos*
			   :test #'string-equal :key #'car))

(define-demo "Exit" (exit-demo))

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
