;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

;;; We have to interface to various foreign functions in the toolkit

(defclass application-context (ff:foreign-pointer)
  ((displays :initform nil :accessor application-context-displays)))

(defparameter *error-handler-function-address* nil)
(defparameter *warning-handler-function-address* nil)

(defmethod initialize-instance :after ((c application-context) &key context)
  (let ((context (or context (xt_create_application_context))))
    (setf (foreign-pointer-address c) context)
    (xt_app_set_error_handler
     context
     (or *error-handler-function-address*
	 (setq *error-handler-function-address*
	   (register-foreign-callable 'toolkit-error-handler))))
    (xt_app_set_warning_handler
     context
     (or *warning-handler-function-address*
	 (setq *warning-handler-function-address*
	   (register-foreign-callable 'toolkit-warning-handler))))))

(defun create-application-context ()
  (make-instance 'application-context))

(defparameter *large-connection-quantum* 20)

(defun open-display (&key (context (create-application-context))
			  (host nil)
			  (application-name "clim")
			  (application-class "Clim")
			  (options 0)
			  (num-options 0)
			  (argc 0)
			  (argv 0))
  (let ((d (with-ref-par ((argc argc :int))
	     (let ((temp (mp:process-quantum mp:*current-process*)))
	       (unwind-protect
		   (progn (setf (mp:process-quantum mp:*current-process*) *large-connection-quantum*)
			  (mp:process-allow-schedule)
			  (xt_open_display context
					   (if host
					       (lisp-string-to-string8 host)
					     0)
					   (lisp-string-to-string8 application-name)

					   (lisp-string-to-string8 application-class)
					   options
					   num-options &argc argv))
		 (setf (mp:process-quantum mp:*current-process*) temp)
		 (mp:process-allow-schedule))))))
    (if (zerop d)
	(error "cannot open the display: ~A" host))
    ;; Used for debugging:
    #+ignore
    (x11:xsynchronize d 1)
    d))

(defmethod initialize-instance :after ((d display) &rest args
				       &key display
				       &allow-other-keys)
  (push d (application-context-displays (slot-value d 'context)))
  (setf (foreign-pointer-address d)
    (or display
	(apply #'open-display args)))
  (register-address d))

(defun display-database (display)
  (make-instance 'resource-database
		 :foreign-address (xt_database display)))


(defun get-application-name-and-class (display)
  (with-ref-par ((name 0 *)
		 (class 0 *))
    (xt_get_application_name_and_class display &name &class)
    (values (excl:native-to-string name)
	    (excl:native-to-string class))))

