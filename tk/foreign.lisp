;; -*- mode: common-lisp; package: tk -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: foreign.lisp,v 1.24.36.1.14.1 2000/08/11 00:03:54 cley Exp $

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
	   (register-function 'toolkit-error-handler))))
    (xt_app_set_warning_handler
     context
     (or *warning-handler-function-address*
	 (setq *warning-handler-function-address*
	   (register-function 'toolkit-warning-handler))))))

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

