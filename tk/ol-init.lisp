;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: ol-init.lisp,v 1.5 92/02/16 20:55:04 cer Exp $


(in-package :tk)

(defforeign 'ol_toolkit_initialize :entry-point "_OlToolkitInitialize")

(defun ol-initialize ()
  (ol_toolkit_initialize))

(defun-c-callable ol-error-handler ((message :unsigned-long))
  (error "toolkit error: ~A"
	 (char*-to-string message)))


(defun-c-callable ol-warning-handler ((message :unsigned-long))
  (warn "toolkit error: ~A"
	(char*-to-string message)))

(defun-c-callable ol-error-va-handler ((message :unsigned-long))
  (error "toolkit error: ~A"
	 (char*-to-string message)))


(defun-c-callable ol-warning-va-handler ((message :unsigned-long))
  (warn "toolkit error: ~A"
	(char*-to-string message)))


(defforeign 'ol_set_warning_handler 
    :entry-point "_OlSetWarningHandler")

(defforeign 'ol_set_error_handler 
  :entry-point "_OlSetErrorHandler")

(defforeign 'ol_set_va_display_error_msg_handler
    :entry-point "_OlSetVaDisplayErrorMsgHandler")

(defforeign 'ol_set_va_display_warning_msg_handler
    :entry-point "_OlSetVaDisplayWarningMsgHandler")

(ol_set_warning_handler (register-function 'ol-warning-handler))
(ol_set_va_display_warning_msg_handler (register-function 'ol-warning-va-handler))
(ol_set_error_handler (register-function 'ol-error-handler))
(ol_set_va_display_error_msg_handler (register-function 'ol-error-va-handler))



(defvar *ol-done* nil)

(unless *ol-done*
  (ol-initialize)
  (define-toolkit-classes *openlook-classes*)
  (setq *ol-done* t))

(defmethod make-widget ((w event) 
			&rest args &key parent (managed t) (name "") &allow-other-keys)
  (remf :managed args)
  (remf :name args)
  (remf :parent args)
  (let ((class (class-of w)))
    (if managed
	(apply #'create-managed-widget name class parent args)
      (apply #'create-widget name class parent args))))

(add-resource-to-class (find-class 'menu-button)
			   (make-instance 'resource
					  :name :menu-pane
					  :type 'widget
					  :original-name 
					  (string-to-char*
					   "menuPane")))

(add-resource-to-class (find-class 'abbrev-menu-button)
		       (make-instance 'resource
				      :name :menu-pane
				      :type 'widget
				      :original-name 
				      (string-to-char*
				       "menuPane")))


;(setq shell (intern-widget shell (widget-class-of shell)))
;
;(defforeign 'xt_display :entry-point "_XtDisplay")
;(defforeign 'xt_display_to_application_context
;    :entry-point "_XtDisplayToApplicationContext")
;
;(setf (slot-value shell 'display)
;      (let* ((d (xt_display (object-handle shell)))
;	     (c (xt_display_to_application_context d)))
;	(intern-widget d 'display 
;		       :display d
;		       :context 
;		       (make-instance 'application-context :context c))))
    
