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
;; $fiHeader$


(in-package :tk)

(defforeign 'ol_initialize :entry-point "_OlInitialize")

(defun ol-initialize (&key (shell-name "foo")
			   (application-class "foo")
			   )
  (with-ref-par ((argc 0)
		 (argv 0))
		(ol_initialize
		 (string-to-char* shell-name)
		 (string-to-char* application-class)
		 0 ;; options
		 0;; num-options
		 argc ;; argc
		 argv ;; argv
		 )))

(defun-c-callable ol-error-handler ((message :unsigned-long))
  (error "toolkit error: ~A"
	 (char*-to-string message)))


(defun-c-callable ol-warning-handler ((message :unsigned-long))
  (warn "toolkit error: ~A"
	(char*-to-string message)))

(defforeign 'ol_set_warning_handler 
    :entry-point "_OlSetWarningHandler")

(defforeign 'ol_set_error_handler 
  :entry-point "_OlSetErrorHandler")


(ol_set_warning_handler (register-function 'ol-warning-handler))
(ol_set_error_handler (register-function 'ol-error-handler))

(setq shell (ol-initialize))

(defvar *done* nil)
(defvar *n-classes*)

(unless *done*
  (setq *n-classes* (insert_classes))
  (make-classes *n-classes*)
  (setq *done* t))


(setq shell (intern-widget shell (widget-class-of shell)))

(defforeign 'xt_display :entry-point "_XtDisplay")
(defforeign 'xt_display_to_application_context
    :entry-point "_XtDisplayToApplicationContext")

(setf (slot-value shell 'display)
      (let* ((d (xt_display (object-handle shell)))
	     (c (xt_display_to_application_context d)))
	(intern-widget d 'display 
		       :display d
		       :context 
		       (make-instance 'application-context :context c))))
    
