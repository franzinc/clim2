
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
    
