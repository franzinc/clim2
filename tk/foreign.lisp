(in-package :tk)

;;; We have to interface to various foreign functions in the toolkit

;; 



(defclass application-context (handle-class)
  ((displays :initform nil :accessor application-context-displays)))

(defparameter *error-handler-function-address*
  (register-function 'toolkit-error-handler))

(defparameter *warning-handler-function-address*
  (register-function 'toolkit-warning-handler))

(defmethod initialize-instance :after ((c application-context) &key context)
  (let ((context (or context (create_application_context))))
    (setf (slot-value c 'handle)
      context)
    (app_set_error_handler 
     context
     *error-handler-function-address*)
    (app_set_warning_handler
     context
     *warning-handler-function-address*)))

(defun create-application-context ()
  (make-instance 'application-context))

(defun open-display (&key (context (create-application-context))
			  (host nil)
			  (name "foo")
			  (class "Foo")
			  (options 0)
			  (num-options 0)
			  (argc 0)
			  (argv 0))
  (let ((d (with-ref-par ((argc argc))
	     (open_display (object-handle context)
			   (if host 
			       (string-to-char* host)
			     0)
			   (string-to-char* name)
			   (string-to-char* class)
			   options 
			   num-options
			   argc
			   argv))))
    (if (zerop d)
	(error "cannot open the display: ~A" host)
      d)))

(defclass display ()
  ((handle :reader display-handle)
   (context :initarg :context :reader display-context)))
  


(defmethod initialize-instance :after ((d display) &rest args &key
							      host display)
  (push d (application-context-displays (slot-value d 'context)))
  (setf (slot-value d 'handle)
	(or display
	    (apply #'open-display args)) ))



(defforeign 'string_create_l_to_r
    :entry-point "_XmStringCreateLtoR"
    :return-type :integer)

(defforeign 'string_get_l_to_r
    :entry-point "_XmStringGetLtoR"
    :return-type :integer)

(defforeign 'get_pixmap
    :entry-point "_XmGetPixmap")

(defforeign 'display_default_screen
    :entry-point "_XDefaultScreenOfDisplay")

(defforeign 'screen_white_pixel
    :entry-point "_XWhitePixelOfScreen")

(defforeign 'screen_black_pixel
  :entry-point "_XBlackPixelOfScreen")

(defun display-default-screen (display)
  (display_default_screen (display-handle display)))


