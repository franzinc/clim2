(in-package :tk)

(defforeign 'app_pending 
    :return-type :fixnum
    :entry-point "_XtAppPending")

(defforeign 'app_process_event
    :entry-point "_XtAppProcessEvent")

(defun simple-event-loop (context)
  (loop 
      (process-one-event context)))

(defun process-one-event (context &key timeout wait-function)
  (let (mask
	(fds (mapcar #'(lambda (display)
			 (x11::display-fd (display-handle display)))
		     (application-context-displays context)))
	reason)
    
    (unwind-protect
	(progn (mapc #'multiprocessing::mpwatchfor fds)
	       (flet ((wait-function ()
				     (or (plusp (setq mask (app-pending context)))
					 (and wait-function
					      (funcall wait-function)
					      (setq reason :wait)))))
		     (if timeout
			 (multiprocessing:process-wait-with-timeout 
			  "Waiting for toolkit" 
			  timeout
			  #'wait-function)
		       (mp::process-wait 
			"Waiting for toolkit" 
			#'wait-function))))
      (mapc #'multiprocessing::mpunwatchfor fds))
    (cond ((plusp mask)
	   (app-process-event context mask)
	   t)
	  (reason :wait-function)
	  (t :timeout))))


(defun app-pending (context)
  (app_pending (object-handle context)))

(defun app-process-event (context mask)
  (app_process_event (object-handle context) mask))

(defforeign 'add_event_handler
    :entry-point "_XtAddEventHandler")

(defvar *event* nil)

(defun-c-callable event-handler ((widget :unsigned-long)
				 (client-data :unsigned-long)
				 (event :unsigned-long)
				 (continue-to-dispatch
				  :unsigned-long))
  (let* ((widget (find-object-from-address widget))
	 (eh-info (or (assoc client-data (widget-event-handler-data widget))
		      (error "Cannot find event-handler info ~S,~S"
			     widget client-data))))
    (destructuring-bind (ignore (fn &rest args))
	eh-info
      (declare (ignore ignore))
      (apply fn widget event args)
      0)))

(defvar *event-handler-address* (register-function 'event-handler))

(defun add-event-handler (widget events maskable function &rest args)
  (add_event_handler
   (object-handle widget)
   (encode-event-mask events)
   maskable
   *event-handler-address*
   (caar (push
	  (list (new-callback-id) (cons function args))
	  (widget-event-handler-data widget)))))

(defforeign 'xt_build_event_mask 
    :entry-point "_XtBuildEventMask")

(defun build-event-mask (widget)
  (xt_build_event_mask (object-handle widget)))


