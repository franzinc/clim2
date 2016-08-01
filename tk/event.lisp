;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defun simple-event-loop (context)
  (loop
    (multiple-value-bind (mask reason)
	(wait-for-event context)
      (process-one-event context mask reason))))

(defconstant *xt-im-xevent*		1)
(defconstant *xt-im-timer*		2)
(defconstant *xt-im-alternate-input*	4)
(defconstant *xt-im-all* (logior *xt-im-xevent*  *xt-im-timer*  *xt-im-alternate-input*))


;;; In wait-for-event, pending input is checked for before the process
;;; waits. This is necessary as if there are any timer events that
;;; have "occurred" but have not yet been processed then
;;; xt_app_interval_next_timer will return -1 which would otherwise
;;; result in an indefinite block hence losing the timer event.
;;; If mp:wait-for-input-available returns nil - indicating time out -
;;; then mask is set by a further call to xt_app_pending. (cim)


(defvar *inside-event-wait-function* nil)

(defun wait-for-event (context &key timeout wait-function)
  (let ((mask 0))
    (if (plusp (setq mask (xt_app_pending context)))
	(values mask nil)
      (wait-for-event-1 context timeout wait-function))))

(defun wait-for-event-1 (context timeout wait-function)
  (let* ((mask 0)
	 (fds (mapcar #'(lambda (display) (x11::display-fd display))
		      (application-context-displays context)))
	 (reason nil))
    (declare (fixnum mask))

    (flet ((wait-function (fd)
	     (declare (ignore fd))
	     (let ((*inside-event-wait-function* '#:wait-for-event))
	       (catch *inside-event-wait-function*
		 (or (plusp (setq mask (xt_app_pending context)))
		     (and wait-function
			  (funcall wait-function)
			  (setq reason :wait)))))))

      (let* ((interval (xt_app_interval_next_timer context))
	     (new-timeout (if (< interval 0)
			      timeout
			    ;; rewrite this so it doesn't cons!
			    (if (and timeout
				     (<= (* timeout 1000) interval))
				timeout
			      (multiple-value-bind (sec msec)
				  (truncate interval 1000)
				(cons sec msec))))))
	(unless (mp:wait-for-input-available fds
					     :wait-function #'wait-function
					     :timeout new-timeout)
	  (setq mask (xt_app_pending context)))))
    (values mask reason)))

(defun process-one-event (context mask reason)
  (cond ((plusp mask)
	 (xt_app_process_event context mask)
	 t)
	(reason :wait-function)
	(t :timeout)))

(defvar *sequence-matching-data*)

(defparameter *match-event-sequence-and-types-address* nil)

(defvar *event-matching-event* nil)

(defun make-xevent ()
  (clim-utils::allocate-cstruct 'x11::xevent :initialize t))

(defun event-matching-event ()
  (or *event-matching-event*
      ;; XXX/mp afuchs 2010-11-23: this isn't threadsafe.
      (setq *event-matching-event* (make-xevent))))

(def-foreign-type event-match-info (:struct (display (* :void))
                                            (seq-no :unsigned-long)
                                            (n-types :int)
                                            (event-types (:array :int 16))))

(defun-foreign-callable match-event-sequence-and-types-using-structure
    ((display :foreign-address)
     (event :foreign-address)
     (arg :foreign-address))
  (let* ((desired-display (fslot-value-typed 'event-match-info :c arg 'display))
	 (desired-sequence (fslot-value-typed 'event-match-info :c arg 'seq-no))
         (n-types (fslot-value-typed 'event-match-info :c arg 'n-types))
	 (event-type (x11:xevent-type event)))
    (if (and (eql desired-display display)
	     (eql desired-sequence (x11:xanyevent-serial event))
             (loop for i upfrom 0 below n-types
                   for desired-type = (fslot-value-typed 'event-match-info :c arg 'event-types i)
                   if (eql desired-type event-type)
                     do (return t)))
	1
        0)))

(defun get-event-matching-sequence-and-types (display-object seq-no types
					      &key (block t))
  (unless (consp types)
    (setq types (list types)))
  (assert (<= (length types) 16))
  (let* ((display (object-display display-object))
	 (resulting-event (event-matching-event))   ; XXX/mp: this isn't threadsafe (bind *e-m-e*?)
	 (addr (or *match-event-sequence-and-types-address*
		   (setq *match-event-sequence-and-types-address*
		     (register-foreign-callable 'match-event-sequence-and-types-using-structure)))))
    (let ((data (allocate-fobject 'event-match-info :c)))
      (unwind-protect
          (progn
            (setf (fslot-value-typed 'event-match-info :c data 'display) (ff:foreign-pointer-address display)
                  (fslot-value-typed 'event-match-info :c data 'seq-no) seq-no
                  (fslot-value-typed 'event-match-info :c data 'n-types) (length types))
            (loop for i from 0
                  for type in types
                  do (setf (fslot-value-typed 'event-match-info :c data 'event-types i)
                           (position type *event-types*)))
            (cond (block
                      (x11:xifevent display resulting-event addr data)
                    resulting-event)
                  ((zerop (x11:xcheckifevent display resulting-event addr data))
                   nil)
                  (t
                   resulting-event)))
        (free-fobject data)))))


(defvar *event* nil)

(defun-foreign-callable event-handler ((widget :foreign-address)
				       (client-data :foreign-address)
				       (event :foreign-address)
				       (continue-to-dispatch :foreign-address))
  (declare (ignore continue-to-dispatch))

  #+ignore (print (event-type event) excl:*initial-terminal-io*)

  (let* ((widget (find-object-from-address widget))
	 (eh-info (or (assoc client-data (widget-event-handler-data widget))
		      (error "Cannot find event-handler info ~S,~S"
			     widget client-data))))
    (destructuring-bind (ignore (fn &rest args))
	eh-info
      (declare (ignore ignore))
      (apply fn widget event args)
      0)))

(defvar *event-handler-address* nil)

(defun add-event-handler (widget events maskable function &rest args)
  (xt_add_event_handler
   widget
   (encode-event-mask events)
   maskable
   (or *event-handler-address*
       (setq *event-handler-address* (register-foreign-callable 'event-handler)))
   (caar (push
	  (list (new-callback-id) (cons function args))
	  (widget-event-handler-data widget)))))

(defun build-event-mask (widget)
  (xt_build_event_mask widget))


