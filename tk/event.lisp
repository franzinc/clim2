;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[Fri Aug 20 18:53:50 1993 by layer]-
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
;; $fiHeader: event.lisp,v 1.20 1993/08/31 04:54:47 layer Exp $

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
;;; xt_app_interval_next_timer will return 0 which would otherwise
;;; result in an indefinite block hence losing the timer event.
;;; If mp:wait-for-input-available returns nil - indicating time out -
;;; then mask is set by a further call to xt_app_pending. (cim)


(defun wait-for-event (context &key timeout wait-function)
  (let ((mask 0))
    (if (plusp (setq mask (xt_app_pending context)))
	(values mask nil)
      (wait-for-event-1 context timeout wait-function))))

(defun wait-for-event-1 (context timeout wait-function)
  (let* ((mask 0)
	 (fds (mapcar #'(lambda (display) (x11::display-fd display))
		      (application-context-displays context)))
	 result
	 (reason nil))
    (declare (fixnum mask))
    
    (flet ((wait-function (fd)
	     (declare (ignore fd))
	     (let ((*inside-event-wait-function* fds))
	       (setq result
		 (catch *inside-event-wait-function*
		   (or (plusp (setq mask (xt_app_pending context)))
		       (and wait-function
			    (funcall wait-function)
			    (setq reason :wait))))))))
      
      (let* ((interval (xt_app_interval_next_timer context))
	     (new-timeout (if (plusp interval)
			      (if (and timeout
				       (< (* timeout 1000) interval))
				  timeout
				(multiple-value-bind (sec msec)
				    (truncate interval 1000)
				  (cons sec msec)))
			    timeout)))
	(unless (mp:wait-for-input-available fds 
					     :wait-function #'wait-function
					     :timeout new-timeout)
	  (setq mask (xt_app_pending context)))))
    (values mask reason)))

(defun process-one-event (context mask reason)
  (cond ((plusp mask)

	 #+debug
	 (when (logtest mask *xt-im-xevent*)
	   (let ((event (x11:make-xevent)))
	     (unless (zerop (xt_app_peek_event context event))
	       (print (event-type event) excl:*initial-terminal-io*))))

	 (xt_app_process_event
	  context
	  ;; Because of a feature in the OLIT toolkit we need to
	  ;; give preference to events rather than timer events
	  (if (logtest mask *xt-im-xevent*) *xt-im-xevent* mask))
	 t)
	(reason :wait-function)
	(t :timeout)))

(defun-c-callable match-event-sequence-and-types ((display :unsigned-long)
						  (event :unsigned-long)
						  (arg :unsigned-long))
  ;; Arg points to a n element (unsigned-byte 32) vector, where the first
  ;; element is the display, the second is the sequence number, and
  ;; the other elements are the event types to be matched (null terminated).
  (let ((desired-display (sys:memref-int arg 0 0 :unsigned-long))
	(desired-sequence (sys:memref-int arg 4 0 :unsigned-long))
	(event-type (x11:xevent-type event)))
    (if (and (eql desired-display display)
	     (eql desired-sequence (x11:xanyevent-serial event))
	     (do* ((i 8 (+ i 4))
		   (desired-type (sys:memref-int arg i 0 :unsigned-long)
				 (sys:memref-int arg i 0 :unsigned-long)))
		 ((zerop desired-type) nil)
	       (if (eql desired-type event-type)
		   (return t))))
	1
      0)))

(defparameter *match-event-sequence-and-types-address*
    (register-function 'match-event-sequence-and-types))

(defun get-event-matching-sequence-and-types (display-object seq-no types
					      &key (block t))
  (unless (consp types)
    (setq types (list types)))
  (let ((display (object-display display-object))
	(data (make-array (+ 3 (length types))
			  :element-type '(unsigned-byte 32)))
	(i 2)
	(resulting-event (x11:make-xevent)))
    (declare (type (simple-array (unsigned-byte 32) (*)) data)
	     (fixnum i)) 
    (setf (aref data 0) (ff:foreign-pointer-address display))
    (setf (aref data 1) seq-no)
    (dolist (type types)
      (setf (aref data i) (position type tk::*event-types*))
      (incf i))
    (setf (aref data i) 0)
    (cond (block
	   (x11:xifevent display resulting-event
			 *match-event-sequence-and-types-address* data)
	   resulting-event)
	  ((zerop (x11:xcheckifevent display resulting-event
				     *match-event-sequence-and-types-address*
				     data))
	   nil)
	  (t
	   resulting-event))))


(defvar *event* nil)

(defun-c-callable event-handler ((widget :unsigned-long)
				 (client-data :unsigned-long)
				 (event :unsigned-long)
				 (continue-to-dispatch
				  :unsigned-long))
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

(defvar *event-handler-address* (register-function 'event-handler))

(defun add-event-handler (widget events maskable function &rest args)
  (xt_add_event_handler
   widget
   (encode-event-mask events)
   maskable
   *event-handler-address*
   (caar (push
	  (list (new-callback-id) (cons function args))
	  (widget-event-handler-data widget)))))

(defun build-event-mask (widget)
  (xt_build_event_mask widget))


