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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Header: /repo/cvs.copy/clim2/tk/event.lisp,v 1.28 1998/05/19 18:51:12 layer Exp $

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

(defun-c-callable match-event-sequence-and-types ((display :unsigned-long)
						  (event :unsigned-long)
						  (arg :unsigned-long))
  (declare (ignore arg))
  ;; Arg points to a n element (unsigned-byte 32) vector, where the first
  ;; element is the display, the second is the sequence number, and
  ;; the other elements are the event types to be matched (null terminated).
  (let* ((arg *sequence-matching-data*)
	 (desired-display (aref arg 0))
	 (desired-sequence (aref arg 1))
	 (event-type (x11:xevent-type event)))
    (declare (type (simple-array (unsigned-byte 32) (*)) arg))

    (if (and (eql desired-display display)
	     (eql desired-sequence (x11:xanyevent-serial event))
	     (do* ((i 2 (1+ i))
		   (desired-type (aref arg i) (aref arg i)))
		 ((zerop desired-type) nil)
	       (if (eql desired-type event-type)
		   (return t))))
	1
      0)))

(defparameter *match-event-sequence-and-types-address* nil)

(defvar *event-matching-event* (x11:make-xevent :in-foreign-space t))

(defun get-event-matching-sequence-and-types (display-object seq-no types
					      &key (block t))
  (unless (consp types)
    (setq types (list types)))
  (let* ((display (object-display display-object))
	 ;;-- This is pretty scarey. Heap allocated objects are passed
	 ;;-- to C which then passes them to lisp. If a GC happens we
	 ;;-- could be hosed
	 (data (make-array (+ 3 (length types))
			   :element-type '(unsigned-byte 32)))
	 (i 2)
	 (resulting-event *event-matching-event*)
	 (addr (or *match-event-sequence-and-types-address*
		   (setq *match-event-sequence-and-types-address*
		     (register-function 'match-event-sequence-and-types))))
	 (*sequence-matching-data* data))
    (declare (type (simple-array (unsigned-byte 32) (*)) data)
	     (fixnum i))
    (setf (aref data 0) (ff:foreign-pointer-address display))
    (setf (aref data 1) seq-no)
    (dolist (type types)
      (setf (aref data i) (position type tk::*event-types*))
      (incf i))
    (setf (aref data i) 0)
    (cond (block
	   (x11:xifevent display resulting-event addr 0)
	   resulting-event)
	  ((zerop (x11:xcheckifevent display resulting-event addr 0))
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

(defvar *event-handler-address* nil)

(defun add-event-handler (widget events maskable function &rest args)
  (xt_add_event_handler
   widget
   (encode-event-mask events)
   maskable
   (or *event-handler-address*
       (setq *event-handler-address* (register-function 'event-handler)))
   (caar (push
	  (list (new-callback-id) (cons function args))
	  (widget-event-handler-data widget)))))

(defun build-event-mask (widget)
  (xt_build_event_mask widget))


