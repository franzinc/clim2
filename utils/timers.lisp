;; -*- mode: common-lisp; package: clim-utils -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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

(in-package :clim-utils)

;;;----  Perhaps this should be elsewhere
;;; Implementation of timers
;;; Its lame but its ugly.
;;; You can:
;;;   make-instance 'timer
;;;   queue-timer
;;;   delete-time

(defclass timer ()
	  ((delay :initarg :delay :reader timer-delay)
	   (repeat :initarg :repeat :initform nil :reader timer-repeat)
	   (function :initarg :function :reader timer-function)
	   (when :accessor timer-when))
  )

(defvar *timer-input-queue* (make-queue))

(defun add-timer (timer)
  (ensure-timer-process)
  (setf (timer-when timer) 
    (+ (get-internal-real-time) 
       (* internal-time-units-per-second
	  (timer-delay timer))))
  (queue-put *timer-input-queue* timer))

(defun delete-timer (timer)
  (queue-put *timer-input-queue* `(:delete ,timer)))

(defvar *timers* nil)

(defvar *timer-process* nil)

(defun queue-process-function ()
  (unwind-protect
      (progn
	(setq *timer-process* (Current-process))
	(flet ((add-timer-to-queue (timer)
		 (setq *timers* (merge 'list (list timer)  *timers* '< :key #'timer-when))))
	  (declare (dynamic-extent #'add-timer-to-queue))
	  (loop
	    (let ((head (car *timers*)))
	
	      (if head
		  (let* ((time-now (get-internal-real-time))
			 (time-to-wait (- (timer-when head) time-now)))
		    (process-wait-with-timeout
		     "Waiting"
		     (float (/ time-to-wait internal-time-units-per-second))
		     #'(lambda () (not (queue-empty-p *timer-input-queue*)))))
		(process-wait
		 "Waiting"
		 #'(lambda () (not (queue-empty-p *timer-input-queue*)))))

      
	      (loop
		(let ((head (queue-pop *timer-input-queue*)))
		  (etypecase head
		    (null (return nil))
		    (timer (add-timer-to-queue head))
		    (cons
		     (ecase (first head)
		       (:delete (setf *timers* (delete (second head) *timers*))))))))

	      (loop
		(unless *timers* (return nil))
		(let ((time-now (get-internal-real-time))
		      (timer (car *timers*)))
		  (if (< (timer-when timer) time-now)
		      (progn
			(pop *timers*)
			(funcall (timer-function timer) timer)
			(when (timer-repeat timer)
			  (setf (timer-when timer) 
			    (+ (get-internal-real-time) 
			       (* internal-time-units-per-second
				  (timer-repeat timer))))
			  (add-timer-to-queue timer)))
		    (return nil))))))))
    (setq *timer-process* nil)))

(defun ensure-timer-process ()
  (unless *timer-process*
    (make-process #'queue-process-function :name "Timer")))







