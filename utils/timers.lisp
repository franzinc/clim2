;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1992 Franz, Inc.  All rights reserved."

;;; Implementation of timers
;;; It's lame but it's ugly

(defclass timer ()
    ((delay    :initarg :delay    :initform 0   :reader timer-delay)
     (interval :initarg :interval :initform nil :reader timer-interval)
     (function :initarg :function :initform nil :reader timer-function)
     (when :accessor timer-when)))

(defvar *timers* nil)
(defvar *timer-process* nil)
(defvar *timer-input-queue* (make-locking-queue))

(defun make-timer (&key (delay 0) interval function)
  (make-instance 'timer
    :delay delay :interval interval :function function))

(defun add-timer (timer)
  (ensure-timer-process)
  (setf (timer-when timer) 
        (+ (get-internal-real-time) 
           (* internal-time-units-per-second
              (timer-delay timer))))
  (queue-put *timer-input-queue* timer)
  timer)

(defun delete-timer (timer)
  (queue-put *timer-input-queue* `(:delete ,timer)))

(defun timer-queue-process-function ()
  (unwind-protect
      (progn
        (setq *timer-process* (current-process))
	;; stop it asking us if it is safe to exit. rfe8293
	#+(and allegro (version>= 9 0))
	(setf (mp::process-keeps-lisp-alive-p (current-process)) nil)
	#+allegro (setf (mp::process-interruptible-p (current-process)) nil)
        (flet ((add-timer-to-queue (timer)
                 (without-scheduling
                   (setq *timers* (merge 'list (list timer) *timers* #'<
                                         :key #'timer-when)))))
          (declare (dynamic-extent #'add-timer-to-queue))
          (loop
            (let ((head (car *timers*)))
              (if head
                  (let* ((time-now (get-internal-real-time))
                         (time-to-wait (- (timer-when head) time-now)))
                    (process-wait-with-timeout "Waiting"
                      (float (/ time-to-wait internal-time-units-per-second) 0f0)
                      #'(lambda () (not (queue-empty-p *timer-input-queue*)))))
                  (process-wait "Waiting"
                    #'(lambda () (not (queue-empty-p *timer-input-queue*))))))
            (loop
              (let ((head (queue-pop *timer-input-queue*)))
                (etypecase head
                  (null (return nil))
                  (timer (add-timer-to-queue head))
                  (cons
                    (ecase (first head)
                      (:delete 
                        (without-scheduling
                          (setf *timers* (delete (second head) *timers*)))))))))
            (loop
              (unless *timers* (return nil))
              (let ((time-now (get-internal-real-time))
                    (timer (car *timers*)))
                (if (< (timer-when timer) time-now)
                    (progn
                      (pop *timers*)
                      (when (timer-function timer)
                        (funcall (timer-function timer) timer))
                      (when (timer-interval timer)
                        (setf (timer-when timer) 
                              (+ (get-internal-real-time) 
                                 (* internal-time-units-per-second
                                    (timer-interval timer))))
                        (add-timer-to-queue timer)))
                    (return nil)))))))
    (setq *timer-process* nil)))

(defun ensure-timer-process ()
  (unless *timer-process*
    (make-process #'timer-queue-process-function :name "Timer")))

#+Genera
(scl:add-initialization "Flush timers"
  '(progn
     (when *timer-process*
       (destroy-process *timer-process*)
       (setq *timer-process* nil))
     (setq *timers* nil)
     (queue-flush *timer-input-queue*))
  '(before-cold))
