;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: timers.lisp,v 2.7 2007/04/17 21:45:54 layer Exp $

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
	;; stop it asking us if it is safe to exit.
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
