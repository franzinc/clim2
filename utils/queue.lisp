;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: queue.lisp,v 1.10 2002/07/09 20:57:19 layer Exp $

;;;
;;; Copyright (c) 1989 by Xerox Corporations.  All rights reserved.
;;;
;;; This code was taken from the ARIA code which is copyrighted by MCC, Xerox,
;;; and Franz, Inc.   It was further modified by Ramana Rao.    
;;;

(in-package :clim-utils)

;;;
;;;
;;;    Implementation of simple variable length queues - fifo & lifo
;;;
;;;

(defclass queue ()
    ((head :initform nil)
     (tail :initform nil)
     (free-list :initform nil)))

(define-constructor make-queue queue () )

(defmacro queue-head (queue)
  `(slot-value ,queue 'head))

(defmacro queue-tail (queue)
  `(slot-value ,queue 'tail))

(defmethod print-object ((queue queue) stream)
  "Print a queue object"
  (print-unreadable-object (queue stream :type t :identity t)
    (format stream "element-type: ~A" (queue-contents-type queue))))

;;;
;;; Utility Macros
;;;

(defmacro get-free-cons (queue)
  ;; get the first cons off the free list or a new cons if none
  `(rplacd
     (prog1
         (or
           (slot-value ,queue 'free-list)
           (cons nil nil))
       (setf (slot-value ,queue 'free-list) 
             (cdr (slot-value ,queue 'free-list))))
     nil))
          
(defmacro free-cons (queue cons)
  ;; add a cons cell to the list of free cons cells
  `(prog1
       (cdr ,cons)
     (rplacd ,cons (slot-value ,queue 'free-list))
     (rplaca ,cons nil)
     (setf (slot-value ,queue 'free-list) ,cons)))

;;;
;;; external queue operations
;;;

(defmethod queue-contents-type ((queue queue))
  "return t to indicate that a simple queue stores anything"
  t)

(defmethod queue-size ((queue queue))
  "return nil to indicate that this queue is variable length"
  nil)

(defmethod queue-length ((queue queue))
  "Return an integer indicating the number of items on this queue"
  (length (queue-head queue)))

(defmethod queue-contents-list ((queue queue))
  "return a list of all the queue contents"
  (copy-list (queue-head queue)))

(defgeneric map-over-queue (function queue)
  (declare (dynamic-extent function)))

(defmethod map-over-queue (function (queue queue))
  (declare (dynamic-extent function))
  (mapc function (queue-head queue)))

(defmethod queue-next ((queue queue))
  "return the head element of the queue 
   without modifying the queue"
  (car (queue-head queue)))

(defmethod queue-last ((queue queue))
  "return the last element of the queue
   without modifying the queue"
  (car (queue-tail queue)))

(defmethod queue-flush ((queue queue))
  "delete all elements from the queue"
  (do ()
      ((null (queue-head queue)))
    (setf (queue-head queue) (free-cons queue (queue-head queue))))
  (setf (queue-tail queue) nil))

(defmethod queue-empty-p ((queue queue))
  (null (queue-head queue)))

(defmethod queue-full-p ((queue queue))
  "return nil to indicate that varibale length queues are never empty"
  nil)

(defmethod queue-put ((queue queue) item)
  "put a new element at the tail of the fifo queue
   or at the head of the lifo queue"
  (let ((new-item (rplaca (get-free-cons queue) item)))
    (if (queue-empty-p queue)
        (psetf (queue-head queue) new-item
               (queue-tail queue) new-item)
        (progn
          (rplacd (queue-tail queue) new-item)
          (setf (queue-tail queue) new-item)))
    queue))
      
(defmethod queue-get ((queue queue) &optional default)
  "return the element at the head of the queue
   deleteing it from the queue"
  (if (queue-empty-p queue)
      default
      (prog1                                
          (queue-next queue)
        (setf (queue-head queue) (free-cons queue (queue-head queue))))))

(defmethod queue-unget ((queue queue) item)
  ;;--- Eventually this will check to see that the item being ungotten
  ;;--- is the same as the last gotten item.
  (let ((new-item (rplaca (get-free-cons queue) item)))
    (if (queue-empty-p queue)
        (psetf (queue-head queue) new-item
               (queue-tail queue) new-item)
        (psetf (cdr new-item) (queue-head queue)
               (queue-head queue) new-item))))

(defmethod queue-push ((queue queue) item)
  "put a new element at the tail of the fifo queue
   or at the head of the lifo queue"
  (let ((new-item (rplaca (get-free-cons queue) item)))
    (if (queue-empty-p queue)
        (setf (queue-head queue) new-item
              (queue-tail queue) new-item)
        (progn
          (rplacd new-item (queue-head queue))
          (setf (queue-head queue) new-item)))
    queue))

(defmethod queue-pop ((queue queue) &optional default)
  "return the element at the head of the queue
   deleteing it from the queue"
  (if (queue-empty-p queue)
      default
      (prog1                                
          (queue-next queue)
        (setf (queue-head queue) (free-cons queue (queue-head queue))))))

;;;
;;;     Locking Queues
;;;

(defclass locking-queue (queue)
    ((lock-place :initform (make-lock "a queue lock"))))
  
(define-constructor make-locking-queue queue () )

(defmacro with-queue-locked (queue &body body)
  #+ccl (declare (ignore queue))
  #+ccl `(progn ,@body)
  #-ccl
  `(with-slots (lock-place) ,queue
     (with-lock-held (lock-place "Queue lock") 
       ,@body)))

(defmethod queue-length ((queue locking-queue))
  (with-queue-locked queue 
    (call-next-method)))

(defmethod queue-contents-list ((queue locking-queue))
  (with-queue-locked queue 
    (call-next-method)))

(defmethod map-over-queue (function (queue locking-queue))
  #-aclpc (declare (ignore function))
  (with-queue-locked queue 
    (call-next-method)))

(defmethod queue-flush ((queue locking-queue))
  (with-queue-locked queue 
    (call-next-method)))

(defmethod queue-put ((queue locking-queue) item)
  #-aclpc (declare (ignore item))
  (with-queue-locked queue 
    (call-next-method)))

(defmethod queue-get ((queue locking-queue) &optional default)
  #-aclpc (declare (ignore default))
  (with-queue-locked queue 
    (call-next-method)))

(defmethod queue-push ((queue locking-queue) item)
  #-aclpc (declare (ignore item))
  (with-queue-locked queue 
    (call-next-method)))

(defmethod queue-pop ((queue locking-queue) &optional default)
  #-aclpc (declare (ignore default))
  (with-queue-locked queue 
    (call-next-method)))




