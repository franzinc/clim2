;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package :clim-internals)

;;; $fiHeader: $

"Copyright (c) 1991, 1992 Symbolics, Inc.  All rights reserved."

(defvar *progress-notes* ())
(defvar *current-progress-note*)

(defclass progress-note ()
    ((name   :accessor progress-note-name :initarg :name)
     (stream :initarg :stream)
     (frame  :initarg :frame)
     (numerator   :initform 0)
     (denominator :initform 1)
     ;; Keep the flicker down as much as possible
     (name-displayed :initform nil)
     (bar-length :initform 0)))
    
(define-constructor make-progress-note progress-note (name stream frame)
  :name name :stream stream :frame frame)

(defmethod (setf progress-note-name) :after (name (note progress-note))
  (declare (ignore name))
  (with-slots (name-displayed bar-length frame) note
    (setq name-displayed nil
	  bar-length 0)
    (frame-manager-display-progress-note (frame-manager frame) note)))

(defun add-progress-note (name stream)
  (check-type name string)
  (when (or (null stream) (eq stream 't))
    (setq stream (frame-pointer-documentation-output *application-frame*)))
  (let ((note (make-progress-note name stream *application-frame*)))
    (without-scheduling
      (push note *progress-notes*))
    note))

(defun remove-progress-note (note)
  (frame-manager-clear-progress-note (frame-manager (slot-value note 'frame)) note)
  (without-scheduling
    (setq *progress-notes* (delete note *progress-notes*))))

(defmethod progress-note-fraction-done ((note progress-note))
  (with-slots (numerator denominator) note
    (/ numerator denominator)))

(defmacro noting-progress ((stream name &optional (note-var '*current-progress-note*))
			   &body body)
  (check-type note-var symbol)
  `(let (,note-var)
     (unwind-protect
	 (progn
	   (setq ,note-var (add-progress-note ,name ,stream))
	   ,@body)
       (remove-progress-note ,note-var))))

(defun note-progress (numerator &optional (denominator 1) (note *current-progress-note*))
  (when note
    (when (and (= denominator 1) (rationalp numerator))
      (let ((num   (numerator   numerator))
	    (denom (denominator numerator)))
	(setq numerator   num 
	      denominator denom)))
    (setf (slot-value note 'numerator) numerator
	  (slot-value note 'denominator) denominator))
  (frame-manager-display-progress-note (frame-manager (slot-value note 'frame)) note)
  nil)

(defun note-progress-in-phases (numerator
				&optional (denominator 1)
					  (phase-number 1) (n-phases 1)
					  (note *current-progress-note*))
  (note-progress (+ (* denominator (1- phase-number)) numerator)
		 (* denominator n-phases)
		 note)
  nil)

(defmacro dolist-noting-progress ((var listform name
				   &optional stream (note-var '*current-progress-note*))
				  &body body)
  (let ((count-var '#:count)
	(total-var '#:total)
	(list-var '#:list))
    `(noting-progress (,stream ,name ,note-var)
       (let* ((,list-var ,listform)
	      (,total-var (length ,list-var))
	      (,count-var 0))
	 (dolist (,var ,list-var)
	   ,@body
	   (incf ,count-var)
	   (note-progress ,count-var ,total-var))))))

(defmacro dotimes-noting-progress ((var countform name
				    &optional stream (note-var '*current-progress-note*))
				   &body body)
  (let ((count-var '#:count))
    `(let ((,count-var ,countform))
       (noting-progress (,stream ,name ,note-var)
	 (dotimes (,var ,count-var)
	   ,@body
	   ;; We want the progress bar to advance after the first iteration
	   ;; even though DOTIMES is zero-based, so add one to the numerator.
	   (note-progress (1+ ,var) ,count-var))))))


(defmethod frame-manager-clear-progress-note 
	   ((framem standard-frame-manager) (note progress-note))
  (with-slots (stream) note
    (when stream
      (window-clear stream))))

(defmethod frame-manager-display-progress-note
	   ((framem standard-frame-manager) (note progress-note))
  (with-slots (name stream numerator denominator name-displayed bar-length) note
    (when stream
      (let* ((stream-width (bounding-rectangle-width stream))
	     (line-height  (stream-line-height stream))
	     (new-bar-length (floor (* stream-width numerator) denominator)))
	(unless name-displayed
	  (window-clear stream))
	(with-output-recording-options (stream :record nil)
	  (with-end-of-line-action (stream :allow)
	    (with-end-of-page-action (stream :allow)
	      (unless name-displayed
		(stream-set-cursor-position stream 0 0)
		(write-string name stream)
		(setq name-displayed t))
	      (when (< new-bar-length bar-length)
		(draw-line* stream 
			    0 (+ line-height 2) bar-length (+ line-height 2)
			    :line-thickness 2 :ink +background-ink+))
	      (draw-line* stream
			  0 (+ line-height 2) new-bar-length (+ line-height 2)
			  :line-thickness 2)
	      (setq bar-length new-bar-length)
	      (force-output stream))))))))
