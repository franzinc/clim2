;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Line Style

(define-protocol-class line-style ())

(defclass standard-line-style (line-style)
    ((unit :type (member :normal :point)
	   :initform :normal :initarg :unit
	   :reader line-style-unit)
     (thickness :type real
		:initform 1 :initarg :thickness
		:reader line-style-thickness)
     (joint-shape :type (member :miter :bevel :round :none)
		  :initform :miter :initarg :joint-shape
		  :reader line-style-joint-shape)
     (cap-shape :type (member :butt :square :round :no-end-point)
		:initform :butt :initarg :cap-shape
		:reader line-style-cap-shape)
     (dashes :initform nil :initarg :dashes
	     :reader line-style-dashes)
     #+++ignore (initial-dash-phase :initform 0 :initarg :initial-dash-phase
				    :reader line-style-initial-dash-phase)))

(defmethod print-object ((line-style standard-line-style) stream)
  (print-unreadable-object (line-style stream :type t :identity t)
    (with-slots (unit thickness joint-shape cap-shape dashes) line-style
      (format stream "Units ~(~A~), thickness ~D, joint ~(~A~), cap ~(~A~), dashes ~S"
	      unit thickness joint-shape cap-shape dashes))))

(defvar +default-line-style+ (make-instance 'standard-line-style))
(defvar +dashed-line-styles+
	(make-array 5 :initial-contents
		        (list (make-instance 'standard-line-style :thickness 0 :dashes t)
			      (make-instance 'standard-line-style :thickness 1 :dashes t)
			      (make-instance 'standard-line-style :thickness 2 :dashes t)
			      (make-instance 'standard-line-style :thickness 3 :dashes t)
			      (make-instance 'standard-line-style :thickness 4 :dashes t))))
(defvar +undashed-line-styles+
	(make-array 5 :initial-contents
		        (list (make-instance 'standard-line-style :thickness 0 :dashes nil)
			      (make-instance 'standard-line-style :thickness 1 :dashes nil)
			      (make-instance 'standard-line-style :thickness 2 :dashes nil)
			      (make-instance 'standard-line-style :thickness 3 :dashes nil)
			      (make-instance 'standard-line-style :thickness 4 :dashes nil))))

(defun-inline make-line-style-1 (unit thickness dashes joint-shape cap-shape)
  #+Genera (declare lt:(side-effects simple reducible))
  (if (and (eq unit :normal)
	   (eq joint-shape :miter)
	   (eq cap-shape :butt)
	   (integerp thickness) (<= 0 thickness 4)
	   (or (eq dashes t) (eq dashes nil)))
      ;; Cache the common case when only :DASHES and :THICKNESS are provided
      (svref (if dashes +dashed-line-styles+ +undashed-line-styles+) thickness)
      (make-instance 'standard-line-style
		     :unit unit :thickness thickness :dashes dashes
		     :joint-shape joint-shape :cap-shape cap-shape)))

(defun make-line-style (&key (unit :normal) (thickness 1) (dashes nil)
			     (joint-shape :miter) (cap-shape :butt))
  #+Genera (declare lt:(side-effects simple reducible))
  (make-line-style-1 unit thickness dashes joint-shape cap-shape))

(setq +highlighting-line-style+ (make-line-style :thickness 1))

(defmethod make-load-form ((line-style standard-line-style) &optional environment)
  (declare (ignore environment))
  (with-slots (unit thickness joint-shape cap-shape dashes) line-style
    `(make-line-style ,@(unless (eq unit :points) `(:unit ,unit))
		      ,@(unless (= thickness 1) `(:thickness ,thickness))
		      ,@(unless (eq joint-shape :miter) `(:joint-shape ,joint-shape))
		      ,@(unless (eq cap-shape :butt) `(:cap-shape ,cap-shape))
		      ,@(unless (eq dashes nil) `(:dashes ,dashes)))))


(defclass drawing-state-mixin
	  ()
    ((ink :initform +foreground-ink+ :accessor medium-ink)
     (transformation :initform +identity-transformation+ :accessor medium-transformation)
     (transformed-clipping-region :initform +everywhere+)
     (line-style :initform +default-line-style+ :accessor medium-line-style)
     (+y-upward-p :initform nil :accessor medium-+y-upward-p)))

(defmethod medium-clipping-region ((medium drawing-state-mixin))
  (with-slots (transformation transformed-clipping-region) medium
    (untransform-region transformation transformed-clipping-region)))

(defmethod (setf medium-clipping-region) (clipping-region (medium drawing-state-mixin))
  (with-slots (transformation transformed-clipping-region) medium
    (setf transformed-clipping-region (transform-region transformation clipping-region)))
  clipping-region)

;; NOTE: if you change the keyword arguments accepted by this method, you
;; also have to change the list of keywords in *ALL-DRAWING-OPTIONS*
(defmethod invoke-with-drawing-options
	   ((medium drawing-state-mixin) continuation
	    &key ink clipping-region transformation
		 line-style line-unit line-thickness (line-dashes nil dashes-p)
		 line-joint-shape line-cap-shape
		 (text-style nil text-style-p) (text-family nil text-family-p)
		 (text-face nil text-face-p) (text-size nil text-size-p))
  (with-slots ((medium-ink ink)
	       (medium-transformation transformation)
	       transformed-clipping-region
	       (medium-line-style line-style)) medium
    ;; Close the current output record if the drawing ink is changing
    (unless (eq medium-ink ink)
      (stream-close-text-output-record medium))
    (let* ((saved-ink medium-ink)
	   (saved-transformation medium-transformation)
	   (saved-clipping-region transformed-clipping-region)
	   (saved-line-style medium-line-style))
      (unwind-protect
	  (progn
	    (when ink
	      (setf medium-ink ink))
	    (when transformation
	      (setf medium-transformation
		    (compose-transformations saved-transformation transformation)))
	    (when clipping-region
	      (setf transformed-clipping-region
		    (region-intersection saved-clipping-region
					 (transform-region medium-transformation
							   clipping-region))))
	    (cond ((or line-unit line-thickness line-joint-shape line-cap-shape dashes-p)
		   (when (null line-style)
		     (setf line-style saved-line-style))
		   (setf medium-line-style
			 (make-line-style-1
			   (or line-unit (line-style-unit line-style))
			   (or line-thickness (line-style-thickness line-style))
			   (if dashes-p line-dashes (line-style-dashes line-style))
			   (or line-joint-shape (line-style-joint-shape line-style))
			   (or line-cap-shape (line-style-cap-shape line-style)))))
		  (line-style
		   (setf medium-line-style line-style)))
	    (when (or text-family-p text-face-p text-size-p)
	      (if text-style-p
		  (setq text-style (with-stack-list (style text-family text-face text-size)
				     (merge-text-styles style text-style)))
		  (setq text-style (make-text-style text-family text-face text-size)
			text-style-p t)))
	    (if text-style-p
		(flet ((call-continuation (stream)
			 (declare (ignore stream))
			 (funcall continuation)))
		  (declare (dynamic-extent #'call-continuation))
		  (invoke-with-text-style medium #'call-continuation text-style medium))
	        (funcall continuation)))
	(setf medium-line-style saved-line-style)
	(setf transformed-clipping-region saved-clipping-region)
	(setf medium-transformation saved-transformation)
	(setf medium-ink saved-ink)))))

(defmethod invoke-with-drawing-options
	   ((stream standard-encapsulating-stream) continuation
	    &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'invoke-with-drawing-options (slot-value stream 'stream) continuation args))
