;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: db-box.lisp,v 1.20 92/08/18 17:23:26 cer Exp Locker: cer $

(in-package :silica)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 Xerox Corp.  All rights reserved."


;;;
;;; Boxing Panes
;;;



(defclass box-pane (layout-pane)
    ((space :initform 1 :initarg :spacing)
     contents))

(defmethod initialize-instance :after 
	   ((pane box-pane) &key contents &allow-other-keys)
  (dolist (child contents)
    (etypecase child
      (number)
      ((member :fill))
      (pane (sheet-adopt-child pane child))
      (cons
	;; Handle top-down layout syntax
	(unless (and (typep (first child) '(or (member :fill) (real 0 1)))
		     (panep (second child)))
	  (error "Invalid box child: ~S" contents))
	(sheet-adopt-child pane (second child)))))
  (setf (slot-value pane 'contents) contents))

(defmethod handle-event :after ((pane box-pane) (event pointer-motion-event))
  (deallocate-event event))

(defmethod compose-box 
	   ((box-pane box-pane)
	    fn-major fn-major+ fn-major- fn-minor fn-minor+ fn-minor- width-or-height
	    keys)
  (with-slots (contents space) box-pane
    (if (null contents) 
	(make-space-requirement)
      (let ((major 0)
	    (major+ 0)
	    (major- 0)
	    (minor 0)  
	    minor+ minor-
	    (minor-min 0)
	    ;;--- is this safe?
	    (minor-max most-positive-fixnum)
	    scale)
	(dolist (entry contents)
	  (cond ((eq entry :fill) (incf major+ +fill+))
		((numberp entry) (incf major entry))
		(t
		 (if (consp entry)
		     (psetf entry (second entry) 
			    scale (let ((n (first entry)))
				    (if (numberp n) (float n) n)))
		     (setf scale 1.0))
		 (let ((space-req (apply #'compose-space entry width-or-height)))
		   (flet ((scale (x) 
			    (if (and (numberp scale) (< x +fill+)) (/ x scale) x)))
		     (declare (dynamic-extent #'scale))
		     (cond ((eq scale :fill)
			    (incf major+ +fill+))
			   (t
			    (incf major (+ (scale (funcall fn-major space-req)) space))
			    (incf major+ (scale (funcall fn-major+ space-req)))))
		     (incf major- (scale (funcall fn-major- space-req)))
		     (setq minor (max minor (funcall fn-minor space-req))) 
		     (maxf minor-min (funcall fn-minor- space-req))
		     (minf minor-max (funcall fn-minor+ space-req)))))))
	(decf major space)
	;;--- These calcs lead to weirdness when fills are involved.
	;;--- This looks like a leftover from the time when +/- were relative
	;;(setq minor- (max 0 (- minor minor-min)))
	;;(setq minor+ (max 0 (- minor-max minor)))
	(setq minor- minor-min)
	(setq minor+ minor-max)
	(apply #'make-space-requirement
	       (mapcan #'list
		       keys 
		       (list major major+ major- minor minor+ minor-)))))))


(defclass hbox-pane (box-pane) ())

(defmethod compose-space ((box-pane hbox-pane) &key width height)
  (declare (ignore width))
  (compose-box box-pane 
	       #'space-requirement-width 
	       #'space-requirement-max-width
	       #'space-requirement-min-width
	       #'space-requirement-height
	       #'space-requirement-max-height
	       #'space-requirement-min-height
	       `(:height ,height)
	       '(:width :max-width :min-width :height :max-height :min-height)))

(defmethod allocate-space ((box-pane hbox-pane) width height)
  (with-slots (contents space) box-pane
    (let ((space-requirement
	   (compose-space box-pane :width width :height height)))
      (flet ((compose (x)
	       (cond ((atom x) (compose-space x :height height))
		     ((eq (car x) :fill) :fill)
		     (t (make-space-requirement :height 0 :width (* (car x) width))))))
	(declare (dynamic-extent #'compose))
	(let ((sizes 
	       (allocate-space-to-items
		width
		space-requirement
		contents
		#'space-requirement-min-width
		#'space-requirement-width
		#'space-requirement-max-width
		#'compose))
	      (x 0))
	  (mapc #'(lambda (sheet size)
		    (when (or (panep sheet)
			      (and (consp sheet)
				   (panep (second sheet))
				   (setq sheet (second sheet))))
		      (move-and-resize-sheet 
		       sheet x 0 (frob-size size width x) height))
		    (incf x size))
		contents sizes))))))


(defclass vbox-pane (box-pane) ())

(defmethod compose-space ((box-pane vbox-pane) &key width height)
  (declare (ignore height))
  (compose-box box-pane 
	       #'space-requirement-height
	       #'space-requirement-max-height
	       #'space-requirement-min-height
	       #'space-requirement-width 
	       #'space-requirement-max-width
	       #'space-requirement-min-width
	       `(:width ,width)
	       `(:height :max-height :min-height :width :max-width :min-width)))

(defmethod allocate-space ((box-pane vbox-pane) width height)
  (with-slots (contents space) box-pane
    (let ((space-requirement
	   (compose-space box-pane :width width :height height)))
      (flet ((compose (x)
	       (cond ((atom x) (compose-space x :width width))
		     ((eq (car x) :fill) :fill)
		     (t (make-space-requirement :width 0 :height (* (car x) height))))))
	(declare (dynamic-extent #'compose))
	(let ((sizes 
	       (allocate-space-to-items
		height
		space-requirement
		contents
		#'space-requirement-min-height
		#'space-requirement-height
		#'space-requirement-max-height
		#'compose))
	      (y 0))
	  (mapc #'(lambda (sheet size)
		    (when (or (panep sheet)
			      (and (consp sheet)
				   (panep (second sheet))
				   (setq sheet (second sheet))))
		      (move-and-resize-sheet
		       sheet 0 y width (frob-size size height y)))
		    (incf y size))
		contents sizes))))))

;;--- In theory this should work OK.
(defun frob-size (wanted-size available where-we-are-now)
  #---ignore (declare (ignore available where-we-are-now))
  #---ignore wanted-size
  #+++ignore (min wanted-size (1- (- available where-we-are-now))))
