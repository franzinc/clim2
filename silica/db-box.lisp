;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved. 
;;;
;; $fiHeader: db-box.lisp,v 1.8 92/02/24 13:04:24 cer Exp $

(in-package :silica)


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
      (pane (sheet-adopt-child pane child))))
  (with-slots ((c contents)) pane
    (setf c contents)))

#+ignore
(defmethod insert-pane ((lcm box-pane) pane
			&key position batch-p &allow-other-keys)
  (if (panep pane)
      (call-next-method)
      (progn
	(unless (or (eq pane :fill)
		    (numberp pane))
	  (error "Illegal element in a box's contents"))
	(with-slots (contents reverse-p) lcm
	  (unless batch-p
	    (setq position (check-position position reverse-p contents)))
	  (if (zerop position)
	      (push pane contents)
	      (let ((tail (nthcdr (1- position) contents)))
		(setf (cdr tail) 
		      (cons pane (cdr tail)))))
	  (unless batch-p
	    (note-space-requirement-changed (sheet-parent lcm) lcm))))))


(defmacro compose-box (box-pane
		       (major major+ major- minor minor+ minor- width-or-height)
		       keys)
  `(with-slots (contents space) ,box-pane
     (if (null contents) 
	 (make-space-requirement)
	 (let ((major 0)
	       (major+ 0)
	       (major- 0)
	       (minor 0)  
	       minor+ minor-
	       (minor-min 0)
	       ;;--- is this safe?
	       (minor-max most-positive-fixnum))
	   (dolist (entry contents)
	     (cond ((eq entry :fill) (incf major+ +fill+))
		   ((numberp entry) (incf major entry))
		   (t 
		    (let ((space-req (compose-space entry ,@width-or-height)))
		      (incf major (+ (,major space-req) space))
		      (incf major+ (,major+ space-req))
		      (incf major- (,major- space-req))
		      (setq minor (max minor (,minor space-req))) 
		      (setq minor-min
			    (max minor-min (- (,minor space-req)
					      (,minor- space-req))))
		      (setq minor-max
			    (min minor-max (+ (,minor space-req)
					      (,minor+ space-req))))))))
	   (decf major space)
	   ;; ?? These calcs lead to weirdness when fills are involved.
	   (setq minor- (max 0 (- minor minor-min)))
	   (setq minor+ (max 0 (- minor-max minor)))
	   (make-space-requirement
	    ,@(mapcan #'(lambda (key val) (list key val))
		      keys '(major major+ major- minor minor+ minor-)))))))


(defclass hbox-pane (box-pane) ())

(defmethod compose-space ((box-pane hbox-pane) &key width height)
  (compose-box box-pane (space-requirement-width 
			 space-requirement-max-width
			 space-requirement-min-width
			 space-requirement-height
			 space-requirement-max-height
			 space-requirement-min-height
			 (:height height))
	       (:width :max-width :min-width :height :max-height :min-height)))

(defmethod allocate-space ((box-pane hbox-pane) width height)
  (with-slots (contents space space-requirement) box-pane
    (unless space-requirement 
      (compose-space box-pane :width width :height height))
    (let ((sizes 
	    (allocate-space-to-items
	      width
	      space-requirement
	      contents
	      #'space-requirement-min-width
	      #'space-requirement-width
	      #'space-requirement-max-width
	      #'(lambda (x) (compose-space x :height height))))
	  (x 0))
      (mapc #'(lambda (sheet size)
		(move-and-resize-sheet* sheet 
					x 0
					(frob-size size width x) height)
		(incf x size))
	    contents sizes))))


(defclass vbox-pane (box-pane) ())

(defmethod compose-space ((box-pane vbox-pane) &key width height)
  (compose-box box-pane (space-requirement-height
			 space-requirement-max-height
			 space-requirement-min-height
			 space-requirement-width 
			 space-requirement-max-width
			 space-requirement-min-width
			 (:width width))
	       (:height :max-height :min-height :width :max-width :min-width)))

(defmethod allocate-space ((box-pane vbox-pane) width height)
  (with-slots (contents space space-requirement) box-pane
    (unless space-requirement 
      (compose-space box-pane :width width :height height))
    (let ((sizes 
	    (allocate-space-to-items
	      height
	      space-requirement
	      contents
	      #'space-requirement-min-height
	      #'space-requirement-height
	      #'space-requirement-max-height
	      #'(lambda (x) (compose-space x :width width))))
	  (y 0))
      (mapc #'(lambda (sheet size)
		(move-and-resize-sheet* sheet 
					0 y
					width (frob-size size height y))
		(incf y size))
	    contents sizes))))

;;--- Yow
(defun frob-size (wanted-size available where-we-are-now)
  (min wanted-size (1- (- available where-we-are-now))))
		   


