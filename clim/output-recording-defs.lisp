;; -*- mode: common-lisp; package: clim-internals -*-
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
;; $fiHeader: output-recording-defs.cl,v 1.3 92/01/02 15:33:26 cer Exp $

(in-package :clim-internals)

(defmacro define-output-recorder (class name
				  medium-components 
				  &key bounding-rectangle
				       (superclasses
					'(output-record-element-mixin 
					  graphics-displayed-output-record)))
  (destructuring-bind
      (function-args &key points-to-transform
		     point-sequences-to-transform
		     distances-to-transform
		     &allow-other-keys)
      (silica::get-drawing-function-description name)
    (let ((medium-graphics-function* (intern (format nil "~a~a*" 'medium- name))))
      `(progn
	 (defclass ,class ,superclasses 
		   ,(mapcar #'(lambda (x)
				(list x
				      :initarg
				      (intern x :keyword)))
			    (append medium-components function-args)))
	 ;;--- Need to define a speedy constructor
	 (defmethod ,medium-graphics-function* :around ((medium output-recording-mixin) ,@function-args)
	   (when (stream-recording-p medium)
	     (multiple-value-bind
		 (abs-x abs-y)
		 (point-position*
		  (stream-output-history-position medium))
	       (declare (type coordinate abs-x abs-y))

	       ,@(mapcar #'(lambda (p)
			     `(silica::transform-point-sequence
			       ((medium-transformation medium))
			       ,p))
			 point-sequences-to-transform)
	       (silica::transform-points ((medium-transformation medium)) ,@points-to-transform)
	       (silica::transform-distances ((medium-transformation medium)) ,@distances-to-transform)
	       (let ((rec (make-instance ',class
					 ,@(mapcan #'(lambda (arg)
						       (list (intern arg :keyword)
							     arg))
						   function-args)
					 ,@(mapcan #'(lambda (medium-component)
						       (list (intern medium-component :keyword)
							     `(,(fintern"~A~A" 'medium- medium-component)
							       medium)))
						   medium-components))))

		 (multiple-value-bind
		     (lf tp rt bt)
		     (progn ,bounding-rectangle)
		   (declare (type coordinate lf tp rt bt))
		   (bounding-rectangle-set-edges
		     rec
		     (- lf abs-x) (- tp abs-y) (- rt abs-x) (- bt abs-y))
		   (multiple-value-bind (cx cy) (stream-cursor-position* medium)
		     (declare (type coordinate cx cy))
		     ;; Doing this directly beats calling OUTPUT-RECORD-SET-START-CURSOR-POSITION*
		     (with-slots (start-x start-y) rec
		       (setq start-x (- cx abs-x)
			     start-y (- cy abs-y))))
		   ;; Adjust the stored coordinates by the current cursor position

		   ,@(mapcar #'(lambda (p)
				 `(with-slots (,p) rec
				    (setf ,p (adjust-point-sequence ,p abs-x abs-y))))
			     point-sequences-to-transform)
		   ,@(when points-to-transform
		       `((with-slots ,points-to-transform rec
			   (setf ,@(do ((p points-to-transform (cddr p))
					r)
				       ((null p) (nreverse r))
				     (push (first p) r)
				     (push `(- ,(first p) abs-x) r)
				     (push (second p) r)
				     (push `(- ,(second p) abs-y) r)))))))
		 (stream-add-output-record medium rec))))
	 
	   (when (stream-drawing-p medium)
	     (call-next-method)))
       
	 (defmethod replay-output-record ((rec ,class) stream 
					  &optional region (x-offset 0) (y-offset 0))
	   (declare (ignore region))
	   (multiple-value-bind (minx miny)
	       (bounding-rectangle-position* rec)
	     (setq minx x-offset) ; ---------------- Ignore the
				  ; 
	     (setq miny y-offset)
	     (with-slots (,@function-args ,@medium-components) rec
	       (letf-globally (( (medium-transformation stream) +identity-transformation+))
			      (with-drawing-options 
				  (stream ,@(mapcan #'(lambda (medium-component)
							(list (intern medium-component :keyword)
							      medium-component))
						    medium-components))
	       
				(let (,@(mapcar #'(lambda (p)
						    (list p p))
						points-to-transform)
				      ,@(mapcar #'(lambda (p)
						    (list p p))
						point-sequences-to-transform))
				  ,@(mapcar #'(lambda (p)
						`(setq ,p
						   (adjust-point-sequence
						    ,p (- minx) (- miny))))
					    point-sequences-to-transform)
				  (setf ,@(do ((p points-to-transform (cddr p))
					       r)
					      ((null p) (nreverse r))
					    (push (first p) r)
					    (push `(+ ,(first p) minx) r)
					    (push (second p) r)
					    (push `(+ ,(second p) miny) r)))
				  (with-sheet-medium (medium stream)
				    (,medium-graphics-function* medium ,@function-args))))))))
       
	 #+ignore
	 (defmethod output-record-refined-sensitivity-test ())
	 #+ignore
	 (defmethod highlight-output-record-1 ())))))


(define-output-recorder line-output-record silica::draw-line (ink line-style)
  :bounding-rectangle 
  silica::(values (min from-x to-x)
		  (min from-y to-y)
		  (max from-x to-x)
		  (max from-y to-y)))

;;; This is all kind of broken because of the weirdness concerning the
;;; radius-1/2-dx/y stuff. They are vectors that are center relative
;;; Perhaps they just want to be scaled rather than translated.

(define-output-recorder ellipse-output-record
    silica::draw-ellipse (ink line-style)
    :bounding-rectangle
    silica::(clim-utils::elliptical-arc-box
	     center-x 
	     center-y 
	     radius-1-dx
	     radius-1-dy
	     radius-2-dx
	     radius-2-dy
	     start-angle
	     end-angle
	     (line-style-thickness (medium-line-style medium))))

(define-output-recorder rectangle-output-record
    silica::draw-rectangle (ink line-style)
    :bounding-rectangle
    silica::(values (min from-x to-x)
		    (min from-y to-y)
		    (max from-x to-x)
		    (max from-y to-y)))



(define-output-recorder polygon-output-record
    silica::draw-polygon (ink line-style)
    :bounding-rectangle
    silica::(point-sequence-bounding-rectangle list-of-x-and-ys))



;; etc etc

(defun map-point-sequence (fn list-of-x-and-ys)
  (do ((p list-of-x-and-ys (cddr p)))
      ((null p))
    (funcall fn (car p) (cadr p))))

;(defun transform-point-sequence (transformation list-of-x-and-ys)
;  (let (r)
;    (map-point-sequence
;     #'(lambda (x y)
;	 (multiple-value-setq
;	  (x y)
;	  (transform-point* transformation x y))
;	 (push x r)
;	 (push y r))
;     list-of-x-and-ys)
;    (nreverse r)))

(defun adjust-point-sequence (list-of-x-and-ys dx dy)
  (let (r)
    (map-point-sequence
     #'(lambda (x y)
	 (push (- x dx) r)
	 (push (- y dy) r))
     list-of-x-and-ys)
    (nreverse r)))


(defun silica::point-sequence-bounding-rectangle (list-of-x-and-ys)
  (let* ((minx (car list-of-x-and-ys))
	 (miny (second list-of-x-and-ys))
	 (maxx minx)
	 (maxy miny))
    (map-point-sequence
     #'(lambda (x y)
	 (minf minx x)
	 (minf miny y)
	 (maxf maxx x)
	 (maxf maxy y))
     list-of-x-and-ys)
    (values minx miny maxx maxy)))

       
