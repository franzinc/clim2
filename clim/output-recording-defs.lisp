(in-package :clim-internals)
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

(defmacro define-output-recorder (class medium-graphics-function*
				  medium-components 
				  &key  points-to-transform
					point-sequences-to-transform
					bounding-rectangle
					distances-to-transform
					points-not-to-move
					(superclasses '(graphics-displayed-output-record)))
  (let ((function-args (get medium-graphics-function* 'silica::args)))
    `(progn
       (defclass ,class ,superclasses 
		 ,(mapcar #'(lambda (x)
			      (list x
				    :initarg
				    (intern x :keyword)))
			  (append medium-components function-args)))
       (defmethod ,medium-graphics-function* :around ((medium basic-output-recording) ,@function-args)

	 (when (stream-record-p medium)
	   ;; Transform the coordinates
	   ,@(mapcar #'(lambda (p)
			 `(setq ,p (transform-point-sequence
				    (medium-transformation medium)
				    ,p)))
		     point-sequences-to-transform)
	   
	   ,@(do ((p points-to-transform (cddr p))
		  r)
		 ((null p)
		  r)
	       (push `(multiple-value-setq
			  (,(car p) ,(cadr p))
			(transform-point* (medium-transformation medium)
					  ,(car p) ,(cadr p)))
		     r))
	   
	   ,@(do ((p distances-to-transform (cddr p))
		  r)
		 ((null p)
		  r)
	       (push `(multiple-value-setq
			  (,(car p) ,(cadr p))
			(transform-distance (medium-transformation medium)
					  ,(car p) ,(cadr p)))
		     r))
	   

	   
	   
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
		 (minx miny maxx maxy) 
		 (progn ,bounding-rectangle)
	       
	       (bounding-rectangle-set-edges rec 
					     minx
					     miny
					     maxx
					     maxy)
	       ,@(mapcar #'(lambda (p)
			     `(with-slots (,p) rec
				(setf ,p (adjust-point-sequence ,p minx miny))))
			 point-sequences-to-transform)
	       ,@(when points-to-transform
		   `((with-slots ,points-to-transform rec
		       (setf ,@(do ((p points-to-transform (cddr p))
				    r)
				   ((null p) (nreverse r))
				 (unless (member (car p) points-not-to-move)
				   (push (first p) r)
				   (push `(- ,(first p) minx) r)
				   (push (second p) r)
				   (push `(- ,(second p) miny) r)))))))
				      
	       (multiple-value-bind
		   (cursor-x cursor-y)
		   (stream-cursor-position* medium)
		 (output-record-set-start-cursor-position* rec
							   cursor-x cursor-y)
		 
		 (stream-add-output-record medium rec)))))
	 
	 (when (stream-draw-p medium)
	   (call-next-method)))
       
       (defmethod replay-output-record ((rec ,class) stream &optional region)
	 (declare (ignore region))
	 (multiple-value-bind
	     (minx miny)
	     (bounding-rectangle-position* rec)
	   (with-slots (,@function-args ,@medium-components) rec
	     (letf-globally (( (medium-transformation stream) +identity-transformation+))
			    (silica::with-drawing-options 
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
						 (adjust-point-sequence ,p (-
									    minx) (- miny))))
					  point-sequences-to-transform)
				(setf ,@(do ((p points-to-transform (cddr p))
					     r)
					    ((null p) (nreverse r))
					  (unless (member (car p) points-not-to-move)
					    (push (first p) r)
					    (push `(+ ,(first p) minx) r)
					    (push (second p) r)
					    (push `(+ ,(second p) miny) r))))
				(with-sheet-medium (medium stream)
				  (,medium-graphics-function* medium ,@function-args))))))))
       
       #+ignore
       (defmethod output-record-refined-sensitivity-test ())
       #+ignore
       (defmethod highlight-output-record-1 ()))))


(define-output-recorder line-output-record silica::medium-draw-line* (ink line-style)
  :points-to-transform silica::(from-x from-y to-x to-y)
  :bounding-rectangle 
  silica::(values (min from-x to-x)
		  (min from-y to-y)
		  (max from-x to-x)
		  (max from-y to-y)))

;;; This is all kind of broken because of the weirdness concerning the
;;; radius-1/2-dx/y stuff. They are vectors that are center relative
;;; Perhaps they just want to be scaled rather than translated.

(define-output-recorder ellipse-output-record
    silica::medium-draw-ellipse* (ink line-style)
    :points-to-transform silica::(center-x 
				  center-y 
				  ) 
    ;; When we move the output record these do not move!
    ;; kuldge
    :distances-to-transform silica::(radius-1-dx radius-1-dy
						 radius-2-dx
						 radius-2-dy)

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
	     (silica::line-style-thickness (silica::medium-line-style medium))))

(define-output-recorder rectangle-output-record
    silica::medium-draw-rectangle* (ink line-style)
    :points-to-transform silica::(from-x from-y to-x to-y)
    :bounding-rectangle
    silica::(values (min from-x to-x)
		    (min from-y to-y)
		    (max from-x to-x)
		    (max from-y to-y)))



(define-output-recorder polygon-output-record
    silica::medium-draw-polygon* (ink line-style)
    :point-sequences-to-transform silica::(list-of-x-and-ys)
    :bounding-rectangle
    silica::(point-sequence-bounding-rectangle list-of-x-and-ys))



;; etc etc

(defun map-point-sequence (fn list-of-x-and-ys)
  (do ((p list-of-x-and-ys (cddr p)))
      ((null p))
    (funcall fn (car p) (cadr p))))

(defun transform-point-sequence (transformation list-of-x-and-ys)
  (let (r)
    (map-point-sequence
     #'(lambda (x y)
	 (multiple-value-setq
	  (x y)
	  (transform-point* transformation x y))
	 (push x r)
	 (push y r))
     list-of-x-and-ys)
    (nreverse r)))

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

       
