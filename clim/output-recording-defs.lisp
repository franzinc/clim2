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
					bounding-rectangle
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
	   ,@(do ((p points-to-transform (cddr p))
		  r)
		 ((null p)
		  r)
	       (push `(multiple-value-setq
			  (,(car p) ,(cadr p))
			(transform-point* (medium-transformation medium)
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
	       
	       ,@(when points-to-transform
		   `((with-slots ,points-to-transform rec
		       (setf ,@(do ((p points-to-transform (cddr p))
				    r)
				   ((null p) (nreverse r))
				 (push (first p) r)
				 (push `(- ,(first p) minx) r)
				 (push (second p) r)
				 (push `(- ,(second p) miny) r))))))
				      
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
	     (silica::with-drawing-options 
		 (stream ,@(mapcan #'(lambda (medium-component)
				       (list (intern medium-component :keyword)
					     medium-component))
				   medium-components))
	    
	       (let ,(mapcar #'(lambda (p)
				 (list p p))
		      points-to-transform)
		 (setf ,@(do ((p points-to-transform (cddr p))
			      r)
			     ((null p) (nreverse r))
			   (push (first p) r)
			   (push `(+ ,(first p) minx) r)
			   (push (second p) r)
			   (push `(+ ,(second p) miny) r)))
		 (with-sheet-medium (medium stream)
		   (,medium-graphics-function* medium ,@function-args)))))))
       
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
					 
;; draw-rectangle*
;; etc etc


