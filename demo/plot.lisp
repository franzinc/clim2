;; -*- mode: common-lisp; package: clim-user -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader$


(in-package :clim-user)

;;; Clim based plotting package

;;-- How does incremental redisplay fit into all of this.
;;-- What happens if we use it and redisplay is not called at the
;;-- outer level


(defmacro plotting-data ((stream &rest  options) &body body)
  ;; For each X value we want to specify the Y values
  ;; Specify labels for each X value
  ;; Specify label for each set of Y values
  ;; Specify the size of the graph we want draw.
  ;; Range of values for the axiss
  (let ((point-plotting-continuation (gensym))
	(plot-data-continuation (gensym)))
    `(flet ((plotting-data-body (,point-plotting-continuation ,plot-data-continuation) 
				;; Perhaps we can also have a way of
				;; specify all the points in one go
				(flet ((plot-point (x &rest ys)
					 (apply ,point-plotting-continuation x ys))
				       (plot-data (array)
					 (funcall ,plot-data-continuation array)))
				  ,@body)))
	   (invoke-plotting-data ,stream #'plotting-data-body ,@options))))

;; We should make presentations of these types

(define-presentation-type graph-plot ())
(define-presentation-type graph-region ())

(define-presentation-method presentation-typep (object (type graph-region))
  (typep object 'standard-rectangle))

(define-command-table plot-command-table)

(define-presentation-translator select-graph-region
    (graph-plot graph-region plot-command-table :gesture :select)
  (x y window)
  (let ((nx x)
	(ny y)
	(ox x)
	(oy y))
    (with-output-recording-options (window :record nil)
      (flet ((draw-it ()
	       (draw-rectangle* window nx ny ox oy :filled nil :ink +flipping-ink+)))
	(draw-it)
	(tracking-pointer
	 (window)
	 (:pointer-motion
	  (x y)
	  (draw-it) 
	  (setq nx x ny y)
	  (draw-it))
	 (:pointer-button-press
	  (x y)
	  (draw-it)
	  (setq nx x ny y)
	  (return (make-rectangle* ox oy nx ny))))))))

;; Define a presentation translator from a graph-plot to a graph-region.
;; In that way we trivially write a command that will zoom into a
;; particular region of the graph.

(define-presentation-type graph-point ())
(define-presentation-type graph-line ())

(define-presentation-type graph-axis ())

(defun invoke-plotting-data (stream continuation 
			     &key x-labels 
				  y-labels 
				  y-labelling
				  x-min y-min
				  x-max y-max width height)
  (let ((points nil))
    (flet ((point-collector (x &rest ys)
	     (push (list* x ys) points))
	   (plot-data-collector (data)
	     (setq points data)))
      (funcall continuation #'point-collector #'plot-data-collector))

    (etypecase points
      ((array t (* *)) nil)
      (list
       (setq points (make-array (list (length points) (length (car points)))
				:initial-contents (nreverse points)))))

    (multiple-value-setq  (width height)
      (default-width-and-height stream width height))
    
    (multiple-value-setq (x-min y-min x-max y-max y-labelling)
      (default-graph-axis points x-min y-min x-max y-max y-labelling))

    (let ((transform
	   (let ((scaling-transform
		  (make-scaling-transformation
		   (/ width (- x-max x-min))
		   (/ height (- y-max y-min)))))
	     (multiple-value-bind (ox oy)
		 (transform-position scaling-transform x-min y-min)
	       (compose-transformations
		(make-translation-transformation (- ox) (- oy))
		scaling-transform)))))
      (with-new-output-record (stream 'standard-sequence-output-record)
	(with-output-as-presentation (stream nil 'graph-plot :single-box :position)
	  (formatting-item-list (stream :n-columns 2)
	      (formatting-cell (stream)
		  (with-room-for-graphics (stream)
		    (draw-axis
		     stream 
		     width height
		     x-min y-min x-max y-max
		     x-labels 
		     y-labels
		     points
		     transform
		     y-labelling)
		    (draw-data stream width height x-min y-min x-max
			       y-max points transform)))
	    (formatting-cell (stream)
		(draw-caption stream y-labels))))))))

(defun draw-caption (stream y-labels)
  (updating-output (stream :unique-id 'captions
			   :cache-value (copy-list y-labels)
			   :cache-test #'equalp)
      (let ((n-lines (length y-labels))
	    (ascent (text-style-ascent (medium-text-style stream) stream))
	    (descent (text-style-descent (medium-text-style stream) stream))
	    (i 0))
	(surrounding-output-with-border (stream)
	    (formatting-table (stream)
		(dolist (label y-labels)
		  (formatting-row (stream)
		      (formatting-cell (stream)
			  (draw-rectangle* stream 0 0 20 (+ ascent descent) :ink +background-ink+)
			(draw-line* stream 0 (/ ascent 2) 20 (/ ascent 2)
				    :ink (make-contrasting-inks n-lines i)
				    :line-dashes (make-contrasting-dash-patterns n-lines i)))
		    (formatting-cell (stream)
			(write-string label stream)))
		  (incf i)))))))
	    

(defun draw-axis (stream width height x-min y-min x-max y-max x-labels y-labels points transform y-labelling)
  ;; Y Axis

  (updating-output (stream :unique-id 'y-axis
			   :cache-value (list width height x-min y-min
					      x-max
					      y-max y-labelling)
			   :cache-test #'equalp)
      (with-output-as-presentation (stream nil 'graph-axis)
	(draw-line* stream 0 0 0 height)
	(do ((y y-min (+ y-labelling y)))
	    ((>= y y-max))
	  (multiple-value-bind (tx ty) 
	      (transform-position transform x-min y)
	    (draw-line* stream (- tx 2) ty (+ tx 2) ty)
	    (let ((label (format nil "~3d" y)))
	      (multiple-value-bind (width height ignore-x ignore-y baseline)
		  (text-size stream label)
		(declare (ignore ignore-x ignore-y))
		(draw-text* stream label (- tx width 2) ty)))))))

  ;; X Axis labelling
  (updating-output (stream :unique-id 'x-axis
			   :cache-value (list width height x-min y-min
					      x-max
					      y-max (copy-list x-labels))
			   :cache-test #'equalp)
      (with-output-as-presentation (stream nil 'graph-axis)
	(draw-line* stream 0 0 width 0)
	(let ((i 0))
	  (dolist (label x-labels)
	    (multiple-value-bind (tx ty) 
		(transform-position transform (aref points i 0) y-min)
	      (draw-line* stream tx (- ty 2) tx (+ ty 2))
	      (multiple-value-bind (width height ignore-x ignore-y baseline)
		  (text-size stream label)
		(declare (ignore ignore-x ignore-y))
		(draw-text* stream label (- tx (/ width 2)) (- ty height 3))
		(incf i))))))))



(defun draw-data (stream width height x-min y-min x-max y-max points transform)
  (destructuring-bind
      (rows columns) (array-dimensions points)
    (let* (
	   (n-lines (1- columns)))
    
      (dotimes (i n-lines)

	(updating-output (stream :unique-id `(plot-line ,i)
				 :id-test #'equal
				 :cache-value (list x-min y-min x-max y-max width height
						    (let ((r nil))
						      (dotimes (j rows
								 (nreverse r))
							(push (aref points j 0) r)
							(push (aref points j (1+ i)) r)))))
	    (let (last-tx last-ty)
	      (with-output-as-presentation (stream (1+ i) 'graph-line)
		(draw-lines*
		 stream 
		 (let ((r nil)) 
		   (dotimes (j rows (nreverse r))
		     (let ((x (aref points j 0))
			   (y (aref points j (1+ i))))
		       (multiple-value-bind
			   (tx ty)
			   (transform-position transform x y)
			 (when last-tx 
			   (push last-tx r) 
			   (push last-ty r)
			   (push tx r)
			   (push ty r))
			 (setq last-tx tx last-ty ty)))))
		 :ink (make-contrasting-inks n-lines i)
		 :line-dashes (make-contrasting-dash-patterns n-lines i))))
      
	  (dotimes (j rows)
	    (let ((x (aref points j 0))
		  (y (aref points j (1+ i))))
	      (multiple-value-bind
		  (tx ty)
		  (transform-position transform x y)
		(with-output-as-presentation (stream (list (1+ i) j) 'graph-point)
		  (draw-circle* stream tx ty 4 :filled t))))))))))
    
(defun default-width-and-height (stream width height)
  (values 400 300))

(defun default-graph-axis (points x-min y-min x-max y-max y-labelling)
  (destructuring-bind
      (rows columns) (array-dimensions points)
    (setq x-min (aref points 0 0) x-max x-min)
    (setq y-min (aref points 0 1) y-max y-min)
    (dotimes (i rows)
      (let ((x (aref points i 0)))
	(clim-utils::minf x-min x)
	(clim-utils::maxf x-max x)
	(dotimes (j (1- columns))
	  (let ((y (aref points i (1+ j))))
	    (clim-utils::minf y-min y)
	    (clim-utils::maxf y-max y))))))
  (values x-min y-min x-max y-max
	  (or y-labelling (float (/ (- y-max y-min) 10)))))


;; Actual demo code.

(define-application-frame plot-demo ()
			  (
			   (y-labelling :initform 5)
			   (plot-data :initform #2a((1960 5 11 14)
						    (1970 8 15 16)
						    (1980 14 18 15.5)
						    (1990 19 21 15.2)
						    (2000 24 22 15.4)))
			   (x-labels :initform '("60" "70" "80" "90" "2000"))
			   (y-labels :initform  '("Mexico City" "Tokyo" "New York"))
			   )
  (:command-table (plot-demo :inherit-from (plot-command-table)))
  (:panes 
   (graph-window :application :display-function 'display-graph
		:incremental-redisplay t
		 :scroll-bars :both
		 :width :compute :height :compute)
   (data-window :application :display-function 'display-data
		:incremental-redisplay t
		:scroll-bars :both
		:width :compute :height :compute)
   (command :interactor :height '(5 :line)))
  (:layouts
   (:default (vertically () graph-window data-window command))))


(defmethod frame-standard-output ((fr plot-demo))
  (get-frame-pane fr 'command))

(define-presentation-type data-point ())
(define-presentation-type x-label ())
(define-presentation-type y-label ())

(defmethod display-data ((frame plot-demo) stream &key &allow-other-keys)
  (with-slots (y-labels x-labels plot-data) frame
    (formatting-table (stream)
	;; Headers
	(formatting-row (stream)
	    (updating-output (stream :unique-id `(-1 ,-1)
				       :id-test #'equal
				       :cache-value nil
				       :cache-test #'equalp)

	     (formatting-cell (stream) stream)) ; Dummy corner

	  (updating-output (stream :unique-id `(-1 ,0)
				   :id-test #'equal
				   :cache-value nil
				   :cache-test #'equalp)
	      (formatting-cell (stream) stream )) ; Over the X values

	  (let ((i 0))
	    (dolist (label y-labels)
	      (updating-output (stream :unique-id `(-1 ,(+ 2 i))
				       :id-test #'equal
				       :cache-value label
				       :cache-test #'equalp)
		  (formatting-cell (stream) 
		      (with-output-as-presentation (stream i 'y-label)
			(write-string label stream))))
		(incf i))))
      (destructuring-bind (rows columns)
	  (array-dimensions plot-data)
	(dotimes (i rows)
	  (let ((label (nth i x-labels)))
	    (formatting-row (stream)
		(updating-output (stream :unique-id `(,i -1)
					 :id-test #'equal
					 :cache-value label
					 :cache-test #'equalp)
		    (formatting-cell (stream)
			(with-output-as-presentation (stream i 'x-label)
			  (write-string label stream))))
	      (dotimes (j columns)
		(let ((n  (aref plot-data i j)))
		  (updating-output (stream :unique-id `(,i ,j)
					   :id-test #'equal
					   :cache-value n
					   :cache-test #'equal)
		      (formatting-cell (stream)
			  (with-output-as-presentation (stream (list i j) 'data-point)
			    (with-output-as-presentation (stream n 'number)
			      (format stream "~2D" n))))))))))))))

(define-plot-demo-command (com-edit-x-label :name t :menu t)
    ((i 'x-label :gesture :select))
  (with-application-frame (frame)
    (setf (nth i (slot-value frame 'x-labels))
      (accept 'string
	      :default (nth i (slot-value frame 'x-labels))))))

(define-plot-demo-command (com-edit-y-label :name t :menu t)
    ((i 'y-label :gesture :select))
  (with-application-frame (frame)
    (setf (nth i (slot-value frame 'y-labels))
      (accept 'string
	      :default (nth i (slot-value frame 'y-labels))))))

(define-plot-demo-command (com-edit-data-point :name t :menu t)
    ((point 'data-point :gesture :select))
  (with-application-frame (frame)
    (destructuring-bind (i j) point
      (setf (aref (slot-value frame 'plot-data) i j)
	(accept 'number
		:default (aref (slot-value frame 'plot-data) i j))))))

(defmethod display-graph ((frame plot-demo) stream &key &allow-other-keys)

  (with-slots (y-labelling plot-data x-labels y-labels) frame
    (plotting-data (stream :y-labelling y-labelling :x-labels x-labels :y-labels y-labels)
		   (plot-data plot-data))))

(define-plot-demo-command com-describe-graph-line 
    ((line 'graph-line :gesture :select))
  (describe line))

(define-plot-demo-command com-describe-graph-point
    ((point 'graph-point :gesture :select))
  (describe point))


(define-plot-demo-command (com-describe-region :name t) 
    ((region 'graph-region))
  (describe region))

(define-plot-demo-command (com-redisplay :name t) 
    ()
  (window-clear (get-frame-pane *application-frame* 'graph-window))
  (window-clear (get-frame-pane *application-frame* 'data-window)))



