;; -*- mode: common-lisp; package: silica -*-
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
;; $fiHeader: graphics.cl,v 1.2 92/01/02 15:09:12 cer Exp $

(in-package :silica)

(eval-when (compile load eval)
  
(defparameter *all-drawing-options*
	      '(:ink :clipping-region :transformation
		:line-style :line-unit :line-thickness :line-dashes
		:line-joint-shape :line-cap-shape
		:text-style :text-family :text-face :text-size))

(defparameter *always-meaningful-drawing-options* '(:ink :clipping-region :transformation))

(defparameter *drawing-option-subsets*
	      '((:point          :line-style :line-thickness :line-unit)
		(:line-cap       :line-style :line-thickness :line-unit
				 :line-dashes :line-cap-shape)
		(:line-joint     :line-style :line-thickness :line-unit
				 :line-dashes :line-joint-shape)
		(:line-joint-cap :line-style :line-thickness :line-unit
				 :line-dashes :line-joint-shape :line-cap-shape)
		(:text           :text-style :text-family :text-face
		 :text-size)))


(defun non-drawing-option-keywords (arglist)
  (do ((l (cdr (member '&key arglist)) (cdr l))
       (non-drawing-option-keywords nil)
       k)
      ((null l) non-drawing-option-keywords)
    (setq k (cond ((atom (car l)) (intern (string (car l)) "KEYWORD"))
		  ((atom (caar l)) (intern (string (caar l)) "KEYWORD"))
		  (t (caaar l))))
    (unless (member k *all-drawing-options*)
      (push k non-drawing-option-keywords))))

;;; Caller must stick &key in front
;;; If drawing-options isn't nil, it's a list of the option keywords accepted.
(defun all-drawing-options-lambda-list (drawing-options)
  (mapcar #'(lambda (keyword) (intern (string keyword)))
	  (cond ((null drawing-options) *all-drawing-options*)
		((atom drawing-options)
		 (append (or (cdr (assoc drawing-options *drawing-option-subsets*))
			     (warn "~S was specified in :drawing-options but is not ~
				    a known drawing-option subset."
				   drawing-options))
			 *always-meaningful-drawing-options*))
		(t
		 (dolist (option drawing-options)
		   (unless (member option *all-drawing-options*)
		     (warn "~S was specified in :drawing-options but ~
			    is not a known drawing option."
			   option)))
		 (append drawing-options *always-meaningful-drawing-options*)))))

) ;; end eval-when 

(defclass sheet-with-graphics-state ()
	  ((foreground :initform +black+ :accessor sheet-foreground)
	   (background :initform +white+ :accessor sheet-background)
	   (ink :initform +white+ :accessor sheet-ink)))

(defmethod (setf sheet-foreground) :after (nv (sheet sheet-with-graphics-state))
  (when (sheet-medium sheet)
    (setf (medium-foreground (sheet-medium sheet))
      nv)))

;; One would imagine that with-drawing-options affects this rather
;; than the medium

#+ignore
(spread-arguments 
	 spread-argument-names 
	 keyword-argument-names
	 other-keyword-arguments
	 keywords)

(eval-when (compile load eval)
(defun decode-graphics-function-arguments (arguments)
  (let* ((keyn (position '&key arguments))
	 (no-keyword (subseq arguments 0 keyn))
	 (keyword (and keyn (subseq arguments (1+ keyn))))
	 unspread-argument-names
	 spread-arguments 
	 spread-argument-names)
    
    (dolist (x no-keyword)
      (if (consp x)
	  (destructuring-bind
	      (argname type . names)
	      x
	    (push argname unspread-argument-names)
	    (ecase type
	      (point-sequence
	       (destructuring-bind
		   (new-name) names
		 (push `(unspread-point-sequence ,argname) spread-arguments)
		 (push new-name spread-argument-names)))
	      (point
	       (destructuring-bind 
		   (x y) names
		 (push `(point-x ,argname) spread-arguments)
		 (push x spread-argument-names)
		 (push `(point-y ,argname) spread-arguments)
		 (push y spread-argument-names)))))
	(progn
	  (push x  unspread-argument-names)
	  (push x  spread-arguments)
	  (push x  spread-argument-names))))

      
    (values (nreverse unspread-argument-names)
	    (nreverse spread-arguments)
	    (nreverse spread-argument-names)
	    (mapcar #'(lambda (x)
			(if (consp x)
			    (car x)
			  x))
		    keyword)
	    keyword
	    (mapcar #'(lambda (x)
			(intern
			 (if (consp x)
			     (car x)
			   x)
			 'keyword))
		    keyword))))
)					; eval-when


(defmacro transform-points ((transform) &rest points)
  (when points
    (assert (evenp (length points)) ()
      "Points must be x/y pairs.  There are an odd number of elements in ~S"
      points)
    (let ((xform '#:transform))
      `(let ((,xform ,transform))
	 ,@(do* ((points points (cddr points))
		 (x (first points) (first points))
		 (y (second points) (second points))
		 (forms nil))
	       ((null points) (nreverse forms))
	     (push `(multiple-value-setq (,x ,y)
		      (transform-point* ,xform ,x ,y))
		   forms))
	 (values)))))

;;; Update the points list in the reference.
(defmacro transform-point-sequence ((transform) points-reference)
  (let ((pts-sequence '#:points-sequence)
	(pts-temp '#:points)
	(original-pts-temp '#:original-points)
	(xform '#:transform))
    ;;--- is a run-time error check worth it?
    `(let ((,pts-sequence ,points-reference)
	   (,xform ,transform))
       (assert (evenp (length ,pts-sequence)) ()
	       "Points must be x/y pairs.  There are an odd number of elements in ~S"
	       ,pts-sequence)
       (setf ,points-reference
	     (let* ((,pts-temp (copy-list ,pts-sequence))
		    (,original-pts-temp ,pts-temp))
	       (dorest (pt ,pts-sequence cddr)
		 (let ((x (first pt))
		       (y (second pt)))
		   (multiple-value-bind (nx ny)
		       (transform-point* ,xform x y)
		     (setf (car ,pts-temp) nx
			   ,pts-temp (cdr ,pts-temp)
			   (car ,pts-temp) ny
			   ,pts-temp (cdr ,pts-temp)))))
	       ,original-pts-temp))
	    (values))))

(defmacro transform-distances ((transform) &rest distances)
  (when distances
       (assert (evenp (length distances)) ()
	 "Distances must be dx/dy pairs.  There are an odd number of elements in ~S"
	 distances)
       (let ((xform '#:transform))
	 `(let ((,xform ,transform))
	    ,@(do* ((distances distances (cddr distances))
		    (dx (first distances) (first distances))
		    (dy (second distances) (second distances))
		    (forms nil))
		  ((null distances) (nreverse forms))
		(push `(multiple-value-setq (,dx ,dy)
			 (transform-distance ,xform ,dx ,dy))
		      forms))
	    (values)))))

(defmacro define-graphics-function (name arguments 
				    &rest args
				    &key 
				    drawing-options
				    points-to-transform
				    distances-to-transform
				    point-sequences-to-transform)
  (let* ((spread-name (intern (format nil "~a*" name)))
	 (drawing-options
	  (all-drawing-options-lambda-list drawing-options))
	 (medium-graphics-function-name
	  (intern (format nil "~a~a*" 'medium- name)))
	 (port-function-name
	  (intern (format nil "~a~a*" 'port- name))))
    (multiple-value-bind
	(unspread-argument-names
	 spread-arguments 
	 spread-argument-names 
	 keyword-argument-names
	 other-keyword-arguments
	 keywords)
	(decode-graphics-function-arguments arguments)
      `(progn
	 (defun ,name (medium ,@unspread-argument-names &rest args
		       &key ,@drawing-options ,@other-keyword-arguments) 
	   (declare (ignore ,@drawing-options)
		    (dynamic-extent args))
	   (apply #',spread-name 
		  medium
		  ,@spread-arguments
		  ,@keyword-argument-names
		  args))
	 (defun ,spread-name (medium ,@spread-argument-names &rest args 
				     &key ,@drawing-options ,@other-keyword-arguments)
	   (declare (ignore ,@drawing-options)
		    (dynamic-extent args))
	   (with-keywords-removed
	    (args args ',keywords)
	    (apply #'invoke-with-drawing-options
		   medium
		   #'(lambda ()
		       (,medium-graphics-function-name 
		      medium
			,@spread-argument-names
			,@keyword-argument-names))
		   args)))
	 (setf (get ',name 'args)
	   '((,@spread-argument-names ,@keyword-argument-names)
	     ,@args)) 
	 (defmethod ,medium-graphics-function-name ((sheet sheet) 
						    ,@spread-argument-names
						    ,@keyword-argument-names)
	   (with-sheet-medium 
	    (medium sheet)
	    (,medium-graphics-function-name medium 
					     ,@spread-argument-names
					     ,@keyword-argument-names)))
	 (defmethod ,medium-graphics-function-name ((medium medium) 
						    ,@spread-argument-names
						    ,@keyword-argument-names)
	   (let* ((sheet (medium-sheet medium)))
	     ;; want to tranform stuff, set up clipping region etc etc
	     ,@(and points-to-transform
		   `((transform-points
		      ((medium-transformation medium))
		      ,@points-to-transform)))
	     ,@(and distances-to-transform
		    `((transform-distances 
		       ((medium-transformation medium))
		       ,@distances-to-transform)))
	     ,@(mapcar #'(lambda (seq)
			    `(transform-point-sequence
			      ((medium-transformation medium)) 
			      ,seq))
			point-sequences-to-transform)
	     (,port-function-name
	      (sheet-port medium)
	      sheet
	      medium
	      ,@spread-argument-names
	      ,@keyword-argument-names)))))))

(define-graphics-function draw-line 
    ((point1 point from-x from-y)
     (point2 point to-x to-y))
  :drawing-options :line-cap
  :points-to-transform (from-x from-y to-x to-y))

(define-graphics-function draw-rectangle ((point1 point from-x from-y)
					  (point2 point to-x to-y)
					  &key (filled t))
  :drawing-options :line-joint
  :points-to-transform (from-x from-y to-x to-y))

(define-graphics-function draw-point ((point point x y))
  :drawing-options :point
  :points-to-transform (x y))
  


;; draw-lines
;; draw-points
;; draw-polygon

(define-graphics-function draw-polygon ((points point-sequence list-of-x-and-ys)
					&key 
					(closed t)
					(filled t))
  :drawing-options :line-joint-cap
  :point-sequences-to-transform (list-of-x-and-ys))



(define-graphics-function draw-ellipse ((center point center-x center-y)
					radius-1-dx
					radius-1-dy
					radius-2-dx
					radius-2-dy
					&key 
					(start-angle 0)
					(end-angle 2pi)
					(filled t))
  :drawing-options :line-cap
  :points-to-transform (center-x center-y)
  :distances-to-transform (radius-1-dx radius-1-dy radius-2-dx radius-2-dy))

(defun draw-circle (medium center radius &rest args)
  (apply #'draw-ellipse medium center 0 radius radius 0 args))

(defun draw-circle* (medium center-x center-y radius &rest args)
  (apply #'draw-ellipse* medium center-x center-y 0 radius radius 0 args))

(define-graphics-function draw-text (string-or-char
				     (point point x y)
				     &key
				     start
				     end
				     (align-x :left)
				     (align-y :baseline))
  :points-to-transform (x y)
  :drawing-options :text)


(defmethod stream-glyph-for-character ((medium medium) 
				       character 
				       appearance &optional our-font)
  (port-glyph-for-character (sheet-port medium)
			     character
			     appearance 
			     our-font))


(defmethod stream-write-string-internal ((medium medium) 
					 glyph-buffer 
					 start 
					 end 
					 x-font 
					 color 
					 x
					 y)
  (port-write-string-internal (sheet-port medium)
			      medium
			      glyph-buffer start end x-font color x
			      y))

(defmethod sheet-beep ((x t))
  x)

(defmethod sheet-beep ((x sheet))
  (port-beep (sheet-port x) x))

(defun get-drawing-function-description (name)
  (or (get name 'args)
      (error "Cannot find description for: ~S")))

(defmacro define-port-graphics-method ((name port-class) &body body)
  (destructuring-bind
      (args . ignore)
      (get-drawing-function-description name)
    (declare (ignore ignore))
    `(defmethod ,(intern (format nil "~a~a*" 'port- name))
	 ((port ,port-class) ,@args)
       ,@body)))



(defun clim-internals::draw-rectangle-internal
    (stream xoff yoff left top right bottom ink style)
  (letf-globally (((medium-line-style stream)
		   (or style (medium-line-style stream)))
		  ((medium-transformation stream) +identity-transformation+))
		 (draw-rectangle*
		  stream
		  (+ left xoff)
		  (+ top yoff)
		  (+ right xoff)
		  (+ bottom yoff)
		  :filled (not style)
		  :ink ink)))

(defun draw-triangle (stream p1 p2 p3 &rest keys)
  (declare (dynamic-extent keys))
  (declare (arglist stream p1 p2 p3
		    &key (filled t) . #.(all-drawing-options-lambda-list :line-joint)))
  (with-stack-list (points p1 p2 p3)
    (apply #'draw-polygon stream points :closed t keys)))

(defun draw-triangle* (stream x1 y1 x2 y2 x3 y3 &rest keys)
  (declare (dynamic-extent keys))
  (declare (arglist stream x1 y1 x2 y2 x3 y3
		    &key (filled t) . #.(all-drawing-options-lambda-list :line-joint)))
  (with-stack-list (points x1 y1 x2 y2 x3 y3)
    (apply #'draw-polygon* stream points :closed t keys)))

(defmethod copy-area (sheet 
		      from-min-x from-min-y 
		      from-max-x from-max-y 
		      to-min-x to-min-y)
  (port-copy-area (sheet-port sheet) 
		  sheet from-min-x from-min-y from-max-x from-max-y to-min-x to-min-y))
