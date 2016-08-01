;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :silica)

;;;"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 Xerox Corp.	All rights reserved."


;;;
;;; Boxing Panes
;;;



(defclass box-pane (layout-pane)
    ((spacing :initform 0 :initarg :spacing)
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
	(unless (and (typep (first child) '(or (member :fill) (real 0 *)))
		     (panep (second child)))
	  (error "Invalid box child: ~S" child))
	(sheet-adopt-child pane (second child)))))
  (setf (slot-value pane 'contents) contents))

(defmethod handle-event :after ((pane box-pane) (event pointer-motion-event))
  (deallocate-event event))

(defmethod compose-box 
    ((box-pane box-pane)
     fn-major fn-major+ fn-major- fn-minor fn-minor+ fn-minor- width-or-height
     keys)
  (with-slots (contents spacing) box-pane
    (if (null contents) 
	#-(or aclpc acl86win32)
	(make-space-requirement)
	#+(or aclpc acl86win32)
	(apply #'make-space-requirement
		 (mapcan #'list
			 keys 
			 (list 20 20 20 20 20 20)))
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
			    (incf major+ +fill+)
			    (incf major (scale (funcall fn-major space-req)))
			    (incf major- (scale (funcall fn-major- space-req))))
			   ((= scale 1.0)
			    (incf major+ (scale (funcall fn-major+ space-req)))
			    (incf major (scale (funcall fn-major space-req)))
			    (incf major- (scale (funcall fn-major- space-req))))
			   ((> scale 1.0)
			    (incf major+ scale)
			    (incf major scale)
			    (incf major- scale))
			   (t
			    ;; It is maxf because we use the scaling
			    ;; to calculate the desired size of the
			    ;; overall pane
			    (maxf major+ (scale (funcall fn-major+ space-req)))
			    (maxf major (scale (funcall fn-major space-req)))
			    (maxf major- (scale (funcall fn-major- space-req)))))
		     (setq minor (max minor (funcall fn-minor space-req))) 
		     (maxf minor-min (funcall fn-minor- space-req))
		     (minf minor-max (funcall fn-minor+ space-req)))))))
	;;--- These calcs lead to weirdness when fills are involved.
	;;--- This looks like a leftover from the time when +/- were relative
	;;(setq minor- (max 0 (- minor minor-min)))
	;;(setq minor+ (max 0 (- minor-max minor)))
	(setq minor- minor-min)
	(setq minor+ minor-max)
	(let ((extra (* (1- (length (sheet-children box-pane))) spacing)))
	  (apply #'make-space-requirement
		 (mapcan #'list
			 keys 
			 (list (+ extra major) 
			       (+ extra major+) 
			       (+ extra major-)
			       minor minor+ minor-))))))))


(defclass hbox-pane (box-pane) 
	  ((spacing :initarg :x-spacing)))

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
  (with-slots (contents spacing) box-pane
    (let ((space-requirement
	    (compose-space box-pane :width width :height height)))
      (flet ((compose (x)
	       (cond ((atom x) (compose-space x :height height))
		     ((eq (car x) :fill) :fill)
		     (t (car x)))))
	(declare (dynamic-extent #'compose))
	(let* ((adjust (* spacing (1- (length (sheet-children box-pane)))))
	       (sizes 
		(allocate-space-to-items
		  (- width adjust)
		  (space-requirement+* space-requirement :height (- adjust))
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
		      (move-and-resize-sheet sheet x 0 size height)
		      (incf x spacing))
		    (incf x size))
		contents sizes))))))


(defclass vbox-pane (box-pane) 
	  ((spacing :initarg :y-spacing)))

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
  (with-slots (contents spacing) box-pane
    (let ((space-requirement
	    (compose-space box-pane :width width :height height)))
      (flet ((compose (x)
	       (cond ((atom x) (compose-space x :width width))
		     ((eq (car x) :fill) :fill)
		     (t (car x)))))
	(declare (dynamic-extent #'compose))
	(let* ((adjust (* spacing (1- (length (sheet-children box-pane)))))
	       (sizes 
		(allocate-space-to-items
		  (- height adjust)
		  (space-requirement+* space-requirement :height (- adjust))
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
		      (move-and-resize-sheet sheet 0 y width size)
		      (incf y spacing))
		    (incf y size))
		contents sizes))))))

;;; Bulletin board
;;; Is there any value in this???
;;; What do we want it to do?
;;; Panes are just as big as they want to be and where they want to be.
;;; Could specify position and size in turns of relative coordinates.
;;; 

(defclass bulletin-board-pane (layout-pane) 
  ((contents :initarg :contents	 :initform nil)))

(defmethod handle-event :after ((pane bulletin-board-pane) (event pointer-motion-event))
  (deallocate-event event))

(defmethod initialize-instance :after ((pane bulletin-board-pane) &key contents)
  (dolist (item contents)
    (destructuring-bind (position child) item
      (check-type position (or point cons))
      (sheet-adopt-child pane child))))

(defmethod compose-space ((pane bulletin-board-pane) &key width height)
  (declare (ignore width height))
  (with-slots (contents) pane
    (let ((max-x 0)
	  (max-y 0))
      (dolist (content contents)
	(destructuring-bind (position pane) content
	  (multiple-value-bind (x y)
	      (typecase position
		(point (values (point-x position)
			       (point-y position)))
		(cons
		 (values (first position)
			 (second position))))
	    (let ((sr (compose-space pane)))
	      (maxf max-x (+ x (space-requirement-width sr)))
	      (maxf max-y (+ y (space-requirement-height sr)))))))
      (make-space-requirement :width max-x :height max-y))))


(defmethod allocate-space ((pane bulletin-board-pane) width height)
  (declare (ignore width height))
  (with-slots (contents) pane
    (dolist (content contents)
      (destructuring-bind (position pane) content
	(let ((sr (compose-space pane)))
	  (multiple-value-bind (x y)
	      (typecase position
		(point (values (point-x position)
			       (point-y position)))
		(cons
		 (values (first position)
			 (second position))))
	    (move-and-resize-sheet 
	     pane x y 
	     (space-requirement-width sr)
	     (space-requirement-height sr))))))))


(defmacro bulletin-board (options &rest contents)
  `(make-pane 'bulletin-board-pane
	      :contents
	      (list ,@(mapcar #'(lambda (item)
				  (destructuring-bind (position sheet) item
				    `(list (list ,@position) ,sheet)))
			      contents))
	      ,@options))

