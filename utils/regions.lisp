;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: regions.lisp,v 1.15 92/11/06 19:05:21 cer Exp $

(in-package :clim-utils)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Generic Functions

(defgeneric transform-region (transformation region))
(defgeneric untransform-region (transformation region))

(defgeneric point-position (point)
  (declare (values x y)))
(defgeneric point-x (point))
(defgeneric point-y (point))

;;--- Many of these methods could stand to be written...
(defgeneric region-equal (region1 region2))
(defgeneric region-contains-position-p (region x y))
(defgeneric region-contains-region-p (region1 region2))
(defgeneric region-intersects-region-p (region1 region2))

(defgeneric region-set-function (region))
(defgeneric region-set-regions (region &key normalize))
(defgeneric map-over-region-set-regions (function region &key normalize)
  (declare (dynamic-extent function)))

(defgeneric region-union (region1 region2))
(defgeneric region-intersection (region1 region2))
(defgeneric region-difference (region1 region2))

(defgeneric polyline-closed (polyline))
(defgeneric polygon-points (polygon))
(defgeneric map-over-polygon-coordinates (function polygon)
  (declare (dynamic-extent function)))
(defgeneric map-over-polygon-segments (function polygon)
  (declare (dynamic-extent function)))

(defgeneric line-start-point (line))
(defgeneric line-end-point (line))
(defgeneric line-start-point* (line))
(defgeneric line-end-point* (line))

(defgeneric rectangle-min-point (rectangle))
(defgeneric rectangle-max-point (rectangle))
(defgeneric rectangle-edges* (rectangle)
  (declare (values min-x min-y max-x max-y)))
(defgeneric rectangle-min-x (rectangle))
(defgeneric rectangle-min-y (rectangle))
(defgeneric rectangle-max-x (rectangle))
(defgeneric rectangle-max-y (rectangle))
(defgeneric rectangle-width (rectangle))
(defgeneric rectangle-height (rectangle))
(defgeneric rectangle-size (rectangle)
  (declare (values width height)))

(defgeneric ellipse-center-point (ellipse))
(defgeneric ellipse-center-point* (ellipse))
(defgeneric ellipse-radii (ellipse)
  (declare (values radius-1-dx radius-1-dy radius-2-dx radius-2-dy)))
(defgeneric ellipse-start-angle (ellipse))
(defgeneric ellipse-end-angle (ellipse))

(defgeneric opacity-value (opacity))

(defgeneric bounding-rectangle* (region)
  (declare (values left top right bottom)))
(defgeneric bounding-rectangle-set-edges (region left top right bottom)
  (declare (values region)))
(defgeneric bounding-rectangle-set-position (region x y)
  (declare (values region)))
(defgeneric bounding-rectangle-set-size (region width height)
  (declare (values region)))

(defmacro define-symmetric-region-method (name (region1 region2) &body body)
  `(progn
     (defmethod ,name (,region1 ,region2) ,@body)
     (defmethod ,name (,region2 ,region1) ,@body)))

(defmacro fix-rectangle (left top right bottom)
  `(values (the coordinate (coordinate ,left 'floor))
	   (the coordinate (coordinate ,top  'floor))
	   (the coordinate (coordinate ,right  'ceiling))
	   (the coordinate (coordinate ,bottom 'ceiling))))


;;; The basic design protocol classes

(define-protocol-class design ())


;;--- Watch out, we define methods on this protocol class!
(define-protocol-class opacity (design))

(defmethod print-object ((design opacity) stream)
  (print-unreadable-object (design stream :type t :identity t)
    (format stream "~D" (opacity-value design))))

;; Opacities are unbounded and uniform, so transformations are a no-op
(defmethod transform-region ((transformation transformation) (opacity opacity)) opacity)


;;--- Watch out, we define methods on this protocol class!
(define-protocol-class color (design))

;; Colors are unbounded and uniform, so transformations are a no-op
(defmethod transform-region ((transformation transformation) (color color)) color)


;;; The basic regions protocol classes

(define-protocol-class region (design))

(defmethod untransform-region ((transformation transformation) region)
  (transform-region (invert-transformation transformation) region))


;;; Nowhere

(defclass nowhere (opacity region) ())

(defmethod make-load-form ((nowhere nowhere))
  '+nowhere+)

(defmethod region-equal ((nowhere1 nowhere) (nowhere2 nowhere)) t)

(defmethod region-contains-position-p ((nowhere nowhere) x y)
  (declare (ignore x y))
  nil)

(defmethod region-contains-region-p ((nowhere nowhere) (region region)) nil)
(defmethod region-contains-region-p ((region region) (nowhere nowhere)) t)
(defmethod region-contains-region-p ((nowhere1 nowhere) (nowhere2 nowhere)) t)

(define-symmetric-region-method region-intersects-region-p ((nowhere nowhere) (region region))
  nil)
(defmethod region-intersects-region-p ((nowhere1 nowhere) (nowhere2 nowhere)) nil)

(defmethod transform-region (transformation (region nowhere))
  (declare (ignore transformation))
  region)

(defmethod opacity-value ((design nowhere)) 0f0)

(defvar +nowhere+ (make-instance 'nowhere))


;;; Everywhere

(defclass everywhere (opacity region) ())

(defmethod make-load-form ((everywhere everywhere))
  '+everywhere+)

(defmethod region-equal ((everywhere1 everywhere) (everywhere2 everywhere)) t)

(defmethod region-contains-position-p ((everywhere everywhere) x y) 
  (declare (ignore x y))
  t)

(defmethod region-contains-region-p ((everywhere everywhere) (region region)) t)
(defmethod region-contains-region-p ((region region) (everywhere everywhere)) nil)

(define-symmetric-region-method region-intersects-region-p
				((everywhere everywhere) (region region))
  (not (eq region +nowhere+)))

(defmethod transform-region (transformation (region everywhere))
  (declare (ignore transformation))
  region)

(defmethod opacity-value ((design everywhere)) 1f0)

(defvar +everywhere+ (make-instance 'everywhere))


(define-protocol-class bounding-rectangle ())


;;; Points

(define-protocol-class point (region bounding-rectangle))

(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream :type t :identity t)
    (format stream "(~D,~D)" (point-x point) (point-y point))))


(defclass standard-point (point)
    ((x :initarg :x :accessor point-x :type coordinate)
     (y :initarg :y :accessor point-y :type coordinate)))

(define-constructor make-point-1 standard-point (x y)
  :x x :y y)

(defun make-point (x y)
  (declare (type real x y))
  (make-point-1 (coordinate x) (coordinate y)))

(defmethod make-load-form ((point standard-point))
  (with-slots (x y) point
    `(make-point ,x ,y)))

(defmethod point-position ((point standard-point))
  (with-slots (x y) point
    (values x y)))

(defmethod region-equal ((point1 standard-point) (point2 standard-point))
  (with-slots ((x1 x) (y1 y)) point1
    (declare (type coordinate x1 y1))
    (with-slots ((x2 x) (y2 y)) point2
      (declare (type coordinate x2 y2))
      (and (= x1 x2) (= y1 y2)))))

(defmethod region-contains-position-p ((point standard-point) x y)
  (declare (type real x y))
  (with-slots ((px x) (py y)) point
    (declare (type coordinate px py))
    (and (= px x) (= py y))))

(defmethod region-contains-region-p ((point1 standard-point) (point2 standard-point))
  (with-slots ((x1 x) (y1 y)) point1
    (declare (type coordinate x1 y1))
    (with-slots ((x2 x) (y2 y)) point2
      (declare (type coordinate x2 y2))
      (and (= x1 x2) (= y1 y2)))))

(defmethod region-contains-region-p ((region region) (point standard-point))
  (with-slots (x y) point
    (declare (type coordinate x y))
    (region-contains-position-p region x y)))

(defmethod region-intersects-region-p ((point1 standard-point) (point2 standard-point))
  (with-slots ((x1 x) (y1 y)) point1
    (declare (type coordinate x1 y1))
    (with-slots ((x2 x) (y2 y)) point2
      (declare (type coordinate x2 y2))
      (and (= x1 x2) (= y1 y2)))))

(define-symmetric-region-method region-intersects-region-p
				((point standard-point) (region region))
  (with-slots ((px x) (py y)) point
    (declare (type coordinate px py))
    (region-contains-position-p region px py)))

(defmethod region-intersection ((point1 standard-point) (point2 standard-point))
  (with-slots ((x1 x) (y1 y)) point1
    (declare (type coordinate x1 y1))
    (with-slots ((x2 x) (y2 y)) point2
      (declare (type coordinate x2 y2))
      (if (and (= x1 x2) (= y1 y2)) point1 +nowhere+))))

(defmethod transform-region (transformation (point standard-point))
  (with-slots (x y) point
    (declare (type coordinate x y))
    (multiple-value-bind (x y)
	(transform-position transformation x y)
      (make-point-1 x y))))

(defmethod bounding-rectangle* ((point standard-point))
  (with-slots (x y) point
    (declare (type coordinate x y))
    (fix-rectangle x y (1+ x) (1+ y))))


;;; Paths

(define-protocol-class path (region bounding-rectangle))

(define-protocol-class polyline (path))

(define-protocol-class line (polyline))

(defmethod print-object ((line line) stream)
  (print-unreadable-object (line stream :type t :identity t)
    (multiple-value-bind (start-x start-y) (line-start-point* line)
      (multiple-value-bind (end-x end-y) (line-end-point* line)
	(format stream "(~D,~D)->(~D,~D)" start-x start-y end-x end-y)))))


;;; Areas

(define-protocol-class area (region bounding-rectangle))

(define-protocol-class polygon (area))

(define-protocol-class rectangle (polygon))

(defmethod print-object ((rectangle rectangle) stream)
  (print-unreadable-object (rectangle stream :type t :identity t)
    (multiple-value-bind (left top right bottom)
	(rectangle-edges* rectangle)
      (format stream "/x ~D:~D y ~D:~D/" left right top bottom))))


;;; Rectangles

(defclass standard-rectangle (rectangle)
    ((min-x :initarg :min-x :reader rectangle-min-x :type coordinate)
     (min-y :initarg :min-y :reader rectangle-min-y :type coordinate)
     (max-x :initarg :max-x :reader rectangle-max-x :type coordinate)
     (max-y :initarg :max-y :reader rectangle-max-y :type coordinate)
     (points :initarg :points :type simple-vector :reader polygon-points)))

(define-constructor make-rectangle-1 standard-rectangle 
		    (min-x min-y max-x max-y points)
  :min-x min-x :min-y min-y :max-x max-x :max-y max-y :points points)

(defun make-rectangle (min-point max-point)
  (multiple-value-bind (min-x min-y) (point-position min-point)
    (declare (type coordinate min-x min-y))
    (multiple-value-bind (max-x max-y) (point-position max-point)
      (declare (type coordinate max-x max-y))
      (assert (<= min-x max-x))
      (assert (<= min-y max-y))
      (make-rectangle-1 min-x min-y max-x max-y
			(vector min-point (make-point min-x max-y)
				max-point (make-point max-x min-y))))))

(define-constructor make-rectangle*-1 standard-rectangle
		    (x1 y1 x2 y2)
  :min-x x1 :min-y y1 :max-x x2 :max-y y2)

(defun make-rectangle* (x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (let ((x1 (coordinate x1))
	(y1 (coordinate y1))
	(x2 (coordinate x2))
	(y2 (coordinate y2)))
    (declare (type coordinate x1 y1 x2 y2))
    (when (> x1 x2) (rotatef x1 x2))
    (when (> y1 y2) (rotatef y1 y2))
    (make-rectangle*-1 x1 y1 x2 y2)))

(defmethod make-load-form ((rectangle standard-rectangle))
  `(make-rectangle* ,(rectangle-min-x rectangle) ,(rectangle-min-y rectangle)
		    ,(rectangle-max-x rectangle) ,(rectangle-max-y rectangle)))

(defmethod rectangle-edges* ((rectangle standard-rectangle))
  (with-slots (min-x min-y max-x max-y) rectangle
    (values min-x min-y max-x max-y)))

(defmethod slot-unbound (class (rectangle standard-rectangle) (slot (eql 'points)))
  (declare (ignore class))
  (with-slots (points min-x min-y max-x max-y) rectangle
    (setf points (vector (make-point min-x min-y) (make-point min-x max-y)
			 (make-point max-x max-y) (make-point max-x min-y)))))

(defmethod rectangle-min-point ((rectangle standard-rectangle))
  (with-slots (points) rectangle
    (svref points 0)))

(defmethod rectangle-max-point ((rectangle standard-rectangle))
  (with-slots (points) rectangle
    (svref points 2)))

(defmethod rectangle-width ((rectangle standard-rectangle))
  (with-slots (min-x max-x) rectangle
    (declare (type coordinate min-x max-x))
    (- max-x min-x)))

(defmethod rectangle-height ((rectangle standard-rectangle))
  (with-slots (min-y max-y) rectangle
    (declare (type coordinate min-y max-y))
    (- max-y min-y)))

(defmethod rectangle-size ((rectangle standard-rectangle))
  (with-slots (min-x min-y max-x max-y) rectangle
    (declare (type coordinate min-x min-y max-x max-y))
    (values (- max-x min-x) (- max-y min-y))))

(defmethod map-over-polygon-coordinates (function (rectangle standard-rectangle))
  (with-slots (min-x min-y max-x max-y) rectangle
    (funcall function min-x min-y)
    (funcall function min-x max-y)
    (funcall function max-x max-y)
    (funcall function max-x min-y)
    nil))

(defmethod map-over-polygon-segments (function (rectangle standard-rectangle))
  (with-slots (min-x min-y max-x max-y) rectangle
    (funcall function min-x min-y min-x max-y)
    (funcall function min-x max-y max-x max-y)
    (funcall function max-x max-y max-x min-y)
    (funcall function max-x min-y min-x min-y)
    nil))

(defmethod region-equal ((rect1 standard-rectangle) (rect2 standard-rectangle))
  (with-slots ((sx1 min-x) (sy1 min-y) (ex1 max-x) (ey1 max-y)) rect1
    (with-slots ((sx2 min-x) (sy2 min-y) (ex2 max-x) (ey2 max-y)) rect2
      (ltrb-equals-ltrb-p sx1 sy1 ex1 ey1
			  sx2 sy2 ex2 ey2))))

(defmethod region-contains-position-p ((rectangle standard-rectangle) x y)
  (with-slots (min-x min-y max-x max-y) rectangle
    (ltrb-contains-position-p min-x min-y max-x max-y 
			      (coordinate x) (coordinate y))))

(defmethod region-contains-region-p ((rect1 standard-rectangle) (rect2 standard-rectangle))
  (with-slots ((sx1 min-x) (sy1 min-y) (ex1 max-x) (ey1 max-y)) rect1
    (with-slots ((sx2 min-x) (sy2 min-y) (ex2 max-x) (ey2 max-y)) rect2
      (ltrb-contains-ltrb-p sx1 sy1 ex1 ey1
			    sx2 sy2 ex2 ey2))))

(defmethod region-intersects-region-p ((rect1 standard-rectangle) (rect2 standard-rectangle))
  (with-slots ((sx1 min-x) (sy1 min-y) (ex1 max-x) (ey1 max-y)) rect1
    (with-slots ((sx2 min-x) (sy2 min-y) (ex2 max-x) (ey2 max-y)) rect2
      (ltrb-overlaps-ltrb-p sx1 sy1 ex1 ey1
			    sx2 sy2 ex2 ey2))))

(defmethod transform-region (transformation (rectangle standard-rectangle))
  (with-slots (min-x min-y max-x max-y) rectangle
    (declare (type coordinate min-x min-y max-x max-y))
    (if (rectilinear-transformation-p transformation)
	(multiple-value-bind (x1 y1)
	    (transform-position transformation min-x min-y)
	  (multiple-value-bind (x2 y2)
	      (transform-position transformation max-x max-y)
	    (make-rectangle* x1 y1 x2 y2)))
      (let ((coords nil))
	(flet ((transform-coord (x y)
		 (multiple-value-bind (nx ny)
		     (transform-position transformation x y)
		   (push ny coords)
		   (push nx coords))))
	  (declare (dynamic-extent #'transform-coord))
	  (map-over-polygon-coordinates #'transform-coord rectangle))
	(make-polygon* (nreverse coords))))))

(defmethod bounding-rectangle* ((rectangle standard-rectangle))
  (with-slots (min-x min-y max-x max-y) rectangle
    (declare (type coordinate min-x min-y max-x max-y))
    (fix-rectangle (min min-x max-x) (min min-y max-y)
		   (max min-x max-x) (max min-y max-y))))


;;; General ellipses

(define-protocol-class elliptical-arc (path))

(define-protocol-class ellipse (area))


;;; Bounding rectangles

;; Bounding rectangles live in the "ground" coordinate system, such that
;; LEFT = MIN-X, RIGHT = MAX-X, TOP = MIN-Y, AND BOTTOM = MAX-Y.
;;--- Can we simply inherit from STANDARD-RECTANGLE and flush some
;;--- of the methods below?
(defclass standard-bounding-rectangle (region bounding-rectangle)
    ((left   :initarg :left   :accessor rectangle-min-x :type coordinate)
     (top    :initarg :top    :accessor rectangle-min-y :type coordinate)
     (right  :initarg :right  :accessor rectangle-max-x :type coordinate)
     (bottom :initarg :bottom :accessor rectangle-max-y :type coordinate)))

(defmethod print-object ((object standard-bounding-rectangle) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "/x ~A:~A y ~A:~A/"
	    (safe-slot-value object 'left)
	    (safe-slot-value object 'right)
	    (safe-slot-value object 'top)
	    (safe-slot-value object 'bottom))))

(define-constructor make-bounding-rectangle-1 standard-bounding-rectangle
		    (left top right bottom)
  :left left :top top :right right :bottom bottom)

(defun make-bounding-rectangle (x1 y1 x2 y2)
  (let ((x1 (coordinate x1))
	(y1 (coordinate y1))
	(x2 (coordinate x2))
	(y2 (coordinate y2)))
    (declare (type coordinate x1 y1 x2 y2))
    (when (> x1 x2) (rotatef x1 x2))
    (when (> y1 y2) (rotatef y1 y2))
    (make-bounding-rectangle-1 x1 y1 x2 y2)))

(defmethod make-load-form ((rectangle standard-bounding-rectangle))
  (with-slots (left top right bottom) rectangle
    `(make-bounding-rectangle ,left ,top ,right ,bottom)))

(defmethod transform-region (transformation (rectangle standard-bounding-rectangle))
  (with-slots (left top right bottom) rectangle
    (declare (type coordinate left top right bottom))
    (multiple-value-bind (x1 y1 x2 y2)
	(transform-rectangle* transformation left top right bottom)
      (make-bounding-rectangle x1 y1 x2 y2))))

(defmethod rectangle-max-point ((rectangle standard-bounding-rectangle))
  (make-point (slot-value rectangle 'right) (slot-value rectangle 'bottom)))

(defmethod rectangle-edges* ((rectangle standard-bounding-rectangle))
  (with-slots (left top right bottom) rectangle
    (values left top right bottom)))

(defmethod rectangle-width ((rectangle standard-bounding-rectangle))
  (with-slots (left right) rectangle
    (declare (type coordinate left right))
    (- right left)))

(defmethod rectangle-height ((rectangle standard-bounding-rectangle))
  (with-slots (top bottom) rectangle
    (declare (type coordinate top bottom))
    (- bottom top)))

(defmethod rectangle-size ((rectangle standard-bounding-rectangle))
  (with-slots (left top right bottom) rectangle
    (declare (type coordinate left top right bottom))
    (values (- right left)
	    (- bottom top))))

(defmethod map-over-polygon-coordinates (function (rectangle standard-bounding-rectangle))
  (with-slots (left top right bottom) rectangle
    (funcall function left top)
    (funcall function left bottom)
    (funcall function right bottom)
    (funcall function right top)
    nil))

(defmethod map-over-polygon-segments (function (rectangle standard-bounding-rectangle))
  (with-slots (left top right bottom) rectangle
    (funcall function left top left bottom)
    (funcall function left bottom right bottom)
    (funcall function right bottom right top)
    (funcall function right top left top)
    nil))

;; This and the next three can also serve for output records, which are built
;; on top of BOUNDING-RECTANGLE.
(defmethod region-equal ((rect1 standard-bounding-rectangle) 
			 (rect2 standard-bounding-rectangle))
  (with-slots ((sx1 left) (sy1 top) (ex1 right) (ey1 bottom)) rect1
    (with-slots ((sx2 left) (sy2 top) (ex2 right) (ey2 bottom)) rect2
      (ltrb-equals-ltrb-p sx1 sy1 ex1 ey1
			  sx2 sy2 ex2 ey2))))

(defmethod region-contains-position-p ((rectangle standard-bounding-rectangle) x y)
  (with-slots (left top right bottom) rectangle
    (ltrb-contains-position-p left top right bottom 
			      (coordinate x) (coordinate y))))

(defmethod region-contains-region-p ((rect1 standard-bounding-rectangle) 
				     (rect2 standard-bounding-rectangle))
  (with-slots ((sx1 left) (sy1 top) (ex1 right) (ey1 bottom)) rect1
    (with-slots ((sx2 left) (sy2 top) (ex2 right) (ey2 bottom)) rect2
      (ltrb-contains-ltrb-p sx1 sy1 ex1 ey1
			    sx2 sy2 ex2 ey2))))

(defmethod region-intersects-region-p ((rect1 standard-bounding-rectangle) 
				       (rect2 standard-bounding-rectangle))
  (with-slots ((sx1 left) (sy1 top) (ex1 right) (ey1 bottom)) rect1
    (with-slots ((sx2 left) (sy2 top) (ex2 right) (ey2 bottom)) rect2
      (ltrb-overlaps-ltrb-p sx1 sy1 ex1 ey1
			    sx2 sy2 ex2 ey2))))

(defmacro with-bounding-rectangle* ((left top right bottom) region &body body)
  #+Genera (declare (zwei:indentation 1 3 2 1))
  `(multiple-value-bind (,left ,top ,right ,bottom)
       (bounding-rectangle* ,region) 
     (declare (type coordinate ,left ,top ,right ,bottom))
     ,@body))

(defmethod bounding-rectangle* ((rectangle standard-bounding-rectangle))
  (with-slots (left top right bottom) rectangle
    (values left top right bottom)))

;; Guaranteed to cons a new rectangle unless REUSE-RECTANGLE is supplied
(defun bounding-rectangle (region &optional reuse-rectangle)
  (with-bounding-rectangle* (left top right bottom) region
    (cond (reuse-rectangle
	   (setf (slot-value reuse-rectangle 'left)   left)
	   (setf (slot-value reuse-rectangle 'top)    top)
	   (setf (slot-value reuse-rectangle 'right)  right)
	   (setf (slot-value reuse-rectangle 'bottom) bottom)
	   reuse-rectangle)
	  (t
	   (make-bounding-rectangle left top right bottom)))))

;; Set the edges of the rectangle, and return the rectangle as the value
;; LEFT, TOP, RIGHT, and BOTTOM had better be of type COORDINATE
(defmethod bounding-rectangle-set-edges ((rectangle standard-bounding-rectangle)
					 left top right bottom)
  (declare (type real left top right bottom))
  #+++ignore (assert (<= left right))
  #+++ignore (assert (<= top bottom))
  (with-slots ((bl left) (bt top) (br right) (bb bottom)) rectangle
    (setq bl (coordinate left)
	  bt (coordinate top)
	  br (coordinate right)
	  bb (coordinate bottom)))
  rectangle)

(defmacro define-bounding-rectangle-setf (name &optional (accessor name))
  (check-type accessor (member left top right bottom))
  (let* ((fspec (fintern "~A-~A" 'bounding-rectangle name))
	 (new (fintern "~A-~A" 'new name))
	 (edges '(left top right bottom)))
    ;; The new value had better be of type COORDINATE
    `(defsetf ,fspec (region) (,new)
       `(with-bounding-rectangle* ,',edges ,region
	  (setq ,',accessor ,,new)
	  (bounding-rectangle-set-edges ,region ,@',edges)
	  ,,new))))

(defun-inline bounding-rectangle-min-x (region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) region
    (declare (ignore min-y max-x max-y))
    min-x))
(define-bounding-rectangle-setf min-x left)

(defun-inline bounding-rectangle-min-y (region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) region
    (declare (ignore min-x max-x max-y))
    min-y))
(define-bounding-rectangle-setf min-y top)

(defun-inline bounding-rectangle-max-x (region) 
  (with-bounding-rectangle* (min-x min-y max-x max-y) region
    (declare (ignore min-x min-y max-y))
    max-x))
(define-bounding-rectangle-setf max-x right)

(defun-inline bounding-rectangle-max-y (region) 
  (with-bounding-rectangle* (min-x min-y max-x max-y) region
    (declare (ignore min-x min-y max-x))
    max-y))
(define-bounding-rectangle-setf max-y bottom)

(defun bounding-rectangle-min-point (region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) region 
    (declare (ignore max-x max-y))
    (make-point min-x min-y)))

(defun bounding-rectangle-max-point (region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) region 
    (declare (ignore min-x min-y))
    (make-point max-x max-y)))

(defun-inline bounding-rectangle-position (region)
  (with-bounding-rectangle* (left top right bottom) region 
    (declare (ignore right bottom))
    (values left top)))

;; Set the position of the rectangle, and return the rectangle as the value
(defmethod bounding-rectangle-set-position ((rectangle standard-bounding-rectangle) x y)
  (declare (type real x y))
  (with-slots (left top right bottom) rectangle
    (declare (type coordinate left top right bottom))
    (let ((x (coordinate x))
	  (y (coordinate y))
	  (width (- right left))
	  (height (- bottom top)))
      (declare (type coordinate x y width height))
      (setq left   x
	    top    y
	    right  (+ x width)
	    bottom (+ y  height))))
  rectangle)

;; Make a new bounding rectangle for the region, and shift its position by DX,DY,
;; and return the new rectangle.
(defun bounding-rectangle-shift-position (region dx dy &optional reuse-rectangle)
  (declare (values region))
  (declare (type real dx dy))
  (let ((rectangle (bounding-rectangle region reuse-rectangle))
	(dx (coordinate dx))
	(dy (coordinate dy)))
    (declare (type coordinate dx dy))
    (with-slots (left top right bottom) rectangle
      (declare (type coordinate left top right bottom))
      (incf left   dx)
      (incf top    dy)
      (incf right  dx)
      (incf bottom dy))
    rectangle))

(defun bounding-rectangle-position-equal (region1 region2)
  (multiple-value-bind (x1 y1) (bounding-rectangle-position region1)
    (declare (type coordinate x1 y1))
    (multiple-value-bind (x2 y2) (bounding-rectangle-position region2)
      (declare (type coordinate x2 y2))
      (and (= x1 x2)
	   (= y1 y2)))))

(defun bounding-rectangle-edges-equal (region1 region2)
  (with-bounding-rectangle* (left1 top1 right1 bottom1) region1
    (with-bounding-rectangle* (left2 top2 right2 bottom2) region2
      (and (= left1 left2)
	   (= top1 top2)
	   (= right1 right2)
	   (= bottom1 bottom2)))))

;; This should only be used to compare COORDINATEs
(defun-inline position-difference (x1 y1 x2 y2)
  (declare (type coordinate x1 y1 x2 y2))
  (values (- x1 x2) (- y1 y2)))

(defun bounding-rectangle-position-difference (region1 region2)
  (multiple-value-bind (x1 y1) (bounding-rectangle-position region1)
    (declare (type coordinate x1 y1))
    (multiple-value-bind (x2 y2) (bounding-rectangle-position region2)
      (declare (type coordinate x2 y2))
      (position-difference x1 y1 x2 y2))))

(defun-inline bounding-rectangle-width (region)
  (with-bounding-rectangle* (left top right bottom) region
    (declare (ignore top bottom))
    (- right left)))

(defun-inline bounding-rectangle-height (region)
  (with-bounding-rectangle* (left top right bottom) region 
    (declare (ignore left right))
    (- bottom top)))

(defun-inline bounding-rectangle-size (region)
  (declare (values width height))
  (with-bounding-rectangle* (left top right bottom) region 
    (values (- right left) (- bottom top))))

;; Set the size of the rectangle, and return the rectangle as the value
(defmethod bounding-rectangle-set-size ((rectangle standard-bounding-rectangle) width height)
  (declare (type real width height))
  (with-slots (left top right bottom) rectangle
    (declare (type coordinate left top right bottom))
    (let ((new-right  (+ left (coordinate width)))
	  (new-bottom (+ top  (coordinate height))))
      (setq right  new-right
	    bottom new-bottom)))
  rectangle)

(defun bounding-rectangle-size-equal (region1 region2)
  (with-bounding-rectangle* (left1 top1 right1 bottom1) region1
    (with-bounding-rectangle* (left2 top2 right2 bottom2) region2
      (ltrb-size-equal left1 top1 right1 bottom1
		       left2 top2 right2 bottom2))))

(defun bounding-rectangle-center (region)
  (with-bounding-rectangle* (left top right bottom) region
    (make-point (+ left (/ (- right left) 2))
		(+ top (/ (- bottom top) 2)))))

(defun bounding-rectangle-center* (region)
  (with-bounding-rectangle* (left top right bottom) region
    (values (+ left (/ (- right left) 2))
	    (+ top (/ (- bottom top) 2)))))

(defun bounding-rectangle-ltrb (region)
  (declare (values left top right bottom))
  (with-bounding-rectangle* (left top right bottom) region
    (values left top right bottom)))

(defmacro with-bounding-rectangle-ltrb ((left top right bottom) region &body body)
  `(with-bounding-rectangle* (,left ,top ,right ,bottom) ,region
     ,@body))

(defun-inline bounding-rectangle-left (region)
  (with-bounding-rectangle-ltrb (left top right bottom) region
    (declare (ignore top right bottom))
    left))
(define-bounding-rectangle-setf left)

(defun-inline bounding-rectangle-top (region)
  (with-bounding-rectangle-ltrb (left top right bottom) region
    (declare (ignore left right bottom))
    top))
(define-bounding-rectangle-setf top)

(defun-inline bounding-rectangle-right (region) 
  (with-bounding-rectangle-ltrb (left top right bottom) region 
    (declare (ignore left top bottom))
    right))
(define-bounding-rectangle-setf right)

(defun-inline bounding-rectangle-bottom (region) 
  (with-bounding-rectangle-ltrb (left top right bottom) region 
    (declare (ignore left top right))
    bottom))
(define-bounding-rectangle-setf bottom)

(defgeneric* (setf bounding-rectangle*) (left top right bottom region))
(defmethod* (setf bounding-rectangle*) 
	    (left top right bottom (region standard-bounding-rectangle))
  (bounding-rectangle-set-edges region left top right bottom))

(defgeneric* (setf bounding-rectangle-position) (x y region))
(defmethod* (setf bounding-rectangle-position) 
	    (x y (region standard-bounding-rectangle))
  (bounding-rectangle-set-position region x y))

(defgeneric* (setf bounding-rectangle-size) (width height region))
(defmethod* (setf bounding-rectangle-size) 
	    (width height (region standard-bounding-rectangle))
  (bounding-rectangle-set-size region width height))


;;; Region Sets

(define-protocol-class region-set (region bounding-rectangle))

;;; Some default methods.

(defmethod region-set-function ((region region)) 'union)

(defmethod region-set-regions ((region region) &key normalize)
  (declare (ignore normalize))
  (list region))

(defmethod map-over-region-set-regions (function (region region) &key normalize)
  (declare (dynamic-extent function) (ignore normalize))
  (funcall function region))

(defmethod map-over-region-set-regions (function (region region-set) &rest args &key normalize)
  (declare (dynamic-extent function args))
  (declare (ignore normalize))
  (map nil function (apply #'region-set-regions region args)))

#+++ignore
(defmethod region-equal ((set1 region-set) (set2 region-set))
  ;;--- How to do this?
  )

(defmethod region-contains-position-p ((region-set region-set) x y)
  (flet ((contains-position-p (region)
	   (when (region-contains-position-p region x y)
	     (return-from region-contains-position-p t))))
    (declare (dynamic-extent #'contains-position-p))
    (map-over-region-set-regions #'contains-position-p region-set))
  nil)

(defmethod region-contains-region-p ((region-set region-set) (other-region region))
  (flet ((contains-region-p (region)
	   (when (region-contains-region-p region other-region)
	     (return-from region-contains-region-p t))))
    (declare (dynamic-extent #'contains-region-p))
    (map-over-region-set-regions #'contains-region-p region-set))
  nil)

#++ignore
(defmethod region-intersects-region-p ((set1 region-set) (set2 region-set))
  ;;--- How to do this?
  )

(defmethod bounding-rectangle* ((region-set region-set))
  (let ((left nil) (top nil) (right nil) (bottom nil))
    (flet ((add-region (region)
	     (with-bounding-rectangle* (rl rt rr rb) region
	       (minf-or left rl)
	       (minf-or top  rt)
	       (maxf-or right  rr)
	       (maxf-or bottom rb))))
      (declare (dynamic-extent #'add-region))
      (map-over-region-set-regions #'add-region region-set))
    (values left top right bottom)))


;;; Geometry utilities

(defconstant pi-single-float (coerce pi 'single-float))
(defconstant  2pi (coerce (* pi-single-float 2) 'single-float))
(defconstant pi/2 (coerce (/ pi-single-float 2) 'single-float))

(defun radians->degrees (radians)
  (* radians (/ 360 2pi)))

(defun degrees->radians (degrees)
  (* degrees (/ 2pi 360)))

;; This macro wouldn't be necessary if we could count on (expt (expression) 2)
;; being optimized properly
(defmacro square (expression)
  (if (symbolp expression)
      `(* ,expression ,expression)
      (let ((var (gensymbol)))
	`(let ((,var ,expression))
	   (* ,var ,var)))))

;; This runs when we already know that the point is inside the bounding box.
(defun point-close-to-line-p (x y from-x from-y to-x to-y &optional (thickness 1))
  (let ((distance (1+ (ceiling thickness 2)))
	(dx (- to-x from-x))
	(dy (- to-y from-y)))
    (or (and (zerop dx) (zerop dy))
	(<= (square (- (* y dx) (* x dy) (- (* from-y to-x) (* from-x to-y))))
	    (* (square distance) (+ (square dx) (square dy)))))))

;; Computes whether a point is inside an ellipse whose center is (0,0).
;; This calculation is exact.
(defun point-inside-ellipse-p (x y radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
  (<= (+ (square (- (* radius-2-dy x) (* radius-2-dx y)))
	 (square (- (* radius-1-dx y) (* radius-1-dy x))))
      (square (- (* radius-1-dx radius-2-dy) (* radius-1-dy radius-2-dx)))))

;; Computes whether a point is on a stroked ellipse whose center is (0,0).
;; This calculation is not exact - the envelope of an ellipse is not an ellipse
;; and an "average radius" is used - but it should be ok for thickness small
;; compared to radii.  The calculation is exact for circles.
(defun point-on-thick-ellipse-p (x y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 half-thickness)
  (let* ((det (- (* radius-1-dx radius-2-dy) (* radius-1-dy radius-2-dx)))
	 (avrad*delta (* (sqrt (abs det)) half-thickness)))
    (<= (square (- det avrad*delta))
	(+ (square (- (* radius-2-dy x) (* radius-2-dx y)))
	   (square (- (* radius-1-dx y) (* radius-1-dy x))))
	(square (+ det avrad*delta)))))

;; Find the singular value decomposition of a 2 by 2 matrix: M = R1.D.R2
;; where R's are rotations and D is diagonal.  The four values returned
;; are the first angle, the two diagonal elements, and the second angle.
;; Used to convert CLIM's representation of ellipses to various window
;; systems' representations.
(defun 2x2-singular-value-decomposition (a b c d)
  (cond ((and (zerop b) (zerop c))
	 (values 0.0 a d 0.0))
	((and (zerop a) (zerop d))
	 (values pi/2 b (- c) 0.0))
	(T
	 (let* ((d+a (+ d a)) (a-d (- a d))
		(c+b (+ c b)) (c-b (- c b))
		(sx+sy (sqrt (+ (square d+a) (square c-b))))
		(sx-sy (sqrt (+ (square a-d) (square c+b))))
		(sx (* 0.5 (+ sx+sy sx-sy)))
		(sy (* 0.5 (- sx+sy sx-sy)))
		(t1+t2 (if (and (zerop c-b) (zerop d+a)) 0.0 (atan c-b d+a)))
		(t1-t2 (if (and (zerop c+b) (zerop a-d)) 0.0 (atan c+b a-d)))
		(t1 (* 0.5 (+ t1+t2 t1-t2)))
		(t2 (* 0.5 (- t1+t2 t1-t2))))
	   (values t2 sx sy t1)))))

;; For a complete ellipse, the box is actually the rectangle that bounds
;; the parallelogram that bounds the ellipse.  That means it's a little
;; bigger than the tightest possible bounding box when the ellipse is
;; not axis-aligned.  It's not worth computing anything tighter because
;; the refined highlighting test will be faster than the computation of
;; a tighter box.
(defun elliptical-arc-box (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			   theta-1 theta-2 thickness)
  (let* ((filled (null thickness))
	 (thickness (or thickness 0))
	 (lthickness (floor thickness 2))
	 (rthickness (- thickness lthickness)))
    (when (null theta-1)
      (return-from elliptical-arc-box
	(let ((dx (+ (abs radius-1-dx) (abs radius-2-dx)))
	      (dy (+ (abs radius-1-dy) (abs radius-2-dy))))
	  (fix-rectangle (- center-x dx lthickness) (- center-y dy lthickness)
			 (+ center-x dx rthickness) (+ center-y dy rthickness)))))
    (setq theta-1 (mod theta-1 2pi)
	  theta-2 (mod theta-2 2pi))
    (multiple-value-bind (x-radius y-radius)
	(cond ((and (= radius-1-dx 0) (= radius-2-dy 0))
	       (values (abs radius-2-dx) (abs radius-1-dy)))
	      ((and (= radius-2-dx 0) (= radius-1-dy 0))
	       (values (abs radius-1-dx) (abs radius-2-dy)))
	      (t
	       (let ((s-1 (+ (* radius-1-dx radius-1-dx) 
			     (* radius-1-dy radius-1-dy)))
		     (s-2 (+ (* radius-2-dx radius-2-dx) 
			     (* radius-2-dy radius-2-dy))))
		 (if (= s-1 s-2)
		     (let ((r (truncate (sqrt s-1))))
		       (values r r))
		   ;; Degrade to drawing a rectilinear ellipse
		   (values (truncate (sqrt s-1)) 
			   (truncate (sqrt s-2)))))))
      (let* ((x1 (+ center-x (* x-radius (cos theta-1))))
	     (y1 (+ center-y (* y-radius (sin theta-1))))
	     (x2 (+ center-x (* x-radius (cos theta-2))))
	     (y2 (+ center-y (* y-radius (sin theta-2))))
	     (left (min x1 x2))
	     (top (min y1 y2))
	     (right (max x1 x2))
	     (bottom (max y1 y2)))
	(when (angle-between-angles-p pi-single-float theta-1 theta-2)
	  (minf left (- center-x x-radius)))
	(when (angle-between-angles-p (* pi-single-float 3/2) theta-1 theta-2)
	  (minf top (- center-y y-radius)))
	(when (angle-between-angles-p 0 theta-1 theta-2)
	  (maxf right (+ center-x x-radius)))
	(when (angle-between-angles-p pi/2 theta-1 theta-2)
	  (maxf bottom (+ center-y y-radius)))
	(when filled
	  (minf left center-x)
	  (minf top center-y)
	  (maxf right center-x)
	  (maxf bottom center-y))
	(fix-rectangle (- left lthickness) (- top lthickness)
		       (+ right rthickness) (+ bottom rthickness))))))

(defun angle-between-angles-p (theta theta-1 theta-2)
  (unless (< theta-1 theta-2)
    (incf theta-2 2pi))
  (unless (< theta-1 theta)
    (incf theta 2pi))
  (< theta theta-2))


;; Exclude the general cases of REGION-EQUAL
(define-symmetric-region-method region-equal ((region region) (nowhere nowhere)) nil)
(define-symmetric-region-method region-equal ((region region) (everywhere everywhere)) nil)
(define-symmetric-region-method region-equal ((point point) (path path)) nil)
(define-symmetric-region-method region-equal ((point point) (area area)) nil)
(define-symmetric-region-method region-equal ((path path) (area area)) nil)
(define-symmetric-region-method region-equal ((line polyline) (arc elliptical-arc)) nil)
(define-symmetric-region-method region-equal ((polygon polygon) (ellipse ellipse)) nil)

;; Exclude the general cases of REGION-CONTAINS-REGION-P
(defmethod region-contains-region-p ((point point) (path path)) nil)
(defmethod region-contains-region-p ((point point) (area area)) nil)
(defmethod region-contains-region-p ((path path) (area area)) nil)
(defmethod region-contains-region-p ((line polyline) (arc elliptical-arc)) nil)
(defmethod region-contains-region-p ((region1 region) (region2 region))
  (or (eq region1 region2)
      (error "No ~S method defined between objects of type ~S and ~S"
	     'region-contains-region-p (type-of region1) (type-of region2))))

;; Exclude the general cases of REGION-INTERSECTS-REGION-P
(defmethod region-intersects-region-p ((region1 region) (region2 region))
  (or (eq region1 region2)
      (error "No ~S method defined between objects of type ~S and ~S"
	     'region-intersects-region-p (type-of region1) (type-of region2))))
