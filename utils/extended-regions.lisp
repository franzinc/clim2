;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: extended-regions.lisp,v 1.3 92/05/07 13:11:37 cer Exp $

(in-package :clim-utils)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Extended regions

;;; Lines

(defclass standard-line (line)
    ((start-x :initarg :start-x :type coordinate)
     (start-y :initarg :start-y :type coordinate)
     (end-x :initarg :end-x :type coordinate)
     (end-y :initarg :end-y :type coordinate)
     (points :type simple-vector :initarg :points :reader polygon-points)))

(define-constructor make-line-1 standard-line (start-x start-y end-x end-y points)
		    :start-x start-x :start-y start-y :end-x end-x :end-y end-y :points points)

(defun make-line (start-point end-point)
  (make-line-1 (point-x start-point) (point-y start-point)
	       (point-x end-point) (point-y end-point)
	       (vector start-point end-point)))

(define-constructor make-line*-1 standard-line (start-x start-y end-x end-y)
		    :start-x start-x :start-y start-y :end-x end-x :end-y end-y)

(defun make-line* (start-x start-y end-x end-y)
  (declare (type real start-x start-y end-x end-y))
  (make-line*-1 (coordinate start-x) (coordinate start-y)
		(coordinate end-x) (coordinate end-y)))

(defmethod make-load-form ((line standard-line))
  `(make-line ',(line-start-point line) ',(line-end-point line)))

(defmethod line-start-point* ((line standard-line))
  (with-slots (start-x start-y) line
    (values start-x start-y)))

(defmethod line-end-point* ((line standard-line))
  (with-slots (end-x end-y) line
    (values end-x end-y)))

(defmethod slot-unbound (class (line standard-line) (slot (eql 'points)))
  (declare (ignore class))
  (with-slots (points start-x start-y end-x end-y) line
    (setf points (vector (make-point start-x start-y) (make-point end-x end-y)))))

(defmethod line-start-point ((line standard-line))
  (with-slots (points) line
    (svref points 0)))

(defmethod line-end-point ((line standard-line))
  (with-slots (points) line
    (svref points 1)))

(defmethod polyline-closed ((line standard-line))
  nil)

(defmethod map-over-polygon-coordinates (function (line standard-line))
  (with-slots (start-x start-y end-x end-y) line
    (funcall function start-x start-y)
    (funcall function end-x end-y)
    nil))

(defmethod map-over-polygon-segments (function (line standard-line))
  (with-slots (start-x start-y end-x end-y) line
    (funcall function start-x start-y end-x end-y)
    nil))

(defmethod region-equal ((line1 standard-line) (line2 standard-line))
  (with-slots ((sx1 start-x) (sy1 start-y) (ex1 end-x) (ey1 end-y)) line1
   (declare (type coordinate sx1 sy1 ex1 ey1))
    (with-slots ((sx2 start-x) (sy2 start-y) (ex2 end-x) (ey2 end-y)) line2
      (declare (type coordinate sx2 sy2 ex2 ey2))
      (or (and (= sx1 sx2) (= sy1 sy2) (= ex1 ex2) (= ey1 ey2))
	  (and (= sx1 ex2) (= sy1 ey2) (= ex1 sx2) (= ey1 sy2))))))

;; By using perpendicular-distance from line instead of slope and intercept
;; we don't have to worry about divide by zero in slope and we're also more
;; robust against roundoff error.
(defmethod region-contains-position-p ((line standard-line) x y)
  (with-slots (start-x start-y end-x end-y) line
    (let ((x1 start-x) (y1 start-y) (x2 end-x) (y2 end-y))
      (declare (type coordinate x1 y1 x2 y2))
      (when (or (<= x1 x x2)
		(>= x1 x x2))
	(= (+ (* (- y2 y1) x)
	      (* (- x1 x2) y))
	   (- (* x1 y2) (* x2 y1)))))))

(defmethod region-contains-region-p ((line1 standard-line) (line2 standard-line))
  (with-slots (start-x start-y end-x end-y) line2
    (and (region-contains-position-p line1 start-x start-y)
	 (region-contains-position-p line1 end-x end-y))))

(defmethod region-intersects-region-p ((line1 standard-line) (line2 standard-line))
  (with-slots ((sx1 start-x) (sy1 start-y) (ex1 end-x) (ey1 end-y)) line1
    (with-slots ((sx2 start-x) (sy2 start-y) (ex2 end-x) (ey2 end-y)) line2
      (let ((sx1 sx1) (sy1 sy1) (ex1 ex1) (ey1 ey1)
	    (sx2 sx2) (sy2 sy2) (ex2 ex2) (ey2 ey2))
	(declare (type coordinate sx1 sy1 ex1 ey1
				  sx2 sy2 ex2 ey2))
	(and (>= (max sx2 ex2) (min sx1 ex1))
	     (>= (max sx1 ex1) (min sx2 ex2))
	     (let ((dx1 (- ex1 sx1)) (dy1 (- ey1 sy1))
		   (dx2 (- ex2 sx2)) (dy2 (- ey2 sy2)))
	       (and (= (* dx1 dy2) (* dx2 dy1)) ;slopes equal
		    (= (* dx1 (- sy1 sy2)) (* dy1 (- sx1 sx2))))))))))

(defmethod region-intersection ((line1 standard-line) (line2 standard-line))
  (if (region-intersects-region-p line1 line2)
      (with-slots ((sx1 start-x) (sy1 start-y) (ex1 end-x) (ey1 end-y)) line1
	(declare (type coordinate sx1 sy1 ex1 ey1))
	(with-slots ((sx2 start-x) (sy2 start-y) (ex2 end-x) (ey2 end-y)) line2
	  (declare (type coordinate sx2 sy2 ex2 ey2))
	  (make-line* (max sx1 sx2) (max sy1 sy2) (min ex1 ex2) (min ey1 ey2))))
      +nowhere+))

(defmethod transform-region (transformation (line standard-line))
  (with-slots (start-x start-y end-x end-y) line
    (multiple-value-bind (sx sy)
	(transform-position transformation start-x start-y)
      (multiple-value-bind (ex ey)
	  (transform-position transformation end-x end-y)
	(make-line* sx sy ex ey)))))

(defmethod bounding-rectangle* ((line standard-line))
  (with-slots (start-x start-y end-x end-y) line
    (declare (type coordinate start-x start-y end-x end-y))
    (fix-rectangle (min start-x end-x) (min start-y end-y)
		   (max start-x end-x) (max start-y end-y))))


;;; Polygons and polylines

(defclass polygon-mixin ()
    ((coords :type vector :initarg :coords)
     (points :type vector :initarg :points :reader polygon-points)))

(defmethod slot-unbound (class (polygon polygon-mixin) (slot (eql 'points)))
  (declare (ignore class))
  (let* ((coords (slot-value polygon 'coords))
	 (npoints (/ (length coords) 2))
	 (points (make-array npoints :fill-pointer nil)))
    (dotimes (i npoints)
      (setf (aref points i) (make-point (aref coords (+ (* i 2) 0))
					(aref coords (+ (* i 2) 1)))))
    (setf (slot-value polygon 'points) points)))

(defmethod slot-unbound (class (polygon polygon-mixin) (slot (eql 'coords)))
  (declare (ignore class))
  (let* ((points (slot-value polygon 'points))
	 (npoints (length points))
	 (coords (make-array (* npoints 2) :fill-pointer nil)))
    (dotimes (i npoints)
      (setf (aref coords (+ (* i 2) 0)) (point-x (aref points i))
	    (aref coords (+ (* i 2) 1)) (point-y (aref points i))))
    (setf (slot-value polygon 'coords) coords)))

(defmethod map-over-polygon-coordinates (function (polygon polygon-mixin))
  (with-slots (coords points) polygon
    (if (slot-boundp polygon 'coords)
	(let ((ncoords (1- (length coords)))
	      (i -1))
	  (loop
	    (funcall function (aref coords (incf i)) (aref coords (incf i)))
	    (when (= i ncoords) (return)))
	  nil)
	(flet ((map-coordinates (point)
		 (funcall function (point-x point) (point-y point))))
	  (declare (dynamic-extent #'map-coordinates))
	  (map nil #'map-coordinates points))))
  nil)

(defmethod map-over-polygon-segments (function (polygon polygon-mixin))
  (with-slots (coords points) polygon
    (if (slot-boundp polygon 'coords)
	(let* ((ncoords (1- (length coords)))
	       (x1 (aref coords 0))
	       (y1 (aref coords 1))
	       (x x1)
	       (y y1)
	       (i 1))
	  (loop
	    (funcall function x y
		     (setf x (aref coords (incf i))) (setf x (aref coords (incf i))))
	    (when (= i ncoords) (return)))
	  (when (polyline-closed polygon)
	    (funcall function x y x1 y1)))
	(multiple-value-bind (x1 y1)
	    (point-position (aref points 0))
	  (let ((x x1) (y y1))
	    (dotimes (i (1- (length points)))
	      (multiple-value-bind (nx ny)
		  (point-position (aref points (1+ i)))
		(funcall function x y nx ny)
		(psetf x nx y ny)))
	    (when (polyline-closed polygon)
	      (funcall function x y x1 y1)))))
    nil))

(defmethod bounding-rectangle* ((polygon polygon-mixin))
  (let ((min-x nil) (min-y nil) (max-x nil) (max-y nil))
    (flet ((add-coord (x y)
	     (minf-or min-x x)
	     (minf-or min-y y)
	     (maxf-or max-x x)
	     (maxf-or max-y y)))
      (declare (dynamic-extent #'add-coord))
      (map-over-polygon-coordinates #'add-coord polygon))
    (fix-rectangle min-x min-y max-x max-y)))


(defclass standard-polyline (polygon-mixin polyline)
    ((closed :initarg :closed :reader polyline-closed)))

(define-constructor make-polyline standard-polyline (point-seq &key closed)
		    :points (coerce point-seq 'vector) :closed closed)

(define-constructor make-polyline* standard-polyline (coord-seq &key closed)
		    :coords (coerce coord-seq 'vector) :closed closed)

(defmethod make-load-form ((polyline standard-polyline))
  (with-slots (closed) polyline
    `(make-polyline ',(polygon-points polyline) :closed ,closed)))

(defmethod transform-region (transformation (polyline standard-polyline))
  (let ((coords nil))
    (flet ((transform-coord (x y)
	     (multiple-value-bind (nx ny)
		 (transform-position transformation x y)
	       (push ny coords)
	       (push nx coords))))
      (declare (dynamic-extent #'transform-coord))
      (map-over-polygon-coordinates #'transform-coord polyline))
    (make-polyline* (nreverse coords) :closed (slot-value polyline 'closed))))


(defclass standard-polygon (polygon-mixin polygon) ())

(define-constructor make-polygon standard-polygon (point-seq)
		    :points (coerce point-seq 'vector))

(define-constructor make-polygon* standard-polygon (coord-seq)
		    :coords (coerce coord-seq 'vector))

(defmethod make-load-form ((polygon standard-polygon))
  `(make-polygon ',(polygon-points polygon)))

(defmethod polyline-closed ((polygon standard-polygon))
  t)

(defmethod transform-region (transformation (polygon standard-polygon))
  (let ((coords nil))
    (flet ((transform-coord (x y)
	     (multiple-value-bind (nx ny)
		 (transform-position transformation x y)
	       (push ny coords)
	       (push nx coords))))
      (declare (dynamic-extent #'transform-coord))
      (map-over-polygon-coordinates #'transform-coord polygon))
    (make-polygon* (nreverse coords))))


;;; Ellipses and elliptical arcs

(defclass ellipse-mixin ()
    ((center-point :type point :initarg :center-point :reader ellipse-center-point)
     (center-x :initarg :center-x :type coordinate)
     (center-y :initarg :center-y :type coordinate)
     (radius-1-dx :initarg :radius-1-dx :type coordinate)
     (radius-1-dy :initarg :radius-1-dy :type coordinate)
     (radius-2-dx :initarg :radius-2-dx :type coordinate)
     (radius-2-dy :initarg :radius-2-dy :type coordinate)
     (start-angle :initarg :start-angle :reader ellipse-start-angle :type single-float)
     (end-angle :initarg :end-angle :reader ellipse-end-angle :type single-float)))

(defmethod slot-unbound (class (ellipse ellipse-mixin) (slot (eql 'ellipse-center-point)))
  (declare (ignore class))
  (with-slots (center-point center-x center-y) ellipse
    (setf center-point (make-point center-x center-y))))

(defmethod ellipse-center-point* ((ellipse ellipse-mixin))
  (with-slots (center-x center-y) ellipse
    (values center-x center-y)))

(defmethod ellipse-radii ((ellipse ellipse-mixin))
  (with-slots (radius-1-dx radius-1-dy radius-2-dx radius-2-dy) ellipse
    (values radius-1-dx radius-1-dy radius-2-dx radius-2-dy)))


(defclass standard-elliptical-arc (ellipse-mixin elliptical-arc) ())

(define-constructor make-elliptical-arc standard-elliptical-arc
  (center-point radius-1-dx radius-1-dy radius-2-dx radius-2-dy
		&key start-angle end-angle)
  :center-point center-point :center-x (point-x center-point) :center-y (point-y center-point)
  :radius-1-dx (coordinate radius-1-dx) :radius-1-dy (coordinate radius-1-dy)
  :radius-2-dx (coordinate radius-2-dx) :radius-2-dy (coordinate radius-2-dy)
  :start-angle (cond (start-angle (float start-angle 0f0))
		     (end-angle 0f0)
		     (t nil))
  :end-angle (cond (end-angle (float end-angle 0f0))
		   (start-angle (float (* 2 pi) 0f0))
		   (t nil)))

(define-constructor make-elliptical-arc* standard-elliptical-arc
  (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	    &key start-angle end-angle)
  :center-x center-x :center-y center-y
  :radius-1-dx (coordinate radius-1-dx) :radius-1-dy (coordinate radius-1-dy)
  :radius-2-dx (coordinate radius-2-dx) :radius-2-dy (coordinate radius-2-dy)
  :start-angle (cond (start-angle (float start-angle 0f0))
		     (end-angle 0f0)
		     (t nil))
  :end-angle (cond (end-angle (float end-angle 0f0))
		   (start-angle (float (* 2 pi) 0f0))
		   (t nil)))

(defmethod make-load-form ((ellipse standard-elliptical-arc))
  (with-slots (center-point radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	       start-angle end-angle) ellipse
    `(make-elliptical-arc ',center-point
			  ,radius-1-dx ,radius-1-dy ,radius-2-dx ,radius-2-dy
			  ,@(when start-angle `(:start-angle ,start-angle))
			  ,@(when end-angle `(:end-angle ,end-angle)))))

(defmethod transform-region (transformation (ellipse standard-elliptical-arc))
  (with-slots (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	       start-angle end-angle) ellipse
    (multiple-value-bind (cx cy)
	(transform-position transformation center-x center-y)
      (multiple-value-bind (r1-dx r1-dy)
	  (transform-distance transformation radius-1-dx radius-1-dy)
	(multiple-value-bind (r2-dx r2-dy)
	    (transform-distance transformation radius-2-dx radius-2-dy)
	  (make-elliptical-arc* cx cy r1-dx r1-dy r2-dx r2-dy
				;;--- How to transform start and end angles?
				:start-angle start-angle :end-angle end-angle))))))

(defmethod bounding-rectangle* ((ellipse standard-elliptical-arc))
  (with-slots (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	       start-angle end-angle) ellipse
    (elliptical-arc-box center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			start-angle end-angle 0)))


(defclass standard-ellipse (ellipse-mixin ellipse) ())

(define-constructor make-ellipse standard-ellipse
  (center-point radius-1-dx radius-1-dy radius-2-dx radius-2-dy
		&key start-angle end-angle)
  :center-point center-point :center-x (point-x center-point) :center-y (point-y center-point)
  :radius-1-dx (coordinate radius-1-dx) :radius-1-dy (coordinate radius-1-dy)
  :radius-2-dx (coordinate radius-2-dx) :radius-2-dy (coordinate radius-2-dy)
  :start-angle (cond (start-angle (float start-angle 0f0))
		     (end-angle 0f0)
		     (t nil))
  :end-angle (cond (end-angle (float end-angle 0f0))
		   (start-angle (float (* 2 pi) 0f0))
		   (t nil)))

(define-constructor make-ellipse* standard-ellipse
  (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	    &key start-angle end-angle)
  :center-x center-x :center-y center-y
  :radius-1-dx (coordinate radius-1-dx) :radius-1-dy (coordinate radius-1-dy)
  :radius-2-dx (coordinate radius-2-dx) :radius-2-dy (coordinate radius-2-dy)
  :start-angle (cond (start-angle (float start-angle 0f0))
		     (end-angle 0f0)
		     (t nil))
  :end-angle (cond (end-angle (float end-angle 0f0))
		   (start-angle (float (* 2 pi) 0f0))
		   (t nil)))

(defmethod make-load-form ((ellipse standard-ellipse))
  (with-slots (center-point radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			    start-angle end-angle) ellipse
    `(make-ellipse ',center-point
		   ,radius-1-dx ,radius-1-dy ,radius-2-dx ,radius-2-dy
		   ,@(when start-angle `(:start-angle ,start-angle))
		   ,@(when end-angle `(:end-angle ,end-angle)))))

(defmethod transform-region (transformation (ellipse standard-ellipse))
  (with-slots (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	       start-angle end-angle) ellipse
    (multiple-value-bind (cx cy)
	(transform-position transformation center-x center-y)
      (multiple-value-bind (r1-dx r1-dy)
	  (transform-distance transformation radius-1-dx radius-1-dy)
	(multiple-value-bind (r2-dx r2-dy)
	    (transform-distance transformation radius-2-dx radius-2-dy)
	  (make-ellipse* cx cy r1-dx r1-dy r2-dx r2-dy
			 ;;--- How to transform start and end angles?
			 :start-angle start-angle :end-angle end-angle))))))

(defmethod bounding-rectangle* ((ellipse standard-ellipse))
  (with-slots (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	       start-angle end-angle) ellipse
    (elliptical-arc-box center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			start-angle end-angle nil)))
