;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

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

(defmethod make-load-form ((line standard-line) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
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

;; spec says only coincidence satisfies region-intersects-region-p for lines
;; this is often more useful since it returns true if they touch *anywhere*
(defun segments-intersect-p (line1 line2)
  (with-slots ((sx1 start-x) (sy1 start-y) (ex1 end-x) (ey1 end-y)) line1
    (with-slots ((sx2 start-x) (sy2 start-y) (ex2 end-x) (ey2 end-y)) line2
      (let ((sx1 sx1) (sy1 sy1) (ex1 ex1) (ey1 ey1)
	    (sx2 sx2) (sy2 sy2) (ex2 ex2) (ey2 ey2))
	(declare (type coordinate sx1 sy1 ex1 ey1
		       sx2 sy2 ex2 ey2))
	;; do the lines' x-components and y-components even overlap?
	(and (>= (max sx2 ex2) (min sx1 ex1))
	     (>= (max sx1 ex1) (min sx2 ex2))
	     (>= (max sy2 ey2) (min sy1 ey1))
	     (>= (max sy1 ey1) (min sy2 ey2))
	     (let* ((dx1 (- ex1 sx1)) (dy1 (- ey1 sy1))
		    (dx2 (- ex2 sx2)) (dy2 (- ey2 sy2))
		    (dsx (- sx1 sx2)) (dsy (- sy1 sy2))
		    (slope-diff (- (* dx1 dy2) (* dx2 dy1))))
	       (if (= slope-diff 0)	; parallel
		   (= (* dsy dx1) (* dsx dy1)) ; coincident
		 (let ((r (/ (- (* dsy dx2) (* dsx dy2)) slope-diff))
		       (s (/ (- (* dsy dx1) (* dsx dy1)) slope-diff)))
		   (and (<= 0 r 1) (<= 0 s 1)))))))))) ; intersecting

;; as per the dimensionality rules in the spec (why?)
;; this only returns true if the segments coincide;
;; use segments-intersect-p above to find out if they cross
;; and point-of-intersection to get the p-o-i
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
	  (flet ((between-p (x1 y1 x2 y2 x y)
		   (and (or (<= x1 x x2) (<= x2 x x1))
			(or (<= y1 y y2) (<= y2 y y1)))))
	    (let ((sx nil) (sy nil) (ex nil) (ey nil))
	      ;;          A     B     C     D
	      ;; line1:  ___  _____  ____ ____
	      ;; line2: -----  ---  ----   ----
	      (if (between-p sx1 sy1 ex1 ey1 sx2 sy2)
		  ;; cases B/D
		  (setf sx sx2 sy sy2))
	      (if (between-p sx1 sy1 ex1 ey1 ex2 ey2)
		  ;; cases B/C
		  (if sx (setf ex ex2 ey ey2) ;; case B
		    (setf sx ex2 sy ey2))) ;; case C
	      (if (between-p sx2 sy2 ex2 ey2 sx1 sy1)
		  ;; cases A/C
		  (if sx (setf ex sx1 ey sy1) ;; case C
		    (setf sx sx1 sy sy1))) ;; case A
	      (if (between-p sx2 sy2 ex2 ey2 ex1 ey1)
		  ;; cases A/D
		  (setf ex ex1 ey ey1))
	      (make-line* sx sy ex ey)))))
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
		     (setf x (aref coords (incf i))) (setf y (aref coords (incf i))))
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

(defmethod make-load-form ((polyline standard-polyline) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
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

(defmethod make-load-form ((polygon standard-polygon) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
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

;;; from patch3519, various missing region methods on polygons, polylines, etc.
;;; note that these work in the hairy cases too, eg:
;;;         _         
;;; |\ /|  / \  |\  /|
;;; | X | |   | | \/ |
;;; |/ \|  \_/  |____|

(defmethod region-contains-position-p ((polyline standard-polyline) x y)
  (map-over-polygon-segments
   #'(lambda (x1 y1 x2 y2)
       (when (region-contains-position-p (make-line* x1 y1 x2 y2) x y)
	 (return-from region-contains-position-p t)))
   polyline))

(define-symmetric-region-method region-intersects-region-p ((polyline standard-polyline) (other-region region))
  (map-over-polygon-segments
   #'(lambda (x1 y1 x2 y2)
       (when (region-intersects-region-p (make-line* x1 y1 x2 y2) other-region)
	 (return-from region-intersects-region-p t)))
   polyline))

(defmethod region-contains-region-p ((polyline standard-polyline) (line standard-line))
  (let ((x nil) (y nil))
    (map-over-polygon-segments
     #'(lambda (x1 y1 x2 y2)
	 (let ((this-segment (make-line* x1 y1 x2 y2)))
	   (when (region-contains-region-p this-segment line)
	     (return-from region-contains-region-p t))
	   ;; also check for this case:  \__.__/
	   ;; imagine parallel lines are   ---    superimposed.
	   (unless (and x
			(let ((last-n-segments (make-line* x y x2 y2)))
			  ;; if the last few segments form a straight line...
			  (and (region-contains-region-p last-n-segments this-segment)
			       (if (region-contains-region-p last-n-segments line)
				   (return-from region-contains-region-p t)
				 ;; else-- no dice, but keep x and y
				 ;; because maybe there's an even longer
				 ;; straight line in the polyline
				 t))))
	     (setf x x1 y y1))))
     polyline)))

(defmethod region-contains-region-p ((polyline1 standard-polyline) (polyline2 standard-polyline))
  (map-over-polygon-segments
   #'(lambda (x1 y1 x2 y2)
       (unless (region-contains-region-p polyline1 (make-line* x1 y1 x2 y2))
	 (return-from region-contains-region-p nil)))
   polyline2)
  t)

(defmethod region-contains-position-p ((polygon polygon) x y)
  (with-bounding-rectangle* (left top right bottom) polygon
    (and (<= left x right)
	 (<= top y bottom)
	 (let ((line-to-infinity (make-line* x y (+ 10 right) (+ 10 bottom)))
	       (intersections 0))
	   (map-over-polygon-segments
	    #'(lambda (x1 y1 x2 y2)
		(if (region-contains-position-p line-to-infinity x1 y1)
		    (when (and (= x x1) (= y y1))
		      ;; position is a vertex of the polygon [if not,
		      ;; line-to-infinity crosses a vertex of the poly, but we
		      ;; still don't incf this time since the same intersection
		      ;; will have shown up in the previous segment and we don't
		      ;; want to count it twice]
		      (return-from region-contains-position-p t))
		  (let ((segment (make-line* x1 y1 x2 y2)))
		    (when (segments-intersect-p line-to-infinity segment)
		      (when (region-contains-position-p segment x y)
			;; position sits on a line of the polygon
			(return-from region-contains-position-p t))
		      (incf intersections)))))
	    polygon)
	   ;; position is bounded by polygon if line from position to infinity
	   ;; crosses polygon boundaries an odd number of times
	   (oddp intersections)))))

(defmethod region-intersects-region-p ((polygon1 polygon) (polygon2 polygon))
  (or (region-contains-region-p polygon1 polygon2)
      (region-contains-region-p polygon2 polygon1)
      (map-over-polygon-segments
       #'(lambda (sx sy ex ey)
	   (let ((segment-of-p1 (make-line* sx sy ex ey)))
	     (map-over-polygon-segments
	      #'(lambda (x1 y1 x2 y2)
		  (when (segments-intersect-p segment-of-p1
					      (make-line* x1 y1 x2 y2))
		    (return-from region-intersects-region-p t)))
	      polygon2)))
       polygon1)))

;; just-touches-p is useful in many situations when we know two things
;; intersect but would like to determine whether they just touch or
;; actually cross, ie the lines in T or V just touch but in X they cross.

;; default to nil
(defmethod just-touches-p ((region1 region) (region2 region)))

(defmethod just-touches-p ((line1 standard-line) (line2 standard-line))
  (with-slots ((sx1 start-x) (sy1 start-y) (ex1 end-x) (ey1 end-y)) line1
    (with-slots ((sx2 start-x) (sy2 start-y) (ex2 end-x) (ey2 end-y)) line2
      ;; if any segment endpoint is *on* the other segment, they don't cross
      (or (region-contains-position-p line2 sx1 sy1)
	  (region-contains-position-p line2 ex1 ey1)
	  (region-contains-position-p line1 sx2 sy2)
	  (region-contains-position-p line1 ex2 ey2)))))
			  
(define-symmetric-region-method just-touches-p ((polygon polygon) (line standard-line))
  (and (not (region-contains-region-p polygon line))
       ;; relies on map-over-polygon-segments returning nil on completion
       (not (map-over-polygon-segments
	     #'(lambda (x1 y1 x2 y2)
		 (let ((segment (make-line* x1 y1 x2 y2)))
		   (unless (or (not (segments-intersect-p segment line))
			       (just-touches-p segment line))
		     (return-from just-touches-p nil))))
	     polygon))))

(defmethod just-touches-p ((polygon1 polygon) (polygon2 polygon))
  (and (not (map-over-polygon-segments
	     #'(lambda (a b c d)
		 (let ((segment-of-1 (make-line* a b c d)))
		   (map-over-polygon-segments
		    #'(lambda (x1 y1 x2 y2)
			(let ((segment-of-2 (make-line* x1 y1 x2 y2)))
			  (unless (or (not (segments-intersect-p segment-of-1 segment-of-2))
				      (and (just-touches-p segment-of-1 polygon2)
					   (just-touches-p segment-of-2 polygon1)))
			    (return-from just-touches-p nil))))
		    polygon2)))
	     polygon1))))

;; does-not-overlap-p might seem redundant but is a distinction which needs
;; to be made in the case where a line meets an region on the border of a
;; region-difference.

(defmethod does-not-overlap-p ((region1 region) (region2 region))
  (just-touches-p region1 region2))

(defmethod does-not-overlap-p ((line1 standard-line) (line2 standard-line))
  (not (region-intersects-region-p line1 line2)))

;; if poly first, just-touches-p is called
(defmethod does-not-overlap-p ((line standard-line) (poly polygon))
  (not (region-intersects-region-p line poly)))


(defmethod region-contains-region-p ((polygon polygon) (line standard-line))
  (with-slots ((sx start-x) (sy start-y) (ex end-x) (ey end-y)) line
    (declare (type coordinate sx sy ex ey))
    (and (region-contains-position-p polygon sx sy)
	 (region-contains-position-p polygon ex ey)
	 (not (map-over-polygon-segments
	       #'(lambda (x1 y1 x2 y2)
		   (let ((segment (make-line* x1 y1 x2 y2)))
		     (when (segments-intersect-p line segment)
		       (unless (and (just-touches-p line segment)
				    ;; make sure the midpoint is bounded--
				    ;; remember, polygons can be concave
				    (region-contains-position-p
				     polygon
				     (+ sx (/ (- ex sx) 2))
				     (+ sy (/ (- ey sy) 2))))
			 ;; line crosses out
			 (return-from region-contains-region-p nil)))))
	       polygon)))))

(defmethod region-contains-region-p ((polygon1 polygon) (polygon2 polygon))
  (map-over-polygon-segments
   #'(lambda (x1 y1 x2 y2)
       (unless (region-contains-region-p polygon1 (make-line* x1 y1 x2 y2))
	 (return-from region-contains-region-p nil)))
   polygon2)
  t)

(defmethod region-equal ((polygon1 polygon) (polygon2 polygon))
  ;; inefficient?
  (and (region-contains-region-p polygon1 polygon2)
       (region-contains-region-p polygon2 polygon1)))

(define-symmetric-region-method region-intersects-region-p ((polygon polygon) (line standard-line))
  (or (region-contains-region-p polygon line)
      (map-over-polygon-segments
       #'(lambda (x1 y1 x2 y2)
	   (when (segments-intersect-p line (make-line* x1 y1 x2 y2))
	     (return-from region-intersects-region-p t)))
       polygon)))

;; previously missing region-set methods -- region-set is essentially equivalent to
;; region-union since there are no specialized region-union methods

(defmethod region-intersects-region-p ((set1 region-set) (set2 region-set))
  (map-over-region-set-regions
   #'(lambda (region-of-set1)
       (map-over-region-set-regions
	#'(lambda (region-of-set2)
	    (when (region-intersects-region-p region-of-set1 region-of-set2)
	      (return-from region-intersects-region-p t)))
	set2))
   set1)
  nil)

(defmethod region-equal ((set1 region-set) (set2 region-set))
  ;; inefficient?  bah.  consider making this the primary method.
  (and (region-contains-region-p set1 set2)
       (region-contains-region-p set2 set1)))

(define-symmetric-region-method region-intersects-region-p ((set region-set) (other-region region))
  (map-over-region-set-regions
   #'(lambda (set-region)
       (when (region-intersects-region-p set-region other-region)
	 (return-from region-intersects-region-p t)))
   set))

(defun point-of-intersection (sx1 sy1 ex1 ey1 sx2 sy2 ex2 ey2)
  ;; do the lines' x-components and y-components even overlap?
  (when (and (>= (max sx2 ex2) (min sx1 ex1))
	     (>= (max sx1 ex1) (min sx2 ex2))
	     (>= (max sy2 ey2) (min sy1 ey1))
	     (>= (max sy1 ey1) (min sy2 ey2)))
    (let* ((dx1 (- ex1 sx1)) (dy1 (- ey1 sy1))
	   (dx2 (- ex2 sx2)) (dy2 (- ey2 sy2))
	   (dsx (- sx1 sx2)) (dsy (- sy1 sy2))
	   (slope-diff (- (* dx1 dy2) (* dx2 dy1))))
      (unless (= slope-diff 0)		; parallel or coincident
	(let ((r (/ (- (* dsy dx2) (* dsx dy2)) slope-diff))
	      (s (/ (- (* dsy dx1) (* dsx dy1)) slope-diff)))
	  (when (and (<= 0 r 1) (<= 0 s 1)) ; intersecting
	    (values (+ sx1 (* r (- ex1 sx1))) ; xy of intersection
		    (+ sy1 (* r (- ey1 sy1))))))))))

(defmethod break-line-across-poly ((line standard-line) (poly polygon))
  (with-slots ((sx start-x) (sy start-y) (ex end-x) (ey end-y)) line
    (let ((poi nil))
      (map-over-polygon-segments
       #'(lambda (x1 y1 x2 y2)
	   (multiple-value-bind (poi-x poi-y)
	       (point-of-intersection x1 y1 x2 y2 sx sy ex ey)
	     (when poi-x
	       ;; index on distance from start point
	       (let ((d (+ (abs (- sx poi-x))
			   (abs (- sy poi-y)))))
		 (push (list d poi-x poi-y) poi)))))
       poly)
      (let ((sorted-poi (apply #'append (mapcar #'cdr (sort poi #'< :key #'car)))))
	(make-polyline* (append (list sx sy) sorted-poi (list ex ey)))))))

(defmethod break-line-across-poly ((line1 standard-line) (line2 standard-line))
  (with-slots ((sx1 start-x) (sy1 start-y) (ex1 end-x) (ey1 end-y)) line1
    (with-slots ((sx2 start-x) (sy2 start-y) (ex2 end-x) (ey2 end-y)) line2
      (flet ((between (x y)
	       (if (and (or (<= sx1 x ex1) (<= ex1 x sx1))
			(or (<= sy1 y ey1) (<= ey1 y sy1)))
		   (+ (abs (- sx1 x)) (abs (- sy1 y))))))
	(let* ((ds (between sx2 sy2))
	       (de (between ex2 ey2))
	       (mid (if (and ds de)
			(if (> de ds)
			    (list sx2 sy2 ex2 ey2)
			  (list ex2 ey2 sx2 sy2))
		      (if ds (list sx2 sy2)
			(if de (list ex2 ey2))))))
	  (make-polyline* (append (list sx1 sy1) mid (list ex1 ey1))))))))
			    
(defmethod region-contains-region-p ((region-set region-set) (line standard-line))
  (let ((possibly-uncontained-parts (list line)))
    (map-over-region-set-regions
     #'(lambda (this-region)
	 (let ((still-uncontained nil))
	   (dolist (smaller-part possibly-uncontained-parts)
	     (unless (region-contains-region-p this-region smaller-part)
	       (if (region-intersects-region-p smaller-part this-region)
		   (let ((broken-line (break-line-across-poly line this-region)))
		     (map-over-polygon-segments
		      #'(lambda (x1 y1 x2 y2)
			  (let ((this-part (make-line* x1 y1 x2 y2)))
			    (unless (region-contains-region-p this-region this-part)
			      (push this-part still-uncontained))))
		      broken-line))
		 ;; else
		 (push smaller-part still-uncontained))))
	   (setf possibly-uncontained-parts
	     (or still-uncontained
		 (return-from region-contains-region-p t)))))
     region-set)))

(defmethod region-contains-region-p ((other-region region) (region-set region-set))
  (map-over-region-set-regions
   #'(lambda (this-region)
       (unless (region-contains-region-p other-region this-region)
	 (return-from region-contains-region-p nil)))
   region-set)
  t)

(defmethod region-contains-region-p ((region-set region-set) (polygon polygon))
  (map-over-polygon-segments
   #'(lambda (x1 y1 x2 y2)
       (unless (region-contains-region-p region-set (make-line* x1 y1 x2 y2))
	 (return-from region-contains-region-p nil)))
   polygon)
  t)


;; standard-region-intersection comparisons

(defmethod region-contains-position-p ((set standard-region-intersection) x y)
  (map-over-region-set-regions
   #'(lambda (region)
       (unless (region-contains-position-p region x y)
	 (return-from region-contains-position-p nil)))
   set)
  t)

;; this fails on the following case: it gives a false positive on an
;; other-region which *does* intersect all regions in the set but not at
;; their intersection eg the region-intersection of any two regions below
;; with the third.
;;
;; poly A     poly B 
;;  +--+       +--+
;;  |   \     /   |
;;--+----\---/----+-- line C
;;  |     \ /     |
;;  |      X      |
;;  |     / \     |
;;  +----/---+    |
;;      /         |
;;     +----------+
;; but not to worry, we catch most of these cases with the specializations
;; on lines/polys below
(define-symmetric-region-method region-intersects-region-p ((set standard-region-intersection) (other-region region))
  (map-over-region-set-regions
   #'(lambda (set-region)
       (unless (region-intersects-region-p set-region other-region)
	 (return-from region-intersects-region-p nil)))
   set)
  t)

(define-symmetric-region-method region-intersects-region-p ((set standard-region-intersection) (line standard-line))
  (let ((possibly-intersecting-parts (list line)))
    (map-over-region-set-regions
     #'(lambda (this-region)
	 (let ((intersects-so-far nil))
	   (dolist (this-part possibly-intersecting-parts)
	     (when (and (region-intersects-region-p this-region this-part)
			(not (just-touches-p this-region this-part)))
	       (let ((broken-line (break-line-across-poly line this-region)))
		 (map-over-polygon-segments
		  #'(lambda (x1 y1 x2 y2)
		      (let ((this-subsegment (make-line* x1 y1 x2 y2)))
			(when (region-contains-region-p this-region this-subsegment)
			  (push this-subsegment intersects-so-far))))
		  broken-line))))
	   (setf possibly-intersecting-parts
	     (or intersects-so-far
		 (return-from region-intersects-region-p nil)))))
       set))
  t)

(define-symmetric-region-method region-intersects-region-p ((set standard-region-intersection) (poly polygon))
  (or (region-contains-region-p poly set)
      (map-over-polygon-segments
       #'(lambda (x1 y1 x2 y2)
	   (when (region-intersects-region-p set (make-line* x1 y1 x2 y2))
	     (return-from region-intersects-region-p t)))
       poly)))

(defmethod region-contains-region-p ((set standard-region-intersection) (other-region region))
  (map-over-region-set-regions
   #'(lambda (set-region)
       (unless (region-contains-region-p set-region other-region)
	 (return-from region-contains-region-p nil)))
   set)
  t)

;; this fails too often - works for union but not intersection... but we
;; catch many such cases with poly specialization below
(defmethod region-contains-region-p ((other-region region) (set standard-region-intersection))
  (map-over-region-set-regions
   #'(lambda (set-region)
       (unless (region-contains-region-p other-region set-region)
	 (return-from region-contains-region-p nil)))
   set)
  t)

;; what a headache... this checks through each side of each region in the
;; set against all other regions in the set and hacks each side down into
;; the part (if any) which is actually *in* all of them (ie bounding or
;; inside the intersection).  As it goes it checks to make sure each such
;; segment is contained by the poly in question.
(defmethod region-contains-region-p ((poly polygon) (set standard-region-intersection))
  (let ((current-region 0))
    (map-over-region-set-regions
     #'(lambda (this-region)
	 ;; should check if polygon first
	 (map-over-polygon-segments
	  #'(lambda (x1 y1 x2 y2)
	      (let ((parts-of-this-side (list (make-line* x1 y1 x2 y2)))
		    (counter 0))
		(dolist (that-region (region-set-regions set))
		  (unless parts-of-this-side (return))
		  (let ((smaller-parts nil))
		    (unless (= counter current-region)
		      (dolist (this-segment parts-of-this-side)
			(if (region-intersects-region-p this-segment that-region)
			    (let ((broken-line (break-line-across-poly this-segment that-region)))
			      (map-over-polygon-segments
			       #'(lambda (sx sy ex ey)
				   (let ((subseg (make-line* sx sy ex ey)))
				     (if (region-contains-region-p
					  that-region subseg)
					 (push subseg smaller-parts))))
			       broken-line))))
		      (setf parts-of-this-side smaller-parts)))
		  (incf counter))
		(dolist (segment-bounding-intersection parts-of-this-side)
		  (unless (region-contains-region-p poly segment-bounding-intersection)
		    (return-from region-contains-region-p nil)))))
	  this-region)
	 (incf current-region))
     set))
  t)


;; standard-region-difference comparisons

(defmethod region-contains-position-p ((diff standard-region-difference) x y)
  (with-slots (region1 region2) diff
    (and (region-contains-position-p region1 x y)
	 (not (region-contains-position-p region2 x y)))))

(defmethod region-contains-region-p ((diff standard-region-difference) (other-region region))
  (with-slots (region1 region2) diff
    (and (region-contains-region-p region1 other-region)
	 (or (not (region-intersects-region-p region2 other-region))
	     ;; okay to touch?
	     (does-not-overlap-p region2 other-region)))))

(defmethod region-contains-region-p ((other-region region) (diff standard-region-difference))
  (with-slots (region1 region2) diff
    (or (region-contains-region-p other-region region1)
	(map-over-polygon-segments
	 #'(lambda (x1 y1 x2 y2)
	     (let ((this-side (make-line* x1 y1 x2 y2)))
	       (if (and (region-intersects-region-p this-side region2)
			(not (does-not-overlap-p this-side region2)))
		   ;; what if region2 not poly?
		   (let ((broken-line (break-line-across-poly this-side region2)))
		     (map-over-polygon-segments
		      #'(lambda (sx sy ex ey)
			  (let ((subseg (make-line* sx sy ex ey)))
			    (unless (or (region-contains-region-p region2 subseg)
					(region-contains-region-p other-region subseg))
			      (return-from region-contains-region-p nil))))
		      broken-line))
		 (unless (region-contains-region-p other-region this-side)
		   (return-from region-contains-region-p nil)))))
	 region1)
	t)))

(define-symmetric-region-method region-intersects-region-p ((diff standard-region-difference) (other-region region))
  (with-slots (region1 region2) diff
    (and (region-intersects-region-p region1 other-region)
	 (or (not (region-intersects-region-p region2 other-region))
	     (just-touches-p region2 other-region)))))

(define-symmetric-region-method region-intersects-region-p ((diff standard-region-difference) (poly polygon))
  (or (region-contains-region-p poly diff)
      (region-contains-region-p diff poly)
      (with-slots (region1 region2) diff
	(and (region-intersects-region-p region1 poly)
	     (or (not (region-intersects-region-p region2 poly))
		 (and (region-contains-region-p poly region1)
		      (not (region-contains-region-p region2 region1)))
		 (does-not-overlap-p region2 poly)
		 (map-over-polygon-segments
		  #'(lambda (x1 y1 x2 y2)
		      (when (region-intersects-region-p diff (make-line* x1 y1 x2 y2))
			(return-from region-intersects-region-p t)))
		  poly))))))

(define-symmetric-region-method region-intersects-region-p ((diff standard-region-difference) (line standard-line))
  (with-slots (region1 region2) diff
    (and (region-intersects-region-p line region1)
	 (or (not (region-intersects-region-p line region2))
	     (does-not-overlap-p region2 line)
	     (and (not (region-contains-region-p region2 line))
		  (let ((broken-line (break-line-across-poly line region2)))
		    (map-over-polygon-segments
		     #'(lambda (x1 y1 x2 y2)
			 (let ((this-subsegment (make-line* x1 y1 x2 y2)))
			   (when (and (region-intersects-region-p this-subsegment region1)
				      (not (region-contains-region-p region2 this-subsegment)))
			     (return-from region-intersects-region-p t))))
		     broken-line)))))))
	

;; line-poly-intersection assumes region-intersects-region-p is true.
;; produces line(s) clipped to stay inside poly.
;; we don't use this yet-- but spec says region-intersection and
;; region-difference of paths and areas (eg lines and polys) should produce
;; clipped lines... how to do this?
#+ignore-for-now
(defun line-poly-intersection (line poly)
  (let ((segments nil)
	(broken-line (break-line-across-poly line poly)))
    (map-over-polygon-segments
     #'(lambda (x1 y1 x2 y2)
	 (let ((this-segment (make-line* x1 y1 x2 y2)))
	   (when (region-contains-region-p poly this-segment)
	     (push this-segment segments))))
     broken-line)
    (apply #'make-region-union segments)))


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

(defmethod make-load-form ((ellipse standard-elliptical-arc) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
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

(defmethod make-load-form ((ellipse standard-ellipse) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
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


;;; +++pr move this to protocol.lsp ???
#+(or aclpc acl86win32)
(defmethod make-load-form ((des design) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  '(make-instance 'design))
 

