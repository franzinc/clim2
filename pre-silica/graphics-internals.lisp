;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defmacro with-half-thickness ((lthickness rthickness) line-style &body body)
  (let ((thickness '#:thickness))
    `(let* ((,thickness  (if ,line-style (line-style-thickness ,line-style) 0))
	    (,lthickness (floor ,thickness 2))
	    (,rthickness (- ,thickness ,lthickness)))
       ,@body)))

;; Note that points and paths themselves have no thickness.  The stroking
;; model CLIM uses is that, when a path is stroked, the pen is centered on
;; the path, so that the thickness of the stroke is divided on both sides
;; of the path.  For even-width pens, the extra ink appears on the left
;; (or below) the path.
(define-graphics-internal draw-point-internal (x y ink line-style)
  :points-to-convert (x y)
  :bounding-rectangle
    (with-half-thickness (lthickness rthickness) line-style
      (fix-rectangle (- x lthickness) (- y lthickness)
		     (+ x rthickness) (+ y rthickness))))

(defun make-design-from-output-record (record)
  (multiple-value-bind (xoff yoff) (compute-output-record-offsets record)
    (make-design-from-output-record-1 record xoff yoff)))

(defmethod make-design-from-output-record-1
	   ((record output-record-mixin) x-offset y-offset)
  (let ((designs nil))
    (flet ((make-design (record)
	     (multiple-value-bind (xoff yoff) (output-record-position record)
	       (declare (type coordinate xoff yoff))
	       (let ((design
		       (make-design-from-output-record-1
			 record
			 (+ x-offset xoff) (+ y-offset yoff))))
		 (when design (push design designs))))))
      (declare (dynamic-extent #'make-design))
      (map-over-output-records #'make-design record))
    (make-instance 'composite-over :designs (apply #'vector designs))))
			
(defmethod make-design-from-output-record-1
	   ((point point-displayed-output-record) x-offset y-offset)
  (with-slots (x y ink) point
    (compose-in
      ink
      (make-point (+ x x-offset) (+ y y-offset)))))

(defgeneric draw-design (design stream &rest args)
  (declare (arglist design stream &key . #.(all-drawing-options-lambda-list nil))))

(defmethod draw-design ((point standard-point) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
	   (ignore ink line-style))
  (multiple-value-bind (x y) (point-position point)
    (apply #'draw-point* stream x y args)))


(define-graphics-internal draw-line-internal (x1 y1 x2 y2 ink line-style)
  :points-to-convert (x1 y1 x2 y2)
  :bounding-rectangle
    (with-half-thickness (lthickness rthickness) line-style
      (fix-rectangle (- (min x1 x2) lthickness) (- (min y1 y2) lthickness)
		     (+ (max x1 x2) rthickness) (+ (max y1 y2) rthickness)))
  :highlighting-test
    ((x y)
     (with-slots (x1 y1 x2 y2 line-style) record
       (point-close-to-line-p x y x1 y1 x2 y2 (line-style-thickness line-style))))
  :highlighting-function
    ((stream state)
     (declare (ignore state))					;for now.
     (multiple-value-bind (xoff yoff)
	 (convert-from-relative-to-absolute-coordinates
	   stream (output-record-parent record))
       (with-slots (x1 y1 x2 y2 line-style) record
	 (outline-line-with-hexagon stream xoff yoff
				    x1 y1 x2 y2 (line-style-thickness line-style))))))

(defmethod make-design-from-output-record-1
	   ((line line-displayed-output-record) x-offset y-offset)
  (with-slots (x1 x2 y1 y2 ink) line
    (compose-in
      ink
      (make-line* (+ x1 x-offset) (+ y1 y-offset)
		  (+ x2 x-offset) (+ y2 y-offset)))))

(defmethod draw-design ((line standard-line) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
	   (ignore ink line-style))
  (multiple-value-bind (x1 y1) (line-start-point* line)
    (multiple-value-bind (x2 y2) (line-end-point* line)
      (apply #'draw-line* stream x1 y1 x2 y2 args))))

(defun outline-line-with-hexagon (stream xoff yoff
				  from-x from-y to-x to-y &optional (thickness 1))
  (let ((distance (1+ (round thickness 2))))
    (multiple-value-bind (x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6)
	(cond ((eq (minusp (- to-x from-x)) (minusp (- to-y from-y)))
	       (values (- from-x distance) (- from-y distance)
		       (- from-x distance) (+ from-y distance)
		       (- to-x distance) (+ to-y distance)
		       (+ to-x distance) (+ to-y distance)
		       (+ to-x distance) (- to-y distance)
		       (+ from-x distance) (- from-y distance)))
	      (t
	       (when (> to-y from-y)
		 ;; Make line go down to right.
		 (rotatef to-x from-x)
		 (rotatef to-y from-y))
	       (values (- from-x distance) (+ from-y distance)
		       (- from-x distance) (- from-y distance)
		       (- to-x distance) (- to-y distance)
		       (+ to-x distance) (- to-y distance)
		       (+ to-x distance) (+ to-y distance)
		       (+ from-x distance) (+ from-y distance))))
      (macrolet ((line (x1 y1 x2 y2)
		   `(draw-line-internal stream xoff yoff
					,x1 ,y1 ,x2 ,y2
					+flipping-ink+ +highlighting-line-style+)))
	(with-output-recording-options (stream :record nil)
	  (line x1 y1 x2 y2)
	  (line x2 y2 x3 y3)
	  (line x3 y3 x4 y4)
	  (line x4 y4 x5 y5)
	  (line x5 y5 x6 y6)
	  (line x6 y6 x1 y1))))))


(define-graphics-internal draw-rectangle-internal (x1 y1 x2 y2 ink line-style)
  :points-to-convert (x1 y1 x2 y2)
  :bounding-rectangle
    (with-half-thickness (lthickness rthickness) line-style
      (fix-rectangle (- x1 lthickness) (- y1 lthickness)
		     (+ x2 rthickness) (+ y2 rthickness)))
  :highlighting-test
    ((x y)
     (with-slots (x1 y1 x2 y2 line-style) record
       (or (null line-style)
	   (with-half-thickness (lthickness rthickness) line-style
	     (not (and (<= (+ x1 rthickness) x)
		       (<= (+ y1 rthickness) y)
		       (>= (- x2 lthickness) x)
		       (>= (- y2 lthickness) y)))))))
  :highlighting-function
    ((stream state)
     (declare (ignore state))
     (multiple-value-bind (xoff yoff)
	 (convert-from-relative-to-absolute-coordinates
	   stream (output-record-parent record))
       (with-slots (x1 y1 x2 y2 line-style) record
	 (with-output-recording-options (stream :record nil)
	   (with-half-thickness (lthickness rthickness) line-style
	     (draw-rectangle-internal
	       stream xoff yoff
	       (- x1 lthickness 1) (- y1 lthickness 1)
	       (+ x2 rthickness 1) (+ y2 rthickness 1)
	       +flipping-ink+ +highlighting-line-style+)))))))

(defmethod make-design-from-output-record-1
	   ((rectangle rectangle-displayed-output-record) x-offset y-offset)
  (with-slots (x1 x2 y1 y2 line-style ink) rectangle
    (compose-in
      ink
      (if (null line-style)
	  (make-rectangle* (+ x1 x-offset) (+ y1 y-offset)
			   (+ x2 x-offset) (+ y2 y-offset))
          (make-polyline* (list (+ x1 x-offset) (+ y1 y-offset)
				(+ x2 x-offset) (+ y1 y-offset)
				(+ x2 x-offset) (+ y2 y-offset)
				(+ x1 x-offset) (+ y2 y-offset))
			  :closed t)))))

(defmethod draw-design ((rectangle standard-rectangle) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
	   (ignore ink line-style))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rectangle)
    (apply #'draw-rectangle* stream x1 y1 x2 y2 :filled t args)))


;;--- This needs a :HIGHLIGHTING-TEST and :HIGHLIGHTING-FUNCTION
(define-graphics-internal draw-polygon-internal (list-of-x-and-ys closed ink line-style)
  :point-sequence-to-convert list-of-x-and-ys
  :bounding-rectangle
    (let* ((min-x (first list-of-x-and-ys))
	   (min-y (second list-of-x-and-ys))
	   (max-x min-x)
	   (max-y min-y))
      (dorest (position list-of-x-and-ys cddr)
	(minf min-x (first position))
	(minf min-y (second position))
	(maxf max-x (first position))
	(maxf max-y (second position)))
      (with-half-thickness (lthickness rthickness) line-style
	(fix-rectangle (- min-x lthickness) (- min-y lthickness)
		       (+ max-x rthickness) (+ max-y rthickness)))))

(defmethod make-design-from-output-record-1
	   ((polygon polygon-displayed-output-record) x-offset y-offset)
  (with-slots (list-of-xs-and-ys closed line-style ink) polygon
    (let ((coords (copy-list list-of-xs-and-ys)))
      (translate-position-sequence x-offset y-offset coords)
      (compose-in
	ink
	(if (null line-style)
	    (make-polygon* coords)
            (make-polyline* coords :closed closed))))))

(defmethod draw-design ((polygon standard-polygon) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
	   (ignore ink line-style))
  (apply #'draw-polygon* stream (coerce (slot-value polygon 'clim-utils::coords) 'list)
			 :closed t :filled t args))

(defmethod draw-design ((polyline standard-polyline) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
	   (ignore ink line-style))
  (apply #'draw-polygon* stream (coerce (slot-value polyline 'clim-utils::coords) 'list)
			 :closed (polyline-closed polyline) :filled nil args))


;; By the time we get here, START-ANGLE and END-ANGLE are either both NIL
;; or are both an angle measured in radians.
(define-graphics-internal draw-ellipse-internal
			  (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			   start-angle end-angle ink line-style)
  :points-to-convert (center-x center-y)
  :bounding-rectangle
    (multiple-value-bind (left top right bottom)
	(elliptical-arc-box center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			    start-angle end-angle
			    (and line-style (line-style-thickness line-style)))
      ;;--- Make this a bit too big because most hosts rasterize
      ;;--- ellipses to be a shade too big on the right
      (values left top (1+ right) (1+ bottom)))
  :highlighting-test
    ((x y)
     (with-slots (center-x center-y
		  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
		  start-angle end-angle ink line-style) record
       (and (or (null start-angle)
		;; NYI - check for within the proper angle
		t)
	    (if (null line-style)
		(point-inside-ellipse-p (- x center-x) (- y center-y)
					radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
		(point-on-thick-ellipse-p (- x center-x) (- y center-y)
					  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
					  (ceiling (line-style-thickness line-style) 2))))))
  :highlighting-function
    ((stream state)
     (declare (ignore state))
     (multiple-value-bind (xoff yoff)
	 (convert-from-relative-to-absolute-coordinates
	   stream (output-record-parent record))
       (with-slots (center-x center-y
		    radius-1-dx radius-1-dy radius-2-dx radius-2-dy
		    start-angle end-angle ink line-style) record
	 (let ((delta 2)
	       (radius-1 (sqrt (+ (* radius-1-dx radius-1-dx) (* radius-1-dy radius-1-dy))))
	       (radius-2 (sqrt (+ (* radius-2-dx radius-2-dx) (* radius-2-dy radius-2-dy)))))
	   (when line-style
	     (incf delta (ceiling (line-style-thickness line-style) 2)))
	   (let ((delta-1-dx (round (* delta radius-1-dx) radius-1))
		 (delta-1-dy (round (* delta radius-1-dy) radius-1))
		 (delta-2-dx (round (* delta radius-2-dx) radius-2))
		 (delta-2-dy (round (* delta radius-2-dy) radius-2)))
	     (with-output-recording-options (stream :record nil)
	       (draw-ellipse-internal
		 stream xoff yoff
		 center-x center-y
		 (+ radius-1-dx delta-1-dx) (+ radius-1-dy delta-1-dy)
		 (+ radius-2-dx delta-2-dx) (+ radius-2-dy delta-2-dy)
		 start-angle end-angle
		 +flipping-ink+ +highlighting-line-style+))))))))

(defmethod make-design-from-output-record-1
	   ((ellipse ellipse-displayed-output-record) x-offset y-offset)
  (with-slots (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	       start-angle end-angle line-style ink) ellipse
    (compose-in
      ink
      (if (null line-style)
	  (make-ellipse* (+ center-x x-offset) (+ center-y y-offset)
			 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			 :start-angle start-angle :end-angle end-angle)
          (make-elliptical-arc* (+ center-x x-offset) (+ center-y y-offset)
				radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				:start-angle start-angle :end-angle end-angle)))))

(defmethod draw-design ((ellipse standard-ellipse) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
	   (ignore ink line-style))
  (multiple-value-bind (center-x center-y)
      (ellipse-center-point* ellipse)
    (multiple-value-bind (radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
	(ellipse-radii ellipse)
      (apply #'draw-ellipse* stream center-x center-y
			     radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			     :start-angle (ellipse-start-angle ellipse)
			     :end-angle (ellipse-end-angle ellipse)
			     :filled t args))))

(defmethod draw-design ((ellipse standard-elliptical-arc) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
	   (ignore ink line-style))
  (multiple-value-bind (center-x center-y)
      (ellipse-center-point* ellipse)
    (multiple-value-bind (radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
	(ellipse-radii ellipse)
      (apply #'draw-ellipse* stream center-x center-y
			     radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			     :start-angle (ellipse-start-angle ellipse)
			     :end-angle (ellipse-end-angle ellipse)
			     :filled nil args))))


(define-graphics-internal draw-string-internal
			  (string x y start end align-x align-y text-style ink)
  :points-to-convert (x y)
  ;; Don't want temporary strings to make it into the history
  :output-recording-hook (setq string (evacuate-temporary-string string))
  :bounding-rectangle
    ;;--- How are we allowed to call STREAM-STRING-WIDTH if this isn't
    ;;--- an extended output stream??
    (let ((width (stream-string-width stream string :text-style text-style))
	  (height (stream-line-height stream text-style))
	  vx vt vr vb)
      (declare (type coordinate width height))
      (ecase align-x
	(:left (setq vx x
		     vr (+ x width)))
	(:right (setq vx (- x width)
		      vr x))
	(:center (setq vx (- x (round width 2))
		       vr (+ x (round width 2)))))
      (ecase align-y
	;;--- Using STREAM-LINE-HEIGHT for baseline isn't right.
	(:baseline (setq vt (- y height)
			 vb y))
	(:top (setq vt y
		    vb (+ y height)))
	(:bottom (setq vt (- y height)
		       vb y))
	;;--- Use FLOOR and CEILING, no?
	(:center (setq vt (- y (round height 2))
		       vb (+ y (round height 2)))))
      (fix-rectangle vx vt vr vb)))

(define-graphics-internal draw-character-internal
			  (character x y align-x align-y text-style ink)
  :points-to-convert (x y)
  ;;--- This has to deal with align-x and -y
  :bounding-rectangle 
    ;;--- How are we allowed to call STREAM-CHARACTER-WIDTH if this isn't
    ;;--- an extended output stream?
    (multiple-value-bind (width height)
	(stream-character-size stream character text-style)
      (declare (type coordinate width height))
      (let (vx vt vr vb)
	(ecase align-x
	  (:left (setq vx x
		       vr (+ x width)))
	  (:right (setq vx (- x width)
			vr x))
	  (:center (setq vx (- x (round width 2))
			 vr (+ x (round width 2)))))
	(ecase align-y
	  (:baseline (setq vt (- y height)
			   vb y))
	  (:top (setq vt y
		      vb (+ y height)))
	  (:bottom (setq vt (- y height)
			 vb y))
	  ;;--- Use FLOOR and CEILING, no?
	  (:center (setq vt (- y (round height 2))
			 vb (+ y (round height 2)))))
	(fix-rectangle vx vt vr vb))))


;;; Simple rendering of composite designs

(defmethod draw-design ((composite composite-over) stream &rest args)
  (declare (dynamic-extent args))
  (with-slots ((designs clim-utils::designs)) composite
    (dovector (design designs :from-end t)
      (apply #'draw-design design stream args))))

(defmethod draw-design ((composite composite-in) stream &rest args &key ink &allow-other-keys)
  (declare (dynamic-extent args))
  (with-slots ((designs clim-utils::designs)) composite
    (let ((ink (or ink (aref designs 0)))	;should be COMPOSE-OVER
	  (design (aref designs 1)))
      ;; Clips INK to the inside of DESIGN.
      (apply #'draw-design design stream :ink ink args))))

(defmethod draw-design ((composite composite-out) stream &rest args &key ink &allow-other-keys)
  (declare (dynamic-extent args))
  (with-slots ((designs clim-utils::designs)) composite
    (let ((ink (or ink (aref designs 0)))	;should be COMPOSE-OVER
	  (design (aref designs 1)))
      ;;--- Should clip INK to the outside of DESIGN, but I don't know how
      (nyi))))

(defmethod draw-design ((region standard-region-union) stream &rest args)
  (declare (dynamic-extent args))
  (with-slots ((regions clim-utils::regions)) region
    (dolist (region regions)
      (apply #'draw-design region stream args))))

(defmethod draw-design ((region standard-region-intersection) stream &rest args)
  (declare (dynamic-extent args))
  (with-slots ((regions clim-utils::regions)) region
    ;;--- Should draw just the intersection, but I dunno how to do that in general
    (nyi)))

(defmethod draw-design ((region standard-region-difference) stream &rest args)
  (declare (dynamic-extent args))
  (with-slots ((region1 clim-utils::region1)
	       (region2 clim-utils::region2)) region
    ;;--- Should draw just the difference, but I dunno how to do that in general
    (nyi)))
