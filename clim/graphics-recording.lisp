;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: graphics-recording.lisp,v 1.3 92/04/14 15:29:53 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

(defmacro define-output-recorder (class name medium-components 
				  &key bounding-rectangle
				       highlighting-test
				       highlighting-function)
  (destructuring-bind (function-args
		       &key points-to-transform point-sequences-to-transform
			    distances-to-transform
		       &allow-other-keys)
      (get-drawing-function-description name)
    ;; Gross me right out!
    (setq function-args
	  (mapcar #'(lambda (x) (intern (symbol-name x) *package*))
		  function-args))
    (setq points-to-transform
	  (mapcar #'(lambda (x) (intern (symbol-name x) *package*))
		  points-to-transform))
    (setq point-sequences-to-transform
	  (mapcar #'(lambda (x) (intern (symbol-name x) *package*))
		  point-sequences-to-transform))
    (setq distances-to-transform
	  (mapcar #'(lambda (x) (intern (symbol-name x) *package*))
		  distances-to-transform))
    (let* ((medium-graphics-function*
	     (intern (format nil "~A~A*" 'medium- name)))
	   (superclasses '(output-record-element-mixin 
			   graphics-displayed-output-record))
	   (slots
	     (remove 'filled (append medium-components function-args)))
	   (slot-descs 
	     (mapcar #'(lambda (x)
			 (list x :initarg (intern (symbol-name x) *keyword-package*)))
			 slots)))
      `(progn
	 (defclass ,class ,superclasses ,slot-descs)
	 ;;--- Need to define a speedy constructor
	 (defmethod ,medium-graphics-function* :around
		    ((medium output-recording-mixin) ,@function-args)
	   (when (stream-recording-p medium)
	     (let ((transformation (medium-transformation medium))
		   ,@(mapcar #'(lambda (medium-component)
					  (list medium-component
						`(,(fintern"~A~A" 'medium- medium-component)
						  medium)))
			     medium-components))
	       ;; Overload FILLED and LINE-STYLE -- when FILLED is T,
	       ;; the LINE-STYLE is ignored and must be NIL
	       ,@(when (and (member 'filled function-args)
			    (member 'line-style medium-components))
		   `((when filled (setq line-style nil))))
	       (multiple-value-bind (abs-x abs-y)
		   (point-position (stream-output-history-position medium))
		 (declare (type coordinate abs-x abs-y))
		 ,@(mapcar #'(lambda (p)
			       `(transform-position-sequence (transformation) ,p))
			   point-sequences-to-transform)
		 (transform-positions (transformation) ,@points-to-transform)
		 (transform-distances (transformation) ,@distances-to-transform)
		 (let ((record
			 (make-instance ',class
			   ,@(mapcan #'(lambda (x)
					 (list (intern (symbol-name x) *keyword-package*) x))
				     slots))))
		   (multiple-value-bind (lf tp rt bt)
		       (progn ,bounding-rectangle)
		     (declare (type coordinate lf tp rt bt))
		     (bounding-rectangle-set-edges
		       record
		       (- lf abs-x) (- tp abs-y) (- rt abs-x) (- bt abs-y))
		     (multiple-value-bind (cx cy) (stream-cursor-position medium)
		       (declare (type coordinate cx cy))
		       ;; Doing this directly beats calling
		       ;; OUTPUT-RECORD-SET-START-CURSOR-POSITION
		       (with-slots (start-x start-y) record
			 (setq start-x (- cx abs-x)
			       start-y (- cy abs-y))))
		     ;; Adjust the stored coordinates by the current cursor position
		     ,@(mapcar #'(lambda (p)
				   `(with-slots (,p) record
				      (setf ,p (adjust-point-sequence ,p abs-x abs-y))))
			       point-sequences-to-transform)
		     ,@(when points-to-transform
			 `((with-slots ,points-to-transform record
			     (setf ,@(do ((p points-to-transform (cddr p))
					  r)
					 ((null p) (nreverse r))
				       (push (first p) r)
				       (push `(- ,(first p) abs-x) r)
				       (push (second p) r)
				       (push `(- ,(second p) abs-y) r)))))))
		   (stream-add-output-record medium record)))))
	   (when (stream-drawing-p medium)
	     (call-next-method)))

	 (defmethod replay-output-record ((record ,class) stream 
					  &optional region (x-offset 0) (y-offset 0))
	   (declare (ignore region))
	   (with-slots (,@slots) record
	     (with-sheet-medium (medium stream)
	       (letf-globally (((medium-transformation medium) +identity-transformation+))
	         (with-drawing-options 
		     (medium ,@(mapcan #'(lambda (medium-component)
					   (list (intern (symbol-name medium-component)
							 *keyword-package*)
						 medium-component))
				       medium-components))
		   (let (,@(when (and (member 'filled function-args)
				      (member 'line-style medium-components))
			     `((filled (not line-style))))
			 ,@(mapcar #'(lambda (p) (list p p))
				   points-to-transform)
			 ,@(mapcar #'(lambda (p) (list p p))
				   point-sequences-to-transform))
		     ,@(mapcar #'(lambda (p)
				   `(setq ,p (adjust-point-sequence 
					       ,p (- x-offset) (- y-offset))))
			       point-sequences-to-transform)
		     (setf ,@(do ((p points-to-transform (cddr p))
				  r)
				 ((null p) (nreverse r))
			       (push (first p) r)
			       (push `(+ ,(first p) x-offset) r)
			       (push (second p) r)
			       (push `(+ ,(second p) y-offset) r)))
		     (,medium-graphics-function* medium ,@function-args)))))))
	 
	 ,@(when highlighting-test
	     (let ((args (first highlighting-test))
		   (body (rest highlighting-test)))
	       `((defmethod output-record-refined-sensitivity-test ((record ,class) ,@args)
		   ,@body))))

	 ,@(when highlighting-function
	     (let ((args (first highlighting-function))
		   (body (rest highlighting-function)))
	       `((defmethod highlight-output-record-1 ((record ,class) ,@args)
		   ,@body))))))))

(defmacro with-half-thickness ((lthickness rthickness) line-style &body body)
  (let ((ls '#:line-style)
	(thickness '#:thickness))
    `(let* ((,ls ,line-style)
	    (,thickness  (if ,ls (line-style-thickness ,ls) 0))
	    (,lthickness (floor ,thickness 2))
	    (,rthickness (- ,thickness ,lthickness)))
       ,@body)))


;;; Designs

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

(defgeneric draw-design (design stream &rest args)
  (declare (arglist design stream &key . #.(all-drawing-options-lambda-list nil))))


;;; Simple composite designs
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


(define-output-recorder point-output-record draw-point (ink line-style)
  :bounding-rectangle 
    (with-half-thickness (lthickness rthickness) line-style
      (values (- x lthickness)
	      (- y lthickness)
	      (+ x rthickness)
	      (+ y rthickness))))

(defmethod make-design-from-output-record-1
	   ((point point-output-record) x-offset y-offset)
  (with-slots (x y ink) point
    (compose-in
      ink
      (make-point (+ x x-offset) (+ y y-offset)))))

(defmethod draw-design ((point standard-point) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
	   (ignore ink line-style))
  (multiple-value-bind (x y) (point-position point)
    (apply #'draw-point* stream x y args)))


(define-output-recorder line-output-record draw-line (ink line-style)
  :bounding-rectangle 
    (with-half-thickness (lthickness rthickness) line-style
      (values (- (min x1 x2) lthickness)
	      (- (min y1 y2) lthickness)
	      (+ (max x1 x2) rthickness)
	      (+ (max y1 y2) rthickness)))
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
	   ((line line-output-record) x-offset y-offset)
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


(define-output-recorder rectangle-output-record draw-rectangle (ink line-style)
  :bounding-rectangle
    (with-half-thickness (lthickness rthickness) line-style
      (values (- (min x1 x2) lthickness)
	      (- (min y1 y2) lthickness)
	      (+ (max x1 x2) rthickness)
	      (+ (max y1 y2) rthickness)))
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
	   ((rectangle rectangle-output-record) x-offset y-offset)
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
(define-output-recorder polygon-output-record draw-polygon (ink line-style)
  :bounding-rectangle
    (point-sequence-bounding-rectangle 
      list-of-x-and-ys line-style))

(defmethod make-design-from-output-record-1
	   ((polygon polygon-output-record) x-offset y-offset)
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
  (apply #'draw-polygon stream (polygon-points polygon) :closed t :filled t args))

(defmethod draw-design ((polyline standard-polyline) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
	   (ignore ink line-style))
  (with-slots (closed) polyline
    (apply #'draw-polygon stream (polygon-points polyline) :closed closed :filled nil args)))

(defun point-sequence-bounding-rectangle (list-of-x-and-ys line-style)
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
    (with-half-thickness (lthickness rthickness) line-style
      (values (- minx lthickness)
	      (- miny lthickness)
	      (+ maxx rthickness)
	      (+ maxy rthickness)))))

(defun map-point-sequence (fn list-of-x-and-ys)
  (do ((p list-of-x-and-ys (cddr p)))
      ((null p))
    (funcall fn (car p) (cadr p))))

(defun adjust-point-sequence (list-of-x-and-ys dx dy)
  (let (r)
    (map-point-sequence
     #'(lambda (x y)
	 (push (- x dx) r)
	 (push (- y dy) r))
     list-of-x-and-ys)
    (nreverse r)))


(define-output-recorder ellipse-output-record draw-ellipse (ink line-style)
  :bounding-rectangle
    (multiple-value-bind (left top right bottom)
	(elliptical-arc-box
	  center-x center-y 
	  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	  start-angle end-angle
	  (line-style-thickness (medium-line-style medium)))
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
	   ((ellipse ellipse-output-record) x-offset y-offset)
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


;;--- Where is DRAW-TEXT?  and/or DRAW-STRING and DRAW-CHARACTER?

(define-output-recorder text-output-record
    draw-text (text-style ink)
    :bounding-rectangle
    (text-bounding-box medium string-or-char x y start end align-x
			       align-y text-style))

(defun text-bounding-box (stream string x y start end align-x
				  align-y text-style)
  (let ((width (stream-string-width stream string
				    :start start
				    :end end
				    :text-style text-style))
	;;-- The height really depends on the text in the string
	;;rather than just the text-style
	(height (stream-line-height stream text-style))
	vx vt vr vb)
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
    (values vx vt vr vb)))

