;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 International Lisp Associates."

(defmacro define-graphics-recording (name medium-components 
                                     &key bounding-rectangle
                                          refined-position-test
                                          highlighting-function
                                          recording-hook
                                     &environment env)
  (destructuring-bind (function-args
                       &key positions-to-transform position-sequences-to-transform
                            optional-positions-to-transform distances-to-transform
                       &allow-other-keys)
      (get-drawing-function-description name)
    ;; Gross me right out!
    (setq function-args
          (mapcar #'(lambda (x) (intern (symbol-name x) *package*))
                  function-args))
    (setq positions-to-transform
          (mapcar #'(lambda (x) (intern (symbol-name x) *package*))
                  positions-to-transform))
    (setq position-sequences-to-transform
          (mapcar #'(lambda (x) (intern (symbol-name x) *package*))
                  position-sequences-to-transform))
    (setq optional-positions-to-transform
          (mapcar #'(lambda (x) (intern (symbol-name x) *package*))
                  optional-positions-to-transform))
    (setq distances-to-transform
          (mapcar #'(lambda (x) (intern (symbol-name x) *package*))
                  distances-to-transform))
    (let* ((class (fintern "~A-~A" 
			   (remove-word-from-string "DRAW-" name)
			   'output-record))
           (constructor (fintern "~A-~A" 'make class))
           (medium-graphics-function* (fintern "~A~A*"
					       'medium- name))
           (superclasses '(output-record-element-mixin 
                           graphics-displayed-output-record))
           (slots
             (remove 'filled (append medium-components function-args)))
           (slot-descs 
             (mapcar #'(lambda (x)
                         (let ((keyword (intern (symbol-name x) *keyword-package*)))
                           (if (eq keyword :ink)
                               `(,x :initarg ,keyword :accessor displayed-output-record-ink)
                               `(,x :initarg ,keyword))))
                     slots)))
      `(progn
         (defclass ,class ,superclasses ,slot-descs)
         (define-constructor-using-prototype-instance ,constructor ,class ,slots
           ,@(mapcar #'(lambda (arg) 
                         `(,arg ,(intern (symbol-name arg) *keyword-package*) ,arg)) 
                     slots))
         (defmethod ,medium-graphics-function* :around
                    ((stream output-recording-mixin) ,@function-args)
           #+Genera (declare (sys:function-parent ,name define-graphics-recording))
           ,recording-hook                ;call the recording hook first
           (when (stream-recording-p stream)
             ;; It's safe to call SHEET-MEDIUM because output recording
             ;; streams always have a medium
             (let* ((medium (sheet-medium stream))
                    (transformation (medium-transformation medium))
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
                   (point-position (stream-output-history-position stream))
                 (declare (type coordinate abs-x abs-y))
                 ;; Be sure to cons new point sequences, since they
                 ;; get stored in the output record
                 ,@(mapcar #'(lambda (p)
                               `(setq ,p (transform-position-sequence
                                           transformation ,p t)))
                           position-sequences-to-transform)
                 ,@(do ((pts positions-to-transform (cddr pts))
                        (r nil))
                       ((null pts)
                        (nreverse r))
                     (push 
                       (if (member (first pts) optional-positions-to-transform)
                           `(when ,(first pts)
                              (transform-positions transformation
                                ,(first pts) ,(second pts)))
                           `(transform-positions transformation
                              ,(first pts) ,(second pts)))
                       r))
                 (transform-distances transformation ,@distances-to-transform)
                 (let ((record (,constructor ,@slots)))
                   (multiple-value-bind (lf tp rt bt)
                       (progn ,bounding-rectangle)
                     (declare (type coordinate lf tp rt bt))
                     (bounding-rectangle-set-edges record
                       (- lf abs-x) (- tp abs-y) (- rt abs-x) (- bt abs-y))
                     (multiple-value-bind (cx cy) (stream-cursor-position stream)
                       (declare (type coordinate cx cy))
                       ;; Doing this directly beats calling
                       ;; OUTPUT-RECORD-SET-START-CURSOR-POSITION
                       (with-slots (start-x start-y) record
                         (setq start-x (- cx abs-x)
                               start-y (- cy abs-y))))
                     ;; Adjust the stored coordinates by the current cursor position
                     ,@(mapcar #'(lambda (p)
                                   `(with-slots (,p) record
                                      (setf ,p (adjust-position-sequence ,p abs-x abs-y))))
                               position-sequences-to-transform)
                     ,@(when positions-to-transform
                         `((with-slots ,positions-to-transform record
                             ,@(do ((p positions-to-transform (cddr p))
                                    r)
                                   ((null p) (nreverse r))
                                 (let ((b `(setf ,(first p)  (- ,(first p) abs-x)
                                                 ,(second p) (- ,(second p) abs-y))))
                                   (push
                                     (if (member (car p) optional-positions-to-transform)
                                         `(when ,(car p) ,b)
                                         b) 
                                     r)))))))
                   (stream-add-output-record stream record)))))
           (when (stream-drawing-p stream)
             (call-next-method)))

         (defmethod replay-output-record ((record ,class) stream 
                                          &optional region 
                                                    (x-offset (coordinate 0)) 
                                                    (y-offset (coordinate 0)))
           (declare (type coordinate x-offset y-offset))
           #-aclpc (declare (ignore region))
           #+Genera (declare (sys:function-parent ,name define-graphics-recording))
           (with-slots (,@slots) record
             ;; We can call SHEET-MEDIUM because stream will be an output
             ;; recording stream or an encapsulating stream
             (let ((medium (sheet-medium stream)))
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
                                   positions-to-transform)
                         ,@(mapcar #'(lambda (p) (list p p))
                                   position-sequences-to-transform))
                     ,@(mapcar #'(lambda (p)
                                   `(setq ,p (adjust-position-sequence 
                                               ,p (- x-offset) (- y-offset))))
                               position-sequences-to-transform)
                     ,@(do ((p positions-to-transform (cddr p))
                            r)
                           ((null p) (nreverse r))
                         (let ((b `(setf ,(first p) (+ ,(first p) x-offset)
                                         ,(second p) (+ ,(second p) y-offset))))
                           (push
                             (if (member (car p) optional-positions-to-transform)
                                 `(when ,(car p) ,b)
                                 b)
                             r)))
                     (,medium-graphics-function* medium ,@function-args)))))))
         
         ,@(when refined-position-test
             (let ((args (first refined-position-test))
                   (body (rest refined-position-test)))
               (multiple-value-bind (doc-string declarations body)
                   (extract-declarations body env)
                 (declare (ignore doc-string))
                 `((defmethod output-record-refined-position-test ((record ,class) ,@args)
                     ,@declarations
                     #+Genera (declare (sys:function-parent ,name define-graphics-recording))
                     (block refined-position-test
                       ,@body))))))

         ,@(when highlighting-function
             (let ((args (first highlighting-function))
                   (body (rest highlighting-function)))
               (multiple-value-bind (doc-string declarations body)
                   (extract-declarations body env)
                 (declare (ignore doc-string))
                 `((defmethod highlight-output-record ((record ,class) ,@args)
                     ,@declarations
                     #+Genera (declare (sys:function-parent ,name define-graphics-recording))
                     (block highlighting-function
                       ,@body))))))))))

(defmacro with-half-thickness ((lthickness rthickness) line-style &body body)
  (let ((ls '#:line-style))
    `(let* ((,ls ,line-style))
       (with-half-thickness-1 (,lthickness ,rthickness) (if ,ls (line-style-thickness ,ls) 0)
         ,@body))))

(defmacro with-half-thickness-1 ((lthickness rthickness) thickness &body body)
  (let ((thicky '#:thicky))
    `(let* ((,thicky ,thickness)
            (,lthickness (floor ,thicky 2))
            (,rthickness (- ,thicky ,lthickness)))
       ,@body)))

(define-graphics-recording draw-point (ink line-style clipping-region)
  :bounding-rectangle 
  (with-half-thickness (lthickness rthickness) line-style
      (values (- x lthickness)
              (- y lthickness)
              (+ x rthickness)
              (+ y rthickness))))

(define-graphics-recording draw-points (ink line-style clipping-region)
  :bounding-rectangle 
    (position-sequence-bounding-rectangle 
      position-seq line-style)
  :refined-position-test
    ((x y)
     (let ((position-seq (slot-value record 'position-seq))
           (line-style (slot-value record 'line-style)))
       (with-half-thickness (lthickness rthickness) line-style
         (map-position-sequence
           #'(lambda (px py)
               (when (ltrb-contains-position-p (- px lthickness) (- py lthickness)
                                               (+ px rthickness) (+ py rthickness)
                                               x y)
                 (return-from refined-position-test t)))
           position-seq))))
  :highlighting-function
    ((stream state)
     (declare (ignore state))                        ;for now.
     (multiple-value-bind (xoff yoff)
         (convert-from-relative-to-absolute-coordinates
           stream (output-record-parent record))
       (let ((position-seq (slot-value record 'position-seq))
             (line-style (slot-value record 'line-style)))
         (with-half-thickness (lthickness rthickness) line-style
           (map-position-sequence
             #'(lambda (px py)
                 (draw-rectangle* stream
                                  (+ (- px lthickness) xoff) (+ (- py lthickness) yoff)
                                  (+ (+ px rthickness) xoff) (+ (+ py rthickness) yoff)
                                  :ink +flipping-ink+
                                  :line-style +highlighting-line-style+))
             position-seq))))))


(define-graphics-recording draw-line (ink line-style clipping-region)
  :bounding-rectangle 
    (with-half-thickness (lthickness rthickness) line-style
      (values (- (min x1 x2) lthickness)
              (- (min y1 y2) lthickness)
              (+ (max x1 x2) rthickness)
              (+ (max y1 y2) rthickness)))
  :refined-position-test
    ((x y)
     (with-slots (x1 y1 x2 y2 line-style) record
       (point-close-to-line-p x y x1 y1 x2 y2 (line-style-thickness line-style))))
  :highlighting-function
    ((stream state)
     (declare (ignore state))                                        ;for now.
     (multiple-value-bind (xoff yoff)
         (convert-from-relative-to-absolute-coordinates
           stream (output-record-parent record))
       (with-slots (x1 y1 x2 y2 line-style) record
         (outline-line-with-hexagon stream xoff yoff
                                    x1 y1 x2 y2 (line-style-thickness line-style))))))

(define-graphics-recording draw-lines (ink line-style clipping-region)
  :bounding-rectangle 
    (position-sequence-bounding-rectangle 
      position-seq line-style)
  :refined-position-test
    ((x y)
     (let* ((position-seq (slot-value record 'position-seq))
            (line-style (slot-value record 'line-style))
            (thickness (line-style-thickness line-style)))
       (with-half-thickness (lthickness rthickness) line-style
         (map-endpoint-sequence
          #'(lambda (x1 y1 x2 y2)
              (when (point-close-to-line-p x y x1 y1 x2 y2 thickness)
                (when (> x1 x2) (rotatef x1 x2))
                (when (> y1 y2) (rotatef y1 y2))
                (when (ltrb-contains-position-p (- x1 lthickness)
                                                (- y1 lthickness)
                                                (+ x2 rthickness)
                                                (+ y2 rthickness)
                                                x y)
                (return-from refined-position-test t))))
          position-seq))))
  :highlighting-function
    ((stream state)
     (declare (ignore state))                        ;for now.
     (multiple-value-bind (xoff yoff)
         (convert-from-relative-to-absolute-coordinates
           stream (output-record-parent record))
       (let* ((position-seq (slot-value record 'position-seq))
              (line-style (slot-value record 'line-style))
              (thickness (line-style-thickness line-style)))
         (map-endpoint-sequence
           #'(lambda (x1 y1 x2 y2)
               (outline-line-with-hexagon stream xoff yoff
                                          x1 y1 x2 y2 thickness))
           position-seq)))))

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


(define-graphics-recording draw-rectangle (ink line-style clipping-region)
  :bounding-rectangle
    (with-half-thickness (lthickness rthickness) line-style
      (values (- (min x1 x2) lthickness)
              (- (min y1 y2) lthickness)
              (+ (max x1 x2) rthickness)
              (+ (max y1 y2) rthickness)))
  :recording-hook
    (unless (rectilinear-transformation-p (medium-transformation stream))
      ;; Not too inefficient, since we're about to create an output record anyway
      (with-stack-list (list x1 y1 x2 y1 x2 y2 x1 y2)
        (return-from medium-draw-rectangle*
          (medium-draw-polygon* stream list t filled))))
  :refined-position-test
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

(define-graphics-recording draw-rectangles (ink line-style clipping-region)
  :bounding-rectangle 
    (position-sequence-bounding-rectangle 
      position-seq line-style)
  :recording-hook
    (unless (rectilinear-transformation-p (medium-transformation stream))
      (medium-draw-transformed-rectangles* stream position-seq filled)
      (return-from medium-draw-rectangles* nil))
  :refined-position-test
    ((x y)
     (let ((position-seq (slot-value record 'position-seq))
           (line-style (slot-value record 'line-style)))
       (map-endpoint-sequence
         #'(lambda (x1 y1 x2 y2)
             (when (if (null line-style)
                       (ltrb-contains-position-p x1 y1 x2 y2 x y)
                       (with-half-thickness (lthickness rthickness) line-style
                         (not (and (<= (+ x1 rthickness) x)
                                   (<= (+ y1 rthickness) y)
                                   (>= (- x2 lthickness) x)
                                   (>= (- y2 lthickness) y)))))
               (return-from refined-position-test t)))
         position-seq)))
  :highlighting-function
    ((stream state)
     (declare (ignore state))                        ;for now.
     (multiple-value-bind (xoff yoff)
         (convert-from-relative-to-absolute-coordinates
           stream (output-record-parent record))
       (let ((position-seq (slot-value record 'position-seq))
             (line-style (slot-value record 'line-style)))
         (with-half-thickness (lthickness rthickness) line-style
           (map-endpoint-sequence
             #'(lambda (x1 y1 x2 y2)
                 (draw-rectangle-internal
                   stream xoff yoff
                   (- x1 lthickness 1) (- y1 lthickness 1)
                   (+ x2 rthickness 1) (+ y2 rthickness 1)
                   +flipping-ink+ +highlighting-line-style+))
             position-seq))))))


;;--- This needs both :REFINED-POSITION-TEST and :HIGHLIGHTING-FUNCTION
;;--- Note that POSITION-SEQ will be a vector in those methods
(define-graphics-recording draw-polygon (ink line-style clipping-region)
  :bounding-rectangle
    (position-sequence-bounding-rectangle 
      position-seq line-style))

(defun position-sequence-bounding-rectangle (position-seq line-style)
  (let* ((minx (elt position-seq 0))
         (miny (elt position-seq 1))
         (maxx minx)
         (maxy miny))
    (map-position-sequence
      #'(lambda (x y)
          (minf minx x)
          (minf miny y)
          (maxf maxx x)
          (maxf maxy y))
      position-seq)
    (with-half-thickness (lthickness rthickness) line-style
      (values (- minx lthickness)
              (- miny lthickness)
              (+ maxx rthickness)
              (+ maxy rthickness)))))

(defun adjust-position-sequence (position-seq dx dy)
  (if (and (zerop dx) (zerop dy))
      position-seq
      (let ((result (make-array (length position-seq)))
            (i 0))
        (declare (type simple-vector result))
        (map-position-sequence
          #'(lambda (x y)
              (setf (svref result i) (- x dx)
                    (svref result (1+ i)) (- y dy)
                    i (+ i 2)))
          position-seq)
        result)))


;; This revision of the ellipse functions fixes several problems: - smh 10Jan97
;;  - The bounding box and drawing calculations had different
;;    notions about the y-axis sign.  (The windows back end also needs to
;;    be revised to agree with this fix.)
;;  - angle-between-angles-p did exact comparisons, but computations
;;    in radians (involving pi) are necessarily approximate.
;; But there are still bugs:
;;  - X (and presumably also Windows) take start and end in skewed angles.
;;    CLIM defines these arguments in non-skewed form.  Thios might just be a bug
;;    in the CLIM standard, but anyway, no one has ever noticed.  Anyway, until
;;    this is resolved the start/end angle refined position test cannot be fixed.
;;    For ellipses spanning less than PI radians, the bounding box is sufficient
;;    and the start/end limit checks are redundant.  But for larger spans the
;;    bounding-box check is ineffective, and the entire 2PI span of the ellipse
;;    will be sensitive to input.

(define-graphics-recording draw-ellipse (ink line-style clipping-region)
  :bounding-rectangle
  (multiple-value-bind (left top right bottom)
      (elliptical-arc-box center-x center-y
			  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			  start-angle end-angle
			  (line-style-thickness (medium-line-style medium)))
    ;; Make this a bit too big because most hosts rasterize ellipses
    ;; to be a shade too big on the right
    (values left top (1+ right) (1+ bottom)))
  :refined-position-test
  ((x y)
   (with-slots (center-x center-y
			 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			 start-angle end-angle ink line-style) record
     ;; Added test for angle. -smh 26dec96
     (let (#+never (z nil))
       (and (if (null line-style)
		(point-inside-ellipse-p (- x center-x) (- y center-y)
					radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
	      (point-on-thick-ellipse-p (- x center-x) (- y center-y)
					radius-1-dx radius-1-dy radius-2-dx radius-2-dy
					(ceiling (line-style-thickness line-style) 2)))
	    ;; These start and end angle tests, although slow, are probably correct.
	    ;; Unfortunately, the drawing functions in both X and Windows neglect
	    ;; to take into accout that non-circular ellipses are define in terms of
	    ;; skewed angles.  See you favorite Xlib manual fdor details.
	    ;; - smh 30Dec96
	    #+never
	    (or (null start-angle)
		(progn (setq z (phase (complex (- x center-x) (- center-y y))))
		       (loop
			   while (<= z start-angle)
			   do (incf z 2pi))
		       t))
	    #+never
	    (or (null end-angle)
		(loop
		    initially (unless z
				(setq z (phase (complex (- x center-x) (- center-y y)))))
		    with start = (or start-angle 0)
		    while (<= z start)
		    do (incf z 2pi))
		#+never (format *trace-output* "~&[~f,~f] end ~f ~f~%"
				(- x center-x) (- y center-y) end-angle z)
		(<= z end-angle))))))
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


(define-graphics-recording draw-bezier-curve (ink line-style clipping-region)
  :bounding-rectangle
    (position-sequence-bounding-rectangle 
      position-seq line-style))


(define-graphics-recording draw-text (ink text-style clipping-region)
  :bounding-rectangle
    (medium-text-bounding-box medium string-or-char x y
                              start end align-x align-y
                              (medium-merged-text-style medium)
                              towards-x towards-y transform-glyphs transformation))

(defmethod medium-text-bounding-box ((sheet standard-sheet-output-mixin) string x y
                                     start end align-x align-y text-style
                                     towards-x towards-y transform-glyphs transformation)
  (medium-text-bounding-box (sheet-medium sheet) string x y
                            start end align-x align-y text-style
                            towards-x towards-y transform-glyphs transformation))

(defmethod medium-text-bounding-box ((medium basic-medium) string x y
                                     start end align-x align-y text-style
                                     towards-x towards-y transform-glyphs transformation)
  (declare (ignore transform-glyphs transformation))
  (let* ((width (stream-string-width (medium-sheet medium) string
                                      :start start :end end
                                      :text-style text-style))
         ;;-- Should we just call the string-output-size function here?
          (ascent (text-style-ascent text-style medium))
          (descent (text-style-descent text-style medium))
          (height (+ ascent descent))
          vx vt vr vb)
    (ecase align-x
      (:left (setq vx x
                   vr (+ x width)))
      (:right (setq vx (- x width) vr x)
              (when towards-x (decf towards-x width)))
      (:center (let ((w2 (round width 2)))
                 (setq vx (- x w2)
                       vr (+ x w2))
                 (when towards-x (decf towards-x w2)))))
    (ecase align-y
      (:baseline (setq vt (- y height)
                       vb (+ y descent)))
      (:top (setq vt y vb (+ y height))
            (when towards-y (incf towards-y ascent)))
      (:bottom (setq vt (- y height) vb y)
               (when towards-y (decf towards-y descent)))
      (:center (let ((h2 (floor height 2)))
                 (setq vt (- y h2)
                       vb (+ y (ceiling height 2)))
                 (when towards-y (decf towards-y h2)))))
    (values (coordinate vx) (coordinate vt) 
            (coordinate vr) (coordinate vb)
            (coordinate vx)
            (coordinate (+ vt ascent))
            towards-x
            towards-y)))
            

(define-graphics-recording draw-pixmap (ink clipping-region)
  :bounding-rectangle
  (values x y (+ x (pixmap-width pixmap)) (+ y (pixmap-height pixmap))))
