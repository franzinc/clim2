;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


;;; Designs meet output records

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
  #-(or aclpc acl86win32) (declare (arglist design stream &key . #.(all-drawing-options-lambda-list nil))))


;;; Simple composite designs
(defmethod draw-design ((composite composite-over) stream &rest args)
  (declare (dynamic-extent args))
  (with-slots ((designs clim-utils::designs)) composite
    (dovector (design designs :from-end t)
      (apply #'draw-design design stream args))))

(defmethod draw-design ((composite composite-in) stream &rest args &key ink &allow-other-keys)
  (declare (dynamic-extent args))
  (with-slots ((designs clim-utils::designs)) composite
    (let ((ink (or ink (aref designs 0)))        ;should be COMPOSE-OVER
          (design (aref designs 1)))
      ;; Clips INK to the inside of DESIGN.
      (apply #'draw-design design stream :ink ink args))))

(defun nyi ()
  (error "This internal CLIM operation is NYI (Not Yet Implemented)."))

(defmethod draw-design ((composite composite-out) stream &rest args &key ink &allow-other-keys)
  (declare (dynamic-extent args))
  (with-slots ((designs clim-utils::designs)) composite
    (let ((ink (or ink (aref designs 0)))        ;should be COMPOSE-OVER
          (design (aref designs 1)))
      ;;--- Should clip INK to the outside of DESIGN, but I don't know how
      design ink args stream
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
    args stream 
    (nyi)))

(defmethod draw-design ((region standard-region-difference) stream &rest args)
  (declare (dynamic-extent args))
  (with-slots ((region1 clim-utils::region1)
               (region2 clim-utils::region2)) region
    ;;--- Should draw just the difference, but I dunno how to do that
    ;;in general
    args stream
    (nyi)))


;;; Simple geometric designs 

(defmethod make-design-from-output-record-1
           ((point point-output-record) x-offset y-offset)
  (with-slots (x y ink) point
    (compose-in
      ink
      (make-point (+ x x-offset) (+ y y-offset)))))

(defmethod make-design-from-output-record-1
           ((points points-output-record) x-offset y-offset)
  (with-slots (position-seq ink) points
    (compose-in
      ink
      (apply #'make-region-union
             (let ((points nil))
               (map-position-sequence
                 #'(lambda (x y)
                     (push (make-point (+ x x-offset) (+ y y-offset)) points))
                 position-seq)
               (nreverse points))))))

(defmethod draw-design ((point standard-point) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
           (ignore ink line-style))
  (multiple-value-bind (x y) (point-position point)
    (apply #'draw-point* stream x y args)))


(defmethod make-design-from-output-record-1
           ((line line-output-record) x-offset y-offset)
  (with-slots (x1 x2 y1 y2 ink) line
    (compose-in
      ink
      (make-line* (+ x1 x-offset) (+ y1 y-offset)
                  (+ x2 x-offset) (+ y2 y-offset)))))

(defmethod make-design-from-output-record-1
           ((lines lines-output-record) x-offset y-offset)
  (with-slots (position-seq ink) lines
    (compose-in
      ink
      (apply #'make-region-union
             (let ((lines nil))
               (map-endpoint-sequence
                 #'(lambda (x1 y1 x2 y2)
                     (push (make-line* (+ x1 x-offset) (+ y1 y-offset)
                                       (+ x2 x-offset) (+ y2 y-offset)) lines))
                 position-seq)
               (nreverse lines))))))

(defmethod draw-design ((line standard-line) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
           (ignore ink line-style))
  (multiple-value-bind (x1 y1) (line-start-point* line)
    (multiple-value-bind (x2 y2) (line-end-point* line)
      (apply #'draw-line* stream x1 y1 x2 y2 args))))


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

(defmethod make-design-from-output-record-1
           ((rectangles rectangles-output-record) x-offset y-offset)
  (with-slots (position-seq ink) rectangles
    (compose-in
      ink
      (apply #'make-region-union
             (let ((rectangles nil))
               (map-endpoint-sequence
                 #'(lambda (left top right bottom)
                     (push (make-rectangle* (+ left x-offset) (+ top y-offset)
                                            (+ right x-offset) (+ bottom y-offset))
                           rectangles))
                 position-seq)
               (nreverse rectangles))))))

(defmethod draw-design ((rectangle standard-rectangle) stream &rest args &key ink line-style)
  (declare (dynamic-extent args)
           (ignore ink line-style))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rectangle)
    (apply #'draw-rectangle* stream x1 y1 x2 y2 :filled t args)))


(defmethod make-design-from-output-record-1
           ((polygon polygon-output-record) x-offset y-offset)
  (with-slots (position-seq closed line-style ink) polygon
    (translate-position-sequence x-offset y-offset position-seq)
    (compose-in
      ink
      (if (null line-style)
          (make-polygon* position-seq)
          (make-polyline* position-seq :closed closed)))))

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

