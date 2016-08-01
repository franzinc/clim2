;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; General region union

(defclass standard-region-union (region-set area)
    ((regions :type list :initarg :regions)))

(define-constructor make-region-union-1 standard-region-union (regions)
  :regions regions)

(defun make-region-union (&rest regions)
  (declare (dynamic-extent regions))
  (make-region-union-1 (copy-list regions)))

(defmethod region-set-function ((region standard-region-union)) 'union)

(defmethod region-set-regions ((region standard-region-union) &key normalize)
  (declare (ignore normalize))
  (slot-value region 'regions))

(defmethod transform-region (transformation (region-set standard-region-union))
  (let ((regions nil))
    (flet ((transform (region)
             (push (transform-region transformation region) regions)))
      (declare (dynamic-extent #'transform))
      (map-over-region-set-regions #'transform region-set))
    (make-region-union-1 (nreverse regions))))

(define-symmetric-region-method region-union ((region region) (nowhere nowhere)) region)
(define-symmetric-region-method region-union ((everywhere everywhere) (region region)) +everywhere+)

;; Take the region of maximum dimensionality
(define-symmetric-region-method region-union ((point point) (path path)) path)
(define-symmetric-region-method region-union ((point point) (area area)) area)
(define-symmetric-region-method region-union ((path path) (area area)) area)

(defmethod region-union ((point1 point) (point2 point))
  (if (region-equal point1 point2)
      point1
      (make-region-union point1 point2)))

(defmethod region-union ((path1 path) (path2 path))
  (cond ((region-contains-region-p path1 path2) path1)
        ((region-contains-region-p path2 path1) path2)
        (t (make-region-union path1 path2))))

(defmethod region-union ((area1 area) (area2 area))
  (cond ((region-contains-region-p area1 area2) area1)
        ((region-contains-region-p area2 area1) area2)
        (t (make-region-union area1 area2))))

(defmethod region-union ((region1 region) (region2 region))
  (make-region-union region1 region2))

(define-symmetric-region-method region-union ((region region) (union standard-region-union))
  (apply #'make-region-union region (slot-value union 'regions)))

(defmethod region-union ((region1 standard-region-union) (region2 standard-region-union))
  (apply #'make-region-union (append (slot-value region1 'regions)
                                     (slot-value region2 'regions))))


;;; General region intersection

(defclass standard-region-intersection (region-set area)
    ((regions :type list :initarg :regions)))

(define-constructor make-region-intersection-1 standard-region-intersection (regions)
  :regions regions)

(defun make-region-intersection (&rest regions)
  (declare (dynamic-extent regions))
  (make-region-intersection-1 (copy-list regions)))

(defmethod region-set-function ((region standard-region-intersection)) 'intersection)

(defmethod region-set-regions ((region standard-region-intersection) &key normalize)
  (declare (ignore normalize))
  (slot-value region 'regions))

(defmethod transform-region (transformation (region-set standard-region-intersection))
  (let ((regions nil))
    (flet ((transform (region)
             (push (transform-region transformation region) regions)))
      (declare (dynamic-extent #'transform))
      (map-over-region-set-regions #'transform region-set))
    (make-region-intersection-1 (nreverse regions))))

(define-symmetric-region-method region-intersection ((region region) (nowhere nowhere)) +nowhere+)
(define-symmetric-region-method region-intersection ((everywhere everywhere) (region region)) region)

;; Take the region of minumum dimensionality
(define-symmetric-region-method region-intersection ((point point) (path path))
  (if (region-intersects-region-p point path) point +nowhere+))
(define-symmetric-region-method region-intersection ((point point) (area area))
  (if (region-intersects-region-p point area) point +nowhere+))
;; this looks fishy to me-- what if the path and area are like so:
;;      +-----+
;;      |   --+-----
;;      |     |
;;      +-----+   then the path is *not* the intersection!
;; and in fact the spec backs me up on this
#+ignore ;; tjm 17Mar97
(define-symmetric-region-method region-intersection ((path path) (area area))
  (if (region-intersects-region-p path area) path +nowhere+))

(defmethod region-intersection ((point1 point) (point2 point))
  (if (region-equal point1 point2) point1 +nowhere+))

;; This catches paths and areas, too
(defmethod region-intersection ((region1 region) (region2 region))
  (if (region-intersects-region-p region1 region2)
      (make-region-intersection region1 region2)
      +nowhere+))

(define-symmetric-region-method region-intersection ((region region) (intersection standard-region-intersection))
  (apply #'make-region-intersection region (slot-value intersection 'regions)))

(defmethod region-intersection ((region1 standard-region-intersection) (region2 standard-region-intersection))
  (apply #'make-region-intersection (append (slot-value region1 'regions)
                                            (slot-value region2 'regions))))


;;; General region difference

(defclass standard-region-difference (region-set area)
    ((region1 :type region :initarg :region1)
     (region2 :type region :initarg :region2)
     (regions :type list)))

(define-constructor make-region-difference standard-region-difference
                    (region1 region2)
  :region1 region1 :region2 region2)

(defmethod region-set-function ((region standard-region-difference)) 'set-difference)

(defmethod region-set-regions ((region standard-region-difference) &key normalize)
  (declare (ignore normalize))
  (slot-value region 'regions))

(defmethod map-over-region-set-regions
           (function (region standard-region-difference) &key normalize)
  (declare (dynamic-extent function) (ignore normalize))
  (with-slots (region1 region2) region
    (funcall function region1)
    (funcall function region2))
  nil)

(defmethod slot-unbound (class (region standard-region-difference) (slot (eql 'regions)))
  (declare (ignore class))
  (with-slots (regions region1 region2) region
    (setf regions (list region1 region2))))

(defmethod transform-region (transformation (region-set standard-region-difference))
  (with-slots (region1 region2) region-set
    (make-region-difference (transform-region transformation region1)
                            (transform-region transformation region2))))

(defmethod region-difference ((nowhere nowhere) (region region)) +nowhere+)
(defmethod region-difference ((region region) (nowhere nowhere)) region)
(defmethod region-difference ((region region) (everywhere everywhere)) +nowhere+)

;; For the case where the first region has higher dimensionality, the
;; first region is the result.
(defmethod region-difference ((path path) (point point)) path)
(defmethod region-difference ((area area) (point point)) area)
(defmethod region-difference ((area area) (path path)) area)

(defmethod region-difference ((region1 region) (region2 region))
  (if (region-intersects-region-p region1 region2)
      (make-region-difference region1 region2)
    region1))


;;; Simple rectangle (LTRB) arithmetic
;;; These operate only on COORDINATEs, so be careful!

(defun ltrb-well-formed-p (left top right bottom)
  (declare (type coordinate left top right bottom))
  ;;--- Should we really allow zero-sized LTRBs?
  (and (>= right left)
       (>= bottom top)))

(defun ltrb-equals-ltrb-p (left1 top1 right1 bottom1
                           left2 top2 right2 bottom2)
  (declare (type coordinate left1 top1 right1 bottom1
                             left2 top2 right2 bottom2))
  (and (= left1 left2)
       (= top1 top2)
       (= right1 right2)
       (= bottom1 bottom2)))

(defun ltrb-size-equal (left1 top1 right1 bottom1 
                        left2 top2 right2 bottom2)
  (declare (type coordinate left1 top1 right1 bottom1
                            left2 top2 right2 bottom2))
  (and (= (- right1 left1) (- right2 left2))
       (= (- bottom1 top1) (- bottom2 top2))))

(defun ltrb-contains-position-p (left top right bottom x y)
  (declare (type coordinate left top right bottom x y))
  (and (<= left x)
       (<= top y)
       (>= right x)
       (>= bottom y)))

(defun ltrb-contains-ltrb-p (left1 top1 right1 bottom1
                             left2 top2 right2 bottom2)
  (declare (type coordinate left1 top1 right1 bottom1
                            left2 top2 right2 bottom2))
  (and (<= left1 left2)
       (<= top1 top2)
       (>= right1 right2)
       (>= bottom1 bottom2)))

(defun ltrb-overlaps-ltrb-p (left1 top1 right1 bottom1
                             left2 top2 right2 bottom2)
  (declare (type coordinate left1 top1 right1 bottom1
                             left2 top2 right2 bottom2)
           (values valid-p left top right bottom))
  (let ((left (max left1 left2))
        (top (max top1 top2))
        (right (min right1 right2))
        (bottom (min bottom1 bottom2)))
    (when (ltrb-well-formed-p left top right bottom)
      (values t left top right bottom))))

;; Returns a list of bounding rectangles that represent the union
(defun ltrb-union (left1 top1 right1 bottom1
                   left2 top2 right2 bottom2 &optional (banding :x-banding))
  (declare (type coordinate left1 top1 right1 bottom1
                             left2 top2 right2 bottom2)
           (values rectangles))
  (cond ((ltrb-contains-ltrb-p left1 top1 right1 bottom1
                               left2 top2 right2 bottom2)
         (list (make-bounding-rectangle-1 left1 top1 right1 bottom1)))
        ((ltrb-contains-ltrb-p left2 top2 right2 bottom2
                               left1 top1 right1 bottom1)
         (list (make-bounding-rectangle-1 left2 top2 right2 bottom2)))
        ((not (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
                                    left2 top2 right2 bottom2))
         (list (make-bounding-rectangle-1 left1 top1 right1 bottom1)
               (make-bounding-rectangle-1 left2 top2 right2 bottom2)))
        (t
         (ecase banding
           (:x-banding
            (let ((result nil))
              ;; Three slices
              ;;; Top slice
              (cond ((< top1 top2)
                      (push (make-bounding-rectangle left1 top1 right1 top2) result))
                     ((< top2 top1)
                      (push (make-bounding-rectangle left2 top2 right2 top1) result)))
              ;; Bottom slice
              (cond ((< bottom1 bottom2)
                     (push (make-bounding-rectangle left2 bottom1 right2 bottom2) result))
                    ((< bottom2 bottom1)
                     (push (make-bounding-rectangle left1 bottom2 right1 bottom1) result)))
              ;; Middle slice
              (push (make-bounding-rectangle (min left1 left2)
                                             (max top1 top2)
                                             (max right1 right2)
                                             (min bottom1 bottom2))
                    result)
              result))
           
           (:y-banding
            (let ((result nil))
              ;; Three slices
              ;;; Left slice
              (cond ((< left1 left2)
                      (push (make-bounding-rectangle left1 top1 left2 bottom1) result))
                     ((< left2 left1)
                      (push (make-bounding-rectangle left2 top2 left1 bottom2) result)))
              ;; Right slice
              (cond ((< right1 right2)
                     (push (make-bounding-rectangle right1 top2 right2 bottom2) result))
                    ((< right2 right1)
                     (push (make-bounding-rectangle right2 top1 right1 bottom1) result)))
              ;; Middle slice
              (push (make-bounding-rectangle (max left1 left2)
                                             (min top1 top2)
                                             (min right1 right2)
                                             (max bottom1 bottom2))
                    result)
              result))))))

;; Returns a single bounding rectangle that represents the intersection, or NIL.
(defun ltrb-intersection (left1 top1 right1 bottom1
                          left2 top2 right2 bottom2)
  (declare (type coordinate left1 top1 right1 bottom1
                            left2 top2 right2 bottom2)
           (values rectangle))
  (multiple-value-bind (valid-p left top right bottom)
      (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
                            left2 top2 right2 bottom2)
    (when valid-p
      (make-bounding-rectangle-1 left top right bottom))))

;; Returns a list of bounding rectangles that represent the difference, or NIL.
;; Diagrams of rectangle differences:
;;
;;     111111111111111111
;;     1aaaaaaaaaaaaaaaa1
;;     1aaaaaaaaaaaaaaaa1
;;     1aaaaaaaaaaaaaaaa1
;;     1aaaaaaaaaaaaaaaa1
;;     1cccccc222222222232222222222
;;     1cccccc2         1         2
;;     1cccccc2         1         2
;;     1cccccc2         1         2
;;     111111131111111111         2
;;            2                   2
;;            2                   2
;;            222222222222222222222
;;
;;
;;     111111111111111111
;;     1aaaaaaaaaaaaaaaa1
;;     1aaaaaaaaaaaaaaaa1
;;     1aaaaaaaaaaaaaaaa1
;;     1aaaaaaaaaaaaaaaa1
;; 2222322222222222222dd1
;; 2   1             2dd1
;; 2   1             2dd1
;; 2   1             2dd1
;; 2   1             2dd1
;; 2   1             2dd1
;; 2   1             2dd1
;; 2222322222222222222dd1
;;     1bbbbbbbbbbbbbbbb1
;;     1bbbbbbbbbbbbbbbb1
;;     111111111111111111
(defun ltrb-difference (left1 top1 right1 bottom1
                        left2 top2 right2 bottom2)
  (declare (type coordinate left1 top1 right1 bottom1
                            left2 top2 right2 bottom2)
           (values rectangles))
  (unless (ltrb-contains-ltrb-p left2 top2 right2 bottom2
                                left1 top1 right1 bottom1)
    (if (not (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
                                   left2 top2 right2 bottom2))
        (list (make-bounding-rectangle-1 left1 top1 right1 bottom1))
        ;; If the second ltrb contains the first ltrb, the difference is NIL.
        (let ((result nil))
          (when (< top1 top2)                        ;Area A above
            (push (make-bounding-rectangle left1 top1 right1 top2) result))
          (when (> bottom1 bottom2)                ;Area B above
            (push (make-bounding-rectangle left1 bottom2 right1 bottom1) result))
          (when (< left1 left2)                        ;Area C above
            (let ((top (max top1 top2))
                  (bottom (min bottom1 bottom2)))
              (when (> bottom top)
                (push (make-bounding-rectangle left1 top left2 bottom) result))))
          (when (> right1 right2)                ;Area D above
            (let ((top (max top1 top2))
                  (bottom (min bottom1 bottom2)))
              (when (> bottom top)
                (push (make-bounding-rectangle right2 top right1 bottom) result))))
          result))))


;;; Special cases for bounding rectangles

(defclass standard-rectangle-set (region-set)
    ((left   :initarg :left :type coordinate)
     (top    :initarg :top  :type coordinate)
     (right  :initarg :right  :type coordinate)
     (bottom :initarg :bottom :type coordinate)
     (rectangles :type list :initarg :rectangles :reader rectangle-set-rectangles)
     (x-banded-rectangles :type list)
     (y-banded-rectangles :type list)))

(define-constructor make-rectangle-set-1 standard-rectangle-set
                    (rectangles left top right bottom)
  :rectangles rectangles
  :left left :top top :right right :bottom bottom)

(defun make-rectangle-set (&rest rectangles)
  (declare (dynamic-extent rectangles))
  (let ((left nil) (top nil) (right nil) (bottom nil))
    (dolist (rectangle rectangles)
      (with-bounding-rectangle* (rl rt rr rb) rectangle
        (minf-or left rl)
        (minf-or top  rt)
        (maxf-or right  rr)
        (maxf-or bottom rb)))
    (make-rectangle-set-1 (copy-list rectangles)
                          (coordinate left)  (coordinate top) 
                          (coordinate right) (coordinate bottom))))

(defmethod bounding-rectangle* ((rectangle standard-rectangle-set))
  (with-slots (left top right bottom) rectangle
    (values left top right bottom)))

(defmethod transform-region (transformation (set standard-rectangle-set))
  (flet ((transform (rect)
           (transform-region transformation rect)))
    (declare (dynamic-extent #'transform))
    (apply #'make-rectangle-set
           (map 'list #'transform (rectangle-set-rectangles set)))))

(defmethod region-set-function ((region standard-rectangle-set)) 'union)

(defmethod region-set-regions ((region standard-rectangle-set) &key normalize)
  (with-slots (rectangles x-banded-rectangles y-banded-rectangles) region
    (ecase normalize
      ((nil) rectangles)
      ((:x-banding) x-banded-rectangles)
      ((:y-banding) y-banded-rectangles))))

(defmethod slot-unbound
           (class (region standard-rectangle-set) (slot (eql 'x-banded-rectangles)))
  (declare (ignore class))
  (with-slots (x-banded-rectangles) region
    (setq x-banded-rectangles (normalize-rectangles region :x-banding))
    x-banded-rectangles))

(defmethod slot-unbound
           (class (region standard-rectangle-set) (slot (eql 'y-banded-rectangles)))
  (declare (ignore class))
  (with-slots (y-banded-rectangles) region
    (setq y-banded-rectangles (normalize-rectangles region :y-banding))
    y-banded-rectangles))

(defmethod region-union ((rect1 standard-bounding-rectangle)
                         (rect2 standard-bounding-rectangle))
  (with-slots ((left1 left) (top1 top) (right1 right) (bottom1 bottom)) rect1
    (with-slots ((left2 left) (top2 top) (right2 right) (bottom2 bottom)) rect2
      (let ((new-rectangles (ltrb-union left1 top1 right1 bottom1
                                        left2 top2 right2 bottom2)))
        (if (= (length new-rectangles) 1)
            (first new-rectangles)
            (apply #'make-rectangle-set new-rectangles))))))

(define-symmetric-region-method region-union ((rect standard-bounding-rectangle)
                                              (set standard-rectangle-set))
  (apply #'make-rectangle-set rect (slot-value set 'rectangles)))

(defmethod region-union ((set1 standard-rectangle-set) (set2 standard-rectangle-set))
  (apply #'make-rectangle-set (append (slot-value set1 'rectangles)
                                      (slot-value set2 'rectangles))))

(defmethod region-intersection ((rect1 standard-bounding-rectangle)
                                (rect2 standard-bounding-rectangle))
  (with-slots ((left1 left) (top1 top) (right1 right) (bottom1 bottom)) rect1
    (with-slots ((left2 left) (top2 top) (right2 right) (bottom2 bottom)) rect2
      (or (ltrb-intersection left1 top1 right1 bottom1
                             left2 top2 right2 bottom2)
          +nowhere+))))

(define-symmetric-region-method region-intersection ((rect standard-bounding-rectangle)
                                                     (set standard-rectangle-set))
  (let ((new-rectangles nil))
    (with-slots ((left1 left) (top1 top) (right1 right) (bottom1 bottom)) rect
      (flet ((do-intersection (rectangle)
               (with-slots ((left2 left) (top2 top) (right2 right) (bottom2 bottom)) rectangle
                 (let ((new (ltrb-intersection left1 top1 right1 bottom1
                                               left2 top2 right2 bottom2)))
                   (when new (push new new-rectangles))))))
        (declare (dynamic-extent #'do-intersection))
        (map-over-region-set-regions #'do-intersection set))
      (if new-rectangles
          (apply #'make-rectangle-set new-rectangles)
          +nowhere+))))

(defmethod region-intersection ((set1 standard-rectangle-set) (set2 standard-rectangle-set))
  (let ((new-rectangles nil))
    (map-over-region-set-regions
      #'(lambda (rect1)
          (with-slots ((left1 left) (top1 top) (right1 right) (bottom1 bottom)) rect1
            (map-over-region-set-regions
              #'(lambda (rect2)
                  (with-slots ((left2 left) (top2 top) (right2 right) (bottom2 bottom)) rect2
                    (let ((new (ltrb-intersection left1 top1 right1 bottom1
                                                  left2 top2 right2 bottom2)))
                      (when new (push new new-rectangles)))))
              set2)))
      set1)
    (if new-rectangles
        (apply #'make-rectangle-set new-rectangles)
        +nowhere+)))

(defmethod region-difference ((rect1 standard-bounding-rectangle) 
                              (rect2 standard-bounding-rectangle))
  (with-slots ((left1 left) (top1 top) (right1 right) (bottom1 bottom)) rect1
    (with-slots ((left2 left) (top2 top) (right2 right) (bottom2 bottom)) rect2
      (let ((new-rectangles (ltrb-difference left1 top1 right1 bottom1
                                             left2 top2 right2 bottom2)))
        (if new-rectangles
            (if (= (length new-rectangles) 1)
                (first new-rectangles)
                (apply #'make-rectangle-set new-rectangles))
            +nowhere+)))))

(defmethod region-difference ((rect standard-bounding-rectangle) 
                              (set standard-rectangle-set))
  (let ((new-rectangles nil))
    (with-slots ((left1 left) (top1 top) (right1 right) (bottom1 bottom)) rect
      (flet ((do-difference (rectangle)
               (with-slots ((left2 left) (top2 top) (right2 right) (bottom2 bottom)) rectangle
                 (let ((new (ltrb-difference left1 top1 right1 bottom1
                                             left2 top2 right2 bottom2)))
                   (when new (push new new-rectangles))))))
        (declare (dynamic-extent #'do-difference))
        (map-over-region-set-regions #'do-difference set))
      (if new-rectangles
          (apply #'make-rectangle-set new-rectangles)
          +nowhere+))))

(defmethod region-difference ((set standard-rectangle-set) 
                              (rect standard-bounding-rectangle))
  (let ((new-rectangles nil))
    (with-slots ((left2 left) (top2 top) (right2 right) (bottom2 bottom)) rect
      (flet ((do-difference (rectangle)
               (with-slots ((left1 left) (top1 top) (right1 right) (bottom1 bottom)) rectangle
                 (let ((new (ltrb-difference left1 top1 right1 bottom1
                                             left2 top2 right2 bottom2)))
                   (when new (push new new-rectangles))))))
        (declare (dynamic-extent #'do-difference))
        (map-over-region-set-regions #'do-difference set))
      (if new-rectangles
          (apply #'make-rectangle-set new-rectangles)
          +nowhere+))))

(defmethod region-difference ((set1 standard-rectangle-set) (set2 standard-rectangle-set))
  (let ((new-rectangles nil))
    (map-over-region-set-regions
      #'(lambda (rect1)
          (with-slots ((left1 left) (top1 top) (right1 right) (bottom1 bottom)) rect1
            (map-over-region-set-regions
              #'(lambda (rect2)
                  (with-slots ((left2 left) (top2 top) (right2 right) (bottom2 bottom)) rect2
                    (let ((new (ltrb-difference left1 top1 right1 bottom1
                                                left2 top2 right2 bottom2)))
                      (when new (push new new-rectangles)))))
              set2)))
      set1)
    (if new-rectangles
        (apply #'make-rectangle-set new-rectangles)
        +nowhere+)))

(defmethod region-empty-p ((rectangle standard-bounding-rectangle))
  (with-slots (left top right bottom) rectangle
    (declare (type coordinate left top right bottom))
    (or (<= right left)
        (<= bottom top))))

(defmethod region-empty-p ((set standard-rectangle-set))
  (every #'region-empty-p (rectangle-set-rectangles set)))

(defmethod normalize-rectangles ((set standard-rectangle-set) banding)
  (labels ((collect-rectangles (region)
             (etypecase region
               (standard-rectangle-set
                 (mapcan #'collect-rectangles (rectangle-set-rectangles region)))
               (standard-bounding-rectangle
                 (list region))
               (everywhere
                 (list region))))
           (reduce-rectangles (pending-rectangles processed-rectangles)
             (cond ((null pending-rectangles)
                    processed-rectangles)
                   ((region-empty-p (first pending-rectangles))
                    (reduce-rectangles (rest pending-rectangles)
                                       processed-rectangles))
                   (t
                    (let ((intersecting-region 
                            (flet ((intersects-p (rect)
                                     (region-intersects-region-p 
                                       rect (first pending-rectangles))))
                              (declare (dynamic-extent #'intersects-p))
                              (find-if #'intersects-p (rest pending-rectangles)))))
                      (if (null intersecting-region)
                          (reduce-rectangles 
                            (rest pending-rectangles)
                            (cons (first pending-rectangles) processed-rectangles))
                          (reduce-rectangles 
                            (nconc (reduce-rectangle-pair 
                                     (first pending-rectangles)
                                     intersecting-region)
                                   (delete intersecting-region
                                           (rest pending-rectangles)))
                            processed-rectangles))))))
           (reduce-rectangle-pair (rect1 rect2)
             (with-slots ((left1 left) (top1 top) (right1 right) (bottom1 bottom)) rect1
               (with-slots ((left2 left) (top2 top) (right2 right) (bottom2 bottom)) rect2
                 (delete-if 
                   #'region-empty-p 
                   ;; Don't use REGION-UNION, because we are only prepared
                   ;; to deal with bounding rectangles
                   (ltrb-union left1 top1 right1 bottom1
                               left2 top2 right2 bottom2 
                               banding))))))
    (reduce-rectangles (collect-rectangles set) nil)))
