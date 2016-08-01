;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;; The output record generics:
;;;  output-record-children: (record)
;;;  add-output-record: (child record)
;;;  delete-output-record: (child record)
;;;  clear-output-record: (record)
;;;  replay-output-record: (record stream)
;;;  recompute-extent: (record)
;;;  recompute-extent-for-new-child: (there's a default): (record child)
;;;  recompute-extent-for-changed-child: (record child)
;;;  map-over-output-records-overlapping-region:
;;;   (function record region
;;;    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
;;;  map-over-output-records-containing-position:
;;;   (function record x y
;;;    &optional (x-offset 0) (y-offset 0) &rest continuation-args)

(defclass standard-tree-output-record
          (output-record-mixin output-record-element-mixin output-record)
    ((coordinate-sorted-set)                        ;a simple vector, by gawd
     (fill-pointer :initform 0)
     (tallest-box-height :initform (coordinate 0))))

(defmethod initialize-instance :after ((record standard-tree-output-record)
                                       &key (size 200))
  (with-slots (coordinate-sorted-set) record
    (setf coordinate-sorted-set (make-array size))))


;; A top-level coordinate sorted set, used for CLIM windows by default
(defclass standard-tree-output-history
          (stream-output-history-mixin standard-tree-output-record)
    ())


;;; For debugging.
(defmethod output-record-children ((record standard-tree-output-record))
  (with-slots (coordinate-sorted-set fill-pointer) record
    (let ((result (make-list fill-pointer)))
      (replace result coordinate-sorted-set :end1 fill-pointer :end2 fill-pointer)
      result)))

(defmethod output-record-element ((record standard-tree-output-record) index)
  (with-slots (coordinate-sorted-set) record
    (svref coordinate-sorted-set index)))

(defmethod output-record-count ((record standard-tree-output-record) &key fastp)
  (declare (ignore fastp))
  (slot-value record 'fill-pointer))

(defmethod clear-output-record ((record standard-tree-output-record))
  (with-slots (coordinate-sorted-set fill-pointer tallest-box-height) record
    (setf tallest-box-height (coordinate 0))
    ;; Release pointers to objects
    (fill coordinate-sorted-set nil :start 0 :end fill-pointer)
    (setf fill-pointer 0)))

(defmethod recompute-extent-for-changed-child :after
           ((record standard-tree-output-record) child
            old-left old-top old-right old-bottom)
  (declare (ignore old-left old-top old-right old-bottom))
  (with-slots (coordinate-sorted-set fill-pointer tallest-box-height) record
    (maxf tallest-box-height (bounding-rectangle-height child))
    (with-bounding-rectangle* (left top right bottom) child
      (declare (ignore left top))
      (let ((fp fill-pointer)
            (vector coordinate-sorted-set))
        (declare (type simple-vector vector) (type fixnum fp))
        (unless (eql fp 1)
          (let ((old-index
                 (or (coordinate-sorted-set-position
                      child vector fp)
                     ;; If we couldn't find it with the binary search, try
                     ;; again the hard way.  If these things were more
                     ;; disciplined with respect to managing overlapping
                     ;; records, we wouldn't have to resort to this.
                     (position child vector :end fp))))
            (declare (type fixnum old-index))
            (when old-index
              (let ((new-index (coordinate-sorted-set-index-for-position
                                vector right bottom 0 fp)))
                (declare (type fixnum new-index))
                (unless (eql old-index new-index)
                  ;; Effectively child is removed from position
                  ;; old-index and then added at new-index. However
                  ;; the following code does both of these in one
                  ;; operation. The removal operation would cause all
                  ;; indices to the right to be offset by one - so
                  ;; when new-index is greater than old-index we must
                  ;; decrement it in order to get the right place
                  ;; (cim)
                  (when (> new-index old-index)
                    (decf new-index))
                  (let ((d (signum (- new-index old-index))))
                    (declare (type fixnum d))
                    (do* ((i old-index j)
                          (j (+ i d) (+ i d)))
                        ((eql i new-index))
                      (declare (type fixnum i j)
                               (optimize (speed 3) (safety 0)))
                      (setf (svref vector i) (svref vector j))))
                  (setf (svref vector new-index) child))))))))))


(defmethod add-output-record (child (record standard-tree-output-record))
  (with-slots (coordinate-sorted-set fill-pointer tallest-box-height) record
    (let ((vector coordinate-sorted-set)
          (fp fill-pointer))
      (declare (type simple-vector vector) (type fixnum fp))
      (maxf tallest-box-height (bounding-rectangle-height child))
      (with-bounding-rectangle* (left top right bottom) child
        ;; Quick check for doing output at the bottom of the window
        (if (or (zerop fp)
                (let ((other-child (svref vector (1- fp))))
                  (when (eq other-child child)
                    (return-from add-output-record nil))
                  (with-bounding-rectangle* (oleft otop oright obottom) other-child
                    (declare (ignore oleft otop))
                    (or (> bottom obottom)
                        (and (= bottom obottom) 
                             (>= right oright))))))
            (multiple-value-setq (coordinate-sorted-set fill-pointer)
              (simple-vector-push-extend child vector fp 200))
          (let ((index (coordinate-sorted-set-index-for-position
                         vector right bottom 0 fp)))
            (declare (type fixnum index))
            ;; Make sure that the new child comes after any child it overlaps
            ;; so that replaying happens in the right order.
            (loop
              (if (and (< index fp)
                       (with-bounding-rectangle* (oleft otop oright obottom) 
                                                 (svref vector index)
                         (ltrb-overlaps-ltrb-p left top right bottom
                                               oleft otop oright obottom)))
                  (incf index)
                  (return)))
            (multiple-value-setq (coordinate-sorted-set fill-pointer)
              (simple-vector-insert-element child index vector fp 200))))))))

(defmethod delete-output-record
           (child (record standard-tree-output-record) &optional (errorp t))
  (with-slots (coordinate-sorted-set fill-pointer tallest-box-height) record
    (let ((index (or (coordinate-sorted-set-position
                       child coordinate-sorted-set fill-pointer)
                     ;; If we couldn't find it with the binary search, try
                     ;; again the hard way.  If these things were more
                     ;; disciplined with respect to managing overlapping
                     ;; records, we wouldn't have to resort to this.
                     (position child coordinate-sorted-set :end fill-pointer))))
      (cond (index
             (let ((new-fp (the fixnum (1- fill-pointer)))
                   (vector coordinate-sorted-set))
               (declare (type simple-vector vector) (fixnum new-fp))
               (unless (= (the fixnum index) new-fp)
                 ;; Shift the whole vector downward
                 (do ((i (the fixnum index) (1+ i)))
                     ((= i new-fp))
                   (declare (type fixnum i)
                            (optimize (speed 3) (safety 0)))
                   (setf (svref vector i) (svref vector (1+ i)))))
               (setf fill-pointer new-fp)
               t))
            (errorp
             (error "The output record ~S was not found in ~S" child record))))))

(defmethod map-over-output-records-overlapping-region
           (function (record standard-tree-output-record) region
            &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (declare (optimize (safety 0)))
  (let ((vector (slot-value record 'coordinate-sorted-set))
        (length (slot-value record 'fill-pointer)))
    (declare (type simple-vector vector) (fixnum length))
    (if (or (null region) (eq region +everywhere+))
        (dovector ((child index) vector :start 0 :end length :simple-p t)
          (apply function child continuation-args))
      (with-bounding-rectangle* (left1 top1 right1 bottom1) region
        (translate-coordinates (coordinate x-offset) (coordinate y-offset)
          left1 top1 right1 bottom1)
        (let ((start (coordinate-sorted-set-index-for-position 
                       vector (coordinate 0) top1 0 length))
              (limit (+ bottom1 (slot-value record 'tallest-box-height))))
          (declare (type fixnum start)
                   (type coordinate limit))
          ;; Subtract out the record offset from the region, to make comparison fair
          (multiple-value-bind (xoff yoff)
              (output-record-position record)
            (translate-coordinates (- xoff) (- yoff) left1 top1 right1 bottom1))
          (do ((index start (the fixnum (1+ (the fixnum index)))))
              ((= (the fixnum index) length))
            (declare (type fixnum index))
            (let ((child (svref vector index)))
              (with-bounding-rectangle* (left2 top2 right2 bottom2) child
                (when (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
                                            left2 top2 right2 bottom2)
                  (apply function child continuation-args))
                (when (> bottom2 limit)
                  (return nil))))))))))

(defmethod map-over-output-records-containing-position
           (function (record standard-tree-output-record) x y
            &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (declare (optimize (safety 0)))
  (translate-coordinates (coordinate x-offset) (coordinate y-offset)
    x y)
  (let ((vector (slot-value record 'coordinate-sorted-set))
        (length (slot-value record 'fill-pointer))
        (bound (slot-value record 'tallest-box-height)))
    (declare (type simple-vector vector)
             (type fixnum length)
             (type coordinate bound))
    (let ((end (coordinate-sorted-set-index-for-position
                 vector (coordinate 0) (+ y bound 1) 0 length))
          (limit (- y bound)))
      (declare (type fixnum end)
               (type coordinate limit))
      (multiple-value-bind (xoff yoff)
          (output-record-position record)
        (translate-coordinates (- xoff) (- yoff) x y))
      (do ((index (min (1- length) end) (the fixnum (1- (the fixnum index)))))
          ((< (the fixnum index) 0))
        (declare (type fixnum index))
        (let ((child (svref vector index)))
          (with-bounding-rectangle* (left top right bottom) child
            (when (ltrb-contains-position-p left top right bottom x y)
              (apply function child continuation-args))
            (when (< bottom limit)
              (return nil))))))))

;; Like POSITION, but searches coordinate sorted sets
(defun coordinate-sorted-set-position (object vector fill-pointer)
  (declare (type simple-vector vector) (fixnum fill-pointer))
  (declare (optimize (speed 3) (safety 0)))
  (with-bounding-rectangle* (left top right bottom) object
    ;; Binary search to find where this one goes.
    (let ((search-index (coordinate-sorted-set-index-for-position
                          vector right bottom 0 fill-pointer)))
      (declare (type fixnum search-index))
      ;; Search back over things in the same place, accounting for overlap
      (when (< search-index fill-pointer)
        (dovector ((record index) vector :start 0 :end (1+ search-index)
                                         :from-end t :simple-p t)
          (when (eq record object)
            (return-from coordinate-sorted-set-position index))
          (with-bounding-rectangle* (oleft otop oright obottom) record
            (unless (and (= right oright) (= bottom obottom))
              (unless (ltrb-overlaps-ltrb-p left top right bottom
                                            oleft otop oright obottom)
                (return))))))
      ;; Search forward too, also accounting for overlap
      (dovector ((record index) vector
                 :start (if (< search-index fill-pointer) (1+ search-index) 0)
                 :end fill-pointer :simple-p t)
        (when (eq record object)
          (return index))
        (with-bounding-rectangle* (oleft otop oright obottom) record
          (when (> obottom bottom)
            (unless (ltrb-overlaps-ltrb-p left top right bottom
                                          oleft otop oright obottom)
              (return nil))))))))

;; Binary search; dictionary order Y, X.
(defun coordinate-sorted-set-index-for-position (vector right bottom start end)
  (declare (type simple-vector vector) 
           (type fixnum start end)
           (type coordinate right bottom))
  (declare (optimize (speed 3) (safety 0)))
  (let ((below start)
        (above end))
    (declare (type fixnum below above))
    (assert (<= below above))                        ;Binary search will loop otherwise.
    (let (#+(or Genera Minima) (vector vector))
      #+(or Genera Minima) (declare (type simple-vector vector))
      (loop
        (when (= above below)
          (return above))
        (let* ((index (the fixnum (ash (the fixnum (+ above below)) -1)))
               (other-box (svref vector index)))
          (with-bounding-rectangle* (oleft otop oright obottom) other-box
            (declare (ignore oleft otop))
            (cond ((or (< bottom obottom)
                       (and (= bottom obottom) (< right oright)))
                   (setq above index))
                  ((or (> bottom obottom)
                       (and (= bottom obottom) (> right oright)))
                   (if (= below index)
                       (return above)
                       (setq below index)))
                  (t
                   (return index)))))))))
