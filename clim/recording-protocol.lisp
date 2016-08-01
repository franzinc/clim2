;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 International Lisp Associates."


;;; A mix-in for classes that can be stored by other output records
;;; OUTPUT-RECORD-ELEMENT-MIXIN has slots for
;;;     (bounding rectangle, parent, start-position, end-position, contents-ok)
;;;     plus space to store old bounding rectangle and position for incremental redisplay
;;;  graphics output records
;;;  text output records
;;;  OUTPUT-RECORD-MIXIN has slots for (generation-tick, old-children)
;;;      both used for incremental redisplay.
;;;   STANDARD-SEQUENCE-OUTPUT-RECORD
;;;    table, row, column, cell, presentation
;;;   STANDARD-TREE-OUTPUT-RECORD
;;;   KD-TREE-OUTPUT-RECORD (someday)

;;; The output-record-element "protocol"
;;;   :x-position, :y-position, :parent init args.
;;;   bounding rectangle protocol

;;; The output record protocol:
;;;  output-record-children: (record)
;;;  add-output-record: (child record)
;;;  delete-output-record: (child record)
;;;  clear-output-record: (record)
;;;  replay-output-record: (record stream &optional region (x-offset 0) (y-offset 0)
;;;  recompute-extent: (record)
;;;  recompute-extent-for-new-child (there's a default): (record child)
;;;  recompute-extent-for-changed-child: (record child)
;;;  map-over-output-records-overlapping-region:
;;;   (function record region
;;;    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
;;;  map-over-output-records-containing-position:
;;;   (function record x y
;;;    &optional (x-offset 0) (y-offset 0) &rest continuation-args)

;;; The incremental redisplay protocol:
;;;  see file incremental-redisplay-protocol.text, and incremental-redisplay.lisp.
;;;

;; Bounding rectangle position and set-position are in relative coordinates, 
;;   relative to (OUTPUT-RECORD-POSITION (OUTPUT-RECORD-PARENT RECORD)).
;; The bounding rectangle measures just the ink.
;; (OUTPUT-RECORD-START-CURSOR-POSITION RECORD) refers to the position of the
;; cursor at the start of RECORD.  It is also the origin of the
;; coordinate system for all children of RECORD.
(defclass output-record-element-mixin (standard-bounding-rectangle)
    ;; start position is relative to the parent's start-position
    ;; start position is where the cursor was when the new
    ;; output-record was started.
    ((start-x :initform (coordinate 0) :initarg :start-x :type coordinate)
     (start-y :initform (coordinate 0) :initarg :start-y :type coordinate)
     ;; end position is relative to start position.
     ;; end position is where the cursor was when we finished the
     ;; output-record.
     (end-x :initform (coordinate 0) :initarg :end-x :type coordinate)
     (end-y :initform (coordinate 0) :initarg :end-y :type coordinate)
     ;; old-start-position is relative to old-start-position of parent
     (old-start-x :initform (coordinate 0) :type coordinate)
     (old-start-y :initform (coordinate 0) :type coordinate)
     ;; old bounding rectangle is relative to parents' old-start-position.
     (old-bounding-rectangle :initform nil
                             :accessor output-record-old-bounding-rectangle)
     (contents-ok :initform nil :accessor output-record-contents-ok)
     (parent :accessor output-record-parent :initarg :parent)
     (stream :initform nil :accessor output-record-stream))
  (:default-initargs :parent nil 
                     :left  (coordinate 0) :top (coordinate 0)
                     :right (coordinate 0) :bottom (coordinate 0)))

;;; Give initial rectangle of 0 size, it will get expanded as children are added.
(defmethod initialize-instance :after ((record output-record-element-mixin)
                                       &key (x-position 0) (y-position 0))
  (with-slots (left top right bottom) record
    (let ((x (coordinate x-position))
          (y (coordinate y-position)))
      (setf left   x
            top    y
            right  x
            bottom y))))

;;; Shadow the method on RECTANGLE with this one that keeps the start-position and 
;;; bounding rectangle in synch.
(defmethod bounding-rectangle-set-position ((record output-record-element-mixin) nx ny)
  (declare (type real nx ny))
  (with-slots (left top right bottom start-x start-y parent) record
    (declare (type coordinate left top right bottom start-x start-y))
    (let ((nx (coordinate nx))
          (ny (coordinate ny)))
      ;; Move the start position by as much as we do the record.
      (setq start-x (+ start-x (- nx left)))
      (setq start-y (+ start-y (- ny top)))
      (let ((width (- right left))
            (height (- bottom top)))
        (setf left nx top ny)
        (setf right  (+ nx width)
              bottom (+ ny height))))))

#+++ignore
(defmethod bounding-rectangle-set-position :around ((record output-record-element-mixin) nx ny)
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (call-next-method)
    (note-output-record-moved record (- nx x1) (- ny y1) (- nx x2) (- ny y2))))

#+++ignore
(defmethod bounding-rectangle-set-edges :around ((record output-record-element-mixin) 
                                                 left top right bottom)
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (call-next-method)
    (note-output-record-moved record (- left x1) (- top y1) (- right x2) (- bottom y2))))

;; For people who roll their own from scratch
(defmethod note-output-record-moved ((record t) dx1 dy1 dx2 dy2)
  (declare (ignore dx1 dy1 dx2 dy2))
  nil)

(defmethod note-output-record-moved ((record output-record-element-mixin) dx1 dy1 dx2 dy2)
  (declare (ignore dx1 dy1 dx2 dy2))
  nil)

(defun-inline output-record-position (record)
  (output-record-start-cursor-position record))

(defmethod output-record-set-position ((record output-record-element-mixin) x y)
  (declare (type coordinate x y))
  (bounding-rectangle-set-position record (coordinate x) (coordinate y)))

(defgeneric* (setf output-record-position) (x y record))
(defmethod* (setf output-record-position) (x y (record t))
  (output-record-set-position record x y))

(defmethod output-record-start-cursor-position ((record output-record-element-mixin))
  (with-slots (start-x start-y) record
    (values start-x start-y)))

;;; Keep the start-position and bounding rectangle in synch
(defmethod output-record-set-start-cursor-position
           ((record output-record-element-mixin) nx ny)
  (declare (type coordinate nx ny))
  (with-slots (start-x start-y) record
    (declare (type coordinate start-x start-y))
    (let ((nx (coordinate nx))
          (ny (coordinate ny)))
      (with-bounding-rectangle* (left top right bottom) record
        (let ((dx (- nx start-x))
              (dy (- ny start-y)))
          (bounding-rectangle-set-edges 
            record
            (+ left dx) (+ top dy) (+ right dx) (+ bottom dy))
          (setf start-x nx start-y ny))))))

(defgeneric* (setf output-record-start-cursor-position) (x y record))
(defmethod* (setf output-record-start-cursor-position) (x y (record t))
  (output-record-set-start-cursor-position record x y))

(defmethod output-record-end-cursor-position ((record output-record-element-mixin))
  (with-slots (end-x end-y) record
    (values end-x end-y)))
  
(defmethod output-record-set-end-cursor-position ((record output-record-element-mixin) nx ny)
  (declare (type coordinate nx ny))
  (with-slots (end-x end-y) record
    (setf end-x (coordinate nx))
    (setf end-y (coordinate ny))))

(defgeneric* (setf output-record-end-cursor-position) (x y record))
(defmethod* (setf output-record-end-cursor-position) (x y (record t))
  (output-record-set-end-cursor-position record x y))

(defmethod output-record-old-start-cursor-position ((record output-record-element-mixin))
  (with-slots (old-start-x old-start-y) record
    (values old-start-x old-start-y)))

(defmethod output-record-set-old-start-cursor-position
           ((record output-record-element-mixin) nx ny)
  (declare (type coordinate nx ny))
  (with-slots (old-start-x old-start-y) record
    (setf old-start-x (coordinate nx))
    (setf old-start-y (coordinate ny))))


(defmethod output-record-children ((record output-record-element-mixin))
  nil)

;;; For specialization by PRESENTATIONs, for example
(defmethod output-record-refined-position-test ((record output-record-element-mixin) x y)
  (declare (ignore x y))
  t)


#+ignore
(defun compute-output-record-offsets (record)
  (let ((parent (output-record-parent record)))
    (if (null parent)
        (values (coordinate 0) (coordinate 0))
      (multiple-value-bind (x y)
          (compute-output-record-offsets parent)
        (declare (type coordinate x y))
        (multiple-value-bind (our-x our-y) (output-record-position record)
          (declare (type coordinate our-x our-y))
          (values (+ our-x x) (+ our-y y)))))))


(defun compute-output-record-offsets (record)
  (compute-output-record-offsets-1 (output-record-parent record)))


(defun compute-output-record-offsets-1 (record)
  (if (null record)
      (values 0 0)
  (let ((parent (output-record-parent record)))
    (multiple-value-bind (x y)
          (compute-output-record-offsets-1 parent)
        (declare (type coordinate x y))
        (multiple-value-bind (our-x our-y) (output-record-position record)
          (declare (type coordinate our-x our-y))
          (values (+ our-x x) (+ our-y y)))))))

(defmethod region-equal
           ((record1 output-record-element-mixin) (record2 output-record-element-mixin))
  (with-bounding-rectangle* (left1 top1 right1 bottom1) record1
    (with-bounding-rectangle* (left2 top2 right2 bottom2) record2
      (if (eq (output-record-parent record1) (output-record-parent record2))
          (ltrb-equals-ltrb-p left1 top1 right1 bottom1
                              left2 top2 right2 bottom2)
        (multiple-value-bind (xoff1 yoff1) (compute-output-record-offsets record1)
          (declare (type coordinate xoff1 yoff1))
          (multiple-value-bind (xoff2 yoff2) (compute-output-record-offsets record2)
            (declare (type coordinate xoff2 yoff2))
            (translate-coordinates xoff1 yoff1 left1 top1 right1 bottom1)
            (translate-coordinates xoff2 yoff2 left2 top2 right2 bottom2)
            (ltrb-equals-ltrb-p left1 top1 right1 bottom1
                                left2 top2 right2 bottom2)))))))

(defmethod region-contains-position-p
           ((record output-record-element-mixin) x y)
  (with-bounding-rectangle* (left top right bottom) record
    (multiple-value-bind (xoff yoff) (compute-output-record-offsets record)
      (declare (type coordinate xoff yoff))
      (ltrb-contains-position-p left top right bottom
                                (+ (coordinate x) xoff) (+ (coordinate y) yoff)))))

(defmethod region-contains-region-p
           ((record1 output-record-element-mixin) (record2 output-record-element-mixin))
  (with-bounding-rectangle* (left1 top1 right1 bottom1) record1
    (with-bounding-rectangle* (left2 top2 right2 bottom2) record2
      (if (eq (output-record-parent record1) (output-record-parent record2))
          (ltrb-contains-ltrb-p left1 top1 right1 bottom1
                                left2 top2 right2 bottom2)
        (multiple-value-bind (xoff1 yoff1) (compute-output-record-offsets record1)
          (declare (type coordinate xoff1 yoff1))
          (multiple-value-bind (xoff2 yoff2) (compute-output-record-offsets record2)
            (declare (type coordinate xoff2 yoff2))
            (translate-coordinates xoff1 yoff1 left1 top1 right1 bottom1)
            (translate-coordinates xoff2 yoff2 left2 top2 right2 bottom2)
            (ltrb-contains-ltrb-p left1 top1 right1 bottom1
                                  left2 top2 right2 bottom2)))))))

(defmethod region-intersects-region-p
           ((record1 output-record-element-mixin) (record2 output-record-element-mixin))
  (with-bounding-rectangle* (left1 top1 right1 bottom1) record1
    (with-bounding-rectangle* (left2 top2 right2 bottom2) record2
      (if (eq (output-record-parent record1) (output-record-parent record2))
          (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
                                left2 top2 right2 bottom2)
        (multiple-value-bind (xoff1 yoff1) (compute-output-record-offsets record1)
          (declare (type coordinate xoff1 yoff1))
          (multiple-value-bind (xoff2 yoff2) (compute-output-record-offsets record2)
            (declare (type coordinate xoff2 yoff2))
            (translate-coordinates xoff1 yoff1 left1 top1 right1 bottom1)
            (translate-coordinates xoff2 yoff2 left2 top2 right2 bottom2)
            (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
                                  left2 top2 right2 bottom2)))))))

(defun region-contains-offset-region-p (region1 region2 xoff yoff)
  (declare (type coordinate xoff yoff))
  (with-bounding-rectangle* (left1 top1 right1 bottom1) region1
    (with-bounding-rectangle* (left2 top2 right2 bottom2) region2
      (ltrb-contains-ltrb-p left1 top1 right1 bottom1
                            (+ left2 xoff) (+ top2 yoff) (+ right2 xoff) (+ bottom2 yoff)))))

(defun region-intersects-offset-region-p (region1 region2 xoff yoff)
  (declare (type coordinate xoff yoff))
  (with-bounding-rectangle* (left1 top1 right1 bottom1) region1
    (with-bounding-rectangle* (left2 top2 right2 bottom2) region2
      (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
                            (+ left2 xoff) (+ top2 yoff) (+ right2 xoff) (+ bottom2 yoff)))))

(defun offset-region-contains-position-p (region xoff yoff x y)
  (declare (type coordinate xoff yoff x y))
   (with-bounding-rectangle* (left top right bottom) region
     (ltrb-contains-position-p left top right bottom
                               (- x xoff) (- y yoff))))

;;; This maps over all of the children of the record
#+Genera (zwei:defindentation (map-over-output-records 1 1))
(defun map-over-output-records (function record
                                &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
                                &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
#+(or aclpc acl86win32)  
  (when (null x-offset)
    (setf x-offset (coordinate 0))) ;;; investigate why this is necessary +++
#+(or aclpc acl86win32)  
  (when (null y-offset)
    (setf y-offset (coordinate 0)))
  (apply #'map-over-output-records-overlapping-region
         function record nil (coordinate x-offset) (coordinate y-offset) continuation-args))

;;; This must map over the children in such a way that, when it maps over
;;; overlapping children, the topmost (most recently inserted) child is
;;; hit last.  This is because this function is used for things such as
;;; replaying, where the most recently drawn thing must come out on top
;;; (i.e., must be drawn last).  If the region is NIL, then this maps over
;;; all of the children in the output record.
#+Genera (zwei:defindentation (map-over-output-records-overlapping-region 2 1))
(defgeneric map-over-output-records-overlapping-region
            (function record region
             &optional x-offset y-offset &rest continuation-args)
  (declare (dynamic-extent function continuation-args)))

;;; This must map over the children in such a way that, when it maps over
;;; overlapping children, the topmost (most recently inserted) child is
;;; hit first, that is, the opposite order of MAP-...-OVERLAPPING-REGION.
;;; This is because this function is used for things like locating the
;;; presentation under the pointer, where the topmost thing wants to be
;;; located first.
#+Genera (zwei:defindentation (map-over-output-records-containing-position 3 1))
(defgeneric map-over-output-records-containing-position
            (function record x y
             &optional x-offset y-offset &rest continuation-args)
  (declare (dynamic-extent function continuation-args)))

;;; X-offset and Y-offset represent the accumulated offset between the
;;; regions's native coordinates and "our" coordinates and must be added
;;; to our local coordinates (or subtracted from the region, if
;;; possible) in order to validly compare them.
;;; 
;;; In the absence of x- and y- offsets, region should be in the
;;; coordinate system of the record - i.e. relative to 
;;;  (OUTPUT-RECORD-POSITION RECORD).
;;; This is the same coordinate system as the output record children
;;; we are mapping over.
(defmethod map-over-output-records-overlapping-region
           (function (record output-record-element-mixin) region
            &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
            &rest continuation-args)
  (declare (ignore region function x-offset y-offset continuation-args)
           (dynamic-extent function continuation-args))
  nil)

(defmethod map-over-output-records-containing-position
           (function (record output-record-element-mixin) x y
            &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
            &rest continuation-args)
  (declare (ignore x y function x-offset y-offset continuation-args)
           (dynamic-extent function continuation-args))
  nil)

(defmethod map-over-output-records-containing-position
           (function (record t) x y
            &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (apply #'map-over-output-records-overlapping-region
         function record (make-point x y)
         (coordinate x-offset) (coordinate y-offset) continuation-args))


;; A mix-in for classes that can store other output records
(defclass output-record-mixin ()
     ;; OLD-CHILDREN is the list of unmatched output records from last redisplay
     ;; pass.  if you implement your own FIND-CHILD-OUTPUT-RECORD, and iff you
     ;; match from OLD-CHILDREN, you are required to remove the match from
     ;; OLD-CHILDREN, probably by using DECACHE-CHILD-OUTPUT-RECORD.
     ((old-children :initform nil :accessor output-record-old-children)
      (generation-tick :initform 0 :initarg :generation-tick
                       :accessor output-record-generation-tick)))

(defmethod children-never-overlap-p ((record output-record-mixin)) nil)

(defmethod note-output-record-moved :after ((record output-record-mixin) dx1 dy1 dx2 dy2)
  (unless (and (zerop dx1)
               (zerop dy1)
               (zerop dx2)
               (zerop dy2))
    (flet ((note-moved (child)
             (note-output-record-moved child dx1 dy1 dx2 dy2)))
      (declare (dynamic-extent #'note-moved))
      (map-over-output-records #'note-moved record))))

(defmethod bounding-rectangle-set-position :around ((record output-record-mixin) nx ny)
  (multiple-value-bind (ox oy)
      (output-record-position record)
    (call-next-method)
    (when (or (/= ox nx) (/= oy ny))
      (note-output-record-moved record 
                                (coordinate 1) (coordinate 1)
                                (coordinate 1) (coordinate 1)))))

(defmethod bounding-rectangle-set-edges :around ((record output-record-mixin) 
                                                 left top right bottom)
  #-aclpc (declare (ignore left top right bottom))
  (multiple-value-bind (ox oy)
      (output-record-position record)
    (call-next-method)
    (multiple-value-bind (nx ny)
        (output-record-position record)
      (when (or (/= ox nx) (/= oy ny))
        (note-output-record-moved record 
                                  (coordinate 1) (coordinate 1)
                                  (coordinate 1) (coordinate 1))))))


;; If some coordinate is relative to a given output-record, then
;; CONVERT-FROM-RELATIVE-TO-ABSOLUTE-COORDINATES returns an x,y offset
;; to be ADDED to any coordinates relative to OUTPUT-RECORD to give you
;; absolute coordinates.
(defun convert-from-relative-to-absolute-coordinates (stream output-record)
  (declare (values x-offset y-offset))
  (cond ((null output-record)
         ;;--- Why on earth do we need this now?
         (values (coordinate 0) (coordinate 0)))
        ((and stream
              (eq output-record (stream-current-output-record stream)))
         (let ((position (stream-output-history-position stream)))
           (values (point-x position) (point-y position))))
        ((null (output-record-parent output-record))
         (values (coordinate 0) (coordinate 0)))
        (t
         (multiple-value-bind (x y)
             (convert-from-relative-to-absolute-coordinates
               stream (output-record-parent output-record))
           (declare (type coordinate x y))
           (multiple-value-bind (our-x our-y) (output-record-position output-record)
             (declare (type coordinate our-x our-y))
             (values (+ our-x x) (+ our-y y)))))))

;; If some coordinate is in absolute coordinates, then
;; CONVERT-FROM-ABSOLUTE-TO-RELATIVE-COORDINATES returns an x,y offset
;; to be ADDED to any absolute coordinates to give you coordinates
;; relative to OUTPUT-RECORD.
(defun convert-from-absolute-to-relative-coordinates (stream output-record)
  (declare (values x-offset y-offset))
  (cond ((null output-record)
         ;;--- Why on earth do we need this now?
         (values (coordinate 0) (coordinate 0)))
        ((eq output-record (stream-current-output-record stream))
         (let ((position (stream-output-history-position stream)))
           (values (- (point-x position)) (- (point-y position)))))
        ((null (output-record-parent output-record))
         (values (coordinate 0) (coordinate 0)))
        (t
         (multiple-value-bind (x y)
             (convert-from-absolute-to-relative-coordinates
               stream (output-record-parent output-record))
           (declare (type coordinate x y))
           (multiple-value-bind (our-x our-y) (output-record-position output-record)
             (declare (type coordinate our-x our-y))
             (values (- x our-x) (- y our-y)))))))

;; if ANCESTOR is an output-record with DESCENDANT as a descendant
;; output-record (member of the transitive closure of all children of
;; ancestor) then:  CONVERT-FROM-ANCESTOR-TO-DESCENDANT-COORDINATES
;; returns an x,y offset pair that can be ADDED to any coordinates
;; relative to ANCESTOR in order to get coordinates relative to
;; DESCENDANT.
(defun convert-from-ancestor-to-descendant-coordinates (ancestor descendant)
  (declare (values x-offset y-offset))
  (cond ((eq descendant ancestor)
         (values (coordinate 0) (coordinate 0)))
        ((null descendant)
         (error "~S was not an ancestor of ~S" ancestor descendant))
        (t
         (multiple-value-bind (x y)
             (convert-from-ancestor-to-descendant-coordinates
               ancestor (output-record-parent descendant))
           (declare (type coordinate x y))
           (multiple-value-bind (our-x our-y) (output-record-position descendant)
             (declare (type coordinate our-x our-y))
             (values (- x our-x) (- y our-y)))))))

;; if ANCESTOR is an output-record with DESCENDANT as a descendant
;; output-record (member of the transitive closure of all children of
;; ancestor) then:  CONVERT-FROM-DESCENDANT-TO-ANCESTOR-COORDINATES
;; returns an x,y offset pair that can be ADDED to any coordinates
;; relative to DESCENDANT in order to get coordinates relative to
;; ANCESTOR.
(defun convert-from-descendant-to-ancestor-coordinates (descendant ancestor)
  (declare (values x-offset y-offset))
  (cond ((eq descendant ancestor)
         (values (coordinate 0) (coordinate 0)))
        ((null descendant)
         (error "~S was not an ancestor of ~S" ancestor descendant))
        (t
         (multiple-value-bind (x y)
             (convert-from-descendant-to-ancestor-coordinates
               (output-record-parent descendant) ancestor)
           (declare (type coordinate x y))
           (multiple-value-bind (our-x our-y) (output-record-position descendant)
             (declare (type coordinate our-x our-y))
             (values (+ our-x x) (+ our-y y)))))))

;; CONVERT-FROM-CHILD-TO-PARENT-COORDINATE returns an x,y offset pair
;; that can be ADDED to any coordinates relative to RECORD in
;; order to get coordinates relative to the parent of RECORD

(defun-inline convert-from-child-to-parent-coordinates (record)
  (output-record-position record))
  

;;; Rest of stuff started in clim-defs...
(defun construct-output-record-1 (type &rest initargs)
  (declare (dynamic-extent initargs))
  (let ((constructor (gethash type *output-record-constructor-cache*)))
    (if constructor
        (apply constructor initargs)
        (apply #'make-instance type initargs))))

#||
;;; A hash table associating vectors holding free output records.
;;; The idea is to not cons on allocate/free, but be fast.
(defvar *record-resource-table* (make-hash-table))
(defvar *use-record-resources* nil)

(defun allocate-record (type)
  (when *use-record-resources*
    (multiple-value-bind (record-vector found-p)
        (gethash type *record-resource-table*)
      (unless found-p
        (setq record-vector
              (setf (gethash type *record-resource-table*)
                    (make-array 20 :fill-pointer 0))))
      (vector-pop record-vector))))

(defun free-record (record)
  (when *use-record-resources*
    (let ((type (class-name (class-of record))))
      (multiple-value-bind (record-vector found-p)
          (gethash type *record-resource-table*)
        (unless found-p
          (setq record-vector
                (setf (gethash type *record-resource-table*)
                      (make-array 20 :fill-pointer 0))))
        (setf (output-record-parent record) nil)
        (vector-push-extend record record-vector)))))
||#

(defmethod invoke-with-new-output-record
           ((stream output-recording-mixin) continuation record-type constructor
            &rest initargs &key parent &allow-other-keys)
  (declare (dynamic-extent initargs))
  (with-keywords-removed (initargs initargs '(:parent))
    (let* ((current-output-record (stream-current-output-record stream))
           (new-output-record (and (stream-redisplaying-p stream)
                                   (or parent current-output-record)
                                   (apply #'find-child-output-record
                                          (or parent current-output-record) t record-type initargs))))
      (multiple-value-bind (cursor-x cursor-y)
          (stream-cursor-position stream)
        (declare (type coordinate cursor-x cursor-y))
        (multiple-value-bind (x y)
            (multiple-value-bind (px py)
                (point-position
                  (stream-output-history-position stream))
              (declare (type coordinate px py))
              (position-difference cursor-x cursor-y px py))
          (declare (type coordinate x y))
          (if new-output-record
              (copy-display-state new-output-record nil)
              (setq new-output-record
                    ;; Note that we set the x- and y-positions to 0.  We
                    ;; do this because setting the cursor position to (x,y)
                    ;; has the appropriate effect, and setting the other
                    ;; positions can cause the record to be mis-positioned
                    ;; if we don't actually do any output into it.
                    (if constructor
                        (apply constructor
                               :x-position 0 :y-position 0 initargs)
                        (apply #'construct-output-record-1 record-type
                               :x-position 0 :y-position 0 initargs))))
          (output-record-set-start-cursor-position new-output-record x y)
          (with-output-record-1 continuation 
                                stream new-output-record cursor-x cursor-y)
          (when (stream-redisplaying-p stream)
            (recompute-contents-ok new-output-record))
          ;; We set the parent after doing everything else so that calls
          ;; to RECOMPUTE-CONTENTS-OK inside the dynamic extent of the
          ;; continuation won't take forever.
          (let ((parent (or parent
                            current-output-record
                            (stream-output-history stream))))
            (when parent 
              (add-output-record new-output-record parent)))
          new-output-record)))))

(defmethod invoke-with-new-output-record
           ((stream t) continuation record-type constructor
            &rest initargs &key parent &allow-other-keys)
  #-aclpc (declare (ignore record-type constructor initargs parent))
  (funcall continuation stream))

(defun with-output-record-1 (continuation stream record abs-x abs-y)
  ;; Close the text record before and after
  (stream-close-text-output-record (encapsulating-stream stream))
  (let ((current-output-position
          (stream-output-history-position stream)))
    (letf-globally (((point-x current-output-position) abs-x)
                    ((point-y current-output-position) abs-y)
                    ((stream-current-output-record stream) record))
      (funcall continuation record)
      (multiple-value-bind (end-x end-y)
          (stream-cursor-position stream)
        (declare (type coordinate end-x end-y))
        (output-record-set-end-cursor-position
          record (- end-x abs-x) (- end-y abs-y)))
      (stream-close-text-output-record (encapsulating-stream stream)))))

(defun invoke-with-room-for-graphics (stream continuation record-type move-cursor 
                                      &key height (first-quadrant t))
  (let ((record
          (if first-quadrant
              (with-output-recording-options (stream :draw nil :record t)
                (with-first-quadrant-coordinates (stream)
                  (with-new-output-record (stream record-type)
                    (funcall continuation stream))))
              (with-output-recording-options (stream :draw nil :record t)
                (with-new-output-record (stream record-type)
                  (funcall continuation stream))))))
    (multiple-value-bind (x y) (output-record-position record)
      (declare (type coordinate x y))
      ;;--- Hey, there is something wierd going on here.  The problem is that
      ;;--- OUTPUT-RECORD-POSITION and OUTPUT-RECORD-SET-POSITION seem to obey
      ;;--- different coordinate system conventions.  Geez.
      (when height
        (incf y (- height (bounding-rectangle-height record))))
      (output-record-set-position record x y))
    (tree-recompute-extent record)
    (replay record stream)
    (when move-cursor
      (move-cursor-beyond-output-record stream record))
    record))

(defun normalize-replay-region (region stream)
  (cond ((or (null region)
             (eq region +everywhere+))
         ;; If we've been asked to replay everything, set the replay
         ;; region to the viewport
         (or (pane-viewport-region stream) (sheet-region stream)))
        ((or (output-record-p region)
             (displayed-output-record-p region))
         ;; If the replay region is itself an output record, make a
         ;; new region in the proper coordinate system
         (with-bounding-rectangle* (left top right bottom) region
           (multiple-value-bind (xoff yoff)
               (convert-from-relative-to-absolute-coordinates 
                 stream (output-record-parent region))
             (translate-coordinates xoff yoff left top right bottom))
           (make-bounding-rectangle left top right bottom)))
        (t region)))

(defun replay (record stream &optional region)
  (when (stream-drawing-p stream)
    (setq region (normalize-replay-region region stream))
    (multiple-value-bind (x-offset y-offset)
        (convert-from-relative-to-absolute-coordinates stream (output-record-parent record))
      ;; Output recording should be off, but let's be forgiving...
      (letf-globally (((stream-recording-p stream) nil))
        (replay-output-record record stream region x-offset y-offset)))))

;; Replay all the the inferiors of RECORD that overlap REGION.
(defmethod replay-output-record ((record output-record-mixin) stream
                                 &optional region
                                           (x-offset (coordinate 0)) (y-offset (coordinate 0)))
  ;;--- Doing things this way bypasses any REPLAY-OUTPUT-RECORD methods supplied on 
  ;;--- non-standard classes that satisfy the output record protocol.
  ;;--- Too bad, this relative coordinates stuff is a disaster anyway.
  (declare (type coordinate x-offset y-offset))
  (labels ((replay-1 (record x-offset y-offset)
             (declare (type coordinate x-offset y-offset))
             (if (output-record-p record)
                 (multiple-value-bind (xoff yoff) (output-record-position record)
                   (map-over-output-records-overlapping-region 
                     #'replay-1 record region 
                     (- x-offset) (- y-offset)
                     (+ x-offset xoff) (+ y-offset yoff))
                   ;;--- Nasty hack to get around nasty bug caused by
                   ;;--- doing things this way (namely, gadget output
                   ;;--- records don't get seen at the right time)
                   (note-output-record-replayed record stream region x-offset y-offset))
                 (replay-output-record record stream region x-offset y-offset))))
    (declare (dynamic-extent #'replay-1))
    (replay-1 record x-offset y-offset)))

;;--- Flush this when REPLAY-OUTPUT-RECORD calls itself recursively
(defmethod note-output-record-replayed ((record t) stream
                                        &optional region x-offset y-offset)
  (declare (ignore stream region x-offset y-offset))
  nil)

;;--- Flush this when REPLAY-OUTPUT-RECORD calls itself recursively
(defmethod note-output-record-replayed ((record output-record-mixin) stream
                                        &optional region x-offset y-offset)
  (declare (ignore stream region x-offset y-offset))
  nil)

(defun change-output-record-ink (record ink)
  (labels ((change-ink (record)
             (if (output-record-p record)
                 (map-over-output-records #'change-ink record)
                 (setf (displayed-output-record-ink record) ink))))
    (declare (dynamic-extent #'change-ink))
    (change-ink record)))

(defun move-cursor-beyond-output-record (stream record &optional (y-offset 0))
  (multiple-value-bind (xoff yoff)
      (convert-from-relative-to-absolute-coordinates
        stream (output-record-parent record))
    (declare (type coordinate xoff yoff))
    (with-bounding-rectangle* (left top right bottom) record
      (declare (ignore left top))
      (with-end-of-page-action (stream :allow)
        (stream-set-cursor-position
          stream
          (+ right xoff) (- (+ bottom yoff) (stream-line-height stream) y-offset))))))


(defmethod recompute-extent-for-changed-child ((record output-record-mixin) child
                                               old-left old-top old-right old-bottom)
  (declare (type coordinate old-top old-right old-bottom))
  ;; (assert (child-completely-contained-within-extent-of record child))
  (with-bounding-rectangle* (left top right bottom) record
    ;; We must recompute the extent if the child is not completely contained
    ;; or if it used to "define" one of the old edges.
    ;; A picture would help, but we're not going to draw it here. (:-)
    (multiple-value-bind (xoff yoff)
        (convert-from-child-to-parent-coordinates record)
      (translate-coordinates xoff yoff
                             old-left old-top old-right old-bottom)
      (when (or (not (region-contains-offset-region-p record child xoff yoff))
                (= old-left left)
                (= old-top top)
                (= old-right right)
                (= old-bottom bottom))
        (recompute-extent record)))))

(defmethod recompute-extent ((record output-record-mixin))
  (with-slots (parent) record
    (with-bounding-rectangle* (old-left old-top old-right old-bottom) record
      (let ((once nil)
            (min-x (coordinate 0))
            (min-y (coordinate 0))
            (max-x (coordinate 0))
            (max-y (coordinate 0)))
        (declare (type coordinate min-x min-y max-x max-y))
        (flet ((recompute-extent-of-child (child)
                 (with-bounding-rectangle* (left top right bottom) child
                   (cond (once
                          (minf min-x left)
                          (minf min-y top)
                          (maxf max-x right)
                          (maxf max-y bottom))
                         (t
                          (setq min-x left
                                min-y top
                                max-x right
                                max-y bottom
                                once  t))))))
          (declare (dynamic-extent #'recompute-extent-of-child))
          (map-over-output-records #'recompute-extent-of-child record))
        (multiple-value-bind (xoff yoff)
            (convert-from-child-to-parent-coordinates record)
          (declare (type coordinate xoff yoff))
          (if once
              (progn (assert (ltrb-well-formed-p min-x min-y max-x max-y))
                     (bounding-rectangle-set-edges
                       record
                       (+ min-x xoff) (+ min-y yoff)
                       (+ max-x xoff) (+ max-y yoff)))
              ;; No children
              (bounding-rectangle-set-edges record
                                            (coordinate 0) (coordinate 0)
                                            (coordinate 0) (coordinate 0))))
        (when parent
          (recompute-extent-for-changed-child
            parent record old-left old-top old-right old-bottom))))))

(defmethod recompute-extent-for-new-child ((record output-record-mixin) child)
  (with-slots (parent) record
    (with-bounding-rectangle* (left top right bottom) record
      (let ((old-left left)
            (old-top top)
            (old-right right)
            (old-bottom bottom))
        (with-bounding-rectangle* (eleft etop eright ebottom) child
          (multiple-value-bind (xoff yoff)
              (convert-from-child-to-parent-coordinates record)
            (translate-coordinates xoff yoff
              eleft etop eright ebottom))
          (cond ((= (output-record-count record :fastp t) 1)
                 (bounding-rectangle-set-edges record eleft etop eright ebottom))
                (t (bounding-rectangle-set-edges record
                                                 (min left eleft) (min top etop)
                                                 (max right eright) (max bottom ebottom)))))
        (when parent
          (recompute-extent-for-changed-child
            parent record old-left old-top old-right old-bottom))))))

;;; This is for adjusting extents after a bunch of leaves have been moved.
(defmethod tree-recompute-extent ((record output-record-element-mixin))
  (with-bounding-rectangle* (old-left old-top old-right old-bottom) record
    (let ((parent (output-record-parent record)))
      (tree-recompute-extent-1 record)
      (when parent
        (recompute-extent-for-changed-child
          parent record old-left old-top old-right old-bottom)))))

;;; Common to all implementations.
;;; ADD-OUTPUT-RECORD assumes that CHILD's start-position and bounding 
;;; rectangle have already been normalized to RECORD's coordinate system.
#-(or aclpc acl86win32) 
(defmethod add-output-record :after (child (record output-record-mixin)) ;was :around
  (setf (output-record-parent child) record)
  (recompute-extent-for-new-child record child)
  (when (output-record-stream record)
    (note-output-record-attached child (output-record-stream record))))

;; For people who roll their own from scratch
(defmethod note-output-record-attached ((record t) stream)
  (declare (ignore stream))
  nil)

(defmethod note-output-record-attached ((record output-record-element-mixin) stream)
  (setf (output-record-stream record) stream))

(defmethod note-output-record-attached :after ((record output-record-mixin) stream)
  (flet ((note-attached (rec)
           (note-output-record-attached rec stream)))
    (declare (dynamic-extent #'note-attached))
    (map-over-output-records #'note-attached record)))

;;; Ditto.
#-(or aclpc acl86win32) 
(defmethod delete-output-record :after
           (child (record output-record-mixin) &optional (errorp t))
  #-(or aclpc acl86win32) (declare (ignore errorp))
  (with-bounding-rectangle* (left top right bottom) child
    (recompute-extent-for-changed-child record child left top right bottom))
  ;;in case other things are still pointing to it.
  (setf (output-record-parent child) nil))

#-(or aclpc acl86win32) 
(defmethod delete-output-record :around (child (record output-record-mixin) &optional errorp)
  #-(or aclpc acl86win32) (declare (ignore errorp))
  (let ((stream (output-record-stream child)))
    (multiple-value-prog1
        (call-next-method)
      (when stream
        (note-output-record-detached child)))))

;; For people who roll their own from scratch
(defmethod note-output-record-detached ((record t))
  nil)

(defmethod note-output-record-detached ((record output-record-element-mixin))
  (setf (output-record-stream record) nil))

(defmethod note-output-record-detached :after ((record output-record-mixin))
  (map-over-output-records
   #'note-output-record-detached
   record))

;;; Recurse down all inferiors returning them to the "resource" table.
#+++ignore
(defmethod clear-output-record :before ((record output-record-mixin))
           (free-output-record record))

;;; Invoked by CLEAR-OUTPUT-RECORD.
#+++ignore
(defmethod free-output-record ((record output-record-element-mixin))
  (free-record record)
  (map-over-output-records #'free-output-record record))

#-(or aclpc acl86win32) 
(defmethod clear-output-record :after ((record output-record-mixin))
  (bounding-rectangle-set-edges record 
                                (coordinate 0) (coordinate 0)
                                (coordinate 0) (coordinate 0)))

#-(or aclpc acl86win32) 
(defmethod clear-output-record :around ((record output-record-mixin))
  (when (output-record-stream record)
    (map-over-output-records #'note-output-record-detached record))
  (call-next-method))


;;; Sequence output records store their children in a vector
(defclass standard-sequence-output-record
          (output-record-mixin output-record-element-mixin output-record)
    ((elements :initform nil)
     (fill-pointer :initform 0 :type fixnum)))

(define-output-record-constructor standard-sequence-output-record
                                  (&key x-position y-position (size 5))
  :x-position x-position :y-position y-position :size size)

(defmethod initialize-instance :after ((record standard-sequence-output-record) 
                                       &key (size 5))
  ;; probably want to save size away somewhere so that the
  ;; guy who actually makes the array can reference it...
  (declare (ignore size))
  ;; size defaults to very small to save space
  ;; most dependent classes will have supplied default-initargs with better
  ;; chosen default.
  (with-slots (elements fill-pointer) record
    ;; We run initialize-instance to re-initialize the record, so don't re-alloc the array
    (etypecase elements
      ((or null output-record displayed-output-record) (setf elements nil))
      (array (setf fill-pointer 0)))))

;;; For debugging.
(defmethod output-record-children ((record standard-sequence-output-record))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null nil)
      (array
        (let ((result (make-list fill-pointer)))
          (replace result elements :end1 fill-pointer :end2 fill-pointer)
          result))
      ;; It must be an OUTPUT-RECORD or a DISPLAYED-OUTPUT-RECORD
      (otherwise (list elements)))))

(defmethod output-record-element ((record standard-sequence-output-record) index)
  (with-slots (elements) record
    (typecase elements
      (null nil)
      (array (svref elements index))
      (otherwise
        (if (zerop index) elements nil)))))

(defmethod output-record-count ((record standard-sequence-output-record) &key fastp)
  (declare (ignore fastp))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null 0)
      (array fill-pointer)
      ;; It must be an OUTPUT-RECORD or a DISPLAYED-OUTPUT-RECORD
      (otherwise 1))))

(defmethod clear-output-record ((record standard-sequence-output-record))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null nil)
      (array (setf fill-pointer 0))
      ;; It must be an OUTPUT-RECORD or a DISPLAYED-OUTPUT-RECORD
      (otherwise (setf elements nil)))))

#+(or aclpc acl86win32)
(defmethod clear-output-record :after ((record output-record-mixin))
  (bounding-rectangle-set-edges record 
                                (coordinate 0) (coordinate 0)
                                (coordinate 0) (coordinate 0)))

#+(or aclpc acl86win32)
(defmethod clear-output-record :around ((record output-record-mixin))
  (when (output-record-stream record)
    (map-over-output-records #'note-output-record-detached record))
  (call-next-method))

(defmethod add-output-record (child (record standard-sequence-output-record))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null
        (setf elements child))
      (array
        (multiple-value-setq (elements fill-pointer)
          (simple-vector-push-extend child elements fill-pointer)))
      ;; It must be an OUTPUT-RECORD or a DISPLAYED-OUTPUT-RECORD
      (otherwise
        (let ((first elements))
          (setf elements (make-array 5))
          (setf fill-pointer 2)
          (setf (svref elements 0) first)
          (setf (svref elements 1) child))))))

#+(or aclpc acl86win32)
(defmethod add-output-record :after (child (record output-record-mixin)) ;was :around
  (setf (output-record-parent child) record)
  (recompute-extent-for-new-child record child)
  (when (output-record-stream record)
    (note-output-record-attached child (output-record-stream record))))

(defmethod delete-output-record 
    (child (record standard-sequence-output-record) &optional (errorp t))
  (flet ((not-found ()
           (when errorp
             (error "The output record ~S was not found in ~S" child record))))
    (with-slots (elements fill-pointer) record
      (typecase elements
        (null (not-found))
        (array
         (let ((index (position child elements :end fill-pointer)))
           (if index
               (let ((new-fp (the fixnum (1- fill-pointer)))
                        (vector elements))
                 (declare (type simple-vector vector) (fixnum new-fp))
                 (unless (= (the fixnum index) new-fp)
                   ;; Shift the whole vector downward
                   (do ((i (the fixnum index) (1+ i)))
                       ((= i new-fp))
                     (declare (type fixnum i)
                              (optimize (speed 3) (safety 0)))
                     (setf (svref vector i) (svref vector (1+ i)))))
                 (setf fill-pointer new-fp))
             (not-found))))
        ;; It must be an OUTPUT-RECORD or a DISPLAYED-OUTPUT-RECORD
        (otherwise
         (if (eq elements child)
             (setf elements nil)
           (not-found))))))
  t)

#+(or aclpc acl86win32)
(defmethod delete-output-record :after
           (child (record output-record-mixin) &optional (errorp t))
  (declare (ignore errorp))
  (with-bounding-rectangle* (left top right bottom) child
    (recompute-extent-for-changed-child record child left top right bottom))
  ;;in case other things are still pointing to it.
  (setf (output-record-parent child) nil))

#+(or aclpc acl86win32)
(defmethod delete-output-record :around (child (record output-record-mixin) &optional errorp)
  (declare (ignore errorp))
  (let ((stream (output-record-stream child)))
    (multiple-value-prog1
        (call-next-method)
      (when stream
        (note-output-record-detached child)))))

(defmethod map-over-output-records-overlapping-region
           (function (record standard-sequence-output-record) region  
            &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (declare (optimize (safety 0)))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null nil)
      (array
        (if (or (null region) (eq region +everywhere+))
            (dovector (child elements :start 0 :end fill-pointer :simple-p t)
              (apply function child continuation-args))
          (with-bounding-rectangle* (left1 top1 right1 bottom1) region
            (translate-coordinates (coordinate x-offset) (coordinate y-offset) 
              left1 top1 right1 bottom1)
            ;; Subtract out the record offset from the region, to make comparison fair
            (multiple-value-bind (xoff yoff)
                (output-record-position record)
              (translate-coordinates (- xoff) (- yoff) left1 top1 right1 bottom1))
            (dovector (child elements :start 0 :end fill-pointer :simple-p t)
              (with-bounding-rectangle* (left2 top2 right2 bottom2) child
                (when (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
                                            left2 top2 right2 bottom2)
                  (apply function child continuation-args)))))))
      (otherwise
        (if (or (null region) (eq region +everywhere+))
            (apply function elements continuation-args)
          (multiple-value-bind (xoff yoff)
              (output-record-position record)
            (declare (type coordinate xoff yoff))
            (when (region-intersects-offset-region-p
                    elements region 
                    (- (coordinate x-offset) xoff) (- (coordinate y-offset) yoff))
              (apply function elements continuation-args)))))))
  nil)

(defmethod map-over-output-records-containing-position
           (function (record standard-sequence-output-record) x y 
            &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (declare (optimize (safety 0)))
  (translate-coordinates (coordinate x-offset) (coordinate y-offset) x y)
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null nil)
      (array
        (multiple-value-bind (xoff yoff)
            (output-record-position record)
          (translate-coordinates (- xoff) (- yoff) x y))
        (dovector (child elements :start 0 :end fill-pointer :from-end t :simple-p t)
          (with-bounding-rectangle* (left top right bottom) child
            (when (ltrb-contains-position-p left top right bottom x y)
              (apply function child continuation-args)))))
      (otherwise
        (multiple-value-bind (xoff yoff) (output-record-position record)
          (when (offset-region-contains-position-p elements xoff yoff x y)
            (apply function elements continuation-args))))))
  nil)


;;; Mix this in for top-level output histories
(defclass stream-output-history-mixin () ((stream)))

(defclass standard-sequence-output-history
          (stream-output-history-mixin standard-sequence-output-record)
    ())

(defmethod bounding-rectangle-set-edges ((record stream-output-history-mixin)
                                         nleft ntop nright nbottom)
  #+++ignore (assert (<= nleft nright))
  #+++ignore (assert (<= ntop  nbottom))
  (with-slots (left top right bottom parent stream) record
    ;; Top-level output records must not have their upper left corner any
    ;; "later" than (0,0), or else scroll bars and scrolling will not do
    ;; the right thing.
    (setq left (min (coordinate nleft) (coordinate 0))
          top  (min (coordinate ntop)  (coordinate 0))
          right  (coordinate nright)
          bottom (coordinate nbottom)))
  record)

(defmethod bounding-rectangle-set-edges :around ((record stream-output-history-mixin) 
                                                 nleft ntop nright nbottom)
  (let ((nleft (coordinate nleft))
        (ntop  (coordinate ntop))
        (nright  (coordinate nright))
        (nbottom (coordinate nbottom)))
    (with-bounding-rectangle* (left top right bottom) record
      (call-next-method)
      (unless (and (= left nleft)
                   (= top  ntop)
                   (= right  nright)
                   (= bottom nbottom))
        ;; This should update the scroll bars, etc 
        (let* ((stream (output-record-stream record))
               (viewport (pane-viewport stream)))
          (when viewport
            (update-scroll-bars viewport)
            (update-region stream nleft ntop nright nbottom)))))))

;;; DEFCLASS of OUTPUT-RECORDING-MIXIN, etc. is in STREAM-CLASS-DEFS
(defmethod initialize-instance :after ((stream output-recording-mixin) &rest args)
  (declare (ignore args))
  (with-slots (output-record) stream
    ;;--- Our OUTPUT-RECORDING-MIXIN expects extended output...
    (multiple-value-bind (x y) (stream-cursor-position stream)
      ;; I don't understand why the output record's initial position was set to
      ;; some untransformed "viewport" coordinate.  The cursor position is the
      ;; right place, no?
      (output-record-set-position output-record x y))
    (when (typep output-record 'stream-output-history-mixin)
      (setf (slot-value output-record 'stream) stream))))


;(defmethod (setf stream-output-history) ((history stream-output-history-mixin)
;                                         (stream output-recording-mixin))
;  (let ((old (stream-output-history stream)))
;    (when old (note-output-record-detached old)))
;  (call-next-method)
;  ;;--- Our OUTPUT-RECORDING-MIXIN expects extended output...
;  (multiple-value-bind (x y) (stream-cursor-position stream)
;    ;; I don't understand why the output record's initial position was set to
;    ;; some untransformed "viewport" coordinate.  The cursor position is the
;    ;; right place, no?
;    (output-record-set-position history x y))
;  ;;
;  (setf (slot-value history 'stream) stream)
;  history)

(defmethod clear-output-history ((stream output-recording-mixin))
  (when (stream-output-history stream)
    (clear-output-record (stream-output-history stream)))
  (setf (stream-text-output-record stream) nil)
  (setf (stream-highlighted-presentation stream) nil))

(defmethod stream-add-output-record ((stream output-recording-mixin) record)
  (with-slots (output-record current-output-record-stack) stream
    (let ((the-output-record (or current-output-record-stack output-record)))
     (add-output-record record the-output-record)
     #+++ignore                ;--- the output history already does this, we think
     (let ((width (bounding-rectangle-width stream))
           (height (bounding-rectangle-height stream)))
       (declare (type coordinate width height))
       (with-bounding-rectangle* (rl rt rr rb) the-output-record
         (when (or (< rl 0) (< width rr)
                   (< rt 0) (< height rb))
           (update-region stream rl rt rr rb)))))))

(defmethod stream-replay ((stream output-recording-mixin) &optional region)
  (when (stream-drawing-p stream)
    (with-slots (output-record text-output-record record-p) stream
      (when (or output-record text-output-record)
        (setq region (normalize-replay-region region stream))
        (with-drawing-options (stream :clipping-region region)
          (letf-globally ((record-p nil))
            (when output-record
              (replay output-record stream region))
            (when text-output-record
              (replay text-output-record stream region))))))))

(defmethod erase-output-record (record (stream output-recording-mixin) &optional (errorp t))
  (macrolet ((draw-it ()
               `(if (or (= left right) (= top bottom))
                    ;; Handle specially, since a thin line is wider than a
                    ;; rectangle of zero width or height
                    (draw-line-internal stream xoff yoff
                                        left top right bottom
                                        +background-ink+ nil)
                  (draw-rectangle-internal stream xoff yoff
                                           left top 
                                           #-(or aclpc acl86win32) right
                                           #+(or aclpc acl86win32)
                                           (if (let () 
                                                 (declare (special *editting-field-p*))
                                                 ;;mm: defined in accept-v.lsp later
                                                 *editting-field-p*)
                                               (+ right 6) right)
                                           bottom
                                           +background-ink+ nil))))
    (if (listp record)
        (let ((replay-region +nowhere+))
          (with-output-recording-options (stream :record nil)
            (dolist (record record)
              (let ((parent (output-record-parent record)))
                (multiple-value-bind (xoff yoff)
                    (convert-from-relative-to-absolute-coordinates stream parent)
                  (with-bounding-rectangle* (left top right bottom) record
                    (draw-it)
                    (when parent
                      (delete-output-record record parent errorp))
                    (translate-coordinates xoff yoff left top right bottom)
		    #-ignore ;; may be faster to send one big rectangle
		    (if (eq replay-region +nowhere+)
			(setq replay-region
			  (make-bounding-rectangle left top right bottom))
		      (let ((lf2 left) (tp2 top) (rt2 right) (bt2 bottom))
			(with-slots (left top right bottom) replay-region
			  (minf left lf2) (minf top tp2)
			  (maxf right rt2) (maxf bottom bt2))))
		    #+ignore
                    (setq replay-region
                      (region-union
                       replay-region
                       (make-bounding-rectangle left top right bottom))))))))
          (frame-replay *application-frame* stream replay-region))
      (let ((parent (output-record-parent record)))
        (multiple-value-bind (xoff yoff)
            (convert-from-relative-to-absolute-coordinates stream parent)
          (with-bounding-rectangle* (left top right bottom) record
            (with-output-recording-options (stream :record nil)
              (draw-it))))
        (when parent
          (delete-output-record record parent errorp))
        ;; Use the output record itself as the replay region, and replay
        ;; the stuff that might have been obscured by the erased output
        (frame-replay *application-frame* stream record)))))

(defmethod invoke-with-output-recording-options ((stream output-recording-mixin)
                                                 continuation record draw)
  (letf-globally (((stream-recording-p stream) record)
                  ((stream-drawing-p stream) draw))
    (funcall continuation)))

;;; The following two are only called when STREAM-RECORDING-P is true and the
;;; characters are printable (see CHARACTER-DRAWING.LISP).
(defmethod stream-add-string-output ((stream output-recording-mixin) string
                                     start end text-style width height baseline)
  (declare (type fixnum start end))
  (when (< start end)
    (let ((record (get-text-output-record stream text-style)))
      (add-string-output-to-text-record record string start end text-style
                                        width height baseline))))

(defmethod stream-add-character-output ((stream output-recording-mixin) character
                                        text-style width height baseline)
  (let ((record (get-text-output-record stream text-style)))
    (add-character-output-to-text-record record character text-style
                                         width height baseline)))

(defmethod stream-close-text-output-record ((stream output-recording-mixin)
                                            &optional wrapped)
  ;; It's faster to access the slot directly instead of going through 
  ;; STREAM-TEXT-OUTPUT-RECORD
  (let ((text-record (slot-value stream 'text-output-record)))
    (when text-record
      (when wrapped
        (setf (slot-value text-record 'wrapped-p) t))
      (stream-add-output-record stream text-record)
      (when (stream-redisplaying-p stream)
        (recompute-contents-ok text-record))
      (setf (slot-value stream 'text-output-record) nil))))

(defmethod stream-force-output :after ((stream output-recording-mixin))
  (stream-close-text-output-record stream))

(defmethod stream-finish-output :after ((stream output-recording-mixin))
  (stream-close-text-output-record stream))

;; When setting cursor position, have to dump old text record.
;; This is necessary in order to capture the correct cursor position in
;; text output records.  If we did not close the current text record,
;; a sequence such as WRITE-STRING/SET-CURSORPOS/WRITE-STRING would
;; create only a single output record, and intervening whitespace would
;; be lost if the two WRITE-STRINGs took place on the same line.
(defmethod stream-set-cursor-position :before ((stream output-recording-mixin) x y)
  (declare (ignore x y))
  (stream-close-text-output-record stream))

;; This gets used to reposition the cursor when drawing text.  We need to
;; close the text output record when there was a line wrap, but not when
;; we are simply incrementing the cursor beyond the just-written glyph.
(defmethod stream-set-cursor-position-internal :before ((stream output-recording-mixin) x y)
  (declare (ignore x))
  (multiple-value-bind (old-x old-y) (stream-cursor-position stream)
    (declare (ignore old-x))
    (unless (eq y old-y)
      (stream-close-text-output-record stream))))

;;; This method should cover a multitude of sins.
(defmethod handle-repaint :after ((stream output-recording-mixin) region)
  (let ((clear (region-intersection region
                                    (or (pane-viewport-region stream)
                                        (sheet-region stream)))))
    (unless (eq clear +nowhere+)
      (with-sheet-medium (medium stream)
        (with-bounding-rectangle* (left top right bottom) clear
          (medium-clear-area medium left top right bottom)))))
  (stream-replay stream region)
  (let ((presentation (highlighted-presentation stream nil)))
    (when presentation
      (highlight-presentation 
        presentation (presentation-type presentation) stream :highlight))))


;;; Genera compatibility

#+Genera
(defmethod stream-compatible-output-as-presentation
           ((stream output-recording-mixin)
            continuation xstream
            &key (object nil) (type t) single-box &allow-other-keys)
  (dw:with-type-decoded (type-name nil pr-args) type
    (if (or (null type)
            (and (eq type-name 'sys:expression)
                 (not (getf pr-args :escape *print-escape*))
                 (stringp object)))
        (funcall continuation xstream)
        (multiple-value-bind (object clim-type changed-p)
            (dw-type-to-clim-type object type)
          (if changed-p
              (with-output-as-presentation (xstream object clim-type
                                            :single-box single-box)
                (funcall continuation xstream))
              (funcall continuation xstream))))))

#+Genera
(defmethod stream-compatible-output-as-presentation-1
           ((stream output-recording-mixin)
            continuation continuation-args
            &key (object nil) (type t) single-box &allow-other-keys)
  (dw:with-type-decoded (type-name nil pr-args) type
    (if (or (null type)
            (and (eq type-name 'sys:expression)
                 (not (getf pr-args :escape *print-escape*))
                 (stringp object)))
        (apply continuation continuation-args)
        (multiple-value-bind (object clim-type changed-p)
            (dw-type-to-clim-type object type)
          (if changed-p
              (with-output-as-presentation (stream object clim-type
                                            :single-box single-box)
                (apply continuation continuation-args))
              (apply continuation continuation-args))))))


