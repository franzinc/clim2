;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: recording-protocol.lisp,v 1.1 92/02/24 13:18:28 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;; The protocol class for an object that obeys the output record protocol,
;; that is, can hold output record elements
(define-protocol-class output-record (bounding-rectangle))

;; The protocol class for output records that are leaves.
(define-protocol-class displayed-output-record (bounding-rectangle))

;; The protocol class for textual displayed output records.
(define-protocol-class text-displayed-output-record (displayed-output-record))

;; The protocol class for graphical displayed output records.
(define-protocol-class graphics-displayed-output-record (displayed-output-record))


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
;;;  map-over-output-records-containing-point*:
;;;   (function record x y
;;;    &optional (x-offset 0) (y-offset 0) &rest continuation-args)

;;; The incremental redisplay protocol:
;;;  see file incremental-redisplay-protocol.text, and incremental-redisplay.lisp.
;;;

;; Bounding rectangle position and set-position are in relative coordinates, 
;;   relative to (OUTPUT-RECORD-POSITION* (OUTPUT-RECORD-PARENT RECORD)).
;; The bounding rectangle measures just the ink.
;; (OUTPUT-RECORD-START-CURSOR-POSITION* RECORD) refers to the position of the
;; cursor at the start of RECORD.  It is also the origin of the
;; coordinate system for all children of RECORD.
(defclass output-record-element-mixin (standard-bounding-rectangle)
    ;; start position is relative to the parent's start-position
    ;; start position is where the cursor was when the new
    ;; output-record was started.
    ((start-x :initform 0 :initarg :start-x)
     (start-y :initform 0 :initarg :start-y)
     ;; end position is relative to start position.
     ;; end position is where the cursor was when we finished the
     ;; output-record.
     (end-x :initform 0 :initarg :end-x)
     (end-y :initform 0 :initarg :end-y)
     ;; old-start-position is relative to old-start-position of parent
     (old-start-x :initform 0)
     (old-start-y :initform 0)
     ;; old bounding rectangle is relative to parents' old-start-position.
     (old-bounding-rectangle :initform nil
			     :accessor output-record-old-bounding-rectangle)
     (contents-ok :initform nil :accessor output-record-contents-ok)
     (parent :accessor output-record-parent :initarg :parent)
     (stream :initform nil :accessor output-record-stream))
  (:default-initargs :parent nil :left 0 :top 0 :right 0 :bottom 0))

;;; Give initial rectangle of 0 size, it will get expanded as children are added.
(defmethod initialize-instance :after ((record output-record-element-mixin)
				       &key (x-position 0) (y-position 0))
  (with-slots (left top right bottom) record
    (setf left x-position
	  top  y-position
	  right  x-position
	  bottom y-position)))

;;; Shadow the method on RECTANGLE with this one that keeps the start-position and 
;;; bounding rectangle in synch.
(defmethod bounding-rectangle-set-position* ((record output-record-element-mixin) nx ny)
  (declare (type coordinate nx ny))
  (with-slots (left top right bottom start-x start-y parent) record
    (declare (type coordinate left top right bottom start-x start-y))
    ;; Move the start position by as much as we do the record.
    (setq start-x (+ start-x (- nx left)))
    (setq start-y (+ start-y (- ny top)))
    (let ((width (- right left))
	  (height (- bottom top)))
      (setf left nx top ny)
      (setf right  (+ nx width)
	    bottom (+ ny height)))))

#+ignore
(defmethod bounding-rectangle-set-position* :around ((record output-record-element-mixin) nx ny)
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (call-next-method)
    (note-output-record-moved record (- nx x1) (- ny y1) (- nx x2) (- ny y2))))

#+ignore
(defmethod bounding-rectangle-set-edges :around ((record output-record-element-mixin) 
						 left top right bottom)
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (call-next-method)
    (note-output-record-moved record (- left x1) (- top y1) (- right x2) (- bottom y2))))

#+Silica
(defmethod note-output-record-moved ((record output-record-element-mixin) dx1 dy1 dx2 dy2)
  (declare (ignore dx1 dy1 dx2 dy2))
  nil)

(defun-inline output-record-position* (record)
  (output-record-start-cursor-position* record))

;; X and Y had better be fixnums
;;--- Coerce to COORDINATE
(defmethod output-record-set-position* ((record output-record-element-mixin) x y)
  (declare (type coordinate x y))
  (bounding-rectangle-set-position* record x y))

(defmethod output-record-start-cursor-position ((record output-record-element-mixin))
  (with-slots (start-x start-y) record
    (make-point start-x start-y)))

(defmethod output-record-start-cursor-position* ((record output-record-element-mixin))
  (with-slots (start-x start-y) record
    (values start-x start-y)))

;;; Keep the start-position and bounding rectangle in synch
(defmethod output-record-set-start-cursor-position*
	   ((record output-record-element-mixin) nx ny)
  (declare (type coordinate nx ny))
  (with-slots (start-x start-y) record
    (declare (type coordinate start-x start-y))
    (with-bounding-rectangle* (left top right bottom) record
      (let ((dx (- nx start-x))
	    (dy (- ny start-y)))
	(bounding-rectangle-set-edges 
	  record
	  (+ left dx) (+ top dy) (+ right dx) (+ bottom dy))
	(setf start-x nx start-y ny)))))

(defmethod output-record-end-cursor-position* ((record output-record-element-mixin))
  (with-slots (end-x end-y) record
    (values end-x end-y)))
  
(defmethod output-record-set-end-cursor-position* ((record output-record-element-mixin) nx ny)
  (declare (type coordinate nx ny))
  (with-slots (end-x end-y) record
    (setf end-x nx)
    (setf end-y ny)))

(defmethod output-record-old-start-cursor-position ((record output-record-element-mixin))
  (with-slots (old-start-x old-start-y) record
    (make-point old-start-x old-start-y)))

(defmethod output-record-old-start-cursor-position* ((record output-record-element-mixin))
  (with-slots (old-start-x old-start-y) record
    (values old-start-x old-start-y)))

(defmethod output-record-set-old-start-cursor-position*
	   ((record output-record-element-mixin) nx ny)
  (declare (type coordinate nx ny))
  (with-slots (old-start-x old-start-y) record
    (setf old-start-x nx)
    (setf old-start-y ny)))

#+CLIM-1-compatibility
(progn
(define-compatibility-function (output-record-start-position
				output-record-start-cursor-position)
			       (record)
  (output-record-start-cursor-position record))

(define-compatibility-function (output-record-start-position*
				output-record-start-cursor-position*)
			       (record)
  (output-record-start-cursor-position* record))

(define-compatibility-function (output-record-set-start-position*
				output-record-set-start-cursor-position*)
			       (record nx ny)
  (output-record-set-start-cursor-position* record nx ny))

(define-compatibility-function (output-record-end-position*
				output-record-end-cursor-position*)
			       (record)
  (output-record-end-cursor-position* record))

(define-compatibility-function (output-record-set-end-position*
				output-record-set-end-cursor-position*)
			       (record nx ny)
  (output-record-set-end-cursor-position* record nx ny))
)	;#+CLIM-1-compatibility


(defmethod output-record-children ((record output-record-element-mixin))
  nil)

;;; For specialization by PRESENTATIONs, for example
(defmethod output-record-refined-sensitivity-test ((record output-record-element-mixin) x y)
  (declare (ignore x y))
  T)

(defun compute-output-record-offsets (record)
  (let ((parent (output-record-parent record)))
    (if (null parent)
	(values 0 0)
      (multiple-value-bind (x y)
	  (compute-output-record-offsets parent)
	(declare (type coordinate x y))
	(multiple-value-bind (our-x our-y) (output-record-position* record)
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
	    (translate-fixnum-positions xoff1 yoff1 left1 top1 right1 bottom1)
	    (translate-fixnum-positions xoff2 yoff2 left2 top2 right2 bottom2)
	    (ltrb-equals-ltrb-p left1 top1 right1 bottom1
				left2 top2 right2 bottom2)))))))

(defmethod region-contains-point*-p
	   ((record output-record-element-mixin) x y)
  (with-bounding-rectangle* (left top right bottom) record
    (multiple-value-bind (xoff yoff) (compute-output-record-offsets record)
      (declare (type coordinate xoff yoff))
      (ltrb-contains-point*-p left top right bottom
			      (+ x xoff) (+ y yoff)))))

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
	    (translate-fixnum-positions xoff1 yoff1 left1 top1 right1 bottom1)
	    (translate-fixnum-positions xoff2 yoff2 left2 top2 right2 bottom2)
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
	    (translate-fixnum-positions xoff1 yoff1 left1 top1 right1 bottom1)
	    (translate-fixnum-positions xoff2 yoff2 left2 top2 right2 bottom2)
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

(defun offset-region-contains-point*-p (region xoff yoff x y)
  (declare (type coordinate xoff yoff x y))
   (with-bounding-rectangle* (left top right bottom) region
     (ltrb-contains-point*-p (+ left xoff) (+ top yoff) (+ right xoff) (+ bottom yoff)
			     x y)))

;;; This maps over all of the children of the record
#+Genera (zwei:defindentation (map-over-output-records 1 1))
(defun map-over-output-records (function record
				&optional (x-offset 0) (y-offset 0)
				&rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (apply #'map-over-output-records-overlapping-region
	 function record nil x-offset y-offset continuation-args))

#+CLIM-1-compatibility
(define-compatibility-function (map-over-output-record-elements
				map-over-output-records)
			       (record function
				&optional x-offset y-offset &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (apply #'map-over-output-records
	 function record x-offset y-offset continuation-args))

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

#+CLIM-1-compatibility
(define-compatibility-function (map-over-output-record-elements-overlapping-region
				map-over-output-records-overlapping-region)
			       (record region function
				&optional x-offset y-offset &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (apply #'map-over-output-records-overlapping-region
	 function record region x-offset y-offset continuation-args))

;;; This must map over the children in such a way that, when it maps over
;;; overlapping children, the topmost (most recently inserted) child is
;;; hit first, that is, the opposite order of MAP-...-OVERLAPPING-REGION.
;;; This is because this function is used for things like locating the
;;; presentation under the pointer, where the topmost thing wants to be
;;; located first.
#+Genera (zwei:defindentation (map-over-output-records-containing-point* 3 1))
(defgeneric map-over-output-records-containing-point*
	    (function record x y
	     &optional x-offset y-offset &rest continuation-args)
  (declare (dynamic-extent function continuation-args)))

#+CLIM-1-compatibility
(define-compatibility-function (map-over-output-record-elements-containing-point*
				map-over-output-records-containing-point*)
			       (record x y function
				&optional x-offset y-offset &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (apply #'map-over-output-records-containing-point*
	 function record x y x-offset y-offset continuation-args))

;;; X-offset and Y-offset represent the accumulated offset between the
;;; regions's native coordinates and "our" coordinates and must be added
;;; to our local coordinates (or subtracted from the region, if
;;; possible) in order to validly compare them.
;;; 
;;; In the absence of x- and y- offsets, region should be in the
;;; coordinate system of the record - i.e. relative to 
;;;  (OUTPUT-RECORD-POSITION* RECORD).
;;; This is the same coordinate system as the output record children
;;; we are mapping over.
(defmethod map-over-output-records-overlapping-region
	   (function (record output-record-element-mixin) region
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (ignore region function x-offset y-offset continuation-args)
	   (dynamic-extent function continuation-args))
  nil)

(defmethod map-over-output-records-containing-point*
	   (function (record output-record-element-mixin) x y
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (ignore x y function x-offset y-offset continuation-args)
	   (dynamic-extent function continuation-args))
  nil)

(defmethod map-over-output-records-containing-point*
	   (function (record t) x y
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (apply #'map-over-output-records-overlapping-region
	 function record (make-point x y)
	 x-offset y-offset continuation-args))


;; A mix-in for classes that can store other output records
(defclass output-record-mixin ()
     ;; OLD-CHILDREN is the list of >unmatched< output records from last
     ;; redisplay pass.  if you implement your own FIND-INFERIOR-OUTPUT-RECORD,
     ;; and iff you match from OLD-CHILDREN, you are required to remove the
     ;; match from OLD-CHILDREN, probably by using DECACHE-INFERIOR-OUTPUT-RECORD.
     ((old-children :initform nil :accessor output-record-old-children)
      (generation-tick :initform 0 :initarg :generation-tick
		       :accessor output-record-generation-tick)))

(defmethod inferiors-never-overlap-p ((record output-record-mixin)) nil)

#+Silica
(defmethod note-output-record-moved :after ((record output-record-mixin) dx1 dy1 dx2 dy2)
  (unless (= 0 dx1 dy1 dx2 dy2)
    (map-over-output-records
     #'(lambda (child)
	 (note-output-record-moved child dx1 dy1 dx2 dy2))
     record)))

#+Silica
(defmethod bounding-rectangle-set-position* :around ((record output-record-mixin) nx ny)
  (multiple-value-bind (ox oy)
      (output-record-position* record)
    (call-next-method)
    (when (or (/= ox nx) (/= oy ny))
      (note-output-record-moved record 1 1 1 1))))

#+Silica
(defmethod bounding-rectangle-set-edges :around ((record output-record-mixin) 
						 left top right bottom)
  (declare (ignore  left top right bottom))
  (multiple-value-bind (ox oy)
      (output-record-position* record)
    (call-next-method)
    (multiple-value-bind (nx ny)
	(output-record-position* record)
      (when (or (/= ox nx) (/= oy ny))
	(note-output-record-moved record 1 1 1 1)))))


;; If some coordinate is relative to a given output-record, then
;; CONVERT-FROM-RELATIVE-TO-ABSOLUTE-COORDINATES returns an x,y offset
;; to be ADDED to any coordinates relative to OUTPUT-RECORD to give you
;; absolute coordinates.
(defun convert-from-relative-to-absolute-coordinates (stream output-record)
  (declare (values x-offset y-offset))
  (cond ((null output-record)
	 (values 0 0)) ;;---------------------------- Why?
	((and stream
	      (eql output-record (stream-current-output-record stream)))
	 (let ((position (stream-output-history-position stream)))
	   (values (point-x position) (point-y position))))
	((null (output-record-parent output-record))
	 (values 0 0))
	(t
	 (multiple-value-bind (x y)
	     (convert-from-relative-to-absolute-coordinates
	       stream (output-record-parent output-record))
	   (declare (type coordinate x y))
	   (multiple-value-bind (our-x our-y) (output-record-position* output-record)
	     (declare (type coordinate our-x our-y))
	     (values (+ our-x x) (+ our-y y)))))))

;; If some coordinate is in absolute coordinates, then
;; CONVERT-FROM-ABSOLUTE-TO-RELATIVE-COORDINATES returns an x,y offset
;; to be ADDED to any absolute coordinates to give you coordinates
;; relative to OUTPUT-RECORD.
(defun convert-from-absolute-to-relative-coordinates (stream output-record)
  (declare (values x-offset y-offset))
  (cond ((eql output-record (stream-current-output-record stream))
	 (let ((position (stream-output-history-position stream)))
	   (values (- (point-x position)) (- (point-y position)))))
	((null (output-record-parent output-record))
	 (values 0 0))
	(t
	 (multiple-value-bind (x y)
	     (convert-from-absolute-to-relative-coordinates
	       stream (output-record-parent output-record))
	   (declare (type coordinate x y))
	   (multiple-value-bind (our-x our-y) (output-record-position* output-record)
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
  (cond ((eql descendant ancestor)
	 (values 0 0))
	((null descendant)
	 (error "~S was not an ancestor of ~S" ancestor descendant))
	(t
	 (multiple-value-bind (x y)
	     (convert-from-ancestor-to-descendant-coordinates
	       ancestor (output-record-parent descendant))
	   (declare (type coordinate x y))
	   (multiple-value-bind (our-x our-y) (output-record-position* descendant)
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
  (cond ((eql descendant ancestor)
	 (values 0 0))
	((null descendant)
	 (error "~s was not an ancestor of ~s" ancestor descendant))
	(t
	 (multiple-value-bind (x y)
	     (convert-from-descendant-to-ancestor-coordinates
	       (output-record-parent descendant) ancestor)
	   (declare (type coordinate x y))
	   (multiple-value-bind (our-x our-y) (output-record-position* descendant)
	     (declare (type coordinate our-x our-y))
	     (values (+ our-x x) (+ our-y y)))))))


(defun with-output-record-1 (continuation stream record &optional abs-x abs-y)
  ;; Close the text record before and after, 
  (stream-close-text-output-record stream)
  (let ((current-output-position
	  (stream-output-history-position stream)))
    (unless abs-y
      (multiple-value-setq (abs-x abs-y)
	(stream-cursor-position* stream)))
    (letf-globally (((point-x current-output-position) abs-x)
		    ((point-y current-output-position) abs-y)
		    ((stream-current-output-record stream) record))
      (funcall continuation record)
      (multiple-value-bind (end-x end-y)
	  (stream-cursor-position* stream)
	(declare (type coordinate end-x end-y))
	(output-record-set-end-cursor-position*
	  record (- end-x abs-x) (- end-y abs-y)))
      (stream-close-text-output-record stream))))

;;; Rest of stuff started in clim-defs...
(defun construct-output-record-1 (type &rest init-args)
  (declare (dynamic-extent init-args))
  (let ((constructor (gethash type *output-record-constructor-cache*)))
    (if constructor
	(apply constructor init-args)
	(apply #'make-instance type init-args))))

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

(defun invoke-with-new-output-record (stream continuation record-type constructor
				      &rest init-args &key parent &allow-other-keys)
  (declare (dynamic-extent init-args))
  (with-keywords-removed (init-args init-args '(:parent))
    (let* ((current-output-record (stream-current-output-record stream))
	   (new-output-record (and (stream-redisplaying-p stream)
				   current-output-record
				   (apply #'find-inferior-output-record-1
					  current-output-record record-type init-args))))
      (multiple-value-bind (cursor-x cursor-y)
	  (stream-cursor-position* stream)
	(declare (type coordinate cursor-x cursor-y))
	(multiple-value-bind (x y)
	    (multiple-value-bind (px py)
		(point-position*
		  (stream-output-history-position stream))
	      (declare (type coordinate px py))
	      (position-difference* cursor-x cursor-y px py))
	  (declare (type coordinate x y))
	  (if new-output-record
	      (copy-display-state new-output-record nil)
	      (setq new-output-record
		    ;;--- Used to call ALLOCATE-RECORD, then initialize by
		    ;;--- setting the edges (or INITIALIZE-INSTANCE)
		    (if constructor
			(apply constructor
			       :x-position x :y-position y init-args)
			(apply #'construct-output-record-1 record-type
			       :x-position x :y-position y init-args))))
	  (output-record-set-start-cursor-position* new-output-record x y)
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

(defun invoke-with-room-for-graphics (stream continuation record-type move-cursor &key height)
  (let ((record
	  (with-output-recording-options (stream :draw nil :record t)
	    (with-first-quadrant-coordinates (stream)
	      (with-new-output-record (stream record-type)
		(funcall continuation stream))))))
    (multiple-value-bind (x y) (output-record-position* record)
      (declare (type coordinate x y))
      ;;--- Hey, there is something wierd going on here.  The problem is that
      ;;--- OUTPUT-RECORD-POSITION* and OUTPUT-RECORD-SET-POSITION* seem to obey
      ;;--- different coordinate system conventions.  Geez.
      (when height
	(incf y (- height (bounding-rectangle-height record))))
      (output-record-set-position* record x y))
    (tree-recompute-extent record)
    (replay record stream)
    (when move-cursor
      (move-cursor-beyond-output-record stream record))
    record))

(defun replay (record stream &optional region)
  (when (stream-drawing-p stream)
    (multiple-value-bind (x-offset y-offset)
	(convert-from-relative-to-absolute-coordinates stream (output-record-parent record))
      ;; Output recording should be off, but let's be forgiving...
      (letf-globally (((stream-recording-p stream) nil))
	(replay-output-record record stream region x-offset y-offset)))))

;; Replay all the the inferiors of RECORD that overlap REGION.
(defmethod replay-output-record ((record output-record-mixin) stream
				 &optional region (x-offset 0) (y-offset 0))
  (declare (type coordinate x-offset y-offset))
  ;;--- Doing things this way bypasses any REPLAY-OUTPUT-RECORD methods supplied on 
  ;;--- non-standard classes that satisfy the output record protocol.
  ;;--- Too bad, this relative coordinates stuff is a disaster anyway.
  (labels ((replay-1 (record x-offset y-offset)
	     (declare (type coordinate x-offset y-offset))
	     (if (output-record-p record)
		 (multiple-value-bind (xoff yoff) (output-record-position* record)
		   (map-over-output-records-overlapping-region 
		     #'replay-1 record region 
		     (- x-offset) (- y-offset)
		     (+ x-offset xoff) (+ y-offset yoff)))
		 (replay-output-record record stream region x-offset y-offset))))
    (declare (dynamic-extent #'replay-1))
    (replay-1 record x-offset y-offset)))

(defun move-cursor-beyond-output-record (stream record)
  (multiple-value-bind (x-offset y-offset)
      (convert-from-relative-to-absolute-coordinates
	stream (output-record-parent record))
    (declare (type coordinate x-offset y-offset))
    (with-bounding-rectangle* (left top right bottom) record
      (declare (ignore left top))
      (with-end-of-page-action (stream :allow)
	(stream-set-cursor-position*
	  stream
	  (+ right x-offset) (- (+ bottom y-offset) (stream-line-height stream)))))))


(defmethod recompute-extent-for-changed-child ((record output-record-mixin) child
					       old-left old-top old-right old-bottom)
  (declare (type coordinate old-top old-right old-bottom))
  ;; old edges are passed in parent's coordinate system because
  ;; their reference point may have changed.
  ;; (assert (child-completely-contained-within-extent-of record child))
  (with-slots (parent) record
    (with-bounding-rectangle* (left top right bottom) record
      ;; We must recompute the extent if the child is not completely contained
      ;; or if it used to "define" one of the old edges.
      ;; A picture would help, but we're not going to draw it here. (:-)
      (multiple-value-bind (xoff yoff)
	  (convert-from-descendant-to-ancestor-coordinates record parent)
	(when (or (not (region-contains-offset-region-p record child xoff yoff))
		  (= old-left left)
		  (= old-top top)
		  (= old-right right)
		  (= old-bottom bottom))
	  (recompute-extent record))))))

(defmethod recompute-extent ((record output-record-mixin))
  (with-slots (parent) record
    (with-bounding-rectangle* (old-left old-top old-right old-bottom) record
      (let ((once nil)
	    (min-x 0) (min-y 0) (max-x 0) (max-y 0))
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
	    (convert-from-descendant-to-ancestor-coordinates record parent)
	  (declare (type coordinate xoff yoff))
	  (if once
	      (progn (assert (ltrb-well-formed-p min-x min-y max-x max-y))
		     (bounding-rectangle-set-edges
		       record
		       (+ min-x xoff) (+ min-y yoff)
		       (+ max-x xoff) (+ max-y yoff)))
	      ;; No children
	      (bounding-rectangle-set-edges record 0 0 0 0))
	  ;; Pass these coordinates in parent's coordinate system (I think)
	  (translate-fixnum-positions xoff yoff
	    old-left old-top old-right old-bottom))
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
	      (convert-from-descendant-to-ancestor-coordinates record parent)
	    (translate-fixnum-positions xoff yoff
	      eleft etop eright ebottom
	      ;; pass these coordinates in parent's coordinate system.
	      old-left old-top old-right old-bottom))
	  (cond ((= (output-record-count record) 1)
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
      (multiple-value-bind (xoff yoff)
	  (convert-from-descendant-to-ancestor-coordinates record parent)
	(declare (type coordinate xoff yoff))
	;; we must pass the old coordinates in the parent's coordinate system
	;; because tree-recompute-extent-1 may adjust the reference point.
	(translate-fixnum-positions xoff yoff old-left old-top old-right old-bottom))
      (tree-recompute-extent-1 record)
      (when parent
	(recompute-extent-for-changed-child
	  parent record old-left old-top old-right old-bottom)))))

#+CLIM-1-compatibility
(progn
(define-compatibility-function (add-output-record-element add-output-record)
			       (record child)
  (add-output-record child record))

(define-compatibility-function (delete-output-record-element delete-output-record)
			       (record child &optional (errorp t))
  (delete-output-record child record errorp))

(define-compatibility-function (output-record-elements output-record-children)
			       (record)
  (output-record-children record))

(define-compatibility-function (output-record-element-count output-record-count)n
			       (record)
  (output-record-count record))
)	;#+CLIM-1-compatibility


;;; Common to all implementations.
;;; ADD-OUTPUT-RECORD assumes that CHILD's start-position and bounding 
;;; rectangle have already been normalized to RECORD's coordinate system.
(defmethod add-output-record :after (child (record output-record-mixin)) ;was :around
  (setf (output-record-parent child) record)
  (recompute-extent-for-new-child record child)
  (when (output-record-stream record)
    (note-output-record-attached child (output-record-stream record))))

(defmethod note-output-record-attached ((record output-record-element-mixin) stream)
  (setf (output-record-stream record) stream))

(defmethod note-output-record-attached :after ((record output-record-mixin) stream)
  (map-over-output-records
   #'(lambda (rec)
       (note-output-record-attached rec stream))
   record))

;;; Ditto.
(defmethod delete-output-record :after
	   (child (record output-record-mixin) &optional (errorp t))
  (declare (ignore errorp))
  (with-bounding-rectangle* (left top right bottom) child
    (multiple-value-bind (xoff yoff)
	(convert-from-descendant-to-ancestor-coordinates child record)
      (declare (type coordinate xoff yoff))
      (translate-fixnum-positions xoff yoff left top right bottom)
      (recompute-extent-for-changed-child record child left top right bottom)))
  (setf (output-record-parent child) nil))	;in case other things are still pointing to it.

(defmethod delete-output-record :around (child (record output-record-mixin) &optional errorp)
  (declare (ignore errorp))
  (let ((stream (output-record-stream child)))
    (multiple-value-prog1
	(call-next-method)
      (when stream
	(note-output-record-detached child)))))


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

(defmethod clear-output-record :after ((record output-record-mixin))
  (bounding-rectangle-set-edges record 0 0 0 0))

(defmethod clear-output-record :around ((record output-record-mixin))
  (when (output-record-stream record)
    (map-over-output-records
     #'note-output-record-detached
     record))
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
    (svref elements index)))

(defmethod output-record-count ((record standard-sequence-output-record))
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

(defmethod delete-output-record 
	   (child (record standard-sequence-output-record) &optional (errorp t))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null (error "The output record ~S was not found in ~S" child record))
      (array
	(let ((index (position child elements :end fill-pointer)))
	  (cond (index
		 (let ((new-fp (the fixnum (1- fill-pointer)))
		       (vector elements))
		   (declare (type simple-vector vector) (fixnum new-fp)
			    #+Genera (sys:array-register vector))
		   (unless (= (the fixnum index) new-fp)
		     ;; Shift the whole vector downward
		     (do ((i (the fixnum index) (1+ i)))
			 ((= i new-fp))
		       (declare (fixnum i) (optimize (speed 3) (safety 0)))
		       (setf (svref vector i) (svref vector (1+ i)))))
		   (setf fill-pointer new-fp)))
		(errorp
		 (error "The output record ~S was not found in ~S" child record)))))
      ;; It must be an OUTPUT-RECORD or a DISPLAYED-OUTPUT-RECORD
      (otherwise
	(unless (eql elements child)
	  (error "The output record ~S was not found in ~S" child record))
	(setf elements nil))))
  t)

(defmethod map-over-output-records-overlapping-region
	   (function (record standard-sequence-output-record) region  
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (declare (type coordinate x-offset y-offset))
  (declare (optimize (safety 0)))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null nil)
      (array
	(if (or (null region) (eql region +everywhere+))
	    (dovector (child elements :start 0 :end fill-pointer :simple-p t)
	      (apply function child continuation-args))
	  (with-bounding-rectangle* (left1 top1 right1 bottom1) region
	    (translate-fixnum-positions x-offset y-offset left1 top1 right1 bottom1)
	    ;; Subtract out the record offset from the region, to make comparison fair
	    (multiple-value-bind (xoff yoff)
		(output-record-position* record)
	      (translate-fixnum-positions (- xoff) (- yoff) left1 top1 right1 bottom1))
	    (dovector (child elements :start 0 :end fill-pointer :simple-p t)
	      (with-bounding-rectangle* (left2 top2 right2 bottom2) child
		(when (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
					    left2 top2 right2 bottom2)
		  (apply function child continuation-args)))))))
      (otherwise
	(if (or (null region) (eql region +everywhere+))
	    (apply function elements continuation-args)
	  (multiple-value-bind (xoff yoff)
	      (output-record-position* record)
	    (declare (type coordinate xoff yoff))
	    (when (region-intersects-offset-region-p
		    elements region (- x-offset xoff) (- y-offset yoff))
	      (apply function elements continuation-args)))))))
  nil)

(defmethod map-over-output-records-containing-point*
	   (function (record standard-sequence-output-record) x y 
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (declare (type coordinate x y x-offset y-offset))
  (declare (optimize (safety 0)))
  (translate-fixnum-positions x-offset y-offset x y)
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null nil)
      (array
	(multiple-value-bind (xoff yoff)
	    (output-record-position* record)
	  (translate-fixnum-positions (- xoff) (- yoff) x y))
	(dovector (child elements :start 0 :end fill-pointer :from-end t :simple-p t)
	  (with-bounding-rectangle* (left top right bottom) child
	    (when (ltrb-contains-point*-p left top right bottom x y)
	      (apply function child continuation-args)))))
      (otherwise
	(multiple-value-bind (xoff yoff) (output-record-position* record)
	  (when (offset-region-contains-point*-p elements xoff yoff x y)
	    (apply function elements continuation-args))))))
  nil)


;;; Mix this in for top-level output histories
(defclass stream-output-history-mixin () ((stream)))

(defmethod bounding-rectangle-set-edges ((record stream-output-history-mixin)
					 left top right bottom)
  (declare (type coordinate left top right bottom))
  #+ignore (assert (<= left right))
  #+ignore (assert (<= top bottom))
  (with-slots ((bl left) (bt top) (br right) (bb bottom) parent stream) record
    ;; Top-level output records must not have their upper left corner any
    ;; "later" than (0,0), or else scroll bars and scrolling will not do
    ;; the right thing.
    (let ((old-left bl)
	  (old-top bt)
	  (old-right br)
	  (old-bottom bb))
      (declare (type coordinate old-left old-top old-right old-bottom))
      (setq bl (min left 0)
	    bt (min top 0)
	    br right
	    bb bottom)))
  record)

#+Silica
(defmethod bounding-rectangle-set-edges :around ((r stream-output-history-mixin) 
						 nminx nminy nmaxx nmaxy)
  (multiple-value-bind
      (minx miny maxx maxy)
      (bounding-rectangle* r)
    (call-next-method)
    (unless (and (= minx nminx)
		 (= miny nminy)
		 (= maxx nmaxx)
		 (= maxy nmaxy))
      ;; This should update the scrollbars etc 
      (let* ((stream (output-record-stream r))
	     (vp (pane-viewport stream)))
	(when vp
	  (update-scrollbars vp))
	(update-region stream (- nmaxx nminx) (- nmaxy nminy))))))

;;; Defclass of OUTPUT-RECORDING-MIXIN, etc. is in STREAM-CLASS-DEFS
(defmethod initialize-instance :after ((stream output-recording-mixin) &rest args)
  (declare (ignore args))
  (with-slots (output-record) stream
    ;;--- Our OUTPUT-RECORDING-MIXIN expects extended output...
    (multiple-value-bind (x y) (stream-cursor-position* stream)
      ;; I don't understand why the output record's initial position was set to
      ;; some untransformed "viewport" coordinate.  The cursor position is the
      ;; right place, no?
      (output-record-set-position* output-record x y))
    (setf (slot-value output-record 'stream) stream)))

(defmethod clear-output-history ((stream output-recording-mixin))
  (when (stream-output-history stream)
    (clear-output-record (stream-output-history stream)))
  (setf (stream-text-output-record stream) nil)
  (setf (stream-highlighted-presentation stream) nil))

(defmethod stream-add-output-record ((stream output-recording-mixin) record)
  (with-slots (output-record current-output-record-stack) stream
    (let ((the-output-record (or current-output-record-stack output-record)))
     (add-output-record record the-output-record)
     #+Silica
     (let ((width (bounding-rectangle-width stream))
	   (height (bounding-rectangle-height stream)))
       (declare (type coordinate width height))
       (with-bounding-rectangle* (rl rt rr rb) the-output-record
	 (when (or (< rl 0) (< width rr)
		   (< rt 0) (< height rb))
	   (update-region stream (- rr rl) (- rb rt))))))))

(defmethod stream-replay ((stream output-recording-mixin) &optional region)
  (when (stream-drawing-p stream)
    (with-slots (output-record text-output-record record-p) stream
      (when (or output-record text-output-record)
	(letf-globally ((record-p nil))
	  (when output-record
	    (replay-output-record output-record stream region 0 0))
	  (when text-output-record
	    (replay-output-record text-output-record stream region 0 0)))))))

(defun erase-output-record (output-record stream)	;--- specialize on stream?
  (multiple-value-bind (xoff yoff)
      (convert-from-relative-to-absolute-coordinates 
	;; --- I'm certainly going to forget to use the PARENT at some point!
	stream (output-record-parent output-record))
    (with-bounding-rectangle* (left top right bottom) output-record
      (with-output-recording-options (stream :record nil)
	(if (or (= left right) (= top bottom))
	    ;; Handle specially, for a line is wider than a rectangle of zero width or height
	    (draw-line-internal stream xoff yoff
				left top right bottom
				+background-ink+ nil)
	    (draw-rectangle-internal stream xoff yoff
				     left top right bottom
				     +background-ink+ nil)))))
  (when (output-record-parent output-record)
    (delete-output-record output-record (output-record-parent output-record)))
  ;; Use the output record itself as the replay region, and replay
  ;; the stuff that might have been obscured by the erased output
  (frame-replay *application-frame* stream output-record))

(defmethod invoke-with-output-recording-options ((stream output-recording-mixin)
						 continuation record draw)
  (letf-globally (((stream-recording-p stream) record)
		  ((stream-drawing-p stream) draw))
    (funcall continuation)))

;;; The following two are only called when STREAM-RECORDING-P is true and the
;;; characters are printable (see CHARACTER-DRAWING.LISP).
(defmethod stream-add-string-output ((stream output-recording-mixin) string
				     start end text-style width height baseline)
  (declare (fixnum start end))
  (when (< start end)
    (let ((record (get-text-output-record stream text-style)))
      (add-string-output-to-text-record record string start end text-style
					width height baseline))))

(defmethod stream-add-character-output ((stream output-recording-mixin) character
					text-style width height baseline)
  (let ((record (get-text-output-record stream text-style)))
    (add-character-output-to-text-record record character text-style
					 width height baseline)))

(defmethod get-text-output-record ((stream output-recording-mixin) style)
  (let ((default-style (medium-default-text-style stream)))
    (let ((record (stream-text-output-record stream)))
      (when record
	;; If we're changing styles mid-stream, need to convert this
	;; text record to the more expensive form
	(when (and (not (eq style default-style))
		   (not (typep record 'styled-text-output-record)))
	  (setq record (stylize-text-output-record record default-style stream)))
	(return-from get-text-output-record record)))
    (let* ((string (make-array 16 :element-type 'extended-char	;--- 16?
				  :fill-pointer 0 :adjustable t))
	   (record (if (not (eq style default-style))
		       (make-styled-text-output-record (medium-ink stream) string)
		       (make-standard-text-output-record (medium-ink stream) string))))
      (setf (stream-text-output-record stream) record)
      (multiple-value-bind (abs-x abs-y)
	  (point-position*
	    (stream-output-history-position stream))
	(declare (type coordinate abs-x abs-y))
	(multiple-value-bind (cx cy) (stream-cursor-position* stream)
	  (declare (type coordinate cx cy))
	  (output-record-set-start-cursor-position*
	    record (- cx abs-x) (- cy abs-y))))
      ;; Moved to STREAM-CLOSE-TEXT-OUTPUT-RECORD, since we don't need this thing
      ;; in the history until then.  This should save an extra recompute-extent call
      ;; (one in here, one when the string is added).
      ;; (stream-add-output-record stream record)
      record)))

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
(defmethod stream-set-cursor-position* :before ((stream output-recording-mixin) x y)
  (declare (ignore x y))
  (stream-close-text-output-record stream))

;; This gets used to reposition the cursor when drawing text.  We need to
;; close the text output record when there was a line wrap, but not when
;; we are simply incrementing the cursor beyond the just-written glyph.
(defmethod stream-set-cursor-position*-internal :before ((stream output-recording-mixin) x y)
  (declare (ignore x))
  (multiple-value-bind (old-x old-y) (stream-cursor-position* stream)
    (declare (ignore old-x))
    (unless (eql y old-y)
      (stream-close-text-output-record stream))))

;; Copy just the text from the window to the stream.  If REGION is supplied,
;; only the text overlapping that region is copied.
;; This loses information about text styles, presentations, and graphics, and
;; doesn't deal perfectly with tab characters and changing baselines.
(defun copy-textual-output-history (window stream &optional region)
  (let* ((char-width (stream-character-width window #\space))
	 (line-height (stream-line-height window))
	 (history (stream-output-history window))
	 (array (make-array (ceiling (bounding-rectangle-height history) line-height)
			    :fill-pointer 0 :adjustable t :initial-element nil)))
    (labels ((collect (record x-offset y-offset)
	       (multiple-value-bind (start-x start-y)
		   (output-record-start-cursor-position* record)
		 (translate-positions x-offset y-offset start-x start-y)
		 (when (typep record 'standard-text-output-record)
		   (vector-push-extend (list* start-y start-x (slot-value record 'string))
				       array))
		 (map-over-output-records-overlapping-region
		   #'collect record region 
		   (- x-offset) (- y-offset) start-x start-y))))
      (declare (dynamic-extent #'collect))
      (collect history 0 0))
    (sort array #'(lambda (r1 r2)
		    (or (< (first r1) (first r2))
			(and (= (first r1) (first r2))
			     (< (second r1) (second r2))))))
    (let ((current-x 0)
	  (current-y (first (aref array 0))))
      (dotimes (i (fill-pointer array))
	(let* ((item (aref array i))
	       (y (pop item))
	       (x (pop item)))
	  (unless (= y current-y)
	    (dotimes (j (round (- y current-y) line-height))
	      #-(or Allegro Minima) (declare (ignore j))
	      (terpri stream)
	      (setq current-x 0))
	    (setq current-y y))
	  (unless (= x current-x)
	    (dotimes (j (round (- x current-x) char-width))
	      #-(or Allegro Minima) (declare (ignore j))
	      (write-char #\space stream))
	    (setq current-x x))
	  (write-string item stream)
	  (incf current-x (stream-string-width window item)))))))

;;; This method should cover a multitude of sins.
#+Silica
(defmethod repaint-sheet :after ((stream output-recording-mixin) region)
  ;;--- Who should establish the clipping region?
  ;;--- Is it here or in the handle-repaint method
  ;; Who should clear the region?
  (with-sheet-medium (medium stream)
    (multiple-value-call #'draw-rectangle*
	medium
	(bounding-rectangle* (region-intersection
			      region 
			      (or (pane-viewport-region stream)
				  (bounding-rectangle stream))))
	:ink +background-ink+)
    (stream-replay stream region)))



;;; For Silica
;;;--- Consider these old methods on a case-by-case basis to see if the
;;; general handle-repaint method subsumes them.

;;; --- should merge our process-update-region with handle-repaint
;;; Do we use it anywhere where Silica isn't generating handle-repaint?

;;; Mix in window-output-recording when you have mixed together
;;; something supporting the window protocol and something supporting
;;; the output recording protocol.
#-Silica
(progn

(defmethod window-process-update-region :around ((stream window-output-recording))
  (let ((update-region (slot-value stream 'update-region)))
    (when update-region
      (with-output-recording-options (stream :draw t :record nil)
	(let ((highlighted-presentation (slot-value stream 'highlighted-presentation)))
	  (when highlighted-presentation
	    (highlight-output-record stream highlighted-presentation :unhighlight))
	  (call-next-method)
	  (dolist (region update-region)
	    (with-clipping-region (stream region)
	      (frame-replay *application-frame* stream region)))
	  (when highlighted-presentation
	    (highlight-output-record stream highlighted-presentation :highlight))))
      (window-flush-update-region stream))))

;;;--- We need some version of this code to do the area copying.
(defmethod window-set-viewport-position* :around ((stream window-output-recording)
						  new-x new-y)
  (declare (ignore new-x new-y))
  (with-bounding-rectangle* (left top right bottom) (window-viewport stream)
    (call-next-method)
    ;; now replay
    (with-bounding-rectangle* (nl nt nr nb) (window-viewport stream)
      (cond
	;; if some of the stuff that was previously on display is still on display
	;; bitblt it into the proper place and redraw the rest.
	((ltrb-overlaps-ltrb-p left top right bottom
			       nl nt nr nb)
	 ;; move the old stuff to the new position
	 (window-shift-visible-region stream left top right bottom
				      nl nt nr nb)
	 (window-process-update-region stream))
	;; otherwise, just redraw the whole visible viewport
	;; Adjust for the left and top margins by hand so clear-area doesn't erase
	;; the margin components.
	(t (multiple-value-bind (ml mt) (window-margins stream)
	     ;;--- Will these be coords or fixnums?
	     (declare (type coordinate ml mt))
	     (multiple-value-bind (vw vh) (window-inside-size stream)
	       (declare (type coordinate vw vh))
	       (window-clear-area stream
				  ml mt (+ ml vw) (+ mt vh))))
	   (frame-replay *application-frame* stream (window-viewport stream)))))))

(defmethod window-refresh :after ((stream window-output-recording))
  ;; don't bother me, it takes too long and is useless since
  ;; we'll refresh this again when it eventually becomes visible
  (when (window-drawing-possible stream)
    (frame-replay *application-frame* stream (window-viewport stream))
    (let ((text-record (stream-text-output-record stream)))
      (when text-record (replay text-record stream)))
    (redisplay-decorations stream)))

;;; I don't think that this is needed.
(defmethod window-note-size-or-position-change :after ((stream window-output-recording)
						       left top right bottom)
  (declare (ignore left top right bottom))
  #+Ignore
  (when (window-visibility stream)
    (window-refresh stream)))

;;; --- Define Silica version of this.
(defmethod window-clear :before ((stream window-output-recording))
  (clear-output-history stream))

) ; end of #-Silica PROGN


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


