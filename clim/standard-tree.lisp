;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-internals; Base: 10; Lowercase: Yes -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;

;; $fiHeader: standard-tree.lisp,v 1.1 91/11/25 10:00:54 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
Copyright (c) 1991, Franz Inc. All rights reserved
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;;   (record x y continuation
;;;    &optional (x-offset 0) (y-offset 0) &rest continuation-args)

(defclass standard-tree-output-record (output-record)
    ((coordinate-sorted-set)			;a simple vector, by gawd
     (fill-pointer :initform 0 :type fixnum)
     (tallest-box-height :initform 0 :type fixnum)))

(defmethod initialize-instance :after ((record standard-tree-output-record)
				       &key (size 200))
  (with-slots (coordinate-sorted-set) record
    (setf coordinate-sorted-set (make-array size))))


(defmethod output-record-children ((record standard-tree-output-record))
  (with-slots (coordinate-sorted-set fill-pointer) record
    (let ((result (make-list fill-pointer)))
      (replace result coordinate-sorted-set :end1 fill-pointer :end2 fill-pointer)
      result)))

(defmethod output-record-element ((record standard-tree-output-record) index)
  (with-slots (coordinate-sorted-set) record
    (svref coordinate-sorted-set index)))

(defmethod output-record-count ((record standard-tree-output-record))
  (slot-value record 'fill-pointer))

(defmethod clear-output-record ((record standard-tree-output-record))
  (with-slots (coordinate-sorted-set fill-pointer tallest-box-height) record
    (setf tallest-box-height 0)
    ;; Release pointers to objects
    (fill coordinate-sorted-set nil :start 0 :end fill-pointer)
    (setf fill-pointer 0)))

(defmethod add-output-record (element (record standard-tree-output-record))
  (with-slots (coordinate-sorted-set fill-pointer tallest-box-height) record
    (let ((vector coordinate-sorted-set)
	  (fp fill-pointer))
      (declare (type simple-vector vector) #+Genera (sys:array-register vector))
      (declare (fixnum fp))
      (maxf tallest-box-height (bounding-rectangle-height element))
      (with-bounding-rectangle* (left top right bottom) element
        (declare (ignore left top))
	;; Quick check for doing output at the bottom of the window
	(if (or (zerop fp)
		(let ((other-element (svref vector (1- fp))))
		  (when (eq other-element element)
		    (return-from add-output-record nil))
		  (with-bounding-rectangle* (other-left other-top other-right other-bottom)
					    other-element
		    (declare (ignore other-left other-top))
		    (or (> bottom other-bottom)
			(and (= bottom other-bottom) 
			     (>= right other-right))))))
	    (multiple-value-setq (coordinate-sorted-set fill-pointer)
	      (simple-vector-push-extend element vector fp 200))
	  (let ((index (coordinate-sorted-set-index-for-position
			 vector right bottom 0 fp)))
	    (declare (fixnum index))
	    ;; Make sure that the new element comes after any element it overlaps
	    ;; so that replaying happens in the right order.
	    (loop
	      (if (and (< index fp)
		       (region-intersects-region-p element (svref vector index)))
		  (incf index)
		  (return)))
	    (multiple-value-setq (coordinate-sorted-set fill-pointer)
	      (simple-vector-insert-element element index vector fp 200))))))))

(defmethod remove-output-record (element (record standard-tree-output-record))
  (with-slots (coordinate-sorted-set fill-pointer tallest-box-height) record
	      (let ((index (coordinate-sorted-set-position element coordinate-sorted-set fill-pointer)))
		(cond (index
		       (let ((new-fp (the fixnum (1- fill-pointer)))
			     (vector coordinate-sorted-set))
			 (declare (type simple-vector vector) (fixnum new-fp)
				  #+Genera (sys:array-register vector))
			 (unless (= (the fixnum index) new-fp)
			   ;; Shift the whole vector downward
			   (do ((i (the fixnum index) (1+ i)))
			       ((= i new-fp))
			     (declare (fixnum i) (optimize (speed 3) (safety 0)))
			     (setf (svref vector i) (svref vector (1+ i)))))
			 (setf fill-pointer new-fp)
			 t))
		      (t
		       (error "The element ~S was not found in ~S" element record))))))

(defmethod map-over-output-record-children
    (continuation (record standard-tree-output-record) region)
  (let ((vector (slot-value record 'coordinate-sorted-set))
	(length (slot-value record 'fill-pointer)))
    (declare (type simple-vector vector) (fixnum length)
	     #+Genera (sys:array-register vector))
    (if (or (null region) (eql region +everywhere+))
	(dovector ((element index) vector :start 0 :end length :simple-p t)
	  (funcall continuation element))
      (with-bounding-rectangle* (left1 top1 right1 bottom1) region
	(let ((start (coordinate-sorted-set-index-for-position vector 0 top1 0 length))
	      (limit (+ bottom1 (slot-value record 'tallest-box-height))))
	  (declare (fixnum start limit))
	  (do ((index start (the fixnum (1+ (the fixnum index)))))
	      ((= (the fixnum index) length))
	    (declare (fixnum index))
	    (let ((element (svref vector index)))
	      (with-bounding-rectangle* (left2 top2 right2 bottom2) element
					(when (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
								    left2 top2 right2 bottom2)
					  (funcall continuation element))
					(when (> bottom2 limit)
					  (return nil))))))))))

(defmethod map-over-output-records-containing-point*
  ((record standard-tree-output-record) continuation x y)
  (let ((vector (slot-value record 'coordinate-sorted-set))
	(length (slot-value record 'fill-pointer))
	(bound (slot-value record 'tallest-box-height)))
    (declare (type simple-vector vector) (fixnum length bound)
	     #+Genera (sys:array-register vector))
    (let ((end (coordinate-sorted-set-index-for-position vector 0 (+ y bound 1) 0 length))
	  (limit (- y bound)))
      (declare (fixnum end limit))
      (do ((index (min (1- length) end) (the fixnum (1- (the fixnum index)))))
	  ((< (the fixnum index) 0))
	(declare (fixnum index))
	(let ((element (svref vector index)))
	  (with-bounding-rectangle* (left top right bottom) element
				    (when (ltrb-contains-point*-p left top right bottom x y)
				      (funcall continuation element))
				    (when (< bottom limit)
				      (return nil))))))))


(defun coordinate-sorted-set-position (object vector fill-pointer)
  (declare (type simple-vector vector) (fixnum fill-pointer))
  (declare (optimize (speed 3) (safety 0)))
  (with-bounding-rectangle* (left top right bottom) object
    (declare (ignore left top))
    ;; Binary search to find where this one goes.
    (let ((search-index (coordinate-sorted-set-index-for-position
			  vector right bottom 0 fill-pointer)))
      (declare (fixnum search-index))
      ;; Search back over things in the same place.
      (when (< search-index fill-pointer)
	(dovector ((element index) vector :start 0 :end (1+ search-index)
					  :from-end t :simple-p t)
	  (when (eq element object)
	    (return-from coordinate-sorted-set-position index))
	  (with-bounding-rectangle* (other-left other-top other-right other-bottom)
				    element
	    (declare (ignore other-left other-top))
	    (unless (and (= right other-right) (= bottom other-bottom))
	      (return)))))
      ;; Search forward too.
      (dovector ((element index) vector
		 :start (if (< search-index fill-pointer) (1+ search-index) 0)
		 :end fill-pointer :simple-p t)
	(when (eq element object)
	  (return index)
	  (when (> (bounding-rectangle-bottom element) bottom)
	    (return nil)))))))

;; Binary search; dictionary order Y, X.

(defun coordinate-sorted-set-index-for-position (vector right bottom start end)
  (declare (type simple-vector vector) (fixnum right bottom start end))
  (declare (optimize (speed 3) (safety 0)))
  (let ((below start)
	(above end))
    (declare (fixnum below above))
    (assert (<= below above))			;Binary search will loop otherwise.
    (let (#+Genera (vector vector))
      #+Genera (declare (sys:array-register vector))
      (loop
	(when (= above below)
	  (return above))
	(let* ((index (the fixnum (ash (the fixnum (+ above below)) -1)))
	       (other-box (svref vector index)))
	  (with-bounding-rectangle* (other-left other-top other-right other-bottom)
				    other-box
	    (declare (ignore other-left other-top))
	    (cond ((or (< bottom other-bottom)
		       (and (= bottom other-bottom) (< right other-right)))
		   (setq above index))
		  ((or (> bottom other-bottom)
		       (and (= bottom other-bottom) (> right other-right)))
		   (if (= below index)
		       (return above)
		       (setq below index)))
		  (t
		   (return index)))))))))


;; Top level stuff

;(defclass stream-output-history-mixin (standard-tree-output-record)
;	  ((stream :reader stream-output-history-mixin-stream)))
;
;(defmethod bounding-rectangle-set-edges ((r stream-output-history-mixin) 
;					 nminx nminy nmaxx nmaxy)
;  (multiple-value-bind
;      (minx miny maxx maxy)
;      (bounding-rectangle* r)
;    (call-next-method)
;    (unless (and (= minx nminx)
;		 (= miny nminy)
;		 (= maxx nmaxx)
;		 (= maxy nmaxy))
;      ;; This should update the scrollbars etc 
;      (let* ((stream (stream-output-history-mixin-stream r))
;	     (vp (pane-viewport-sheet stream)))
;	(when vp
;	  (update-scrollbars vp))
;	(update-region stream 
;		       nminx
;		       nminy
;		       nmaxy
;		       nmaxx)))))

