;; -*- mode: common-lisp; package: clim-internals -*-
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
;; $fiHeader$

(in-package :clim-internals)


(defclass standard-sequence-output-record (output-record)
    ((elements :initform nil)
     (fill-pointer :initform 0 :type fixnum)))

  
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
      ((or null output-record-element) (setf elements nil))
      (array (setf fill-pointer 0)))))


(defmethod output-record-children ((record standard-sequence-output-record))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null nil)
      (array
	(let ((result (make-list fill-pointer)))
	  (replace result elements :end1 fill-pointer :end2 fill-pointer)
	  result))
      ;; It must be an OUTPUT-RECORD-ELEMENT
      (otherwise (list elements)))))

(defmethod output-record-element ((record standard-sequence-output-record) index)
  (with-slots (elements) record
    (svref elements index)))

(defmethod output-record-count ((record standard-sequence-output-record))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null 0)
      (array fill-pointer)
      ;; It must be an OUTPUT-RECORD-ELEMENT
      (otherwise 1))))

(defmethod clear-output-record ((record standard-sequence-output-record))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null nil)
      (array (setf fill-pointer 0))
      ;; It must be an OUTPUT-RECORD-ELEMENT
      (otherwise (setf elements nil)))))

(defmethod add-output-record (element (record standard-sequence-output-record))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null
	(setf elements element))
      (array
	(multiple-value-setq (elements fill-pointer)
	  (simple-vector-push-extend element elements fill-pointer)))
      ;; It must be an OUTPUT-RECORD-ELEMENT
      (otherwise
	(let ((first elements))
	  (setf elements (make-array 5))
	  (setf fill-pointer 2)
	  (setf (svref elements 0) first)
	  (setf (svref elements 1) element))))))

(defmethod remove-output-record (element (record standard-sequence-output-record))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (null (error "The element ~S was not found in ~S" element record))
      (array
	(let ((index (position element elements :end fill-pointer)))
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
		(t
		 (error "The element ~S was not found in ~S" element record)))))
      ;; It must be an OUTPUT-RECORD-ELEMENT
      (otherwise
	(unless (eql elements element)
	  (error "The element ~S was not found in ~S" element record))
	(setf elements nil))))
  t)

(defmethod map-over-output-record-children (continuation
					    (record
					     standard-sequence-output-record) region)
  (with-slots (elements fill-pointer) record
	      (typecase elements
			(null nil)
			(array
			 (if (or (null region) (eql region +everywhere+))
			     (dovector (element elements :start 0 :end fill-pointer :simple-p t)
				       (funcall continuation element))
			   (dovector (element elements :start 0 :end fill-pointer
					      :simple-p t)
				     (when (region-intersects-region-p element region)
				       (funcall continuation element)))))
			(otherwise
			 (when (or (null region)
				   (eql region +everywhere+)
				   (region-intersects-region-p elements region))
			   (funcall continuation elements))))
	      nil))

(defmethod map-over-output-records-containing-point* ((record standard-sequence-output-record)  continuation x y)
  (with-slots (elements fill-pointer) record
	      (typecase elements
			(null nil)
			(array
			 (dovector (element elements :start 0 :end fill-pointer :from-end t :simple-p t)
				   (when (region-contains-point*-p element x y)
				     (funcall continuation element))))
			(otherwise
			 (when (region-contains-point*-p elements x y)
			   (funcall continuation elements))))
	      nil))

