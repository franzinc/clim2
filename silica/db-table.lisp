;;; -*- Mode: Lisp; Package: silica; Base: 10.; Syntax: Common-Lisp -*-
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
;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved.
;;;

(in-package :silica)

;;;
;;; Table
;;;

(defclass table-pane (pane-background-mixin
			      mute-input-mixin
			      composite-pane 
			      space-req-cache-mixin)
    ((contents :accessor table-contents)
     (rows)
     (columns)
     (row-h :initform nil)
     (column-w :initform nil)
     ;; ??? order of space, h and heightpace initarg matters with autoconstructors
     (widthpace :initform 0 :initarg :space :initarg :widthpace)
     (heightpace :initform 0 :initarg :space :initarg :heightpace)))


(defmethod initialize-instance :after 
	   ((pane table-pane) &key contents &allow-other-keys)

;;; The space composition and allocation for tables depends on there being
;;; no empty rows or columns in the data structure; otherwise, the space
;;; request comes back with MOST-POSITIVE-FIXNUM size requirements in the
;;; direction of the empty row/column.  The following two forms delete the
;;; empty rows and columns.  The row deletion is easy: the data structure is
;;; set up for that purpose.  The column deletetion is more complex.
;;;  -- rsl 13 November 1990

  ;; Delete empty rows.
  (setf contents (delete-if #'(lambda (row) (every #'null row)) 
			    contents))

  ;; Delete empty columns when there are any; this CONSes too much otherwise.
  (when (block there-is-an-empty-column
	  (flet ((check-for-empty-column (&rest column)
		   (declare (dynamic-extent column))
		   (when (every #'null column)
		     (return-from there-is-an-empty-column t))))
	    (declare (dynamic-extent #'check-for-empty-column))
	    (apply #'mapc #'check-for-empty-column contents))
	  nil)  ;; -> No empty columns
    ;; Yes, there is at least one empty column
    (setf contents
	    (flet ((delete-column-if-empty (&rest column)
		     (declare (clim-utils::non-dynamic-extent column))
		     (if (every #'null column)
			 nil			;Delete this column entirely
			 (list column))))
		     (declare (dynamic-extent #'delete-column-if-empty))
		     (apply #'mapcan #'delete-column-if-empty contents))))

  (with-slots ((table-contents contents)
	       rows columns) pane
    (setf rows (length contents))
    (setf columns (apply #'max (mapcar #'length contents)))
    (let* (i j)

      (setf table-contents (make-array (list rows columns) 
				       :initial-element nil))
      (setq i 0)
      (dolist (row
		;; Vertical list are easier to specify top to bottom, but
		;; easier to deal with bottom to top.  
		(reverse contents))
	(setq j 0)
	(dolist (item row)
	  (cond ((null item) nil)
		((panep item) (adopt-child 
			       pane
			       (setf (aref table-contents i j) item)))
		((eq item :give-left) (warn "Give left not supported"))
		((eq item :give-down) (warn "Give down not supported"))
		(t (error "Illegal element in table contents")))
	  (incf j))
	(incf i)))))

(defmethod compose-space ((pane table-pane))
  (with-slots (rows columns contents widthpace heightpace row-h column-w) pane
    (let (size 
	  max-size
	  min-size
	  (width 0) (max-width 0) (min-width 0)
	  (height 0) (max-height 0) (min-height 0))
      (setf row-h (or row-h (make-array rows :initial-element 0)))
      (setf column-w (or column-w (make-array columns :initial-element 0)))
      (dotimes (i rows)
	(setq size 0)
	(setq max-size most-positive-fixnum)
	(setq min-size 0)
	(dotimes (j columns)
	  (when (aref contents i j)
	    (let ((space-req (compose-space (aref contents i j))))
	      (setq size  (max size (space-req-height space-req)))
	      (setq max-size (min max-size (+ (space-req-height space-req)
					      (space-req-max-height space-req))))
	      (setq min-size (max min-size (- (space-req-height space-req)
					      (space-req-min-height space-req)))))))
	(setf (aref row-h i) (list size 
				   (max (- max-size size) 0)
				   (max (- size min-size) 0)
				   nil)))
      (dotimes (i rows)
	(let ((req (aref row-h i)))
	  (incf height  (first req))
	  (incf max-height (second req))
	  (incf min-height (third req))))
      (incf height (* (1- rows) heightpace))
      
      (dotimes (j columns)
	(setq size 0)
	(setq max-size most-positive-fixnum)
	(setq min-size 0)
	(dotimes (i rows)
	  (when (aref contents i j)
	    (let ((space-req (compose-space (aref contents i j))))
	      (setq size  (max size (space-req-width space-req)))
	      (setq max-size (min max-size (+ (space-req-width space-req)
					      (space-req-max-width space-req))))
	      (setq min-size (max min-size (- (space-req-width space-req)
					      (space-req-min-width space-req)))))))
	(setf (aref column-w j) (list size 
				      (max (- max-size size) 0)
				      (max (- size min-size) 0)
				      nil)))
      (dotimes (j columns)
	(let ((req (aref column-w j)))
	  (incf width  (first req))
	  (incf max-width (second req))
	  (incf min-width (third req))))
      (incf width (* (1- columns) widthpace))
      (make-space-req :width width :max-width max-width :min-width min-width :height height :max-height max-height :min-height min-height))))

(defmacro allocate-to-rows (contract alloc-major rows row-h accessors)
  (let ((major  (first accessors))
	(major+ (second accessors))
	(major- (third accessors))
	(row-major  'first)
	(row-major+ 'second)
	(row-major- 'third)
	(row-alloc  'fourth))
    `(with-slots (space-req) ,contract
       (let ((stretch-p (> ,alloc-major (,major space-req)))
	     give extra used)
	 (if stretch-p
	     (progn (setq give (,major+ space-req))
		    (setq extra (min (- ,alloc-major (,major space-req))
				     give)))
	     (progn (setq give (,major- space-req))
		    (setq extra (min (- (,major space-req) ,alloc-major)
				     give))))
	 (dotimes (i ,rows)
	   (let* ((row-space-req (aref ,row-h i))
		  (alloc (,row-major row-space-req)))
	     (when (> give 0)
	       (if stretch-p
		   (progn (setq used (/ (* (,row-major+ row-space-req) extra)
					give))
			  (incf alloc used)
			  (decf give (,row-major+ row-space-req)))
		   (progn (setq used (/ (* (,row-major- row-space-req)
					   extra)
					give))
			  (decf alloc used)
			  (decf give (,row-major+ row-space-req))))
	       (decf extra used))
	     (setf (,row-alloc row-space-req) alloc)))))))

(defmethod allocate-space ((pane table-pane) width height)
  ;; assuming that pane has been composed previously.
  (with-slots (rows columns contents widthpace heightpace row-h column-w space-req)
      pane
    
    (unless space-req (compose-space pane)) ;; ?? Depends on it being here
    
    (allocate-to-rows pane height rows row-h
		      (space-req-height
		       space-req-max-height space-req-min-height
		       space-req-width space-req-max-width space-req-min-width))
    (allocate-to-rows pane width columns column-w
		      (space-req-width 
		       space-req-max-width space-req-min-width
		       space-req-height space-req-max-height space-req-min-height))

    ;; Now we should know what to do, so let's do it.
    (let (min-x min-y width height) 
      (setq min-y 0)
      (dotimes (i rows)
	(setq min-x 0)
	(setq height (fourth (aref row-h i)))
	(dotimes (j columns)
	  
	  (setq width (fourth (aref column-w j)))
	  (when (aref contents i j)
	    (move-and-resize-sheet* (aref contents i j) 
				    min-x min-y width height))
	  (incf min-x (+ width widthpace)))
	(incf min-y (+ height heightpace))))))

