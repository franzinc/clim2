;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-
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

;; $fiHeader: table-formatting.lisp,v 1.9 91/08/05 14:35:38 cer Exp $

(in-package :clim)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
Copyright (c) 1991, Franz Inc. All rights reserved
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(defclass table-output-record (standard-sequence-output-record) 
     ((row-table-p :initform :unknown)		;t ==> table consists of rows
      (inter-row-spacing :initarg :inter-row-spacing)
      (inter-column-spacing :initarg :inter-column-spacing)
      (equalize-column-widths :initarg :equalize-column-widths))
  (:default-initargs :size 25))

(define-output-record-constructor table-output-record
				  (&key inter-row-spacing inter-column-spacing
					equalize-column-widths (size 25))
				  :inter-row-spacing inter-row-spacing
				  :inter-column-spacing inter-column-spacing
				  :equalize-column-widths equalize-column-widths
				  :size size)

(defmethod elements-never-overlap-p ((record table-output-record)) t)

(defclass row-output-record (standard-sequence-output-record) ()
  (:default-initargs :size 5))

(define-output-record-constructor row-output-record
				  (&key  (size 5))
 :size size)

(defmethod elements-never-overlap-p ((record row-output-record)) t)

(defclass column-output-record (standard-sequence-output-record) ()
  (:default-initargs :size 5))

(define-output-record-constructor column-output-record (&key (size 5))
				  :size size)

(defmethod elements-never-overlap-p ((record column-output-record)) t)

(defclass cell-output-record (standard-sequence-output-record)
     ((x-alignment :initform ':left :initarg :align-x)
      (y-alignment :initform ':top :initarg :align-y)
      (minimum-width :initform 0 :initarg :minimum-width
		     :accessor cell-minimum-width)
      (minimum-height :initform 0 :initarg :minimum-height
		      :accessor cell-minimum-height))
  (:default-initargs :size 5))

(define-output-record-constructor cell-output-record
				  (&key  
					(align-x ':left) (align-y ':top)
					(size 5) minimum-width minimum-height)
				  :size size
  :align-x align-x :align-y align-y
  :minimum-width minimum-width
  :minimum-height minimum-height)

#+Genera (zwei:defindentation (map-over-table-elements-helper 2 1))
(defmethod map-over-table-elements-helper ((record row-output-record) type function)
  (declare (dynamic-extent function))
  (unless (member type '(row row-or-column))
    (error "Expected a ~(~S~), but this is a row.~@
            Your table directives are improperly nested."
	   type))
  (funcall function record))

(defmethod map-over-table-elements-helper ((record column-output-record) type function)
  (declare (dynamic-extent function))
  (unless (member type '(column row-or-column))
    (error "Expected a ~(~S~), but this is a column.~@
            Your table directives are improperly nested."
	   type))
  (funcall function record))

(defmethod map-over-table-elements-helper ((record cell-output-record) type function)
  (declare (dynamic-extent function))
  (unless (eql type 'cell) 
    (error "Expected a ~(~S~), but this is a cell.~@
            Your table directives are improperly nested."
	   type))
  (funcall function record))

(defmethod map-over-table-elements-helper ((record output-record-mixin) type function)
  (declare (dynamic-extent function))
  ;; recurse into this guy
  (map-over-table-elements record type function))

(defmethod map-over-table-elements-helper ((record displayed-output-record) type function)
  (declare (ignore function)
	   (dynamic-extent function))
  (error "Expected a ~(~S~), but this is not a table cell at all.~@
          You are probably missing some table directives." type))

#+Genera (zwei:defindentation (map-over-table-elements 2 1))
(defmethod map-over-table-elements ((record output-record-mixin) element-type function)
  ;; function is applied to each inferior of type element-type.  Error checking is
  ;; done to verify that the inferiors are, in fact, of type element-type.
  (declare (dynamic-extent function))
  (map-over-output-record-children #'(lambda (rec)
				       (map-over-table-elements-helper rec element-type function))
				   record
				   +everywhere+))

#+Genera (zwei:defindentation (map-over-table-rows-or-columns 1 1))
(defmethod map-over-table-rows-or-columns ((table table-output-record) function)
  (declare (dynamic-extent function))
  (map-over-table-elements table 'row-or-column function))

#+Genera (zwei:defindentation (map-over-table-rows 1 1))
(defmethod map-over-table-rows ((table table-output-record) function)
  (declare (dynamic-extent function))
  (map-over-table-elements table 'row function))

#+Genera (zwei:defindentation (map-over-row-cells 1 1))
(defmethod map-over-row-cells ((row row-output-record) function)
  (declare (dynamic-extent function))
  (map-over-table-elements row 'cell function))

#+Genera (zwei:defindentation (map-over-table-columns 1 1))
(defmethod map-over-table-columns ((table table-output-record) function)
  (declare (dynamic-extent function))
  (map-over-table-elements table 'column function))

;; We call it MAP-OVER-ROW-CELLS for convenience sake, since a column is just
;; a row tilted on its end.
(defmethod map-over-row-cells ((column column-output-record) function)
  (declare (dynamic-extent function))
  (map-over-table-elements column 'cell function))

(defmethod tree-recompute-extent-1 ((record displayed-output-record))
  (bounding-rectangle* record))

;;; Cells have been positioned manually, probably.
;;; Also, this should probably be an :around method so that we can 
;;; drag the circle inside a cell and have the cell get updated automatically.
;;; Some extra bit will be needed.
;;; Until we do this, only table formatting really works (and arbitrary dragging
;;; of things above cells.
(defmethod tree-recompute-extent-1 ((record cell-output-record))
  (bounding-rectangle* record))

(defmethod tree-recompute-extent-1 ((record output-record-mixin))
  (let ((once nil)
	(min-x 0) (min-y 0) (max-x 0) (max-y 0))
    (declare (fixnum min-x min-y max-x max-y))
    (multiple-value-bind (x-offset y-offset)
	(convert-from-descendant-to-ancestor-coordinates
	  record (output-record-parent record))
      (declare (fixnum x-offset y-offset))
      (flet ((recompute-extent-of-element (element)
	       (multiple-value-bind (left top right bottom)
		   (tree-recompute-extent-1 element)
		 (declare (fixnum left top right bottom))
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
	(declare (dynamic-extent #'recompute-extent-of-element))
	(map-over-output-record-children #'recompute-extent-of-element
					 record 
					 +everywhere+))
      (when (null min-x)
	(setq min-x 0 min-y 0 max-x 0 max-y 0))
      #+ignore (assert (ltrb-well-formed-p min-x min-y max-x max-y))
      (translate-fixnum-positions x-offset y-offset min-x min-y max-x max-y)
      (bounding-rectangle-set-edges record min-x min-y max-x max-y)
      (values min-x min-y max-x max-y))))

;;--- These are here 'cause the Lucid Sun 4 compiler can't find
;;--- the MACROLETted versions in the functions below.
;;--- The MCL compiler gets it even worse!
#+(or lucid ccl-2)
(progn
(defmacro row-max-height (row-number)
  `(the fixnum (svref row-array ,row-number)))
(defmacro column-max-width (column-number)
  `(the fixnum (svref column-array ,column-number)))
(defmacro row-height (row-number)
  `(the fixnum (svref row-array ,row-number)))
(defmacro column-width (column-number)
  `(the fixnum (svref column-array ,column-number)))
)

(defun compute-row-table-p (table)
  ;; find the first thing.
  (flet ((find-row-or-column (row-or-column)
	   (return-from compute-row-table-p
	     (values (typep row-or-column 'row-output-record)))))
    (declare (dynamic-extent #'find-row-or-column))
    (map-over-table-rows-or-columns table #'find-row-or-column)))

(defmethod row-table-p ((table table-output-record))
  (with-slots (row-table-p) table
    (cond ((not (eql row-table-p ':unknown))
	   row-table-p)
	  (t
	   (setf row-table-p (compute-row-table-p table))))))

;; How can we make compute-output-size work without saving both an ink-rectangle
;; and a whitespace rectangle for each output record?
;; If we did that, table formatting would work much better as well.

(defmethod adjust-table-cells ((table table-output-record))
  (let* ((nrows 0)
	 (ncells nil)
	 (cells 0)
	 (row-table-p (row-table-p table))
	 (table-mapper (if row-table-p #'map-over-table-rows #'map-over-table-columns))
	 (inter-row-spacing (slot-value table 'inter-row-spacing))
	 (inter-column-spacing (slot-value table 'inter-column-spacing))
	 (equalize-column-widths (slot-value table 'equalize-column-widths)))
    (declare (fixnum nrows cells inter-row-spacing inter-column-spacing))
    (labels ((count-rows (row)
	       (incf nrows)
	       (setq cells 0)
	       (map-over-row-cells row #'count-cells)
	       (cond ((null ncells)
		      (setq ncells cells))
		     (t
		      (maxf ncells cells))))
	     (count-cells (cell)
	       (assert (typep cell 'cell-output-record))
	       (incf cells)))
      (declare (dynamic-extent #'count-rows #'count-cells))
      ;; calculate nrows & ncells (= ncells per row)
      (funcall table-mapper table #'count-rows))
    ;; If there are no rows, COUNT-ROWS won't get invoked and NCELLS will be
    ;; NIL.  This will give MAKE-ARRAY fits.
    (when (null ncells) (setq ncells 0))
    (with-stack-array (row-array nrows :initial-element nil)
      (with-stack-array (column-array ncells :initial-element nil)
	(let ((x-pos nil)
	      (y-pos nil)
	      (row-count 0) (column-count 0)
	      (total-width 0) (total-height 0))
	  (declare (fixnum row-count column-count total-width total-height))
	  ;; We always want the table to start at its START-X and START-Y positions.
	  (multiple-value-setq (x-pos y-pos) (output-record-position* table))
	  (macrolet (#-ccl-2 (row-max-height (row-number)
			       `(the fixnum (svref row-array ,row-number)))
		     #-ccl-2 (column-max-width (column-number)
			       `(the fixnum (svref column-array ,column-number))))
	    ;; Figure out max height for each row,
	    ;;            max width for each column.
	    ;; Collect row heights and column widths into temp arrays.
	    ;; We need to remember for each row its total height and
	    ;; the difference between the smallest top and the largest top.
	    ;; For each row remember the total height and then remember the maximum
	    ;; difference between the row top and the y-position of the row.
	    ;; Rows and columns are pretty symmetric, but we need to arrange
	    ;; for a few things to work out right...
	    (unless row-table-p
	      (rotatef row-array column-array))
	    (if row-table-p (setq row-count -1) (setq column-count -1))
	    (flet ((row-mapper (row)
		     (if row-table-p (incf row-count) (incf column-count))
		     (if row-table-p (setq column-count -1) (setq row-count -1))
		     (adjust-table-cells row)
		     (flet ((cell-mapper (cell)
			      (if row-table-p (incf column-count) (incf row-count))
			      (multiple-value-bind (width height)
				  (bounding-rectangle-size cell)
				(declare (fixnum width height))
				(maxf-or (row-max-height row-count)
					 (max height (cell-minimum-height cell)))
				(maxf-or (column-max-width column-count)
					 (max width (cell-minimum-width cell))))))
		       (declare (dynamic-extent #'cell-mapper))
		       (map-over-row-cells row #'cell-mapper))))
	      (declare (dynamic-extent #'row-mapper))
	      (funcall table-mapper table #'row-mapper))
	    (when equalize-column-widths
	      (let ((column-width 0)
		    (n-columns (1+ column-count)))
		(declare (fixnum column-width n-columns))
		(dotimes (i n-columns)
		  (maxf column-width (column-max-width i)))
		(dotimes (i n-columns)
		  (setf (column-max-width i) column-width))))
	    (if row-table-p (setq row-count -1) (setq column-count -1))
	    (flet ((row-mapper (row)
		     (if row-table-p (incf row-count) (incf column-count))
		     (let ((this-row-height (row-max-height row-count))
			   (this-column-width (column-max-width column-count)))
		       (declare (fixnum this-row-height this-column-width))
		       ;; all numbers are in (output-record-parent table) coordinates
		       (if row-table-p (setq column-count -1) (setq row-count -1))
		       (setq total-width x-pos
			     total-height y-pos)
		       (flet ((cell-mapper (cell)
				(if row-table-p (incf column-count) (incf row-count))
				(let ((column-width (column-max-width column-count))
				      (row-height (row-max-height row-count))
				      (cell-width (bounding-rectangle-width cell))
				      (cell-height (bounding-rectangle-height cell))
				      (x-alignment-adjust 0)
				      (y-alignment-adjust 0))
				  (declare (fixnum column-width row-height
						   cell-width cell-height
						   x-alignment-adjust y-alignment-adjust))
				  (ecase (slot-value cell 'x-alignment)
				    (:left )
				    (:right
				      (setq x-alignment-adjust
					    (the fixnum (- column-width cell-width))))
				    (:center
				      (setq x-alignment-adjust
					    (the fixnum (floor (- column-width cell-width) 2)))))
				  (ecase (slot-value cell 'y-alignment)
				    (:top )
				    (:bottom
				      (setq y-alignment-adjust
					    (the fixnum (- row-height cell-height))))
				    (:center
				      (setq y-alignment-adjust
					    (the fixnum (floor (- row-height cell-height) 2)))))
				  (multiple-value-bind (x-offset
							y-offset)
				      (values 0 0)
				    (declare (fixnum x-offset y-offset))
				    (output-record-set-position*
				      cell
				      (the fixnum (+ x-offset total-width  x-alignment-adjust))
				      (the fixnum (+ y-offset total-height y-alignment-adjust))))
				  (if row-table-p
				      (incf total-width
					    (the fixnum (+ column-width inter-column-spacing)))
				    (incf total-height
					  (the fixnum (+ row-height inter-row-spacing)))))))
			 (declare (dynamic-extent #'cell-mapper))
			 (map-over-row-cells row #'cell-mapper))
		       (if row-table-p
			   (incf y-pos
				 (the fixnum (+ this-row-height inter-row-spacing)))
			   (incf x-pos
				 (the fixnum (+ this-column-width inter-column-spacing)))))))
	      (declare (dynamic-extent #'row-mapper))
	      (funcall table-mapper table #'row-mapper)))))))
  (tree-recompute-extent table))

;;; Table has already been laid out.
(defmethod adjust-multiple-columns ((table table-output-record) stream
				    &optional n-columns inter-column-spacing)
  (let ((row-count 0))
    (declare (fixnum row-count))
    (when (slot-value table 'row-table-p)
      (flet ((row-counter (row)
	       (declare (ignore row))
	       (incf row-count)))
	(declare (dynamic-extent #'row-counter))
	(map-over-table-rows table #'row-counter))
      (when (> row-count 5)
	(with-bounding-rectangle* (tleft ttop tright tbottom) table
	  (multiple-value-bind (stream-width stream-height)
	      #-Silica (window-inside-size stream)
	      #+Silica (bounding-rectangle-size stream)
	    (declare (fixnum stream-width) (ignore stream-height))	;for now
	    (let* ((table-width (- tright tleft))
		   (between-column-margin
		     (if inter-column-spacing
			 (process-spacing-arg stream inter-column-spacing
					      'formatting-table
					      ':multiple-columns-inter-column-spacing)
		         (stream-string-width stream " ")))
		   (column-width (+ table-width between-column-margin))
		   (possible-columns (or n-columns (floor stream-width column-width)))
		   (inter-row-spacing (slot-value table 'inter-row-spacing))
		   (rows-per-column (max 3 (ceiling row-count possible-columns)))
		   (row-number 0)
		   (row-x 0)
		   (row-y 0))
	      (declare (fixnum table-width between-column-margin
			       column-width possible-columns rows-per-column
			       row-number row-x row-y))
	      (flet ((layout-multiple-columns (row)
		       (multiple-value-bind (rl rt) (bounding-rectangle-position* row)
			 (multiple-value-bind (xoff yoff)
			     (values 0 0)
			   (declare (fixnum xoff yoff))
			   (translate-fixnum-positions xoff yoff rl rt))
			 ;; Position the row so that the X position relative to the
			 ;; original table is preserved, so that :ALIGN-X :RIGHT works
			 ;;--- ROW-Y needs the same treatment for :ALIGN-Y
			 (output-record-set-position* row (+ row-x (- rl tleft)) row-y)
			 (incf row-number)
			 (incf row-y (+ (bounding-rectangle-height row) inter-row-spacing))
			 (when (zerop (mod row-number rows-per-column))
			   (setq row-x (+ row-x column-width)
				 row-y 0)))))
		(declare (dynamic-extent #'layout-multiple-columns))
		(map-over-table-rows table #'layout-multiple-columns)))))
	(tree-recompute-extent table)))))

(defmethod adjust-table-cells ((row row-output-record))
  #+ignore
  (let ((inter-column-spacing (slot-value (output-record-parent row) 'inter-column-spacing))
	(x-position 0))
    (map-over-row-cells row
      #'(lambda (cell)
	  (with-bounding-rectangle* (left top right bottom) cell
	    (declare (ignore bottom))
	    (output-record-set-position* cell x-position top)
	    (incf x-position (+ (- right left) inter-column-spacing)))))))

(defmethod adjust-table-cells ((column column-output-record))
  #+ignore
  (let ((inter-column-spacing (slot-value (output-record-parent column) 'inter-column-spacing))
	(x-position 0))
    (map-over-row-cells column
      #'(lambda (cell)
	  (with-bounding-rectangle* (left top right bottom) cell
	    (declare (ignore bottom))
	    (output-record-set-position* cell x-position top)
	    (incf x-position (+ (- right left) inter-column-spacing)))))))

(defun process-spacing-arg (stream spacing form &optional clause)
  (cond ((null spacing) nil)
	((integerp spacing) spacing)
	((stringp spacing) (stream-string-width stream spacing))
	((characterp spacing) (stream-character-width stream spacing))
	((or (symbolp spacing) (functionp spacing)) (funcall spacing stream))
	((listp spacing)
	 (let ((units (second spacing))
	       (number (first spacing)))
	   (case units
	     (:point (round (* number (implementation-pixels-per-point stream))))
	     (:pixel number)
	     ;; which character?  Width or height?
	     (:character (* number (stream-character-width stream #\W))))))
	(t (error "The ~:[~;~S ~]spacing specification, ~S, to ~S was invalid"
		  clause clause spacing form))))

;; FORMATTING-TABLE macro in FORMATTED-OUTPUT-DEFS
(defun formatting-table-internal (stream continuation
				  &key inter-row-spacing inter-column-spacing
				       (record-type 'table-output-record)
				       multiple-columns multiple-columns-inter-column-spacing
				       equalize-column-widths
				       (move-cursor t))
  (let ((table (with-new-output-record (stream record-type nil
					:inter-row-spacing
					  (or (process-spacing-arg stream inter-row-spacing
								   'formatting-table
								   ':inter-row-spacing)
					      (stream-vsp stream))
					:inter-column-spacing
					  (or (process-spacing-arg stream inter-column-spacing
								   'formatting-table
								   ':inter-column-spacing)
					      (stream-string-width stream " "))
					:equalize-column-widths equalize-column-widths)
		 (with-output-recording-options (stream :draw-p nil :record-p t)
		   (with-end-of-line-action (:allow stream)
		     (with-end-of-page-action (:allow stream)
		       (funcall continuation stream)))))))
    (adjust-table-cells table)
    (when multiple-columns
      (adjust-multiple-columns table stream
			       (and (numberp multiple-columns) multiple-columns)
			       (and multiple-columns multiple-columns-inter-column-spacing)))
    (replay table stream)
    (when move-cursor
      (move-cursor-beyond-output-record stream table))
    table))

;; FORMATTING-CELL macro in FORMATTED-OUTPUT-DEFS
(defmethod formatting-cell-internal ((stream output-protocol-mixin) continuation
				     &key (record-type 'cell-output-record)
					  (align-x ':left) (align-y ':top)
					  minimum-width minimum-height)
  (setq minimum-width (or (process-spacing-arg stream minimum-width
					       'formatting-cell :minimum-width)
			  0)
	minimum-height (or (process-spacing-arg stream minimum-height
					       'formatting-cell :minimum-height)
			  0))
  ;; --- jump through a hoop to get a constant record-type symbol into the 
  ;; WITH-NEW-OUTPUT-RECORD macro so we invoke a PCL instance-constructor function
  ;; instead of slow make-instance.  If this body was just expanded in-line in formatting-cell
  ;; it would just work.  We could skip the IF and just call with-new-output-record-internal...
  (let ((stream (or *original-stream* stream)))
    (multiple-value-bind (x y) (stream-cursor-position* stream)
      (if (eql record-type 'cell-output-record)
	  (with-new-output-record (stream 'cell-output-record nil
				   :align-x align-x :align-y align-y
				   :minimum-width minimum-width
				   :minimum-height minimum-height)
	    (funcall continuation stream))
          (with-new-output-record (stream record-type nil
				   :align-x align-x :align-y align-y
				   :minimum-width minimum-width
				   :minimum-height minimum-height)
	    (funcall continuation stream)))
      (stream-set-cursor-position* stream x y))))

(defmethod formatting-cell-internal :around ((stream basic-output-recording) continuation
					     &rest options)
  (declare (ignore continuation options))
  (letf-globally (((output-recording-stream-text-output-record stream) nil))
    (call-next-method)))

(defclass item-list-output-record (standard-sequence-output-record) 
     ((inter-row-spacing :initarg :inter-row-spacing)
      (inter-column-spacing :initarg :inter-column-spacing)
      (no-initial-spacing :initarg :no-initial-spacing)
      (n-columns :initarg :n-columns)
      (n-rows :initarg :n-rows)
      (max-width :initarg :max-width)
      (max-height :initarg :max-height)
      (stream-width :initarg :stream-width)
      (stream-height :initarg :stream-height))
  (:default-initargs :size 25 :n-columns nil :n-rows nil
		     :max-width nil :max-height nil
		     :stream-width nil :stream-height nil))

(define-output-record-constructor item-list-output-record
				  (&key inter-row-spacing inter-column-spacing
					no-initial-spacing
					n-columns n-rows
					max-width max-height stream-width stream-height
					(size 25))
  :inter-row-spacing inter-row-spacing :inter-column-spacing inter-column-spacing
  :no-initial-spacing no-initial-spacing
  :n-columns n-columns :n-rows n-rows :max-width max-width :max-height max-height
  :stream-width stream-width :stream-height stream-height :size size)

(defmethod elements-never-overlap-p ((record item-list-output-record)) t)

;;; map-over-TABLE-cells??
#+Genera (zwei:defindentation (map-over-menu-cells 1 1))
(defmethod map-over-menu-cells ((menu item-list-output-record) function)
  (declare (dynamic-extent function))
  (map-over-table-elements menu 'cell function))

;; If we had our hands on a stream, we could use twice the width of #\Space
(defvar *default-minimum-menu-column-spacing* 10)

(defmethod adjust-table-cells ((menu item-list-output-record))
  (let ((ncolumns (slot-value menu 'n-columns))
	(nrows (slot-value menu 'n-rows))
	(ncells 0)
	(max-cell-width 0)
	(max-cell-height 0)
	(inter-row-spacing (slot-value menu 'inter-row-spacing))
	(inter-column-spacing (slot-value menu 'inter-column-spacing))
	(no-initial-spacing (slot-value menu 'no-initial-spacing))
	(max-width (slot-value menu 'max-width))	;don't make menu
	(max-height (slot-value menu 'max-height))	;exceed these bounds
	(constrain t)
	(preferred-geometry 'column)		;vertical menu
	(golden-ratio 1.6))
    (declare (fixnum ncells max-cell-width max-cell-height))
    (flet ((count-cells (cell)
	     (multiple-value-bind (width height) (bounding-rectangle-size cell)
	       (declare (fixnum width height))
	       (maxf max-cell-width width)
	       (maxf max-cell-height height)
	       (incf ncells))))
      (declare (dynamic-extent #'count-cells))
      (map-over-menu-cells menu #'count-cells))
    ;; --- preferred-geometry isn't used yet
    ;; what's the preferred orientation of the menu?
    (unless (or nrows ncolumns)
      ;; anything explicit overrides
      (when constrain				;orientation like window orientation.
	(let ((swidth (slot-value menu 'stream-width))
	      (sheight (slot-value menu 'stream-height)))
	  (when (and swidth sheight)
	    (when (> (/ swidth sheight) golden-ratio)
	      ;; when the stream is wider than it is high by more than the golden
	      ;; ratio, make the preferred ordering 'row
	      (setq preferred-geometry 'row))))))
    ;; compute geometry
    (cond (ncolumns
	   (setq nrows (max 1 (ceiling (/ ncells ncolumns))))
	   ;;
	   )
	  (nrows
	   (setq ncolumns (max 1 (ceiling (/ ncells nrows)))))
	  (max-height
	   ;; could compute this better
	   (setq nrows 
		 (max 1
		      (let ((acc-height 0)
			    (count 0))
			(loop 
			  (incf acc-height max-cell-height)
			  (when (> acc-height max-height)
			    (return count))
			  (incf count)
			  (incf acc-height inter-row-spacing)))))
	   (setq ncolumns (max 1 (ceiling (/ ncells nrows)))))
	  (max-width
	   (let ((spacing (or inter-column-spacing *default-minimum-menu-column-spacing*)))
	     (setq ncolumns (or (block try-one-row
				  (let ((acc-width spacing))	;left margin
				    (flet ((sum-width (cell)
					     (incf acc-width
						   (+ (bounding-rectangle-width cell) spacing))
					     (when (> acc-width max-width)
					       (return-from try-one-row nil))))
				      (declare (dynamic-extent #'sum-width))
				      (map-over-menu-cells menu #'sum-width))
				    (max ncells 1)))
				;; Won't fit in one row, use a more conservative computation
				;; that uses max-cell-width instead of the actual widths
				;; This could still be more accurate than it is.
				(max 1 (let ((acc-width spacing)	;left margin
					     (count 0))
					 (loop
					   (incf acc-width (+ max-cell-width spacing))
					   (when (> acc-width max-width)
					     (return count))
					   (incf count))))))
	     (setq nrows (max 1 (ceiling (/ ncells ncolumns))))))
	  (t
	   ;; try to make this a golden-ratio menu
	   ;; deduce golden ratio from other parameters
	   (setq ncolumns (max 1 (floor (sqrt (/ (* ncells
						    max-cell-width
						    max-cell-height)
						 1.6))
					(max max-cell-width 1))))
	   (setq nrows (max 1 (ceiling (/ ncells ncolumns))))))
    ;; remember geometry for later debuggability
    (setf (slot-value menu 'n-columns) ncolumns)
    (setf (slot-value menu 'n-rows) nrows)
    (with-stack-array (row-array nrows :initial-element nil)
      (with-stack-array (column-array ncolumns :initial-element nil)
	(let ((row-count 0)
	      (column-count 0))
	  (declare (fixnum row-count column-count))
	  (macrolet (#-ccl-2 (row-height (row-number)
			       `(the fixnum (svref row-array ,row-number)))
		     #-ccl-2 (column-width (column-number)
			       `(the fixnum (svref column-array ,column-number))))
	    ;; Collect row heights and column widths into temp arrays.
	    ;; We need to remember for each row its total height and
	    ;; the difference between the smallest top and the largest top.
	    ;; For each row remember the total height and then remember the maximum
	    ;; difference between the row top and the y-position of the row.
	    (flet ((size-cells (cell)
		     (multiple-value-bind (width height) (bounding-rectangle-size cell)
		       (declare (fixnum width height))
		       (maxf-or (column-width column-count) width)
		       (maxf-or (row-height row-count) height))
		     (incf column-count)
		     (when (= column-count ncolumns)
		       (incf row-count)
		       (setf column-count 0))))
	      (declare (dynamic-extent #'size-cells))
	      (map-over-menu-cells menu #'size-cells))
	    ;; Now default the inter-column-spacing to a spacing that spreads the
	    ;; columns evenly over the entire width of the menu
	    (unless inter-column-spacing
	      (setf (slot-value menu 'inter-column-spacing)
		    (setq inter-column-spacing
			  (if max-width
			      (let ((accumulated-width 0))
				(declare (fixnum accumulated-width))
				(dotimes (column ncolumns)
				  (incf accumulated-width (column-width column)))
				(floor (- max-width accumulated-width) (1+ ncolumns)))
			      *default-minimum-menu-column-spacing*))))
	    (setq row-count 0
		  column-count 0)
	    (multiple-value-bind (left-margin top-margin) (bounding-rectangle-position* menu)
	      (declare (fixnum left-margin top-margin))
	      (let ((accumulated-height 0)
		    (accumulated-width (if no-initial-spacing 0
					 inter-column-spacing)))
		(declare (fixnum accumulated-height
				 accumulated-width))
		
		(flet ((adjust-cells (cell)
			 (let ((column-width (column-width column-count))
			       (row-height (row-height row-count))
			       (cell-width (bounding-rectangle-width cell))
			       (cell-height (bounding-rectangle-height cell))
			       (x-alignment-adjust 0)
			       (y-alignment-adjust 0))
			   (declare (fixnum column-width row-height cell-width cell-height
					    x-alignment-adjust y-alignment-adjust))
			   (ecase (slot-value cell 'x-alignment)
			     (:left )
			     (:right
			       (setq x-alignment-adjust
				     (the fixnum (- column-width cell-width))))
			     (:center
			       (setq x-alignment-adjust
				     (the fixnum (floor (- column-width cell-width) 2)))))
			   (ecase (slot-value cell 'y-alignment)
			     (:top )
			     (:bottom
			       (setq y-alignment-adjust
				     (the fixnum (- row-height cell-height))))
			     (:center
			       (setq y-alignment-adjust
				     (the fixnum (floor (- row-height cell-height) 2)))))
			   (multiple-value-bind (x-offset y-offset)
			       (values 0 0)
			     (declare (fixnum x-offset y-offset))

				    
			     (output-record-set-position*
			       cell
			       (the fixnum (+ x-offset left-margin accumulated-width x-alignment-adjust))
			       (the fixnum (+ y-offset top-margin accumulated-height y-alignment-adjust)))))
			 (incf accumulated-width (column-width column-count))
			 (incf accumulated-width inter-column-spacing)
			 (incf column-count)
			 (when (= column-count ncolumns)
			   (setf accumulated-width (if no-initial-spacing 0 inter-column-spacing))
			   (incf accumulated-height (row-height row-count))
			   (incf accumulated-height inter-row-spacing)
			   (setf column-count 0)
			   (incf row-count))))
		  (declare (dynamic-extent #'adjust-cells))
		  (map-over-menu-cells menu #'adjust-cells)))))))))
  (tree-recompute-extent menu))

;; FORMATTING-ITEM-LIST macro in FORMATTED-OUTPUT-DEFS
(defun formatting-item-list-internal (stream continuation
				      &key (record-type 'item-list-output-record)
					   inter-row-spacing inter-column-spacing
					   (no-initial-spacing t)
					   n-columns n-rows
					   max-width max-height
					   stream-width stream-height
					   (move-cursor t))
  (let ((menu (with-new-output-record (stream record-type nil
				       :n-columns n-columns :n-rows n-rows
				       :max-width max-width :max-height max-height
				       :stream-width stream-width :stream-height stream-height
				       :inter-row-spacing
				         (or (process-spacing-arg stream inter-row-spacing
								  'formatting-item-list
								  ':inter-row-spacing)
					     (stream-vsp stream))
				       :inter-column-spacing
				         (process-spacing-arg stream inter-column-spacing
							      'formatting-item-list
							      ':inter-column-spacing)
				       :no-initial-spacing no-initial-spacing)
		(with-output-recording-options (stream :draw-p nil :record-p t)
		  (with-end-of-line-action (:allow stream)
		    (with-end-of-page-action (:allow stream)
		      (funcall continuation stream)))))))
    (adjust-table-cells menu)
    (replay menu stream)
    (when move-cursor
      (move-cursor-beyond-output-record stream menu))
    menu))

(defun format-items (item-list &key (stream *standard-output*) printer presentation-type
				    inter-row-spacing inter-column-spacing
				    (no-initial-spacing t)
				    n-rows n-columns max-width max-height
				    (record-type 'item-list-output-record)
				    (cell-align-x ':left) (cell-align-y ':top))
  (when (and printer presentation-type)
    (error "Only one of ~S or ~S can be specified." ':printer ':presentation-type))
  (when (and (null printer) (null presentation-type))
    (error "One of ~S or ~S must be specified." ':printer ':presentation-type))
  (formatting-item-list (stream
			  :record-type record-type
			  :n-rows n-rows :n-columns n-columns
			  :max-width max-width :max-height max-height
			  :inter-row-spacing inter-row-spacing
			  :inter-column-spacing inter-column-spacing
			  :no-initial-spacing no-initial-spacing)
    ;;--- Lists?  Sequences?
    (dolist (item item-list)
      (formatting-cell (stream :align-x cell-align-x :align-y cell-align-y)
	(cond (printer
	       (funcall printer item stream))
	      (presentation-type
	       (present item presentation-type :stream stream)))))))
