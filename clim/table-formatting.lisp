;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;
;; $Id: table-formatting.lisp,v 2.7 2007/04/17 21:45:50 layer Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;; Tables

(define-protocol-class table-output-record (output-record))

(defclass standard-table-output-record
          (standard-sequence-output-record table-output-record)
    ((row-table-p :initform :unknown)                ;t ==> table consists of rows
     (x-spacing :initarg :x-spacing)
     (y-spacing :initarg :y-spacing)
     (equalize-column-widths :initarg :equalize-column-widths))
  (:default-initargs :size 25))

(define-output-record-constructor standard-table-output-record
                                  (&key x-position y-position
                                        x-spacing y-spacing
                                        equalize-column-widths (size 25))
  :x-position x-position :y-position y-position
  :x-spacing x-spacing :y-spacing y-spacing
  :equalize-column-widths equalize-column-widths
  :size size)

(defmethod children-never-overlap-p ((record standard-table-output-record)) t)


(define-protocol-class row-output-record (output-record))

(defclass standard-row-output-record
          (standard-sequence-output-record row-output-record)
    ()
  (:default-initargs :size 5))

(define-output-record-constructor standard-row-output-record
                                  (&key x-position y-position (size 5))
  :x-position x-position :y-position y-position :size size)

(defmethod children-never-overlap-p ((record standard-row-output-record)) t)


(define-protocol-class column-output-record (output-record))

(defclass standard-column-output-record
          (standard-sequence-output-record column-output-record)
    ()
  (:default-initargs :size 5))

(define-output-record-constructor standard-column-output-record
                                  (&key x-position y-position (size 5))
  :x-position x-position :y-position y-position :size size)

(defmethod children-never-overlap-p ((record standard-column-output-record)) t)


(define-protocol-class cell-output-record (output-record))

(defclass standard-cell-output-record
          (standard-sequence-output-record cell-output-record)
  ((x-alignment :initform ':left :initarg :align-x)
   (y-alignment :initform ':top :initarg :align-y)
   (min-width :initform 0 :initarg :min-width
              :accessor cell-min-width :type coordinate)
   (min-height :initform 0 :initarg :min-height
               :accessor cell-min-height :type coordinate))
  (:default-initargs :size 5))

(define-output-record-constructor standard-cell-output-record
                                  (&key x-position y-position
                                        (align-x ':left) (align-y ':top)
                                        (size 5) min-width min-height)
  :x-position x-position :y-position y-position :size size
  :align-x align-x :align-y align-y
  :min-width (coordinate min-width) :min-height (coordinate min-height))

#+Genera (zwei:defindentation (map-over-table-elements-helper 2 1))
(defmethod map-over-table-elements-helper
           (function (record standard-row-output-record) type)
  (declare (dynamic-extent function))
  (unless (member type '(:row :row-or-column))
    (error "Expected a ~(~S~), but this is a row.~@
            Your table directives are improperly nested."
           type))
  (funcall function record))

(defmethod map-over-table-elements-helper
           (function (record standard-column-output-record) type)
  (declare (dynamic-extent function))
  (unless (member type '(:column :row-or-column))
    (error "Expected a ~(~S~), but this is a column.~@
            Your table directives are improperly nested."
           type))
  (funcall function record))

(defmethod map-over-table-elements-helper
           (function (record standard-cell-output-record) type)
  (declare (dynamic-extent function))
  (unless (eq type :cell)
    (error "Expected a ~(~S~), but this is a cell.~@
            Your table directives are improperly nested."
           type))
  (funcall function record))

(defmethod map-over-table-elements-helper
           (function (record output-record-mixin) type)
  (declare (dynamic-extent function))
  ;; Recurse into this guy
  (map-over-table-elements function record type))

(defmethod map-over-table-elements-helper
           (function (record output-record-element-mixin) type)
  (declare (ignore function)
           (dynamic-extent function))
  (error "Expected a ~(~S~), but this is not a table cell at all.~@
          You are probably missing some table directives." type))

#+Genera (zwei:defindentation (map-over-table-elements 2 1))
(defmethod map-over-table-elements (function (record output-record-mixin) element-type)
  ;; Function is applied to each inferior of type element-type.  Error checking is
  ;; done to verify that the inferiors are, in fact, of type element-type.
  (declare (dynamic-extent function))
  (flet ((map-table-1 (child)
           (map-over-table-elements-helper function child element-type)))
    (declare (dynamic-extent #'map-table-1))
    (map-over-output-records #'map-table-1 record)))

#+Genera (zwei:defindentation (map-over-table-rows-or-columns 1 1))
(defmethod map-over-table-rows-or-columns (function (table standard-table-output-record))
  (declare (dynamic-extent function))
  (map-over-table-elements function table :row-or-column))

#+Genera (zwei:defindentation (map-over-table-rows 1 1))
(defmethod map-over-table-rows (function (table standard-table-output-record))
  (declare (dynamic-extent function))
  (map-over-table-elements function table :row))

#+Genera (zwei:defindentation (map-over-row-cells 1 1))
(defmethod map-over-row-cells (function (row standard-row-output-record))
  (declare (dynamic-extent function))
  (map-over-table-elements function row :cell))

#+Genera (zwei:defindentation (map-over-table-columns 1 1))
(defmethod map-over-table-columns (function (table standard-table-output-record))
  (declare (dynamic-extent function))
  (map-over-table-elements function table :column))

;; We call it MAP-OVER-ROW-CELLS for convenience sake, since a column is just
;; a row tilted on its end.
(defmethod map-over-row-cells (function (column standard-column-output-record))
  (declare (dynamic-extent function))
  (map-over-table-elements function column :cell))

(defmethod tree-recompute-extent-1 ((record output-record-element-mixin))
  (bounding-rectangle* record))

;; Cells have been positioned manually, probably.
;; Also, this should probably be an :around method so that we can
;; drag the circle inside a cell and have the cell get updated automatically.
;; Some extra bit will be needed.
;; Until we do this, only table formatting really works (and arbitrary dragging
;; of things above cells.
(defmethod tree-recompute-extent-1 ((record standard-cell-output-record))
  (bounding-rectangle* record))

(defmethod tree-recompute-extent-1 ((record output-record-mixin))
  (let ((once nil)
        (min-x (coordinate 0))
        (min-y (coordinate 0))
        (max-x (coordinate 0))
        (max-y (coordinate 0)))
    (declare (type coordinate min-x min-y max-x max-y))
    (multiple-value-bind (x-offset y-offset)
        (convert-from-child-to-parent-coordinates record)
      (declare (type coordinate x-offset y-offset))
      (flet ((recompute-extent-of-child (child)
               (multiple-value-bind (left top right bottom)
                   (tree-recompute-extent-1 child)
                 (declare (type coordinate left top right bottom))
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
      #+++ignore (assert (ltrb-well-formed-p min-x min-y max-x max-y))
      (translate-coordinates x-offset y-offset min-x min-y max-x max-y)
      (bounding-rectangle-set-edges record min-x min-y max-x max-y)
      (values min-x min-y max-x max-y))))

;;--- These are here 'cause the Lucid Sun 4 compiler can't find
;;--- the MACROLETted versions in the functions below.
;;--- The MCL compiler gets it even worse!
#+(or Lucid CCL-2)
(progn
(defmacro row-max-height (row-number)
  `(svref row-array ,row-number))
(defmacro column-max-width (column-number)
  `(svref column-array ,column-number))
(defmacro row-height (row-number)
  `(svref row-array ,row-number))
(defmacro column-width (column-number)
  `(svref column-array ,column-number))
)

(defun compute-row-table-p (table)
  ;; Find the first thing
  (flet ((find-row-or-column (row-or-column)
           (return-from compute-row-table-p
             (values (row-output-record-p row-or-column)))))
    (declare (dynamic-extent #'find-row-or-column))
    (map-over-table-rows-or-columns #'find-row-or-column table)))

(defmethod row-table-p ((table standard-table-output-record))
  (with-slots (row-table-p) table
    (cond ((not (eq row-table-p ':unknown))
           row-table-p)
          (t
           (setq row-table-p (compute-row-table-p table))))))

;; How can we make compute-output-size work without saving both an ink-rectangle
;; and a whitespace rectangle for each output record?
;; If we did that, table formatting would work much better as well.
(defmethod adjust-table-cells ((table standard-table-output-record) stream)
  (let* ((nrows 0)
         (ncells nil)
         (cells 0)
         (row-table-p (row-table-p table))
         (table-mapper (if row-table-p #'map-over-table-rows #'map-over-table-columns))
         (x-spacing (slot-value table 'x-spacing))
         (y-spacing (slot-value table 'y-spacing))
         (equalize-column-widths (slot-value table 'equalize-column-widths)))
    (declare (type fixnum nrows cells))
    (declare (type coordinate x-spacing y-spacing))
    (labels ((count-rows (row)
               (incf nrows)
               (setq cells 0)
               (map-over-row-cells #'count-cells row)
               (assert (not (zerop cells)) ()
		 "Row or column in table does not contain any cells")
               (cond ((null ncells)
                      (setq ncells cells))
                     (t
                      (maxf ncells cells))))
             (count-cells (cell)
               (assert (cell-output-record-p cell))
               (incf cells)))
      (declare (dynamic-extent #'count-rows #'count-cells))
      ;; Calculate nrows & ncells (= ncells per row)
      (funcall table-mapper #'count-rows table))
    ;; If there are no rows, COUNT-ROWS won't get invoked and NCELLS
    ;; will be NIL.  If all the rows and columns are empty, NCELLS will
    ;; be 0.  In either case, that means we're done.
    (when (or (null ncells) (= ncells 0))
      (return-from adjust-table-cells
        (tree-recompute-extent table)))
    (with-stack-array (row-array nrows :initial-element 0)
      (with-stack-array (column-array ncells :initial-element 0)
        (let ((x-pos nil)
              (y-pos nil)
              (row-count 0)
              (column-count 0)
              (total-width (coordinate 0))
              (total-height (coordinate 0)))
          (declare (type fixnum row-count column-count))
          (declare (type coordinate total-width total-height))
	  ;; We always want the table to start at its START-X and START-Y positions.
          (multiple-value-setq (x-pos y-pos) (output-record-position table))
          (macrolet (#-CCL-2 (row-max-height (row-number)
                               `(svref row-array ,row-number))
			     #-CCL-2 (column-max-width (column-number)
				       `(svref column-array ,column-number)))
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
                     (adjust-table-cells row stream)
                     (flet ((cell-mapper (cell)
                              (if row-table-p (incf column-count) (incf row-count))
                              (multiple-value-bind (width height)
                                  (bounding-rectangle-size cell)
                                (declare (type coordinate width height))
                                (maxf (row-max-height row-count)
                                      (max height (cell-min-height cell)))
                                (maxf (column-max-width column-count)
                                      (max width (cell-min-width cell))))))
                       (declare (dynamic-extent #'cell-mapper))
                       (map-over-row-cells #'cell-mapper row))))
              (declare (dynamic-extent #'row-mapper))
              (funcall table-mapper #'row-mapper table))
            (when equalize-column-widths
              (let ((column-width (coordinate 0))
                    (n-columns (1+ column-count)))
                (declare (type fixnum n-columns))
                (declare (type coordinate column-width))
                (dotimes (i n-columns)
                  (maxf column-width (column-max-width i)))
                (dotimes (i n-columns)
                  (setf (column-max-width i) column-width))))
            (if row-table-p (setq row-count -1) (setq column-count -1))
            (flet ((row-mapper (row)
                     (if row-table-p (incf row-count) (incf column-count))
                     (let ((this-row-height (row-max-height row-count))
                           (this-column-width (column-max-width column-count)))
                       (declare (type coordinate this-row-height this-column-width))
		       ;; All numbers are in (output-record-parent table) coordinates
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
                                  (declare (type coordinate column-width row-height
						 cell-width cell-height))
                                  (ecase (slot-value cell 'x-alignment)
                                    (:left )
                                    (:right
				     (setq x-alignment-adjust
				       (- column-width cell-width)))
                                    (:center
				     (setq x-alignment-adjust
				       (floor (- column-width cell-width) 2))))
                                  (ecase (slot-value cell 'y-alignment)
                                    (:top )
                                    (:bottom
				     (setq y-alignment-adjust
				       (- row-height cell-height)))
                                    (:center
				     (setq y-alignment-adjust
				       (floor (- row-height cell-height) 2))))
                                  (multiple-value-bind (x-offset y-offset)
                                      (convert-from-ancestor-to-descendant-coordinates
				       (output-record-parent table) (output-record-parent cell))
                                    (declare (type coordinate x-offset y-offset))
				    ;;; Make sure output-record of a row fills
				    ;;; the entire row height.
				    ;;; The reason for this is that the code that
				    ;;; calculates the total-extent of the table
				    ;;; uses the size and position of the child-cells.
				    ;;; As a result, the bottom row is drawn only
				    ;;; to its minimal height.
				    (when row-table-p
				      (let ((old-width (bounding-rectangle-width cell))) 
					(bounding-rectangle-set-size 
					 cell
					 old-width
					 this-row-height)))
                                    (output-record-set-position
				     cell
				     (+ x-offset total-width  x-alignment-adjust)
				     (+ y-offset total-height y-alignment-adjust)))
                                  (if row-table-p
                                      (incf total-width (+ column-width x-spacing))
                                    (incf total-height (+ row-height y-spacing))))))
                         (declare (dynamic-extent #'cell-mapper))
                         (map-over-row-cells #'cell-mapper row))
                       (if row-table-p
                           (incf y-pos (+ this-row-height y-spacing))
			 (incf x-pos (+ this-column-width x-spacing))))))
              (declare (dynamic-extent #'row-mapper))
              (funcall table-mapper #'row-mapper table)))))))
  (tree-recompute-extent table))

;; Table has already been laid out.
(defmethod adjust-multiple-columns ((table standard-table-output-record) stream
                                    &optional n-columns x-spacing)
  (let ((row-count 0))
    (declare (type fixnum row-count))
    (when (slot-value table 'row-table-p)
      (flet ((row-counter (row)
               (declare (ignore row))
               (incf row-count)))
        (declare (dynamic-extent #'row-counter))
        (map-over-table-rows #'row-counter table))
      (when (> row-count 5)
        (with-bounding-rectangle* (tleft ttop tright tbottom) table
          (multiple-value-bind (stream-width stream-height)
              (bounding-rectangle-size stream)
            (declare (type coordinate stream-width)
                     (ignore stream-height)) ;for now
	    tbottom
	    ttop
            (let* ((table-width (- tright tleft))
                   #+ignore (table-height (- tbottom ttop))
                   (between-column-margin
                     (if x-spacing
                         (process-spacing-arg stream x-spacing
                                              'formatting-table
                                              ':multiple-columns-x-spacing)
                         (stream-string-width stream " ")))
                   (column-width (+ table-width between-column-margin))
                   (possible-columns (or n-columns
                                         (max (floor stream-width column-width) 1)))
                   (y-spacing (slot-value table 'y-spacing))
                   (rows-per-column (max 3 (ceiling row-count possible-columns)))
                   (row-number 0)
                   (row-x (coordinate 0))
                   (row-y (coordinate 0)))
              (declare (type coordinate table-width between-column-margin
                                        column-width row-x row-y))
              (declare (type fixnum possible-columns rows-per-column row-number))
              (flet ((layout-multiple-columns (row)
                       (multiple-value-bind (rl rt) (bounding-rectangle-position row)
                         (multiple-value-bind (xoff yoff)
                             (convert-from-descendant-to-ancestor-coordinates
                               row (output-record-parent table))
                           (declare (type coordinate xoff yoff))
                           (translate-coordinates xoff yoff rl rt))
                         ;; Position the row so that the X position relative to the
                         ;; original table is preserved, so that :ALIGN-X :RIGHT works
                         ;;--- ROW-Y needs the same treatment for :ALIGN-Y
                         (output-record-set-position row (+ row-x (- rl tleft)) row-y)
                         (incf row-number)
                         (incf row-y (+ (bounding-rectangle-height row) y-spacing))
                         (when (zerop (mod row-number rows-per-column))
                           (setq row-x (+ row-x column-width)
                                 row-y (coordinate 0))))))
                (declare (dynamic-extent #'layout-multiple-columns))
                (map-over-table-rows #'layout-multiple-columns table)))))
        (tree-recompute-extent table)))))

(defmethod adjust-table-cells ((row standard-row-output-record) stream)
  (declare (ignore stream))
  #+++ignore
  (let ((x-spacing (slot-value (output-record-parent row) 'x-spacing))
        (x-position (coordinate 0)))
    (map-over-row-cells
      #'(lambda (cell)
          (with-bounding-rectangle* (left top right bottom) cell
            (declare (ignore bottom))
            (output-record-set-position cell x-position top)
            (incf x-position (+ (- right left) x-spacing))))
      row)))

(defmethod adjust-table-cells ((column standard-column-output-record) stream)
  (declare (ignore stream))
  #+++ignore
  (let ((x-spacing (slot-value (output-record-parent column) 'x-spacing))
        (x-position (coordinate 0)))
    (map-over-row-cells
      #'(lambda (cell)
          (with-bounding-rectangle* (left top right bottom) cell
            (declare (ignore bottom))
            (output-record-set-position cell x-position top)
            (incf x-position (+ (- right left) x-spacing))))
      column)))

(defun process-spacing-arg (stream spacing form &optional clause)
  (cond ((null spacing) nil)
        ((integerp spacing)
         (coordinate spacing))
        ((stringp spacing)
         (coordinate (stream-string-width stream spacing)))
        ((characterp spacing)
         (coordinate (stream-character-width stream spacing)))
        ((or (symbolp spacing) (functionp spacing))
         (coordinate (funcall spacing stream)))
        ((listp spacing)
         (let ((units (second spacing))
               (number (first spacing)))
           (coordinate
             (ecase units
               (:point (round (* number (graft-pixels-per-point (graft stream)))))
               ((:pixel :device) number)
               ;; Which character?  Width or height?
               (:character (* number (stream-character-width stream #\0)))
               (:line (+ (* number (stream-line-height stream))
                         (* (1- number) (stream-vertical-spacing stream))))))))
        (t (error "The ~:[~;~S ~]spacing specification, ~S, to ~S was invalid"
                  clause clause spacing form))))

;; FORMATTING-CELL macro is in FORMATTED-OUTPUT-DEFS
(defmethod invoke-formatting-cell ((stream output-protocol-mixin) continuation
                                   &rest initargs
                                   &key (align-x ':left) (align-y ':top)
                                        min-width min-height
                                        (record-type 'standard-cell-output-record)
                                   &allow-other-keys)
  (declare (dynamic-extent initargs))
  (with-keywords-removed (initargs initargs '(:record-type :align-x :align-y
                                              :min-width :min-height))
    (setq min-width (or (process-spacing-arg
                          stream min-width 'formatting-cell :min-width)
                        (coordinate 0))
          min-height (or (process-spacing-arg
                           stream min-height 'formatting-cell :min-height)
                         (coordinate 0)))
    ;; Jump through a hoop to get a constant record-type symbol into the
    ;; WITH-NEW-OUTPUT-RECORD macro so we invoke a fast constructor instead
    ;; of a slow MAKE-INSTANCE.  If this body was just expanded inline in
    ;; FORMATTING-CELL, it would just work to slip the IF and simply call
    ;; INVOKE-WITH-NEW-OUTPUT-RECORD...  Too bad.
    (let ((stream (encapsulating-stream stream)))
      (with-stream-cursor-position-saved (stream)
        (flet ((invoke-formatting-cell-1 (record)
                 (declare (ignore record))
                 (funcall continuation stream)))
          (declare (dynamic-extent #'invoke-formatting-cell-1))
          (if (eq record-type 'standard-cell-output-record)
              (apply #'invoke-with-new-output-record
                     stream #'invoke-formatting-cell-1
                     'standard-cell-output-record 'standard-cell-output-record-constructor
                     :align-x align-x :align-y align-y
                     :min-width min-width :min-height min-height
                     initargs)
              (apply #'invoke-with-new-output-record
                     stream #'invoke-formatting-cell-1 record-type nil
                     :align-x align-x :align-y align-y
                     :min-width min-width :min-height min-height
                     initargs)))))))

(defmethod invoke-formatting-cell :around ((stream output-recording-mixin) continuation
                                           &rest options)
  #-aclpc (declare (ignore continuation options))
  (letf-globally (((stream-text-output-record stream) nil))
    (call-next-method)))


;;; Item lists

(define-protocol-class item-list-output-record (output-record))

(defclass standard-item-list-output-record
          (standard-sequence-output-record item-list-output-record)
     ((x-spacing :initarg :x-spacing)
      (y-spacing :initarg :y-spacing)
      (initial-spacing :initarg :initial-spacing)
      (n-columns :initarg :n-columns)
      (n-rows :initarg :n-rows)
      (max-width :initarg :max-width)
      (max-height :initarg :max-height)
      (stream-width :initarg :stream-width)
      (stream-height :initarg :stream-height)
      (row-wise :initarg :row-wise))
  (:default-initargs :size 25 :n-columns nil :n-rows nil
                     :max-width nil :max-height nil
                     :stream-width nil :stream-height nil
                     :row-wise t))

(define-output-record-constructor standard-item-list-output-record
                                  (&key x-position y-position x-spacing y-spacing
                                        initial-spacing n-columns n-rows
                                        max-width max-height stream-width stream-height
                                        (row-wise t) (size 25))
  :x-position x-position :y-position y-position
  :x-spacing x-spacing :y-spacing y-spacing :initial-spacing initial-spacing
  :n-columns n-columns :n-rows n-rows :max-width max-width :max-height max-height
  :stream-width stream-width :stream-height stream-height :row-wise row-wise :size size)

(defmethod children-never-overlap-p ((record standard-item-list-output-record)) t)

#+Genera (zwei:defindentation (map-over-item-list-cells 1 1))
(defmethod map-over-item-list-cells ((menu standard-item-list-output-record) function)
  (declare (dynamic-extent function))
  (map-over-table-elements function menu :cell))

;; If we had our hands on a stream, we could use twice the width of #\Space
(defvar *default-minimum-menu-x-spacing* (coordinate 10))

(defmethod adjust-table-cells ((menu standard-item-list-output-record) stream)
  ;; We will set the local variables NROWS and NCOLUMNS below, but we never
  ;; change the slot values themselves, because that will break redisplay.
  ;; Ditto, X-SPACING and Y-SPACING.
  (let ((ncolumns (slot-value menu 'n-columns))
        (nrows (slot-value menu 'n-rows))
        (ncells 0)
        (max-cell-width (coordinate 0))
        (max-cell-height (coordinate 0))
        (x-spacing (slot-value menu 'x-spacing))
        (y-spacing (slot-value menu 'y-spacing))
        (initial-spacing (slot-value menu 'initial-spacing))
        (max-width (slot-value menu 'max-width))        ;don't make the menu
        (max-height (slot-value menu 'max-height))        ; exceed these bounds
        (row-wise (slot-value menu 'row-wise))
        (constrain t)
	;;(preferred-geometry :column)                ;vertical menu
        (golden-ratio 1.6))
    (declare (type fixnum ncells))
    (declare (type coordinate max-cell-width max-cell-height))
    (flet ((count-cells (cell)
             (multiple-value-bind (width height) (bounding-rectangle-size cell)
               (declare (type coordinate width height))
               (maxf max-cell-width (max width (cell-min-width cell)))
               (maxf max-cell-height (max height (cell-min-height cell)))
               (incf ncells))))
      (declare (dynamic-extent #'count-cells))
      (map-over-item-list-cells menu #'count-cells))
    ;;--- Perhaps we should check that NCELLS is not zero?
    ;;--- PREFERRED-GEOMETRY isn't used yet
    ;; What's the preferred orientation of the menu?
    (unless (or nrows ncolumns)
      ;; Anything explicit overrides
      (when constrain                                ;orientation like window orientation
        (let ((swidth (slot-value menu 'stream-width))
              (sheight (slot-value menu 'stream-height)))
          (when (and swidth sheight)
            (when (> (/ swidth sheight) golden-ratio)
              ;; When the stream is wider than it is high by more than the golden
              ;; ratio, make the preferred ordering :row
	      ;;(setq preferred-geometry :row)
	      )))))
    ;; Compute geometry
    (cond (ncolumns
           (setq nrows (max 1 (ceiling (/ ncells ncolumns)))))
          (nrows
           (setq ncolumns (max 1 (ceiling (/ ncells nrows)))))
          (max-height
           ;; Could compute this better
           (setq nrows
                 (max 1
                      (let ((acc-height (coordinate 0))
                            (count 0))
                        (loop
                          (incf acc-height max-cell-height)
                          (when (> acc-height max-height)
                            (return count))
                          (incf count)
                          (incf acc-height y-spacing)))))
           (setq ncolumns (max 1 (ceiling (/ ncells nrows)))))
          (max-width
           (let ((spacing (or x-spacing *default-minimum-menu-x-spacing*)))
             (setq ncolumns (or (block try-one-row
                                  (let ((acc-width spacing))        ;left margin
                                    (flet ((sum-width (cell)
                                             (incf acc-width
                                                   (+ (bounding-rectangle-width cell) spacing))
                                             (when (> acc-width max-width)
                                               (return-from try-one-row nil))))
                                      (declare (dynamic-extent #'sum-width))
                                      (map-over-item-list-cells menu #'sum-width))
                                    (max ncells 1)))
                                ;; Won't fit in one row, use a more conservative computation
                                ;; that uses max-cell-width instead of the actual widths
                                ;; This could still be more accurate than it is.
                                (max 1 (let ((acc-width spacing)        ;left margin
                                             (count 0))
                                         (loop
                                           (incf acc-width (+ max-cell-width spacing))
                                           (when (> acc-width max-width)
                                             (return count))
                                           (incf count))))))
             (setq nrows (max 1 (ceiling (/ ncells ncolumns))))))
          (t
           ;; Try to make this a golden-ratio menu
           ;; Deduce golden ratio from other parameters
           (setq ncolumns (max 1 (floor (sqrt (/ (* ncells
                                                    max-cell-width
                                                    max-cell-height)
                                                 1.6))
                                        (max max-cell-width 1))))
           (setq nrows (max 1 (ceiling (/ ncells ncolumns))))))
    (with-stack-array (row-array nrows :initial-element 0)
      (with-stack-array (column-array ncolumns :initial-element 0)
        (let ((row-count 0)
              (column-count 0))
          (declare (type fixnum row-count column-count))
          (macrolet (#-CCL-2 (row-height (row-number)
                               `(svref row-array ,row-number))
                     #-CCL-2 (column-width (column-number)
                               `(svref column-array ,column-number)))
            ;; Collect row heights and column widths into temp arrays.
            ;; We need to remember for each row its total height and
            ;; the difference between the smallest top and the largest top.
            ;; For each row remember the total height and then remember the maximum
            ;; difference between the row top and the y-position of the row.
            (flet ((size-cells (cell)
                     (multiple-value-bind (width height) (bounding-rectangle-size cell)
                       (declare (type coordinate width height))
                       (maxf (column-width column-count)
                             (max width (cell-min-width cell)))
                       (maxf (row-height row-count)
                             (max height (cell-min-height cell))))
                     (cond (row-wise
                            (incf column-count)
                            (when (= column-count ncolumns)
                              (incf row-count)
                              (setq column-count 0)))
                           (t
                            (incf row-count)
                            (when (= row-count nrows)
                              (incf column-count)
                              (setq row-count 0))))))
              (declare (dynamic-extent #'size-cells))
              (map-over-item-list-cells menu #'size-cells))
            ;; Now default the x-spacing to a spacing that spreads the
            ;; columns evenly over the entire width of the menu
            (unless x-spacing
              (setq x-spacing
                    (if max-width
                        (let ((accumulated-width (coordinate 0)))
                          (declare (type coordinate accumulated-width))
                          (dotimes (column ncolumns)
                            (incf accumulated-width (column-width column)))
                          (floor (- max-width accumulated-width) (1+ ncolumns)))
                        *default-minimum-menu-x-spacing*)))
            (setq row-count 0
                  column-count 0)
            (multiple-value-bind (left-margin top-margin) (bounding-rectangle-position menu)
              (declare (type coordinate left-margin top-margin))
              (let ((accumulated-height (coordinate 0))
                    (accumulated-width
                      (if (or (stream-redisplaying-p stream)
                              (not initial-spacing))
                          (coordinate 0)
                          (coordinate x-spacing))))
                (declare (type coordinate accumulated-height accumulated-width))
                (flet ((adjust-cells (cell)
                         (let ((column-width (column-width column-count))
                               (row-height (row-height row-count))
                               (cell-width (bounding-rectangle-width cell))
                               (cell-height (bounding-rectangle-height cell))
                               (x-alignment-adjust 0)
                               (y-alignment-adjust 0))
                           (declare (type coordinate column-width row-height
                                          cell-width cell-height))
                           (ecase (slot-value cell 'x-alignment)
                             (:left )
                             (:right
                               (setq x-alignment-adjust
                                     (- column-width cell-width)))
                             (:center
                               (setq x-alignment-adjust
                                     (floor (- column-width cell-width) 2))))
                           (ecase (slot-value cell 'y-alignment)
                             (:top )
                             (:bottom
                               (setq y-alignment-adjust
                                     (- row-height cell-height)))
                             (:center
                               (setq y-alignment-adjust
                                     (floor (- row-height cell-height) 2))))
                           (multiple-value-bind (x-offset y-offset)
                               (convert-from-ancestor-to-descendant-coordinates
                                 (output-record-parent menu) (output-record-parent cell))
                             (declare (type coordinate x-offset y-offset))
                             (output-record-set-position
                               cell
                               (+ x-offset left-margin accumulated-width x-alignment-adjust)
                               (+ y-offset top-margin accumulated-height y-alignment-adjust))))
                         (cond (row-wise
                                (incf accumulated-width (column-width column-count))
                                (incf accumulated-width x-spacing)
                                (incf column-count)
                                (when (= column-count ncolumns)
                                  (setq accumulated-width
                                        (if (or (stream-redisplaying-p stream)
                                                (not initial-spacing))
                                            (coordinate 0)
                                            (coordinate x-spacing)))
                                  (incf accumulated-height (row-height row-count))
                                  (incf accumulated-height y-spacing)
                                  (setq column-count 0)
                                  (incf row-count)))
                               (t
                                (incf accumulated-height (row-height row-count))
                                (incf accumulated-height y-spacing)
                                (incf row-count)
                                (when (= row-count nrows)
                                  (setq accumulated-height 0)
                                  (incf accumulated-width (column-width column-count))
                                  (incf accumulated-width x-spacing)
                                  (setq row-count 0)
                                  (incf column-count))))))
                  (declare (dynamic-extent #'adjust-cells))
                  (map-over-item-list-cells menu #'adjust-cells)))))))))
  (tree-recompute-extent menu))

;; FORMATTING-ITEM-LIST macro is in FORMATTED-OUTPUT-DEFS
(defun invoke-formatting-item-list (stream continuation
                                    &rest initargs
                                    &key x-spacing y-spacing initial-spacing
                                         n-columns n-rows
                                         max-width max-height stream-width stream-height
                                         (row-wise t) (move-cursor t)
                                         (record-type 'standard-item-list-output-record)
                                    &allow-other-keys)
  (declare (dynamic-extent initargs))
  (with-keywords-removed (initargs initargs '(:x-spacing :y-spacing :initial-spacing
                                              :n-columns :n-rows :max-width :max-height
                                              :stream-width :stream-height
                                              :row-wise :move-cursor :record-type))
    (let ((menu
            (with-output-recording-options (stream :draw nil :record t)
              (with-end-of-line-action (stream :allow)
                (with-end-of-page-action (stream :allow)
                  (flet ((invoke-formatting-item-list-1 (record)
                           (declare (ignore record))
                           (funcall continuation stream)))
                    (declare (dynamic-extent #'invoke-formatting-item-list-1))
                    (apply #'invoke-with-new-output-record
                           stream #'invoke-formatting-item-list-1 record-type nil
                           :n-columns n-columns :n-rows n-rows
                           :max-width max-width :max-height max-height
                           :stream-width stream-width :stream-height stream-height
                           :x-spacing
                             (process-spacing-arg stream x-spacing
                                                  'formatting-item-list ':x-spacing)
                           :y-spacing
                             (or (process-spacing-arg stream y-spacing
                                                      'formatting-item-list ':y-spacing)
                                 (stream-vertical-spacing stream))
                           :initial-spacing initial-spacing
                           :row-wise row-wise
                           initargs)))))))
      (adjust-table-cells menu stream)
      (replay menu stream)
      (when move-cursor
        (move-cursor-beyond-output-record stream menu))
      menu)))

(defun format-items (items &key (stream *standard-output*) printer presentation-type
                                x-spacing y-spacing initial-spacing (row-wise t)
                                n-rows n-columns max-width max-height
                                (record-type 'standard-item-list-output-record)
                                (cell-align-x ':left) (cell-align-y ':top))
  (when (and printer presentation-type)
    (error "Only one of ~S or ~S can be specified." ':printer ':presentation-type))
  (when (and (null printer) (null presentation-type))
    (error "One of ~S or ~S must be specified." ':printer ':presentation-type))
  (formatting-item-list (stream :record-type record-type
                                :n-rows n-rows :n-columns n-columns
                                :max-width max-width :max-height max-height
                                :x-spacing x-spacing :y-spacing y-spacing
                                :initial-spacing initial-spacing
                                :row-wise row-wise)
    (flet ((format-item (item)
             (formatting-cell (stream :align-x cell-align-x :align-y cell-align-y)
               (cond (printer
                      (funcall printer item stream))
                     (presentation-type
                      (present item presentation-type :stream stream))))))
      (declare (dynamic-extent #'format-item))
      (map nil #'format-item items))))
