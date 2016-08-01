;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :silica)

;;;"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 Xerox Corp.  All rights reserved."


;;;
;;; Table
;;;

(defclass table-pane (layout-pane)
    (row-space-requirements
     column-space-requirements
     contents))


(defmethod initialize-instance :after ((pane table-pane) &key contents)
  ;; The contents should be a list of lists
  (setf (slot-value pane 'contents)
        (make-array (list (length contents)
                          (length (car contents)))
                    :initial-contents contents))
  (dolist (row contents)                ;a row of panes, that is
    (dolist (cell row)                        ;a cell is one pane in a row
      (when cell
        (sheet-adopt-child pane cell)))))

(defmethod handle-event :after ((pane table-pane) (event pointer-motion-event))
  (deallocate-event event))

(defmacro tabling ((&rest options) &body contents)
  `(make-pane 'table-pane
     :contents (list ,@(mapcar #'(lambda (x) `(list ,@x)) contents))
     ,@options))

(defmethod compose-space ((table table-pane) &key width height)
  (declare (ignore width height))
  (let ((contents (slot-value table 'contents))
        (omin-w 0)
        (omin-h 0)
        (oh 0)
        (ow 0)
        (omax-h 0)
        (omax-w 0)
        row-srs
        column-srs)
    ;; Iterate over the rows, determining the height of each
    (dotimes (row (array-dimension contents 0))
      (let ((min-h 0)
            (h 0)
            (max-h nil))
        (dotimes (column (array-dimension contents 1))
          (let ((item (aref contents row column)))
            (when (and item
                       (sheet-enabled-p item))
              (let ((isr (compose-space item)))
                ;; Max the heights
                (maxf h (space-requirement-height isr))
                (maxf min-h (space-requirement-min-height isr))
                ;; should this be min or max
                (if max-h
                    #+(or aclpc acl86win32)     ;mm: try max  ??
                    (maxf max-h (space-requirement-max-height isr))
                    #-(or aclpc acl86win32)
                    (minf max-h (space-requirement-max-height isr))

                    (setf max-h (space-requirement-max-height isr)))))))
        (push (make-space-requirement
                :width 0
                :min-height min-h :height h :max-height (or max-h 0))
              row-srs)
        ;; Add the heights
        (incf oh h)
        (incf omin-h min-h)
        (incf omax-h (or max-h 0))))
    (setf (slot-value table 'row-space-requirements) (nreverse row-srs))
    ;; Iterate over the columns determing the widths of each
    (dotimes (column (array-dimension contents 1))
      (let ((min-w 0)
            (w 0)
            (max-w nil))
        (dotimes (row (array-dimension contents 0))
          (let ((item (aref contents row column)))
            (when (and item
                       (sheet-enabled-p item))
              (let ((isr (compose-space item)))
                ;; Max the widths
                (maxf w (space-requirement-width isr))
                (maxf min-w (space-requirement-min-width isr))
                ;; Should this be min or max???
                (if max-w
                    #+(or aclpc acl86win32)     ;mm: alpha said max   ???
                    (maxf max-w (space-requirement-max-width isr))
                    #-(or aclpc acl86win32)
                    (minf max-w (space-requirement-max-width isr))

                    (setf max-w (space-requirement-max-width isr)))))))
        (push (make-space-requirement
                :min-width min-w :width w :max-width (or max-w 0)
                :height 0)
              column-srs)
        (incf ow w)
        (incf omin-w min-w)
        (incf omax-w (or max-w 0))))
    (setf (slot-value table 'column-space-requirements) (nreverse column-srs))
    (make-space-requirement
      :min-width omin-w :width ow :max-width omax-w
      :min-height omin-h :height oh :max-height omax-h)))

(defmethod allocate-space ((table table-pane) width height)
  (with-slots (column-space-requirements row-space-requirements) table
    (let* ((space-requirement 
             (compose-space table :width width :height height))
           (contents (slot-value table 'contents))
           (row-heights
             (allocate-space-to-items
               height
               space-requirement
               row-space-requirements
               #'space-requirement-min-height
               #'space-requirement-height
               #'space-requirement-max-height
               #'identity))
           (column-widths
             (allocate-space-to-items
               width
               space-requirement
               column-space-requirements
               #'space-requirement-min-width
               #'space-requirement-width
               #'space-requirement-max-width
               #'identity))
           (y 0))
      (dotimes (row (array-dimension contents 0))
        (let ((row-height (pop row-heights))
              (column-widths column-widths)
              (x 0))
          (dotimes (column (array-dimension contents 1))
            (let ((column-width (pop column-widths))
                  (item (aref contents row column)))
              (when (and item
                         (sheet-enabled-p item))
                (move-and-resize-sheet
                  item x y 
                  column-width
                  row-height))
              (incf x column-width)))
          (incf y row-height))))))
