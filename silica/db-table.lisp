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



(defclass table-pane (mute-input-mixin
		      pane-background-mixin
		      composite-pane 
		      space-req-cache-mixin)
	  (row-space-reqs column-space-reqs contents))


(defmethod initialize-instance :after ((pane table-pane) &key contents)
  ;; The contents should be a list of lists
  (with-slots ((c contents)) pane
    (setf c (make-array (list (length contents)
			      (length (car contents)))
			:initial-contents contents)))
  (dolist (c contents)
    (dolist (d c)
      (when d (adopt-child pane d)))))


(defmacro tabling (options &rest contents)
  `(realize-pane 'table-pane
		 :contents (list ,@(mapcar #'(lambda (x) `(list ,@x)) contents))
		 ,@options))

#+ignore
(tabling (:space space)
		 (vscrollbar viewport)
		 (nil        hscrollbar))

(defmethod compose-space ((x table-pane))
  (with-slots (contents) x
    (let ((omin-w 0)
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
	      (max-h 0))
	  (dotimes (column (array-dimension contents 1))
	    (let ((item (aref contents row column)))
	      (when item
		(let ((isr (compose-space item)))
		  ;; Max the heights
		  (maxf h (space-req-height isr))
		  (maxf min-h (space-req-min-height isr))
		  (maxf max-h (space-req-max-height isr))))))

	  (push
	   (make-space-req
	    :width 0
	    :min-height min-h :height h :max-height max-h)
	   row-srs)
	  
	  ;; Add the heights
	  (incf oh h)
	  (incf omin-h min-h)
	  (incf omax-h max-h)))
      
      (setf (slot-value x 'row-space-reqs) (nreverse row-srs))
      
      ;; Iterate over the columns determing the widths of each
            
      (dotimes (column (array-dimension contents 1))
	(let ((min-w 0)
	      (w 0)
	      (max-w 0))
	
	  (dotimes (row (array-dimension contents 0))
	    (let ((item (aref contents row column)))
	      (when item
		(let ((isr (compose-space item)))
		  ;; Max the widths
		  (maxf w (space-req-width isr))
		  (maxf min-w (space-req-min-width isr))
		  (maxf max-w (space-req-max-width isr))))))

	  (push
	   (make-space-req
	    :min-width min-w :width w :max-width max-w
	    :height 0)
	   column-srs)
	
	  (incf ow w)
	  (incf omin-w min-w)
	  (incf omax-w max-w)))
      
      
      (setf (slot-value x 'column-space-reqs) (nreverse column-srs))
      
      (make-space-req
       :min-width omin-w :width ow :max-width omax-w
       :min-height omin-h :height oh :max-height omax-h))))

(defmethod allocate-space ((x table-pane) width height)
  (with-slots (contents column-space-reqs 
			row-space-reqs space-req) x
    (unless space-req (compose-space x))
    
    (let ((row-heights
	   (allocate-space-to-items
	    height
	    space-req
	    row-space-reqs
	    #'space-req-min-height
	    #'space-req-height
	    #'space-req-max-height
	    #'identity))
	  (column-widths
	   (allocate-space-to-items
	    width
	    space-req
	    column-space-reqs
	    #'space-req-min-width
	    #'space-req-width
	    #'space-req-max-width
	    #'identity))
	  (y 0))
      (dotimes (row (array-dimension contents 0))
	(let ((row-height (pop row-heights))
	      (column-widths column-widths)
	      (x 0))
	  (dotimes (column (array-dimension contents 1))
	    (let ((column-width (pop column-widths))
		  (item (aref contents row column)))
	      (when item
		(move-and-resize-sheet*
		 item x y 
		 (min column-width (1- (- width x)))
		 (min row-height (1- (- height y)))))
	      (incf x column-width)))
	  (incf y row-height))))))
