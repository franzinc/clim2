;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: stipples.lisp,v 1.3 92/01/31 14:58:44 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;; What is a "stipple"?  A stipple is a tiling of a two-dimensional
;; pattern of the foreground and background colors.

;; A list of all the stipples.
(defvar *stipple-arrays* nil)

;; a list of pairs; each pair is (stipple gray-level)
;; sorted in order of descending gray-level
(defvar *gray-level-stipple-arrays* `((,+foreground-ink+ 1) (,+background-ink+ 0)))

;; This is broken out so it can be used in CLX-IMPLEMENTATION
(defun make-stipple-array (height width patterns)
  (let ((array (make-array (list height width) :element-type 'bit)))
    ;; oh for LOOP!!
    (let ((h -1))
      (dolist (pattern patterns)
	(incf h)
	(let ((w width))
	  (dotimes (pos width)
	    (decf w)
	    (setf (aref array h w) (ldb (byte 1 pos) pattern))))))
    array))

(defun make-stipple (height width patterns)
  (assert (= height (length patterns)) (height patterns)
	  "Height should be same as number of patterns supplied")
  (check-type width (integer 0))
  ;; --- the issue of raster arrays is important here, and I am glossing over it.
  (make-rectangular-tile (make-pattern (make-stipple-array height width patterns)
				       (vector +background-ink+ +foreground-ink+))
			 width height))

;;; The argument is the level of +foreground-ink+, not the luminance
;;; Thus this function is not consistent with make-gray-color
(defun find-stipple-for-gray-level (gray-level)
  (dorest (stipples *gray-level-stipple-arrays*)
    (let* ((this-stipple-elt (first stipples))
	   (next-stipple-elt (second stipples))
	   (this-stipple (first this-stipple-elt))
	   (this-stipple-level (second this-stipple-elt))
	   (next-stipple (first next-stipple-elt))
	   (next-stipple-level (second next-stipple-elt)))
      (when (null next-stipple-elt)
	(return this-stipple))
      (when (>= gray-level next-stipple-level)
	(return
	  (let ((darker-diff (- this-stipple-level gray-level))
		(lighter-diff (- gray-level next-stipple-level)))
	    (if (> darker-diff lighter-diff)
		next-stipple this-stipple)))))))

;; Define stipples
#+Genera
(scl:defprop defstipple defvar zwei:definition-function-spec-type)

(defmacro defstipple (name (height width) options patterns)
  #+Genera (declare (scl:arglist name (height width) () patterns))
  `(defparameter ,name (defstipple-1 ,height ,width ',patterns ,@options)))

(defun defstipple-1 (height width patterns)
  (let ((stipple (make-stipple height width patterns)))
    (pushnew stipple *stipple-arrays*)
    stipple))

;;; Gray level stipples
;; convert gray-level to ratio
;; numerator is num pixels on
;; denom is total num pixels needed for stipple

(defun appropriate-dimensions-for-gray-stipple (gray-level)
  (declare (values num-per-row num-rows real-gray-level))
  (let* ((num-per-array (round (* gray-level 64)))
	 (num-per-row (/ num-per-array 8))
	 (real-gray-level (/ num-per-array 64)))
    (values num-per-row 8 real-gray-level)))

(defun make-gray-level-stipple (gray-level)
  (declare (values stipple real-gray-level))
  (multiple-value-bind (num-per-row num-rows new-gray-level)
      (appropriate-dimensions-for-gray-stipple gray-level)
    (let ((patterns (make-gray-level-stipple-patterns num-per-row num-rows)))
      (values
	(make-stipple num-rows num-rows patterns)
	new-gray-level))))

(defun make-gray-level-row-pattern (num-per-row num-rows)
  (cond ((> num-per-row (/ num-rows 2))
	 (ldb (byte num-rows 0)
	      (logxor -1 (make-gray-level-row-pattern-1 (- num-rows num-per-row) num-rows))))
	(t (make-gray-level-row-pattern-1 num-per-row num-rows))))

(defun make-gray-level-row-pattern-1 (num-per-row num-rows)
  ;; maximize whitespace between on bits
  ;; isn't this gray-level all over again???
  (if (zerop num-per-row) 
      0
      (let ((space-between-bits (/ num-rows num-per-row)))
	(let ((result 0)
	      (where 0))
	  (dotimes (i num-per-row)
	    #-(or Allegro Minima) (declare (ignore i))
	    (setq result
		  (dpb 1 (byte 1 (floor where)) result))
	    (incf where space-between-bits))
	  result))))

(defun rotate-pattern (pattern amount size)
  (let ((rot-pat (ash pattern amount)))
    (logior (ldb (byte size 0) rot-pat)
	    (ldb (byte size size) rot-pat))))

(defun best-amount-to-shift (num-rows)
  (declare (values shift-amount shift-period))
  (let ((oks nil))
    (dotimes (i (1+ (ceiling (/ num-rows 2))))	;over half is same as under half...
      (when (> i 1)
	(block check-it
	  (dotimes (j (1- num-rows))
	    (let ((jj (* (1+ j) i)))
	      (when (zerop (mod jj num-rows))
		(push (list i (1+ j)) oks)
		(return-from check-it)))))
	(push (list i num-rows) oks)))
    (let ((oks (remove 1 oks :key #'first)))
      ;; find "best looking" shift

      ;; best looking shift
      ;; is largest shift that's not a gcd of num-rows
      ;; whose period is the smallest gcd of num-rows.

      ;; if none match, then pick pattern with smallest period

      (let ((found-shifts nil))
	(dolist (ok oks)
	  (let ((shift (first ok))
		(period (second ok)))
	    (when (and (/= shift (gcd shift num-rows))
		       ;; don't want obvious straight lines
		       (/= (1+ shift) period))
	      ;; it's a candidate
	      (push ok found-shifts))))

	(flet ((doit (list predicate)
		 (let ((found-period nil))
		   (dolist (ok list)
		     (let ((shift (first ok))
			   (period (second ok)))
		       (declare (ignore shift))
		       (when (or (null found-period)
				 (funcall predicate period found-period))
			 (setq found-period period))))
		   ;; now we've found the period.
		   ;; if there is more than one shift for this period, pick the one
		   ;; in the "middle"
		   (let* ((new-list (remove-if-not #'(lambda (x)
						       (= (second x) found-period)) list
						   ))
			  (length (length new-list)))
		     (cond ((= length 1)
			    (values-list (first new-list)))
			   (t (let ((average nil)
				    (sum 0)
				    (count 0))
				(dolist (thing new-list)
				  (let ((first (first thing)))
				    (incf count)
				    (incf sum first)))
				(setq average (* (/ sum count) 3/4))
				(dolist (thing new-list)
				  (when (>= (first thing) average)
				    (return-from doit
				      (values-list thing)))))))))))
	  ;(format t "~S, ~S" oks found-shifts)
	  (cond (found-shifts
		 (doit found-shifts #'<))
		((null oks)
		 (values 1 1))			;---
		(t
		 (doit oks #'<))))))))

(defun make-gray-level-stipple-patterns (num-per-row num-rows)
  (let* ((pattern1 (make-gray-level-row-pattern (floor num-per-row) num-rows))
	 (pattern2 (make-gray-level-row-pattern (ceiling num-per-row) num-rows))
	 (gcd (gcd (ceiling num-per-row) num-rows))
	 (min-pattern (/ num-rows gcd)))
    (multiple-value-bind (amount-to-shift cutoff-threshold)
	(best-amount-to-shift num-rows)
      ;; This makes 1/4 gray look good in Coral
      (setq amount-to-shift (min amount-to-shift (round (/ min-pattern 2))))
    
      (let ((patterns nil)
	    (column-vector (make-array num-rows :element-type '(member t nil)))
	    (idx 0)
	    (residue 0))
	;; basically, the idea is that if we shift around to where we started from without
	;; putting something in each column, we increment the shift by 1 and continue.
	;; residue is the number of fractional bits that have been omitted so far.
	(dotimes (i num-rows)
	  (when (zerop (mod i min-pattern))
	    (fill column-vector nil)
	    (setq idx 0))
	  (block foo
	    (loop
	      (setq idx (mod idx min-pattern))
	      (cond ((and (< i cutoff-threshold) (aref column-vector idx))
		     (incf idx))
		    (t (return-from foo)))))
	  (let* ((pattern-to-shift (cond ((< residue 1/2)
					  pattern1)
					 (t	;put in an extra bit this time
					  (decf residue)
					  pattern2)))
		 (new-pattern (rotate-pattern pattern-to-shift idx num-rows)))
	    (incf residue (mod num-per-row 1))
	    (push new-pattern patterns)
	    (setf (aref column-vector idx) t))
	  (incf idx amount-to-shift))
	(nreverse patterns)))))

(defmacro define-gray-level-stipples (&rest gray-levels)
  `(progn
     (dolist (gray-level ',gray-levels)
       (multiple-value-bind (stipple real-gray-level)
	   (make-gray-level-stipple gray-level)
	 (push-unique (list stipple real-gray-level) *gray-level-stipple-arrays*
		      :key #'second :test #'=)
	 (setq *gray-level-stipple-arrays*
	       (sort *gray-level-stipple-arrays* #'> :key #'second))))))



;; Predefine stipples for common gray levels
(define-gray-level-stipples 
  1/2           ;50%
  1/4           ;25%
  3/4           ;75%
  1/3           ;33%
  1/8           ;12.5%
  1/10          ;10%
  1/11          ;9%
  1/12          ;8%
  1/14          ;7%
  1/16          ;6%
  1/18          ;5.5%
  )

(defstipple *tiles-stipple* (8 8) ()
	    (#b10000000
	     #b10000000 
	     #b01000001 
	     #b00111110
	     #b00001000
	     #b00001000 
	     #b00010100 
	     #b11100011))

(defstipple *hearts-stipple* (8 8) ()
	    (#b01101100
	     #b10010010 
	     #b10010010 
	     #b01000100
	     #b00101000
	     #b00010000 
	     #b00000000 
	     #b00000000))

(defstipple *parquet-stipple* (8 8) ()
	    (#b10000000
	     #b11000001 
	     #b00100010 
	     #b00011100
	     #b00001000
	     #b00010000
	     #b00100000
	     #b01000000))

(defstipple *bricks-stipple* (8 8) ()
	    (#b00010000
	     #b00010000
	     #b11111111
	     #b00000000
	     #b00000000
	     #b00000000
	     #b00000000
	     #b00000000
	     ))

