;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
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

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;; What is a "stipple"?  A stipple is a pattern stored in row major order.
;; The size of a stipple is implementation specific.

;; Since CL doesn't provide for :named-array-leader type defstructs,
;; there's no need to make stipples named structures.  Too bad, too,
;; because it's nice to be able to print the stipple and know what it
;; will "look like".  Sigh.

;; A list of all the stipples.
(defvar *stipple-arrays* nil)

;; a list of pairs; each pair is (stipple gray-level)
;; sorted in order of descending gray-level
(defvar *gray-level-stipple-arrays* '((t 1) (nil 0)))

(defclass stipple ()
    ((array :initarg :array :reader stipple-array)))

(defun make-stipple (height width patterns)
  (assert (= height (length patterns)) (height patterns)
	  "Height should be same as number of patterns supplied")
  (check-type width (integer 0))
  ;; --- the issue of raster arrays is important here, and I am glossing over it.
  (let ((array (make-array (list height width) :element-type 'bit)))
    ;; oh for LOOP!!
    (let ((h -1))
      (dolist (pattern patterns)
	(incf h)
	(let ((w width))
	  (dotimes (pos width)
	    (decf w)
	    (setf (aref array h w) (ldb (byte 1 pos) pattern))))))
    (make-instance 'stipple :array array)))

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

;;; This exists so implementations can allow "internal" objects as stipples.
(defgeneric stipple-dimensions (stipple))

(defmethod stipple-dimensions ((stipple stipple))
  #+Genera (declare (values width height))
  (with-slots (array) stipple
    (values (array-dimension array 1)
	    (array-dimension array 0))))

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
  #+Genera (declare (values num-per-row num-rows real-gray-level))
  (let* ((num-per-array (* gray-level 64))
	 (num-per-row (round (/ num-per-array 8)))
	 (real-gray-level (/ (* num-per-row 8) 64)))
    (values num-per-row 8 real-gray-level)))

(defun make-gray-level-stipple (gray-level)
  #+Genera (declare (values stipple real-gray-level))
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
	    #+Genera-release-8 (declare (ignore i))
	    (setq result
		  (dpb 1 (byte 1 (floor where)) result))
	    (incf where space-between-bits))
	  result))))

(defun rotate-pattern (pattern amount size)
  (let ((rot-pat (ash pattern amount)))
    (logior (ldb (byte size 0) rot-pat)
	    (ldb (byte size size) rot-pat))))

(defun best-amount-to-shift (num-rows)
  #+Genera (declare (values shift-amount shift-period))
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
  (let* ((pattern-to-shift (make-gray-level-row-pattern num-per-row num-rows))
	 (gcd (gcd num-per-row num-rows))
	 (min-pattern (/ num-rows gcd)))
    (multiple-value-bind (amount-to-shift cutoff-threshold)
	(best-amount-to-shift num-rows)

      ;; This makes 1/4 gray look good in #+:Coral
      (setq amount-to-shift (min amount-to-shift (round (/ min-pattern 2))))
    
      (let ((min-patterns nil)
	    (patterns nil)
	    (column-vector (make-array min-pattern :element-type '(member t nil)
				       :initial-element nil))
	    (idx 0))
	;; basically, the idea is that if we shift around to where we started from without
	;; putting something in each column, we increment the shift by 1 and continue.
	(dotimes (i min-pattern)
	  (block foo
	    (loop
	      (setq idx (mod idx min-pattern))
	      (cond ((and (< i cutoff-threshold) (aref column-vector idx))
		     (incf idx))
		    (t (return-from foo)))))
	  (let ((new-pattern (rotate-pattern pattern-to-shift idx num-rows)))
	    (push new-pattern min-patterns)
	    (setf (aref column-vector idx) t))
	  (incf idx amount-to-shift))
	(setq min-patterns (nreverse min-patterns))
	(dotimes (i gcd)
	  #+Genera-release-8 (declare (ignore i))
	  (setq patterns (append patterns min-patterns)))
	patterns))))

(defmacro define-gray-level-stipples (&rest gray-levels)
  `(progn
     (dolist (gray-level ',gray-levels)
       (multiple-value-bind (stipple real-gray-level)
	   (make-gray-level-stipple gray-level)
	 (declare (ignore real-gray-level))
	 (push-unique (list stipple (float gray-level)) *gray-level-stipple-arrays*
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


;;; Utilities
#||
(defun show-stipple (stipple &optional flip-p)
  (scl:destructuring-bind (height width)
      (array-dimensions stipple)
    (fresh-line)
    (scl:formatting-table (nil :inter-row-spacing 0 :inter-column-spacing 0)
      (dotimes (i height)
	(scl:formatting-row ()
	  (dotimes (j width)
	    (scl:formatting-cell ()
	      (unless (funcall (if flip-p #'(lambda (x) (= x 1)) #'zerop) (aref stipple i j))
		(multiple-value-bind (x y)
		    (scl:send *standard-output* :read-cursorpos)
		  (graphics:draw-rectangle x y (+ x 5) (+ y 5))))
	      #+Ignore
	      (write (aref stipple j i)))))))))


(defun show-array (array)
  (scl:destructuring-bind (width height)
      (array-dimensions array)
    (fresh-line)
    (scl:formatting-table ()
      (dotimes (i height)
	(scl:formatting-row ()
	  (dotimes (j width)
	    (scl:formatting-cell ()
	      (write (aref array j i)))))))))

(defun show-gray-palette (win &optional clear)
  (when clear
    (window-clear win))
  (window-expose win)
  ;; really need "scroll to visible cursor"
  (stream-set-cursor-position* win 0 100)
  (let* ((width (entity-width win))
         (rect-width (floor (- (/ width (or #+:Coral 12 10)) 3))))
    (simple-formatting-table (win :inter-column-spacing 1)
      (simple-formatting-row (win)
        (dotimes (i 11)
          (simple-formatting-cell (win :align :center)
            (write-string (format nil "~A" (float (/ i 10))) win)))
        #+:Coral
        (simple-formatting-cell (win :align :center)
          (write-string "Dk" win))
        #+:Coral
        (simple-formatting-cell (win :align :center)
          (write-string "Lt" win))
        )

      (simple-formatting-row (win)
        (dotimes (i 11)
          (simple-formatting-cell (win)
            (with-user-coordinates (win)
	      (draw-rectangle* 0 0 rect-width 40 :stream win :gray-level (/ i 10)))))
        #+:Coral
        (simple-formatting-cell (win)
          (with-user-coordinates (win)
             (draw-rectangle* 0 0 rect-width 40 :stream win
                             :stipple user::*dark-gray-pattern*)))
        #+:Coral
        (simple-formatting-cell (win)
          (with-user-coordinates (win)
             (draw-rectangle* 0 0 rect-width 40 :stream win
                             :stipple user::*light-gray-pattern*)))
        ))
    (stream-force-output win)))

(defun sheet-show-gray-palette ()
  (let* ((rect-width 20))
    (scl:formatting-table (t :inter-column-spacing 1)
      (scl:formatting-row ()
        (dotimes (i 11)
          (scl:formatting-cell (t :align :center)
            (write-string (format nil "~A" (float (/ i 10))))))
        )

      (scl:formatting-row ()
        (dotimes (i 11)
          (scl:formatting-cell ()
	    (multiple-value-bind (x y) (scl:send *standard-output* :read-cursorpos)
	      (graphics:draw-rectangle x y (+ x rect-width) (+ y 40) :gray-level (/ i 10)))))
        ))))

(defun test-rotate (pattern)
  (dotimes (i 9)
    (format t "~&~8,'0B" pattern)
    (setq pattern (rotate-pattern pattern 1))))

(defun test-pattern (num-per-row num-rows)
  (let ((pattern (make-gray-level-row-pattern num-per-row num-rows)))
    (format t "~V,'0B" num-rows pattern)))

(defun test-make (num-per-row &optional num-rows)
  (unless num-rows
    (multiple-value-setq (num-per-row num-rows)
      (appropriate-dimensions-for-gray-stipple num-per-row)))
  (let ((patterns (make-gray-level-stipple-patterns num-per-row num-rows)))
    (print-patterns patterns num-rows)))

(defun print-patterns (patterns size)
  (dolist (pattern patterns)
    (format t "~&~v,'0b" size pattern)))

(defun test-gray-stipple (gray-level win)
  (let ((stipple (make-gray-level-stipple gray-level)))
    (window-clear win)
    (draw-rectangle* 0 0 100 100 :stream win :stipple stipple)))

#+Genera
(defun sheet-test-gray-stipple (gray-level)
  (let ((stipple (make-gray-level-stipple gray-level)))
    (graphics:with-room-for-graphics (t 100)
      (graphics:draw-rectangle 0 0 100 100 :stipple stipple)
      (graphics:draw-rectangle 100 0 200 100 :gray-level gray-level))))

||#
