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




;;; Text line output recording.  A stream which does text output creates one of
;;; these per line of text.  Lines are delimited by either #\RETURNs or by
;;; wrapping.  Line boundaries are not recalculated when window size is changed; a
;;; higher-level kind of output record is required for that (protocol not yet
;;; defined, but should be obvious).

;;; The string is a vector of those characters which were output on the current
;;; line.  The initial-text-style is the very first style which
;;; appeared on the line.  The current-text-style is the style of
;;; the last character which appeared on the line.  The baseline is used to
;;; determine where to draw the glyphs.

;;; The text-style-changes is an NCONCed list of conses of the form
;;; (style . position).  The characters between the beginning of the record
;;; and the first change are in the initial-text-style.  Unfortunately,
;;; you can't just use STREAM-WRITE-STRING-INTERNAL on the substring involved
;;; because that function returns in the middle when it encounters a character it
;;; can't deal with, such as tabs and non-graphic characters.  There should never
;;; be a #\Return character in a text-output-record-element.

(defclass text-displayed-output-record
    (displayed-output-record)
    ((string :initarg :string)
     (wrapped-p :initform nil :initarg :wrapped-p)
     (ink :initarg :ink)))
     

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass styled-text-displayed-output-record
	  (text-displayed-output-record)
    ((initial-text-style :initform nil :initarg :initial-style)
     (text-style-changes :initform nil)
     (current-text-style :initform nil :initarg :current-style)
     (baseline :initform 0 :initarg :baseline :type fixnum)))

(define-constructor make-text-output-record-element
		     text-displayed-output-record (ink string)
		    :ink ink :string string)

(define-constructor make-styled-text-output-record-element
		    styled-text-displayed-output-record (ink string)
		    :ink ink :string string)

(define-constructor make-styled-text-output-record-element-1
		    styled-text-displayed-output-record
  (ink string wrapped-p style baseline)
  :ink ink :string string :wrapped-p wrapped-p
  :initial-style style :current-style style :baseline baseline)

(defun safe-slot-value (instance slot-name)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)
      "Unbound"))

(defmethod print-object ((object text-displayed-output-record) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S /x ~A:~A y ~A:~A/"
	    (safe-slot-value object 'string)
	    (safe-slot-value object 'left)
	    (safe-slot-value object 'right)
	    (safe-slot-value object 'top)
	    (safe-slot-value object 'bottom))))

(defmethod output-record-unique-id ((text text-displayed-output-record))
  (slot-value text 'string))

(defmethod replay-output-record ((record text-displayed-output-record) stream &optional region)
  (declare (ignore region))
  (let* ((string (slot-value record 'string))
	 (start 0)
	 (end (length string))
	 (text-style (medium-default-text-style stream))
	 (baseline (- (text-style-height text-style stream)
		      (text-style-descent text-style stream)))
	 (glyph-buffer (stream-output-glyph-buffer stream))
	 (color (slot-value record 'ink)))
    (declare (fixnum start end baseline))
    (with-sheet-medium (medium stream)
      	     (letf-globally (( (medium-transformation medium) +identity-transformation+))
     (macrolet
      ((do-it (end-position)
	 `(loop
	    (when (>= start ,end-position) (return))
	    (multiple-value-bind (write-char next-char-index
				  new-cursor-x new-baseline new-height font)
		(stream-scan-string-for-writing stream medium string start ,end-position
						text-style cursor-x
						most-positive-fixnum 
						glyph-buffer)
	      ;; GLYPH-BUFFER NIL => pass the string to the port-specific code.
	      (if glyph-buffer
		  (stream-write-string-internal
		    medium glyph-buffer 0 (the fixnum (- next-char-index start))
		    font color
		    cursor-x (the fixnum (+ cursor-y (- baseline new-baseline))))
		  (stream-write-string-internal
		    medium string start next-char-index
		    font color
		    cursor-x (the fixnum (+ cursor-y (- baseline new-baseline)))))
	      (setf cursor-x new-cursor-x start next-char-index)
	      (when write-char
		(cond ((eql write-char #\Tab)	;Only non-lozenged exception char?
		       (setf cursor-x (stream-next-tab-column stream cursor-x text-style)))
		      (t 
		       (multiple-value-bind (new-cursor-x new-cursor-y)
			   (stream-draw-lozenged-character
			     stream write-char cursor-x cursor-y new-baseline new-height
			     text-style nil t)
			 (setf cursor-x new-cursor-x
			       cursor-y new-cursor-y))))
		(incf start))))))
      (multiple-value-bind (cursor-x cursor-y) (output-record-position* record)
	(declare (fixnum cursor-x cursor-y))
	(do-it end)))))))

(defmethod replay-output-record ((record styled-text-displayed-output-record) stream &optional region)
  (declare (ignore region))
  (let* ((string (slot-value record 'string))
	 (start 0)
	 (end (length string))
	 (text-style (slot-value record 'initial-text-style))
	 (baseline (slot-value record 'baseline))
	 (glyph-buffer (stream-output-glyph-buffer stream))
	 (color (slot-value record 'ink)))
    (declare (fixnum start end baseline))
    (with-sheet-medium (medium stream)
            	     (letf-globally (( (medium-transformation medium) +identity-transformation+))
     (macrolet
      ((do-it (end-position)
	 `(loop
	    (when (>= start ,end-position) (return))
	    (multiple-value-bind (write-char next-char-index
				  new-cursor-x new-baseline new-height font)
		(stream-scan-string-for-writing stream 
						medium
						string start ,end-position
						text-style cursor-x
						most-positive-fixnum 
						glyph-buffer)
	      (draw-text* medium string
			  cursor-x (the fixnum (+ cursor-y (- baseline new-baseline)))
			  :start start :end next-char-index 
			  :align-y :top
			  :text-style text-style)
	      (setf cursor-x new-cursor-x start next-char-index)
	      (when write-char
		(cond ((eql write-char #\Tab)	;Only non-lozenged exception char?
		       (setf cursor-x (stream-next-tab-column stream cursor-x text-style)))
		      (t 
		       (multiple-value-bind (new-cursor-x new-cursor-y)
			   (stream-draw-lozenged-character
			     stream write-char cursor-x cursor-y new-baseline new-height
			     text-style nil t)
			 (setf cursor-x new-cursor-x
			       cursor-y new-cursor-y))))
		(incf start))))))
      (multiple-value-bind (cursor-x cursor-y) 
	  (output-record-start-cursor-position* record)
	(declare (fixnum cursor-x cursor-y))
	(dolist (text-style-change (slot-value record 'text-style-changes))
	  (let ((new-text-style (car text-style-change))
		(change-position (cdr text-style-change)))
	    (do-it change-position)
	    (setf text-style new-text-style
		  start change-position)))
	(do-it end)))))))

(defmethod add-string-output-to-text-record ((record text-displayed-output-record)
					     text-string start end text-style
					     new-width new-height new-baseline)
  (declare (ignore text-style new-baseline))
  (declare (fixnum start end))
  (when (>= start end)
    (return-from add-string-output-to-text-record))
  (let* ((count (the fixnum (- end start)))
	 (string (prepare-text-record-for-appending record count nil))
	 (fill-pointer (fill-pointer string)))
    (multiple-value-bind (width height) (bounding-rectangle-size record)
      (declare (fixnum width height))
      (setf (fill-pointer string) (the fixnum (+ fill-pointer count)))
      (replace string text-string :start1 fill-pointer :start2 start :end2 end)
      (incf width new-width)
      (maxf height new-height)
      (bounding-rectangle-set-size record width height))))

(defmethod add-string-output-to-text-record ((record styled-text-displayed-output-record)
					     text-string start end text-style
					     new-width new-height new-baseline)
  (declare (fixnum start end))
  (when (>= start end)
    (return-from add-string-output-to-text-record))
  (let* ((count (the fixnum (- end start)))
	 (string (prepare-text-record-for-appending record count text-style))
	 (fill-pointer (fill-pointer string)))
    (multiple-value-bind (width height) (bounding-rectangle-size record)
      (declare (fixnum width height))
      (setf (fill-pointer string) (the fixnum (+ fill-pointer count)))
      (replace string text-string :start1 fill-pointer :start2 start :end2 end)
      (incf width new-width)
      (maxf height new-height)
      (maxf (slot-value record 'baseline) new-baseline)
      (bounding-rectangle-set-size record width height))))

(defmethod add-character-output-to-text-record ((record text-displayed-output-record)
						character text-style
						new-width new-height new-baseline)
  (declare (ignore text-style new-baseline))
  (let* ((string (prepare-text-record-for-appending record 1 nil))
	 (fill-pointer (fill-pointer string)))
    (multiple-value-bind (width height) (bounding-rectangle-size record)
      (declare (fixnum width height))
      (setf (fill-pointer string) (1+ fill-pointer)
	    (aref string fill-pointer) character)
      (incf width new-width)
      (maxf height new-height)
      (bounding-rectangle-set-size record width height))))
  
(defmethod add-character-output-to-text-record ((record styled-text-displayed-output-record)
						character text-style
						new-width new-height new-baseline)
  (let* ((string (prepare-text-record-for-appending record 1 text-style))
	 (fill-pointer (fill-pointer string)))
    (multiple-value-bind (width height) (bounding-rectangle-size record)
      (declare (fixnum width height))
      (setf (fill-pointer string) (1+ fill-pointer)
	    (aref string fill-pointer) character)
      (incf width new-width)
      (maxf height new-height)
      (maxf (slot-value record 'baseline) new-baseline)
      (bounding-rectangle-set-size record width height))))
  
(defmethod prepare-text-record-for-appending
    ((record text-displayed-output-record) space-needed style)
  (declare (fixnum space-needed))
  (declare (ignore style))
  (let* ((string (slot-value record 'string))
	 (fill-pointer (fill-pointer string)))
    (declare (fixnum fill-pointer))
    (when (> (the fixnum (+ fill-pointer space-needed)) (array-dimension string 0))
      (setf string (adjust-array string (the fixnum (+ fill-pointer space-needed 16))))
      (setf (slot-value record 'string) string))
    string))

(defmethod prepare-text-record-for-appending
    ((record styled-text-displayed-output-record) space-needed style)
  (declare (fixnum space-needed))
  (with-slots (initial-text-style current-text-style
	       text-style-changes baseline) record
    (let* ((string (slot-value record 'string))
	   (fill-pointer (fill-pointer string)))
      (unless (eql style current-text-style)
	(if (null initial-text-style)
	    (setf initial-text-style style)
	    (let ((change-record (cons style fill-pointer)))
	      (setf text-style-changes 
		    (nconc text-style-changes (list change-record)))))
	(setf current-text-style style))
      (when (> (the fixnum (+ fill-pointer space-needed)) (array-dimension string 0))
	(setf string (adjust-array string (the fixnum (+ fill-pointer space-needed 16))))
	(setf (slot-value record 'string) string))
      string)))

(defun text-recompute-contents-id-test (id1 id2)
  (or (eql id1 id2)
      (and (stringp id2)
	   (string= id1 id2))))

;; We don't do a WITH-NEW-OUTPUT-RECORD-INTERNAL for TEXT-DISPLAYED-OUTPUT-RECORDs.
;; However, CLOSE-CURRENT-TEXT-OUTPUT-RECORD does a RECOMPUTE-CONTENTS-OK, too.
(defmethod recompute-contents-ok ((text text-displayed-output-record))
  (with-slots (string wrapped-p) text
    (let* ((output-record (output-record-parent text))
	   (match (and output-record
		       (find-inferior-output-record
			 output-record t 'text-displayed-output-record
			 :unique-id string :id-test #'text-recompute-contents-id-test))))
      (when match
	;; The old extent is a copy of MATCH's bounding rectangle
	(setf (output-record-old-extent text) (bounding-rectangle match))
	(when (and (bounding-rectangle-size-equal match text)
		   (eql wrapped-p (slot-value match 'wrapped-p))
		   (eql (class-of text) (class-of match)))
	  (setf (output-record-contents-ok text) t)
	  ;; make sure that old bounding-rect is the same relative position from
	  ;; old-start-position as the bounding-rect is from start-position
	  (multiple-value-bind (delta-x delta-y)
	      (multiple-value-bind (ex ey) (bounding-rectangle-position* text)
		(declare (fixnum ex ey))
		(multiple-value-bind (sx sy) (output-record-position* text)
		  (declare (fixnum sx sy))
		  (position-difference* ex ey sx sy)))
	    (declare (fixnum delta-x delta-y))
	    (multiple-value-bind (old-start-x old-start-y)
		(multiple-value-bind (px py) (bounding-rectangle-position* match)
		  (declare (fixnum px py))
		  (position-difference* px py delta-x delta-y))
	      (output-record-set-old-start-position* text old-start-x old-start-y))))))))

(defmethod recompute-contents-ok ((text styled-text-displayed-output-record))
  (with-slots (string wrapped-p initial-text-style current-text-style text-style-changes)
	      text
    (let* ((output-record (output-record-parent text))
	   (match (and output-record
		       (find-inferior-output-record
			 output-record t 'styled-text-displayed-output-record
			 :unique-id string :id-test #'text-recompute-contents-id-test))))
      (when match
	;; The old extent is a copy of MATCH's bounding rectangle
	(setf (output-record-old-extent text) (bounding-rectangle match))
	;; --- maybe make a method out of this to get efficient slot access?
	(when (and (bounding-rectangle-size-equal match text)
		   (eql wrapped-p (slot-value match 'wrapped-p))
		   (eql (class-of text) (class-of match))
		   (eql initial-text-style
			(slot-value match 'initial-text-style))
		   (eql current-text-style
			(slot-value match 'current-text-style))
		   (equal text-style-changes
			  (slot-value match 'text-style-changes)))
	  (setf (output-record-contents-ok text) t)
	  ;; make sure that old bounding-rect is the same relative position from
	  ;; old-start-position as the bounding-rect is from
	  ;; start-position
	  (multiple-value-bind (old-start-x old-start-y)
		(bounding-rectangle-position* match)
	      (output-record-set-old-start-position* text old-start-x old-start-y))
	  #+ignore
	  (multiple-value-bind (delta-x delta-y)
	      (multiple-value-bind (ex ey) (bounding-rectangle-position* text)
		(declare (fixnum ex ey))
		(multiple-value-bind (sx sy) (output-record-start-position* text)
		  (declare (fixnum sx sy))
		  (position-difference* ex ey sx sy)))
	    (declare (fixnum delta-x delta-y))
	    (multiple-value-bind (old-start-x old-start-y)
		(multiple-value-bind (px py) (bounding-rectangle-position* match)
		  (declare (fixnum px py))
		  (position-difference* px py delta-x delta-y))
	      (output-record-set-old-start-position* text old-start-x old-start-y))))))))

(defun find-text-baseline (record stream)
  ;; This finds the lowest baseline of the text in RECORD, which will be slower than, say,
  ;; the first baseline but more likely to look good with misaligned things.
  (let ((baseline 0)
	(style (medium-default-text-style stream)))
    (declare (fixnum baseline))
    (labels ((find-or-recurse (element)
	       (typecase element
		 (styled-text-displayed-output-record
		   (maxf baseline (the fixnum (+ (bounding-rectangle-min-y element) (slot-value element 'baseline)))))
		 (text-displayed-output-record
		   (maxf baseline
			 (the fixnum (+ (bounding-rectangle-min-y element)
					 (- (text-style-height style stream)
						    (text-style-descent style stream))))))
		 (t
		  (map-over-output-record-children
		   #'find-or-recurse
		   element
		   +everywhere+)))))
      (declare (dynamic-extent #'find-or-recurse))
      (find-or-recurse record))
    baseline))


;; The cost of stylizing an existing record is actually fairly low, and we
;; don't do it very often, because of the optimization in GET-TEXT-OUTPUT-RECORD
;; that creates a stylized record as early as possible.

(defmethod stylize-text-output-record ((record text-displayed-output-record) style stream)
  (with-slots (ink string wrapped-p
	       left top right bottom start-x start-y end-x end-y) record
    (let ((new-record (make-styled-text-output-record-element-1
			ink string wrapped-p
			style (- (text-style-height style stream)
				 (text-style-descent style stream)))))
      (with-slots ((new-left left) (new-top top) (new-right right) (new-bottom bottom)
		   (new-sx start-x) (new-sy start-y) (new-ex end-x) (new-ey end-y)
		   (new-wrapped-p wrapped-p)) new-record
	(setq new-left left
	      new-top top
	      new-right right
	      new-bottom bottom
	      new-sx start-x
	      new-sy start-y
	      new-ex end-x
	      new-ey end-y))
      (setf (output-recording-stream-text-output-record stream) new-record)
      new-record)))
