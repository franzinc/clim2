;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: text-recording.lisp,v 1.1 92/01/31 16:22:15 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

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
;;; you can't just use STREAM-WRITE-STRING-1 on the substring involved
;;; because that function returns in the middle when it encounters a character it
;;; can't deal with, such as tabs and non-graphic characters.  There should never
;;; be a #\Return character in a STANDARD-TEXT-OUTPUT-RECORD.

(defclass standard-text-output-record
	  (output-record-element-mixin text-displayed-output-record)
    ((string :initarg :string)
     (wrapped-p :initform nil :initarg :wrapped-p)
     (ink :initarg :ink)))

(defclass styled-text-output-record
	  (standard-text-output-record text-displayed-output-record)
    ((initial-text-style :initform nil :initarg :initial-style)
     (text-style-changes :initform nil)
     (current-text-style :initform nil :initarg :current-style)
     (baseline :initform 0 :initarg :baseline)))

(define-constructor make-standard-text-output-record
		    standard-text-output-record (ink string)
		    :ink ink :string string)

(define-constructor make-styled-text-output-record
		    styled-text-output-record (ink string)
		    :ink ink :string string)

(define-constructor make-styled-text-output-record-1
		    styled-text-output-record (ink string wrapped-p style baseline)
  :ink ink :string string :wrapped-p wrapped-p
  :initial-style style :current-style style :baseline baseline)

(defmethod print-object ((object standard-text-output-record) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S /x ~A:~A y ~A:~A/"
	    (safe-slot-value object 'string)
	    (safe-slot-value object 'left)
	    (safe-slot-value object 'right)
	    (safe-slot-value object 'top)
	    (safe-slot-value object 'bottom))))

(defmethod output-record-unique-id ((text standard-text-output-record))
  (slot-value text 'string))

(defmethod replay-output-record ((record standard-text-output-record) stream
				 &optional region (x-offset 0) (y-offset 0))
  (declare (type coordinate x-offset y-offset))
  (declare (ignore region))
  (let* ((string (slot-value record 'string))
	 (start 0)
	 (end (length string))
	 (text-style (medium-default-text-style stream))
	 #+Silica (port (sheet-port stream))
	 (baseline (- (text-style-height text-style #-Silica stream #+Silica port)
		      (text-style-descent text-style #-Silica stream #+Silica port)))
	 (glyph-buffer (stream-output-glyph-buffer stream))
	 (color (slot-value record 'ink)))
    (declare (fixnum start end))
    (#-Silica progn
     #+Silica with-sheet-medium #+Silica (medium stream)
     (macrolet
      ((do-it (end-position)
	 `(loop
	    (when (>= start ,end-position) (return))
	    (multiple-value-bind (write-char next-char-index
				  new-cursor-x new-baseline new-height font)
		(stream-scan-string-for-writing stream #+Silica medium
						string start ,end-position text-style
						cursor-x
						;;--- MOST-POSITIVE-FIXNUM loses
						most-positive-fixnum 
						glyph-buffer)
	      ;; GLYPH-BUFFER NIL => pass the string to the port-specific code.
	      #-Silica
	      (if glyph-buffer
		  (stream-write-string-1
		    stream glyph-buffer 0 (the fixnum (- next-char-index start))
		    font color
		    cursor-x (+ cursor-y (- baseline new-baseline)))
		  (stream-write-string-1
		    stream string start next-char-index
		    font color
		    cursor-x (+ cursor-y (- baseline new-baseline))))
	      #+Silica
	      (with-identity-transformation (medium)
		(draw-text* medium string
			    cursor-x (+ cursor-y (- baseline new-baseline))
			    :start start :end next-char-index 
			    :align-y :top
			    :text-style text-style :ink color))
	      (setf cursor-x new-cursor-x start next-char-index)
	      (when write-char
		(cond ((eql write-char #\Tab)	;Only non-lozenged exception char?
		       (setf cursor-x (stream-next-tab-column stream cursor-x text-style)))
		      (t 
		       (multiple-value-bind (new-cursor-x new-cursor-y)
			   (stream-draw-lozenged-character
			     stream write-char cursor-x cursor-y new-baseline new-height
			     ;;--- MOST-POSITIVE-FIXNUM loses
			     text-style most-positive-fixnum nil t)
			 (setf cursor-x new-cursor-x
			       cursor-y new-cursor-y))))
		(incf start))))))
      (multiple-value-bind (cursor-x cursor-y) 
	  (output-record-start-cursor-position* record)
	(declare (type coordinate cursor-x cursor-y))
	(translate-fixnum-positions x-offset y-offset cursor-x cursor-y)
	(do-it end)
	#-Silica
	(when (slot-value record 'wrapped-p)
	  (draw-character-wrap-indicator
	    stream cursor-y (bounding-rectangle-height record) (stream-text-margin stream) nil)))))))

(defmethod replay-output-record ((record styled-text-output-record) stream
				 &optional region (x-offset 0) (y-offset 0))
  (declare (type coordinate x-offset y-offset))
  (declare (ignore region))
  (let* ((string (slot-value record 'string))
	 (start 0)
	 (end (length string))
	 (text-style (slot-value record 'initial-text-style))
	 (baseline (slot-value record 'baseline))
	 (glyph-buffer (stream-output-glyph-buffer stream))
	 (color (slot-value record 'ink)))
    (declare (fixnum start end))
    (#-Silica progn
     #+Silica with-sheet-medium #+Silica (medium stream)
     (macrolet
      ((do-it (end-position)
	 `(loop
	    (when (>= start ,end-position) (return))
	    (multiple-value-bind (write-char next-char-index
				  new-cursor-x new-baseline new-height font)
		(stream-scan-string-for-writing stream #+Silica medium
						string start ,end-position text-style
						cursor-x
						;;--- LOSES
						most-positive-fixnum 
						glyph-buffer)
	      #-Silica
	      (if glyph-buffer
		  (stream-write-string-1
		    stream glyph-buffer 0 (the fixnum (- next-char-index start))
		    font color
		    cursor-x (+ cursor-y (- baseline new-baseline)))
		  (stream-write-string-1
		    stream string start next-char-index
		    font color
		    cursor-x (+ cursor-y (- baseline new-baseline))))
	      #+Silica
	      (with-identity-transformation (medium)
		(draw-text* medium string
			    cursor-x (+ cursor-y (- baseline new-baseline))
			    :start start :end next-char-index 
			    :align-y :top
			    :text-style text-style :ink color))
	      (setf cursor-x new-cursor-x start next-char-index)
	      (when write-char
		(cond ((eql write-char #\Tab)	;Only non-lozenged exception char?
		       (setf cursor-x (stream-next-tab-column stream cursor-x text-style)))
		      (t 
		       (multiple-value-bind (new-cursor-x new-cursor-y)
			   (stream-draw-lozenged-character
			     stream write-char cursor-x cursor-y new-baseline new-height
			     ;;--- LOSES
			     text-style most-positive-fixnum nil t)
			 (setf cursor-x new-cursor-x
			       cursor-y new-cursor-y))))
		(incf start))))))
      (multiple-value-bind (cursor-x cursor-y) 
	  (output-record-start-cursor-position* record)
	(declare (type coordinate cursor-x cursor-y))
	(translate-fixnum-positions x-offset y-offset cursor-x cursor-y)
	(dolist (text-style-change (slot-value record 'text-style-changes))
	  (let ((new-text-style (car text-style-change))
		(change-position (cdr text-style-change)))
	    (do-it change-position)
	    (setf text-style new-text-style
		  start change-position)))
	(do-it end)
	#-Silica
	(when (slot-value record 'wrapped-p)
	  (draw-character-wrap-indicator
	    stream cursor-y (bounding-rectangle-height record) 
	    (stream-text-margin stream) nil)))))))

(defmethod bounding-rectangle-set-edges :around
	   ((record standard-text-output-record) new-left new-top new-right new-bottom)
  (declare (ignore new-left new-top new-right new-bottom))
  (let ((parent (output-record-parent record)))
    (if (not (null parent))
	(with-bounding-rectangle* (old-left old-top old-right old-bottom) record
	  (multiple-value-bind (xoff yoff)
	      (convert-from-descendant-to-ancestor-coordinates record parent)
	    (declare (type coordinate xoff yoff))
	    (translate-fixnum-positions xoff yoff old-left old-top old-right old-bottom)
	    (call-next-method)
	    (recompute-extent-for-changed-child parent record
						old-left old-top old-right old-bottom)))
	(call-next-method))))

(defmethod add-string-output-to-text-record ((record standard-text-output-record)
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
      (declare (type coordinate width height))
      (setf (fill-pointer string) (the fixnum (+ fill-pointer count)))
      (replace string text-string :start1 fill-pointer :start2 start :end2 end)
      (incf width new-width)
      (maxf height new-height)
      (bounding-rectangle-set-size record width height))))

(defmethod add-string-output-to-text-record ((record styled-text-output-record)
					     text-string start end text-style
					     new-width new-height new-baseline)
  (declare (fixnum start end))
  (when (>= start end)
    (return-from add-string-output-to-text-record))
  (let* ((count (the fixnum (- end start)))
	 (string (prepare-text-record-for-appending record count text-style))
	 (fill-pointer (fill-pointer string)))
    (multiple-value-bind (width height) (bounding-rectangle-size record)
      (declare (type coordinate width height))
      (setf (fill-pointer string) (the fixnum (+ fill-pointer count)))
      (replace string text-string :start1 fill-pointer :start2 start :end2 end)
      (incf width new-width)
      (maxf height new-height)
      (maxf (slot-value record 'baseline) new-baseline)
      (bounding-rectangle-set-size record width height))))

(defmethod add-character-output-to-text-record ((record standard-text-output-record)
						character text-style
						new-width new-height new-baseline)
  (declare (ignore text-style new-baseline))
  (let* ((string (prepare-text-record-for-appending record 1 nil))
	 (fill-pointer (fill-pointer string)))
    (multiple-value-bind (width height) (bounding-rectangle-size record)
      (declare (type coordinate width height))
      (setf (fill-pointer string) (1+ fill-pointer)
	    (aref string fill-pointer) character)
      (incf width new-width)
      (maxf height new-height)
      (bounding-rectangle-set-size record width height))))

(defmethod add-character-output-to-text-record ((record styled-text-output-record)
						character text-style
						new-width new-height new-baseline)
  (let* ((string (prepare-text-record-for-appending record 1 text-style))
	 (fill-pointer (fill-pointer string)))
    (multiple-value-bind (width height) (bounding-rectangle-size record)
      (declare (type coordinate width height))
      (setf (fill-pointer string) (1+ fill-pointer)
	    (aref string fill-pointer) character)
      (incf width new-width)
      (maxf height new-height)
      (maxf (slot-value record 'baseline) new-baseline)
      (bounding-rectangle-set-size record width height))))

(defmethod prepare-text-record-for-appending
    ((record standard-text-output-record) space-needed style)
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
    ((record styled-text-output-record) space-needed style)
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

;; We don't do a INVOKE-WITH-NEW-OUTPUT-RECORD for STANDARD-TEXT-OUTPUT-RECORDs.
;; However, STREAM-CLOSE-TEXT-OUTPUT-RECORD does a RECOMPUTE-CONTENTS-OK, too.
(defmethod recompute-contents-ok ((text standard-text-output-record))
  (with-slots (string wrapped-p) text
    (let* ((output-record (output-record-parent text))
	   (match (and output-record
		       (find-inferior-output-record
			 output-record t 'standard-text-output-record
			 :unique-id string :id-test #'text-recompute-contents-id-test))))
      (when match
	;; The old extent is a copy of MATCH's bounding rectangle
	(setf (output-record-old-bounding-rectangle text) (bounding-rectangle match))
	(when (and (bounding-rectangle-size-equal match text)
		   (eql wrapped-p (slot-value match 'wrapped-p))
		   (eql (class-of text) (class-of match)))
	  (setf (output-record-contents-ok text) t)
	  ;; make sure that old bounding-rect is the same relative position from
	  ;; old-start-position as the bounding-rect is from start-position
	  (multiple-value-bind (delta-x delta-y)
	      (multiple-value-bind (ex ey) (bounding-rectangle-position* text)
		(declare (type coordinate ex ey))
		(multiple-value-bind (sx sy) 
		    (output-record-start-cursor-position* text)
		  (declare (type coordinate sx sy))
		  (position-difference* ex ey sx sy)))
	    (declare (type coordinate delta-x delta-y))
	    (multiple-value-bind (old-start-x old-start-y)
		(multiple-value-bind (px py) (bounding-rectangle-position* match)
		  (declare (type coordinate px py))
		  (position-difference* px py delta-x delta-y))
	      (output-record-set-old-start-cursor-position*
		text old-start-x old-start-y))))))))

(defmethod recompute-contents-ok ((text styled-text-output-record))
  (with-slots (string wrapped-p initial-text-style current-text-style text-style-changes)
	      text
    (let* ((output-record (output-record-parent text))
	   (match (and output-record
		       (find-inferior-output-record
			 output-record t 'styled-text-output-record
			 :unique-id string :id-test #'text-recompute-contents-id-test))))
      (when match
	;; The old extent is a copy of MATCH's bounding rectangle
	(setf (output-record-old-bounding-rectangle text) (bounding-rectangle match))
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
	  ;; old-start-position as the bounding-rect is from start-position
	  (multiple-value-bind (delta-x delta-y)
	      (multiple-value-bind (ex ey) (bounding-rectangle-position* text)
		(declare (type coordinate ex ey))
		(multiple-value-bind (sx sy) 
		    (output-record-start-cursor-position* text)
		  (declare (type coordinate sx sy))
		  (position-difference* ex ey sx sy)))
	    (declare (type coordinate delta-x delta-y))
	    (multiple-value-bind (old-start-x old-start-y)
		(multiple-value-bind (px py) (bounding-rectangle-position* match)
		  (declare (type coordinate px py))
		  (position-difference* px py delta-x delta-y))
	      (output-record-set-old-start-cursor-position*
		text old-start-x old-start-y))))))))

(defun find-text-baseline (record stream)
  ;; This finds the lowest baseline of the text in RECORD, which will be slower than, say,
  ;; the first baseline but more likely to look good with misaligned things.
  (let ((baseline 0)
	(style (medium-default-text-style stream))
	#+Silica (port (sheet-port stream)))
    (declare (type coordinate baseline))
    (labels ((find-or-recurse (record y-offset)
	       (declare (type coordinate y-offset))
	       (typecase record
		 (styled-text-output-record
		   (maxf baseline (+ y-offset (slot-value record 'baseline))))
		 (standard-text-output-record
		   (maxf baseline
			 (+ y-offset 
			    (- (text-style-height style #-Silica stream #+Silica port)
			       (text-style-descent style #-Silica stream #+Silica port)))))
		 (t
		   (multiple-value-bind (xoff yoff) (output-record-position* record)
		     (declare (type coordinate yoff))
		     (declare (ignore xoff))
		     (map-over-output-records #'find-or-recurse record
					      0 0 (+ yoff y-offset)))))))
      (declare (dynamic-extent #'find-or-recurse))
      (find-or-recurse record 0))
    baseline)) 

;; The cost of stylizing an existing record is actually fairly low, and we
;; don't do it very often, because of the optimization in GET-TEXT-OUTPUT-RECORD
;; that creates a stylized record as early as possible.
(defmethod stylize-text-output-record ((record standard-text-output-record) style stream)
  (with-slots (ink string wrapped-p left top right bottom
	       start-x start-y end-x end-y) record
    (let* (#+Silica (port (sheet-port stream))
	   (new-record (make-styled-text-output-record-1
			 ink string wrapped-p
			 style (- (text-style-height style #-Silica stream #+Silica port)
				  (text-style-descent style #-Silica stream #+Silica port)))))
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
      (setf (stream-text-output-record stream) new-record)
      new-record)))
