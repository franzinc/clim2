;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: text-recording.lisp,v 1.11 92/09/08 15:18:38 cer Exp Locker: cer $

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
    ((string :initarg :string :reader text-displayed-output-record-string)
     (wrapped-p :initform nil :initarg :wrapped-p)
     (ink :initarg :ink)
     (clipping-region :initarg :clipping-region)))

(defclass styled-text-output-record
	  (standard-text-output-record text-displayed-output-record)
    ((initial-text-style :initform nil :initarg :initial-style)
     (text-style-changes :initform nil)
     (current-text-style :initform nil :initarg :current-style)
     (baseline :initform (coordinate 0) :initarg :baseline)))

(define-constructor make-standard-text-output-record
		    standard-text-output-record (string ink clipping-region)
		    :ink ink :string string :clipping-region clipping-region)

(define-constructor make-styled-text-output-record
		    styled-text-output-record (string ink clipping-region)
		    :ink ink :string string :clipping-region clipping-region)

(define-constructor make-styled-text-output-record-1
		    styled-text-output-record 
		    (string ink clipping-region wrapped-p style baseline)
  :ink ink :string string :clipping-region clipping-region :wrapped-p wrapped-p
  :initial-style style :current-style style :baseline baseline)



(defmethod print-object ((object standard-text-output-record) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S /x ~A:~A y ~A:~A/"
	    (safe-slot-value object 'string)
	    (safe-slot-value object 'left)
	    (safe-slot-value object 'right)
	    (safe-slot-value object 'top)
	    (safe-slot-value object 'bottom))))

(defmethod highlight-output-record-1 ((record standard-text-output-record) stream state)
  ;; State is :HIGHLIGHT or :UNHIGHLIGHT
  (declare (ignore state))
  (multiple-value-bind (xoff yoff)
      (convert-from-relative-to-absolute-coordinates stream (output-record-parent record))
    (declare (type coordinate xoff yoff))
    (with-bounding-rectangle* (left top right bottom) record
      (draw-rectangle-internal stream xoff yoff
			       ;; Offset the top and bottom so that the boxes
			       ;; for multi-line text output "cancel" each
			       ;; other out.  Depends on using XOR, of course.
			       ;;--- This obviously works best when the stream's
			       ;;--- vertical spacing is two...
			       left (1- top) right (1+ bottom)
			       +flipping-ink+ +highlighting-line-style+))))

(defmethod output-record-unique-id ((text standard-text-output-record))
  (slot-value text 'string))

(defmethod replay-output-record ((record standard-text-output-record) stream
				 &optional region 
					   (x-offset (coordinate 0)) (y-offset (coordinate 0)))
  (declare (type coordinate x-offset y-offset))
  (declare (ignore region)) 
  (let* ((medium (sheet-medium stream))
	 (string (slot-value record 'string))
	 (start 0)
	 (end (length string))
	 (text-style (medium-default-text-style stream))
	 (baseline (- (text-style-height text-style medium)
		      (text-style-descent text-style medium)))
	 (glyph-buffer (stream-output-glyph-buffer stream))
	 (ink (slot-value record 'ink))
	 (clipping-region (slot-value record 'clipping-region)))
    (declare (type fixnum start end))
    (macrolet
      ((do-it (end-position)
	 `(loop
	    (when (>= start ,end-position) (return))
	    (multiple-value-bind (write-char next-char-index
				  new-cursor-x new-baseline new-height font)
		(stream-scan-string-for-writing 
		  stream medium string start ,end-position text-style
		  cursor-x +largest-coordinate+ glyph-buffer)
	      (declare (ignore font))
	      (draw-text* medium string
			  cursor-x (+ cursor-y (- baseline new-baseline))
			  :start start :end next-char-index 
			  :align-y :top)
	      (setf cursor-x new-cursor-x start next-char-index)
	      (when write-char
		(cond ((eql write-char #\Tab)	;Only non-lozenged exception char?
		       (setf cursor-x (stream-next-tab-column stream cursor-x text-style)))
		      (t 
		       (multiple-value-bind (new-cursor-x new-cursor-y)
			   (stream-draw-lozenged-character
			     stream write-char cursor-x cursor-y new-baseline new-height
			     text-style +largest-coordinate+ nil t)
			 (setf cursor-x new-cursor-x
			       cursor-y new-cursor-y))))
		(incf start))))))
      (multiple-value-bind (cursor-x cursor-y) 
	  (output-record-start-cursor-position record)
	(declare (type coordinate cursor-x cursor-y))
	(translate-coordinates x-offset y-offset cursor-x cursor-y)
	(with-drawing-options (medium :ink ink :text-style text-style
				      :clipping-region clipping-region)
	  (with-identity-transformation (medium)
	    (do-it end)))
	(when (slot-value record 'wrapped-p)
	  (draw-character-wrap-indicator
	    stream cursor-y (bounding-rectangle-height record)
	    (stream-text-margin stream) nil))))))

(defmethod replay-output-record ((record styled-text-output-record) stream
				 &optional region 
					   (x-offset (coordinate 0)) (y-offset (coordinate 0)))
  (declare (type coordinate x-offset y-offset))
  (declare (ignore region))
  (let* ((medium (sheet-medium stream))
	 (string (slot-value record 'string))
	 (start 0)
	 (end (length string))
	 (text-style (slot-value record 'initial-text-style))
	 (baseline (slot-value record 'baseline))
	 (glyph-buffer (stream-output-glyph-buffer stream))
	 (ink (slot-value record 'ink))
	 (clipping-region (slot-value record 'clipping-region)))
    (declare (type fixnum start end))
    (macrolet
      ((do-it (end-position)
	 `(loop
	    (when (>= start ,end-position) (return))
	    (multiple-value-bind (write-char next-char-index
				  new-cursor-x new-baseline new-height font)
		(stream-scan-string-for-writing 
		  stream medium string start ,end-position text-style
		  cursor-x +largest-coordinate+ glyph-buffer)
	      (declare (ignore font))
	      (draw-text* medium string
			  cursor-x (+ cursor-y (- baseline new-baseline))
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
			     text-style +largest-coordinate+ nil t)
			 (setf cursor-x new-cursor-x
			       cursor-y new-cursor-y))))
		(incf start))))))
      (multiple-value-bind (cursor-x cursor-y) 
	  (output-record-start-cursor-position record)
	(declare (type coordinate cursor-x cursor-y))
	(translate-coordinates x-offset y-offset cursor-x cursor-y)
	(with-drawing-options (medium :ink ink :clipping-region clipping-region)
	  (with-identity-transformation (medium)
	    (dolist (text-style-change (slot-value record 'text-style-changes))
	      (let ((new-text-style (car text-style-change))
		    (change-position (cdr text-style-change)))
		(do-it change-position)
		(setf text-style new-text-style
		      start change-position)))
	    (do-it end)))
	(when (slot-value record 'wrapped-p)
	  (draw-character-wrap-indicator
	    stream cursor-y (bounding-rectangle-height record) 
	    (stream-text-margin stream) nil))))))

(defmethod bounding-rectangle-set-edges :around
	   ((record standard-text-output-record) new-left new-top new-right new-bottom)
  (declare (ignore new-left new-top new-right new-bottom))
  (let ((parent (output-record-parent record)))
    (if (not (null parent))
	(with-bounding-rectangle* (old-left old-top old-right old-bottom) record
	  (multiple-value-bind (xoff yoff)
	      (convert-from-descendant-to-ancestor-coordinates record parent)
	    (declare (type coordinate xoff yoff))
	    (translate-coordinates xoff yoff old-left old-top old-right old-bottom)
	    (call-next-method)
	    (recompute-extent-for-changed-child parent record
						old-left old-top old-right old-bottom)))
	(call-next-method))))

(defmethod add-string-output-to-text-record ((record standard-text-output-record)
					     text-string start end text-style
					     new-width new-height new-baseline)
  (declare (ignore text-style new-baseline))
  (declare (type fixnum start end))
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
  (declare (type fixnum start end))
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
  (declare (type fixnum space-needed))
  (declare (ignore style))
  (let* ((string (slot-value record 'string))
	 (fill-pointer (fill-pointer string)))
    (declare (type fixnum fill-pointer))
    (when (> (the fixnum (+ fill-pointer space-needed)) (array-dimension string 0))
      (setf string (adjust-array string (the fixnum (+ fill-pointer space-needed 16))))
      (setf (slot-value record 'string) string))
    string))

(defmethod prepare-text-record-for-appending
    ((record styled-text-output-record) space-needed style)
  (declare (type fixnum space-needed))
  (with-slots (initial-text-style current-text-style
	       text-style-changes baseline) record
    (let* ((string (slot-value record 'string))
	   (fill-pointer (fill-pointer string)))
      (unless (eq style current-text-style)
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
      #-Lucid					;--- is this right in general?
      (and (stringp id2)
	   (string= id1 id2))))

;; We don't do a INVOKE-WITH-NEW-OUTPUT-RECORD for STANDARD-TEXT-OUTPUT-RECORDs.
;; However, STREAM-CLOSE-TEXT-OUTPUT-RECORD does a RECOMPUTE-CONTENTS-OK, too.
(defmethod recompute-contents-ok ((text standard-text-output-record))
  (with-slots (string wrapped-p) text
    (let* ((output-record (output-record-parent text))
	   (match (and output-record
		       (find-child-output-record
			 output-record t 'standard-text-output-record
			 :unique-id string :id-test #'text-recompute-contents-id-test))))
      (when match
	;; The old extent is a copy of MATCH's bounding rectangle
	(setf (output-record-old-bounding-rectangle text) (bounding-rectangle match))
	(when (and (bounding-rectangle-size-equal match text)
		   (eq wrapped-p (slot-value match 'wrapped-p))
		   (eq (class-of text) (class-of match)))
	  (setf (output-record-contents-ok text) t)
	  ;; make sure that old bounding-rect is the same relative position from
	  ;; old-start-position as the bounding-rect is from start-position
	  (multiple-value-bind (delta-x delta-y)
	      (multiple-value-bind (ex ey) (bounding-rectangle-position text)
		(declare (type coordinate ex ey))
		(multiple-value-bind (sx sy) 
		    (output-record-start-cursor-position text)
		  (declare (type coordinate sx sy))
		  (position-difference ex ey sx sy)))
	    (declare (type coordinate delta-x delta-y))
	    (multiple-value-bind (old-start-x old-start-y)
		(multiple-value-bind (px py) (bounding-rectangle-position match)
		  (declare (type coordinate px py))
		  (position-difference px py delta-x delta-y))
	      (output-record-set-old-start-cursor-position
		text old-start-x old-start-y))))))))

(defmethod recompute-contents-ok ((text styled-text-output-record))
  (with-slots (string wrapped-p initial-text-style current-text-style text-style-changes)
	      text
    (let* ((output-record (output-record-parent text))
	   (match (and output-record
		       (find-child-output-record
			 output-record t 'styled-text-output-record
			 :unique-id string :id-test #'text-recompute-contents-id-test))))
      (when match
	;; The old extent is a copy of MATCH's bounding rectangle
	(setf (output-record-old-bounding-rectangle text) (bounding-rectangle match))
	(when (and (bounding-rectangle-size-equal match text)
		   (eq wrapped-p (slot-value match 'wrapped-p))
		   (eq (class-of text) (class-of match))
		   (eq initial-text-style
		       (slot-value match 'initial-text-style))
		   (eq current-text-style
		       (slot-value match 'current-text-style))
		   (equal text-style-changes
			  (slot-value match 'text-style-changes)))
	  (setf (output-record-contents-ok text) t)
	  ;; make sure that old bounding-rect is the same relative position from
	  ;; old-start-position as the bounding-rect is from start-position
	  (multiple-value-bind (delta-x delta-y)
	      (multiple-value-bind (ex ey) (bounding-rectangle-position text)
		(declare (type coordinate ex ey))
		(multiple-value-bind (sx sy) 
		    (output-record-start-cursor-position text)
		  (declare (type coordinate sx sy))
		  (position-difference ex ey sx sy)))
	    (declare (type coordinate delta-x delta-y))
	    (multiple-value-bind (old-start-x old-start-y)
		(multiple-value-bind (px py) (bounding-rectangle-position match)
		  (declare (type coordinate px py))
		  (position-difference px py delta-x delta-y))
	      (output-record-set-old-start-cursor-position
		text old-start-x old-start-y))))))))

(defmethod get-text-output-record ((stream output-recording-mixin) style)
  (let ((default-style (medium-default-text-style stream)))
    (let ((record (stream-text-output-record stream)))
      (when record
	;; If we're changing styles mid-stream, need to convert this
	;; text record to the more expensive form
	(when (and (not (eq style default-style))
		   (not (typep record 'styled-text-output-record)))
	  (setq record (stylize-text-output-record record default-style stream)))
	(return-from get-text-output-record record)))
    (let* ((string (make-array 16 :element-type 'character
				  :fill-pointer 0 :adjustable t))
	   (record (if (not (eq style default-style))
		       (make-styled-text-output-record 
			 string (medium-ink stream) (medium-clipping-region stream))
		       (make-standard-text-output-record
			 string (medium-ink stream) (medium-clipping-region stream)))))
      (setf (stream-text-output-record stream) record)
      (multiple-value-bind (abs-x abs-y)
	  (point-position
	    (stream-output-history-position stream))
	(declare (type coordinate abs-x abs-y))
	(multiple-value-bind (cx cy) (stream-cursor-position stream)
	  (declare (type coordinate cx cy))
	  (output-record-set-start-cursor-position
	    record (- cx abs-x) (- cy abs-y))))
      ;; Moved to STREAM-CLOSE-TEXT-OUTPUT-RECORD, since we don't need this thing
      ;; in the history until then.  This should save an extra recompute-extent call
      ;; (one in here, one when the string is added).
      ;; (stream-add-output-record stream record)
      record)))

;; The cost of stylizing an existing record is actually fairly low, and we
;; don't do it very often, because of the optimization in GET-TEXT-OUTPUT-RECORD
;; that creates a stylized record as early as possible.
(defmethod stylize-text-output-record ((record standard-text-output-record) style stream)
  (with-slots (string ink clipping-region wrapped-p left top right bottom
	       start-x start-y end-x end-y) record
    (let ((new-record (make-styled-text-output-record-1
			string ink clipping-region wrapped-p
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
      (setf (stream-text-output-record stream) new-record)
      new-record)))


(defun find-text-baseline (record stream)
  ;; This finds the lowest baseline of the text in RECORD, which will be slower than, say,
  ;; the first baseline but more likely to look good with misaligned things.
  (let ((baseline (coordinate 0))
	(style (medium-default-text-style stream)))
    (declare (type coordinate baseline))
    (labels ((find-or-recurse (record y-offset)
	       (declare (type coordinate y-offset))
	       (typecase record
		 (styled-text-output-record
		   (maxf baseline (+ y-offset (slot-value record 'baseline))))
		 (standard-text-output-record
		   (maxf baseline
			 (+ y-offset 
			    (- (text-style-height style stream)
			       (text-style-descent style stream)))))
		 (t
		   (multiple-value-bind (xoff yoff) (output-record-position record)
		     (declare (type coordinate yoff))
		     (declare (ignore xoff))
		     (map-over-output-records #'find-or-recurse record
					      0 0 (+ yoff y-offset)))))))
      (declare (dynamic-extent #'find-or-recurse))
      (find-or-recurse record (coordinate 0)))
    baseline)) 

;; Copy just the text from the window to the stream.  If REGION is supplied,
;; only the text overlapping that region is copied.
;; This loses information about text styles, presentations, and graphics, and
;; doesn't deal perfectly with tab characters and changing baselines.
(defun copy-textual-output-history (window stream &optional region record)
  (let* ((char-width (stream-character-width window #\space))
	 (line-height (stream-line-height window))
	 (history (or record (stream-output-history window)))
	 (array (make-array (ceiling (bounding-rectangle-height history) line-height)
			    :fill-pointer 0 :adjustable t :initial-element nil)))
    (labels ((collect (record x-offset y-offset)
	       (multiple-value-bind (start-x start-y)
		   (output-record-start-cursor-position record)
		 (translate-positions x-offset y-offset start-x start-y)
		 (when (typep record 'standard-text-output-record)
		   (vector-push-extend (list* start-y start-x (slot-value record 'string))
				       array))
		 (map-over-output-records-overlapping-region
		   #'collect record region 
		   (- x-offset) (- y-offset) start-x start-y))))
      (declare (dynamic-extent #'collect))
      (collect history (coordinate 0) (coordinate 0)))
    (sort array #'(lambda (r1 r2)
		    (or (< (first r1) (first r2))
			(and (= (first r1) (first r2))
			     (< (second r1) (second r2))))))
    (let ((current-x (coordinate 0))
	  (current-y (first (aref array 0))))
      (dotimes (i (fill-pointer array))
	(let* ((item (aref array i))
	       (y (pop item))
	       (x (pop item)))
	  (unless (= y current-y)
	    (repeat (round (- y current-y) line-height)
	      (terpri stream)
	      (setq current-x (coordinate 0)))
	    (setq current-y y))
	  (unless (= x current-x)
	    (repeat (round (- x current-x) char-width)
	      (write-char #\space stream))
	    (setq current-x x))
	  (write-string item stream)
	  (incf current-x (stream-string-width window item)))))))
