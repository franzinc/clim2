(in-package :clim-internals)
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

(defclass output-record-mixin ()
	  ((old-start-x :initform 0 :type fixnum)
	   (old-start-y :initform 0 :type fixnum)
	   (old-extent :initform nil	;a bounding rectangle, for redisplay
		       :accessor output-record-old-extent)
	   (contents-ok :initform nil :accessor
			output-record-contents-ok)
	   
     (start-cursor-x :initform 0)
     (start-cursor-y :initform 0)
     (end-cursor-x :initform 0)
     (end-cursor-y :initform 0)))


(defmethod output-record-set-start-cursor-position* ((r output-record-mixin)
						     new-x new-y)
  (multiple-value-bind
      (x y) (bounding-rectangle-position* r)
    (with-slots (start-cursor-x start-cursor-y) r
      (setf start-cursor-x (- new-x x) start-cursor-y (-  new-y y)))))

(defmethod output-record-set-end-cursor-position* ((r output-record-mixin) new-x new-y)
  (multiple-value-bind
      (x y) (bounding-rectangle-position* r)
    (with-slots (end-cursor-x end-cursor-y) r
      (setf end-cursor-x (- new-x x) end-cursor-y (- new-y y)))))


(defmethod output-record-start-cursor-position* ((r output-record-mixin))
  (multiple-value-bind
      (x y) (bounding-rectangle-position* r)
    (with-slots (start-cursor-x start-cursor-y) r
      (values (+ x start-cursor-x) (+ y start-cursor-y)))))


(defmethod output-record-end-cursor-position* ((r output-record-mixin))
  (multiple-value-bind
      (x y) (bounding-rectangle-position* r)
    (with-slots (end-cursor-x end-cursor-y) r
      (values (+ end-cursor-x x) (+ y end-cursor-y)))))



(defmethod elements-never-overlap-p ((record output-record-mixin)) nil)

(defmethod output-record-set-old-start-position* ((rec output-record-mixin) x y)
  (with-slots (old-start-x old-start-y) rec
    (setf old-start-x x  old-start-y y)))

(defmethod output-record-old-start-position* ((rec output-record-mixin))
  (with-slots (old-start-x old-start-y) rec
    (values old-start-x old-start-y)))

(defmethod output-record-old-start-position ((rec output-record-mixin))
  (with-slots (old-start-x old-start-y) rec
    (make-point old-start-x old-start-y)))



(defclass output-record (bounding-rectangle output-record-mixin)
	  ((parent :initform nil :accessor output-record-parent)
	   (old-elements :initform nil :accessor output-record-old-elements)
	   (generation-tick :initform 0 :initarg :generation-tick
		       :accessor output-record-generation-tick))
  (:default-initargs :left 0 :right 0 :top 0 :bottom 0))

(defmethod output-record-p ((r output-record)) t)
(defmethod output-record-p ((r t)) nil)

(defclass displayed-output-record (output-record output-record-mixin)
	  ())

(defmethod displayed-output-record-p ((r displayed-output-record)) t)
(defmethod displayed-output-record-p ((r t)) nil)
(defun displayed-output-record-element-p (x)
  (displayed-output-record-p x))

(defmethod output-record-position* ((r output-record))
  (bounding-rectangle-position* r))

(defun convert-from-relative-to-absolute-coordinates (stream record)
  (if record
      (bounding-rectangle* record)
    (values 0 0 0 0)))

#+ignore
(defun output-record-relative-position* (record)
  (let ((p (output-record-parent record)))
    (if p
	(multiple-value-bind
	    (px py)
	    (bounding-rectangle-position* p)
	  (with-bounding-rectangle* (a b c d) record
				    (values (- a px)
					    (- b py)
					    (- c px)
					    (- d py))))
      (bounding-rectangle* record))))

#+ignore
(defun output-record-set-relative-position* (record new-x new-y)
  (if (output-record-parent record)
      (multiple-value-bind
	  (px py) (output-record-position* (output-record-parent record))
	(output-record-set-position* record (+ px new-x) (+ py new-y)))
    ;; this does not seem right
    (output-record-set-position* record new-x new-y)))

(defun relative-bounding-rectangle (record)
  (multiple-value-call #'make-bounding-rectangle 
    (output-record-relative-position* record)))

(defun output-record-relative-end-position* (rec)
  (multiple-value-bind
      (a b c d)
      (output-record-relative-position* rec)
    (declare (ignore a b))
    (values c d)))

(defun output-record-start-position (record)
  (multiple-value-bind
      (x y) 
      (output-record-position* record)
    (make-point x y)))
    

(defclass graphics-displayed-output-record (displayed-output-record)
	  ())

(defmethod graphics-displayed-output-record-p ((x graphics-displayed-output-record)) t)
(defmethod graphics-displayed-output-record-p ((x t)) nil)

(defmethod output-record-set-position* ((r output-record) new-x new-y)
  
 ;; (when (and (zerop new-x)(zerop new-y)) (break "zerop"))
  (when (minusp new-x) (warn "Zerop new-x ~S,~S" new-x new-y))
  (multiple-value-bind
      (old-x old-y)
      (output-record-position* r)
    (bounding-rectangle-set-position* r new-x new-y)
    (let ((delta-x (- new-x old-x))
	  (delta-y (- new-y old-y)))
      (map-over-output-record-children
       #'(lambda (r)
	   (multiple-value-bind
	       (old-x old-y)
	       (output-record-position* r)
	     (output-record-set-position* 
	      r
	      (+ old-x delta-x)
	      (+ old-y delta-y))))
       r
       +everywhere+))))

(defmethod map-over-output-records-containing-point* (rec continuation x y) nil)

;(defmethod output-record-start-cursor-position* ((r output-record))
;  nil)
;
;(defmethod output-record-set-start-cursor-position* ((r output-record) new-x new-y)
;  (declare (ignore new-x new-y))
;  nil)
;
;(defmethod output-record-end-cursor-position* ((r output-record))
;  nil)
;
;(defmethod output-record-set-end-cursor-position* ((r output-record) new-x new-y)
;  (declare (ignore new-x new-y))
;  nil)

(defun replay (record stream &optional (region +everywhere+))
  ;; Why did the spec not suggest this? or did I misread it.
  (when (stream-draw-p stream)
    (replay-output-record record stream region)))

(defmethod replay-output-record (record stream &optional (region +everywhere+))
  (map-over-output-record-children
   #'(lambda (r) (replay-output-record r stream region))
   record
   region))


(defmethod output-record-hit-detection-rectangle* ((r output-record))
  (bounding-rectangle* r))

(defmethod output-record-refined-sensitivity-test ((r output-record) x y)
  (region-contains-point*-p r x y))

(defmethod highlight-output-record ((r output-record) stream state)
  (declare (ignore state))
  (multiple-value-call #'draw-rectangle* stream (bounding-rectangle* r) :ink +flipping-ink+))

;; output record database protocol

(defmethod output-record-children ((r output-record)) nil)

(defmethod map-over-output-record-children (fn r region)
  (declare (ignore fn r region))
  nil)  

(defmethod add-output-record (child (record output-record))
  (error "cannot add an output record: ~S,~S" child record))

(defmethod add-output-record :after (child (record output-record))
  (setf (output-record-parent child) record)
  (recompute-extent-for-new-child record child))

(defmethod remove-output-record (child (record output-record))
  (error "cannot remove an output record: ~S,~S" child record))

(defmethod remove-output-record :after (child (record output-record))
  (setf (output-record-parent child) nil)
  (multiple-value-call
      #'recompute-extent-for-changed-child
    record
    child
    (bounding-rectangle* child)))
    


(defmethod clear-output-record :after ((record output-record))
  (bounding-rectangle-set-edges record  0 0 0 0))


(defmethod bounding-rectangle-set-position*  ((r output-record) new-x new-y)
  (declare (ignore new-x new-y))
  (multiple-value-bind
      (ominx ominy omaxx omaxy)
      (bounding-rectangle* r)
    (call-next-method)
    (let ((parent (output-record-parent r)))
      (when parent
	(recompute-extent-for-changed-child
	 parent
	 r
	 ominx ominy omaxx omaxy)))))

(defmethod bounding-rectangle-set-edges  ((r output-record) minx miny maxx maxy)
  (declare (ignore minx miny maxx maxy))
  (multiple-value-bind
      (ominx ominy omaxx omaxy)
      (bounding-rectangle* r)
    (call-next-method)
    (when (output-record-parent r)
      (recompute-extent-for-changed-child
       (output-record-parent r)
       r
       ominx ominy omaxx omaxy))))

(defmethod recompute-extent-for-changed-child (record child ominx ominy omaxx omaxy)
  (declare (ignore  ominx ominy omaxx omaxy))
  (tree-recompute-extent record))

(defmethod tree-recompute-extent ((record output-record))
  (let (once minx maxx miny maxy)
    (map-over-output-record-children
     #'(lambda (child)
	 (with-bounding-rectangle* (left top right bottom) child
				   (unless (and (= left right)
						(= top bottom))
				   (if once
				       (progn
					 (minf minx left)
					 (minf miny top)
					 (maxf maxx right)
					 (maxf maxy bottom))
				     (setq once t
					   minx left
					   miny top
					   maxx right
					   maxy bottom)))))
     record
     +everywhere+)
    (if once
	(bounding-rectangle-set-edges record minx miny maxx maxy)
      (bounding-rectangle-set-edges record 0 0 0 0))))

(defmethod recompute-extent-for-new-child (record child)
  (declare (ignore child))
  (tree-recompute-extent record))

;;; Defclass of BASIC-OUTPUT-RECORDING, etc. is in STREAM-CLASS-DEFS
(defmethod initialize-instance :after ((stream basic-output-recording) &rest args)
  (declare (ignore args))
  (with-slots (output-record) stream
    ;; --- our basic-output-recording expects extended output...
    (multiple-value-bind (x y) (stream-cursor-position* stream)
      ;; I don't understand why the output record's initial position was set to
      ;; some untransformed "viewport" coordinate.  The cursor position is the
      ;; right place, no?
      (setf (slot-value output-record 'stream) stream)
      (output-record-set-start-cursor-position* output-record x y)
      #+ignore
      (multiple-value-bind (wx wy) (stream-untransform-point x y stream)
	(bounding-rectangle-set-position* output-record wx wy)))))

(defmethod clear-output-history ((stream basic-output-recording))
  (when (output-recording-stream-output-record stream)
    (clear-output-record (output-recording-stream-output-record stream)))
  (setf (output-recording-stream-text-output-record stream) nil)
  (setf (output-recording-stream-highlighted-presentation stream) nil))

(defmethod stream-add-output-record ((stream basic-output-recording) element)
  (with-slots (output-record current-output-record-stack) stream
    (let ((the-output-record (or current-output-record-stack output-record)))
      (add-output-record element the-output-record))))

(defmethod output-recording-stream-replay ((stream basic-output-recording) &optional region)
  (when (stream-draw-p stream)
    (with-slots (output-record text-output-record-element record-p) stream
      (when (or output-record text-output-record-element)
	(letf-globally ((record-p nil))
	  (when output-record
	    (replay output-record stream region))
	  (when text-output-record-element
	    (replay text-output-record-element stream region)))))))

(defmethod erase-output-record (record stream)
  (let ((parent (output-record-parent record)))
    (with-output-recording-options (stream :record-p nil)
      (multiple-value-call #'draw-rectangle* stream (bounding-rectangle* record)
			   :ink +background+
			   :filled t))
    (remove-output-record record parent)
    (replay (output-recording-stream-output-record stream)
	    stream
	    record)))


#+ignore
(defun erase-output-record (output-record stream)	;--- specialize on stream?
  (multiple-value-bind (xoff yoff)
      (convert-from-relative-to-absolute-coordinates 
	;; --- I'm certainly going to forget to use the PARENT at some point!
	stream (output-record-parent output-record))
    (declare (fixnum xoff yoff))
    (with-bounding-rectangle* (left top right bottom) output-record
      (translate-fixnum-positions xoff yoff left top right bottom)
      (with-output-recording-options (stream :record-p nil)
	(draw-rectangle* stream left top right bottom :ink +background+ :filled t))))
  (when (output-record-parent output-record)
    (delete-output-record-element (output-record-parent output-record) output-record))
  ;; Use the output record itself as the replay region, and replay
  ;; the stuff that might have been obscured by the erased output
  (frame-replay *application-frame* stream output-record))

(defmethod with-output-recording-options-internal ((stream basic-output-recording)
						   draw-p record-p continuation)
  (letf-globally (((stream-record-p stream) record-p)
		  ((stream-draw-p stream) draw-p))
    (funcall continuation)))

(defmethod get-text-output-record ((stream basic-output-recording) style)
  (let ((default-style (medium-default-text-style stream)))
    (let ((record (output-recording-stream-text-output-record stream)))
      (when record
	;; If we're changing styles mid-stream, need to convert this
	;; text record to the more expensive form
	(when (and (not (eq style default-style))
		   (not (typep record 'styled-text-displayed-output-record)))
	  (setq record (stylize-text-output-record record default-style stream)))
	(return-from get-text-output-record record)))
    (let* ((string (make-array 16 :element-type 'extended-char	;--- 16?
				  :fill-pointer 0 :adjustable t))
	   (record (if (not (eq style default-style))
		       (make-styled-text-output-record-element (medium-ink stream) string)
		       (make-text-output-record-element (medium-ink stream) string))))
      (setf (output-recording-stream-text-output-record stream) record)
      (multiple-value-bind (cx cy) (stream-cursor-position* stream)
	  (declare (fixnum cx cy))
	  ;; output-record-set-start-position
	  (output-record-set-position*
	    record cx cy)
	  (output-record-set-start-cursor-position*
	    record cx cy))
      ;; Moved to close-current-text-output-record, since we don't need this thing
      ;; in the history until then.  This should save an extra recompute-extent call
      ;; (one in here, one when the string is added).
      ;; (add-output-record stream record)
      record)))

;;; The following two are only called when STREAM-RECORD-P is true and the
;;; characters are printable (see CHARACTER-DRAWING.LISP).
(defmethod add-string-output-to-output-record ((stream basic-output-recording)
					       string start end text-style
					       width height baseline)
  (declare (fixnum start end))
  (when (< start end)
    (let ((record (get-text-output-record stream text-style)))
      (add-string-output-to-text-record record string start end text-style
					width height baseline))))

(defmethod add-character-output-to-output-record ((stream basic-output-recording)
						  character text-style
						  width height baseline)
  (let ((record (get-text-output-record stream text-style)))
    (add-character-output-to-text-record record character text-style
					 width height baseline)))

(defmethod close-current-text-output-record ((stream basic-output-recording)
					     &optional wrapped)
  ;; It's faster to access the slot directly instead of going through 
  ;; OUTPUT-RECORDING-STREAM-TEXT-OUTPUT-RECORD
  (let ((text-record (slot-value stream 'text-output-record-element)))
    
    (when text-record

      (when wrapped
	(setf (slot-value text-record 'wrapped-p) t))
      (stream-add-output-record stream text-record)
      (when (stream-redisplaying-p stream)
	(recompute-contents-ok text-record))
      (setf (slot-value stream 'text-output-record-element) nil))))

(defmethod stream-force-output :after ((stream basic-output-recording))
  (close-current-text-output-record stream))

(defmethod stream-finish-output :after ((stream basic-output-recording))
  (close-current-text-output-record stream))

;; When setting cursor position, have to dump old text record.
;; This is necessary in order to capture the correct cursor position in
;; text output records.  If we did not close the current text record,
;; a sequence such as WRITE-STRING/SET-CURSORPOS/WRITE-STRING would
;; create only a single output record, and intervening whitespace would
;; be lost if the two WRITE-STRINGs took place on the same line.
(defmethod stream-set-cursor-position* :before ((stream basic-output-recording) x y)
  (declare (ignore x y))
  (close-current-text-output-record stream))

;; This gets used to reposition the cursor when drawing text.  We need to
;; close the text output record when there was a line wrap, but not when
;; we are simply incrementing the cursor beyond the just-written glyph.
(defmethod stream-set-cursor-position*-internal :before ((stream basic-output-recording) x y)
  (declare (ignore x))
  (multiple-value-bind (old-x old-y) (stream-cursor-position* stream)
    (declare (ignore old-x))
    (unless (eql y old-y)
      (close-current-text-output-record stream))))

;; Copy just the text from the window to the stream.  If REGION is supplied,
;; only the text overlapping that region is copied.
;; This loses information about text styles, presentations, and graphics, and
;; doesn't deal perfectly with tab characters and changing baselines.
(defun copy-textual-output-history (window stream &optional region)
  (let* ((char-width (stream-character-width window #\space))
	 (line-height (stream-line-height window))
	 (history (output-recording-stream-output-record window))
	 (array (make-array (ceiling (bounding-rectangle-height history) line-height)
			    :fill-pointer 0 :adjustable t :initial-element nil)))
    (labels ((collect (record x-offset y-offset)
	       (multiple-value-bind (start-x start-y)
		   (output-record-start-position* record)
		 (translate-positions x-offset y-offset start-x start-y)
		 (when (typep record 'text-output-record-element)
		   (vector-push-extend (list* start-y start-x (slot-value record 'string))
				       array))
		 (map-over-output-record-elements-overlapping-region
		   record region #'collect
		   (- x-offset) (- y-offset) start-x start-y))))
      (declare (dynamic-extent #'collect))
      (collect history 0 0))
    (sort array #'(lambda (r1 r2)
		    (or (< (first r1) (first r2))
			(and (= (first r1) (first r2))
			     (< (second r1) (second r2))))))
    (let ((current-x 0)
	  (current-y (first (aref array 0))))
      (dotimes (i (fill-pointer array))
	(let* ((item (aref array i))
	       (y (pop item))
	       (x (pop item)))
	  (unless (= y current-y)
	    (dotimes (j (round (- y current-y) line-height))
	      #-excl (declare (ignore j))
	      (terpri stream)
	      (setq current-x 0))
	    (setq current-y y))
	  (unless (= x current-x)
	    (dotimes (j (round (- x current-x) char-width))
	      #-excl (declare (ignore j))
	      (write-char #\space stream))
	    (setq current-x x))
	  (write-string item stream)
	  (incf current-x (stream-string-width window item)))))))


;;; New class sheet-output-recording is for intermediary methods that can exist
;;; only when both the basic Silica sheet (window) protocol and the output recording
;;; protocol are mixed together.

;;;--- Move to stream-class-defs
(defclass sheet-output-recording () ())

;;; This method should cover a multitude of sins.

(defmethod silica::repaint-sheet ((stream basic-output-recording) region)
  ;;--- Who should establish the clipping region?
  ;; Who should clear the region?
  (with-sheet-medium (medium stream)
    (multiple-value-call 
	#'draw-rectangle*
      medium
      (bounding-rectangle* (region-intersection region 
						(bounding-rectangle stream)))
      :ink +background+)
    (output-recording-stream-replay stream region)))

;;; For Silica
;;;--- Consider these old methods on a case-by-case basis to see if the
;;; general handle-repaint method subsumes them.

;;; --- should merge our process-update-region with handle-repaint
;;; Do we use it anywhere where Silica isn't generating handle-repaint?

;;; Mix in window-output-recording when you have mixed together
;;; something supporting the window protocol and something supporting
;;; the output recording protocol.


;;; Genera compatibility

#+Genera
(defmethod stream-compatible-output-as-presentation
	   ((stream basic-output-recording)
	    continuation xstream
	    &key (object nil) (type t) single-box &allow-other-keys)
  (dw:with-type-decoded (type-name nil pr-args) type
    (if (or (null type)
	    (and (eq type-name 'sys:expression)
		 (not (getf pr-args :escape *print-escape*))
		 (stringp object)))
	(funcall continuation xstream)
        (multiple-value-bind (object clim-type changed-p)
	    (dw-type-to-clim-type object type)
	  (if changed-p
	      (with-output-as-presentation (:stream xstream
					    :object object
					    :type clim-type
					    :single-box single-box)
		(funcall continuation xstream))
	      (funcall continuation xstream))))))

#+Genera
(defmethod stream-compatible-output-as-presentation-1
	   ((stream basic-output-recording)
	    continuation continuation-args
	    &key (object nil) (type t) single-box &allow-other-keys)
  (dw:with-type-decoded (type-name nil pr-args) type
    (if (or (null type)
	    (and (eq type-name 'sys:expression)
		 (not (getf pr-args :escape *print-escape*))
		 (stringp object)))
	(apply continuation continuation-args)
        (multiple-value-bind (object clim-type changed-p)
	    (dw-type-to-clim-type object type)
	  (if changed-p
	      (with-output-as-presentation (:stream stream
					    :object object
					    :type clim-type
					    :single-box single-box)
		(apply continuation continuation-args))
	      (apply continuation continuation-args))))))

(defun with-new-output-record-internal (continuation stream record-type constructor
					&rest init-args &key parent &allow-other-keys)
  (declare (dynamic-extent init-args))
  (with-rem-keywords (init-args init-args '(:parent))
    (let* ((current-output-record (output-recording-stream-current-output-record-stack stream))
	   (new-output-record (and (stream-redisplaying-p stream)
				   current-output-record
				   (apply #'find-inferior-output-record-internal
					  current-output-record record-type init-args))))
      (multiple-value-bind (x y)
	  (stream-cursor-position* stream)
	(declare (fixnum x y))
	(if new-output-record
	    (copy-display-state new-output-record nil)
	  (setq new-output-record
	    ;;--- Used to call ALLOCATE-RECORD, then initialize by
	    ;;--- setting the edges (or INITIALIZE-INSTANCE)
	    (if constructor
		(apply constructor init-args)
	      (apply #'construct-output-record-1 record-type
		      init-args))))
	(output-record-set-position* new-output-record x y)
	(output-record-set-start-cursor-position* new-output-record x y)
	(with-output-record-internal continuation stream new-output-record
				     x y)
	(when (stream-redisplaying-p stream)
	  (recompute-contents-ok new-output-record))
	;; We set the parent after doing everything else so that calls
	;; to RECOMPUTE-CONTENTS-OK inside the dynamic extent of the
	;; continuation won't take forever.
	(let ((parent (or parent
			  current-output-record
			  (output-recording-stream-output-record stream))))
	  (when parent (add-output-record new-output-record parent)))
	new-output-record))))


(defun construct-output-record-1 (type &rest init-args)
  (declare (dynamic-extent init-args))
  (let ((constructor (gethash type *output-record-constructor-cache*)))
    (if constructor
	(apply constructor init-args)
      (apply #'make-instance type init-args))))


(defun with-output-record-internal (continuation stream record &optional abs-x abs-y)
  ;; Close the text record before and after, 
  (close-current-text-output-record stream)
  (let ((current-output-position
	  (output-recording-stream-output-record-absolute-position stream)))
    (unless abs-y
      (multiple-value-setq (abs-x abs-y)
	(stream-cursor-position* stream)))
    (letf-globally (((point-x current-output-position) abs-x)
		    ((point-y current-output-position) abs-y)
		    ((output-recording-stream-current-output-record-stack stream) record))
      (funcall continuation record)
      (multiple-value-bind (end-x end-y)
	  (stream-cursor-position* stream)
	(declare (fixnum end-x end-y))
	(output-record-set-end-cursor-position*
	  record end-x end-y))
      (close-current-text-output-record stream))))

(defun with-room-for-graphics-1 (stream record-type move-cursor
				 continuation &key height)
  (multiple-value-bind (x y) 
      (stream-cursor-position* stream)
    (let ((record
	   (with-output-recording-options (stream :draw-p nil :record-p t)
	     (with-first-quadrant-coordinates (stream)
	       (with-new-output-record (stream record-type)
		 (funcall continuation stream))))))
      ;;--- Hey, there is something wierd going on here.  The problem is that
      ;;--- OUTPUT-RECORD-POSITION* and OUTPUT-RECORD-SET-POSITION* seem to obey
      ;;--- different coordinate system conventions.  Geez.
      (when height
	(incf y (- height (bounding-rectangle-height record))))
      (output-record-set-position* record x y)
      (tree-recompute-extent record)
      (replay record stream)
      (when move-cursor
	(move-cursor-beyond-output-record stream record))
      record)))


(defun move-cursor-beyond-output-record (stream record)
  (with-bounding-rectangle* (left top right bottom) record
			    (declare (ignore left top))
			    (with-end-of-page-action (:allow stream)
			      (stream-set-cursor-position*
			       stream
			       (the fixnum right)
			       (the fixnum (- bottom
					      (stream-line-height stream)))))))

