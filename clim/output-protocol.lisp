;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: output-protocol.lisp,v 1.30 92/12/03 10:27:20 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; This file implements our extended output protocol on top of the
;;; proposed "standard" protocol defined in cl-streams.lisp.


;;; This is the class that you mix in to any extended output stream
;;; implementation that you define.  It exists only to provide the
;;; extended-output-stream-p method and to hang
;;; implementation-independent code.
(defclass basic-extended-output-protocol
	  (fundamental-character-output-stream)
    ())

(define-protocol-p-method extended-output-stream-p basic-extended-output-protocol)

(defmethod interactive-stream-p ((stream basic-extended-output-protocol))
  nil)


(defclass output-protocol-mixin
	  (basic-extended-output-protocol)
     ((cursor-x :type coordinate :initform (coordinate 0))
      (cursor-y :type coordinate :initform (coordinate 0))
      ;; We use the next three to initialize the medium
      (foreground :initform nil :initarg :foreground
		  :accessor medium-foreground)
      (background :initform nil :initarg :background
		  :accessor medium-background)
      (default-text-style :initform nil
			  :initarg :default-text-style :initarg :text-style
			  :accessor medium-default-text-style)
      (baseline :accessor stream-baseline
		:type coordinate :initform (coordinate 0))
      (current-line-height :accessor stream-current-line-height
			   :type coordinate :initform (coordinate 0))
      (vertical-space :accessor stream-vertical-spacing
		      :type coordinate
		      :initform (coordinate 2) :initarg :vertical-spacing)
      (end-of-line-action :initarg :end-of-line-action
			  :accessor stream-end-of-line-action)
      (end-of-page-action :initarg :end-of-page-action
			  :accessor stream-end-of-page-action)
      ;; The following two are either NIL or a coordinate
      (text-margin :initarg :text-margin)
      (default-text-margin :accessor stream-default-text-margin
			   :initarg :default-text-margin)
      (output-glyph-buffer :accessor stream-output-glyph-buffer
                           :initarg :output-glyph-buffer)
      (default-view :initform +textual-view+ :initarg :default-view
		    :accessor stream-default-view))
  (:default-initargs :end-of-line-action :wrap
		     :end-of-page-action :scroll
		     ;;--- Is this really appropriate?
		     :default-text-margin +largest-coordinate+
                     :output-glyph-buffer (make-array 512 :element-type '(unsigned-byte 16)
							  :initial-element 0)
		     :text-margin nil))

;; Prevent using the instance-ref instructions on these, since they
;; usually have :BEFORE/:AFTER qualifiers on the SETF methods that will
;; cause the instance-ref instructions to take a slow trap.
#+(or Genera Minima)
(progn
  (declaim (notinline medium-foreground (setf medium-foreground)))
  (declaim (notinline medium-background (setf medium-background)))
  (declaim (notinline medium-text-style (setf medium-text-style)))
  (declaim (notinline medium-default-text-style (setf medium-default-text-style))))

(defmethod initialize-instance :after ((stream output-protocol-mixin)
				       &key text-margin)
  (when text-margin
    (setq text-margin (process-spacing-arg stream text-margin 'stream-text-margin))
    (setf (slot-value stream 'text-margin) text-margin)))

(defmethod (setf medium-foreground) :after (new-value (stream output-protocol-mixin))
  (let ((medium (sheet-medium stream)))
    ;; Watch out for uninitialized MEDIUM slot.
    (when (mediump medium)
      (setf (medium-foreground medium) new-value))))

(defmethod (setf medium-background) :after (new-value (stream output-protocol-mixin))
  (let ((medium (sheet-medium stream)))
    ;; Watch out for uninitialized MEDIUM slot.
    (when (mediump medium)
      (setf (medium-background medium) new-value))))

;;--- Need to write some macros to define these trampolines
(defmethod (setf medium-default-text-style) :after (style (stream output-protocol-mixin))
  (declare (ignore style))
  (with-slots (default-text-style) stream
    (setq default-text-style (parse-text-style default-text-style))))

(defmethod engraft-medium :after
	   ((medium basic-medium) port (stream output-protocol-mixin))
  ;; We set the slots directly in order to avoid running any per-port
  ;; :AFTER methods (or whatever).  That work should be done by similar
  ;; per-port methods on ENGRAFT-MEDIUM.
  (with-slots (silica::foreground silica::background
	       silica::text-style silica::default-text-style 
	       silica::merged-text-style-valid) medium
    (setf silica::foreground 
	    (or (medium-foreground stream)
		(setf (slot-value stream 'foreground) *default-pane-foreground*))
 	  silica::background 
	    (or (medium-background stream)
		(setf (slot-value stream 'background) *default-pane-background*))
	  silica::default-text-style
	    (parse-text-style (or (medium-default-text-style stream)
				  (setf (slot-value stream 'default-text-style)
					(port-default-text-style port))))
	  silica::text-style 
	    (parse-text-style silica::default-text-style)
	  silica::merged-text-style-valid nil)))

;;--- I sure don't like having to do this to make string streams work
(defmethod stream-default-view ((stream t)) +textual-view+)

(defmethod stream-vertical-spacing ((stream t)) 
  (coordinate 0))

;;; The default text margin, by the way, isn't in user-visible coordinates.
(defmethod stream-text-margin ((stream output-protocol-mixin))
  (or (slot-value stream 'text-margin)
      (stream-default-text-margin stream)))

(defmethod (setf stream-text-margin) (new-value (stream output-protocol-mixin))
  (let ((text-margin (process-spacing-arg stream new-value 'stream-text-margin)))
    (setf (slot-value stream 'text-margin) text-margin)))

;;; Genera supports passing an environment to CONSTANTP and EVAL.  Allegro doesn't.
;;; Until we test all other candidates, be conservative.
(defmacro with-end-of-page-action ((stream action) &body body &environment env)
  #-(or Genera Minima) (declare (ignore env))
  (default-output-stream stream)
  (let ((actions '(:wrap :scroll :allow))
	(assert-required t)
	(wrapped-body `(letf-globally (((stream-end-of-page-action ,stream) ,action))
			 ,@body)))
    (when (constantp action #+(or Genera Minima) env)
      (setf action (eval action #+(or Genera Minima-Developer) env))
      (if (member action actions)
	  (setf assert-required nil)
	  (warn "~S action must be one of ~S, not ~S" 'with-end-of-page actions action))
      (setf action `',action))
    (when assert-required
      (setf wrapped-body
	    `(progn (assert (member ,action ',actions))
		    ,wrapped-body)))
    wrapped-body))

(defmacro with-end-of-line-action ((stream action) &body body &environment env)
  #-(or Genera Minima) (declare (ignore env))
  (default-output-stream stream)
  (let ((actions '(:wrap :scroll :allow))
	(assert-required t)
	(wrapped-body `(letf-globally (((stream-end-of-line-action ,stream) ,action))
			 ,@body)))
    (when (constantp action #+(or Genera Minima) env)
      (setf action (eval action #+(or Genera Minima-Developer) env))
      (if (member action actions)
	  (setf assert-required nil)
	  (warn "~S action must be one of ~S, not ~S" 'with-end-of-line actions action))
      (setf action `',action))
    (when assert-required
      (setf wrapped-body
	    `(progn (assert (member ,action ',actions))
		    ,wrapped-body)))
    wrapped-body))

(defmethod stream-cursor-position ((stream output-protocol-mixin))
  (with-slots (cursor-x cursor-y) stream
    (values cursor-x cursor-y)))

(defmethod stream-set-cursor-position ((stream output-protocol-mixin) x y)
  (with-slots (cursor-x cursor-y current-line-height baseline) stream
    (when x 
      (setf cursor-x (coordinate x)))
    (when y
      (unless (eq y cursor-y)
	(setf current-line-height (coordinate 0)	;going to a new line
	      baseline (coordinate 0)))
      (setf cursor-y (coordinate y))))
  (stream-ensure-cursor-visible stream x y))

(defgeneric* (setf stream-cursor-position) (x y stream))
(defmethod* (setf stream-cursor-position) (x y (stream t))
  (stream-set-cursor-position stream x y))

;; NB: X and Y are already coordinates!
(defmethod stream-set-cursor-position-internal ((stream output-protocol-mixin) x y)
  (declare (type coordinate x y))
  (with-slots (cursor-x cursor-y current-line-height baseline) stream
    (when x 
      (setf cursor-x (coordinate x)))
    (when y
      (unless (eq y cursor-y)
	(setf current-line-height (coordinate 0)	;going to a new line
	      baseline (coordinate 0)))
      (setf cursor-y (coordinate y))))
  (stream-ensure-cursor-visible stream x y))

#+Genera
(defmethod stream-compatible-cursor-position ((stream output-protocol-mixin) &optional unit)
  (with-slots (cursor-x cursor-y) stream
    (if (eq unit :character)
	(values (floor cursor-x (stream-character-width stream #\0))
		(floor cursor-y (stream-line-height stream)))
	(values cursor-x cursor-y))))

#+Genera
(defmethod stream-compatible-set-cursor-position ((stream output-protocol-mixin) x y
						  &optional unit)
  (when (eq unit :character)
    (setq x (* x (stream-character-width stream #\0))
	  y (* y (stream-line-height stream))))
  (stream-set-cursor-position stream x y))

#+Genera
(defmethod stream-compatible-increment-cursor-position ((stream output-protocol-mixin) x y
							&optional unit)
  (when (eq unit :character)
    (when x
      (setq x (* x (stream-character-width stream #\0))))
    (when y
      (setq y (* y (stream-line-height stream)))))
  (stream-increment-cursor-position stream x y))


;;; Make normal output streams obey some parts of other protocols for efficiency.

;;--- We put these methods on T so that string streams work.
;;--- It would be nice to put them on something a bit more specific,
;;--- but OUTPUT-PROTOCOL-MIXIN is too specific.  Sigh.

;;; "Normal" output streams are never recording.
(defmethod stream-recording-p ((stream t)) nil)

;;; It is an error to set record-p on non-recording streams to non-NIL.
(defmethod (setf stream-recording-p) (new-value (stream t))
  (when new-value (error "Attempt to set RECORD-P for stream ~S" stream)))

;;; "Normal" output streams are always drawing.
(defmethod stream-drawing-p ((stream t)) t)

;;; It is an error to set draw-p on non-recording streams to NIL.
(defmethod (setf stream-drawing-p) (new-value (stream t))
  (when (not new-value) (error "Attempt to set DRAW-P for stream ~S" stream)))

(defmethod invoke-with-output-recording-options ((stream t) continuation record draw)
  ;; Enforce the assumptions
  (unless draw
    (error "Attempt to disable drawing on the stream ~S" stream))
  (when record
    ;; Unfortunately WITH-OUTPUT-AS-PRESENTATION always tries to turn on
    ;; RECORD-P, so forgive this pecadillo.
    #+++ignore
    (error "Attempt to do output recording on the stream ~S" stream))
  (funcall continuation))


(defmethod stream-force-output ((stream output-protocol-mixin))
  (medium-force-output (sheet-medium stream)))

(defmethod stream-finish-output ((stream output-protocol-mixin))
  (medium-finish-output (sheet-medium stream)))

(defmethod stream-terpri ((output-stream output-protocol-mixin))
  (stream-write-char output-stream #\Newline)
  nil)

(defmethod stream-fresh-line ((output-stream output-protocol-mixin))
  (unless (zerop (slot-value output-stream 'cursor-x))
    (stream-write-char output-stream #\Newline)
    t))

;;; Required methods
;stream-finish-output
;stream-force-output
;stream-clear-output

(defmethod stream-start-line-p ((output-stream output-protocol-mixin))
  (zerop (slot-value output-stream 'cursor-x)))

(defmethod stream-line-column ((output-stream output-protocol-mixin))
  (multiple-value-bind (origin-x origin-y space-width)
      (port-glyph-for-character (port output-stream) #\Space
				(medium-merged-text-style output-stream))
    (declare (ignore origin-x origin-y))
    #-Symbolics
    (multiple-value-bind (column remainder)
 	(floor (slot-value output-stream 'cursor-x) space-width)
      (and (= remainder 0) column))
    #+Symbolics		;Symbolics prefers to return a rational number to NIL
    (/ (slot-value output-stream 'cursor-x) space-width)))

(defmethod stream-advance-to-column ((output-stream output-protocol-mixin) column)
  (multiple-value-bind (origin-x origin-y space-width)
      (port-glyph-for-character (port output-stream) #\Space
				(medium-merged-text-style output-stream))
    (declare (ignore origin-x origin-y))
    (let ((new-x (floor (* column space-width))))
      (when (< (slot-value output-stream 'cursor-x) new-x)
	(stream-set-cursor-position output-stream new-x nil)
	t))))

(defmethod stream-increment-cursor-position ((stream output-protocol-mixin) dx dy)
  (with-slots (cursor-x cursor-y) stream
    (declare (type coordinate cursor-x cursor-y))
    (let ((cx (if dx (+ cursor-x dx) cursor-x))
	  (cy (if dy (+ cursor-y dy) cursor-y)))
      (stream-set-cursor-position stream cx cy))))

(defmethod stream-advance-cursor-x ((stream output-protocol-mixin) amount)
  (declare (type real amount))
  (with-slots (cursor-x cursor-y) stream
    (declare (type coordinate cursor-x cursor-y))
    (stream-set-cursor-position 
      stream (+ cursor-x amount) cursor-y)))

(defmethod stream-advance-cursor-line ((stream output-protocol-mixin))
  (with-slots (cursor-x cursor-y vertical-space current-line-height baseline) stream
    (declare (type coordinate cursor-x cursor-y
		              vertical-space current-line-height baseline))
    (stream-set-cursor-position 
      stream 0 (+ cursor-y (stream-line-height stream) vertical-space))))

(defmethod window-clear :before ((stream output-protocol-mixin))
  (with-slots (baseline current-line-height) stream
    (setf baseline (coordinate 0)
	  current-line-height (coordinate 0))))

(defmethod stream-ensure-cursor-visible ((stream output-protocol-mixin) &optional cx cy)
  (when (and (or (not (output-recording-stream-p stream))
		 (stream-drawing-p stream))
	     (pane-scroller stream))
    (unless cy
      (multiple-value-setq (cx cy) (stream-cursor-position stream)))
    (let ((viewport (or (pane-viewport-region stream) (sheet-region stream)))
	  (new-x nil)
	  (new-y nil))
      (with-bounding-rectangle* (vleft vtop vright vbottom) viewport
	;; Vertical case
        (unless (eq (stream-end-of-page-action stream) ':allow)
	  ;; --- Kludge UNLESS to prevent infinite recursion.  WINDOW-CLEAR clears
	  ;; the output history then sets the cursorpos to (0,0).  Clearing the
	  ;; output history does SCROLL-HOME (which scrolls the stream pane to
	  ;; (0,0)), then sets cursorpos to (0,0), which comes in here and tries to
	  ;; scroll to (0,line-height), which causes the stream pane to be repainted,
	  ;; which may run a frame display function that does WINDOW-CLEAR, sending us
	  ;; back through again.
	  (unless (= cy 0)
	    (incf cy (min (- vbottom vtop)
			  (* 2 (stream-line-height stream)))))
	  (unless (<= vtop cy vbottom)
	    (setf new-y (max 0 (- cy (- vbottom vtop))))))
	;; Horizontal case
	(unless (eq (stream-end-of-line-action stream) ':allow)
	  (unless (<= vleft cx vright)
	    (setf new-x (max 0 (- cx (- vright vleft 
					(* 4 (stream-character-width stream #\0))))))))
	;; If the cursor moves outside the current region, expand
	;; it to include the new cursor position.
	(when (and viewport
		   (> cy (bounding-rectangle-height stream)))
	  (multiple-value-bind (sx sy) (bounding-rectangle-position stream)
	    (update-region stream 
			   sx sy cx cy
			   :no-repaint t)))
	(when (or new-x new-y)
	  (scroll-extent stream :x (or new-x vleft) :y (or new-y vtop)))))))

(defparameter *character-wrap-indicator-width* 3)

(defmethod stream-write-char ((stream output-protocol-mixin) character)
  (with-cursor-state (stream nil)
    (multiple-value-bind (cursor-x cursor-y baseline height style 
			  max-x record-p draw-p)
	(decode-stream-for-writing stream)
      (declare (type coordinate cursor-x cursor-y baseline height max-x))
      (cond ((or (graphic-char-p character)
		 (diacritic-char-p character)
		 ;; Special case so that we don't lozenge this.  It is up to
		 ;; the caller to have established the correct text style.
		 #+CCL-2 (eql character #\CommandMark))
	     (let ((medium (sheet-medium stream)))
	       (dotimes (i 2)
		 (multiple-value-bind (no-wrap new-cursor-x new-baseline new-height
				       font index)
		     (stream-scan-character-for-writing 
		       stream medium character style cursor-x max-x)
		   (declare (type coordinate new-cursor-x new-baseline new-height))
		   (declare (ignore index font))
		   (when no-wrap
		     (when record-p
		       (stream-add-character-output
			 stream character style (- new-cursor-x cursor-x)
			 new-height new-baseline))
		     (when draw-p
		       (when (< baseline new-baseline)
			 (stream-note-line-height-change
			   stream baseline new-baseline height cursor-x cursor-y)
			 (setf baseline new-baseline))
		       ;;--- Need DRAW-GLYPHS, which will take a port-specific font
		       ;;--- object, as well as the :INK option.
		       (with-identity-transformation (medium)
			 (draw-text* medium character	; (code-char index)??
				     cursor-x (+ cursor-y (- baseline new-baseline))
				     :text-style style
				     :align-x :left :align-y :top)))
		     (encode-stream-after-writing
		       stream new-cursor-x cursor-y
		       (max baseline new-baseline) (max height new-height))
		     (return))
		   (case i
		     (0 (multiple-value-setq (cursor-x cursor-y baseline height)
			  (stream-handle-line-wrap
			    stream cursor-y height max-x draw-p record-p)))
		     (1 (error "Couldn't fit character ~S into window" character)))))))
	    ((eql character #\Tab)
	     (let ((new-cursor-x (stream-next-tab-column stream cursor-x style)))
	       (declare (type coordinate new-cursor-x))
	       (when (> (+ new-cursor-x *character-wrap-indicator-width*) max-x)
		 (multiple-value-setq (cursor-x cursor-y baseline height)
		   (stream-handle-line-wrap
		     stream cursor-y height max-x draw-p record-p)))
	       (setf new-cursor-x (stream-next-tab-column stream cursor-x style))
	       (when record-p
		 (stream-add-character-output	;Have to handle tabs in output record
		   stream character style (- new-cursor-x cursor-x)
		   height baseline))
	       ;; We always want to update the cursor -- it will be put back
	       ;; if only recording.
	       (encode-stream-after-writing 
		 stream new-cursor-x cursor-y baseline height)))
	    ((or (eql character #\Newline)
		 (eql character #\Return))
	     ;; STREAM-ADVANCE-CURSOR-LINE will close the current text record.
	     (stream-advance-cursor-line stream))
	    (t
	     (multiple-value-bind (new-cursor-x new-cursor-y new-baseline new-height)
		 (stream-draw-lozenged-character
		   stream character cursor-x cursor-y baseline
		   height style max-x record-p draw-p)
	       (encode-stream-after-writing
		 stream new-cursor-x new-cursor-y new-baseline new-height))))))
  character)					;Return the right value

;;--- Hack COORDINATEs correctly here...
(defmethod stream-draw-lozenged-character ((stream output-protocol-mixin) character
					   cursor-x cursor-y baseline height style
					   max-x record-p draw-p)
  (let ((text-style (merge-text-styles '(nil nil :very-small) 
				       (port-default-text-style (port stream))))
	(ink (medium-ink stream))
	(name (char-name character)))
    (setq name (if name
		   (string-upcase name)
		   #+Genera (format nil "~O" (char-code character))
		   #-Genera (format nil "~X" (char-code character))))
    (multiple-value-bind (last-x largest-x last-y total-height lozenge-baseline)
	(stream-string-output-size stream name :text-style text-style)
      (declare (ignore largest-x last-y lozenge-baseline))
      (let* ((lozenge-height (logand (+ total-height 2) -2))
	     (lozenge-half-height (ceiling lozenge-height 2))
	     (new-line-height (max height lozenge-height))
	     (new-line-offset (- new-line-height height))
	     (new-baseline (+ baseline new-line-offset))
	     (lozenge-left (+ cursor-x lozenge-half-height 1))
	     (lozenge-top (+ cursor-y (- new-baseline lozenge-height) -2))
	     (lozenge-right (+ lozenge-left last-x))
	     (lozenge-bottom (+ cursor-y new-baseline -1))
	     (lozenge-y-point (- lozenge-bottom lozenge-half-height))
	     (lozenge-left-point cursor-x)
	     (lozenge-right-point (+ lozenge-right lozenge-half-height 1))
	     (new-cursor-x (+ lozenge-right-point 1)))
	(when (> (+ new-cursor-x *character-wrap-indicator-width*) max-x)
	  (multiple-value-setq (cursor-x cursor-y baseline height)
	    (stream-handle-line-wrap stream cursor-y height max-x draw-p record-p))
	  (setf new-line-height lozenge-height
		new-line-offset 0
		baseline lozenge-height
		new-baseline lozenge-height
		lozenge-left (+ cursor-x lozenge-half-height 1)
		lozenge-top (- cursor-y  2)
		lozenge-right (+ lozenge-left last-x)
		lozenge-bottom (+ cursor-y lozenge-height -1)
		lozenge-y-point (- lozenge-bottom lozenge-half-height)
		lozenge-left-point cursor-x
		lozenge-right-point (+ lozenge-right lozenge-half-height 1)
		new-cursor-x (+ lozenge-right-point 1)))
	(when record-p
	  (stream-add-character-output
	    stream character style
	    (- new-cursor-x cursor-x) new-line-height new-baseline))
	(when draw-p 
	  (when (< baseline new-baseline)
	    (stream-note-line-height-change stream baseline new-baseline height
					    cursor-x cursor-y)
	    (setf baseline new-baseline))
	  (with-identity-transformation (stream)
	    (with-output-recording-options (stream :record nil :draw t)
	      (draw-text* stream name (+ lozenge-left 1) (+ lozenge-top 2)
			  :align-y :top :text-style text-style :ink ink)
	      (macrolet ((line (x1 y1 x2 y2)
			   `(draw-line* stream ,x1 ,y1 ,x2 ,y2
					:ink ink
					:line-style +highlighting-line-style+)))
		(line lozenge-left lozenge-top lozenge-right lozenge-top)
		(line lozenge-left lozenge-bottom lozenge-right lozenge-bottom)
		(line lozenge-left lozenge-top lozenge-left-point lozenge-y-point)
		(line lozenge-left lozenge-bottom lozenge-left-point lozenge-y-point)
		(line lozenge-right lozenge-top lozenge-right-point lozenge-y-point)
		(line lozenge-right lozenge-bottom lozenge-right-point lozenge-y-point)))))
	(values new-cursor-x cursor-y new-baseline new-line-height)))))

(defmethod stream-write-string ((stream output-protocol-mixin) string &optional (start 0) end)
  (unless end (setf end (length string)))
  (when (>= start end)				;No promises to keep
    (return-from stream-write-string string))
  #+(or Cloe-Runtime XLIB)			;--- Figure this out!!! Cloe bug?
  (when (or (eql (aref string start) #\Newline)
	    (eql (aref string start) #\Return))
    (incf start)
    (terpri stream)
    (when (= start end)
      (return-from stream-write-string string)))
					    
  (multiple-value-bind (cursor-x cursor-y baseline height style
			max-x record-p draw-p glyph-buffer)
      (decode-stream-for-writing stream)
    (declare (type coordinate cursor-x cursor-y baseline height max-x))
    (unless (or draw-p record-p)
      (return-from stream-write-string string))	;No deeds to do
    (let* ((medium (sheet-medium stream)))
      (with-cursor-state (stream nil)
	(loop
	  (multiple-value-bind (write-char next-char-index
				new-cursor-x new-baseline new-height font)
	      (stream-scan-string-for-writing 
		stream medium string start end style cursor-x max-x glyph-buffer)
	    (declare (type coordinate new-cursor-x new-baseline new-height))
	    (declare (ignore font))
	    (when record-p
	      (stream-add-string-output
		stream string start next-char-index style
		(- new-cursor-x cursor-x) new-height new-baseline))
	    (when draw-p
	      (when (< baseline new-baseline)
		(stream-note-line-height-change stream baseline new-baseline height
						cursor-x cursor-y)
		(setf baseline new-baseline))
	      (let ((amount (the fixnum (- next-char-index start))))
		(unless (zerop amount)
		  ;;--- Need DRAW-GLYPHS, which will take a port-specific font
		  ;;--- object, as well as the :INK option.
		  (with-identity-transformation (medium)
		    (draw-text* medium string
				cursor-x (+ cursor-y (- baseline new-baseline))
				:text-style style
				:start start :end next-char-index
				:align-x :left :align-y :top)))))
	    (setf baseline (max baseline new-baseline)
		  height (max height new-height)
		  cursor-x new-cursor-x)
	    (when (>= next-char-index end)
	      ;; Always update the cursor even if only recording.  It will 
	      ;; be restored later.
	      (encode-stream-after-writing stream cursor-x cursor-y baseline height)
	      (return-from stream-write-string string))
	    (setf start next-char-index)
	    (when write-char			;Some random character
	      (encode-stream-after-writing stream cursor-x cursor-y baseline height)
	      (stream-write-char stream write-char)	;Take care of everything random
	      (incf start)
	      (when (>= start end)
		(return-from stream-write-string string))
	      (multiple-value-setq (cursor-x cursor-y baseline height)
		(decode-stream-for-writing stream t))	;and find out what happened
	      )))))))

(defmethod stream-note-line-height-change ((stream output-protocol-mixin)
					   old-baseline new-baseline old-height
					   cursor-x cursor-y)
  (declare (type coordinate old-baseline new-baseline old-height cursor-x cursor-y))
  (let ((movement (- new-baseline old-baseline)))
    (when (and (plusp movement) (not (zerop old-baseline)))
      (stream-move-for-line-height-change stream movement old-height cursor-x cursor-y))))

(defmethod stream-move-for-line-height-change ((stream output-protocol-mixin)
					       movement old-height cursor-x cursor-y)
  ;;--- This doesn't appear to work yet, and it's "not cheap"
  #+++ignore (copy-area (sheet-medium stream)
			0 cursor-y cursor-x old-height
			0 (+ cursor-y movement))
  #+++ignore (window-clear-area stream
				0 cursor-y
				cursor-x (+ cursor-y movement)))

(defmethod text-size ((stream output-protocol-mixin) string
		      &key (text-style (medium-merged-text-style stream)) (start 0) end)
  (declare (values largest-x total-height last-x last-y baseline))
  (when (characterp string)
    (setq string (string string)
	  start 0
	  end nil))
  (multiple-value-bind (last-x largest-x last-y total-height baseline)
      (stream-string-output-size stream string
				 :text-style text-style :start start :end end)
    (values largest-x total-height last-x last-y baseline)))

;;; Simple version: does no wrapping, assumes we start at leftmost
;;; column.  It doesn't do what Genera :STRING-LENGTH message does,
;;; useful as that might be.
(defmethod stream-string-output-size ((stream output-protocol-mixin)
				      string &key (start 0) end text-style)
  (declare (values last-x largest-x last-y total-height baseline))
  (when (characterp string) (setq string (string string)))
  (unless end (setf end (length string)))
  (let ((style (or text-style (medium-merged-text-style stream)))
	(cursor-x (coordinate 0))
	(cursor-y (coordinate 0))
	(height (coordinate 0))
	(baseline (coordinate 0))
	(largest-x (coordinate 0)))
    (declare (type coordinate cursor-x cursor-y height baseline largest-x))
    (let ((medium (sheet-medium stream)))
      (loop
	(when (>= start end) (return))
	(multiple-value-bind (write-char next-char-index new-cursor-x new-baseline new-height)
	    (stream-scan-string-for-writing
	      stream medium string start end style cursor-x +largest-coordinate+)
	  (declare (type coordinate new-cursor-x new-baseline new-height))
	  (maxf largest-x new-cursor-x)
	  (maxf baseline new-baseline)
	  (maxf height new-height)
	  (setf cursor-x new-cursor-x
		start next-char-index)
	  (when write-char
	    (cond #+++ignore ;; Can't happen at the moment.
		  ((or (graphic-char-p write-char) (diacritic-char-p write-char))
		   (multiple-value-bind (no-wrap new-cursor-x new-baseline new-height)
		       (stream-scan-character-for-writing stream character style
							  cursor-x +largest-coordinate+)
		     (declare (ignore no-wrap))
		     (maxf largest-x new-cursor-x)
		     (maxf baseline new-baseline)
		     (maxf height new-height)
		     (setf cursor-x new-cursor-x)))
		  ((or (eql write-char #\Newline)
		       (eql write-char #\Return))
		   (setf cursor-x (coordinate 0))
		   (incf cursor-y height)
		   (setf baseline (coordinate 0)
			 height (stream-line-height stream style)))
		  ((eql write-char #\Tab)
		   (setf cursor-x (stream-next-tab-column stream cursor-x style))
		   (maxf largest-x cursor-x))
		  ((diacritic-char-p write-char))
		  (t
		   (multiple-value-bind (new-cursor-x new-cursor-y new-baseline new-height)
		       (stream-draw-lozenged-character 
			 stream write-char cursor-x cursor-y baseline height text-style
			 +largest-coordinate+ nil nil)
		     (setf cursor-x new-cursor-x cursor-y new-cursor-y
			   baseline new-baseline height new-height))))
	    (incf start)))))
    (values cursor-x largest-x
	    cursor-y (+ cursor-y height)
	    baseline)))

(defmethod stream-string-width ((stream output-protocol-mixin) string
				&key (start 0) end text-style)
  (multiple-value-bind (last-x largest-x)
      (stream-string-output-size stream string :start start :end end :text-style text-style)
    (values last-x largest-x)))			;--- Is this right?
  
(defmethod stream-character-size ((stream output-protocol-mixin) character &optional style)
  (let ((style (or style (medium-merged-text-style stream))))
    (cond ((or (graphic-char-p character) (diacritic-char-p character))
	   (multiple-value-bind  (index font escapement-x escapement-y
				  origin-x origin-y bb-x bb-y)
	       (port-glyph-for-character (port stream) character style)
	     (declare (ignore index font escapement-x escapement-y origin-x origin-y))
	     (values bb-x bb-y)))
	  ((or (eql character #\Newline)
	       (eql character #\Return))
	   (values (- (stream-cursor-position stream))
		   (stream-line-height stream style)))
	  ((eql character #\Tab)
	   (let ((here (stream-cursor-position stream)))
	     (- (stream-next-tab-column stream here style) here)))
	  (t (cerror "Continue without drawing the character"
		     "Can't yet draw lozenged characters: ~@C" character)))))

(defmethod stream-character-width ((stream output-protocol-mixin) character
				   &optional text-style)
  (values (stream-character-size stream character text-style)))

(defmethod stream-line-height ((stream output-protocol-mixin) &optional text-style)
  (let ((current-line-height (stream-current-line-height stream)))
    (if (or text-style (zerop current-line-height))
	(multiple-value-bind (index font escapement-x escapement-y origin-x origin-y bb-x bb-y)
	    (port-glyph-for-character (port stream) #\Space
				      (or text-style
					  (medium-merged-text-style stream)))
	  (declare (ignore index font escapement-x escapement-y origin-x origin-y bb-x))
	  (coordinate bb-y))
	current-line-height)))

(defmethod stream-tab-size ((stream output-protocol-mixin) text-style)
  (multiple-value-bind (index font escapement-x escapement-y origin-x origin-y bb-x bb-y)
      (port-glyph-for-character (port stream) #\0	;En space
				(or text-style
				    (medium-merged-text-style stream)))
    (declare (ignore index font escapement-y origin-x origin-y bb-y))
    (coordinate (* (max bb-x escapement-x) 8.))))

;; Fallback method, a kludge...
;;--- This method, and the ones above, should be defined on mediums, too
(defmethod stream-string-output-size ((medium basic-medium) string
				      &key (start 0) end text-style)
  (when (null end)
    (setq end (length string)))
  (let ((width (* (- end start) (text-style-width text-style medium)))
	(height (text-style-height text-style medium)))
    (values width width height height height)))

;; Another fallback method, another kludge...
(defmethod stream-string-width ((medium basic-medium) string
				&key (start 0) end text-style)
  (multiple-value-bind (last-x largest-x)
      (stream-string-output-size medium string :start start :end end :text-style text-style)
    (values last-x largest-x)))


;; Damnable string streams!
(defmethod text-size ((stream t) string &key text-style (start 0) end)
  (declare (values largest-x total-height last-x last-y baseline))
  (declare (ignore text-style))
  (let ((char-width 8)
	(line-height 12)
	(baseline 10))
    (when (characterp string)
      (return-from text-size
	(if (or (eql string #\Newline)
		(eql string #\Return))
	    (values 0 line-height 0 0 0)
	    (values char-width line-height char-width 0 baseline))))
    (let ((largest-x 0)
	  (total-height 0)
	  (last-x 0)
	  (last-y 0))
      (dovector (char string :start start :end end)
	(cond ((or (eql char #\Newline)
		   (eql char #\Return))
	       (incf total-height line-height)
	       (incf last-y line-height)
	       (setq last-x 0))
	      (t
	       (incf last-x char-width)
	       (maxf largest-x last-x)
	       (maxf total-height line-height))))
      (values largest-x total-height last-x last-y baseline))))

(defmethod stream-string-output-size ((stream t) string &key (start 0) end text-style)
  (multiple-value-bind (largest-x total-height last-x last-y baseline)
      (text-size stream string :start start :end end :text-style text-style)
    (values last-x largest-x last-y total-height baseline)))

(defmethod stream-string-width ((stream t) string &key (start 0) end text-style)
  (multiple-value-bind (last-x largest-x)
      (stream-string-output-size stream string :start start :end end :text-style text-style)
    (values last-x largest-x)))

(defmethod stream-character-width ((stream t) character &optional text-style)
  (values (text-size stream character :text-style text-style)))


;;; A few utilities for string writing.

(defmethod decode-stream-for-writing ((stream output-protocol-mixin) &optional brief-p)
  (declare (values cursor-x cursor-y baseline line-height
		   style max-x record-p draw-p glyph-buffer))
  (multiple-value-bind (cursor-x cursor-y) (stream-cursor-position stream)
    (let ((baseline (stream-baseline stream))
	  (line-height (stream-current-line-height stream))
	  (wrap-p (unless brief-p (eq (stream-end-of-line-action stream) ':wrap))))
      (if brief-p
	  (values cursor-x cursor-y baseline line-height)
	  (values cursor-x cursor-y baseline line-height
		  (medium-merged-text-style stream)
		  (cond ((and wrap-p
			      (stream-text-margin stream)))
			(t +largest-coordinate+))
		  (stream-recording-p stream)
		  (stream-drawing-p stream)
		  (stream-output-glyph-buffer stream))))))

(defmethod encode-stream-after-writing ((stream output-protocol-mixin) cursor-x cursor-y
					baseline line-height)
  (stream-set-cursor-position-internal stream cursor-x cursor-y)
  (setf (stream-baseline stream) baseline
	(stream-current-line-height stream) line-height))

;;; This function returns as soon as:
;;; 1. The character to be drawn is not a graphic/diacritic character,
;;; 2. The character to be drawn does not fit on the current line,
;;; 3. The character must be drawn in a different font, or
;;; 4. There are no more characters to be drawn.
;;; It returns a character which must be passed to WRITE-CHAR or
;;; equivalent if it is cases 1 or 2, and the index of where in the
;;; string to start scanning again (which is one past the character
;;; which must be WRITE-CHARed).
(defmacro stream-scan-string-for-writing-1 
	  (stream medium string start end style cursor-x max-x &optional glyph-buffer)
  (declare (ignore stream))
  `(block stream-scan-string-for-writing
     (let ((baseline (coordinate 0))
	   (height (coordinate 0))
	   (our-font nil)
	   (next-glyph (if (and ,glyph-buffer (array-has-fill-pointer-p ,glyph-buffer))
			   (fill-pointer ,glyph-buffer)
			   0))
	   (max-glyph (and ,glyph-buffer (array-dimension ,glyph-buffer 0)))
	   (,glyph-buffer (or ,glyph-buffer #()))
	   #+(or Genera Minima) (,string ,string))
      (declare (type coordinate baseline height))
      (declare (type vector ,string ,glyph-buffer)
	       (type fixnum next-glyph))
       (loop
	 (when (>= ,start ,end)
	   (return-from stream-scan-string-for-writing
	     (values nil ,start ,cursor-x baseline height our-font)))
	 (let ((character (aref ,string ,start)))
	   (unless (or (graphic-char-p character) (diacritic-char-p character))
	     (return-from stream-scan-string-for-writing
	       (values character ,start ,cursor-x baseline height our-font)))
	   (multiple-value-bind (index font escapement-x escapement-y
				 origin-x origin-y bb-x bb-y fixed-width-font-p)
	       ;;--- For now we are asserting that each string passed to WRITE-STRING
	       ;;--- will have no character style changes within it.  So, we can
	       ;;--- eliminate a call to TEXT-STYLE-MAPPING within 
	       ;;--- PORT-GLYPH-FOR-CHARACTER, which saves a lot of time.
	       (port-glyph-for-character (port ,medium) character ,style our-font)
	     (declare (ignore escapement-y origin-x bb-x))
	     (declare (type fixnum index))
	    (let ((origin-y (coordinate origin-y))
		  (bb-y (coordinate bb-y)))
	      (declare (type coordinate origin-y bb-y))
	     (when fixed-width-font-p
	       (let* ((room-left (- ,max-x ,cursor-x *character-wrap-indicator-width*))
		      (spaces-left (if (zerop escapement-x) 
				       room-left	;diacritic chars have no width
				       (floor room-left escapement-x)))
		      (chars-left (- ,end ,start))
		      (glyphs-left
			(if max-glyph (- max-glyph next-glyph) most-positive-fixnum))
		      (chars-to-do (min spaces-left chars-left glyphs-left))
		      (chars-done 0)
		      (offset (the fixnum (- index (char-code character)))))
		 ;; Assumption: character glyphs are at the same offset within the font
		 ;; that they are within the character set.
		 (block scan-for-newlines-etc
		   (dotimes (i chars-to-do)
		     (let* ((char (aref ,string (the fixnum (+ ,start i)))))
		       ;; Stop when we get to an unusual character (e.g. Newline)
		       (unless (graphic-char-p char)	;excludes diacritics
			 (return-from scan-for-newlines-etc))
		       ;; When a glyph-buffer was supplied, store the glyph index into it
		       (when max-glyph
			 (setf (aref ,glyph-buffer next-glyph)
			       (the fixnum (- (char-code char) offset)))
			 (setq next-glyph (the fixnum (+ next-glyph 1))))
		       ;; count the non-unusual chars
		       (setq chars-done (the fixnum (+ chars-done 1))))))
		 (return-from stream-scan-string-for-writing
		   (values (and (< chars-done chars-left)
				(aref ,string (the fixnum (+ ,start chars-done))))
			   (the fixnum (+ ,start chars-done))
			   (+ ,cursor-x (* chars-done escapement-x))
			   origin-y bb-y font))))
	     (when (> (+ ,cursor-x escapement-x *character-wrap-indicator-width*)
		      ,max-x)
	       (return-from stream-scan-string-for-writing
		 (values character ,start ,cursor-x baseline height our-font)))
	     (when (and our-font (not (eq font our-font)))
	       (return-from stream-scan-string-for-writing
		 (values nil ,start ,cursor-x baseline height our-font)))
	     (when max-glyph			;We are recording glyph indices
	       (when (>= next-glyph max-glyph)	;Too many -- write out the ones we have.
		 (return-from stream-scan-string-for-writing
		   (values nil ,start ,cursor-x baseline height our-font)))
	       (setf (aref ,glyph-buffer next-glyph) index)
	       (setq next-glyph (the fixnum (+ next-glyph 1))))
	     (setq ,cursor-x (+ ,cursor-x escapement-x))
	     (maxf baseline origin-y)
	     (maxf height bb-y)
	     (setf our-font font)
	     (setq ,start (the fixnum (+ ,start 1))))))))))

(defmethod stream-scan-string-for-writing ((stream output-protocol-mixin) medium
					   string start end style
					   cursor-x max-x &optional glyph-buffer)
  (declare (values write-char next-char-index new-cursor-x new-baseline new-height font))
  (declare (type coordinate cursor-x max-x))
  (declare (type fixnum start end))
  (stream-scan-string-for-writing-1
    stream medium string start end style cursor-x max-x glyph-buffer))

;;; This function returns NIL as its first value if the character won't
;;; fit on the current line.  It is never called on non-graphic
;;; characters; they are handled in WRITE-CHAR instead.
(defmethod stream-scan-character-for-writing ((stream output-protocol-mixin) medium
					      character style cursor-x max-x)
  (declare (values char-normal new-cursor-x new-baseline new-height font))
  (declare (type coordinate cursor-x max-x))
  (multiple-value-bind (index font escapement-x escapement-y origin-x origin-y bb-x bb-y)
      (port-glyph-for-character (port medium) character style)
    (declare (ignore escapement-y origin-x bb-x))
    (when (> (+ cursor-x escapement-x *character-wrap-indicator-width*) max-x)
      (return-from stream-scan-character-for-writing 
	(values nil cursor-x (coordinate 0) (coordinate 0))))
    (incf cursor-x (coordinate escapement-x))
    (values t cursor-x (coordinate origin-y) (coordinate bb-y) font index)))

(defun-inline draw-character-wrap-indicator (stream y height max-x &optional (old-record-p t))
  (if old-record-p
      (with-output-recording-options (stream :record nil :draw t)
	(draw-rectangle-internal
	  stream (coordinate 0) (coordinate 0)
	  (- max-x *character-wrap-indicator-width* -1) y
	  ;; HEIGHT can be zero if the line has only Tabs in it...
	  max-x (+ y (if (zerop height) 10 height))
	  +foreground-ink+ nil))
      (draw-rectangle-internal
	stream (coordinate 0) (coordinate 0)
	(- max-x *character-wrap-indicator-width* -1) y
	max-x (+ y (if (zerop height) 10 height))
	+foreground-ink+ nil)))

(defmethod stream-handle-line-wrap ((stream output-protocol-mixin) cursor-y height max-x
				    draw-p record-p)
  (declare (values cursor-x cursor-y baseline line-height))
  (when record-p (stream-close-text-output-record stream t))
  (when draw-p
    (draw-character-wrap-indicator stream cursor-y height max-x record-p))
  (stream-advance-cursor-line stream)
  (decode-stream-for-writing stream t))

(defmethod stream-next-tab-column ((stream output-protocol-mixin) cursor-x style)
  (declare (type coordinate cursor-x))
  (let ((tab-size (stream-tab-size stream style)))
    (coordinate (* tab-size (ceiling (1+ cursor-x) tab-size)))))


;;; Speedy, bare-bones drawing functions for things like highlighting

(defun draw-line-internal
       (stream xoff yoff x1 y1 x2 y2 ink style)
  (let ((medium (sheet-medium stream)))
    (letf-globally (((medium-line-style medium)
		     (or style (medium-line-style medium)))
		    ((medium-transformation medium) +identity-transformation+)
		    ((medium-ink medium) ink))
      (medium-draw-line* medium
			 (+ x1 xoff) (+ y1 yoff)
			 (+ x2 xoff) (+ y2 yoff)))))

(defun draw-rectangle-internal
       (stream xoff yoff left top right bottom ink style)
  (let ((medium (sheet-medium stream)))
    (letf-globally (((medium-line-style medium)
		     (or style (medium-line-style medium)))
		    ((medium-transformation medium) +identity-transformation+)
		    ((medium-ink medium) ink))
      (medium-draw-rectangle* medium
			      (+ left xoff) (+ top yoff)
			      (+ right xoff) (+ bottom yoff)
			      (not style)))))

(defun draw-ellipse-internal
       (stream xoff yoff center-x center-y
	radius-1-dx radius-1-dy radius-2-dx radius-2-dy
	start-angle end-angle ink style)
  (let ((medium (sheet-medium stream)))
    (letf-globally (((medium-line-style medium)
		     (or style (medium-line-style medium)))
		    ((medium-transformation medium) +identity-transformation+)
		    ((medium-ink medium) ink))
      (medium-draw-ellipse* medium
			    (+ center-x xoff) (+ center-y yoff)
			    radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			    start-angle end-angle (not style)))))


;;; Genera hooks

#+Genera
(defmethod stream-compatible-output-as-presentation-1
	   ((stream output-protocol-mixin)
	    continuation continuation-args &rest object-options)
  (declare (ignore object-options))
  (apply continuation continuation-args))

#+Genera
(defmethod stream-compatible-output-as-presentation
	   ((stream output-protocol-mixin)
	    continuation xstream &rest object-options)
  (declare (ignore object-options))
  (funcall continuation xstream))

#+Genera
(defmethod stream-compatible-line-out ((stream output-protocol-mixin) string
				       &optional (start 0) end)
  (stream-write-string stream string start end)
  (stream-terpri stream))

#+Genera
;; Translates DW with-character-style into CLIM with-character-style
;; I didn't bother making this work for device-fonts
(defmethod stream-compatible-with-character-style ((stream output-protocol-mixin)
						   new-style continuation xstream
						   &optional bind-line-height)
  (declare (ignore bind-line-height))	;---CLIM doesn't have this concept?
  ;; Get the DW style object
  (setq new-style (si:parse-character-style new-style))
  ;; Translate it into a CLIM style object
  (let ((family (si:cs-family new-style))
	(face (si:cs-face new-style))
	(size (si:cs-size new-style)))
    (setq new-style (make-text-style (case family
				       ((:dutch sage::serif-body) :serif)
				       ((:swiss :geneva sage:sans-serif-body) :sans-serif)
				       (otherwise family))
				     (case face
				       ((:bold-italic) '(:bold :italic))
				       (otherwise face))
				     size)))
  ;; Do the thing
  (if (eq xstream stream)
      (with-text-style (stream new-style)
	(funcall continuation stream))
      (with-text-style (stream new-style)
	(declare (ignore stream))
	(funcall continuation xstream))))

#+Genera
;; Allows outputting a fat string to a CLIM stream
(defmethod stream-write-string :around ((stream output-protocol-mixin) string
					&optional (outer-start 0) outer-end)
  (unless outer-end
    (setq outer-end (length string)))
  (unless (and (> outer-end outer-start) (scl:string-fat-p string))
    (return-from stream-write-string (call-next-method)))
  (let ((string string))
      (declare (type vector string))
      (loop for start = outer-start then end
	    while start
	    as char = (char string start)
	    as style = (si:char-style char)
	    as end = (position style string
			       :test-not #'eql :key #'si:char-style
			       :start start :end outer-end)
	    do
	(scl:with-character-style (style stream)
	  (call-next-method
	    stream
	    (scl:string-thin string :start start :end (or end outer-end)))))))

#+Genera
;; Allows outputting a fat character to a CLIM stream
(defmethod stream-write-char :around ((stream output-protocol-mixin) character)
  (unless (scl:char-fat-p character)
    (return-from stream-write-char (call-next-method)))
  (scl:with-character-style ((si:char-style character) stream)
    (call-next-method stream (si:strip-style character))))


;;; Input Editor support.  This does STREAM-SCAN-STRING-FOR-WRITING and
;;; other stuff, and calls the continuation so the Input Editor can know
;;; where its strings are on the screen.
(Defmethod do-text-screen-real-estate ((stream output-protocol-mixin) continuation
				       string start end
				       cursor-x cursor-y height baseline style max-x)
  ;; Continuation is a function which takes L T R B Baseline
  (declare (dynamic-extent continuation)
	   (values new-cursor-x new-cursor-y new-height new-baseline))
  (unless start (setq start 0))
  (unless end (setq end (length string)))
  (let ((vsp (stream-vertical-spacing stream))
	(medium (sheet-medium stream)))
    (loop
      (when (>= start end)
	(return (values cursor-x cursor-y height baseline)))
      (multiple-value-bind (write-char next-char-index new-cursor-x new-baseline new-height)
	  (stream-scan-string-for-writing 
	    stream medium string start end style cursor-x max-x)
	(maxf height new-height)
	(maxf baseline new-baseline) 
	(setf start next-char-index)
	(unless (= cursor-x new-cursor-x)
	  (funcall continuation cursor-x cursor-y
				new-cursor-x (+ cursor-y height vsp) baseline))
	(setf cursor-x new-cursor-x)
	(cond ((null write-char))		;Nothing to do for this char.
	      ((or (graphic-char-p write-char)	;Must have wrapped
		   (eql write-char #\Newline)
		   (eql write-char #\Return))
	       (incf cursor-y (+ height vsp))
	       (setf height (coordinate 0)
		     cursor-x (coordinate 0)
		     baseline (coordinate 0))
	       ;; If we wrapped, rescan the character normally, but if this is a newline
	       ;; we're done with it.
	       (when (or (eql write-char #\Newline)
			 (eql write-char #\Return))
		 (incf start)))
	      ;; Tabs are a little inefficient in that they call the continuation an
	      ;; extra time when they could be folded into the rest of the string, but
	      ;; I don't think anybody will notice, especially since the callers of
	      ;; this function merge the rectangles.
	      ((char= write-char #\Tab)
	       (let ((new-cursor-x (stream-next-tab-column stream cursor-x style)))
		 (when (> (+ new-cursor-x *character-wrap-indicator-width*) max-x)
		   (incf cursor-y (+ height vsp))
		   (setf height (coordinate 0)
			 cursor-x (coordinate 0)
			 baseline (coordinate 0))
		   (setf new-cursor-x (stream-next-tab-column stream cursor-x style)))
		 (funcall continuation cursor-x cursor-y
				       new-cursor-x (+ cursor-y height vsp) baseline)
		 (setf cursor-x new-cursor-x)
		 (incf start)))
	      ((diacritic-char-p write-char)
	       (incf start))
	      (t (error "~S found char ~A, and doesn't know what to do."	; ??
			'do-text-screen-real-estate write-char)))))))
