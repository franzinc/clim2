;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: output-protocol.lisp,v 1.4 91/03/26 12:48:23 cer Exp $

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

(defclass output-protocol-mixin
	  (basic-extended-output-protocol)
     ((cursor-x :initform 0)
      (cursor-y :initform 0)
      (baseline :initform 0 :accessor stream-baseline)
      (foreground :initform +black+
		  :accessor medium-foreground
		  :initarg :stream-foreground)
      (background :initform +white+
		  :accessor medium-background
		  :initarg :stream-background)
      (current-line-height :initform 0 :accessor stream-current-line-height)
      (vertical-space :initform 2 :initarg :vertical-spacing 
		      :accessor stream-vertical-spacing)
      (end-of-line-action :initarg :end-of-line-action
			  :accessor stream-end-of-line-action)
      (end-of-page-action :initarg :end-of-page-action
			  :accessor stream-end-of-page-action)
      
      (text-margin :initarg :text-margin)
      (default-text-margin :accessor stream-default-text-margin
			   :initarg :default-text-margin)

      #-Silica
      (display-device-type :initarg :display-device-type
			   :accessor stream-display-device-type
			   :initform (error "You must supply a :DISPLAY-DEVICE-TYPE for ~
					     character output devices."))
      #-Silica
      (default-text-style :initarg :default-text-style
			  :accessor medium-default-text-style
			  :initform *default-text-style*)
      #-Silica
      (current-text-style :accessor medium-text-style
			  :initform *null-text-style*)
      (output-glyph-buffer :accessor stream-output-glyph-buffer
                           :initarg :output-glyph-buffer)
      #-Silica
      (merged-text-style :initform *default-text-style*)
      #-Silica
      (merged-text-style-valid :initform nil
			       :accessor medium-merged-text-style-valid)
      #+Silica
      (user-transformation :initform +identity-transformation+
			   :accessor stream-user-transformation)
      (default-view :initform +textual-view+
		    :accessor stream-default-view))
  (:default-initargs :end-of-line-action :wrap
		     :end-of-page-action :scroll
                     ;;--- 16 bit indices into fonts big enough?
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
    (setf (slot-value stream 'text-margin) text-margin))
  #-Silica
  (setf (slot-value stream 'default-text-style)
	(parse-text-style (slot-value stream 'default-text-style))))
    
#+Silica
(defmethod (setf medium-foreground) :after (new-value (stream output-protocol-mixin))
  (let ((medium (sheet-medium stream)))
    ;; Watch out for uninitialized MEDIUM slot.
    (when (and medium (typep medium 'medium))
      (setf (medium-foreground medium) new-value))))

#+Silica
(defmethod (setf medium-background) :after (new-value (stream output-protocol-mixin))
  (let ((medium (sheet-medium stream)))
    ;; Watch out for uninitialized MEDIUM slot.
    (when (and medium (typep medium 'medium))
      (setf (medium-background medium) new-value))))

;;--- Need to write some macros to define these trampolines
(defmethod (setf medium-default-text-style) :after (style (stream output-protocol-mixin))
  (declare (ignore style))
  (with-slots (default-text-style merged-text-style-valid) stream
    (setq default-text-style (parse-text-style default-text-style))
    (setq merged-text-style-valid nil)))

(defmethod (setf medium-text-style) :after (style (stream output-protocol-mixin))
  (declare (ignore style))
  (with-slots (current-text-style merged-text-style-valid) stream
    (setq current-text-style (parse-text-style current-text-style))
    (setq merged-text-style-valid nil)))

(defmethod engraft-medium :after ((medium medium) port
				  (stream output-protocol-mixin))
  (declare (ignore port))
  ;;--- What about text style stuff, too?
  (setf (medium-foreground medium) (medium-foreground stream)
	(medium-background medium) (medium-background stream)))

;;--- I sure don't like having to do this to make string streams work
(defmethod stream-default-view ((stream t)) +textual-view+)

(defmethod stream-vertical-spacing ((stream t)) 0)

;;; The default text margin, by the way, isn't in user-visible coordinates.
(defmethod stream-text-margin ((stream output-protocol-mixin))
  (or (slot-value stream 'text-margin)
      (stream-default-text-margin stream)))

(defmethod (setf stream-text-margin) (new-value (stream output-protocol-mixin))
  (let ((text-margin (process-spacing-arg stream new-value 'stream-text-margin)))
    (setf (slot-value stream 'text-margin) text-margin)))

#+Silica
(defmethod get-transformation ((stream output-protocol-mixin))
  (stream-user-transformation stream))

;;; Genera supports passing an environment to CONSTANTP and EVAL.  Allegro doesn't.
;;; Until we test all other candidates, be conservative.
(defmacro with-end-of-page-action (#+CLIM-1-compatibility (stream &optional action)
				   #-CLIM-1-compatibility (stream action)
				   &body body &environment env)
  #+CLIM-1-compatibility
  (when (or (keywordp stream)
	    (null action))
    (rotatef stream action)
    (warn "Converting old style call to ~S to the new style.~%~
	   Please update your code." 'with-end-of-page-action))
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

(defmacro with-end-of-line-action (#+CLIM-1-compatibility (stream &optional action)
				   #-CLIM-1-compatibility (stream action)
				   &body body &environment env)
  #+CLIM-1-compatibility
  (when (or (keywordp stream)
	    (null action))
    (rotatef stream action)
    (warn "Converting old style call to ~S to the new style.~%~
	   Please update your code." 'with-end-of-line-action))
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

#-Silica
(defmethod medium-merged-text-style ((stream output-protocol-mixin))
  (with-slots (current-text-style default-text-style
	       merged-text-style merged-text-style-valid) stream
    (if merged-text-style-valid
	merged-text-style
	(prog1 (setf merged-text-style (merge-text-styles current-text-style
							  default-text-style))
	       (setf merged-text-style-valid t)))))

(defmethod stream-cursor-position* ((stream output-protocol-mixin))
  (with-slots (cursor-x cursor-y) stream
    (values cursor-x cursor-y)))

;; X and Y had better be fixnums
;;--- Coerce to COORDINATE
(defmethod stream-set-cursor-position* ((stream output-protocol-mixin) x y)
  (declare (type coordinate x y))
  (with-slots (cursor-x cursor-y current-line-height baseline) stream
    (when x (setf cursor-x x))
    (when y
      (unless (eql y cursor-y)
	(setf current-line-height 0 baseline 0))	;going to a new line
      (setf cursor-y y)))
  #+Silica
  ;; --- Call explicitly in Silica version, rather than around methods on
  ;; --- intermediary class.
  (stream-ensure-cursor-visible stream x y))

;; X and Y had better be fixnums
;;--- Coerce to COORDINATE
(defmethod stream-set-cursor-position*-internal ((stream output-protocol-mixin) x y)
  (declare (type coordinate x y))
  (with-slots (cursor-x cursor-y current-line-height baseline) stream
    (when x (setf cursor-x x))
    (when y
      (unless (eql y cursor-y)
	(setf current-line-height 0 baseline 0))	;going to a new line
      (setf cursor-y y)))
  #+Silica
  ;; --- Call explicitly in Silica version, rather than around methods on
  ;; --- intermediary class.
  (stream-ensure-cursor-visible stream x y))

#+Genera
(defmethod stream-compatible-cursor-position* ((stream output-protocol-mixin) &optional unit)
  (with-slots (cursor-x cursor-y) stream
    (if (eq unit :character)
	(values (floor cursor-x (stream-character-width stream #\m))
		(floor cursor-y (stream-line-height stream)))
	(values cursor-x cursor-y))))

#+Genera
(defmethod stream-compatible-set-cursor-position* ((stream output-protocol-mixin) x y
						   &optional unit)
  (when (eq unit :character)
    (setq x (* x (stream-character-width stream #\m))
	  y (* y (stream-line-height stream))))
  (stream-set-cursor-position* stream x y))

#+Genera
(defmethod stream-compatible-increment-cursor-position* ((stream output-protocol-mixin) x y
							 &optional unit)
  (when (eq unit :character)
    (when x
      (setq x (* x (stream-character-width stream #\m))))
    (when y
      (setq y (* y (stream-line-height stream)))))
  (stream-increment-cursor-position* stream x y))


;;; Make normal output streams obey some parts of other protocols for efficiency.

;;; For "normal" output streams, there are no margins.
(defmethod window-margins ((stream output-protocol-mixin))
  (values 0 0 0 0))

;;; "Normal" output streams are never recording.
(defmethod stream-recording-p ((stream output-protocol-mixin))
  nil)

;;; It is an error to set record-p on non-recording streams to non-NIL.
(defmethod (setf stream-recording-p) (new-value (stream output-protocol-mixin))
  (when new-value (error "Attempt to set RECORD-P for stream ~S" stream)))

;;; "Normal" output streams are always drawing.
(defmethod stream-drawing-p ((stream output-protocol-mixin))
  t)

;;; It is an error to set draw-p on non-recording streams to NIL.
(defmethod (setf stream-drawing-p) (new-value (stream output-protocol-mixin))
  (when (not new-value) (error "Attempt to set DRAW-P for stream ~S" stream)))

#+CLIM-1-compatibility
(progn
(define-compatibility-function (stream-draw-p stream-drawing-p)
			       (stream)
  (stream-drawing-p stream))

(define-compatibility-function (stream-record-p stream-recording-p)
			       (stream)
  (stream-recording-p stream))
)	;#+CLIM-1-compatibility


#+Silica
(defmethod stream-force-output ((stream output-protocol-mixin))
  (port-force-output (sheet-port stream)))

#+Silica
(defmethod stream-finish-output ((stream output-protocol-mixin))
  (port-finish-output (sheet-port stream)))

(defmethod stream-fresh-line ((output-stream output-protocol-mixin))
  (unless (zerop (slot-value output-stream 'cursor-x))
    (stream-write-char output-stream #\newline)
    t))

;;; Required methods
;stream-finish-output
;stream-force-output
;stream-clear-output
;stream-write-string-1
;stream-write-char-1

(defmethod stream-start-line-p ((output-stream output-protocol-mixin))
  (zerop (slot-value output-stream 'cursor-x)))

(defmethod stream-line-column ((output-stream output-protocol-mixin))
  (multiple-value-bind (origin-x origin-y space-width)
      (stream-glyph-for-character output-stream #\space
				  (medium-merged-text-style output-stream))
    (declare (ignore origin-x origin-y))
    (multiple-value-bind (column remainder)
	(floor (slot-value output-stream 'cursor-x) space-width)
      (and (= remainder 0)
	   column))))

(defmethod stream-advance-to-column ((output-stream output-protocol-mixin) column)
  (multiple-value-bind (origin-x origin-y space-width)
      (stream-glyph-for-character output-stream #\space
				  (medium-merged-text-style output-stream))
    (declare (ignore origin-x origin-y))
    (let ((new-x (floor (* column space-width))))
      (when (< (slot-value output-stream 'cursor-x) new-x)
	(stream-set-cursor-position* output-stream new-x nil)
	t))))

(defmethod stream-increment-cursor-position* ((stream output-protocol-mixin) dx dy)
  (declare (type coordinate dx dy))
  (with-slots (cursor-x cursor-y) stream
    (declare (type coordinate cursor-x cursor-y))
    (let ((cx (if dx (+ cursor-x dx) cursor-x))
	  (cy (if dy (+ cursor-y dy) cursor-y)))
      (stream-set-cursor-position* stream cx cy))))

(defmethod stream-advance-cursor-x ((stream output-protocol-mixin) amount)
  (declare (type coordinate amount))
  (with-slots (cursor-x cursor-y) stream
    (declare (type coordinate cursor-x cursor-y))
    (stream-set-cursor-position* 
      stream (+ cursor-x amount) cursor-y)))

(defmethod stream-advance-cursor-line ((stream output-protocol-mixin))
  (with-slots (cursor-x cursor-y vertical-space current-line-height baseline) stream
    (declare (type coordinate cursor-x cursor-y
		              vertical-space current-line-height baseline))
    (stream-set-cursor-position* 
      stream 0 (+ cursor-y (stream-line-height stream) vertical-space))))

(defmethod window-clear :before ((stream output-protocol-mixin))
  (with-slots (baseline current-line-height) stream
    (setf baseline 0
	  current-line-height 0)))

;;; --- Should probably be on some intermediary class, since it can only
;;; be run when the stream is part of a WINDSHIELD hierarchy.
#+Silica
;; --- See comment on stream-set-cursor-position*
(defmethod stream-ensure-cursor-visible ((stream output-protocol-mixin)
					 &optional cx cy)
  (when (and (or (not (output-recording-stream-p stream))
		 (stream-drawing-p stream))
	     (pane-scroller stream))
    (unless cy (multiple-value-setq (cx cy) (stream-cursor-position* stream)))
    (let ((viewport (pane-viewport-region stream))
	  (new-x nil)
	  (new-y nil)
	  (old-cy cy))
      (with-bounding-rectangle* (vleft vtop vright vbottom) viewport
	;; Vertical case
        (unless (eql (stream-end-of-page-action stream) ':allow)
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
	(unless (eql (stream-end-of-line-action stream) ':allow)
	  (unless (<= vleft cx vright)
	    (setf new-x (max 0 (- cx (- vright vleft 
					(* 4 (stream-character-width stream #\W))))))))
	;; If the cursor moves outside the current region, expand
	;; it to include the new cursor position.
	(when (> cy (bounding-rectangle-height stream))
	  (update-region stream cx cy :no-repaint t))
	(when (or new-x new-y)
	  (scroll-extent stream :x (or new-x vleft) :y (or new-y vtop)))))))

(defparameter *character-wrap-indicator-width* 3)

(defmethod stream-write-char ((stream output-protocol-mixin) character)
  (multiple-value-bind (cursor-x cursor-y baseline height style max-x record-p draw-p)
      (decode-stream-for-writing stream)
    (declare (type coordinate cursor-x cursor-y baseline height max-x))
    (cond ((or (graphic-char-p character)
	       (diacritic-char-p character)
               ;; Special case so that we don't lozenge this.  It is up to
               ;; the caller to have established the correct text style.
               #+CCL-2 (eql character #\CommandMark))
	   (let (#+Silica (medium (sheet-medium stream))
		 (ink (medium-ink stream)))
	     (dotimes (i 2)
	       (multiple-value-bind (no-wrap new-cursor-x new-baseline new-height font index)
		   (stream-scan-character-for-writing stream character style cursor-x max-x)
		 (declare (type coordinate new-cursor-x new-baseline new-height))
		 (when no-wrap
		   (when record-p
		     (stream-add-character-output
		       stream character style (- new-cursor-x cursor-x)
		       new-height new-baseline))
		   (when draw-p
		     (when (< baseline new-baseline)
		       (stream-note-line-height-change stream baseline new-baseline height
						       cursor-x cursor-y)
		       (setf baseline new-baseline))
		     ;;--- need draw-glyphs, which will take a port-specific font object, 
		     ;;--- as well as the :INK option.
		     #+Silica
		     (with-identity-transformation (medium)
		       (draw-text* medium character ; (code-char index)??
				   cursor-x (+ cursor-y (- baseline new-baseline))
				   :text-style style
				   :align-x :left :align-y :top))
		     #-Silica
		     (stream-write-char-1
		       stream index font ink
		       cursor-x (+ cursor-y (- baseline new-baseline))))
		   (encode-stream-after-writing stream new-cursor-x cursor-y
						(max baseline new-baseline)
						(max height new-height))
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
	     ;;; We always want to update the cursor -- it will be put back if only recording.
	     (encode-stream-after-writing stream new-cursor-x cursor-y baseline height)))
	  ((eql character #\Newline)
	   ;; STREAM-ADVANCE-CURSOR-LINE will close the current text record.
	   (stream-advance-cursor-line stream))
	  (t
	   (multiple-value-bind (new-cursor-x new-cursor-y new-baseline new-height)
	       (stream-draw-lozenged-character stream character cursor-x cursor-y baseline
					       height style max-x record-p draw-p)
	     (encode-stream-after-writing stream new-cursor-x new-cursor-y
					  new-baseline new-height)))))
  character)					;Return the right value

(defmethod stream-draw-lozenged-character ((stream standard-encapsulating-stream) character
					   cursor-x cursor-y baseline height style
					   max-x record-p draw-p)
  (stream-draw-lozenged-character (slot-value stream 'stream)
                                  character cursor-x cursor-y baseline height style
				  max-x record-p draw-p))

(defmethod stream-draw-lozenged-character ((stream output-protocol-mixin) character
					   cursor-x cursor-y baseline height style
					   max-x record-p draw-p)
  (let ((text-style (merge-text-styles '(nil nil :very-small) *default-text-style*))
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
			   `(draw-line-internal stream 0 0
						,x1 ,y1 ,x2 ,y2
						ink +highlighting-line-style+)))
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
  (when (char= (aref string start) #\Newline)
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
    (let (#+Silica (medium (sheet-medium stream))
	  (ink (medium-ink stream)))
      (loop
	(multiple-value-bind (write-char next-char-index
			      new-cursor-x new-baseline new-height font)
	    (stream-scan-string-for-writing stream #+Silica medium string start end style
					    cursor-x max-x glyph-buffer)
	  (declare (type coordinate new-cursor-x new-baseline new-height))
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
	      ;; --- Don't try to write empty strings.
	      ;; But, I don't know why this algorithm produces them!
	      (unless (zerop amount)
		;; --- need draw-glyphs, which will take a port-specific font object,
		;; as well as the :INK option.
		#+Silica
		(with-identity-transformation (medium)
		  (draw-text* medium string
			      cursor-x (+ cursor-y (- baseline new-baseline))
			      :text-style style
			      :start start :end next-char-index
			      :align-x :left :align-y :top))
		#-Silica
                (if glyph-buffer
		    (stream-write-string-1
		      stream glyph-buffer 0 (the fixnum (- next-char-index start)) font
		      ink cursor-x (+ cursor-y (- baseline new-baseline)))
                    (stream-write-string-1
		      stream string start next-char-index font
		      ink cursor-x (+ cursor-y (- baseline new-baseline)))))))
	  (setf baseline (max baseline new-baseline)
		height (max height new-height)
		cursor-x new-cursor-x)
	  (when (>= next-char-index end)
	    ;; Always update the cursor even if only recording.  It will be restored later.
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
	    ))))))

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
  #+ignore (copy-area stream
		      0 cursor-y
		      cursor-x (+ cursor-y old-height)
		      0 (+ cursor-y movement))
  #+ignore (window-clear-area stream
			      0 cursor-y
			      cursor-x (+ cursor-y movement)))

;;; Simple version: does no wrapping, assumes we start at leftmost
;;; column.  It doesn't do what Genera :STRING-LENGTH message does,
;;; useful as that might be.
(defmethod stream-string-output-size ((stream output-protocol-mixin)
				      string &key (start 0) end text-style)
  (declare (values last-x largest-x last-y total-height baseline))
  (unless end (setf end (length string)))
  (let ((style (or text-style (medium-merged-text-style stream)))
	(cursor-x 0)
	(cursor-y 0)
	(height 0)
	(baseline 0)
	(largest-x 0))
    (declare (type coordinate cursor-x cursor-y height baseline largest-x))
    (let (#+Silica (medium (sheet-medium stream)))
      (loop
	(when (>= start end) (return))
	(multiple-value-bind (write-char next-char-index new-cursor-x new-baseline new-height)
	    (stream-scan-string-for-writing stream #+Silica medium string start end style
					    ;;--- MOST-POSITIVE-FIXNUM will
					    ;;--- lose, use a big single-float
					    cursor-x most-positive-fixnum)
	  (declare (type coordinate new-cursor-x new-baseline new-height))
	  (maxf largest-x new-cursor-x)
	  (maxf baseline new-baseline)
	  (maxf height new-height)
	  (setf cursor-x new-cursor-x
		start next-char-index)
	  (when write-char
	    (cond #+Ignore ;; Can't happen at the moment.
		  ((or (graphic-char-p write-char) (diacritic-char-p write-char))
		   (multiple-value-bind (no-wrap new-cursor-x new-baseline new-height)
		       (stream-scan-character-for-writing stream character style
							  ;;--- MOST-POSITIVE-FIXNUM will
							  ;;--- lose, use a big single-float
							  cursor-x most-positive-fixnum)
		     (declare (ignore no-wrap))
		     (maxf largest-x new-cursor-x)
		     (maxf baseline new-baseline)
		     (maxf height new-height)
		     (setf cursor-x new-cursor-x)))
		  ((eql write-char #\Newline)
		   (setf cursor-x 0)
		   (incf cursor-y height)
		   (setf baseline 0
			 height (stream-line-height stream style)))
		  ((eql write-char #\Tab)
		   (setf cursor-x (stream-next-tab-column stream cursor-x style))
		   (maxf largest-x cursor-x))
		  (t
		   (multiple-value-bind (new-cursor-x new-cursor-y new-baseline new-height)
		       (stream-draw-lozenged-character 
			 stream write-char cursor-x cursor-y baseline height text-style
			 ;;--- MOST-POSITIVE-FIXNUM will
			 ;;--- lose, use a big single-float
			 most-positive-fixnum nil nil)
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
	       (stream-glyph-for-character stream character style)
	     (declare (ignore index font escapement-x escapement-y origin-x origin-y))
	     (values bb-x bb-y)))
	  ((eql character #\newline) (values (- (stream-cursor-position* stream))
					     (stream-line-height stream style)))
	  ((eql character #\tab)
	   (let ((here (stream-cursor-position* stream)))
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
	    (stream-glyph-for-character stream #\Space
					(or text-style
					    (medium-merged-text-style stream)))
	  (declare (ignore index font escapement-x escapement-y origin-x origin-y bb-x))
	  bb-y)
	current-line-height)))

(defmethod stream-tab-size ((stream output-protocol-mixin) text-style)
  (multiple-value-bind (index font escapement-x escapement-y origin-x origin-y bb-x bb-y)
      (stream-glyph-for-character stream #\n	;En space
				  (or text-style
				      (medium-merged-text-style stream)))
    (declare (ignore index font escapement-y origin-x origin-y bb-y))
    (declare (type coordinate escapement-x bb-x))
    (* (max bb-x escapement-x) 8.)))

;;;--- These forwarding methods are for Silica conversion convenience only.
;;; Their callers should be changed to invoke the method on the medium directly.
#+Silica
(defmethod stream-glyph-for-character ((stream output-protocol-mixin) character appearance &optional our-font)
  (stream-glyph-for-character (sheet-medium stream) character appearance our-font))

#+Silica
(defmethod stream-write-char-1 ((stream output-protocol-mixin) index x-font color x y)
  (stream-write-char-1 (sheet-medium stream) index x-font color x y))

;;;--- This forwarding method is for Silica conversion convenience only.
#+Silica
(defmethod stream-write-string-1 ((stream output-protocol-mixin)
				  glyph-buffer start end x-font color x y)
  (stream-write-string-1 (sheet-medium stream) glyph-buffer start end x-font color x y))

#+Silica
(defmethod implementation-pixels-per-point ((stream output-protocol-mixin))
  (graft-pixels-per-point (sheet-graft stream)))

;;; A few utilities for string writing.

(defun compute-text-x-adjustment (align-x stream string text-style &optional start end)
  (ecase align-x
    (:left 0)
    (:right (if (characterp string)
		(- (stream-character-width stream string text-style))
		(- (stream-string-width stream string
					:start start :end end
					:text-style text-style))))
    (:center (if (characterp string)
		 (- (round (stream-character-width stream string text-style) 2))
		 (- (round (stream-string-width stream string
						:start start :end end
						:text-style text-style) 2))))))

(defun compute-text-y-adjustment (align-y descent ascent height)
  (ecase align-y
    (:baseline 0)
    (:top ascent)
    (:bottom (- descent))
    (:center (- ascent (round height 2)))))

(defmethod decode-stream-for-writing ((stream output-protocol-mixin) &optional brief-p)
  (declare (values cursor-x cursor-y baseline line-height
		   style max-x record-p draw-p glyph-buffer))
  (multiple-value-bind (cursor-x cursor-y) (stream-cursor-position* stream)
    (let ((baseline (stream-baseline stream))
	  (line-height (stream-current-line-height stream))
	  (wrap-p (unless brief-p (eq (stream-end-of-line-action stream) ':wrap))))
      (if brief-p
	  (values cursor-x cursor-y baseline line-height)
	(values cursor-x cursor-y baseline line-height
		  (medium-merged-text-style stream)
		  (cond ((and wrap-p
			      (stream-text-margin stream)))
			;;--- MOST-POSITIVE-FIXNUM will
			;;--- lose, use a big single-float
			(t most-positive-fixnum))
		  (stream-recording-p stream)
		  (stream-drawing-p stream)
		  (stream-output-glyph-buffer stream))))))

(defmethod encode-stream-after-writing ((stream output-protocol-mixin) cursor-x cursor-y
					baseline line-height)
  (stream-set-cursor-position*-internal stream cursor-x cursor-y)
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

;;; There is a semiobvious optimization in here for fixed-width fonts,
;;; but I was unable to write it elegantly enough so it would pass my
;;; muster, as it apparently requires PROG. --- come back to this when
;;; there is more time. ---+++---

;;; 13 November 1989:
;;; The body of this method is abstracted out into this macro because we are about to replace
;;; stream-glyph-for-char with a per-implementation macro.  Of course, the vanilla method for
;;; this generic function will continue to call the generic function of that name. -- rsl 
;;; WARNING: ALL methods for stream-scan-string-for-writing MUST take the same argument list,
;;; with the arguments named exactly the same.  This technology depends on this...
;;; Note that there is a convention that if the stream's glyph-buffer slot is NIL, no
;;; character-to-glyph-index translation is done, and STREAM-WRITE-STRING-1 will
;;; be called with the original string, not the glyph buffer.
(defmacro stream-scan-string-for-writing-body ()
  '(let ((baseline 0)
	 (height 0)
	 (our-font nil)
	 (next-glyph (if (and glyph-buffer (array-has-fill-pointer-p glyph-buffer))
			 (fill-pointer glyph-buffer)
			 0))
	 (max-glyph (and glyph-buffer (array-dimension glyph-buffer 0)))
	 (string string)
	 #+(or Genera Minima)				;For array-register binding only.
	 (glyph-buffer (or glyph-buffer #())))	;Array-register declaration requires array.
    (declare (type coordinate baseline height next-glyph))
    (declare (type fixnum next-glyph))
     #+Genera (declare (sys:array-register string glyph-buffer))
     #+Minima (declare (type vector string glyph-buffer))
     (loop
       (when (>= start end)
	 (return-from stream-scan-string-for-writing
	   (values nil start cursor-x baseline height our-font)))
       (let ((character (aref string start)))
	 (unless (or (graphic-char-p character) (diacritic-char-p character))
	   (return-from stream-scan-string-for-writing
	     (values character start cursor-x baseline height our-font)))
	 (multiple-value-bind (index font escapement-x escapement-y
			       origin-x origin-y bb-x bb-y fixed-width-font-p)
	     ;; --- For now we are asserting that each string passed to WRITE-STRING
	     ;; will have no character style changes within it.  So, we can
	     ;; eliminate a call to TEXT-STYLE-MAPPING within 
	     ;; STREAM-GLYPH-FOR-CHARACTER, which saves a >lot< of time.
	     (stream-glyph-for-character
	       #+Silica medium
	       #-Silica stream
	       character style our-font)
	   (declare (ignore escapement-y origin-x bb-x))
	   (declare (fixnum index))
	   (declare (type coordinate origin-y bb-y))
	   (when fixed-width-font-p
	     (let* ((room-left (- max-x cursor-x *character-wrap-indicator-width*))
		    (spaces-left (floor room-left escapement-x))
		    (chars-left (- end start))
		    (glyphs-left
			;;--- MOST-POSITIVE-FIXNUM will
			;;--- lose, use a big single-float
		      (if max-glyph (- max-glyph next-glyph) most-positive-fixnum))
		    (chars-to-do (min spaces-left chars-left glyphs-left))
		    (chars-done 0)
		    (offset (the fixnum (- index (char-code character)))))
	       ;; Assumption: character glyphs are at the same offset within the font
	       ;; that they are within the character set.
	       (block scan-for-newlines-etc
		 (dotimes (i chars-to-do)
		   (let* ((char (aref string (the fixnum (+ start i)))))
		     ;; Stop when we get to an unusual character (e.g. Newline)
		     (unless (or (graphic-char-p char) (diacritic-char-p char))
		       (return-from scan-for-newlines-etc))
		     ;; When a glyph-buffer was supplied, store the glyph index into it
		     (when max-glyph
		       (setf (aref glyph-buffer next-glyph)
			     (the fixnum (- (char-code char) offset)))
		       (setq next-glyph (the fixnum (+ next-glyph 1))))
		     ;; count the non-unusual chars
		     (setq chars-done (the fixnum (+ chars-done 1))))))
	       (return-from stream-scan-string-for-writing
		 (values (and (< chars-done chars-left)
			      (aref string (the fixnum (+ start chars-done))))
			 (the fixnum (+ start chars-done))
			 (+ cursor-x (* chars-done escapement-x))
			 origin-y bb-y font))))
	   (when (> (+ cursor-x escapement-x *character-wrap-indicator-width*)
		    max-x)
	     (return-from stream-scan-string-for-writing
	       (values character start cursor-x baseline height our-font)))
	   (when (and our-font (not (eq font our-font)))
	     (return-from stream-scan-string-for-writing
	       (values nil start cursor-x baseline height our-font)))
	   (when max-glyph					;We are recording glyph indices
	     (when (>= next-glyph max-glyph)			;Too many -- write out the ones we have.
	       (return-from stream-scan-string-for-writing
		 (values nil start cursor-x baseline height our-font)))
	     (setf (aref glyph-buffer next-glyph) index)
	     (setq next-glyph (the fixnum (+ next-glyph 1))))
	   (setq cursor-x (+ cursor-x escapement-x))
	   (maxf baseline origin-y)
	   (maxf height bb-y)
	   (setf our-font font)
	   (setq start (the fixnum (+ start 1))))))))

(defmethod stream-scan-string-for-writing ((stream output-protocol-mixin)
					   #+Silica medium string start end
					   style cursor-x max-x
					   &optional glyph-buffer)
  (declare (values write-char next-char-index new-cursor-x new-baseline new-height font))
  (declare (type coordinate cursor-x max-x))
  (declare (fixnum start end))
  (stream-scan-string-for-writing-body))

;;; This function returns NIL as its first value if the character won't
;;; fit on the current line.  It is never called on non-graphic
;;; characters; they are handled in WRITE-CHAR instead.
(defmethod stream-scan-character-for-writing ((stream output-protocol-mixin) character
					      style cursor-x max-x)
  (declare (values char-normal new-cursor-x new-baseline new-height font))
  (declare (type coordinate cursor-x max-x))
  (multiple-value-bind (index font escapement-x escapement-y origin-x origin-y bb-x bb-y)
      (stream-glyph-for-character stream character style)
    (declare (ignore escapement-y origin-x bb-x))
    (when (> (+ cursor-x escapement-x *character-wrap-indicator-width*) max-x)
      (return-from stream-scan-character-for-writing (values nil cursor-x 0 0)))
    (incf cursor-x escapement-x)
    (values t cursor-x origin-y bb-y font index)))

(defun-inline draw-character-wrap-indicator (stream y height max-x &optional (old-record-p t))
  (if old-record-p
      (with-output-recording-options (stream :record nil :draw t)
	(draw-rectangle-internal
	  stream 0 0
	  (- max-x *character-wrap-indicator-width* -1) y
	  ;; HEIGHT can be zero if the line has only Tabs in it...
	  max-x (+ y (if (zerop height) 10 height))
	  +foreground-ink+ nil))
      (draw-rectangle-internal
	stream 0 0
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
    (* tab-size (ceiling (1+ cursor-x) tab-size))))

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

;;; Input Editor support.  This does STREAM-SCAN-STRING-FOR-WRITING and
;;; other stuff, and calls the continuation so the Input Editor can know
;;; where its strings are on the screen.
(defmethod do-text-screen-real-estate ((stream output-protocol-mixin) continuation
				       string start end
				       cursor-x cursor-y height baseline style max-x)
  ;; Continuation is a function which takes L T R B Baseline
  (declare (dynamic-extent continuation)
	   (values new-cursor-x new-cursor-y new-height new-baseline))
  (unless start (setq start 0))
  (unless end (setq end (length string)))
  (let ((vsp (stream-vertical-spacing stream))
	#+Silica (medium (sheet-medium stream)))
    (loop
      (when (>= start end) (return (values cursor-x cursor-y height baseline)))
      (multiple-value-bind (write-char next-char-index new-cursor-x new-baseline new-height)
	  (stream-scan-string-for-writing stream #+Silica medium string start end style
					  cursor-x max-x)
	(maxf height new-height)
	(maxf baseline new-baseline) 
	(setf start next-char-index)
	(unless (= cursor-x new-cursor-x)
	  (funcall continuation cursor-x cursor-y
				new-cursor-x (+ cursor-y height vsp) baseline))
	(setf cursor-x new-cursor-x)
	(cond ((null write-char))		;Nothing to do for this char.
	      ((or (graphic-char-p write-char)	;Must have wrapped
		   (char= write-char #\Newline))
	       (incf cursor-y (+ height vsp))
	       (setf height 0 cursor-x 0 baseline 0)
	       ;; If we wrapped, rescan the character normally, but if this is a newline
	       ;; we're done with it.
	       (when (char= write-char #\Newline) (incf start)))
	      ;; Tabs are a little inefficient in that they call the continuation an
	      ;; extra time when they could be folded into the rest of the string, but
	      ;; I don't think anybody will notice, especially since the callers of
	      ;; this function merge the rectangles.
	      ((char= write-char #\Tab)
	       (let ((new-cursor-x (stream-next-tab-column stream cursor-x style)))
		 (when (> (+ new-cursor-x *character-wrap-indicator-width*) max-x)
		   (incf cursor-y (+ height vsp))
		   (setf height 0 cursor-x 0 baseline 0)
		   (setf new-cursor-x (stream-next-tab-column stream cursor-x style)))
		 (funcall continuation cursor-x cursor-y
				       new-cursor-x (+ cursor-y height vsp) baseline)
		 (setf cursor-x new-cursor-x)
		 (incf start)))
	      (t (error "~S found char ~A, and doesn't know what to do."	; ??
			'do-text-screen-real-estate write-char)))))))
