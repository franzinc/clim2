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

"Copyright (c) 1989, 1990, 1991 International Lisp Associates.  All rights reserved."

;;; This file implements our extended output protocol on top of the
;;; proposed "standard" protocol defined in cl-streams.lisp.


;;; This is the class that you mix in to any extended output stream
;;; implementation that you define.  It exists only to provide the
;;; extended-output-stream-p method and to hang
;;; implementation-independent code.
(defclass basic-extended-output-protocol
	  (fundamental-character-output-stream)
     ()
  )

(define-protocol-p-method extended-output-stream-p basic-extended-output-protocol)

(defclass output-protocol-mixin
	  (basic-extended-output-protocol)
     ((cursor-x :initform 0)
      (cursor-y :initform 0)
      (baseline :initform 0 :accessor stream-baseline)
      (foreground :initform +black+
		  :accessor stream-foreground
		  :initarg :stream-foreground)
      (background :initform +white+
		  :accessor stream-background
		  :initarg :stream-background)
      (current-line-height :initform 0 :accessor stream-current-line-height)
      (vertical-space :initform 2 :accessor stream-vsp :initarg :vsp)
      (end-of-line-action :initarg :end-of-line-action
			  :accessor stream-end-of-line-action)
      (end-of-page-action :initarg :end-of-page-action
			  :accessor stream-end-of-page-action)
      
      (text-margin :initarg :text-margin)
      (default-text-margin :accessor stream-default-text-margin
	:initform most-positive-fixnum)

      ;; TSTYLE
      ;; delete these 
      
;      (default-text-style :initarg :default-text-style
;			  :accessor stream-default-text-style
;			  :initform *default-text-style*)
;      (current-text-style :accessor stream-current-text-style
;			  :initform *null-text-style*)
;      (merged-text-style :initform *default-text-style*)
;      (merged-text-style-valid :initform t
;			       :accessor
;			       stream-merged-text-style-valid)
      
      (output-glyph-buffer :initform (make-array 512 :element-type '(unsigned-byte 16))
						;--- 16 bit indices into fonts big enough? ---
			   :accessor stream-output-glyph-buffer)


      (user-transformation :initform +identity-transformation+
			   :accessor stream-user-transformation)
      (default-view :initform +textual-view+
		    :accessor stream-default-view)
      )
  (:default-initargs :end-of-line-action :wrap
		     :end-of-page-action :scroll
		     :text-margin nil)
  )

(defmethod initialize-instance :after ((stream output-protocol-mixin)
				       &key text-margin background
;;; --- I don't think this should say &ALLOW-OTHER-KEYS -- rsl
				       &allow-other-keys)
  (when text-margin
    (setq text-margin (process-spacing-arg stream text-margin))
    (setf (slot-value stream 'text-margin) text-margin))
  
  #+ignore
  (with-slots (current-text-style default-text-style merged-text-style merged-text-style-valid)
	      stream
    (setf merged-text-style (merge-text-styles current-text-style default-text-style)
	  merged-text-style-valid t))
  
  ;; I don't think this is absolutely necessary, but it can't hurt.
  #+ignore
  (when background
    (setf (ws::pane-background stream) background)))

;;; The next four methods keep the stream<->pane<->medium ink consistency together.
;;; --- I wonder if this should be called MEDIUM-FOREGROUND or FOREGROUND-INK instead 
;;; of STREAM-FOREGROUND, which isn't very descriptive. --- rsl
(defmethod (setf stream-foreground) :after (new-value (stream output-protocol-mixin))
  (let ((medium (sheet-medium stream)))
    ;; Watch out for uninitialized MEDIUM slot.
    (when (and medium (typep medium 'medium))
      (setf (medium-foreground medium) new-value))))

(defmethod (setf stream-background) :after (new-value (stream output-protocol-mixin))
  ;; Setting the pane-background will set the medium-background, so comment it out here.
  ;; (setf (medium-background (sheet-medium stream)) new-value)
  ;; --- Kluge so repaint will clear the window with the right ink
  #+ignore
  (setf (ws::pane-background stream) new-value))

;;; When we set the background of a sheet, propagate to the medium.

#+ignore
(defmethod (setf ws::pane-background) :after (new-value (pane output-protocol-mixin))
  (let ((medium (sheet-medium pane)))
    ;; Watch out for uninitialized MEDIUM slot.
    (when (and medium (typep medium 'medium))
      (setf (medium-background medium) new-value))))

(defmethod engraft-medium :after ((medium medium) port
				  (stream output-protocol-mixin))
  (declare (ignore port))
  (setf (medium-foreground medium) (stream-foreground stream)
	(medium-background medium) (stream-background stream)))

;;; The default text margin, by the way, isn't in user-visible coordinates.
(defmethod stream-text-margin ((stream output-protocol-mixin))
  (or (slot-value stream 'text-margin)
      (stream-default-text-margin stream)))

(defmethod (setf stream-text-margin) (new-value (stream output-protocol-mixin))
  ;; --- does process-spacing-arg want to be moved out of table formatting
  ;; --- to some more "common" place?
  (let ((text-margin (process-spacing-arg stream new-value)))
    (setf (slot-value stream 'text-margin) text-margin)))


(defmethod get-transformation ((stream output-protocol-mixin))
  (stream-user-transformation stream))

;;; wrap, scroll, allow
;;; Genera supports passing an environment to CONSTANTP and EVAL.  Allegro doesn't.  Until we test
;;; all other candidates, be conservative.
(defmacro with-end-of-page-action ((action &optional stream) &body body #+Genera &environment #+Genera env)
  (default-output-stream stream)
  (let ((actions '(:wrap :scroll :allow))
	(assert-required t)
	(wrapped-body `(letf-globally (((stream-end-of-page-action ,stream) ,action))
			 ,@body)))
    (when (constantp action #+Genera env)
      (setf action (eval action #+Genera env))
      (if (member action actions)
	  (setf assert-required nil)
	  (warn "~S action must be one of ~S, not ~S" 'with-end-of-page actions action))
      (setf action `',action))
    (when assert-required
      (setf wrapped-body
	    `(progn (assert (member ,action ',actions))
		    ,wrapped-body)))
    wrapped-body))

(defmacro with-end-of-line-action ((action &optional stream) &body body #+Genera &environment #+Genera env)
  (default-output-stream stream)
  (let ((actions '(:wrap :scroll :allow))
	(assert-required t)
	(wrapped-body `(letf-globally (((stream-end-of-line-action ,stream) ,action))
			 ,@body)))
    (when (constantp action #+Genera env)
      (setf action (eval action #+Genera env))
      (if (member action actions)
	  (setf assert-required nil)
	  (warn "~S action must be one of ~S, not ~S" 'with-end-of-line actions action))
      (setf action `',action))
    (when assert-required
      (setf wrapped-body
	    `(progn (assert (member ,action ',actions))
		    ,wrapped-body)))
    wrapped-body))


(defmacro with-text-style ((style &optional stream) &body body)
  (default-output-stream stream with-text-style)
  `(flet ((with-text-style-body (,stream) ,@body))
     (declare (dynamic-extent #'with-text-style-body))
     (silica::with-text-style-internal ,stream ,style #'with-text-style-body
			       ,stream)))

(defmethod silica::with-text-style-internal ((stream encapsulating-stream-mixin)
				     style continuation original-stream)
  (silica::with-text-style-internal (slot-value stream 'stream)
    style continuation original-stream))

#+ignore
(defmethod (setf stream-default-text-style) :before (new (stream output-protocol-mixin))
  (declare (ignore new))
  (setf (slot-value stream 'merged-text-style-valid) nil))



#+ignore
(defmethod with-text-style-internal ((stream output-protocol-mixin)
				     style continuation original-stream)
  (if (or (null style) (eql style *null-text-style*))
      (funcall continuation original-stream)
      (letf-globally (((stream-merged-text-style-valid stream) nil)
		      ((slot-value stream 'merged-text-style)
		       (slot-value stream 'merged-text-style))
		      ((medium-text-style stream)
		       (merge-text-styles style (medium-text-style stream))))
	(funcall continuation original-stream))))


#+ignore
(defmethod stream-merged-text-style ((stream output-protocol-mixin))
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

(defmethod stream-set-cursor-position* ((stream output-protocol-mixin)
					x y)
  (with-slots (cursor-x cursor-y) stream
    (when x (setf cursor-x x))
    (when y (setf cursor-y y)))

  ;; --- Call explicitly in Silica version, rather than around methods on
  ;; --- intermediary class.
  (stream-ensure-cursor-visible stream x y))


;;; Make normal output streams obey some parts of other protocols for efficiency.

;;; For "normal" output streams, there are no margins.
(defmethod window-margins ((stream output-protocol-mixin))
  (values 0 0 0 0))

;;; For "normal" output streams, there are no margins, so the pointer is never in the margins.
(defmethod window-coordinates-in-margins ((stream output-protocol-mixin) x y)
  (declare (ignore x y))
  nil)

;;; "Normal" output streams don't do anything different when told to draw in the margins.
(defmethod drawing-in-margin-internal ((stream output-protocol-mixin) continuation)
  (funcall continuation))

;;; "Normal" output streams are never recording.
(defmethod stream-record-p ((stream output-protocol-mixin))
  nil)

;;; It is an error to set record-p on non-recording streams to non-NIL.
(defmethod (setf stream-record-p) (new-value (stream output-protocol-mixin))
  (when new-value (error "Attempt to set RECORD-P for stream ~S" stream)))

;;; "Normal" output streams are always drawing.
(defmethod stream-draw-p ((stream output-protocol-mixin))
  t)

;;; It is an error to set draw-p on non-recording streams to NIL.
(defmethod (setf stream-draw-p) (new-value (stream output-protocol-mixin))
  (when (not new-value) (error "Attempt to set DRAW-P for stream ~S" stream)))





(defmethod stream-force-output ((stream output-protocol-mixin))
  (port-force-output (port stream)))

(defmethod stream-finish-output ((stream output-protocol-mixin))
  (port-finish-output (port stream)))

(defmethod stream-terpri ((output-stream output-protocol-mixin))
  (stream-write-char output-stream #\newline)
  nil)

(defmethod stream-fresh-line ((output-stream output-protocol-mixin))
  (unless (zerop (slot-value output-stream 'cursor-x))
    (stream-write-char output-stream #\newline)
    t))

;;; We should definitely be able to support this.
(defmethod stream-start-line-p ((stream output-protocol-mixin))
  (zerop (slot-value stream 'cursor-x)))

;;; I don't know if this is better than just letting the default method work
(defmethod stream-line-column ((stream output-protocol-mixin))
  (ceiling (slot-value stream 'cursor-x)
	   (stream-character-size stream #\Space)))

;;; Required methods
;stream-finish-output
;stream-force-output
;stream-clear-output
;stream-write-string-internal
;stream-write-char-internal

;;; We are not yet implementing the proposal's stream-start-line-p and friends

(defmethod stream-increment-cursor-position* ((stream output-protocol-mixin) dx dy)
  (with-slots (cursor-x cursor-y) stream
    (when dx
      (incf cursor-x dx))
    (when dy
      (incf cursor-y dy)))

  ;; --- See comment on stream-set-cursor-position*
  (stream-ensure-cursor-visible stream))

(defmethod stream-advance-cursor-x ((stream output-protocol-mixin) amount)
  (with-slots (cursor-x) stream
    (incf cursor-x amount))
  ;; --- See comment on stream-set-cursor-position*
  (stream-ensure-cursor-visible stream)
  (values))

(defmethod stream-advance-cursor-line ((stream output-protocol-mixin))
  (with-slots (cursor-x cursor-y vertical-space current-line-height baseline) stream
    (let ((line-height (stream-line-height stream)))
      (incf cursor-y (+ line-height vertical-space))
      (setf cursor-x 0 current-line-height 0 baseline 0)))
  ;; --- See comment on stream-set-cursor-position*
  (stream-ensure-cursor-visible stream)
  (values))

;;; --- Should probably be on some intermediary class, since it can only
;;; be run when the stream is part of a WINDSHIELD hierarchy.

;; --- See comment on stream-set-cursor-position*
(defmethod stream-ensure-cursor-visible ((stream output-protocol-mixin)
					 &optional cx cy &aux ocy)
  ;; --- Grrr.  Without this random reference to CX the Genera compiler
  ;; complains that CX was not referenced.  If I (declare (ignore cx))
  ;; it complains that an ignored variable was used (in the SETQ below)
  cx
  ;; --- this only handles the vertical case today
  (when (and (not (eql (stream-end-of-page-action stream) ':allow))
	     (or (not (output-recording-stream-p stream))
		 (stream-draw-p stream))
	     (pane-scroller stream))
    (unless cy 
      (multiple-value-setq (cx cy) 
	(stream-cursor-position* stream)))
    
    ;; --- kludge UNLESS to prevent infinite recursion.  WINDOW-CLEAR clears
    ;; the output history then sets the cursorpos to 0,0.  Clearing the
    ;; output history does SCROLL-HOME (which scrolls the stream pane to
    ;; 0,0), then sets cursorpos to 0,0, which comes in here and tries to scroll to
    ;; 0,28, which causes the stream pane to be repainted, which may run a
    ;; frame display function that does window-clear, sending us back
    ;; through again.
    
    (setq ocy cy)

    (unless (= cy 0)
      (incf cy (min (bounding-rectangle-height stream)
		    (* 2 (stream-line-height stream)))))
    
    ;; If the cursor moves outside the current region, expand
    ;; it to include the new cursor position.
    (when (> cy (bounding-rectangle-height stream))
      (update-region stream 0 cy :no-repaint t))

    ;; Don't scroll if the desired cursor position is within
    ;; the part of the output history currently visible in the viewport.
    ;; I don't think that this code is any more expensive than what
    ;; would happen if we unconditionally called scroll-extent.
    (let ((viewport (pane-viewport stream)))
      ;; We just need the viewport's size
      (multiple-value-bind (vpw vph)
	  (bounding-rectangle-size (sheet-region viewport))
	(multiple-value-bind (vpx vpy)
	    ;; Transform the cursor position (which is in output history
	    ;; coordinates) into viewport coordinates.
	    (transform-point* (sheet-transformation stream)
						   cx cy)
	  ;; Only scroll if the transformed cursor position is outside the
	  ;; extent of the viewport.
	  (unless (and (<= 0 vpx vpw)				; <= or < or what?
		       (<= 0 vpy vph))
	    #+ignore
	    (format excl:*initial-terminal-io* "vpx = ~D, vpw ~D, vpy ~
~D vph ~D~%" vpx vpw vpy vph)
	    (scroll-extent stream :x 0 :y ocy)))))))


(defparameter *character-wrap-indicator-width* 3)

(defmethod stream-write-char ((stream output-protocol-mixin) character)
  (multiple-value-bind (cursor-x cursor-y baseline height style max-x record-p draw-p color)
      (decode-stream-for-writing stream)
    (cond ((or (graphic-char-p character) (diacritic-char-p character))
	   (let ((medium  (sheet-medium stream)))
	     (dotimes (i 2)
	       (multiple-value-bind (no-wrap new-cursor-x new-baseline new-height font index)
		   (stream-scan-character-for-writing stream character style
						      cursor-x max-x)
		 (declare (ignore index font))
		 (when no-wrap
		   (when record-p (add-character-output-to-output-record
				    stream character style (- new-cursor-x cursor-x)
				    new-height new-baseline))
		   (when draw-p
		     (when (< baseline new-baseline)
		       (stream-note-line-height-change stream baseline new-baseline cursor-y
						       height new-height)
		       (setf baseline new-baseline))
		     ;; --- need draw-glyphs, which will take a port-specific font object, as well
		     ;; as the :INK option.

		     (draw-text* medium character	; (code-char index)??
				 cursor-x (+ cursor-y (- baseline new-baseline))
				 :text-style style
				 :align-y :top))
		   
		   (encode-stream-after-writing stream new-cursor-x cursor-y
						(max baseline new-baseline)
						(max height new-height))
		   (return))
		 (case i
		   (0 (stream-handle-line-wrap stream cursor-y height max-x draw-p record-p)
		      (multiple-value-setq (cursor-x cursor-y baseline height)
			(decode-stream-for-writing stream t)))
		   (1 (error "Couldn't fit character ~S into window"
			     character)))))))
	  
	  ((eql character #\Tab)
	   (let ((new-cursor-x (stream-next-tab-column stream cursor-x style)))
	     (when (> (+ new-cursor-x *character-wrap-indicator-width*) max-x)
	       (stream-handle-line-wrap stream cursor-y height max-x draw-p record-p)
	       (multiple-value-setq (cursor-x cursor-y baseline height)
		 (decode-stream-for-writing stream t))
	       (setf new-cursor-x (stream-next-tab-column stream cursor-x style)))
	     (when record-p
	       (add-character-output-to-output-record	;Have to handle tabs in output record
		 stream character style  (- new-cursor-x cursor-x)
		 height baseline))
	     ;;; We always want to update the cursor -- it will be put back if only recording.
	     (encode-stream-after-writing stream new-cursor-x cursor-y
					  baseline height)))
	  
	  ((eql character #\Newline)
	   (when record-p (close-current-text-output-record stream))
	   (stream-advance-cursor-line stream))
	  
	  (t (multiple-value-bind (new-cursor-x new-cursor-y new-baseline new-height)
		 (stream-draw-lozenged-character stream character cursor-x cursor-y baseline
						 height style color record-p draw-p)
	       (encode-stream-after-writing stream new-cursor-x new-cursor-y
					    new-baseline new-height)))))
    character)					;Return the right value

(defvar *lozenged-character-base* #+Genera 8. #-Genera 16.)
(defvar *lozenged-character-text-style*
	(merge-text-styles '(nil nil #+Tiny-chars :tiny #-Tiny-chars :very-small)
			   *default-text-style*))
(defvar *lozenged-character-upcase-name* #+Tiny-chars t #-Tiny-chars nil)




(defmethod stream-draw-lozenged-character ((stream output-protocol-mixin) character
					   cursor-x cursor-y baseline height style color
					   record-p draw-p)
  (let ((name (or (char-name character)
		  (write-to-string (char-code character) :base *lozenged-character-base*
				   :radix t))))
    (when *lozenged-character-upcase-name* (setf name (string-upcase name)))
    (multiple-value-bind (last-x largest-x last-y total-height lozenged-baseline)
	(stream-string-output-size stream name :text-style *lozenged-character-text-style*)
      (declare (ignore largest-x last-y))
      (let* ((medium (sheet-medium stream))
	     (hex-height (logand (+ total-height 5) -2))	;Make it be even.
	     (hex-half-height (ceiling hex-height 2))
	     (new-line-height (max height hex-height))
	     (new-line-offset (- new-line-height height))
	     (new-baseline (+ baseline new-line-offset))
	     (hex-top (+ cursor-y (- new-baseline hex-height)))
	     (hex-bottom (+ cursor-y new-baseline))
	     (hex-extreme-points-y (- hex-bottom hex-half-height))
	     (hex-extreme-left cursor-x)
	     (hex-left (+ cursor-x hex-half-height 1))
	     (hex-right (+ hex-left last-x 1))
	     (hex-extreme-right (+ hex-right hex-half-height 1))
	     (new-cursor-x (+ hex-extreme-right 1)))
	(when record-p
	  (add-character-output-to-output-record
	    stream character style color
	    (- new-cursor-x cursor-x) new-line-height new-baseline))
	(when draw-p 
	  (with-drawing-options (medium :text-style *lozenged-character-text-style* :ink color)
	    (draw-text* medium name
			(+ hex-left 1) (- hex-bottom (- total-height lozenged-baseline) 1)
			:align-y :bottom)
	    (draw-line* medium hex-left hex-top hex-right hex-top)
	    (draw-line* medium hex-left hex-bottom hex-right hex-bottom)
	    (draw-line* medium hex-left hex-top hex-extreme-left hex-extreme-points-y)
	    (draw-line* medium hex-left hex-bottom hex-extreme-left hex-extreme-points-y)
	    (draw-line* medium hex-right hex-top hex-extreme-right hex-extreme-points-y)
	    (draw-line* medium hex-right hex-bottom hex-extreme-right hex-extreme-points-y)))
	(values new-cursor-x cursor-y new-baseline new-line-height)))))

(defmethod stream-write-string ((stream output-protocol-mixin) string &optional (start 0) end)
  (unless end (setf end (length string)))
  (when (>= start end)						;No promises to keep
    (return-from stream-write-string string))
  #+(or Cloe-Runtime XLIB)					;--- Figure this out!!!
  (when (char= (aref string start) #\Newline)
    (incf start)
    (terpri stream)
    (when (= start end)
      (return-from stream-write-string string)))
					    
  (multiple-value-bind (cursor-x cursor-y baseline height style max-x
			record-p draw-p color )
      (decode-stream-for-writing stream)
    (unless (or draw-p record-p) (return-from stream-write-string string))	;No deeds to do
    (let ((medium (sheet-medium stream)))
      (loop
	(multiple-value-bind (write-char next-char-index
			      new-cursor-x new-baseline new-height font)
	    (stream-scan-string-for-writing stream  medium string start end style
					    cursor-x max-x )
	  (declare (ignore font))
	  (when record-p
	    (add-string-output-to-output-record
	      stream string start next-char-index style
	      (- new-cursor-x cursor-x) new-height new-baseline))
	  (when draw-p
	    (when (< baseline new-baseline)
	      (stream-note-line-height-change stream baseline new-baseline cursor-y
					      height new-height)
	      (setf baseline new-baseline))
	    (let ((amount (- next-char-index start)))
	      ;; --- Don't try to write empty strings.
	      ;; But, I don't know why this algorithm produces them!
	      (unless (zerop amount)

		;; --- need draw-glyphs, which will take a port-specific font object, as well
		;; as the :INK option.

		(draw-text* medium string cursor-x (+ cursor-y (- baseline new-baseline))
			    :start start :end next-char-index :text-style style
			    :align-y :top)

		)))
	  (setf baseline (max baseline new-baseline)
		height (max height new-height)
		cursor-x new-cursor-x)
	  (when (>= next-char-index end)
	    ;; Always update the cursor even if only recording.  It will be restored later.
	    (encode-stream-after-writing stream cursor-x cursor-y baseline height)
	    (return-from stream-write-string string))
	  (setf start next-char-index)
	  (when write-char					;Some random character
	    (encode-stream-after-writing stream cursor-x cursor-y baseline height)
	    (stream-write-char stream write-char)		;Take care of everything random
	    (incf start)
	    (when (>= start end)
	      (return-from stream-write-string string))
	    (multiple-value-setq (cursor-x cursor-y baseline height)
	      (decode-stream-for-writing stream t))		;and find out what happened
	    ))))))

(defmethod stream-note-line-height-change ((stream output-protocol-mixin)
					   old-baseline new-baseline cursor-y
					   old-height new-height)
  (let ((movement (- new-baseline old-baseline))
	(line-height-change (- new-height old-height)))
    (when (plusp movement)
      (stream-move-for-line-height-change stream movement line-height-change cursor-y))))

(defmethod stream-move-for-line-height-change ((stream output-protocol-mixin)
					       movement line-height-change cursor-y)
  (declare (ignore movement line-height-change cursor-y)))

;;; Simple version: does no wrapping, assumes we start at leftmost
;;; column.  It doesn't do what Genera :STRING-LENGTH message does,
;;; useful as that might be.
(defmethod stream-string-output-size ((stream output-protocol-mixin)
				      string &key (start 0) end text-style)
  #+Genera (declare (values last-x largest-x last-y total-height baseline))
  (unless end (setf end (length string)))
  (let ((style (or text-style (medium-merged-text-style stream)))
	(cursor-x 0)
	(cursor-y 0)
	(height 0)
	(vertical-space (stream-vsp stream))
	(baseline 0)
	(largest-x 0))
    (let ((medium  (sheet-medium stream)))
      (loop
	(when (>= start end) (return))
	(multiple-value-bind (write-char next-char-index new-cursor-x new-baseline new-height)
	    (stream-scan-string-for-writing stream  medium string start end style
					    cursor-x most-positive-fixnum)
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
							  cursor-x most-positive-fixnum)
		     (declare (ignore no-wrap))
		     (maxf largest-x new-cursor-x)
		     (maxf baseline new-baseline)
		     (maxf height new-height)
		     (setf cursor-x new-cursor-x)))
		  ((eql write-char #\Newline)
		   (setf cursor-x 0)
		   (incf cursor-y (+ height vertical-space))
		   (setf baseline 0 height (stream-line-height stream style)))
		  ((eql write-char #\Tab)
		   (setf cursor-x (stream-next-tab-column stream cursor-x style))
		   (maxf largest-x cursor-x))
		  (t (multiple-value-bind (new-cursor-x new-cursor-y new-baseline new-height)
			 (stream-draw-lozenged-character stream write-char cursor-x cursor-y
							 baseline height style nil nil nil)
		       (setf cursor-x new-cursor-x cursor-y new-cursor-y
			     baseline new-baseline height new-height))))
	    (incf start)))))
    (values cursor-x largest-x cursor-y (+ cursor-y height) baseline)))

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

(defmethod stream-tab-size ((stream output-protocol-mixin) style)
  (multiple-value-bind (index font escapement-x escapement-y origin-x origin-y bb-x bb-y)
      (stream-glyph-for-character stream #\n style)	;En space
    (declare (ignore index font escapement-y origin-x origin-y bb-y))
    (* (max bb-x escapement-x) 8.)))

;;;--- These forwarding methods are for Silica conversion convenience only.
;;; Their callers should be changed to invoke the method on the medium directly.

(defmethod stream-glyph-for-character ((stream output-protocol-mixin) character appearance &optional our-font)
  (stream-glyph-for-character (sheet-medium stream) character appearance our-font))


(defmethod stream-write-char-internal ((stream output-protocol-mixin) index x-font color x y)
  (stream-write-char-internal (sheet-medium stream) index x-font color x y))

;;;--- This forwarding method is for Silica conversion convenience only.

(defmethod stream-write-string-internal ((stream output-protocol-mixin)
					 glyph-buffer start end x-font color x y)
  (stream-write-string-internal (sheet-medium stream) glyph-buffer start end x-font color x y))


(defmethod implementation-pixels-per-point ((stream output-protocol-mixin))
  (graft-pixels-per-point (graft stream)))

;;; A few utilities for string writing.
(defmethod decode-stream-for-writing ((stream output-protocol-mixin) &optional brief-p)
  #+Genera (declare (values cursor-x cursor-y baseline line-height style max-x
			    record-p draw-p color ))
  (multiple-value-bind (cursor-x cursor-y) (stream-cursor-position* stream)
    (let ((baseline (stream-baseline stream))
	  (line-height (stream-current-line-height stream))
	  (wrap-p (unless brief-p (eq (stream-end-of-line-action stream) ':wrap))))
      (if brief-p
	  (values cursor-x cursor-y baseline line-height)
	  (values cursor-x cursor-y baseline line-height
		  (medium-merged-text-style stream)
		  (cond ((and wrap-p
			      (stream-text-margin stream))
			 )
			(t most-positive-fixnum))
		  (stream-record-p stream)
		  (stream-draw-p stream)
		  (medium-ink (sheet-medium stream))
		  ;; The glyph buffer is not in use in the Silica version, unfortunately.

		  ;; Does anybody actually use this value?  I can't find anybody...

		  )))))

(defmethod encode-stream-after-writing ((stream output-protocol-mixin) cursor-x cursor-y
					baseline line-height)
  (stream-set-cursor-position* stream cursor-x cursor-y)
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
;;;
;;; Expanded commentary 31 January 1991:
;;;
;;; STREAM-SCAN-STRING-FOR-WRITING is a method which calls the generic function
;;; STREAM-GLYPH-FOR-CHARACTER inside its main loop.  This was shown to cause a
;;; significant performance problem in early testing.  In order to improve this
;;; situation, we took the body of STREAM-SCAN-STRING-FOR-WRITING and placed it into a
;;; macro.  Each class which wishes to take advantage of this optimization writes a
;;; macro which MACROLETs an inline version of STREAM-GLYPH-FOR-CHAR around the
;;; expansion of the macro STREAM-SCAN-STRING-FOR-WRITING-BODY.  The method for
;;; STREAM-SCAN-STRING-FOR-WRITING below (on OUTPUT-PROTOCOL-MIXIN) is the only one
;;; which actually uses the generic function dispatch; an implementor who fails to
;;; provide a method for STREAM-SCAN-STRING-FOR-WRITING on his/her own medium class
;;; will use this fallback.  Note that an implementor is still required to provide a
;;; method for STREAM-GLYPH-FOR-CHARACTER in any case.
;;;
;;; See the files CLIM:CLIM;CLIM-<medium type>-STUFF (e.g., CLIM-X-STUFF) for details
;;; about how this can be done.

(defmacro stream-scan-string-for-writing-body ()
  '(let ((baseline 0)
	 (height 0)
	 (our-font nil)
	 (next-glyph (if (and glyph-buffer (array-has-fill-pointer-p glyph-buffer))
			 (fill-pointer glyph-buffer)
			 0))
	 (max-glyph (and glyph-buffer (array-dimension glyph-buffer 0)))
	 (string string)
	 #+Genera				;For array-register binding only.
	 (glyph-buffer (or glyph-buffer #())))	;Array-register declaration requires array.
     #+Genera (declare (sys:array-register string glyph-buffer))
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
	        medium
	       character style our-font)
	   (declare (ignore escapement-y origin-x bb-x))
	   (when fixed-width-font-p
	     (let* ((room-left (- max-x cursor-x *character-wrap-indicator-width*))
		    (spaces-left (floor room-left escapement-x))
		    (chars-left (- end start))
		    (glyphs-left (if max-glyph (- max-glyph next-glyph) most-positive-fixnum))
		    (chars-to-do (min spaces-left chars-left glyphs-left))
		    (chars-done 0)
		    (wrap-char (when (< spaces-left chars-left)
				 (aref string (+ start spaces-left))))
		    (offset (- index (char-code character))))
	       ;; Assumption: character glyphs are at the same offset within the font
	       ;; that they are within the character set.
	       (block scan-for-newlines-etc
		 (dotimes (i chars-to-do)
		   (let* ((char (aref string (+ start i))))
		     ;; Stop when we get to an unusual character (e.g. Newline)
		     (unless (or (graphic-char-p char) (diacritic-char-p char))
		       (return-from scan-for-newlines-etc))
		     ;; When a glyph-buffer was supplied, store the glyph index into it
		     (when max-glyph
		       (setf (aref glyph-buffer next-glyph)
			     (- (char-code char) offset))
		       (incf next-glyph))
		     ;; count the non-unusual chars
		     (incf chars-done))))
	       (return-from stream-scan-string-for-writing
		 (values wrap-char (+ start chars-done)
			 (+ cursor-x (* chars-done escapement-x))
			 origin-y bb-y font))))
	   (when (> (+ cursor-x escapement-x *character-wrap-indicator-width*) max-x)
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
	     (incf next-glyph))
	   (incf cursor-x escapement-x)
	   (maxf baseline origin-y)
	   (maxf height bb-y)
	   (setf our-font font)
	   (incf start))))))

(defmethod stream-scan-string-for-writing ((stream output-protocol-mixin)
					    medium string start end
					   style cursor-x max-x
					   &optional glyph-buffer)
  #+Genera (declare
	     (values write-char next-char-index new-cursor-x new-baseline new-height font))
  (stream-scan-string-for-writing-body))

;;; This function returns NIL as its first value if the character won't
;;; fit on the current line.  It is never called on non-graphic
;;; characters; they are handled in WRITE-CHAR instead.
(defmethod stream-scan-character-for-writing ((stream output-protocol-mixin) character
					      style cursor-x max-x)
  #+Genera (declare (values char-normal new-cursor-x new-baseline new-height font))
  (multiple-value-bind (index font escapement-x escapement-y origin-x origin-y bb-x bb-y)
      (stream-glyph-for-character stream character style)
    (declare (ignore escapement-y origin-x bb-x))
    (when (> (+ cursor-x escapement-x *character-wrap-indicator-width*) max-x)
      (return-from stream-scan-character-for-writing (values nil cursor-x 0 0)))
    (incf cursor-x escapement-x)
    (values t cursor-x origin-y bb-y font index)))


(defmethod stream-handle-line-wrap ((stream output-protocol-mixin) cursor-y height max-x
				    draw-p record-p)
  (declare (ignore draw-p max-x height cursor-y))
  (when record-p (close-current-text-output-record stream t))
  #+ignore
  (when draw-p
    (with-output-recording-options (stream :draw-p t :record-p nil)
      ;; removed obsolete with-identity-transform
      (draw-rectangle* (- max-x *character-wrap-indicator-width* -1) cursor-y
			 max-x (+ cursor-y height)
			 :stream stream)))
  (stream-advance-cursor-line stream))

(defmethod stream-next-tab-column ((stream output-protocol-mixin) cursor-x style)
  (let ((tab-size (stream-tab-size stream style)))
    (* tab-size (ceiling (1+ cursor-x) tab-size))))

;;; Input Editor support.  This does STREAM-SCAN-STRING-FOR-WRITING and
;;; other stuff, and calls the continuation so the Input Editor can know
;;; where its strings are on the screen.
(defmethod do-text-screen-real-estate ((stream output-protocol-mixin) continuation
				       string start end
				       cursor-x cursor-y height baseline style max-x)
  ;; Continuation is a function which takes L T R B Baseline
  (declare (dynamic-extent continuation)
           #+Genera (values new-cursor-x new-cursor-y new-height new-baseline))
  (unless end (setq end (length string)))
  (let ((medium  (sheet-medium stream) )
	(vertical-space (stream-vsp stream)))
    (loop
      (when (>= start end) (return (values cursor-x cursor-y height baseline)))
      (multiple-value-bind (write-char? next-char-index new-cursor-x new-baseline new-height)
	  (stream-scan-string-for-writing stream  medium string start end style
					  cursor-x max-x)
	(maxf height new-height)
	(maxf baseline new-baseline) 
	(setf start next-char-index)
	(unless (= cursor-x new-cursor-x)
	  (funcall continuation cursor-x cursor-y new-cursor-x (+ cursor-y height) baseline))
	(setf cursor-x new-cursor-x)
	(cond ((null write-char?))		;Nothing to do for this char.
	      ;; this used to say (or graphic-char-p newline-p)
	      ((writable-character-p write-char?)		;Must have wrapped
	       (incf cursor-y (+ height vertical-space))
	       (setf height 0 cursor-x 0 baseline 0)
	       ;; We want to skip over the newline to continue the scan.
	       (when (char-equal write-char? #\Newline)
		 (incf start)))
	      ;; Tabs are a little inefficient in that they call the continuation an
	      ;; extra time when they could be folded into the rest of the string, but
	      ;; I don't think anybody will notice, especially since the callers of
	      ;; this function merge the rectangles.
	      ((char-equal write-char? #\Tab)
	       (let ((new-cursor-x (stream-next-tab-column stream cursor-x style)))
		 (when (> (+ new-cursor-x *character-wrap-indicator-width*) max-x)
		   (incf cursor-y (+ height vertical-space))
		   (setf height 0 cursor-x 0 baseline 0)
		   (setf new-cursor-x (stream-next-tab-column stream cursor-x style)))
		 (funcall continuation cursor-x cursor-y new-cursor-x (+ cursor-y height) 
			  baseline)
		 (setf cursor-x new-cursor-x)))
	      (t (error "~S found char ~A, and doesn't know what to do."	; ??
			'do-text-screen-real-estate write-char?)))))))

;;;; --- Move these methods to "window-and-output-mixin" which requires
;;;; from window-mixin and output-protocol-mixin.
;;;;; Does primitive line-wrapping.  Maybe should be increment-x only?
;;;;; What do we do when DY is non-zero and X wraps?
;(defmethod stream-advance-cursor-x ((stream output-protocol-mixin) amount)
;  (with-slots (cursor-x cursor-y) stream
;    (incf cursor-x amount)
;    (when (window-p stream)			;is this always mixed in with window-mixin?
;      ;; --- kludge until we get margins
;      (let ((right (- (window-inside-width stream) 2)))
;	;;--- conditionalize on window-stream-end-of-line-action, when we have it
;	(when (> cursor-x right)
;	  (stream-advance-cursor-line stream)))))
;  (values))
;
;(defmethod stream-advance-cursor-line ((stream output-protocol-mixin))
;  (with-slots (cursor-x cursor-y) stream
;    (let ((line-height (stream-line-height stream)))
;      (incf cursor-y line-height)
;      (setf cursor-x 0)
;      (when (window-p stream)			;is this always mixed in with window-mixin?
;	(with-slots (viewport) stream
;	  ;; If next text output would be off bottom edge, shift viewport
;	  (when (> (+ cursor-y line-height)		; text is :align :top
;		   (entity-bottom viewport))
;	    ;; --- what X position?  0 or leave alone?
;	    (window-stream-set-viewport-position
;	      stream
;	      0 (+ (- cursor-y (entity-height viewport))
;		   ;; --- The number of lines should be a per-window option
;		   line-height))))))))
