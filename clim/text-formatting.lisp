;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: text-formatting.lisp,v 1.8 91/08/05 14:35:42 cer Exp $

(in-package :clim)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

;;; Filling output

(defclass filling-stream (encapsulating-stream-mixin)
    (fill-width
     break-characters
     prefix
     prefix-width
     current-width
     (buffer :initarg :buffer)))

(defun whitespace-char-p (char)
  (or (char-equal char #\Space)
      (char= char #\Tab)))

(defmethod filling-stream-write-buffer ((filling-stream filling-stream) &optional force-p)
  (with-slots (stream fill-width break-characters current-width buffer) filling-stream
    (let ((fresh-line nil))
      (loop
	(when (zerop (fill-pointer buffer))	;Buffer empty
	  (return-from filling-stream-write-buffer nil))
	(unless (> current-width fill-width)	;Haven't reached the fill column
	  (when force-p
	    (stream-write-string stream buffer)
	    (setf (fill-pointer buffer) 0))
	  (return-from filling-stream-write-buffer nil))
	;; We've hit the fill column, search backwards for a break character
	(let ((index (position-if #'(lambda (char) (member char break-characters)) buffer
				  :from-end t)))
	  (when (or index fresh-line)
	    (stream-write-string stream buffer 0 index)
	    (setq fresh-line nil)
	    ;; Shift the remaining parts of the buffer down, skipping whitespace
	    (let ((index (and index
			      (position-if-not #'whitespace-char-p buffer :start index))))
	      (cond ((null index)
		     (setf (fill-pointer buffer) 0))
		    (t
		     (replace buffer buffer :start1 0 :start2 index)
		     (decf (fill-pointer buffer) index)))))
	  ;; After outputting what we can, go to a new line
	  ;; FRESH-LINE prevents an infinite loop if there are no break characters
	  (unless fresh-line
	    (stream-terpri stream)
	    (filling-stream-handle-line-break filling-stream)
	    (setq fresh-line t)))))))

;; Write the prefix string and then set the current width
(defmethod filling-stream-handle-line-break ((filling-stream filling-stream))
  (with-slots (stream prefix prefix-width current-width buffer) filling-stream
    ;; The "after line break" stuff is not a candidate for filling,
    ;; but it does contribute to the width of the line
    (when prefix
      (stream-write-string stream prefix))
    (setq current-width (+ prefix-width (text-size stream buffer)))))

(defmethod stream-terpri ((filling-stream filling-stream))
  (with-slots (stream current-width) filling-stream
    (filling-stream-write-buffer filling-stream t)
    (stream-terpri stream)
    (setq current-width 0)))

(defmethod stream-write-char ((filling-stream filling-stream) char)
  (with-slots (stream current-width buffer) filling-stream
    (cond ((char= char #\Newline)
	   (filling-stream-write-buffer filling-stream t)
	   (stream-terpri stream)
	   (setq current-width 0))
	  (t
	   (vector-push-extend char buffer)
	   (incf current-width (text-size stream char))
	   ;; We need to do the FILLING-STREAM-WRITE-BUFFER for every
	   ;; character since it needs to know as soon as output crosses
	   ;; the fill width.
	   (filling-stream-write-buffer filling-stream)))))

(defmethod stream-write-string ((filling-stream filling-stream) string
				&optional (start 0) end)
  ;;--- Slower than it could be, should do things chunk-wise
  (dovector (char string :start start :end end)
    (stream-write-char filling-stream char)))

(defmethod stream-force-output ((filling-stream filling-stream))
  (filling-stream-write-buffer filling-stream t))

(defmethod stream-finish-output ((filling-stream filling-stream))
  (filling-stream-write-buffer filling-stream t))

;; Need to draw the currently buffered text onto the real stream and start
;; buffering again, but remember the current cursor position instead of starting
;; at the beginning of the line.  This is the way we get presentations to be in
;; the right place, for example.
(defmethod write-buffer-and-continue ((filling-stream filling-stream)
				      continuation &rest continuation-args)
  (declare (dynamic-extent continuation continuation-args))
  (with-slots (stream fill-width buffer) filling-stream
    ;; Flush the current line buffer
    (stream-write-string stream buffer)
    (setf (fill-pointer buffer) 0)
    ;; Move to the next line if necessary, writing the prefix string
    ;;--- STREAM-CURSOR-POSITION* is the wrong thing to look at
    (when (> (stream-cursor-position* stream) fill-width)
      (stream-terpri stream)
      (filling-stream-handle-line-break filling-stream))
    (let ((*original-stream* (or *original-stream* filling-stream)))
      (apply continuation stream continuation-args))))

(defmethod with-text-style-internal ((filling-stream filling-stream)
				     style continuation original-stream)
  (labels ((filling-continuation (stream)
	     (multiple-value-prog1
	       (funcall continuation stream)
	       (close-current-text-output-record stream))))
    (declare (dynamic-extent #'filling-continuation))
    (write-buffer-and-continue filling-stream
			       #'with-text-style-internal
			       style #'filling-continuation original-stream)))

(defmethod with-output-recording-options-internal ((filling-stream filling-stream)
						   draw-p record-p continuation)
  (write-buffer-and-continue filling-stream
			     #'with-output-recording-options-internal
			     draw-p record-p continuation))

(defmethod close-current-text-output-record ((filling-stream filling-stream)
					     &optional wrapped)
  (write-buffer-and-continue filling-stream
			     #'close-current-text-output-record wrapped))

(defresource filling-stream (stream fill-width break-characters prefix prefix-width)
  :constructor (make-instance 'filling-stream
			      :buffer (make-array 100 :element-type 'extended-char
						      :fill-pointer 0
						      :adjustable t))
  :matcher 't
  :initializer
    (progn
      (setf (slot-value filling-stream 'stream) stream)
      (setf (slot-value filling-stream 'fill-width) fill-width)
      (setf (slot-value filling-stream 'break-characters) break-characters)
      (setf (slot-value filling-stream 'prefix) prefix)
      (setf (slot-value filling-stream 'prefix-width) prefix-width)
      (setf (fill-pointer (slot-value filling-stream 'buffer)) 0)
      (setf (slot-value filling-stream 'current-width) 0)))

(defun filling-output-1 (stream continuation
			 &key (fill-width '(80 :character))
			      (break-characters '(#\space))
			      after-line-break after-line-break-initially)
  (declare (dynamic-extent continuation))
  (check-type break-characters list)
  (check-type after-line-break (or null string))
  (let ((fill-width (process-spacing-arg stream fill-width 'filling-output ':fill-width))
	(prefix-width (if after-line-break (text-size stream after-line-break) 0)))
    (assert (< prefix-width fill-width) ()
	    "The prefix string ~S is wider than the fill width" after-line-break)
    (using-resource (filling-stream filling-stream
		     stream fill-width break-characters after-line-break prefix-width)
      (unwind-protect
	  (progn
	    (when after-line-break-initially
	      (filling-stream-handle-line-break filling-stream))
	    (funcall continuation filling-stream))
	(stream-force-output filling-stream)))))


;;; Indenting output

(defclass indenting-output-record (linear-output-record)
    ((indentation :initarg :indentation)))

(define-output-record-constructor indenting-output-record
				  (&key x-position y-position size indentation)
  :x-position x-position :y-position y-position :size size :indentation indentation)

(defun indenting-output-1 (stream indentation continuation &key (move-cursor t))
  (let* ((indentation (process-spacing-arg stream indentation
					   'indenting-output))
	 (indenting-record
	   (with-new-output-record (stream 'indenting-output-record nil
				    :indentation indentation)
	     (with-output-to-output-record (stream)
	       (funcall continuation stream)))))
    (multiple-value-bind (x y) (output-record-position* indenting-record)
      (output-record-set-position* indenting-record (+ x indentation) y))
    (tree-recompute-extent indenting-record)
    (replay indenting-record stream)
    (when move-cursor
      (move-cursor-beyond-output-record stream indenting-record))
    indenting-record))


;;; Textual lists

(defun format-textual-list (sequence printer
			    &key (stream *standard-output*)
				 (separator ", ") conjunction)
  (declare (dynamic-extent printer))
  (let ((length (length sequence)))
    (cond ((= length 1)
	   (funcall printer (elt sequence 0) stream))
	  ((and conjunction (= length 2))
	   (funcall printer (elt sequence 0) stream)
	   (write-char #\Space stream)
	   (write-string conjunction stream)
	   (write-char #\Space stream)
	   (funcall printer (elt sequence 1) stream))
	  (t
	   (let ((index 0))
	     (doseq (item sequence)
	       (incf index)
	       (funcall printer item stream)
	       (unless (= index length)
		 (write-string separator stream)
		 (when (and conjunction (= index (1- length)))
		   (write-string conjunction stream)
		   (write-char #\Space stream)))))))))
