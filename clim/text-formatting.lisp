;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."

;;; Filling output

(defclass filling-stream (standard-encapsulating-stream)
     (fill-width
      break-characters
      prefix
      prefix-width
      current-width
      old-buffers
      on-fresh-line
      (buffer :initarg :buffer)))

(defun flush-buffer-to-stream/filling (stream buffer &optional start end)
  (setf start (or start 0)
        end (or end (length buffer)))

  (stream-write-string stream buffer start end)
  ;; XXX: is whitespace-char-p the right thing to ask for? what about
  ;; other break characters?
  (let ((next-non-whitespace (position-if-not #'whitespace-char-p buffer
                                              :start end)))
    (cond ((null next-non-whitespace)
           (setf (fill-pointer buffer) 0))
          (t
           (replace buffer buffer :start1 0 :start2 next-non-whitespace)
           (decf (fill-pointer buffer) next-non-whitespace)))))

(defun filling-stream-buffer-to-string (filling-stream)
  (with-output-to-string (s)
    (dolist (buffer-entry (reverse (slot-value filling-stream 'old-buffers)))
      (write-string (first buffer-entry) s))
    (write-string (slot-value filling-stream 'buffer) s)))

(defun break-character-predicate (stream)
  (lambda (c)
    (member c (slot-value stream 'break-characters))))

(defun filling-stream-flush-continuations (filling-stream force-p)
  "Returns a list of continuations that, when invoked in order,
will flush a filling-stream's buffer until the last break
character to the appropriate stream with the correct medium
options.

Also removes all buffers that would be flushed in this way from
the filling-stream's old-buffers list."
  (let ((break-characters (slot-value filling-stream 'break-characters))
        (current-buffer-entry
         `(,(slot-value filling-stream 'buffer)
            ,(slot-value filling-stream 'stream)
            ,(lambda (stream buffer start end)
               (flush-buffer-to-stream/filling stream buffer start end))))) 
    (labels ((delete-buffer-entry (buffer-entry)
               (setf (slot-value filling-stream 'old-buffers)
                     (delete buffer-entry (slot-value filling-stream 'old-buffers))))
             (buffer-flusher (buffer-entry &optional (length (length (first buffer-entry)))
                                           (delete-p t))
               (destructuring-bind (buffer stream continuation) buffer-entry
                 (lambda ()
                   (funcall continuation stream buffer 0 length)
                   (when delete-p
                     (delete-buffer-entry buffer-entry))))))
      ;; from youngest buffer entry to the oldest (i.e. right to left)
      ;; search for break-characters; if found, flush & remove all
      ;; earlier buffers, then flush that buffer until occurrence.
      (loop for (buffer-entry . rest) on (cons current-buffer-entry
                                               (slot-value filling-stream 'old-buffers))
            for (buffer) = buffer-entry
            for last-break-char-pos
              = (position-if #'(lambda (c) (member c break-characters))
                         buffer :from-end t)
            when (or last-break-char-pos force-p)
              do (return
                   (values
                    (nreverse (cons     ; flush the found buffer last
                               (buffer-flusher buffer-entry
                                               (if force-p
                                                   (length buffer)
                                                   (1+ last-break-char-pos))
                                               (or force-p (zerop (length buffer))))
                               ;; but first, flush the buffers before it.
                               (mapcar
                                (lambda (buffer-entry)
                                  (buffer-flusher buffer-entry))
                                rest)))
                    last-break-char-pos))))))

(defmethod filling-stream-write-buffer ((filling-stream filling-stream) &optional force-p)
  (with-slots (current-width on-fresh-line) filling-stream
     (let ((stream (slot-value filling-stream 'stream))
           (buffer (slot-value filling-stream 'buffer))
           (fill-width (slot-value filling-stream 'fill-width))
           (should-be-finished-now nil))
       (labels ((break-line-now ()
                  (stream-terpri stream)
                  (filling-stream-handle-line-break filling-stream)
                  (setf on-fresh-line t)))
         (loop
           (let ((old-buffers (slot-value filling-stream 'old-buffers)))
             (when (and (null old-buffers)
                        (zerop (fill-pointer buffer))) ;Buffer empty and no delayed buffers.
               (return-from filling-stream-write-buffer nil))
             (unless (or (> current-width fill-width)
                         force-p)     ;Haven't reached the fill column
               (return-from filling-stream-write-buffer nil))

             (assert (not should-be-finished-now)
                     (should-be-finished-now)
                     "whoa, buffer is ~S, old-buffers ~S, fill-pointer at ~A"
                     buffer old-buffers (fill-pointer buffer))
                       
             ;; We have a line that is larger than the fill column. it
             ;; needs to be broken.
             (multiple-value-bind (flush-continuations found-break-chars)
                 (filling-stream-flush-continuations filling-stream
                                                     force-p)
               ;; output the (possibly delayed) buffer contents until the break char.
               (dolist (continuation flush-continuations)
                 (setf on-fresh-line nil)
                 (funcall continuation))

               (cond ((and (not force-p) found-break-chars)
                      ;; Found a break char that's just before the max width
                      (break-line-now))
                     (force-p
                      ;; flush was forced. We should have written
                      ;; everything now, so bail out.
                      (return-from filling-stream-write-buffer nil))
                     ((not force-p)
                      ;; this line doesn't fit, and there are no break
                      ;; chars.  write the line on a fresh line anyway,
                      ;; and open a fresh line after that.
                      (unless on-fresh-line
                        (break-line-now))
                      (let ((force-continuations
                             (filling-stream-flush-continuations filling-stream
                                                                 t)))
                        (dolist (continuation force-continuations)
                          (setf on-fresh-line nil)
                          (funcall continuation))))))))))))

;; Write the prefix string and then set the current width
(defmethod filling-stream-handle-line-break ((filling-stream filling-stream))
  (with-slots (current-width) filling-stream
    (let ((stream (slot-value filling-stream 'stream))
          (prefix (slot-value filling-stream 'prefix))
          (prefix-width (slot-value filling-stream 'prefix-width)))
      ;; The "after line break" stuff is not a candidate for filling,
      ;; but it does contribute to the width of the line
      (when prefix
        (stream-write-string stream prefix))
      (setq current-width (+ prefix-width
                             (text-size stream (filling-stream-buffer-to-string filling-stream)))))))

(defmethod stream-terpri ((filling-stream filling-stream))
  (let ((stream (slot-value filling-stream 'stream)))
    (filling-stream-write-buffer filling-stream t)
    (stream-terpri stream)
    (filling-stream-handle-line-break filling-stream)))

(defmethod stream-fresh-line ((filling-stream filling-stream))
  (with-slots (current-width) filling-stream
    (let ((prefix-width (slot-value filling-stream 'prefix-width)))
      (unless (= current-width prefix-width)
        (stream-terpri filling-stream)
        t))))

(defmethod stream-write-char ((filling-stream filling-stream) char)
  (with-slots (current-width) filling-stream
     (let* ((stream (slot-value filling-stream 'stream))
            (fill-width (slot-value filling-stream 'fill-width))
            (text-size (text-size stream char))
            (break-chars (slot-value filling-stream 'break-characters))
            (buffer (slot-value filling-stream 'buffer)))
       (cond ((or (eql char #\Newline)
                  (eql char #\Return))
              (stream-terpri filling-stream))
             ((and (> (+ current-width text-size) fill-width)
                   (member char break-chars))
              (stream-terpri filling-stream)
              )
             (t
              (vector-push-extend char buffer)
              (incf current-width text-size)
              ;; FILLING-STREAM-WRITE-BUFFER will only do something if we
              ;; are beyond the fill-width, so optimize calls to it
              (when (> current-width fill-width)
                (filling-stream-write-buffer filling-stream)))))))

(defmethod stream-write-string ((filling-stream filling-stream) string
                                &optional (start 0) end)
  (with-slots (current-width) filling-stream
     (when (null end)
       (setq end (length string)))
     (let ((stream (slot-value filling-stream 'stream))
           (fill-width (slot-value filling-stream 'fill-width))
           (buffer (slot-value filling-stream 'buffer))
           (break-characters (slot-value filling-stream 'break-characters)))
       ;; The general scheme here is to find a break character,
       ;; buffer the string up to the break character, then call
       ;; STREAM-WRITE-CHAR on the break character to do the
       ;; necessary bookkeeping
       (loop
         (let* ((break (position-if #'(lambda (char)
                                        (or (eql char #\Newline)
                                            (eql char #\Return)
                                            (member char break-characters)))
                                    string :start start :end end))
                (width (text-size stream string
                        :start start :end (or break end))))
           (when (or (null break) (> break 0))
             (let* ((ofp (fill-pointer buffer))
                    (nfp (+ ofp (- (or break end) start))))
               (when (> nfp (array-dimension buffer 0))
                 (adjust-array buffer (+ nfp nfp)))
               (setf (fill-pointer buffer) nfp)
               (replace buffer string
                :start1 ofp
                :start2 start :end2 break)))
           (incf current-width width)
           ;; We might need to flush the buffer before we insert
           ;; the next break character
           (when (> current-width fill-width)
             (filling-stream-write-buffer filling-stream))
           (cond (break
                  (stream-write-char filling-stream (aref string break))
                  (setq start (1+ break))
                  (when (>= start end)
                    (return)))
                 (t (return))))))))

(defmethod stream-force-output ((filling-stream filling-stream))
  (filling-stream-write-buffer filling-stream t))

(defmethod stream-finish-output ((filling-stream filling-stream))
  (filling-stream-write-buffer filling-stream t))

;; Need to delay flushing the buffer until we can make a sensible
;; decision about line breaks. See spr32316 for a test case and
;; summary.
(defmethod save-buffer-and-continue ((filling-stream filling-stream) original-stream
                                     continuation &rest continuation-args)
  (let ((original-text-style (clim:medium-merged-text-style original-stream))
        (recording-p (clim:stream-recording-p original-stream))
        (drawing-p (clim:stream-drawing-p original-stream)))
    (labels ((flush-continuation (stream buffer start end)
               (with-text-style (stream original-text-style)
                 (with-output-recording-options (stream :record recording-p :draw drawing-p)
                   (flush-buffer-to-stream/filling stream buffer start end)))))
      (with-slots (buffer old-buffers stream) filling-stream
         (push (list buffer stream #'flush-continuation) old-buffers)
         (setf buffer (make-filling-stream-buffer))
         (apply continuation stream continuation-args)))))

(defmethod invoke-with-text-style ((filling-stream filling-stream)
                                   continuation style original-stream)
  (labels ((filling-continuation (stream)
             (multiple-value-prog1
               (funcall continuation stream)
               (stream-close-text-output-record stream))))
    (declare (dynamic-extent #'filling-continuation))
    (save-buffer-and-continue filling-stream original-stream 
                              #'invoke-with-text-style
                              #'filling-continuation style original-stream)))

(defmethod invoke-with-output-recording-options ((filling-stream filling-stream)
                                                 continuation record draw)
  (save-buffer-and-continue filling-stream filling-stream
                            #'invoke-with-output-recording-options
                            continuation record draw))

(defmethod stream-close-text-output-record :around ((filling-stream filling-stream)
                                                    &optional wrapped)
  (filling-stream-write-buffer filling-stream t)
  (save-buffer-and-continue filling-stream filling-stream
                            #'stream-close-text-output-record wrapped))

(defmethod invoke-with-new-output-record :around ((filling-stream filling-stream)
                                                  function record-type
                                                  constructor &rest initargs)
  (declare (ignorable initargs constructor record-type function))
  (filling-stream-write-buffer filling-stream t)
  (call-next-method))

(defun make-filling-stream-buffer ()
  (make-array 100 :element-type 'character
              :fill-pointer 0
              :adjustable t))

;;; Explicit newline hints on the filling stream
(defmethod filling-stream-conditional-newline (stream &key force &allow-other-keys)
  (when force (terpri stream)))

(defmethod filling-stream-conditional-newline ((stream filling-stream)
                                               &key force (space-for "")
                                               (text-face (text-style-face (medium-merged-text-style stream)))
                                               (text-size (text-style-size (medium-merged-text-style stream)))
                                               (text-family (text-style-family (medium-merged-text-style stream))))
  (when (or force
            (> (+ (text-size stream space-for :text-style (make-text-style text-family text-face text-size))
                  (slot-value stream 'current-width))
               (slot-value stream 'fill-width)))
    (terpri stream)))

(defresource filling-stream (stream fill-width break-characters prefix prefix-width)
  :constructor
    (make-instance 'filling-stream
      :buffer (make-array 100 :element-type 'character
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
      (setf (slot-value filling-stream 'current-width)
        (stream-cursor-position stream))
      (setf (slot-value filling-stream 'old-buffers) nil)
      (setf (slot-value filling-stream 'on-fresh-line) t)))

(defun invoke-filling-output (stream continuation
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

(defclass indenting-output-record
          (standard-sequence-output-record)
    ((indentation :initarg :indentation)))

(define-output-record-constructor indenting-output-record
                                  (&key x-position y-position size indentation)
  :x-position x-position :y-position y-position :size size :indentation indentation)

(defun invoke-indenting-output (stream continuation indentation &key (move-cursor t))
  (let* ((indentation (process-spacing-arg stream indentation
                                           'indenting-output))
         (indenting-record
           (with-output-recording-options (stream :draw nil :record t)
             (with-new-output-record (stream 'indenting-output-record nil
                                      :indentation indentation)
               (with-new-output-record (stream)
                 (funcall continuation stream))))))
    (multiple-value-bind (x y) (output-record-start-cursor-position indenting-record)
      (output-record-set-start-cursor-position indenting-record (+ x indentation) y))
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
