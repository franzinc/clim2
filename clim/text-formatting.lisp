;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: text-formatting.lisp,v 1.20 1999/02/25 08:23:30 layer Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."

;;; Filling output

(defclass filling-stream (standard-encapsulating-stream)
    (fill-width
     break-characters
     prefix
     prefix-width
     current-width
     (buffer :initarg :buffer)))

(defmethod filling-stream-write-buffer ((filling-stream filling-stream) &optional force-p)
  (with-slots (current-width) filling-stream
    (let ((stream (slot-value filling-stream 'stream))
          (buffer (slot-value filling-stream 'buffer))
          (fill-width (slot-value filling-stream 'fill-width))
          (break-characters (slot-value filling-stream 'break-characters))
          (fresh-line nil))
      (loop
        (when (zerop (fill-pointer buffer))        ;Buffer empty
          (return-from filling-stream-write-buffer nil))
        (unless (> current-width fill-width)        ;Haven't reached the fill column
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
                              (position-if-not #'whitespace-char-p buffer
                                               :start index))))
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
  (with-slots (current-width) filling-stream
    (let ((stream (slot-value filling-stream 'stream))
          (buffer (slot-value filling-stream 'buffer))
          (prefix (slot-value filling-stream 'prefix))
          (prefix-width (slot-value filling-stream 'prefix-width)))
      ;; The "after line break" stuff is not a candidate for filling,
      ;; but it does contribute to the width of the line
      (when prefix
        (stream-write-string stream prefix))
      (setq current-width (+ prefix-width (text-size stream buffer))))))

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
    (let ((stream (slot-value filling-stream 'stream))
          (fill-width (slot-value filling-stream 'fill-width))
          (buffer (slot-value filling-stream 'buffer)))
      (cond ((or (eql char #\Newline)
                 (eql char #\Return))
             (stream-terpri filling-stream))
            (t
             (vector-push-extend char buffer)
             (incf current-width (text-size stream char))
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

;; Need to draw the currently buffered text onto the real stream and start
;; buffering again, but remember the current cursor position instead of starting
;; at the beginning of the line.  This is the way we get presentations to be in
;; the right place, for example.

(defmethod write-buffer-and-continue ((filling-stream filling-stream)
                                      continuation &rest continuation-args)
  (declare (dynamic-extent continuation continuation-args))
  (let ((stream (slot-value filling-stream 'stream))
	#+(or aclpc acl86win32)
        (fill-width (slot-value filling-stream 'fill-width))
	#+(or aclpc acl86win32)
        (buffer (slot-value filling-stream 'buffer)))
    ;; Flush the current line buffer
    #-(or aclpc acl86win32)
    (filling-stream-write-buffer filling-stream t)
    #+(or aclpc acl86win32)
    (progn
      (stream-write-string stream buffer)
      (setf (fill-pointer buffer) 0)
      ;; Move to the next line if necessary, writing the prefix string
      ;;--- STREAM-CURSOR-POSITION is the wrong thing to look at
      (when (> (stream-cursor-position stream) fill-width)
	(stream-terpri stream)
	(filling-stream-handle-line-break filling-stream)))
    (let ((*original-stream* (encapsulating-stream filling-stream)))
      (apply continuation stream continuation-args))))

(defmethod invoke-with-text-style ((filling-stream filling-stream)
                                   continuation style original-stream)
  (labels ((filling-continuation (stream)
             (multiple-value-prog1
               (funcall continuation stream)
               (stream-close-text-output-record stream))))
    (declare (dynamic-extent #'filling-continuation))
    (write-buffer-and-continue filling-stream
                               #'invoke-with-text-style
                               #'filling-continuation style original-stream)))

(defmethod invoke-with-output-recording-options ((filling-stream filling-stream)
                                                 continuation record draw)
  (write-buffer-and-continue filling-stream
                             #'invoke-with-output-recording-options
                             continuation record draw))

(defmethod stream-close-text-output-record ((filling-stream filling-stream)
                                            &optional wrapped)
  (write-buffer-and-continue filling-stream
                             #'stream-close-text-output-record wrapped))

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
        (stream-cursor-position stream))))

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
