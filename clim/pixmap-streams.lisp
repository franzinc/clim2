;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package :clim-internals)

;; $fiHeader: pixmap-streams.lisp,v 1.7 92/07/01 15:46:46 cer Exp $

"Copyright (c) 1992 Franz, Inc.  All rights reserved."
"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;;; CLIM pixmap streams

(defclass pixmap-stream (output-recording-mixin
			 input-protocol-mixin
			 output-protocol-mixin
			 pixmap-sheet)
    ()
  (:default-initargs :text-cursor nil))

;; We can be a bit smarter than the usual method, in that if the width
;; and height are not supplied, we can deduce them.  Also, this will
;; support text output, formatted output, etc.
(defmethod invoke-with-output-to-pixmap ((stream output-protocol-mixin) continuation
					 &key width height)
  (let ((record
	  (with-output-to-output-record (stream)
	    (funcall continuation stream))))
    (unless (and width height)
      (output-record-set-position record 0 0)
      (multiple-value-setq (width height) (bounding-rectangle-size record)))
    (let* ((pixmap-medium (make-pixmap-medium (port stream) stream
					      :width width :height height))
	   (pixmap-stream (make-instance 'pixmap-stream 
			    :default-text-margin width
			    :port (port stream)
			    :medium pixmap-medium
			    :width width :height height)))
      (replay record pixmap-stream)
      (slot-value pixmap-medium 'silica::pixmap))))
