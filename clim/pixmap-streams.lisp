;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: pixmap-streams.lisp,v 1.12 92/08/21 16:33:58 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1992 Franz, Inc.  All rights reserved."


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
;;--- Note that, since this calls the continuation on the original stream
;;--- instead of the pixmap stream, things like WINDOW-CLEAR will affect
;;--- the wrong stream
#-Allegro	;--- SWM is willing to live with this, but not CER
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
      (setf (medium-foreground pixmap-medium) (medium-foreground stream)
	    (medium-background pixmap-medium) (medium-background stream))
      (replay record pixmap-stream)
      (slot-value pixmap-medium 'silica::pixmap))))

#+Allegro
(defmethod invoke-with-output-to-pixmap ((stream output-protocol-mixin) continuation
					 &key width height)
  (let* ((pixmap-medium (make-pixmap-medium (port stream) stream
					    :width width :height height))
	 (pixmap-stream (make-instance 'pixmap-stream 
			  :default-text-margin width
			  :port (port stream)
			  :medium pixmap-medium
			  :width width :height height)))
    (setf (medium-foreground pixmap-medium) (medium-foreground stream)
	  (medium-background pixmap-medium) (medium-background stream))
    (funcall continuation pixmap-stream)
    (slot-value pixmap-medium 'silica::pixmap)))

(defun pixmap-from-menu-item (associated-window menu-item printer presentation-type)
  (with-menu (menu associated-window)
    (setf (stream-text-margin menu) 1000)
    (let ((record (with-output-recording-options (menu :draw nil :record t)
		    (with-output-to-output-record (menu)
		      (handler-case
			(if presentation-type
			    (present menu-item presentation-type :stream menu)
			    (funcall printer menu-item menu))
			(error ()
			  (write-string "Error in printer" menu)))))))
      (multiple-value-bind (width height)
	  (bounding-rectangle-size record)
	(with-output-to-pixmap (stream associated-window :width width :height height)
	  (draw-rectangle* stream 0 0 width height
			   :ink +background-ink+ :filled t)
	  (replay-output-record
	    record stream +everywhere+
	    (- (bounding-rectangle-left record))
	    (- (bounding-rectangle-top record))))))))
