;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: dragging-output.lisp,v 1.8 91/08/05 14:29:22 cer Exp $

(in-package :clim)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
Copyright (c) 1991, Franz Inc. All rights reserved
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; Yay!
(defmacro dragging-output ((&optional stream (repaint t) (finish-on-release nil))
			   &body body)
  (default-output-stream stream dragging-output)
  (let ((output-record '#:output-record))
    `(let ((,output-record
	    (with-output-to-output-record (,stream) ,@body)))
       (dragging-output-record ,stream ,output-record
			       :repaint ,repaint
			       :finish-on-release ,finish-on-release))))

(defun dragging-output-record (stream output-record
			       &key (repaint t) (erase #'erase-output-record) feedback
				    (finish-on-release nil))
  (let (last-x last-y
	(delta-x 0)
	(delta-y 0)
	(parent (output-record-parent output-record))
	;; Clipping region for repainting the damaged region
	(region (bounding-rectangle output-record)))
    (multiple-value-bind (initial-x initial-y)
	(stream-pointer-position* stream)
      (declare (fixnum initial-x initial-y))
      (multiple-value-bind (x-offset y-offset)
	  (convert-from-relative-to-absolute-coordinates
	    stream (output-record-parent output-record))
	(declare (fixnum x-offset y-offset))
	(with-bounding-rectangle* (record-x record-y) output-record
	  ;; Deltas are the position of the mouse in the local coordinate
	  ;; system of the record
	  (setq delta-x (the fixnum (- initial-x record-x))
		delta-y (the fixnum (- initial-y record-y))))
	(flet ((finish (x y)
		 (when last-x
		   (if feedback
		       (funcall feedback output-record stream
				initial-x initial-y last-x last-y :erase)
		       (funcall erase output-record stream))
		   ;; Note the asymmetry in the we call the feedback function
		   ;; with LAST-X/Y, but set the position to X/Y.  This is 
		   ;; because the user can be moving the pointer really fast,
		   ;; causing FINISH to be called on coordinates which are
		   ;; "before" the values of LAST-X/Y.
		   (output-record-set-position* output-record (- x delta-x) (- y delta-y))
		   (when parent
		     (add-output-record-element parent output-record)
		     (tree-recompute-extent output-record))
		   (replay-1 output-record stream nil x-offset y-offset))
		 (return-from dragging-output-record
		   (values x y))))
	  (declare (dynamic-extent #'finish))
	  (with-output-recording-options (stream :record-p nil :draw-p t)
	    (when feedback
	      (funcall erase output-record stream)
	      (funcall feedback output-record stream
		       initial-x initial-y initial-x initial-y :draw)
	      (setq last-x initial-x last-y initial-y))
	    (tracking-pointer (stream)
	      (:pointer-motion (#-Silica window #+Silica sheet x y)
	       (when (eql #-Silica window #+Silica sheet stream)
		 (when (or (not (eql x last-x))
			   (not (eql y last-y)))
		   (when last-x
		     (if feedback
			 (funcall feedback output-record stream
				  initial-x initial-y last-x last-y :erase)
		         (funcall erase output-record stream)))
		   (setq last-x x last-y y)
		   ;; Remember the space the record used to occupy
		   (setq region (bounding-rectangle output-record region))
		   ;; Move the record
		   (output-record-set-position* output-record (- x delta-x) (- y delta-y))
		   ;; Replay the region it used to live in, and then replay
		   ;; the record in its new home.
		   (when repaint
		     (frame-replay *application-frame* stream region))
		   (if feedback
		       (funcall feedback output-record stream
				initial-x initial-y x y :draw)
		       (replay-1 output-record stream nil x-offset y-offset)))))
	      #-Silica
	      (:pointer-button-press (x y)
	       (unless finish-on-release
		 (finish x y)))
	      #-Silica
	      (:pointer-button-release (x y)
	       (when finish-on-release
		 (finish x y)))
	      #+Silica
	      (:button-press (x y)
	       (finish x y)))))))))
