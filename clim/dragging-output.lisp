;; -*- mode: common-lisp; package: clim -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: dragging-output.cl,v 1.1 92/01/07 11:19:54 cer Exp Locker: cer $

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
	(parent (output-record-parent output-record))
	;; Clipping region for repainting the damaged region
	(region (bounding-rectangle output-record)))
    (multiple-value-bind (initial-x initial-y)
	(stream-pointer-position* stream)
      (declare (fixnum initial-x initial-y))
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
		   (output-record-set-position* output-record x y)
		   (when parent
		     (add-output-record output-record parent)
		     (tree-recompute-extent output-record))
		   (replay-output-record output-record stream))
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
	      (:pointer-motion (window x y)
	       (when (eql window stream)
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
		   (output-record-set-position* output-record x y)
		   ;; Replay the region it used to live in, and then replay
		   ;; the record in its new home.
		   (when repaint
		     (frame-replay *application-frame* stream region))
		   (if feedback
		       (funcall feedback output-record stream
				initial-x initial-y x y :draw)
		       (replay-output-record output-record stream)))))
	      (:pointer-button-press (x y)
	       (unless finish-on-release
		 (finish x y)))
	      (:pointer-button-release (x y)
	       (when finish-on-release
		 (finish x y)))))))))
