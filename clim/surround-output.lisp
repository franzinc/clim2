;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: surround-output.lisp,v 1.3 92/01/31 14:58:50 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;; --- To do:
;;; --- 1) Ensure that the replay method always gets called at the right time.
;;; --- 2) Use the size of the border to determine the size of the output record.
(defvar *border-shape-drawer-alist* nil)

(eval-when (eval compile load)

(defparameter *border-shape-drawer-arglist* '(stream record left top right bottom))

)

;;--- The arglist should allow room for drawing options
(defmacro define-border-type (shape arglist &body body)
  (assert (symbolp shape))
  (let ((name (fintern "~A-~A" shape 'border-drawer)))
    (multiple-value-bind (arglist ignores)
	(canonicalize-and-match-lambda-lists *border-shape-drawer-arglist* arglist)
      `(progn
	 (defun ,name ,arglist 
	   ,@(and ignores `((declare (ignore ,@ignores))))
	   (with-identity-transformation (,(first arglist))
	     ,@body))
	 (let ((old (assoc ',shape *border-shape-drawer-alist*)))
	   (if old
	       (setf (second old) ',name)
	       (setq *border-shape-drawer-alist*
		     (nconc *border-shape-drawer-alist* (list (list ',shape ',name))))))))))

(defclass border-output-record 
	  (standard-sequence-output-record)
    ((shape :initarg :shape)))

(define-output-record-constructor border-output-record
				  (&key x-position y-position (size 5) shape)
  :x-position x-position :y-position y-position :size size :shape shape)


(define-border-type :rectangle (stream left top right bottom)
  (let ((offset 2))
    (draw-rectangle* stream
		     (- left offset) (- top offset)
		     (+ right offset) (+ bottom offset)
		     :filled nil)))

(define-border-type :oval (stream left top right bottom)
  (let ((offset 2))
    (draw-oval* stream
		(floor (+ left right) 2) (floor (+ top bottom) 2)
		(+ (floor (+ (- right left) offset) 2) 2) (floor (+ (- bottom top) offset) 2)
		:filled nil)))

(defvar +drop-shadow-line-style+ (make-line-style :thickness 3 :joint-shape :miter))
(define-border-type :drop-shadow (stream left top right bottom)
  (let* ((offset 2)
	 (x1 (- left offset))
	 (y1 (- top offset))
	 (x2 (+ right offset))
	 (y2 (+ bottom offset)))
    (draw-line* stream x1 y1 x2 y1)
    (draw-line* stream x1 y1 x1 y2)
    (draw-polygon* stream (list x2 y1 x2 y2 x1 y2)
		   :closed nil :filled nil :line-style +drop-shadow-line-style+)))

(define-border-type :underline (stream record left top right)
  (let ((baseline (find-text-baseline record stream)))
    (incf top baseline)
    (draw-line* stream left top right top)))


;; SURROUNDING-OUTPUT-WITH-BORDER macro in FORMATTED-OUTPUT-DEFS
(defun invoke-surrounding-output-with-border (stream continuation shape &key (move-cursor t))
  (let* ((body nil)				;the record containing the body
	 (border-record				;the entire bordered output record
	   (with-output-recording-options (stream :draw nil :record t)
	     (with-new-output-record (stream 'border-output-record nil
				      :shape shape)
	       (setq body (with-new-output-record (stream)
			    (funcall continuation stream)))))))
    (with-bounding-rectangle* (left top right bottom) body
      (multiple-value-bind (xoff yoff)
	  (convert-from-relative-to-absolute-coordinates
	    stream (output-record-parent (output-record-parent body)))
	(translate-fixnum-positions xoff yoff left top right bottom))
      (with-output-recording-options (stream :draw nil :record t)
	(with-new-output-record (stream 'standard-sequence-output-record nil
				 :parent border-record)
	  (if (funcallable-p shape)
	      (funcall shape stream body left top right bottom)
	    (let ((function (second (assoc shape *border-shape-drawer-alist*))))
	      (if (null function)
		  (error "The shape ~S is not a defined border shape" shape)
		  (funcall function stream body left top right bottom)))))))
    (replay border-record stream)
    (when move-cursor
      (move-cursor-beyond-output-record stream border-record))
    border-record))

(defmethod map-over-table-elements-helper (function (record border-output-record) type)
  (declare (dynamic-extent function))
  ;; A BORDER-OUTPUT-RECORD contains just two records, one that records the
  ;; body and one that records the border.  Just map over the body.
  (with-slots (elements fill-pointer) record
    (when (and (arrayp elements)
	       (= fill-pointer 2))
      (map-over-table-elements function (svref elements 0) type))))
