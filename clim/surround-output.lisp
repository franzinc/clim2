;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(defvar *border-shape-drawer-alist* nil)

(eval-when (eval compile load)

(defparameter *border-shape-drawer-arglist* '(stream record left top right bottom))

)

(defmacro define-border-type (shape arglist &body body)
  (assert (symbolp shape))
  (let ((name (fintern "~A-~A" shape 'border-drawer)))
    (multiple-value-bind (arglist ignores)
        (canonicalize-and-match-lambda-lists *border-shape-drawer-arglist* arglist t)
      (multiple-value-bind (doc declarations body)
          (extract-declarations body)
        (declare (ignore doc))
        `(define-group ,name define-border-type
           (defun ,name ,arglist 
             ,@(and ignores `((declare (ignore ,@ignores))))
             ,@declarations
             #+Genera (declare (sys:function-parent ,shape define-border-type))
             (with-identity-transformation (,(first arglist))
               ,@body))
           (let ((old (assoc ',shape *border-shape-drawer-alist*)))
             (if old
                 (setf (second old) ',name)
                 (setq *border-shape-drawer-alist*
                       (nconc *border-shape-drawer-alist* (list (list ',shape ',name)))))))))))

(defclass border-output-record 
          (standard-sequence-output-record)
    ((shape :initarg :shape)))

(define-output-record-constructor border-output-record
                                  (&key x-position y-position (size 5) shape)
  :x-position x-position :y-position y-position :size size :shape shape)


(define-border-type :rectangle (stream left top right bottom
				       &key (filled nil) 
				       &allow-other-keys)
  (let ((line-style (medium-line-style stream)))
    (unless filled 
      (with-half-thickness (lthickness rthickness) line-style
			   (setq left (- left rthickness)
				 top (- top rthickness)
				 right (+ right lthickness)
				 bottom (+ bottom lthickness))))
    (let ((offset (if filled 2 1)))
      (draw-rectangle* stream
		       (- left offset) (- top offset)
		       (+ right offset #+(or aclpc acl86win32) 1)
		       (+ bottom offset #+(or aclpc acl86win32) 1)
		       :filled filled))
    ;; Y offset for text cursor is 3 if filled else 2 + line thickness
    (if filled 3 (+ 2 (line-style-thickness line-style)))))

(define-border-type :oval (stream left top right bottom
				  &key (filled nil) &allow-other-keys)
  (let* ((offset 2)
         (center-x  (floor (+ left right) 2))
         (center-y (floor (+ top bottom) 2))
         (radius-x (+ (floor (+ (- right left) offset) 2) 2))
         (radius-y (floor (+ (- bottom top) offset) 2)))
    (cond ((> radius-x radius-y)
	   (incf radius-x radius-y))
	  ((> radius-y radius-x)
	   (incf radius-y radius-x)))
    (draw-oval* stream center-x center-y radius-x radius-y :filled filled))
  ;; Y offset for text cursor is 3
  3)

(defvar +drop-shadow-line-style+ (make-line-style :thickness 3 :joint-shape :miter))
(define-border-type :drop-shadow (stream left top right bottom)
  (let* ((offset 2)
	 (x1 (- left offset))
	 (y1 (- top offset))
	 (x2 (+ right offset))
	 (y2 (+ bottom offset)))
    (draw-line* stream x1 y1 x2 y1)
    (draw-line* stream x1 y1 x1 y2)
    (draw-line* stream x1 y2 left y2)
    (draw-line* stream x2 y1 x2 top)
    (draw-polygon* stream (list left (1+ y2) (1+ x2) (1+ y2) (1+ x2) top)
		   :closed nil :filled nil :line-style +drop-shadow-line-style+))
  ;; Y offset for text cursor is 4
  4)

(define-border-type :underline (stream record left top right #+(or aclpc acl86win32) bottom)
  (let ((baseline (find-text-baseline record stream)))
    (incf top (1+ baseline))
    (draw-line* stream
		;; can't get the baseline correctly on windows?  when we
		;; can we should remember to switch this back --tjm
		left #-(or aclpc acl86win32) top #+(or aclpc acl86win32) bottom
		right #-(or aclpc acl86win32) top #+(or aclpc acl86win32) bottom))
  ;; Y offset for text cursor is 0
  0)


;; SURROUNDING-OUTPUT-WITH-BORDER macro in FORMATTED-OUTPUT-DEFS
(defun invoke-surrounding-output-with-border (stream continuation shape
                                              &rest drawing-options
                                              &key (move-cursor t) &allow-other-keys)
  (declare (dynamic-extent drawing-options))
  (let* ((body nil)                                ;the record containing the body
         (border-record                                ;the entire bordered output record
           (with-output-recording-options (stream :draw nil :record t)
             (with-new-output-record (stream 'border-output-record nil
                                      :shape shape)
               (setq body (with-new-output-record (stream)
                            (funcall continuation stream))))))
         offset)
    (with-bounding-rectangle* (left top right bottom) body
      (multiple-value-bind (xoff yoff)
          (convert-from-relative-to-absolute-coordinates
            stream (output-record-parent (output-record-parent body)))
        (translate-coordinates xoff yoff left top right bottom))
      (with-output-recording-options (stream :draw nil :record t)
	(with-new-output-record (stream 'standard-sequence-output-record nil
				 :parent border-record)
	  (setq offset
		(if (funcallable-p shape)
		    (funcall shape stream body left top right bottom)
		    (let ((function (second (assoc shape *border-shape-drawer-alist*))))
		      (if (null function)
			  (error "The shape ~S is not a defined border shape" shape)
			  (with-keywords-removed 
			      (drawing-options drawing-options '(:move-cursor))
			    (flet ((draw-surrounding-border ()
				     (funcall function stream body left top right bottom)))
			      (declare (dynamic-extent #'draw-surrounding-border))
			      (apply #'invoke-with-drawing-options stream
				     #'draw-surrounding-border drawing-options))))))))))
    ;;--- This shocking kludge is to get the border underneath the
    ;;--- output that it is bordering.  Yuck!
    (let ((elements (slot-value border-record 'elements)))
      (rotatef (aref elements 0) (aref elements 1)))
    (replay border-record stream)
    (when move-cursor
      (move-cursor-beyond-output-record stream border-record
                                        ;; Compatibility: filter out bogus offsets
                                        (if (realp offset) offset 0)))
    border-record))

(defmethod map-over-table-elements-helper (function (record border-output-record) type)
  (declare (dynamic-extent function))
  ;; A BORDER-OUTPUT-RECORD contains just two records, one that records the
  ;; body and one that records the border.  Just map over the body.
  (with-slots (elements fill-pointer) record
    (when (and (arrayp elements)
               (= fill-pointer 2))
      ;; The second of the two records is the body
      (map-over-table-elements function (svref elements 1) type))))
