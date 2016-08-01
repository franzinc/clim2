;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(defmacro default-output-stream (stream &optional must-be-variable-macro-name)
  `(cond ((member ,stream '(t nil))
          (setq ,stream '*standard-output*))
         ,@(when must-be-variable-macro-name
             `(((not (and (symbolp ,stream)
                          (not (keywordp ,stream))))
                (warn "The stream argument to ~S, ~S, is invalid.~@
                       This argument must be a variable that can be bound to a new stream."
                      ',must-be-variable-macro-name ,stream)
                (setq ,stream '*standard-output*))))))

(defmacro default-input-stream (stream &optional must-be-variable-macro-name)
  `(cond ((member ,stream '(t nil))
          (setq ,stream '*standard-input*))
         ,@(when must-be-variable-macro-name
             `(((not (and (symbolp ,stream)
                          (not (keywordp ,stream))))
                (warn "The stream argument to ~S, ~S, is invalid.~@
                       This argument must be a variable that can be bound to a new stream."
                      ',must-be-variable-macro-name ,stream)
                (setq ,stream '*standard-input*))))))

(defmacro default-query-stream (stream &optional must-be-variable-macro-name)
  `(cond ((member ,stream '(t nil))
          (setq ,stream '*query-io*))
         ,@(when must-be-variable-macro-name
             `(((not (and (symbolp ,stream)
                          (not (keywordp ,stream))))
                (warn "The stream argument to ~S, ~S, is invalid.~@
                       This argument must be a variable that can be bound to a new stream."
                      ',must-be-variable-macro-name ,stream)
                (setq ,stream '*query-io*))))))


;;; Drawing state macros

(defmacro with-clipping-region ((stream region) &body body)
  (default-output-stream stream with-clipping-region)
  `(flet ((with-clipping-region-body (,stream) ,@body))
     (declare (dynamic-extent #'with-clipping-region-body))
     (invoke-with-clipping-region ,stream #'with-clipping-region-body ,region)))

(defmacro with-drawing-options ((medium &rest options) &body body)
  (declare (arglist (medium
                     &key ink clipping-region transformation
                          line-style line-unit line-thickness line-dashes
                          line-joint-shape line-cap-shape
                          text-style text-family text-face text-size)))
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream medium)
  `(flet ((with-drawing-options-body () ,@body))
     (declare (dynamic-extent #'with-drawing-options-body))
     (invoke-with-drawing-options ,medium #'with-drawing-options-body ,@options)))

(defmacro with-identity-transformation ((medium) &body body)
  `(letf-globally (((medium-transformation ,medium) +identity-transformation+))
     ,@body))

(defmacro with-translation ((medium dx dy) &body body)
  `(with-drawing-options (,medium
                          :transformation (make-translation-transformation ,dx ,dy))
     ,@body))
 
(defmacro with-scaling ((medium sx &optional (sy nil sy-p)) &body body)
  `(with-drawing-options (,medium
                          :transformation (let* ((scale-x ,sx)
                                                 (scale-y ,(if sy-p sy 'scale-x)))
                                            (make-scaling-transformation scale-x scale-y)))
     ,@body))
 
(defmacro with-rotation ((medium angle &optional (origin nil origin-p)) &body body)
  `(with-drawing-options (,medium
                          :transformation (make-rotation-transformation ,angle
                                                                        ,@(if origin-p `(,origin) nil)))
     ,@body))

;; Establish a local +Y-downward coordinate system at the current cursor position,
;; and execute the body
(defmacro with-local-coordinates ((&optional stream x y) &body body)
  (default-output-stream stream with-local-coordinates)
  (let ((cx '#:cx) (cy '#:cy)
        (tx '#:tx) (ty '#:ty))
    `(let ((,cx ,x)
           (,cy ,y))
       (unless (and ,cx ,cy)
         (multiple-value-setq (,cx ,cy) (stream-cursor-position ,stream)))
       (multiple-value-bind (,tx ,ty)
           (transform-position (medium-transformation ,stream) 0 0)
         (with-drawing-options
             (,stream :transformation (make-translation-transformation
                                        (- ,cx ,tx) (- ,cy ,ty)))
           ,@body)))))

;; Establish a local +Y-upward coordinate system at the current cursor position,
;; and execute the body
(defmacro with-first-quadrant-coordinates ((&optional stream x y) &body body)
  (default-output-stream stream with-first-quadrant-coordinates)
  (let ((cx '#:cx) (cy '#:cy)
        (tx '#:tx) (ty '#:ty))
    `(let ((,cx ,x)
           (,cy ,y))
       (unless (and ,cx ,cy)
         (multiple-value-setq (,cx ,cy) (stream-cursor-position ,stream)))
       (multiple-value-bind (,tx ,ty)
           (transform-position (medium-transformation ,stream) 0 0)
         (with-drawing-options
             ;; Don't flip the stream over if we already have
             (,stream :transformation (if (silica:medium-+y-upward-p ,stream)
                                          +identity-transformation+
                                          (make-transformation 1 0 0 -1
                                                               (- ,cx ,tx) (- ,cy ,ty))))
           (letf-globally (((silica:medium-+y-upward-p ,stream) t))
             ,@body))))))
