;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :silica)

;;;"Copyright (c) 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;;; Pixmaps

(defclass pixmap () 
  ((port :reader pixmap-port :initarg :port)))

(defgeneric pixmap-width (pixmap))
(defgeneric pixmap-height (pixmap))

(defmethod port ((pixmap pixmap))
  (pixmap-port pixmap))

;;; Pixmap mediums

(defclass basic-pixmap-medium (basic-medium) 
    ((pixmap :initarg :pixmap)))

(defgeneric make-pixmap-medium (port sheet &key width height))

(defgeneric port-allocate-pixmap (port medium width height))
(defgeneric port-deallocate-pixmap (port pixmap))


(defgeneric medium-copy-area (from-medium from-x from-y width height
                              to-medium to-x to-y
                              &optional function))

(defmethod copy-area ((medium basic-medium) from-x from-y width height to-x to-y
                      &optional (function boole-1))
  (medium-copy-area medium from-x from-y width height
                    medium to-x to-y
                    function))

;;--- Need encapsulating stream method, too
(defmethod copy-area ((sheet basic-sheet) from-x from-y width height to-x to-y
                      &optional (function boole-1))
  (with-sheet-medium (medium sheet)
    (medium-copy-area medium from-x from-y width height
                      medium to-x to-y
                      function)))

(defmethod copy-from-pixmap (pixmap pixmap-x pixmap-y width height
                             (medium basic-medium) medium-x medium-y &optional (function boole-1))
  (medium-copy-area pixmap pixmap-x pixmap-y width height
                    medium medium-x medium-y function))

(defmethod copy-from-pixmap (pixmap pixmap-x pixmap-y width height
                             (sheet basic-sheet) medium-x medium-y &optional (function boole-1))
  (with-sheet-medium (medium sheet)
    (medium-copy-area pixmap pixmap-x pixmap-y width height
                      medium medium-x medium-y function)))

(defmethod copy-to-pixmap ((medium basic-medium) medium-x medium-y width height
                       &optional pixmap (pixmap-x 0) (pixmap-y 0) (function boole-1))
  (unless pixmap
    (setf pixmap (allocate-pixmap medium width height)))
  (medium-copy-area medium medium-x medium-y width height
                    pixmap pixmap-x pixmap-y function)
  pixmap)

(defmethod copy-to-pixmap ((sheet basic-sheet) medium-x medium-y width height
                       &optional pixmap (pixmap-x 0) (pixmap-y 0) (function boole-1))
  (with-sheet-medium (medium sheet)
    (unless pixmap
      (setf pixmap (allocate-pixmap medium width height)))
    (medium-copy-area medium medium-x medium-y width height
                      pixmap pixmap-x pixmap-y function)
    pixmap))


;;; Pixmap sheets

(defclass pixmap-sheet
          (mirrored-sheet-mixin
           sheet-permanently-enabled-mixin
           permanent-medium-sheet-output-mixin
           sheet-transformation-mixin
           basic-sheet)
    ())


(defmethod initialize-instance :after ((sheet pixmap-sheet)
                                       &key port medium width height)
  ;; The medium must be a pixmap medium...
  (check-type medium basic-pixmap-medium)
  (setf (sheet-direct-mirror sheet) (medium-drawable medium)
        (slot-value sheet 'port) port
        (medium-sheet medium) sheet
        (sheet-medium sheet) medium
        (sheet-transformation sheet) +identity-transformation+
        (sheet-region sheet) (make-bounding-rectangle 0 0 width height)))

(defmethod handle-repaint ((pane pixmap-sheet) region)
  (declare (ignore region))
  nil)

(defmethod realize-mirror ((port basic-port) (sheet pixmap-sheet))
  nil)

(defmethod update-mirror-transformation ((port basic-port) (sheet pixmap-sheet))
  nil)

(defmethod update-mirror-region ((port basic-port) (sheet pixmap-sheet))
  nil)

(defmethod fetch-medium-drawable ((sheet pixmap-sheet) pixmap)
  pixmap)


;;; Interface to pixmaps

(defmacro with-output-to-pixmap ((medium-var medium &key width height)
                                 &body body)
  (default-output-stream medium-var with-output-to-pixmap)
  `(flet ((with-output-to-pixmap-body (,medium-var) ,@body))
     (declare (dynamic-extent #'with-output-to-pixmap-body))
     (invoke-with-output-to-pixmap ,medium #'with-output-to-pixmap-body
                                   :width ,width :height ,height)))

(defmethod invoke-with-output-to-pixmap ((medium basic-medium) continuation 
                                         &key width height)
  (invoke-with-output-to-pixmap (medium-sheet medium) continuation
                                :width width :height height))

(defmethod invoke-with-output-to-pixmap ((sheet basic-sheet) continuation 
                                         &key width height)
  (let* ((pixmap-medium (make-pixmap-medium (port sheet) sheet
                                            :width width :height height))
         (pixmap-sheet (make-instance 'pixmap-sheet 
                         :port (port sheet)
                         :medium pixmap-medium
                         :width width :height height)))
    ;;--- Is this a waste of time if this does not have a medium?
    (when (typep sheet 'sheet-with-medium-mixin)
      (with-sheet-medium (medium sheet)
        (setf (medium-foreground pixmap-medium) (medium-foreground medium)
              (medium-background pixmap-medium) (medium-background medium))))
    (funcall continuation pixmap-sheet)
    (slot-value pixmap-medium 'pixmap)))

(defmethod invoke-with-output-to-pixmap ((stream standard-encapsulating-stream) continuation 
                                         &key width height)
  (invoke-with-output-to-pixmap (encapsulating-stream-stream stream) continuation
                                :width width :height height))

(defun allocate-pixmap (medium width height)
  (port-allocate-pixmap (port medium) medium width height))

(defun deallocate-pixmap (pixmap)
  (port-deallocate-pixmap (port pixmap) pixmap))

