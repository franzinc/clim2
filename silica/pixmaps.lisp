;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

(in-package :silica)

;;; $fiHeader$

"Copyright (c) 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;;; Pixmap mediums

(defclass basic-pixmap-medium (basic-medium) 
    ((pixmap :initarg :pixmap)))


(defmethod copy-area ((medium basic-medium) from-x from-y width height to-x to-y)
  (medium-copy-area medium from-x from-y width height
		    medium to-x to-y))

;;--- Need encapsulating stream method, too
(defmethod copy-area ((sheet sheet) from-x from-y width height to-x to-y)
  (with-sheet-medium (medium sheet)
    (medium-copy-area medium from-x from-y width height
		      medium to-x to-y)))

(defun copy-from-pixmap (pixmap pixmap-x pixmap-y width height
			 medium medium-x medium-y)
  (medium-copy-area pixmap pixmap-x pixmap-y width height
		    medium medium-x medium-y))

(defun copy-to-pixmap (medium medium-x medium-y width height
		       &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (unless pixmap
    (setf pixmap (allocate-pixmap medium width height)))
  (medium-copy-area medium medium-x medium-y width height
		    pixmap pixmap-x pixmap-y)
  pixmap)


;;; Pixmap sheets

(defclass pixmap-sheet
	  (mirrored-sheet-mixin
	   sheet-permanently-enabled-mixin
	   permanent-medium-sheet-output-mixin
	   sheet-transformation-mixin
	   sheet)
    ())

(defmethod initialize-instance :after ((sheet pixmap-sheet)
				       &key port medium width height)
  ;; The medium must be a pixmap medium...
  (check-type medium basic-pixmap-medium)
  (setf (sheet-direct-mirror sheet) (medium-drawable medium)
	(port sheet) port
	(sheet-transformation sheet) +identity-transformation+
	(sheet-region sheet) (make-bounding-rectangle 0 0 width height)
	;;--- What about text style?
	(medium-foreground sheet) (or (medium-foreground medium) +black+)
	(medium-background sheet) (or (medium-background medium) +white+)))

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

(defmethod invoke-with-output-to-pixmap ((sheet sheet) continuation 
					 &key width height)
  (let* ((pixmap-medium (make-pixmap-medium (port sheet) sheet
					    :width width :height height))
	 (pixmap-sheet (make-instance 'clim-internals::pixmap-stream 
			 :port (port sheet)
			 :medium pixmap-medium
			 :width width :height height)))
    (funcall continuation pixmap-sheet)
    (slot-value pixmap-medium 'pixmap)))

(defun allocate-pixmap (medium width height)
  (port-allocate-pixmap (port medium) medium width height))

(defun deallocate-pixmap (pixmap)
  (port-deallocate-pixmap (port pixmap) pixmap))

