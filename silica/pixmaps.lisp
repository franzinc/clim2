;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: pixmaps.lisp,v 1.14 92/12/03 10:29:30 cer Exp $

(in-package :silica)

"Copyright (c) 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;;; Pixmaps

(defclass pixmap () ())

(defgeneric pixmap-width (pixmap))
(defgeneric pixmap-height (pixmap))


;;; Pixmap mediums

(defclass basic-pixmap-medium (basic-medium) 
    ((pixmap :initarg :pixmap)))

(defgeneric make-pixmap-medium (port sheet &key width height))

(defgeneric port-allocate-pixmap (port medium width height))
(defgeneric port-deallocate-pixmap (port pixmap))


(defgeneric medium-copy-area (from-medium from-x from-y width height
			      to-medium to-x to-y))

(defmethod copy-area ((medium basic-medium) from-x from-y width height to-x to-y)
  (medium-copy-area medium from-x from-y width height
		    medium to-x to-y))

;;--- Need encapsulating stream method, too
(defmethod copy-area ((sheet basic-sheet) from-x from-y width height to-x to-y)
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
	   basic-sheet)
    ())

(defmethod initialize-instance :after ((sheet pixmap-sheet)
				       &key port medium width height)
  ;; The medium must be a pixmap medium...
  (check-type medium basic-pixmap-medium)
  (setf (sheet-direct-mirror sheet) (medium-drawable medium)
	(port sheet) port
	(sheet-transformation sheet) +identity-transformation+
	(sheet-region sheet) (make-bounding-rectangle 0 0 width height)
	(medium-foreground sheet) (or (medium-foreground medium) +black+)
	(medium-background sheet) (or (medium-background medium) +white+)
	(medium-default-text-style sheet) (or (medium-default-text-style medium)
					      (and port (port-default-text-style port))
					      *default-text-style*)))

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

