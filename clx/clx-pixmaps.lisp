;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clx-clim)

;;;"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defclass clx-pixmap (pixmap)
    ((pixmap :initarg :pixmap)
     (for-medium :initarg :for-medium)))

(defmethod port-allocate-pixmap ((port clx-port) medium width height)
  (fix-coordinates width height)
  (let ((pixmap (xlib:create-pixmap :drawable (medium-drawable medium)
				    :width width :height height
				    :depth (xlib:drawable-depth (medium-drawable medium)))))
    (make-instance 'clx-pixmap 
      :pixmap pixmap
      :for-medium medium)))

(defmethod port-deallocate-pixmap ((port clx-port) (pixmap clx-pixmap))
  (with-slots (pixmap) pixmap
    (xlib:free-pixmap pixmap)
    (setq pixmap nil)))

(defmethod pixmap-width ((pixmap clx-pixmap))
  (xlib:drawable-width (slot-value pixmap 'pixmap)))

(defmethod pixmap-height ((pixmap clx-pixmap))
  (xlib:drawable-height (slot-value pixmap 'pixmap)))

(defmethod pixmap-depth ((pixmap clx-pixmap))
  (xlib:drawable-depth (slot-value pixmap 'pixmap)))

(defmethod port ((pixmap clx-pixmap))
  (port (slot-value pixmap 'for-medium)))


(defclass clx-pixmap-medium (clx-medium basic-pixmap-medium) ())

(defmethod make-pixmap-medium ((port clx-port) sheet &key width height)
  (let* ((pixmap (with-sheet-medium (medium sheet)
		   (port-allocate-pixmap port medium width height)))
	 (medium (make-instance 'clx-pixmap-medium
		   :port port
		   :sheet sheet
		   :pixmap pixmap)))
    (setf (slot-value medium 'drawable) (slot-value pixmap 'pixmap))
    medium))

(defmethod medium-copy-area
	   ((from-medium clx-medium) from-x from-y width height
	    (to-medium clx-medium) to-x to-y)
  (cond ((eq from-medium to-medium)
	 (let ((transform (sheet-device-transformation (medium-sheet from-medium))))
	   (convert-to-device-coordinates transform from-x from-y to-x to-y)
	   (convert-to-device-distances transform width height)
	   (let ((drawable (medium-drawable from-medium))
		 (copy-gc (slot-value from-medium 'copy-gcontext)))
	     (xlib:copy-area drawable copy-gc from-x from-y width height
			     drawable to-x to-y))))
	(t
	 (let* ((from-drawable (medium-drawable from-medium))
		(to-drawable (medium-drawable to-medium))
		(from-transform
		  (sheet-device-transformation (medium-sheet from-medium)))
		(to-transform
		  (sheet-device-transformation (medium-sheet to-medium))))
	   (convert-to-device-coordinates from-transform from-x from-y)
	   (convert-to-device-coordinates to-transform to-x to-y)
	   (convert-to-device-distances from-transform width height)
	   (xlib:copy-area from-drawable (slot-value from-medium 'copy-gcontext)
			   from-x from-y width height 
			   to-drawable to-x to-y)))))

(defmethod medium-copy-area 
	   ((from-medium clx-medium) from-x from-y width height
	    (to-medium clx-pixmap-medium) to-x to-y)
  (let ((transform (sheet-device-transformation (medium-sheet from-medium))))
    (convert-to-device-coordinates transform from-x from-y)
    (convert-to-device-distances transform width height)
    (let ((window (medium-drawable from-medium))
	  (pixmap (medium-drawable to-medium)))
      (xlib:copy-area window (slot-value from-medium 'copy-gcontext)
		      from-x from-y width height 
		      pixmap to-x to-y))))

(defmethod medium-copy-area 
	   ((from-medium clx-pixmap-medium) from-x from-y width height
	    (to-medium clx-medium) to-x to-y)
  (let ((transform (sheet-device-transformation (medium-sheet to-medium))))
    (convert-to-device-coordinates transform to-x to-y)
    (let ((window (medium-drawable to-medium))
	  (pixmap (medium-drawable from-medium)))
      (xlib:copy-area pixmap (slot-value to-medium 'copy-gcontext)
		      from-x from-y width height 
		      window to-x to-y))))

(defmethod medium-copy-area 
	   ((from-medium clx-medium) from-x from-y width height
	    (pixmap clx-pixmap) to-x to-y)
  (let ((transform (sheet-device-transformation (medium-sheet from-medium))))
    (convert-to-device-coordinates transform from-x from-y)
    (convert-to-device-distances transform width height)
    (let ((window (medium-drawable from-medium))
	  (pixmap (slot-value pixmap 'pixmap)))
      (xlib:copy-area window (slot-value from-medium 'copy-gcontext)
		      from-x from-y width height 
		      pixmap to-x to-y))))

(defmethod medium-copy-area 
	   ((pixmap clx-pixmap) from-x from-y width height
	    (to-medium clx-medium) to-x to-y)
  (let ((transform (sheet-device-transformation (medium-sheet to-medium))))
    (convert-to-device-coordinates transform to-x to-y)
    (let ((window (medium-drawable to-medium))
	  (pixmap (slot-value pixmap 'pixmap)))
      (xlib:copy-area pixmap (slot-value to-medium 'copy-gcontext)
		      from-x from-y width height 
		      window to-x to-y))))
