;; -*- mode: common-lisp; package: xm-silica -*-
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
;; $fiHeader: xt-pixmaps.lisp,v 1.9 92/07/27 11:03:48 cer Exp $


(in-package :xm-silica)

(defmethod port-allocate-pixmap ((port xt-port) medium width height)
  (declare (ignore medium))
  (fix-coordinates width height)
  (let ((root (tk::display-root-window (port-display port))))
    (make-instance 'tk::pixmap
      :drawable root
      :width width
      :height height
      :depth (tk::drawable-depth root))))

(defmethod port-deallocate-pixmap ((port xt-port) (pixmap tk::pixmap))
  (break "implement me"))

(defmethod pixmap-width ((pixmap tk::pixmap))
  (tk::pixmap-width pixmap))

(defmethod pixmap-height ((pixmap tk::pixmap))
  (tk::pixmap-height pixmap))

(defmethod pixmap-depth ((pixmap tk::pixmap))
  (tk::pixmap-depth pixmap))


(defclass xt-pixmap-medium (xt-medium basic-pixmap-medium) ())

(defmethod make-pixmap-medium ((port xt-port) sheet &key width height)
  (let* ((pixmap (with-sheet-medium (medium sheet)
		   (port-allocate-pixmap port medium width height)))
	 (medium (make-instance 'xt-pixmap-medium
		   :port port
		   :sheet sheet
		   :pixmap pixmap)))
    (setf (slot-value medium 'drawable) pixmap)
    medium))

(defmethod medium-copy-area 
	   ((from-medium xt-medium) from-x from-y width height
	    (to-medium xt-medium) to-x to-y)
  (let ((from-transform (sheet-native-transformation (medium-sheet from-medium)))
	(to-transform (sheet-native-transformation (medium-sheet to-medium))))
    (convert-to-device-coordinates from-transform from-x from-y)
    (convert-to-device-coordinates to-transform to-x to-y)
    (convert-to-device-distances from-transform width height)
    (let* ((from-drawable (medium-drawable from-medium))
	   (to-drawable (medium-drawable to-medium))
	   (port (port from-medium))
	   (copy-gc (port-copy-gc port)))
	(when (and from-drawable to-drawable)
	  (when (port-safe-backing-store port)
	    ;; Don't bother with no-expose events.
	    (return-from medium-copy-area
	      (tk::copy-area from-drawable copy-gc from-x from-y
			     width height
			     to-drawable to-x to-y)))
	  (with-port-event-lock (port)
	    (let ((seq-no 0))
	      (clim-sys:without-scheduling
		(setq seq-no (x11:xnextrequest (port-display port)))
		(tk::copy-area from-drawable copy-gc from-x from-y
			       width height to-drawable to-x to-y))
	      (let ((event
		     (tk::get-event-matching-sequence-and-types
		      to-drawable seq-no
		      '(:graphics-expose :no-expose))))
		(case (tk::event-type event)
		  (:no-expose
		   nil)
		  (:graphics-expose
		   (loop
		     (let* ((minx (x11::xexposeevent-x event))
			    (miny (x11::xexposeevent-y event))
			    (width (x11::xexposeevent-width event))
			    (height (x11::xexposeevent-height event))
			    (maxx (+ minx width))
			    (maxy (+ miny height)))
		       (let ((sheet (medium-sheet to-medium)))
			 (dispatch-repaint
			   sheet
			   (allocate-event 'window-repaint-event
			     :native-region (make-bounding-rectangle minx miny maxx maxy)
			     :region (untransform-region
				       (sheet-native-transformation sheet)
				       (make-bounding-rectangle minx miny maxx maxy))
			     :sheet sheet))))
		     (setq event (tk::get-event-matching-sequence-and-types
				  to-drawable seq-no '(:graphics-expose) 
				  :block nil))
		     (unless event
		       (return))))))))))))

;;; I dont understand the need for these methods
;;; Also what is the xt-pixmap class below.

#+ignore
(defmethod medium-copy-area 
	   ((from-medium xt-medium) from-x from-y width height
	    (to-medium xt-pixmap-medium) to-x to-y)
  (let ((transform (sheet-native-transformation (medium-sheet from-medium))))
    (convert-to-device-coordinates transform from-x from-y)
    (convert-to-device-distances transform width height)
    ;; We are in a bad situation if the native-transformation is
    ;; anything other than a scaling transformation
    (let ((window (medium-drawable from-medium))
	  (copy-gc (make-instance 'tk::gcontext :drawable window))
	  (pixmap (medium-drawable to-medium)))
      (tk::copy-area 
	window copy-gc from-x from-y width height 
	pixmap to-x to-y))))

#+ignore
(defmethod medium-copy-area 
	   ((from-medium xt-pixmap-medium) from-x from-y width height
	    (to-medium xt-medium) to-x to-y)
  (let ((transform (sheet-native-transformation (medium-sheet to-medium))))
    (convert-to-device-coordinates transform to-x to-y)
    ;; We are in a bad situation if the native-transformation is
    ;; anything other than a scaling transformation
    (let ((window (medium-drawable to-medium))
	  (copy-gc (make-instance 'tk::gcontext :drawable window))
	  (pixmap (medium-drawable from-medium)))
      (tk::copy-area 
	pixmap copy-gc from-x from-y width height
	window to-x to-y))))

#+ignore
(defmethod medium-copy-area 
	   ((from-medium xt-medium) from-x from-y width height
	    (pixmap tk::pixmap) to-x to-y)
  (let ((transform (sheet-native-transformation (medium-sheet from-medium))))
    (convert-to-device-coordinates transform from-x from-y)
    (convert-to-device-distances transform width height)
    (let ((window (medium-drawable from-medium))
	  (copy-gc (make-instance 'tk::gcontext :drawable window)))
      (tk::copy-area 
	window copy-gc from-x from-y width height 
	pixmap to-x to-y))))

#+ignore
(defmethod medium-copy-area 
	   ((pixmap tk::pixmap) from-x from-y width height
	    (to-medium xt-medium) to-x to-y)
  (let ((transform (sheet-native-transformation (medium-sheet to-medium))))
    (convert-to-device-coordinates transform to-x to-y)
    (let ((window (medium-drawable to-medium))
	  (copy-gc (make-instance 'tk::gcontext :drawable window)))
      (tk::copy-area 
	pixmap copy-gc from-x from-y width height
	window to-x to-y))))
