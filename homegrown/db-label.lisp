;; -*- mode: common-lisp; package: silica -*-
;;
;;				-[Thu Jul 22 17:16:08 1993 by colin]-
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $fiHeader: db-label.lisp,v 1.3 1993/07/27 01:50:31 colin Exp $

(in-package :silica)

(defclass generic-label-pane
	  (label-pane
	   space-requirement-mixin
	   leaf-pane)
    ()
  (:default-initargs :align-x :left
		     :text-style *default-label-text-style*))

(defmethod compose-space ((pane generic-label-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (width height)
      (compute-gadget-label-size pane)
    (make-space-requirement :width width :height height)))

(defmethod handle-repaint ((pane generic-label-pane) region)
  (declare (ignore region))			;not worth checking
  (with-sheet-medium (medium pane)
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (declare (ignore right bottom))
      (draw-gadget-label pane medium left top
			 :align-x (gadget-alignment pane) :align-y :top))))

(defmethod draw-gadget-label ((pane labelled-gadget-mixin) medium x y
			      &key (align-x (gadget-alignment pane))
				   (align-y :baseline))
  (let ((label (gadget-label pane)))
    (etypecase label
      (string
	(let ((text-style (slot-value pane 'text-style)))
	  (draw-text* medium label x y
		      :text-style text-style
		      :align-x align-x :align-y align-y)))
      (null)
      (pattern
	(let ((width (pattern-width label))
	      (height (pattern-height label)))
	  (ecase align-x
	    (:left)
	    (:right (decf x width))
	    (:center (decf x (floor width 2))))
	  (ecase align-y
	    ((:top :baseline))
	    (:bottom (decf x height))
	    (:center (decf x (floor height 2))))
	  (draw-pattern* medium label x y)))
      (pixmap
	(let ((width (pixmap-width label))
	      (height (pixmap-height label)))
	  (ecase align-x
	    (:left)
	    (:right (decf x width))
	    (:center (decf x (floor width 2))))
	  (ecase align-y
	    ((:top :baseline))
	    (:bottom (decf x height))
	    (:center (decf x (floor height 2))))
	  (copy-from-pixmap label 0 0 width height
			    medium x y))))))
