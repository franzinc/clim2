;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved.
 Portions copyright (c) 1991, 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)

;; $fiHeader: db-border.lisp,v 1.10 92/08/18 17:23:25 cer Exp Locker: cer $

;;; Border Panes

(defclass border-pane (layout-pane)
    ((thickness :initform 1 :initarg :thickness)))

(defmethod initialize-instance :after ((pane border-pane) &key contents)
  (sheet-adopt-child pane contents))

(defmethod handle-event :after ((pane border-pane) (event pointer-motion-event))
  (deallocate-event event))

(defmethod compose-space ((pane border-pane) &key width height)
  (let ((thickness (slot-value pane 'thickness))
	(child (sheet-child pane)))
    (space-requirement+
      (compose-space child :width width :height height)
      (make-space-requirement 
	:width (* 2 thickness)
	:height (* 2 thickness)))))

(defmethod allocate-space ((pane border-pane) width height)
  (let ((thickness (slot-value pane 'thickness)))
    (move-and-resize-sheet
      (sheet-child pane)
      thickness thickness
      (- width (* 2 thickness)) (- height (* 2 thickness)))))
  
(defmethod repaint-sheet ((pane border-pane) region)
  (declare (ignore region))			;not worth checking
  (with-sheet-medium (medium pane)
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (let ((thickness (slot-value pane 'thickness)))
	(decf right (ceiling thickness 2))
	(decf bottom (ceiling thickness 2))
	(draw-rectangle* medium left top right bottom
			 :line-thickness thickness :filled nil
			 :ink (pane-background pane))))))

(defmacro bordering ((&rest options &key thickness &allow-other-keys)
		     &body contents)
  (declare (ignore thickness))
  `(make-pane 'border-pane
     :contents ,@contents
     ,@options))


(defclass outlined-pane (border-pane)
  ((background :initform +black+ :accessor pane-background)))

(defmacro outlining ((&rest options &key thickness &allow-other-keys)
		     &body contents)
  (declare (ignore thickness))
  `(make-pane 'outlined-pane
     :contents ,@contents
     ,@options))


(defclass spacing-pane (border-pane) ()
  (:default-initargs :thickness 2))

(defmacro spacing ((&rest options &key thickness &allow-other-keys) &body contents)
  (declare (ignore thickness))
  `(make-pane 'spacing-pane
     :contents ,@contents
     ,@options))


;;; Label panes

(defparameter *default-label-text-style* 
    ;;--- This defeats the resource mechanism
    #-ignore nil
    #+ignore (make-text-style :sans-serif :italic :small))

(defclass label-pane (foreground-background-and-text-style-mixin labelled-gadget-mixin)
    ()
  (:default-initargs :align-x :left
		     :text-style *default-label-text-style*))

(defclass generic-label-pane 
	  (label-pane
	   space-requirement-mixin
	   leaf-pane)
    ())

(defmethod compose-space ((pane generic-label-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (width height)
      (compute-gadget-label-size pane)
    (make-space-requirement :width width :height height)))
  
(defmethod repaint-sheet ((pane generic-label-pane) region)
  (declare (ignore region))			;not worth checking
  (with-sheet-medium (medium pane)
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (declare (ignore right bottom))
      (draw-gadget-label pane medium left top
			 :align-x (gadget-alignment pane) :align-y :top))))
