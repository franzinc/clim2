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
;; $fiHeader: xt-gadgets.lisp,v 1.7 92/03/04 16:20:42 cer Exp Locker: cer $

(in-package :xm-silica)

(defclass ask-widget-for-size-mixin () ())

(defmethod realize-pane-1 ((framem xt-frame-manager)
			   frame abstract-type &rest options)
  (let ((type (apply #'realize-pane-class framem abstract-type options)))
    (if type
	(apply #'make-instance type 
	       :frame frame
	       :frame-manager framem
	       (apply #'realize-pane-arglist
		      framem abstract-type options))
	(call-next-method))))

(defmethod compose-space ((pane ask-widget-for-size-mixin) &key width height)
  (multiple-value-bind
      (x y width height borderwidth
       care-x care-y care-width care-height care-borderwidth)
      (tk::widget-best-geometry (sheet-direct-mirror pane) :width
				width :height height)
    (declare (ignore x y borderwidth care-x care-y care-width
		     care-height care-borderwidth)) 
    (make-instance 'space-requirement :width width :height height)))

(defmethod realize-pane-arglist (realizer type &rest options)
  (declare (ignore realizer type))
  options)

;;; Pane

(defmethod find-widget-class-and-initargs-for-sheet :around
	   ((port xt-port) (parent t)
	    (pane pane))
  ;; Include the name of the pane
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (let ((name (pane-name pane)))
      (values class
	      (progn
		(when (and name (not (getf initargs :name)))
		  (setf (getf initargs :name) (string name)))
		initargs)))))

;; Background/foreground/text-style mixin

(defmethod find-widget-class-and-initargs-for-sheet :around
    ((port xt-port) (parent t)
     (sheet silica::foreground-background-and-text-style-mixin))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (with-slots
	(silica::foreground silica::background text-style)
	sheet
      
      ;; Background can either be a pixel of a bitmap
      ;; Foreground has to be a pixel
      ;; decode-gadget-background
      ;; decode-gadget-foreground
      
      (when silica::background
	(with-sheet-medium (medium sheet)
	  (setf initargs
	    (append (decode-gadget-background medium sheet silica::background) 
		    initargs))))
      
      (when silica::foreground
	(with-sheet-medium (medium sheet)
	  (setf initargs
	    (append (decode-gadget-foreground medium sheet silica::foreground)
		    initargs))))

      (when text-style
	(setf (getf initargs :font-list)
	  (text-style-mapping port text-style)))

      (values class initargs))))

(defmethod decode-gadget-background (medium sheet ink)
  (declare (ignore sheet))
  (let ((gc (decode-ink ink medium)))
    (if (tk::gcontext-tile gc)
	(list :background-pixmap (tk::gcontext-tile gc))
      (list :background (tk::gcontext-foreground gc)))))

(defmethod decode-gadget-foreground (medium sheet ink)
  (declare (ignore sheet))
  (let ((pixel (decode-color medium ink)))
    (list :foreground pixel)))

(defclass xt-pane (pane) ())

(defmethod allocate-space ((p xt-pane) width height)
  (when (sheet-mirror p)
    (tk::set-values (sheet-mirror p) 
		    :width (floor width)
		    :height (floor height))))

;(defclass xt-composite-pane () ())

(defclass xt-leaf-pane (sheet-permanently-enabled-mixin
			client-overridability
			xt-pane 
			mirrored-sheet-mixin 
			ask-widget-for-size-mixin)
	  ())

(defmethod sheet-shell (sheet)
  (do ((w (sheet-mirror sheet) (tk::widget-parent w)))
      ((typep w '(or tk::shell null))
       w)))
