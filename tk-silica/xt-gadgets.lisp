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
;; $fiHeader$

(in-package :xm-silica)

(defclass ask-widget-for-size-mixin () ())

(defmethod realize-pane-internal ((realizer xt-frame-manager)
				  frame
				  abstract-type
				  &rest options)
  (let ((type (apply #'realize-pane-class realizer abstract-type options)))
    (if type
	(apply #'make-instance
	       type 
	       :frame frame
	       :frame-manager realizer
	       (apply #'realize-pane-arglist
		      realizer
		      abstract-type options))
      (call-next-method))))

(defmethod compose-space ((pane ask-widget-for-size-mixin))
  (multiple-value-bind
      (x y width height borderwidth
	 care-x care-y care-width care-height care-borderwidth)
      (tk::widget-best-geometry (sheet-direct-mirror pane))
    (declare (ignore x y borderwidth care-x care-y care-width
		     care-height care-borderwidth)) 
    (make-instance 'silica::space-req :width width :height height)))

(defmethod realize-pane-arglist (realizer type &rest options)
  (declare (ignore realizer type))
  options)

;; Background/foreground/text-style mixin

(defmethod find-widget-class-and-initargs-for-sheet 
    :around
    ((port xt-port)
     (sheet 
      silica::foreground-background-and-text-style-mixin))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (with-slots
	(silica::foreground silica::background silica::text-style)
	sheet
      
      ;; Background can either be a pixel of a bitmap
      ;; Foreground has to be a pixel
      ;; clx-decode-gadget-background
      ;; clx-decode-gadget-foreground
      
      (when silica::background
	(with-sheet-medium (medium sheet)
	  (setf initargs
	    (append (clx-decode-gadget-background medium sheet silica::background) 
		    initargs))))
      
      (when silica::foreground
	(with-sheet-medium (medium sheet)
	  (setf initargs
	    (append (clx-decode-gadget-foreground medium sheet silica::foreground)
		    initargs))))

      (when silica::text-style
	(setf (getf initargs :font-list)
	  (realize-text-style port silica::text-style)))

      (values class initargs))))

(defmethod clx-decode-gadget-background (medium sheet ink)
  (declare (ignore sheet))
  (let ((gc (clx-decode-ink ink medium)))
    (if (tk::gcontext-tile gc)
	(list :background-pixmap (tk::gcontext-tile gc))
      (list :background (tk::gcontext-foreground gc)))))


(defmethod clx-decode-gadget-foreground (medium sheet ink)
  (declare (ignore sheet))
  (let ((gc (clx-decode-ink ink medium)))
    (if (tk::gcontext-tile gc)
	(error "Gadget foreground cannot be pixmap")
      (list :foreground (tk::gcontext-foreground gc)))))


;; This needs to be somewhere.
;; It looks like general CLIM code

(defmethod clim::pane-viewport-sheet (x)
  (and (typep (silica::sheet-parent x) 'silica::viewport)
       (silica::sheet-parent x)))

(defmethod clim::pane-viewport (x)
  (clim::pane-viewport-sheet x))

(defmethod clim::pane-viewport-region (x)
  (let ((vp (clim::pane-viewport-sheet x)))
    (and vp
	 (silica::xm-viewport-viewport vp))))

(defun clim::pane-scroller (x)
  (clim::pane-scroller-sheet x))

(defun clim::update-region (stream width height &key no-repaint)
  (when (or (> width (bounding-rectangle-width stream))
	    (> height (bounding-rectangle-height stream)))
    (setf (sheet-region stream)
      (make-bounding-rectangle  0 0 
				(max (bounding-rectangle-width stream) width)
				(max (bounding-rectangle-height
				      stream) height)))))

(defun clim::scroll-extent (stream &key (x 0) (y 0))

  ;; This should copy-area and then do a repaint of the new stuff.
  ;; Perhaps for the time being we can just clear the current area and
  ;; then repaint the whole thing
  #+ignore-why-is-this-here
  (setq y (min y (max (- (bounding-rectangle-height stream)
			 (bounding-rectangle-height
			  (clim::pane-viewport stream)))
		      0)))
  (let ((vp (clim::pane-viewport stream)))
    (setf (sheet-transformation stream)
      (make-translation-transformation (- x) (- y)))
    (bounding-rectangle-set-position*
     (silica::xm-viewport-viewport vp) x y)
    (with-sheet-medium (medium vp)
      (draw-rectangle*
	medium
	0 0 
	(bounding-rectangle-width (silica::xm-viewport-viewport vp))
	(bounding-rectangle-height (silica::xm-viewport-viewport vp))
	:ink +background+
	:filled t))
    (clim::replay
     (clim-internals::output-recording-stream-output-record stream)
     stream
     (silica::xm-viewport-viewport vp))))

(defclass xt-pane (silica::pane) ())

(defmethod allocate-space ((p xt-pane) width height)
  (when (sheet-mirror p)
    (tk::set-values (sheet-mirror p) :width width :height height)))

;(defclass xt-composite-pane () ())

(defclass xt-leaf-pane (sheet-permanently-enabled-mixin
			   silica::client-overridability
			   xt-pane 
			   mirrored-sheet-mixin 
			   ask-widget-for-size-mixin)
	  ())

(defmethod sheet-shell (sheet)
  (do ((w (sheet-mirror sheet) (tk::widget-parent w)))
      ((typep w '(or tk::shell null))
       w)))
