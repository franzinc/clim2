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
;; $fiHeader: xt-gadgets.cl,v 1.1 92/01/17 17:48:37 cer Exp $

(in-package :xm-silica)

(defclass ask-widget-for-size-mixin () ())

(defmethod realize-pane-1 ((realizer xt-frame-manager)
			   frame abstract-type &rest options)
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
    (make-instance 'silica::space-requirement :width width :height height)))

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

(defmethod clim-internals::pane-viewport-sheet (x)
  (and (typep (sheet-parent x) 'silica::viewport)
       (sheet-parent x)))

(defmethod clim-internals::pane-viewport (x)
  (clim-internals::pane-viewport-sheet x))

(defmethod clim-internals::pane-viewport-region (x)
  (let ((vp (clim-internals::pane-viewport-sheet x)))
    (and vp
	 (silica::xm-viewport-viewport vp))))

(defun clim-internals::pane-scroller (x)
  (clim-internals::pane-scroller-sheet x))

(defun clim-internals::update-region (stream width height &key no-repaint)
  (when (or (> width (bounding-rectangle-width stream))
	    (> height (bounding-rectangle-height stream)))
    (setf (sheet-region stream)
      (make-bounding-rectangle  0 0 
				(max (bounding-rectangle-width stream) width)
				(max (bounding-rectangle-height
				      stream) height)))))

(defun clim-internals::scroll-extent (stream &key (x 0) (y 0))
  (let ((vp (clim-internals::pane-viewport stream)))
    (with-bounding-rectangle* (left top right bottom) 
                              (clim-internals::pane-viewport-region stream)
      ;;;---- This should actually bash the sheet-transformation
      (setf (sheet-transformation stream)
	    (make-translation-transformation (- x) (- y)))
      (bounding-rectangle-set-position* (silica::xm-viewport-viewport vp) x y)
      (with-bounding-rectangle* (nleft ntop nright nbottom) 
                         	(clim-internals::pane-viewport-region stream)
        (cond
	 ;; if some of the stuff that was previously on display is still on display
	 ;; bitblt it into the proper place and redraw the rest.
	 ((ltrb-overlaps-ltrb-p left top right bottom
				nleft ntop nright nbottom)
	  ;; move the old stuff to the new position
	  (clim-internals::window-shift-visible-region stream 
				       left top right bottom
				       nleft ntop nright nbottom)
	  (let ((rectangles (ltrb-difference nleft ntop nright nbottom
					     left top right bottom)))
	    (dolist (region rectangles)
	      (with-sheet-medium (medium stream)
		(multiple-value-call
		    #'draw-rectangle*
		  medium
		  (bounding-rectangle* region)
		  :ink +background-ink+
		  :filled t))
	      (replay (stream-output-history stream) stream region))))
	 ;; otherwise, just redraw the whole visible viewport
	 ;; Adjust for the left and top margins by hand so clear-area doesn't erase
	 ;; the margin components.
	 (t 
	  ;;---- We should make the sheet-region bigger at this point
	  ;; Perhaps we do a union of the sheet-region and the viewport
	  (with-sheet-medium (medium vp)
	    (draw-rectangle*
	     medium
	     0 0 
	     (bounding-rectangle-width (silica::xm-viewport-viewport vp))
	     (bounding-rectangle-height (silica::xm-viewport-viewport vp))
	     :ink +background-ink+
	     :filled t))
	  (replay
	   (stream-output-history stream)
	   stream
	   (silica::xm-viewport-viewport vp))))))))

(defclass xt-pane (silica::pane) ())

(defmethod allocate-space ((p xt-pane) width height)
  (when (sheet-mirror p)
    (tk::set-values (sheet-mirror p) 
		    :width (floor width)
		    :height (floor height))))

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
