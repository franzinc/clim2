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

(defmethod realize-pane-class ((realizer openlook-frame-manager) class &rest options) 
  (declare (ignore options))
  (second (assoc class '(
			 (silica::scroll-bar openlook-scrollbar)
			 (slider openlook-slider)
			 (push-button openlook-push-button)
			 (canvas openlook-drawing-area)
			 (text-field openlook-text-field)
			 (toggle-button openlook-toggle-button)
			 (silica::menubar openlook-menubar)
			 (silica::viewport ol-viewport)
			 (silica::radio-box openlook-radio-box)
			 (silica::frame-pane openlook-frame-pane)
			 (silica::top-level-sheet openlook-top-level-sheet)
			 ;; One day
			 (line-editor-pane)
			 (label-button-pane)
			 (radio-button-pane)
			 (horizontal-divider-pane)
			 (vertical-divider-pane)
			 (label-pane)
				   ;;;
			 (list-pane)
			 (menu-bar)
			 (caption-pane)
			 (scroll-bar)
			 (radio-box)
			 (cascade-button)
			 ))))


;;;;;;;;;;;;;;;;;;;;

(defclass openlook-scrollbar (xt-leaf-pane
			   silica::scrollbar)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (sheet openlook-scrollbar))
  (with-accessors ((orientation silica::gadget-orientation)) sheet
		  (values 'tk::scrollbar
			  (list :orientation orientation))))

(defmethod (setf silica::scrollbar-size) (nv (sb openlook-scrollbar))
  ;; (tk::set-values (sheet-direct-mirror sb) :slider-size nv)
  nv)

(defmethod (setf silica::scrollbar-value) (nv (sb openlook-scrollbar))
  (tk::set-values (sheet-direct-mirror sb) :slider-value nv)
  nv)

(defmethod silica::change-scrollbar-values ((sb openlook-scrollbar) &rest args 
					    &key slider-size value)
  (declare (ignore slider-size value))
  (tk::set-values
   (sheet-direct-mirror sb)
   :slider-value value))


(defmethod add-sheet-callbacks ((port openlook-port) (sheet openlook-scrollbar) (widget t))
  (tk::add-callback widget
		    :slider-moved
		    'scrollbar-changed-callback-internal
		    sheet))

(defmethod compose-space ((m openlook-scrollbar))
  (let ((x 16))
    (ecase (silica::gadget-orientation m)
      (:vertical
       (silica::make-space-req :width x
			       :min-height x
			       :height (* 2 x)
			       :max-height +fill+))
      (:horizontal
       (silica::make-space-req :height x
			       :min-width x
			       :width (* 2 x)
			       :max-width +fill+)))))

;;; Ol DrawArea Widgets require all of this

(defmethod add-sheet-callbacks ((port openlook-port) (sheet t) (widget tk::draw-area))
  (tk::add-callback widget 
		    :expose-callback 
		    'sheet-mirror-exposed-callback
		    sheet)
  #+doesnot-have-on-of-these
  (tk::add-callback widget 
		    :input-callback 
		    'sheet-mirror-input-callback
		    sheet)
  (tk::add-event-handler widget
			 '(:key-press 
			   :key-release
			   :button-press 
			   :button-release
			   ;; 
			   :enter-window 
			   :leave-window
			   :pointer-motion-hint
			   :pointer-motion
			   :button1-motion
			   :button2-motion
			   :button3-motion
			   :button4-motion
			   :button5-motion
			   :button-motion
			   )
			 0
			 'sheet-mirror-event-handler
			 sheet))

;;; top level sheet

(defclass openlook-top-level-sheet (mirrored-sheet-mixin
				    sheet-multiple-child-mixin
				    silica::pane)
	  ())


(defmethod add-sheet-callbacks :after ((port openlook-port) 
				       (sheet openlook-top-level-sheet)
				       widget)

  (tk::add-callback widget 
		    :resize-callback 
		    'sheet-mirror-resized-callback
		    sheet))

(defmethod allocate-space ((sheet openlook-top-level-sheet) width height)
  (silica::resize-sheet*  (car (sheet-children sheet)) 
			  width height))

(defmethod compose-space ((sheet openlook-top-level-sheet))
  (compose-space (car (sheet-children sheet))))

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (sheet openlook-top-level-sheet))
  (values 'tk::draw-area
	  (list :resize-policy :none
		:margin-width 0 :margin-height 0)))


;; OpenLook viewport

(defclass ol-viewport (mirrored-sheet-mixin
		       silica::sheet-single-child-mixin
		       sheet-permanently-enabled-mixin
		       silica::wrapping-space-mixin
		       silica::pane
		       silica::viewport)
	  ;; This describes the region that we are displaying
	  ((viewport :accessor silica::xm-viewport-viewport))
  )

(defmethod initialize-instance :after ((vp ol-viewport) &key)
  (setf (slot-value vp 'viewport)
    (make-bounding-rectangle 0 0 (sheet-width vp) (sheet-height vp))))

(defmethod allocate-space ((vp ol-viewport) width height)
  ;; We do nothing to the child of a viewport
  nil)

(defmethod allocate-space :after ((vp ol-viewport) width height)
  (bounding-rectangle-set-size
   (silica::xm-viewport-viewport vp)
   width height)
  (update-scrollbars vp)
  (clim-internals::viewport-region-changed 
   (silica::sheet-child vp)
   vp))


(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port) (sheet ol-viewport))
  (values 'tk::draw-area
	  '(:scrolling-policy :application-defined
	    :margin-width 0 :margin-height 0
	    :resize-policy :none
	    :scroll-bar-display-policy :static)))

(defmethod add-sheet-callbacks  :after ((port openlook-port) (sheet ol-viewport) widget)
  ;; I wonder whether this is needed since it should not be resized by
  ;; the toolkit and only as part of the goe management code that will
  ;; recurse to children anyway
  (tk::add-callback widget 
		    :resize-callback 
		    'sheet-mirror-resized-callback
		    sheet)
;  (tk::add-callback widget 
;		    :expose-callback 
;		    'sheet-mirror-exposed-callback
;		    sheet)
;  (tk::add-callback widget 
;		    :input-callback 
;		    'sheet-mirror-input-callback
;		    sheet)
;  (tk::add-event-handler widget
;			 '(:enter-window 
;			   :leave-window
;			   :pointer-motion-hint
;			   :pointer-motion
;			   :button1-motion
;			   :button2-motion
;			   :button3-motion
;			   :button4-motion
;			   :button5-motion
;			   :button-motion
;			   )
;			 0
;			 'sheet-mirror-event-handler
;			 sheet)
  )
