;; -*- mode: common-lisp; package: xm-silica -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: xm-gadgets.lisp,v 1.7 92/01/31 14:56:29 cer Exp Locker: cer $

(in-package :xm-silica)

(defmethod realize-pane-class ((realizer motif-frame-manager) class &rest options) 
  (declare (ignore options))
  (second (assoc class '(
			 (scroll-bar motif-scrollbar)
			 (slider motif-slider)
			 (push-button motif-push-button)
			 (canvas motif-drawing-area)
			 (text-field motif-text-field)
			 (toggle-button motif-toggle-button)
			 (menu-bar motif-menu-bar)
			 (silica::viewport xm-viewport)
			 (radio-box motif-radio-box)
			 (frame-pane motif-frame-pane)
			 (silica::top-level-sheet motif-top-level-sheet)
			 ;; One day
			 (line-editor-pane)
			 (label-button-pane)
			 (radio-button-pane)
			 (horizontal-divider-pane)
			 (vertical-divider-pane)
			 (label-pane)
			 ;;
			 (list-pane)
			 (caption-pane)
			 (cascade-button)
			 ))))

;;; We now need a lot of classes that mirror the xm classes.


;;; Motif widgets that support the :value resource and value-changed callback

(defclass motif-value-pane () ())

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet motif-value-pane) (widget t))
  (tk::add-callback widget
		    :value-changed-callback
		    'queue-value-changed-event
		    sheet))

(defmethod gadget-value ((gadget motif-value-pane))
  (if (sheet-direct-mirror gadget)
      (tk::get-values (sheet-mirror gadget) :value)
    (call-next-method)))

(defmethod (setf gadget-value) (nv (gadget motif-value-pane))
  (when (sheet-mirror gadget)
    (tk::set-values (sheet-mirror gadget) :value nv)))

(defmethod queue-value-changed-event (widget sheet)
  (declare (ignore widget))
  (distribute-event
   (sheet-port sheet)
   (make-instance 'value-changed-gadget-event
		  :gadget sheet
		  :value (gadget-value sheet))))

;;; Motif widgets that support the activate callback

(defclass motif-action-pane () ())

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet motif-action-pane) (widget t))
  (tk::add-callback widget
		    :activate-callback
		    'queue-active-event
		    sheet))

(defmethod queue-active-event (widget count sheet)
  (declare (ignore widget count))
  (distribute-event
   (sheet-port sheet)
   (make-instance 'activate-gadget-event
		  :gadget sheet)))

;;; Push button

(defclass motif-push-button (xt-leaf-pane 
			     push-button
			     motif-action-pane) 
	  ())



(defmethod find-widget-class-and-initargs-for-sheet (port (sheet motif-push-button))
  (declare (ignore port))
  (with-accessors ((label silica::gadget-label)) sheet
    (values 'tk::xm-push-button 
	    (list :label-string label))))

(defmethod add-sheet-callbacks ((port motif-port) (sheet t) (widget tk::xm-drawing-area))
  (tk::add-callback widget 
		    :expose-callback 
		    'sheet-mirror-exposed-callback
		    sheet)
  (tk::add-callback widget 
		    :input-callback 
		    'sheet-mirror-input-callback
		    sheet)
  (tk::add-event-handler widget
			 '(:enter-window 
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

;; Drawing area
;; Who uses this anyway??????????????

(defclass motif-drawing-area (xt-leaf-pane 
			      standard-sheet-input-mixin
			      permanent-medium-sheet-output-mixin
			      mute-repainting-mixin) 
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (sheet motif-drawing-area))
  (values 'tk::xm-drawing-area (list :margin-width 0 
				     :resize-policy :none
				     :margin-height 0)))

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet motif-drawing-area) widget)
  ;; Now does nothing
  )

;;; Slider

(defclass motif-slider (xt-leaf-pane
			motif-value-pane
			slider)
	  ())


(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port) (sheet motif-slider))
  (with-accessors ((orientation silica::gadget-orientation)
		   (label silica::gadget-label)
		   (value gadget-value)) sheet
    (values 'tk::xm-scale 
	    (append
	     (and label (list :title-string label))
	     (list :orientation orientation)
	     (and value (list :value value))))))


(defmethod compose-space ((m motif-slider) &key width height)
  (let ((x 16))
    (ecase (silica::gadget-orientation m)
      (:vertical
       (make-space-requirement :width x
			       :min-height x
			       :height (* 2 x)
			       :max-height +fill+))
      (:horizontal
       (make-space-requirement :height (if (silica::gadget-label m) ;
								    ;Should ask the label how big
								    ;it wants to be and add that in
					   (+ x 20)
					   x)
			       :min-width x
			       :width (* 2 x)
			       :max-width +fill+)))))

;;; Scrollbar


(defclass motif-scrollbar (xt-leaf-pane
			   silica::scrollbar)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (sheet motif-scrollbar))
  (with-accessors ((orientation silica::gadget-orientation)) sheet
		  (values 'tk::xm-scroll-bar 
			  (list :orientation orientation))))

(defmethod (setf silica::scrollbar-size) (nv (sb motif-scrollbar))
  (tk::set-values (sheet-direct-mirror sb) :slider-size (floor nv))
  nv)

(defmethod (setf silica::scrollbar-value) (nv (sb motif-scrollbar))
  (tk::set-values (sheet-direct-mirror sb) :value nv)
  nv)

(defmethod silica::change-scrollbar-values ((sb motif-scrollbar) &rest args 
					    &key slider-size value)
  (declare (ignore slider-size value))
  (apply #'tk::set-values
	(sheet-direct-mirror sb)
	args))

(defmethod add-sheet-callbacks ((port motif-port) (sheet motif-scrollbar) (widget t))
  (tk::add-callback widget
		    :value-changed-callback
		    'scrollbar-changed-callback-internal
		    sheet))


(defun scrollbar-changed-callback-internal (widget sheet)
  (multiple-value-bind
      (value size)
      (tk::get-values widget :value :slider-size)
    (silica::scrollbar-value-changed-callback
     sheet
     (gadget-client sheet)
     (gadget-id sheet)
     value
     size)))


(defmethod compose-space ((m motif-scrollbar) &key width height)
  (let ((x 16))
    (ecase (silica::gadget-orientation m)
      (:vertical
       (make-space-requirement :width x
			       :min-height x
			       :height (* 2 x)
			       :max-height +fill+))
      (:horizontal
       (make-space-requirement :height x
			       :min-width x
			       :width (* 2 x)
			       :max-width +fill+)))))

;; Should we stick in our preferred scrollbar geometry here?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass motif-top-level-sheet (
				 #+ignore sheet
				 mirrored-sheet-mixin
				 sheet-multiple-child-mixin

				 #+ignore sheet-transformation-mixin
				 #+ignore standard-repainting-medium
				 #+ignore standard-sheet-input-mixin
				 #+ignore permanent-medium-sheet-output-mixin
				 #+ignore mute-repainting-mixin
				 
				 silica::pane)
	  ())

(warn "This is really bogus")

(defmethod stream-read-char-no-hang ((x motif-top-level-sheet))
  nil)

(defmethod add-sheet-callbacks :after ((port motif-port) 
				       (sheet motif-top-level-sheet)
				       widget)

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
  (tk::add-callback widget 
		    :resize-callback 
		    'sheet-mirror-resized-callback
		    sheet))

(defmethod allocate-space ((sheet motif-top-level-sheet) width height)
  (silica::resize-sheet*  (car (sheet-children sheet)) 
			  width height))

(defmethod compose-space ((sheet motif-top-level-sheet) &key width height)
  (compose-space (car (sheet-children sheet)) :width width :height height))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (sheet motif-top-level-sheet))
  (values 'tk::xm-drawing-area 
	  (list :resize-policy :none
		:margin-width 0 :margin-height 0)))


;;;; text field

(defclass motif-text-field (xt-leaf-pane
			    motif-value-pane motif-action-pane
			    text-field)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (sheet motif-text-field))
  (with-accessors ((value gadget-value)) sheet
    (values 'tk::xm-text-field 
	    (append
	     (and value `(:value ,value))))))


;;; Toggle button

(defclass motif-toggle-button (xt-leaf-pane 
			       motif-value-pane
			       toggle-button)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (sheet motif-toggle-button))
  (with-accessors ((set gadget-value)
		   (label silica::gadget-label)
		   (indicator-type silica::gadget-indicator-type)) sheet
    (values 'tk::xm-toggle-button 
	    (append (list :set set)
		    (and label (list :label-string label))
		    (list :indicator-type 
			  (ecase indicator-type
			    (:one-of :one-of-many)
			    (:some-of :n-of-many)))))))

(defmethod gadget-value ((gadget motif-toggle-button))
  (if (sheet-direct-mirror gadget)
      (tk::get-values (sheet-mirror gadget) :set)
    (call-next-method)))

(defmethod (setf gadget-value) (nv (gadget motif-toggle-button))
  (when (sheet-direct-mirror gadget)
    (tk::set-values (sheet-mirror gadget) :set nv)))

(defmethod xm-silica::add-sheet-callbacks :after ((port motif-port) 
						  (sheet clim-internals::extended-stream-sheet)
						  (widget t))
  (declare (ignore sheet))
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
  ;; It would suprise me if we needed this.
  (tk::add-callback widget 
		    :resize-callback 
		    'sheet-mirror-resized-callback
		    sheet))


(defun scrollbar-changed-callback (widget which scroller)
  (let* ((vp (silica::sheet-child scroller))
	 (viewport (silica::xm-viewport-viewport vp))
	 (extent (stream-output-history (sheet-child vp))))
    (multiple-value-bind
	(value size)
	(tk::get-values widget :value :slider-size)
      (case which
	(:vertical
	 (clim-internals::scroll-extent
	  (sheet-child vp)
	  :x (bounding-rectangle-min-x viewport)
	  :y (truncate
	      (* (max 0 (- (bounding-rectangle-height extent)
			   (bounding-rectangle-height viewport)))
		 (if (= size 100)
		     0
		   (/ value (- 100 size)))))))
	(:horizontal
	 (clim-internals::scroll-extent
	  (silica::sheet-child vp)
	  :x (truncate
	      (* (max 0 (- (bounding-rectangle-width extent)
			   (bounding-rectangle-width viewport)))
		 (if (= size 100)
		     0
		   (/ value (- 100 size)))))
	  :y (bounding-rectangle-min-y viewport)))))))
	
;;;;;;;;;;;;;;;

(defclass xm-viewport (mirrored-sheet-mixin
		       silica::sheet-single-child-mixin
		       sheet-permanently-enabled-mixin
		       silica::wrapping-space-mixin
		       silica::pane
		       silica::viewport)
	  ;; This describes the region that we are displaying
	  ((viewport :accessor silica::xm-viewport-viewport))
  )




(defmethod initialize-instance :after ((vp xm-viewport) &key)
  (setf (slot-value vp 'viewport)
    (make-bounding-rectangle 0 0 (sheet-width vp) (sheet-height vp))))

(defmethod allocate-space ((vp xm-viewport) width height)
  ;; We do nothing to the child of a viewport
  nil)

(defmethod allocate-space :after ((vp xm-viewport) width height)
  (bounding-rectangle-set-size
   (silica::xm-viewport-viewport vp)
   width height)
  (update-scrollbars vp)
  (clim-internals::viewport-region-changed 
   (silica::sheet-child vp)
   vp))


(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port) (sheet xm-viewport))
  (values 'tk::xm-drawing-area
	  '(:scrolling-policy :application-defined
	    :margin-width 0 :margin-height 0
	    :resize-policy :none
	    :scroll-bar-display-policy :static)))

(defmethod add-sheet-callbacks  :after ((port motif-port) (sheet xm-viewport) widget)
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


;; In some ways this behaves like a leaf since the management of the
;; children is left to the row column widget

(defclass  motif-radio-box (mirrored-sheet-mixin
			    sheet-multiple-child-mixin
			    sheet-permanently-enabled-mixin
			    radio-box
			    silica::pane
			    ask-widget-for-size-mixin)
	   ((current-selection :accessor radio-box-current-selection)))

(defmethod sheet-adopt-child :after ((gadget motif-radio-box) child)
  (setf (gadget-client child) gadget))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (sheet motif-radio-box))
  
  (with-accessors ((orientation silica::gadget-orientation)) sheet
    (values 'tk::xm-radio-box
	    (list :orientation orientation))))

(defmethod value-changed-callback :after ((v gadget)
					  (client motif-radio-box)
					  (id t)
					  (value t))
  (when (eq value t)
    (setf (radio-box-current-selection client) v)
    (value-changed-callback client 
			    (gadget-client client)
			    (gadget-id client) 
			    v)))

;; This is an experiment at using a frame but there are problems with
;; it

(defclass motif-frame-pane (mirrored-sheet-mixin
			    sheet-single-child-mixin
			    sheet-permanently-enabled-mixin
			    silica::pane
			    ask-widget-for-size-mixin)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (sheet motif-frame-pane))
  (values 'tk::xm-frame nil))
	  
	  
