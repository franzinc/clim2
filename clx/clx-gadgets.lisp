;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader: clx-gadgets.lisp,v 1.1 92/02/24 13:21:55 cer Exp Locker: cer $

(defclass ask-widget-for-size-mixin () ())

(defmethod realize-pane-1 ((framem clx-frame-manager)
			   frame abstract-type &rest options)
  (let ((type (apply #'realize-pane-class framem abstract-type options)))
    (if type
	(apply #'make-instance type 
	       :frame frame
	       :frame-manager framem
	       (apply #'realize-pane-arglist
		      framem abstract-type options))
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
	(silica::foreground silica::background text-style)
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

      (when text-style
	(setf (getf initargs :font-list)
	  (text-style-mapping port text-style)))

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


(defmethod realize-pane-class ((framem clx-frame-manager) class &rest options) 
  (declare (ignore options))
  (second (assoc class '(
			 (scroll-bar motif-scrollbar)
			 (slider motif-slider)
			 (push-button motif-push-button)
			 (canvas motif-drawing-area)
			 (text-field motif-text-field)
			 (toggle-button motif-toggle-button)
			 (menu-bar motif-menu-bar)
			 (viewport clx-viewport)
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


(defmethod compose-space ((m motif-slider))
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
		    'scrollbar-changed-callback-1
		    sheet))


(defun scrollbar-changed-callback-1 (widget sheet)
  (multiple-value-bind
      (value size)
      (tk::get-values widget :value :slider-size)
    (silica::scrollbar-value-changed-callback
     sheet
     (gadget-client sheet)
     (gadget-id sheet)
     value
     size)))


(defmethod compose-space ((m motif-scrollbar))
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


(defclass clx-top-level-sheet (top-level-sheet) ())


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
						  (sheet clim-stream-sheet)
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
	 (viewport (viewport-viewport-region vp))
	 (extent (stream-output-history (sheet-child vp))))
    (multiple-value-bind
      (value size)
	(tk::get-values widget :value :slider-size)
      (case which
	(:vertical
	  (scroll-extent
	    (sheet-child vp)
	    :x (bounding-rectangle-min-x viewport)
	    :y (truncate
		 (* (max 0 (- (bounding-rectangle-height extent)
			      (bounding-rectangle-height viewport)))
		    (if (= size 100)
			0
			(/ value (- 100 size)))))))
	(:horizontal
	  (scroll-extent
	    (silica::sheet-child vp)
	    :x (truncate
		 (* (max 0 (- (bounding-rectangle-width extent)
			      (bounding-rectangle-width viewport)))
		    (if (= size 100)
			0
			(/ value (- 100 size)))))
	    :y (bounding-rectangle-min-y viewport)))))))
	
;;;;;;;;;;;;;;;

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

;; This is an experiment at using a frame but there are problems with it

(defclass motif-frame-pane (mirrored-sheet-mixin
			    sheet-single-child-mixin
			    sheet-permanently-enabled-mixin
			    silica::pane
			    ask-widget-for-size-mixin)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (sheet motif-frame-pane))
  (values 'tk::xm-frame nil))
	  
	  
