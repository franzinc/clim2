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
;; $fiHeader: xm-gadgets.cl,v 1.3 92/01/02 15:32:57 cer Exp Locker: cer $

(in-package :xm-silica)


(defmethod realize-pane-internal ((realizer motif-frame-manager)
				  frame
				  abstract-type
				  &rest options)
  (let ((type (second (assoc abstract-type '(
					     (silica::scroll-bar motif-scrollbar)
					     (slider motif-slider)
					     (push-button motif-push-button)
					     (canvas motif-drawing-area)
					     (text-field motif-text-field)
					     (toggle-button motif-toggle-button)
					     (silica::menubar motif-menubar)
					     (silica::viewport xm-viewport)
					     
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
					     )))))
    (if type
	(apply #'make-instance
	       type 
	       :frame frame
	       :frame-manager realizer
	       (apply #'realize-pane-arglist
		      realizer
		      abstract-type options))
      (call-next-method))))

(defmethod realize-pane-arglist (realizer type &rest options)
  options)

;;; We now need a lot of classes that mirror the xm classes.


(defmethod allocate-space ((p motif-pane) width height)
  (when (sheet-mirror p)
    (tk::set-values (sheet-mirror p) :width width :height height)))

(defclass motif-pane (silica::pane) (#+ignoreresources))


#+ignore
(defmethod initialize-instance :after ((x motif-pane) &rest args)
  (setf (slot-value x 'resources) args))

#+ignore
(defmethod  find-widget-class-and-initargs-for-sheet :around (port (sheet motif-pane))
  (multiple-value-bind
      (x y) (call-next-method)
    (clos::doplist (key value) (slot-value sheet 'resources)
		   (unless (getf y key)
		     (setf (getf y key) value)))
    (values x y)))

(defclass motif-composite-pane () ())

(defclass ask-widget-for-size-mixin () ())

(defmethod compose-space ((pane ask-widget-for-size-mixin))
  (multiple-value-bind
      (x y width height borderwidth
	 care-x care-y care-width care-height care-borderwidth)
      (tk::widget-best-geometry (sheet-direct-mirror pane))
    (declare (ignore x y borderwidth care-x care-y care-width
		     care-height care-borderwidth)) 
    (make-instance 'silica::space-req :width width :height height)))

(defclass motif-leaf-pane (motif-pane mirrored-sheet-mixin ask-widget-for-size-mixin)
	  ())

;;; Motif widgets that support the :value resource and value-changed callback

(defclass motif-value-pane () ())

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet motif-value-pane) (widget t))
  (tk::add-callback widget
		    :value-changed-callback
		    'queue-value-changed-event
		    sheet))



(defmethod gadget-value ((gadget motif-value-pane))
  (tk::get-values (sheet-mirror gadget) :value))

(defmethod (setf gadget-value) (nv (gadget motif-value-pane))
  (tk::set-values (sheet-mirror gadget) :value nv))

(defmethod queue-value-changed-event (widget sheet)
  (declare (ignore widget))
  (distribute-event
   (port sheet)
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

;;; Push button

(defclass motif-push-button (motif-leaf-pane 
			     action-gadget
			     motif-action-pane
			     sheet-permanently-enabled-mixin
			     silica::foreground-background-and-text-style-mixin
			     silica::push-button) 
	  ((label-string :initarg :label-string :initform "")))

(defmethod realize-pane-arglist (realizer (type (eql 'push-button))
				 &rest args 
				 &key (label ""))
  (list* :label-string label
	 (with-rem-keywords (args args '(:label))
	   (apply #'call-next-method realizer type args))))

(defmethod find-widget-class-and-initargs-for-sheet (port (sheet motif-push-button))
  (values 'tk::xm-push-button 
	  (list :label-string (slot-value sheet 'label-string))))

;;; This widget knows how big it wants to be so asking it returns a
;;; meaningful answer. ie.  Something other than the current size.

;; This should support the arm-callback

(defmethod queue-active-event (widget count sheet)
  (declare (ignore widget count))
  (distribute-event
   (port sheet)
   (make-instance 'activate-gadget-event
		  :gadget sheet)))


;; Drawing area

(defclass motif-drawing-area (motif-leaf-pane 
			      standard-sheet-input-mixin
			      permanent-medium-sheet-output-mixin
			      mute-repainting-mixin) ())

(defmethod find-widget-class-and-initargs-for-sheet (port (sheet motif-drawing-area))
  (values 'tk::xm-drawing-area (list :margin-width 0 
				     :resize-policy :none
				     :margin-height 0)))

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet motif-drawing-area) widget)
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

;;; Slider

(defclass motif-slider (motif-leaf-pane 
			sheet-permanently-enabled-mixin
			value-gadget motif-value-pane)
	  ((orientation :initarg :orientation))  
  (:default-initargs :orientation :horizontal))

(defmethod realize-pane-arglist (realizer (type (eql 'slider)) &rest args &key orientation)
  (append (and orientation (list :orientation orientation))
	  (with-rem-keywords (args args '(:orientation))
	    (apply #'call-next-method realizer type args))))

(defmethod find-widget-class-and-initargs-for-sheet (port (sheet motif-slider))
  (values 'tk::xm-scale nil))

;; This should suport the drag callback

(defmethod compose-space ((m motif-slider))
  (let ((x 16))
    (ecase (slot-value m 'orientation)
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

;;; Scrollbar


(defclass motif-scrollbar (motif-leaf-pane 
			   silica::sheet-permanently-enabled-mixin
			   gadget
			   silica::scrollbar)
	  ((orientation :initarg :orientation))
  (:default-initargs :orientation :horizontal))

(defmethod realize-pane-arglist (realizer (type (eql 'silica::scrollbar)) 
				 &rest args &key orientation)
  (list* :orientation orientation
	 (with-rem-keywords (args args '(:orientation))
			    (apply #'call-next-method realizer type args))))

(defmethod find-widget-class-and-initargs-for-sheet (port (sheet motif-scrollbar))
  (values 'tk::xm-scroll-bar nil))

(defmethod (setf silica::scrollbar-size) (nv (sb motif-scrollbar))
  (tk::set-values (sheet-direct-mirror sb) :slider-size nv)
  nv)

(defmethod (setf silica::scrollbar-value) (nv (sb motif-scrollbar))
  (tk::set-values (sheet-direct-mirror sb) :value nv)
  nv)

(defmethod silica::change-scrollbar-values ((sb motif-scrollbar) &rest args &key slider-size value)
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
     (silica::gadget-client sheet)
     (silica::gadget-id sheet)
     value
     size)))


(defmethod compose-space ((m motif-scrollbar))
  (let ((x 16))
    (ecase (slot-value m 'orientation)
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

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet
							  motif-top-level-sheet) widget)

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
			 sheet)
  (tk::add-callback widget 
		    :resize-callback 
		    'sheet-mirror-resized-callback
		    sheet))

(defmethod allocate-space ((sheet motif-top-level-sheet) width height)
  (silica::resize-sheet*  (car (sheet-children sheet)) 
			  width height))

(defmethod compose-space ((sheet motif-top-level-sheet))
  (compose-space (car (sheet-children sheet))))

(defmethod find-widget-class-and-initargs-for-sheet (port (sheet motif-top-level-sheet))
  (values 'tk::xm-drawing-area 
	  (list :resize-policy :none
		:margin-width 0 :margin-height 0)))


;;;; text field

(defclass motif-text-field (motif-leaf-pane value-gadget
			    sheet-permanently-enabled-mixin
			    motif-value-pane motif-action-pane) ())

(defmethod find-widget-class-and-initargs-for-sheet (port (sheet motif-text-field))
  (values 'tk::xm-text-field nil))

;;; Toggle button

(defclass motif-toggle-button (motif-leaf-pane 
			       sheet-permanently-enabled-mixin
			       motif-value-pane 
			       value-gadget) 
	  ())

(defmethod find-widget-class-and-initargs-for-sheet (port (sheet motif-toggle-button))
  (values 'tk::xm-toggle-button nil))

(defmethod gadget-value ((gadget  motif-toggle-button))
  (tk::get-values (sheet-mirror gadget) :set))

(defmethod (setf gadget-value) (nv (gadget motif-toggle-button))
  (tk::set-values (sheet-mirror gadget) :set nv))

(defmethod sheet-shell (sheet)
  (do ((w (sheet-mirror sheet) (tk::widget-parent w)))
      ((typep w '(or tk::shell null))
       w)))

(defmethod xm-silica::add-sheet-callbacks :after
	   ((port motif-port) 
	    (sheet clim::extended-stream-sheet)
	    (widget t))
  (declare (ignore sheet))
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
			 sheet)
  (tk::add-callback widget 
		    :resize-callback 
		    'sheet-mirror-resized-callback
		    sheet))

;;; scrolling stuff
;
;;;; Scroller Sheet -> xm-scrollbar, make some scrollbars also
;;;; Viewport Sheet -> xm-drawing-area 
;;;; Child of the viewport has its transform altered to accomplish the
;;;; scrolling
;
;(defclass xm-scroller-sheet (motif-pane
;			     gadget
;			     mirrored-sheet-mixin
;			     silica::sheet-single-child-mixin
;			     mute-repainting-mixin
;			     ask-widget-for-size-mixin)
;	  (horizontal-scrollbar
;	   vertical-scrollbar))
;
;(defmethod compose-space ((sheet xm-scroller-sheet))
;  (let ((req (call-next-method)))
;    (setf (silica::space-req-max-height req) silica::+fill+
;	  (silica::space-req-max-width req) silica::+fill+)
;    req))
;
;(defmethod initialize-instance :after ((scroller xm-scroller-sheet) &key contents)
;  (let ((vp (make-instance 'xm-viewport :parent scroller)))
;    (adopt-child vp contents)))
;
;(defmethod find-widget-class-and-initargs-for-sheet (port (sheet xm-scroller-sheet))
;  (values 'tk::xm-scrolled-window
;	  '(:scrolling-policy :application-defined
;	    :scroll-bar-display-policy :static)))
;
;(defmethod realize-mirror :around ((port motif-port) (sheet xm-scroller-sheet))
;  (let* ((m (call-next-method))
;	 (sb1 (make-instance 'tk::xm-scroll-bar :parent m :orientation :horizontal))
;	 (sb2 (make-instance 'tk::xm-scroll-bar :parent m :orientation
;			     :vertical)))
;    (setf (slot-value sheet 'horizontal-scrollbar) sb1
;	  (slot-value sheet 'vertical-scrollbar) sb2)
;    (tk::set-values m :horizontal-scroll-bar sb1 :vertical-scroll-bar sb2)
;    (add-scrollbar-callbacks sheet)
;    m))
;
;(defun add-scrollbar-callbacks (sheet)
;  (with-slots (horizontal-scrollbar vertical-scrollbar) sheet
;    (tk::add-callback horizontal-scrollbar
;		      :value-changed-callback
;		      'scrollbar-changed-callback
;		      :horizontal
;		      sheet)
;    (tk::add-callback vertical-scrollbar
;		      :value-changed-callback
;		      'scrollbar-changed-callback
;		      :vertical
;		      sheet)))

(defun scrollbar-changed-callback (widget which scroller)
  (let* ((vp (silica::sheet-child scroller))
	 (viewport (silica::xm-viewport-viewport vp))
	 (extent (clim-internals::output-recording-stream-output-record
		  (silica::sheet-child vp))))
    (multiple-value-bind
	(value size)
	(tk::get-values widget :value :slider-size)
      (case which
	(:vertical
	 (clim::scroll-extent
	  (silica::sheet-child vp)
	  :x (bounding-rectangle-min-x viewport)
	  :y (truncate
	      (* (max 0 (- (bounding-rectangle-height extent)
			   (bounding-rectangle-height viewport)))
		 (if (= size 100)
		     0
		   (/ value (- 100 size)))))))
	(:horizontal
	 (clim::scroll-extent
	  (silica::sheet-child vp)
	  :x (truncate
	      (* (max 0 (- (bounding-rectangle-width extent)
			   (bounding-rectangle-width viewport)))
		 (if (= size 100)
		     0
		   (/ value (- 100 size)))))
	  :y (bounding-rectangle-min-y viewport)))))))


	

;;;;;;;;;;;;;;;

(defclass xm-viewport (#+ignore sheet
		       #+ignore gadget

		       mirrored-sheet-mixin
		       silica::sheet-single-child-mixin
		       sheet-permanently-enabled-mixin
		       
		       #+ignore sheet-transformation-mixin
		       #+ignore standard-repainting-medium
		       #+ignore standard-sheet-input-mixin
		       #+ignore permanent-medium-sheet-output-mixin
		       #+ignore mute-repainting-mixin

		       silica::wrapping-space-mixin
		       silica::pane)
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

;(defun update-scrollbar (horizontal-scrollbar minx maxx vminx vmaxx)
;  (let  ((size (truncate (* 100 
;			    (if (zerop (- maxx minx))
;				1.0
;				(min 1.0 (/ (- vmaxx vminx) (- maxx minx)))))))
;	 (pos (min 1.0
;		   (max 0.0
;			(if (zerop (- (- maxx minx) (- vmaxx vminx)))
;			    0.0
;			    (/ (- vminx minx) 
;				      (- (- maxx minx) (- vmaxx vminx))))))))
;    (tk::set-values horizontal-scrollbar
;		    :slider-size size
;		    :value (min (- 100 size) 
;				(truncate (* 100 pos))))))

(defmethod find-widget-class-and-initargs-for-sheet (port (sheet xm-viewport))
  (values 'tk::xm-drawing-area
	  '(:scrolling-policy :application-defined
	    :margin-width 0 :margin-height 0
	    :resize-policy :none
	    :scroll-bar-display-policy :static)))

; From the days when the parent used to be a motif scroll window
;(defmethod realize-mirror :around ((port motif-port) (sheet xm-viewport))
;  (let* ((m (call-next-method)))
;    (tk::set-values (sheet-mirror (silica::sheet-parent sheet)) :work-window m)
;    m))


(defmethod add-sheet-callbacks  ((port motif-port) (sheet xm-viewport) widget)
  (tk::add-callback widget 
		    :resize-callback 
		    'sheet-mirror-resized-callback
		    sheet)
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

;;; We have the viewport

;;; Viewport describes the region that is being mapped onto


(defmethod clim::pane-viewport-sheet (x)
  (and (typep (silica::sheet-parent x) 'xm-silica::xm-viewport)
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
				(max (bounding-rectangle-height stream) height)))))

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


(defmethod find-widget-class-and-initargs-for-sheet 
    ((port motif-port)
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
