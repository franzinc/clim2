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
;; $fiHeader: xm-gadgets.lisp,v 1.22 92/04/15 11:48:40 cer Exp Locker: cer $

(in-package :xm-silica)

(defmethod make-pane-class ((framem motif-frame-manager) class &rest options) 
  (declare (ignore options))
  (second (assoc class '(
			 ;; experiment
			 (outlined-pane motif-frame-pane)
			 ;;
			 (scroll-bar motif-scroll-bar)
			 (slider motif-slider)
			 (push-button motif-push-button)
			 (label-pane motif-label-pane)
			 (text-field motif-text-field)
			 (text-editor motif-text-editor)
			 (toggle-button motif-toggle-button)
			 (menu-bar motif-menu-bar)
			 (viewport xm-viewport)
			 (radio-box motif-radio-box)
			 (frame-pane motif-frame-pane)
			 (top-level-sheet motif-top-level-sheet)
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

(defmethod (setf gadget-value) (nv (gadget motif-value-pane) &key)
  (when (sheet-mirror gadget)
    (tk::set-values (sheet-mirror gadget) :value nv)))

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

(defmethod queue-active-event (widget count sheet)
  (declare (ignore widget count))
  (distribute-event
   (port sheet)
   (make-instance 'activate-gadget-event
		  :gadget sheet)))

;;; Label

(defclass motif-label-pane (xt-leaf-pane label-pane) 
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-label-pane))
  (with-accessors ((label gadget-label)
		   (alignment silica::gadget-alignment)) sheet
    (values 'tk::xm-label
	    (append
	     (list :alignment 
		   (ecase alignment
		     ((:left nil) :beginning)
		     (:center :center)
		     (:right :end)))
	     (and label (list :label-string label))))))


;;; Push button

(defclass motif-push-button (xt-leaf-pane
			     push-button
			     motif-action-pane) 
	  ())



(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-push-button))
  (declare (ignore port))
  (with-accessors ((label gadget-label)) sheet
    (values 'tk::xm-push-button 
	    (and label (list :label-string label)))))

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
						     (parent t)
						     (sheet motif-drawing-area))
  (values 'tk::xm-drawing-area (list :margin-width 0 
				     :resize-policy :none
				     :margin-height 0)))

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet motif-drawing-area) widget)
  ;; Now does nothing
  )

;;; range pane mixin

(defclass motif-range-pane (motif-value-pane) ())


(defmethod gadget-value ((gadget motif-range-pane))
  ;;--- We should use the scale functions to get the value
  (let ((mirror (sheet-direct-mirror gadget)))
    (if mirror 
	(multiple-value-bind
	    (smin smax) (silica::gadget-range* gadget)
	  (multiple-value-bind
	      (value mmin mmax)
	      (tk::get-values mirror :value :minimum :maximum)
	    (silica::compute-symmetric-value
	     mmin mmax value smin smax)))
      (call-next-method))))


(defmethod (setf gadget-value) (nv (gadget motif-range-pane) &key)
  (let ((gadget (sheet-mirror gadget)))
    (when gadget
      (multiple-value-bind
	  (smin smax) (silica::gadget-range* gadget)
	(multiple-value-bind
	    (mmin mmax)
	    (tk::get-values gadget :minimum :maximum)
	  (tk::set-values gadget
			  :value (silica::compute-symmetric-value
				  smin smax nv mmin mmax)))))))

;;; Slider

(defclass motif-slider (xt-leaf-pane
			motif-range-pane
			slider)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-slider))
  (with-accessors ((orientation gadget-orientation)
		   (label gadget-label)
		   (show-value-p silica::gadget-show-value-p)
		   (value gadget-value)) sheet
    (values 'tk::xm-scale 
	    (append
	     (and show-value-p (list :show-value show-value-p ))
	     (and label (list :title-string label))
	     (list :orientation orientation)
	     (and value (list :value value))))))


;;;--- It seems that motif-sliders will actually tell you how big they
;;;--- want to be. Mostly

;(defmethod compose-space ((m motif-slider) &key width height)
;  (declare (ignore width height))
;  (destructuring-bind
;      (label scroll-bar) (tk::widget-children (sheet-direct-mirror m))
;    (declare (ignore scroll-bar))
;    (multiple-value-bind
;	(label-x label-y label-width label-height)
;	(if (gadget-label m)
;	    (tk::widget-best-geometry label)
;	  (values nil nil 0 0))
;      (declare (ignore label-x label-y))
;      ;;-- We need to estimate the space requirements for the value if
;      ;;-- that is shown.
;      ;;-- Life sucks and then you die.  We need to determine the
;      ;;-- text-extent of the biggest value we can show and incf that
;      ;;-- Plus some scale margin.
;      (let ((fudge 16))
;	(ecase (gadget-orientation m)
;	  (:vertical
;	   (make-space-requirement :width (+ fudge label-width)
;				   :min-height fudge
;				   :height (max (* 2 fudge) label-height)
;				   :max-height +fill+))
;	  (:horizontal
;	   (make-space-requirement :height (+ fudge label-height)
;				   :min-width fudge
;				   :width (max (* 2 fudge) label-width)
;				   :max-width +fill+)))))))

(defmethod compose-space ((m motif-slider) &key width height)
  (declare (ignore width height))
  (multiple-value-bind
      (sx sy swidth sheight)
      (tk::widget-best-geometry (sheet-direct-mirror m))
    (declare (ignore sx sy))
    (let ((fudge 16))
      (ecase (gadget-orientation m)
	(:vertical
	 (make-space-requirement :width swidth
				 :min-height fudge
				 :height (* 2 fudge)
				 :max-height +fill+))
	(:horizontal
	 (make-space-requirement :height sheight
				 :min-width fudge
				 :width (max (* 2 fudge) swidth)
				 :max-width +fill+))))))
	 
;;; Scroll-Bar


(defclass motif-scroll-bar (xt-leaf-pane
			   scroll-bar)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-scroll-bar))
  (with-accessors ((orientation gadget-orientation)) sheet
    ;;-- we should really decide what the min and max resources should be
    (values 'tk::xm-scroll-bar 
	    (list :orientation orientation))))

(defmethod (setf silica::scroll-bar-size) (nv (sb motif-scroll-bar))
  (tk::set-values (sheet-direct-mirror sb) :slider-size (floor nv))
  nv)

(defmethod (setf silica::scroll-bar-value) (nv (sb motif-scroll-bar))
  (tk::set-values (sheet-direct-mirror sb) :value nv)
  nv)

;;;--- We should use the motif functions for getting and changing the
;;;--- values

(defmethod change-scroll-bar-values ((sb motif-scroll-bar) &key slider-size value)
  (let ((mirror (sheet-direct-mirror sb)))
    (multiple-value-bind
	(smin smax) (silica::gadget-range* sb)
      (multiple-value-bind
	  (mmin mmax) (tk::get-values mirror :minimum :maximum)
	(tk::set-values
	 mirror
	 :slider-size 
	 (fix-coordinate
	  (silica::compute-symmetric-value
		       smin smax slider-size mmin mmax))
	 :value (fix-coordinate
		 (silica::compute-symmetric-value
		  smin smax value mmin mmax)))))))


(defmethod add-sheet-callbacks ((port motif-port) (sheet motif-scroll-bar) (widget t))
  (tk::add-callback widget
		    :value-changed-callback
		    'scroll-bar-changed-callback-1
		    sheet))


(defun scroll-bar-changed-callback-1 (widget sheet)
  (multiple-value-bind
      (smin smax) (silica::gadget-range* sheet)
    (multiple-value-bind
	(value size mmin mmax)
	(tk::get-values widget :value :slider-size :minimum :maximum)
      (scroll-bar-value-changed-callback
       sheet
       (gadget-client sheet)
       (gadget-id sheet)
       (silica::compute-symmetric-value
	mmin mmax value smin smax)
       (silica::compute-symmetric-value
	mmin mmax size smin smax)))))


(defmethod compose-space ((m motif-scroll-bar) &key width height)
  (let ((x 16))
    (ecase (gadget-orientation m)
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

;; Should we stick in our preferred scroll-bar geometry here?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass motif-top-level-sheet (top-level-sheet) ())

(defmethod add-sheet-callbacks :after ((port motif-port) 
				       (sheet motif-top-level-sheet)
				       (widget tk::xm-drawing-area))
  (tk::add-callback widget 
		    :resize-callback 'sheet-mirror-resized-callback
		    sheet))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-top-level-sheet))
  (cond 
   ;;--- hack alert
   ;; Seems that we need to use a bulletin board so that everything
   ;; comes up in the right place.
   ((popup-frame-p sheet)
    (values 'tk::xm-bulletin-board
	    (list :margin-width 0 :margin-height 0
		  ;; We specify NIL for accelerators otherwise the
		  ;; bulletin board messes with the event handling of
		  ;; its drawing area children
		  :accelerators nil
		  :resize-policy :none
		  :name (string (frame-name (pane-frame sheet))))))
   (t
    (values 'tk::xm-drawing-area 
	    (list :resize-policy :none
		  :name (string (frame-name (pane-frame sheet)))
		  :margin-width 0 :margin-height 0)))))

;;; 

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet t) 
				       (widget tk::xm-bulletin-board))
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
			   :exposure
			   :structure-notify
			   :key-press
			   :key-release
			   :button-press
			   :button-release
      			   )
			 0
			 'sheet-mirror-event-handler
			 sheet))


;;;; text field

(defclass motif-text-field (xt-leaf-pane
			    motif-value-pane 
			    motif-action-pane
			    text-field)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-text-field))
  (with-accessors ((value gadget-value)) sheet
    (values 'tk::xm-text-field 
	    (append
	     (and value `(:value ,value))))))

;;; 

(defclass motif-text-editor (xt-leaf-pane
			     motif-value-pane 
			     motif-action-pane
			     silica::text-editor)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet
						      motif-text-editor))
  (with-accessors ((value gadget-value)
		   (ncolumns silica::gadget-columns)
		   (nlines silica::gadget-lines)) sheet
    (values 'tk::xm-text
	    (append
	     (list :edit-mode :multi-line)
	     (and ncolumns (list :columns ncolumns))
	     (and nlines (list :rows nlines))
	     (and value `(:value ,value))))))

;(defmethod compute-initial-mirror-geometry (parent (sheet motif-text-editor) initargs)
;  (multiple-value-bind (left top right bottom)
;      (sheet-actual-native-edges* sheet)
;      (setf (getf initargs :x) (floor left)
;	    (getf initargs :y) (floor top))
;      initargs))
;
;(defmethod compute-initial-mirror-geometry (parent (sheet motif-scrolling-window) initargs)
;  (multiple-value-bind (left top right bottom)
;      (sheet-actual-native-edges* sheet)
;      (setf (getf initargs :x) (floor left)
;	    (getf initargs :y) (floor top))
;    initargs))

(defmethod compose-space ((te motif-text-editor) &key width height)
  (declare (ignore width height))
  (let ((sr (call-next-method)))
    (setq sr (silica::copy-space-requirement sr))
    ;;-- What it the correct thing to do???
    (setf (space-requirement-max-width sr) +fill+
	  (space-requirement-max-height sr) +fill+)
    sr))


(defmethod silica::gadget-supplied-scrolling (frame-manager frame (contents motif-text-editor) &rest ignore)
  (declare (ignore ignore))
  (with-look-and-feel-realization (frame-manager frame)
    (make-pane 'motif-scrolling-window :contents contents)))

;;; Toggle button

(defclass motif-toggle-button (xt-leaf-pane 
			       motif-value-pane
			       toggle-button)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-toggle-button))
  (with-accessors ((set gadget-value)
		   (label gadget-label)
		   (indicator-type gadget-indicator-type)) sheet
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

(defmethod (setf gadget-value) (nv (gadget motif-toggle-button) &key)
  (when (sheet-direct-mirror gadget)
    (tk::set-values (sheet-mirror gadget) :set nv)))

(defmethod add-sheet-callbacks :after ((port motif-port) 
				       (sheet clim-stream-sheet)
				       (widget tk::xm-drawing-area))
  ;;---- It would suprise me if we needed this.
  (tk::add-callback widget 
		    :resize-callback 
		    'sheet-mirror-resized-callback
		    sheet))


(defun scroll-bar-changed-callback (widget which scroller)
  (let* ((vp (sheet-child scroller))
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
	    (sheet-child vp)
	    :x (truncate
		 (* (max 0 (- (bounding-rectangle-width extent)
			      (bounding-rectangle-width viewport)))
		    (if (= size 100)
			0
			(/ value (- 100 size)))))
	    :y (bounding-rectangle-min-y viewport)))))))
	
;;;;;;;;;;;;;;;

(defclass xm-viewport
	  (viewport
	   mirrored-sheet-mixin)
    ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet xm-viewport))
  (values 'tk::xm-drawing-area
	  '(:scrolling-policy :application-defined
	    :margin-width 0 :margin-height 0
	    :resize-policy :none
	    :scroll-bar-display-policy :static)))

(defmethod add-sheet-callbacks :after ((port motif-port) 
				       (sheet xm-viewport)
				       (widget tk::xm-drawing-area))
  ;;--- I wonder whether this is needed since it should not be resized by
  ;; the toolkit and only as part of the goe management code that will
  ;; recurse to children anyway
  (tk::add-callback widget 
		    :resize-callback 
		    'sheet-mirror-resized-callback
		    sheet))

(defclass motif-radio-box (motif-geometry-manager
			   mirrored-sheet-mixin
			   sheet-multiple-child-mixin
			   sheet-permanently-enabled-mixin
			   radio-box
			   pane
			   ask-widget-for-size-mixin)
    ())

(defmethod sheet-adopt-child :after ((gadget motif-radio-box) child)
  (setf (gadget-client child) gadget))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-radio-box))
  
  (with-accessors ((orientation gadget-orientation)) sheet
    (values 'tk::xm-radio-box
	    (list :orientation orientation))))

(defmethod value-changed-callback :after ((v gadget)
					  (client motif-radio-box)
					  (id t)
					  (value t))
  (when (eq value t)
    (setf (radio-box-current-selection client) id)
    (value-changed-callback client 
			    (gadget-client client)
			    (gadget-id client) 
			    id)))

;; Frame-viewport that we need because a sheet can have

(defclass xm-frame-viewport
    (sheet-single-child-mixin
	   sheet-permanently-enabled-mixin
	   silica::wrapping-space-mixin
	   pane
	   mirrored-sheet-mixin)
    ())


(defmethod find-widget-class-and-initargs-for-sheet
    ((port xt-port) (parent t) (sheet xm-frame-viewport))
  (values 'xm-drawing-area 
	  ;;---  These are duplicated
	  (list :margin-width 0 
		:resize-policy :none
		:margin-height 0)))

;(defmethod allocate-space ((fr xm-frame-viewport) width height)
;  ;;-- Is this what wrapping space mixin should do???
;  (move-and-resize-sheet* (sheet-child fr) 0 0 width height))

(defmethod add-sheet-callbacks :after ((port motif-port) 
				       (sheet xm-frame-viewport)
				       (widget tk::xm-drawing-area))
  (tk::add-callback widget 
		    :resize-callback 
		    'sheet-mirror-resized-callback
		    sheet))

(defclass motif-frame-pane (motif-geometry-manager
			    mirrored-sheet-mixin
			    sheet-single-child-mixin
			    sheet-permanently-enabled-mixin
			    pane
			    silica::layout-mixin)
	  ())

(defmethod initialize-instance :after ((pane motif-frame-pane) &key
							       frame-manager frame
							       contents)
  (if (typep contents 'mirrored-sheet-mixin)
      (sheet-adopt-child pane contents)
    (let ((viewport (with-look-and-feel-realization (frame-manager frame)
		      (make-pane 'xm-frame-viewport))))
      (sheet-adopt-child pane viewport)
      (sheet-adopt-child viewport contents))))


(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-frame-pane))
  (values 'tk::xm-frame nil))

(defmethod compose-space ((fr motif-frame-pane) &key width height)
  (declare (ignore width height))
  (silica::space-requirement+*
   (compose-space (sheet-child fr))
   :width 4 :height 4))

(defmethod allocate-space ((fr motif-frame-pane) width height)
  ;;-- We do not need to do anything here because
  ;;-- the pane should resize its child
  )

;;; Scrolling Window

(defclass motif-scrolling-window (motif-geometry-manager
				  ask-widget-for-size-mixin
				  mirrored-sheet-mixin
				  sheet-single-child-mixin
				  sheet-permanently-enabled-mixin
				  pane)
	  ;;-- probably one of the options is whether to have vertical
	  ;;-- and/or horizontal scrollbars
	  ())

(defmethod initialize-instance :after ((pane motif-scrolling-window) &key contents)
  (sheet-adopt-child pane contents))

(defmethod compose-space ((fr motif-scrolling-window) &key width height)
  (declare (ignore width height))
  ;;--- This is not quite right because I think scrollbars are a bit
  ;;--- bigger than this. But atleast its a start
  (let ((fudge-factor (+ 16
			 (tk::get-values (sheet-mirror fr)
					 :spacing)))
	(sr (silica::copy-space-requirement (compose-space
					     (sheet-child fr)))))
    (incf (space-requirement-width sr) fudge-factor)
    (incf (space-requirement-height sr) fudge-factor)
    ;;--- Is this the correct thing to do???
    (setf (space-requirement-min-width sr) fudge-factor
	  (space-requirement-min-height sr) fudge-factor)
    sr))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-scrolling-window))
  (values 'xt::xm-scrolled-window nil))

;;-- This needs work but we are getting there
;;-- We should make it abstract and define the interface
;;-- List of items and a unique string to appear.

(defclass motif-list-pane (xt-leaf-pane)
	  ((items :initarg :items :accessor list-pane-items))
  (:default-initargs :items nil))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet
						      motif-list-pane))
  (with-accessors ((items list-pane-items)) sheet
    (values 'xt::xm-list 
	    `(
	      :items ,items :item-count ,(length items)))))

(defmethod silica::gadget-supplied-scrolling (frame-manager frame (contents motif-list-pane) &rest ignore)
  (declare (ignore ignore))
  (with-look-and-feel-realization (frame-manager frame)
    (make-pane 'motif-scrolling-window :contents contents)))

(defmethod gadget-value ((l motif-list-pane))
  )

(defmethod (setf gadget-value) (nv (l motif-list-pane) &key)
  )

;;; Option buttons

(defclass motif-option-pane (xt-leaf-pane silica::labelled-gadget)
	  ((items :initarg :items :accessor option-pane-items))
  (:default-initargs :items nil))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
						     (parent t)
						     (sheet motif-option-pane))
  (with-accessors ((label gadget-label)
		   (items option-pane-items)) sheet
    (let ((pdm (make-instance 'xt::xm-pulldown-menu :managed nil :parent parent)))
      (dolist (item items)
	(make-instance 'tk::xm-push-button :label-string item :parent pdm))
      (values 'xt::xm-option-menu
	      (append
	       (and label `(:label-string ,label))
	       `(:sub-menu-id ,pdm))))))

;;--- What is the interface to this? It seems to represent one of a
;; set of N items. Does that set have a key and test and (setf)
;; gadget-value changes/returns which is the current one?

;; There seems to be a need for various types of dialog boxes.

;;;; Inform-user
;
;(defmethod port-inform-user ((port motif-port) message 
;			     &rest args
;			     &key (title "Not some information")
;			     &allow-other-keys)
;  (apply #'question-user-1 ' message :title title  args))
;
;
;(defmethod port-error-user ((port motif-port) message 
;			    &rest args
;			    &key (title "Not an error")
;			     &allow-other-keys)
;  (apply #'question-user-1  message :title
;	 title  args))
;
;(defmethod port-question-user ((port motif-port) message 
;			       &rest args
;			       &key (title "Not A Question")
;			     &allow-other-keys)
;  (apply #'question-user-1 ' message :title title  args))
;
;(defmethod port-warn-user ((port motif-port) message 
;			   &rest args
;			   &key (title "Not A Warning")
;			   &allow-other-keys)
;  (apply #'question-user-1  message  :title title args))

;;-- Guess we want to specify text of ok,cancel and help buttons
;;-- and whether there is any help for the help-button

(defmethod port-notify-user ((port motif-port)
			     message-string 
			     &key 
			     (style :inform)
			     (frame *application-frame*)
			     (associated-window
			      (frame-top-level-sheet frame))
			     (title "Notify user")
			     documentation
			     (name title))
  (let ((dialog (make-instance (ecase style
				 (:inform 'tk::xm-information-dialog)
				 (:error 'tk::xm-error-dialog)
				 (:question 'tk::xm-question-dialog)
				 (:warning 'tk::xm-warning-dialog))
			       :parent (sheet-mirror associated-window)
			       :name name
			       :dialog-title title
			       :message-string message-string
			       ))
	(result nil))
    (multiple-value-bind
	(help-button)
	(get-message-box-child dialog :help)
      (flet ((set-it (widget r)
	       (declare (ignore widget))
	       (setq result (list r)))
	     (display-help (widget ignore)
	       (declare (ignore widget ignore))
	       (port-notif-user 
		port
		documentation
		:associated-window associated-window)))
	(tk::add-callback dialog :ok-callback #'set-it t)
	(tk::add-callback dialog :cancel-callback #'set-it nil)
	(if documentation
	    (tk::add-callback help-button :activate-callback #'display-help)
	  (xt::set-sensitive help-button nil))
	(unwind-protect
	    (progn
	      (tk::manage-child dialog)
	      (wait-for-callback-invocation
	       (port associated-window)
	       #'(lambda () result)
	       "Waiting for dialog"))
	  (tk::destroy-widget dialog))
	(car result)))))


(defun wait-for-callback-invocation (port predicate &optional (whostate "Waiting for callback"))
  ;;-- Funnily enough if we are in the mouse process then we
  ;;-- are hosed. We should process events until the
  ;;-- predicate is true
  ;;-- and exit sometime
  (if (eq mp:*current-process* (silica::port-process port))
      (progn
	(loop 
	  (when (funcall predicate) (return nil))
	  (process-next-event port)))
      (mp:process-wait whostate predicate)))

(defun get-message-box-child (widget &rest children)
  (values-list
   (mapcar #'(lambda (child)
	       (tk::convert-resource-in
		widget
		'tk::widget
		(tk::xm-message-box-get-child 
		 widget
		 (encode-box-child child))))
	   children)))

(defun encode-box-child (child)
  (let ((x (getf '(
		   :none	          0 
		   :apply	  1
		   :cancel    2
		   :default   3
		   :ok        4
		   :filter-label     5
		   :filter-text      6
		   :help      7
		   :list		  8
		   :history-list     :list
		   :list-label	  9
		   :message-label    10
		   :selection-label  11
		   :prompt-label     :selection-label
		   :symbol-label     12
		   :text	    	  13
		   :value-text       :text
		   :command-text     :text
		   :separator    	  14
		   :dir-list         15
		   :dir-list-label   16
		   :file-list        :list
		   :file-list-label  :list-label
		   ) 
		 child)))
    (cond ((null x)
	   (error "cannot encode child ~S" child))
	  ((symbolp x)
	   (encode-box-child x))
	  (t x))))

;;;; Working
