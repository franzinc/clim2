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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: xm-gadgets.lisp,v 1.62 93/01/21 14:59:24 cer Exp $

(in-package :xm-silica)

(defmethod make-pane-class ((framem motif-frame-manager) class &rest options) 
  (declare (ignore options))
  (second (assoc class '((scroller-pane motif-scroller-pane)
                         (outlined-pane motif-frame-pane)
                         (silica::separator motif-separator)
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
                         (check-box motif-check-box)
                         (frame-pane motif-frame-pane)
                         (top-level-sheet motif-top-level-sheet)
                         (list-pane motif-list-pane)
                         (option-pane motif-option-pane)
                         ))))

;;; We now need a lot of classes that mirror the xm classes.


;;; Motif widgets that support the :value resource

(defclass motif-value-pane () ())

(defmethod gadget-value ((gadget motif-value-pane))
  (if (sheet-direct-mirror gadget)
      (tk::get-values (sheet-mirror gadget) :value)
    (call-next-method)))

(defmethod (setf gadget-value) (nv (gadget motif-value-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((m (sheet-direct-mirror gadget)))
    (when (and m (not (equal nv (tk::get-values m :value))))
      (with-no-value-changed-callbacks
          (tk::set-values m :value nv)))))

;;; Motif widgets that support the value-changed callback

(defclass motif-value-changed-callback-pane (motif-value-pane) ())

(defmethod add-sheet-callbacks :after 
           ((port motif-port) (sheet motif-value-changed-callback-pane) (widget t))
  (tk::add-callback widget
                    :value-changed-callback
                    'queue-value-changed-event
                    sheet))

;;; Motif widgets that support the losing-focus callback

(defclass motif-losing-focus-callback-pane (motif-value-pane) ())

(defmethod add-sheet-callbacks :after 
           ((port motif-port) (sheet motif-losing-focus-callback-pane) (widget t))
  (tk::add-callback widget
                    :losing-focus-callback
                    'queue-losing-focus-event
                    sheet))

;; Gadgets that have a :label initarg

(defclass motif-labelled-gadget () ())

(defmethod find-widget-class-and-initargs-for-sheet
    :around ((port motif-port)
             (parent t)
             (sheet motif-labelled-gadget))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (with-accessors ((alignment gadget-alignment)
                     (label gadget-label)) sheet
      (cond 
       ((stringp label)
        (unless (getf initargs :label-string)
          (setf (getf initargs :label-string) label)))
       ((typep label 'tk::pixmap)
        (unless (getf initargs :label-pixmap)
          (setf (getf initargs :label-pixmap) label)
          ;;-- Perhaps we need to stipple this?
          (setf (getf initargs :label-insensitive-pixmap) label)
          (setf (getf initargs :label-type) :pixmap))))

      (unless (getf initargs :alignment)
        (setf (getf initargs :alignment) 
          (ecase alignment
            ((:left nil) :beginning)
            (:center :center)
            (:right :end)))))
    (values class initargs)))

(defmethod (setf gadget-label) :after (nv (sheet motif-labelled-gadget))
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) :label-string (or nv
                                                                  ""))))

(defmethod (setf gadget-alignment) :after (nv (sheet motif-labelled-gadget))
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) 
                    :alignment (ecase nv
                                 ((:left nil) :beginning)
                                 (:center :center)
                                 (:right :end)))))

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
   (allocate-event 'activate-gadget-event
     :gadget sheet)))

;;; Label

(defclass motif-label-pane (label-pane xt-leaf-pane motif-labelled-gadget) 
          ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-label-pane))
  (values 'tk::xm-label nil))

;;; Push button

(defclass motif-push-button (push-button
                             motif-action-pane
                             motif-labelled-gadget
                             xt-leaf-pane) 
          ())



(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-push-button))
  (values 'tk::xm-push-button nil))

(defmethod add-sheet-callbacks
    ((port motif-port) (sheet t) (widget tk::xm-my-drawing-area))
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
                         1
                         'sheet-mirror-event-handler
                         sheet))

;;; range pane mixin

(defclass motif-range-pane (motif-value-changed-callback-pane)
          ())


(defmethod gadget-value ((gadget motif-range-pane))
  ;;--- We should use the scale functions to get the value
  (let ((mirror (sheet-direct-mirror gadget)))
    (if mirror 
        (multiple-value-bind
            (smin smax) (gadget-range* gadget)
          (multiple-value-bind
              (value mmin mmax)
              (tk::get-values mirror :value :minimum :maximum)
            (compute-symmetric-value
             mmin mmax value smin smax)))
      (call-next-method))))


(defmethod (setf gadget-value) (nv (gadget motif-range-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((mirror (sheet-mirror gadget)))
    (when mirror
      (multiple-value-bind
          (smin smax) (gadget-range* gadget)
        (multiple-value-bind
            (mmin mmax)
            (tk::get-values mirror :minimum :maximum)
          (with-no-value-changed-callbacks
              (tk::set-values mirror
                              :value (fix-coordinate 
                                      (compute-symmetric-value
                                       smin smax nv mmin mmax)))))))))

;;; 

(defclass motif-separator (xt-oriented-gadget
                           xt-leaf-pane
                           silica::separator)
          ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-separator))
  (values 'tk::xm-separator nil))

;;; Slider

(defclass motif-oriented-sliding-gadget (xt-oriented-gadget)
	  ())


;;;

(defmethod set-widget-orientation ((gadget motif-oriented-sliding-gadget) nv)
  (tk::set-values (sheet-direct-mirror gadget) 
		  :processing-direction 
		  (case nv
		    (:vertical :max-on-top)
		    (:horizontal :max-on-right))
		  :orientation nv))

(defclass motif-slider (motif-range-pane
                        motif-oriented-sliding-gadget
                        xt-leaf-pane
                        slider)
          ())

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet motif-slider) (widget t))
  (tk::add-callback widget
                    :drag-callback
                    'queue-drag-event
                    sheet))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-slider))
  (with-accessors ((label gadget-label)
                   (show-value-p gadget-show-value-p)
                   (value gadget-value)) sheet
    (multiple-value-bind
        (smin smax) (gadget-range* sheet)
      (let ((mmin 0) 
            (mmax 100)
            (decimal-points 0)
            (decimal-places (slider-decimal-places sheet)))
        (cond ((and (zerop decimal-places)
                    (typep smin '(signed-byte 32))
                    (typep smax '(signed-byte 32)))
               (setq mmin smin mmax smax decimal-points 0))
              ;; Real case
              (t
               (setq smin (float smin) smax (float smax))
               (let ((scaling (expt 10 decimal-places)))
                 (setq mmin (round (* scaling smin))
                       mmax (round (* scaling smax))
                       decimal-points decimal-places))))

        (assert (and (typep mmin '(signed-byte 32))
                     (typep mmax '(signed-byte 32))))
        
        (values 'tk::xm-scale 
                (append
                 (and show-value-p (list :show-value show-value-p ))
                 (and label (list :title-string label))
                 (list :minimum mmin
                       :maximum mmax
                       :decimal-points decimal-points)
                 (and value 
                      (list :value (fix-coordinate 
                                    (compute-symmetric-value
                                     smin smax value mmin mmax))))))))))

(defmethod (setf gadget-show-value-p) :after (nv (sheet motif-slider)) 
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) :show-value nv)))

(defmethod (setf gadget-label) :after (nv (sheet motif-slider))
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) :title-string (or nv ""))))


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
                                 :height (max (* 2 fudge) sheight)
                                 :max-height +fill+))
        (:horizontal
         (make-space-requirement :height sheight
                                 :min-width fudge
                                 :width (max (* 2 fudge) swidth)
                                 :max-width +fill+))))))
         
;;; Scroll-Bar


(defclass motif-scroll-bar (scroll-bar
                            motif-oriented-sliding-gadget
                            xt-leaf-pane)
          ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-scroll-bar))
  (values 'tk::xm-scroll-bar nil))


(defmethod (setf scroll-bar-size) (nv (sb motif-scroll-bar))
  (tk::set-values (sheet-direct-mirror sb) :slider-size (floor nv))
  nv)

(defmethod (setf scroll-bar-value) (nv (sb motif-scroll-bar))
  (tk::set-values (sheet-direct-mirror sb) :value nv)
  nv)

;;;--- We should use the motif functions for getting and changing the
;;;--- values

(defmethod change-scroll-bar-values ((sb motif-scroll-bar) &key slider-size value)
  (let ((mirror (sheet-direct-mirror sb)))
    (when mirror
      (multiple-value-bind
          (mmin mmax) (tk::get-values mirror :minimum :maximum)
        (multiple-value-bind
            (real-value real-size) (compute-new-scroll-bar-values sb mmin mmax value slider-size)
          (tk::set-values
           mirror
           :slider-size real-size
           :value real-value))))))


(defmethod add-sheet-callbacks :after ((port motif-port) (sheet motif-scroll-bar) (widget t))
  (tk::add-callback widget
                    :value-changed-callback
                    'scroll-bar-changed-callback-1
                    sheet))

(defmethod scroll-bar-changed-callback-1 ((widget t) (sheet motif-scroll-bar))
  (multiple-value-bind
      (smin smax) (gadget-range* sheet)
    (multiple-value-bind
        (value size mmin mmax)
        (tk::get-values widget :value :slider-size :minimum :maximum)
      (scroll-bar-value-changed-callback
       sheet
       (gadget-client sheet)
       (gadget-id sheet)
       (compute-symmetric-value
        mmin mmax value smin smax)
       (compute-symmetric-value
        mmin mmax size smin smax)))))


(defmethod compose-space ((m motif-scroll-bar) &key width height)
  (declare (ignore width height))
  ;;-- We should probably ask the widget
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

(defmethod find-widget-class-and-initargs-for-sheet
    ((port xt-port) (parent t) (sheet standard-sheet))
  (values 'tk::xm-my-drawing-area nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass motif-top-level-sheet (xt-top-level-sheet)
          ())

(defmethod add-sheet-callbacks :after ((port motif-port) 
                                       (sheet top-level-sheet)
                                       (widget tk::xm-my-drawing-area))
  (tk::add-callback widget 
                    :resize-callback 'sheet-mirror-resized-callback
                    sheet))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet top-level-sheet))
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
		  ;; Prevents buttons from deactivating dialog
		  :auto-unmanage nil
		  :resize-policy :none
		  :name (if (pane-frame sheet)
			    (string (frame-name (pane-frame sheet)))
			    "a CLIM pop-up"))))
   (t
    (values 'tk::xm-my-drawing-area 
            (list :resize-policy :none
                  :name (if (pane-frame sheet)
                            (string (frame-name (pane-frame sheet)))
                            "a CLIM sheet")
                  :margin-width 0 :margin-height 0)))))

;;;;;;;;;;;;;;;;


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
                         1
                         'sheet-mirror-event-handler
                         sheet))


;;;; text field

(defclass motif-text-field (motif-losing-focus-callback-pane
			    motif-value-changed-callback-pane
                            motif-action-pane
                            text-field
                            xt-leaf-pane)
          ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-text-field))
  (with-accessors ((editable gadget-editable-p)
		   (value gadget-value)) sheet
    (values 'tk::xm-text-field 
            (append
	     (and (not editable) '(:cursor-position-visible nil))
	     `(:editable ,editable)
             (and value `(:value ,value))))))

#+ignore
(defmethod add-sheet-callbacks :after
	   ((port motif-port) (sheet motif-text-field) (widget t))
  (tk::add-callback widget
		    :modify-verify-callback
		    'queue-modify-verify-event
		    sheet))

#+ignore
(defmethod queue-modify-verify-event (widget doit sheet)
  (format excl:*initial-terminal-io* "~&~s,~s,~s" widget doit sheet))
		    
		    

;;; 

(defclass motif-text-editor (motif-losing-focus-callback-pane
                             motif-action-pane
                             text-editor
                             xt-leaf-pane)
          ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet
                                                      motif-text-editor))
  (with-accessors ((value gadget-value)
                   (editable gadget-editable-p)
                   (ncolumns gadget-columns)
                   (nlines gadget-lines)
		   (word-wrap gadget-word-wrap)) sheet
    (let ((scroll-mode
	   (let ((p (sheet-parent sheet)))
	     (and (typep p 'motif-scroller-pane)
		  (silica::scroller-pane-scroll-bar-policy p)))))
      (values 'tk::xm-text
	      (append
	       `(:scroll-horizontal ,(and (member scroll-mode '(:both :horizonal :dynamic)) t))
	       `(:scroll-vertical ,(and (member scroll-mode '(:both :vertical :dynamic)) t))
	       (and (not editable) '(:cursor-position-visible nil))
	       (list :edit-mode :multi-line)
	       (list :editable editable)
	       (and ncolumns (list :columns ncolumns))
	       (and nlines (list :rows nlines))
	       (and value `(:value ,value))
	       (and word-wrap `(:word-wrap t)))))))

(defmethod (setf gadget-word-wrap) :after (nv (gadget motif-text-editor))
  (tk::set-values (sheet-direct-mirror gadget) :word-wrap (and nv t)))

(defmethod (setf gadget-editable-p) :after (nv (te motif-text-editor))
  (let ((m (sheet-direct-mirror te)))
    (tk::set-values m :editable nv)))

;(defmethod compute-initial-mirror-geometry (parent (sheet motif-text-editor) initargs)
;  (multiple-value-bind (left top right bottom)
;      (sheet-actual-native-edges* sheet)
;      (setf (getf initargs :x) (floor left)
;           (getf initargs :y) (floor top))
;      initargs))
;
;(defmethod compute-initial-mirror-geometry (parent (sheet motif-scrolling-window) initargs)
;  (multiple-value-bind (left top right bottom)
;      (sheet-actual-native-edges* sheet)
;      (setf (getf initargs :x) (floor left)
;           (getf initargs :y) (floor top))
;    initargs))

#+ignore
(defmethod compose-space ((te motif-text-editor) &key width height)
  (declare (ignore width height))
  (let ((sr (call-next-method)))
    (multiple-value-bind (width min-width max-width
                          height min-height max-height)
        (space-requirement-components sr)
      (declare (ignore max-width max-height))
      ;;--- What is the correct thing to do???
      (make-space-requirement
        :width width :min-width min-width :max-width +fill+
        :height height :min-height min-height :max-height +fill+))))

(defmethod compose-space ((te motif-text-editor) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (font-list margin-height margin-width)
      (tk::get-values (sheet-direct-mirror te) :font-list
                      :margin-height :margin-width)
    (let* ((max-width most-negative-fixnum)
           (max-height most-negative-fixnum))
      (assert font-list)
      (dolist (font font-list)
        (let ((font (second font)))
          (maxf max-height (tk::font-height font))
          (maxf max-width (tk::font-width font))))
      (make-space-requirement :width (+ (* 2 margin-width) (* max-width (gadget-columns te)))
                              :height (+ (* 2 margin-height) (* max-height (gadget-lines te)))))))

;;; Toggle button

(defclass motif-toggle-button (motif-labelled-gadget
                               motif-value-changed-callback-pane
                               toggle-button
                               xt-leaf-pane)
          ())

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-toggle-button))
  (with-accessors ((set gadget-value)
                   (indicator-type gadget-indicator-type)) sheet
    (values 'xt::xm-toggle-button 
            (append (list :set set)
                    (ecase indicator-type
                      ;;--- How can we deal with this portably
                      ((nil) (list :indicator-on nil :shadow-thickness 2))
                      (:one-of (list :indicator-type :one-of-many))
                      (:some-of (list :indicator-type :n-of-many)))))))

(defmethod gadget-value ((gadget motif-toggle-button))
  (if (sheet-direct-mirror gadget)
      (tk::get-values (sheet-mirror gadget) :set)
    (call-next-method)))

(defmethod (setf gadget-value) (nv (gadget motif-toggle-button) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((m (sheet-direct-mirror gadget)))
    (when (and m (not (equal nv (tk::get-values m :set))))
      (with-no-value-changed-callbacks
          (tk::set-values m :set nv)))))

#+ignore
(defmethod add-sheet-callbacks :after ((port motif-port) 
                                       (sheet clim-stream-sheet)
                                       (widget tk::xm-my-drawing-area))
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
             (bounding-rectangle-min-x viewport)
             (truncate
                 (* (max 0 (- (bounding-rectangle-height extent)
                              (bounding-rectangle-height viewport)))
                    (if (= size 100)
                        0
                        (/ value (- 100 size)))))))
        (:horizontal
          (scroll-extent
            (sheet-child vp)
             (truncate
                 (* (max 0 (- (bounding-rectangle-width extent)
                              (bounding-rectangle-width viewport)))
                    (if (= size 100)
                        0
                        (/ value (- 100 size)))))
             (bounding-rectangle-min-y viewport)))))))
        
;;;;;;;;;;;;;;;

(defclass xm-viewport
          (mirrored-sheet-mixin
           viewport)
    ())

;;;

(defclass motif-row-column-gadget-mixin (xt-oriented-gadget)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet :around  ((port motif-port)
							      (parent t)
							      (sheet motif-row-column-gadget-mixin))
  (multiple-value-bind (class initargs)
      (let ((x (ecase (gadget-orientation sheet)
		 (:vertical (gadget-columns sheet))
		 (:horizontal (gadget-rows sheet)))))
	(when x (setf (getf initargs :num-columns) x)))
    (values class initargs)))
  
;;;

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet xm-viewport))
  (values 'tk::xm-my-drawing-area
          `(:scrolling-policy :application-defined
            :margin-width 0 :margin-height 0
            :resize-policy :none
            :scroll-bar-display-policy :static
	    :name ,(string (pane-name (car (sheet-children sheet)))))))


(defclass motif-radio-box (motif-geometry-manager
                           mirrored-sheet-mixin
                           xt-oriented-gadget
                           sheet-multiple-child-mixin
                           sheet-permanently-enabled-mixin
                           radio-box
                           ask-widget-for-size-mixin
                           xt-pane)
    ())

(defmethod sheet-adopt-child :after ((gadget motif-radio-box) child)
  (setf (gadget-client child) gadget))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-radio-box))
  (values 'tk::xm-radio-box nil))


(defclass motif-check-box (motif-geometry-manager
                           mirrored-sheet-mixin
                           xt-oriented-gadget
                           sheet-multiple-child-mixin
                           sheet-permanently-enabled-mixin
                           check-box
                           ask-widget-for-size-mixin
                           xt-pane)
    ())

(defmethod sheet-adopt-child :after ((gadget motif-check-box) child)
  (setf (gadget-client child) gadget))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-check-box))
  
  (values 'tk::xm-row-column nil))


;; Frame-viewport that we need because a sheet can have

(defclass xm-frame-viewport-mixin (sheet-single-child-mixin
                                   sheet-permanently-enabled-mixin
                                   wrapping-space-mixin
                                   mirrored-sheet-mixin
                                   basic-pane)
          ())

(defclass xm-frame-viewport (xm-frame-viewport-mixin) ())

(defmethod find-widget-class-and-initargs-for-sheet
    ((port xt-port) (parent t) (sheet xm-frame-viewport-mixin))
  (values 'tk::xm-my-drawing-area 
          ;;---  These are duplicated
          (list :margin-width 0 
                :resize-policy :none
                :margin-height 0)))

;(defmethod allocate-space ((fr xm-frame-viewport-mixin) width height)
;  ;;-- Is this what wrapping space mixin should do???
;  (move-and-resize-sheet (sheet-child fr) 0 0 width height))


(defclass motif-frame-pane (foreground-background-and-text-style-mixin
			    motif-geometry-manager
                            mirrored-sheet-mixin
                            sheet-single-child-mixin
                            sheet-permanently-enabled-mixin
                            layout-mixin
                            basic-pane)
          ((thickness :initform nil :initarg :thickness)))

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
  (space-requirement+*
   (compose-space (sheet-child fr))
   :width 4 :height 4))

(defmethod allocate-space ((fr motif-frame-pane) width height)
  (declare (ignore width height))
  ;;-- We do not need to do anything here because
  ;;-- the pane should resize its child
  )

(defmethod add-sheet-callbacks
    ((port motif-port) (sheet t) (widget tk::xm-frame))
  (tk::add-event-handler widget
                         '(:enter-window 
                           :leave-window)
                         0
                         'sheet-mirror-event-handler
                         sheet))
 
;;; Scrolling Window

(defclass basic-motif-scrolling-window (motif-geometry-manager
                                        ask-widget-for-size-mixin
                                        mirrored-sheet-mixin
                                        sheet-multiple-child-mixin
                                        sheet-permanently-enabled-mixin
                                        basic-pane)
          ;;-- probably one of the options is whether to have vertical
          ;;-- and/or horizontal scrollbars
          ())

#+ignore
(defclass motif-scrolling-window (basic-motif-scrolling-window) ())

#+ignore
(defmethod initialize-instance :after ((pane motif-scrolling-window) &key contents)
  (sheet-adopt-child pane contents))

(defmethod compose-space ((fr basic-motif-scrolling-window) &key width height)
  (declare (ignore width height))
  ;;--- This is not quite right because I think scrollbars are a bit
  ;;--- bigger than this. But atleast its a start
  ;;-- We check to see which scrollbars we have

  (let* ((spacing (tk::get-values (sheet-mirror fr) :spacing))
         (sr (compose-space (pane-contents fr))))
    (multiple-value-bind (width min-width max-width
                          height min-height max-height)
        (space-requirement-components sr)

      ;;--- if scroller-pane-gadget-supplies-scrolling-p is true we should
      ;;--- do something different. Perhaps we can ask the widget itself
      ;;--  for the overall size but what about the min size. Otherwise we
      ;;-- might need to do this is a grubby way.
      ;; Perhaps we just call compose-space on the child and then add in
      ;; the size of the scroll-bars.
      (if (scroller-pane-gadget-supplies-scrolling-p fr)
          (multiple-value-bind
              (hb vb)
              (tk::get-values (sheet-direct-mirror fr) :horizontal-scroll-bar :vertical-scroll-bar)
            (let ((ha (and hb (xt::is-managed-p hb) (tk::get-values hb :height)))
                  (va (and vb (xt::is-managed-p vb) (tk::get-values vb :width))))
              (when va (maxf height (+ spacing (* 2 va))))
              (when ha (incf height (+ spacing ha)))
              (when va (maxf min-height (+ spacing (* 2 va))))
              (when ha (incf min-height (+ spacing ha)))
              (maxf max-height height)

              (when ha (maxf width (+ spacing (* 2 ha))))
              (when va (incf width (+ spacing va)))
              (when ha (maxf min-width (+ spacing (* 2 ha))))
              (when va (incf min-width (+ spacing va)))
              (maxf max-width width)))
        (let* ((vsb (scroller-pane-vertical-scroll-bar fr))
               (vsb-sr (and vsb (compose-space vsb)))
               (hsb (scroller-pane-horizontal-scroll-bar fr))
               (hsb-sr (and hsb (compose-space hsb))))
          (when vsb-sr (maxf height (+ spacing (space-requirement-min-height vsb-sr))))
          (when hsb-sr (incf height (+ spacing (space-requirement-height hsb-sr))))
          (when vsb-sr (maxf min-height (+ spacing (space-requirement-min-height vsb-sr))))
          (when hsb-sr (incf min-height (+ spacing (space-requirement-height hsb-sr))))
          (maxf max-height height)
      
          (when hsb-sr (maxf width (+ spacing (space-requirement-min-width hsb-sr))))
          (when vsb-sr (incf width (+ spacing (space-requirement-width vsb-sr))))
          (when hsb-sr (maxf min-width (+ spacing (space-requirement-min-width hsb-sr))))
          (when vsb-sr (incf min-width (+ spacing (space-requirement-width vsb-sr))))
          (maxf max-width width)))
      (make-space-requirement 
        :width width :min-width min-width :max-width max-width
        :height height :min-height min-height :max-height max-height))))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet basic-motif-scrolling-window))
  (values 'xt::xm-scrolled-window nil))

;; List-pane

(defclass motif-list-pane (list-pane xt-leaf-pane)
          ())


(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet
                                                      motif-list-pane))
  (with-accessors ((items set-gadget-items)
                   (value gadget-value)
                   (value-key set-gadget-value-key)
                   (test set-gadget-test)
                   (mode list-pane-mode)
                   (visible-items gadget-visible-items)
                   (name-key set-gadget-name-key)) sheet
    (let ((selected-items
           (compute-list-pane-selected-items sheet value))
	  (scroll-mode
	   (let ((p (sheet-parent sheet)))
	     (and (typep p 'motif-scroller-pane)
		  (silica::scroller-pane-scroll-bar-policy p)))))
      (values 'xt::xm-list 
              `(
		:scroll-bar-display-policy 
		,(case scroll-mode
		   ((:vertical :both) :static)
		   (t :dynamic))
                ,@(and selected-items
                       `(:selected-item-count ,(length selected-items)
                         :selected-items ,selected-items))
                :selection-policy 
                ,(ecase mode
                   (:exclusive :browse-select)
                   (:nonexclusive :multiple-select))
                ,@(and visible-items `(:visible-item-count ,visible-items))
                :items ,(mapcar name-key items) 
                :item-count ,(length items))))))

(defun list-pane-single-selection-callback (widget item-position sheet)
  (declare (ignore item-count))
  (let ((item (funcall (set-gadget-value-key sheet) 
                       (nth (1- item-position) (set-gadget-items sheet)))))
    ;;-- In browse select mode you get told when it is clicked on
    ;;-- even if it is already set.
    (unless (funcall (set-gadget-test sheet) item (gadget-value sheet))
      (queue-value-changed-event 
       widget sheet
       item))))

(defun list-pane-multiple-selection-callback (widget item-positions sheet)
  (declare (ignore item-count))
  (let ((items (mapcar #'(lambda (item-position)
                           (funcall (set-gadget-value-key sheet) 
                                    (nth (1- item-position)
                                         (set-gadget-items sheet))))
                       item-positions)))
    (queue-value-changed-event 
     widget sheet items)))


(defmethod add-sheet-callbacks ((port motif-port) (sheet motif-list-pane) (widget xt::xm-list))
  (ecase (list-pane-mode sheet)
      (:exclusive
       (tk::add-callback widget :browse-selection-callback
                         'list-pane-single-selection-callback sheet))
      (:nonexclusive
       (tk::add-callback widget :multiple-selection-callback 
                         'list-pane-multiple-selection-callback sheet))))

(defmethod (setf gadget-value) :after (nv (l motif-list-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (when (sheet-direct-mirror l)
    (let ((selected-items
           (compute-list-pane-selected-items l nv)))
      (with-no-value-changed-callbacks
            (tk::set-values (sheet-direct-mirror l)
                            :selected-item-count (length selected-items)
                            :selected-items selected-items)))))

;;; Option buttons

(defclass motif-option-pane (option-pane motif-labelled-gadget xt-leaf-pane)
          ((buttons :accessor motif-option-menu-buttons)))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-option-pane))
  (with-accessors ((items set-gadget-items)
                   (printer silica::option-pane-printer)
                   (name-key set-gadget-name-key)
                   (value-key set-gadget-value-key)) sheet
    (let ((pdm (make-instance 'xt::xm-pulldown-menu :managed nil :parent parent)))
      (setf (motif-option-menu-buttons sheet)
        (mapcar #'(lambda (item)
                    (let* ((name (funcall name-key item))
                           (pixmap (unless (or (null printer) (eq printer #'write-token))
                                     (pixmap-from-menu-item 
                                      sheet name printer nil)))
                           (button 
                            (if (null pixmap)
                                (make-instance 'tk::xm-push-button 
                                               :label-string name
                                               :parent pdm)
                              (make-instance 'tk::xm-push-button 
                                             :label-pixmap pixmap
                                             :label-type :pixmap
                                             :parent pdm))))
                      (tk::add-callback  
                       button
                       :activate-callback
                       'option-menu-callback-function
                       (funcall value-key item)
                       sheet)
                      button))
                items))
      (values 'xt::xm-option-menu
              `(:sub-menu-id ,pdm)))))

(defun option-menu-callback-function (widget count value sheet)
  (declare (ignore count))
  (queue-value-changed-event widget sheet value))

(defmethod (setf gadget-value) :after (nv (gadget motif-option-pane) &key invoke-callback)
  (declare (ignore invoke-callback)) 
  (with-no-value-changed-callbacks
      (set-option-menu-value gadget nv)))

(defmethod realize-mirror :around ((port motif-port) (pane motif-option-pane))
  (prog1 (call-next-method)
    (set-option-menu-value pane (gadget-value pane))))

(defun set-option-menu-value (gadget nv)
  (with-accessors ((items set-gadget-items)
                   (value-key set-gadget-value-key)
                   (test set-gadget-test)
                   (printer silica::option-pane-printer)
                   (name-key set-gadget-name-key)) gadget
    (when (sheet-direct-mirror gadget)
      (let* ((x (position nv items :test test :key value-key))
             (widget (sheet-direct-mirror gadget)))
        (when x
          (tk::set-values widget :menu-history (nth x (motif-option-menu-buttons gadget)))
          (let* ((name (funcall name-key (nth x items)))
                 (pixmap (unless (or (null printer) (eq printer #'write-token))
                           (pixmap-from-menu-item 
                            gadget name printer nil)))
                 (widget (tk::intern-widget (tk::xm_option_button_gadget widget))))
            (if pixmap
                (tk::set-values widget :label-pixmap pixmap :label-type :pixmap)
              (tk::set-values widget :label-string name))))))))

(defmethod frame-manager-notify-user ((framem motif-frame-manager)
                                      message-string 
                                      &key 
                                      text-style
                                      (style :inform)
                                      (frame nil frame-p)
                                      (associated-window
                                        (if frame-p
                                            (frame-top-level-sheet frame)
                                            (graft framem)))
                                      (title "Notify user")
                                      documentation
                                      (exit-boxes
                                        '(:exit
                                           :abort
                                           :help))
                                      (name title))
  (let ((dialog (apply #'make-instance (ecase style
					 (:inform 'tk::xm-information-dialog)
					 (:error 'tk::xm-error-dialog)
					 (:question 'tk::xm-question-dialog)
					 (:warning 'tk::xm-warning-dialog))
		       :dialog-style :primary-application-modal
		       :managed nil
		       :parent (if (typep associated-window 'xt::xt-root-class)
				   associated-window
				 (sheet-mirror associated-window))
		       :name name
		       :dialog-title title
		       :message-string message-string
		       (and text-style
			    (let ((fonts (list (text-style-mapping (port framem) text-style))))
			      `(:label-font-list ,fonts :button-font-list ,fonts :text-font-list ,fonts)))))
        (result nil))
    (multiple-value-bind
        (ok-button cancel-button help-button)
        (get-message-box-child dialog :ok :cancel :help)
      (flet ((set-it (widget r)
               (declare (ignore widget))
               (setq result (list r)))
             (display-help (widget ignore)
               (declare (ignore widget ignore))
               (frame-manager-notify-user 
                framem
                documentation
                :associated-window associated-window)))
        (tk::add-callback dialog :ok-callback #'set-it t)
        (tk::add-callback dialog :cancel-callback #'set-it nil)

        (flet ((set-button-state (name button)
                 (unless (dolist (x exit-boxes nil)
                           (cond ((eq x name) (return t))
                                 ((atom x))
                                 ((eq (car x) name)
                                  (tk::set-values button :label-string (second x))
                                  (return t))))
                   (tk::unmanage-child button))))
          (set-button-state :exit ok-button)
          (set-button-state :abort cancel-button)
          (set-button-state :help help-button)
          
          (if documentation
              (tk::add-callback help-button :activate-callback #'display-help)
            (xt::set-sensitive help-button nil)))
        
        (unwind-protect
            (progn
              (tk::manage-child dialog)
              (wait-for-callback-invocation
               (port framem)
               #'(lambda () (or result (not (tk::is-managed-p dialog))))
               "Waiting for dialog"))
          (tk::destroy-widget dialog))
        (car result)))))

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
                   :none                  0 
                   :apply         1
                   :cancel    2
                   :default   3
                   :ok        4
                   :filter-label     5
                   :filter-text      6
                   :help      7
                   :list                  8
                   :history-list     :list
                   :list-label    9
                   :message-label    10
                   :selection-label  11
                   :prompt-label     :selection-label
                   :symbol-label     12
                   :text                  13
                   :value-text       :text
                   :command-text     :text
                   :separator             14
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

;;; File Selection

(defmethod frame-manager-select-file 
           ((framem motif-frame-manager) &rest options 
            &key (frame nil frame-p)
                 (associated-window
                   (if frame-p
                       (frame-top-level-sheet frame)
                       (graft framem)))
                 (title "Select File")
                 documentation
                 file-search-proc
                 directory-list-label
                 file-list-label
                 (exit-boxes '(:exit :abort :help))
                 (name title))
                                  
  (let ((dialog (make-instance 
                 'tk::xm-file-selection-dialog
                 :dialog-style :primary-application-modal
                 :managed nil
                 :parent (sheet-mirror associated-window)
                 :name name
                 :dialog-title title))
        (result nil))
    
    (when directory-list-label
      (tk::set-values dialog :dir-list-label-string
                      directory-list-label))
    
    (when file-list-label
      (tk::set-values dialog :file-list-label-string file-list-label))
    
    (when file-search-proc
      (tk::set-values dialog 
                      :file-search-proc
                      (make-file-search-proc-function dialog
                                                      file-search-proc))
      (tk::xm_file_selection_do_search 
       dialog (xt::xm_string_create_l_to_r 
               (tk::get-values dialog :dir-mask)
               "")))
      
    (flet ((set-it (widget r)
             (declare (ignore widget))
             (setq result (list r))))
      (tk::add-callback dialog :ok-callback #'set-it t)
      (tk::add-callback dialog :cancel-callback #'set-it nil)

      (unwind-protect
          (progn
            (tk::manage-child dialog)
            (wait-for-callback-invocation
             (port associated-window)
             #'(lambda () (or result (not (tk::is-managed-p dialog))))
             "Waiting for dialog")
            (if (car result)
                (tk::get-values dialog :dir-spec :directory)))
        (tk::destroy-widget dialog)))))

(ff::defun-c-callable file-search-proc-callback ((widget :unsigned-long)
                                                 (cb :unsigned-long))
  (setq widget (xt::find-object-from-address widget))
  (file-search-proc-callback-1 
   widget cb
   (or (cdr (assoc :file-search-proc (tk::widget-callback-data widget)))
       (error "No file search proc ~S" widget))))
  
(defun file-search-proc-callback-1 (widget cb fn)
  (multiple-value-bind
      (new newp)
      (funcall fn
               (tk::convert-resource-in 
                widget 'tk::xm-string 
                (tk::xm-file-selection-box-callback-struct-value cb))
               (tk::convert-resource-in 
                widget 'tk::xm-string 
                (tk::xm-file-selection-box-callback-struct-mask cb))
               (tk::convert-resource-in 
                widget 'tk::xm-string 
                (tk::xm-file-selection-box-callback-struct-dir cb))
               (tk::convert-resource-in 
                widget 'tk::xm-string 
                (tk::xm-file-selection-box-callback-struct-pattern
                 cb)))
    (when newp
      (tk::set-values widget 
                      :file-list-items new 
                      :file-list-item-count (length new)
                      :list-updated t))))

(defvar *file-search-proc-callback-address* (ff:register-function 'file-search-proc-callback))

(defun make-file-search-proc-function (dialog file-search-proc)
  (push (cons :file-search-proc file-search-proc)
        (tk::widget-callback-data dialog))
  *file-search-proc-callback-address*)

;;; What follows is mostly an experiment in integrating motif and CLIM
;;; geometry management

(defclass motif-paned-pane (motif-geometry-manager
                            mirrored-sheet-mixin
                            sheet-multiple-child-mixin
                            sheet-permanently-enabled-mixin
                            layout-mixin
                            basic-pane)
          ((thickness :initform nil :initarg :thickness)))

(defmethod initialize-instance :after ((pane motif-paned-pane) &key
                                                               frame-manager frame
                                                               contents)
  (dolist (child contents)
    (if (typep child 'mirrored-sheet-mixin)
        (sheet-adopt-child pane child)
      (let ((viewport (with-look-and-feel-realization (frame-manager frame)
                        (make-pane 'xm-paned-viewport))))
        (sheet-adopt-child pane viewport)
        (sheet-adopt-child viewport child)))))

(defclass xm-paned-viewport (xm-frame-viewport-mixin) ())


(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-paned-pane))
  (values 'xt::xm-paned-window 
          '(:margin-height 0 :margin-width 0)))


(defmethod compose-space ((fr motif-paned-pane) &key width height)
  (declare (ignore height))
  (let ((sr nil))
    (dolist (child (sheet-children fr) sr)
      (let ((csr (compose-space child :width width))
            (m (sheet-direct-mirror child)))
        (when m
          #+ignore
          (tk::set-values 
           m 
           :pane-minimum (space-requirement-min-height csr)
           :pane-maximum (space-requirement-max-height csr)))
        (setq sr 
          (if sr
              (space-requirement+ (space-requirement+* sr :height 10) csr)
            csr))))))

;;; Basically this gadget does not do anything sensible with its
;;; children except that when it is resized.  The onus seems to be on
;;; the children to determine their own sizes.

;;; We can only call compose-space after the tree has been completely mirrored

(defmethod (setf port) :around ((port motif-port) (sheet motif-paned-pane) &key graft)
  (declare (ignore graft))
  (call-next-method)
  ;; At this point we should have dealt mirrored the whole tree
  ;;--- Perhaps at this point we need to add resize-callbacks if there
  ;;--- not any
  (dolist (child (sheet-children sheet))
    (let ((csr (compose-space child))
          (m (sheet-direct-mirror child)))
      (when m
        (tk::set-values 
         m 
         :pane-minimum (fix-coordinate (space-requirement-min-height csr))
         :pane-maximum (fix-coordinate (space-requirement-max-height csr)))))))

(defmethod allocate-space ((fr motif-paned-pane) width height)
  (declare (ignore height))
  (dolist (child (sheet-children fr))
    (let ((m (sheet-direct-mirror child)))
      (when m (tk::set-values m :width (fix-coordinate width))))))

;; If one of these gadgets tries to change its size the drawing area
;; might very well let it.

;; Perhaps in my drawing area the code to respond to geometry requests
;; needs to be in Lisp also, or perhaps it just needs to refuse all
;; geometry requests.....?????
;; It actually looks like that is happening.

;; Row column

(defclass motif-rc-pane (motif-geometry-manager
                         ask-widget-for-size-mixin
                         mirrored-sheet-mixin
                         sheet-multiple-child-mixin
                         sheet-permanently-enabled-mixin
                         layout-mixin
                         basic-pane)
          ((thickness :initform nil :initarg :thickness)))

(defmethod initialize-instance :after ((pane motif-rc-pane) &key
                                                               frame-manager frame
                                                               contents)
  (dolist (child contents)
    (if (typep child 'mirrored-sheet-mixin)
        (sheet-adopt-child pane child)
      (let ((viewport (with-look-and-feel-realization (frame-manager frame)
                        (make-pane 'xm-paned-viewport))))
        (sheet-adopt-child pane viewport)
        (sheet-adopt-child viewport child)))))

(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-rc-pane))
  (values 'xt::xm-row-column nil))


;; Form


(defclass motif-form-pane (motif-geometry-manager
                           ask-widget-for-size-mixin
                           mirrored-sheet-mixin
                           sheet-multiple-child-mixin
                           sheet-permanently-enabled-mixin
                           layout-mixin
                           basic-pane)
          ((attachments :initform nil :initarg :attachments)
           (fraction-base :initform nil :initarg :fraction-base)))

(defmethod initialize-instance :after ((pane motif-form-pane) &key
                                                               frame-manager frame
                                                               contents)
  (dolist (child contents)
    (if (typep child 'mirrored-sheet-mixin)
        (sheet-adopt-child pane child)
      (let ((viewport (with-look-and-feel-realization (frame-manager frame)
                        (make-pane 'xm-paned-viewport))))
        (sheet-adopt-child pane viewport)
        (sheet-adopt-child viewport child)))))




(defmethod find-widget-class-and-initargs-for-sheet ((port motif-port)
                                                     (parent t)
                                                     (sheet motif-form-pane))
  (with-slots (fraction-base) sheet
    (values 'xt::xm-form 
            (and fraction-base (list :fraction-base fraction-base )))))


(defmethod (setf port) :around ((port motif-port) (sheet motif-form-pane) &key graft)
  (declare (ignore graft))
  (call-next-method)
  ;; At this point we should have dealt mirrored the whole tree
  ;;--- Perhaps at this point we need to add resize-callbacks if there
  ;;--- not any

  (dolist (attachment (slot-value sheet 'attachments))
    (apply #'tk::set-values (sheet-mirror (nth (car attachment) (sheet-children sheet)))
           (cdr attachment)))
               
  (dolist (child (sheet-children sheet))
    (let ((csr (compose-space child))
          (m (sheet-direct-mirror child)))
      (when m
        (tk::set-values 
         m 
         :width (fix-coordinate (space-requirement-width csr))
         :height (fix-coordinate (space-requirement-height csr)))))))




;; Utilize a motif scrolling window to provide the scrollbars and
;; geometry management

#+ignore ;; added this to assoc list in make-pane-class at top of file
(defmethod make-pane-class ((framem motif-frame-manager) 
                            (class (eql 'scroller-pane)) 
                            &rest options) 
  (declare (ignore options))
  'motif-scroller-pane)


(defclass motif-scroller-pane (scroller-pane basic-motif-scrolling-window) 
          ())

(defmethod initialize-instance :after ((sp motif-scroller-pane) &key
                                                                scroll-bars 
                                                                contents frame-manager frame) 
  (if (setf (scroller-pane-gadget-supplies-scrolling-p sp)
        (gadget-supplies-scrolling-p contents))
      (sheet-adopt-child sp contents)
    (with-look-and-feel-realization (frame-manager frame)
      (when (member scroll-bars '(:both :dynamic :vertical))
        (let ((sb (make-pane 'scroll-bar
			     :background (pane-background sp)
			     :orientation :vertical :id :vertical :client sp)))
          (setf (scroller-pane-vertical-scroll-bar sp) sb)
          (sheet-adopt-child sp sb)))
      (when (member scroll-bars '(:both :dynamic :horizontal))
        (let ((sb (make-pane 'scroll-bar 
			     :background (pane-background sp)
			     :orientation :horizontal :id :horizontal :client sp)))
          (setf (scroller-pane-horizontal-scroll-bar sp) sb)
          (sheet-adopt-child sp sb)))
      (sheet-adopt-child sp (setf (slot-value sp 'viewport) (make-pane 'viewport :scroller-pane sp)))
      (sheet-adopt-child (slot-value sp 'viewport) contents))))

(defmethod gadget-supplies-scrolling-p ((sheet t)) nil)
(defmethod gadget-supplies-scrolling-p ((sheet motif-text-editor)) t)
(defmethod gadget-supplies-scrolling-p ((sheet motif-list-pane)) t)

(defmethod initialize-mirror :after ((port motif-port)
                                     (parent motif-scroller-pane)
                                     (parent-widget t)
                                     (sheet motif-scroll-bar) 
                                     (widget t))
  (tk::set-values parent-widget
                  (if (eq sheet (scroller-pane-vertical-scroll-bar parent))
                      :vertical-scroll-bar :horizontal-scroll-bar)
                  widget))

(defmethod initialize-mirror :after ((port motif-port)
                                     (parent motif-scroller-pane)
                                     (parent-widget t)
                                     (sheet xm-viewport)
                                     (widget t))
  (tk::set-values parent-widget :work-window widget))



(defmethod initialize-mirror :after ((port motif-port)
                                     (parent motif-geometry-manager)
                                     (parent-widget t)
                                     (sheet t)
                                     (widget t))
  ;; This is a pane in the butt since you only get configure-notify
  ;; events after you have been created
  ;; Really Xt should have a callback for this. Not the drawing area.
  (typecase widget
    (tk::xm-my-drawing-area
     (tk::add-callback widget 
                       :resize-callback 
                       'sheet-mirror-resized-callback
                       sheet))
    (xt::xm-gadget
     (break "gadget"))                  ; I dont think you can do this
                                        ; with gadgets
    (t
     (tk::add-event-handler widget
                            '(:structure-notify)
                            1
                            'sheet-mirror-event-handler
                            sheet))))

(defmethod discard-accelerator-event-p ((port motif-port) event)
  (or (call-next-method)
      ;;-- There are a whole bunch of other keysyms that need to be
      ;;-- ignored too but there does not appear to be an easy way
      ;;-- of going from the osf name to the keysym that we need to ignore
      ;;--  osfMenuBar
      (member (keyboard-event-key-name event) '(:f10))))


(defmethod set-button-accelerator-from-keystroke ((menubar motif-menu-bar) button keystroke)
  (when keystroke 
    (record-accelerator menubar keystroke)
    (let ((accel (format nil "<Key>~A" (car keystroke)))
          (accel-text (format nil "~A" (car keystroke))))
      (dolist (modifier (cdr keystroke))
        (setq accel-text
          (concatenate 'string 
            (case modifier (:control "Ctrl+") (:meta "Alt+") (t ""))
            accel-text))
        (setq accel
          (concatenate 'string 
            (case modifier (:control "Ctrl") (:meta "Mod1") (t ""))
            accel)))
      (tk::set-values button 
                      :accelerator accel
                      :accelerator-text accel-text))))
