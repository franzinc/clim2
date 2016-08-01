;; See the file LICENSE for the full license governing this code.
;;

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

;;; All our gadgets need to work with pixel-based and character-based
;;; size specifications.
(defmethod silica::normalize-space-requirement ((pane client-overridability-mixin)
                                                (sr silica::general-space-requirement))
  (multiple-value-bind (width min-width max-width height min-height max-height)
      (space-requirement-components sr)
    (macrolet ((fix-it (slot-name)
                 `(if (clim-internals::unit-space-requirement-p ,slot-name)
                      (clim-internals::process-unit-space-requirement pane ,slot-name)
                      ,slot-name)))
      (if (and (numberp width) (numberp max-width) (numberp min-width))
          sr
          (make-space-requirement
           :width (fix-it width)
           :min-width (fix-it min-width)
           :max-width (fix-it max-width)
           :height (fix-it height)
           :min-height (fix-it min-height)
           :max-height (fix-it max-height))))))

;;; We now need a lot of classes that mirror the xm classes.


;;; Motif widgets that support the :value resource

(defclass motif-value-pane () ())

(defmethod gadget-value ((gadget motif-value-pane))
  (let ((mirror (sheet-direct-mirror gadget)))
    (if mirror
	(tk::get-values mirror :value)
      (call-next-method))))

(defmethod (setf gadget-value) :after
    (nv (gadget motif-value-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((m (sheet-direct-mirror gadget)))
    (when (and m (not (equal nv (tk::get-values m :value))))
      (with-no-value-changed-callbacks
          (tk::set-values m :value nv)))))

;;; Motif widgets that support the value-changed callback

(defclass motif-value-changed-callback-pane () ())

(defmethod add-sheet-callbacks :after
           ((port motif-port) (sheet motif-value-changed-callback-pane) (widget t))
  (tk::add-callback widget
                    :value-changed-callback
                    'queue-value-changed-event
                    sheet))

;;; Motif widgets that support the losing-focus callback

(defclass motif-losing-focus-callback-pane () ())

(defmethod add-sheet-callbacks :after
           ((port motif-port) (sheet motif-losing-focus-callback-pane) (widget t))
  (tk::add-callback widget
                    :losing-focus-callback
                    'queue-losing-focus-event
                    sheet))

;; Gadgets that have a :label initarg

(defclass motif-labelled-gadget () ())


(defmethod find-widget-initargs-for-sheet
    :around ((port motif-port)
             (parent t)
             (sheet motif-labelled-gadget))
  (let ((initargs (call-next-method)))
    (with-accessors ((alignment gadget-alignment)
		     (label gadget-label)) sheet
      (etypecase label
	((or null string)
	 (unless (getf initargs :label-string)
	   (setf (getf initargs :label-string) (or label "")))
	 (when (typep sheet 'tk-silica::motif-toggle-button)
	   ;; SPR25487 -- P&C
	   ;; Under Motif2.1  a toggle-button, with an empty 
	   ;; label is drawn too small (Motif1.2 had a 
	   ;; more reasonable size for the label-less toggle.)
	   ;; Since Clim  can draw its own prompts (especially in
	   ;; accepting-values) this can cause display-misalignments.
	   ;; I can find no direct way to over-ride this, 
	   ;; so the work-around is to given the label-less
	   ;; toggle-button a blank-space as a label.
	   (let ((old-label-string (getf initargs :label-string)))
	     (when (or (null old-label-string)
		       (< (length old-label-string) 1))
	       (setf (getf initargs :label-string) " ")))))
	((or tk::pixmap pattern)
	 (unless (getf initargs :label-pixmap)
	   (when (typep label 'pattern)
	     (with-sheet-medium (medium sheet)
	       (setq label
                     (pixmap-from-pattern label medium :pixmap))))
	   (setf (getf initargs :label-pixmap) label)
	   ;;-- Perhaps we need to stipple this?
	   (setf (getf initargs :label-insensitive-pixmap) label)
	   (setf (getf initargs :label-type) :pixmap))))
      (unless (getf initargs :alignment)
	(setf (getf initargs :alignment)
              (ecase alignment
                ((:left nil) :beginning)
                (:center :center)
                (:right :end))))
      (setf (getf initargs :recompute-size) nil))
    initargs))

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
                                 (:right :end))
                    :label-string (or (gadget-label sheet) ""))))

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


;;; Motif widgets that support armed/disarmed callbacks

(defclass motif-arm-disarm-pane () ())

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet motif-arm-disarm-pane) (widget t))
  (tk::add-callback widget
                    :arm-callback
                    'queue-armed-event
                    sheet)
  (tk::add-callback widget
                    :disarm-callback
                    'queue-disarmed-event
                    sheet))

;;; Label

(defclass motif-label-pane (label-pane xt-leaf-pane motif-labelled-gadget)
          ())

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-label-pane))
  'tk::xm-label)


(defmethod compose-space ((sheet motif-label-pane) &key width height)
  (declare (ignore width height))
  (compute-space-requirement-for-push-button-or-label sheet))

(defun compute-space-requirement-for-push-button-or-label (sheet)
  (let ((label (gadget-label sheet))
	width
	height)
    (etypecase label
      ((or null string)
       (with-sheet-medium (medium sheet)
	 (multiple-value-setq (width height)
	   (text-size medium label
		      :text-style (pane-text-style sheet)))))
      (tk::pixmap
       (setq width (xt::pixmap-width label)
	     height (xt::pixmap-height label)))
      (pattern
       (setq width (pattern-width label)
	     height (pattern-height label))))
    (make-space-requirement
     :width (+ width (text-gadget-margin-width sheet))
     :height (+ height (text-gadget-margin-height sheet)))))

;;; Push button

(defclass motif-push-button (push-button
                             motif-action-pane
			     motif-arm-disarm-pane
                             motif-labelled-gadget
                             xt-leaf-pane)
          ())



(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-push-button))
  'tk::xm-push-button)

;;; How we deal with the default button in bulletin boards...
;;;
;;; The problem is that as soon as motif sees a default button in a
;;; bulletin board it makes all buttons use the default button
;;; shadowing. This messes things up visually if you don't take this
;;; into account in the space allocation. Unfortunately looking at the
;;; resource :default-button-shadow-thickness isn't good enough
;;; because this returns 0 for any buttons created *before* any
;;; default is set and 1 for buttons created *after*. However, the
;;; bulletin board uses the default-button shadowing for *all* buttons
;;; if any are set as the default.
;;;
;;; What we do therefore is to allocate more space (by a fudge factor)
;;; for all push buttons which are part of bb's to allow for extra
;;; the extra space needed for the default shadowing.
;;;
;;; This works well as long as there is at least one default button in
;;; a bb so that the shadowing is used. However if there are none then
;;; no shadowing occurs and the buttons look stupidly big. So as a
;;; hack we set the default-button for a bb to be the bb itself
;;; forcing motif to do the default shadowing for any
;;; buttons. Fortunately this doesn't appear to break anything
;;;
;;; cim (8/29/94)

(defun find-bulletin-board (widget)
  (and widget
       (if (typep widget 'tk::xm-bulletin-board)
	   widget
	 (find-bulletin-board (tk::widget-parent widget)))))

(defmethod compose-space ((sheet motif-push-button) &key width height)
  (declare (ignore width height))
  (let ((sr (compute-space-requirement-for-push-button-or-label sheet)))
    (if (find-bulletin-board (sheet-mirror sheet))
	(multiple-value-bind (width min-width max-width
			      height min-height max-height)
	    (space-requirement-components sr)
	  (declare (ignore min-width max-width min-height max-height))
	  (let* ((shadow-thickness (tk::get-values (sheet-direct-mirror sheet)
						   :shadow-thickness))
		 (fudge (+ 4 (* 4 shadow-thickness))))
	    (make-space-requirement
	     :width (+ width fudge)
	     :height (+ height fudge))))
      sr)))

(defmethod initialize-mirror :after ((port motif-port)
				     parent
				     parent-widget
				     (sheet motif-push-button)
				     widget)
  (declare (ignore parent))
  (when (push-button-show-as-default sheet)
    (let ((bb (find-bulletin-board parent-widget)))
      (when bb
	(tk::set-values bb :default-button widget)))))

(defmethod initialize-mirror :after ((port motif-port)
				     parent
				     parent-widget
				     (sheet top-level-sheet)
				     (widget tk::xm-bulletin-board))
  (declare (ignore parent parent-widget))
  (tk::set-values widget :default-button widget))

;; drawing area

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


(defmethod (setf gadget-value) :after
    (nv (gadget motif-range-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((mirror (sheet-mirror gadget)))
    (when mirror
      (multiple-value-bind
          (smin smax) (gadget-range* gadget)
        (multiple-value-bind
            (mmin mmax value)
            (tk::get-values mirror :minimum :maximum :value)
	  (let ((nv (fix-coordinate
		     (compute-symmetric-value smin smax nv mmin mmax))))
	    (unless (eql value nv)
	      (with-no-value-changed-callbacks
		  (tk::set-values mirror :value nv)))))))))

;;;

(defclass motif-separator (xt-oriented-gadget
                           xt-leaf-pane
                           silica::separator)
          ())

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-separator))
  'tk::xm-separator)

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
                    sheet)
  (tk::add-callback widget
		    :value-changed-callback
		    'quantize-slider-value
		    sheet))

(defun quantize-slider-value (widget sheet)
  (when (slot-value sheet 'silica::number-of-quanta)
    (multiple-value-bind (scale-multiple value mmax show-value)
	(tk::get-values widget :scale-multiple :value :maximum :show-value)
      (let ((nv (* scale-multiple (round value scale-multiple))))
	;; this hack is to fix a bug in Motif where setting the value
	;; of the slider will move the slider to the correct rounded
	;; position but not the displayed value. This forces the value
	;; to change so that the second set-values moves both the
	;; slider and the displayed value (cim 9/19/95)
	(when (and show-value (eql nv value))
	  (tk::set-values widget :value (if (eql value mmax)
					    (1- value)
					  (1+ value))))
	(tk::set-values widget :value nv)))))

(defmethod (setf gadget-value) :after
	   (nv (gadget motif-slider) &key invoke-callback)
  (declare (ignore nv invoke-callback))
  (quantize-slider-value (sheet-mirror gadget) gadget))

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-slider))
  'tk::xm-scale)

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet motif-slider))
  (with-accessors ((label gadget-label)
                   (show-value-p gadget-show-value-p)
                   (value gadget-value)) sheet
    (multiple-value-bind
        (smin smax) (gadget-range* sheet)
      (let ((mmin 0)
            (mmax 1000)
            (decimal-points 0)
            (decimal-places (slider-decimal-places sheet))
	    (quanta (slot-value sheet 'silica::number-of-quanta)))
        (cond ((and (zerop decimal-places)
                    (typep smin '(signed-byte 32))
                    (typep smax '(signed-byte 32)))
	       (setq mmin smin mmax smax decimal-points 0))
	      ;; Real case
              (t
               (setq smin (float smin) smax (float smax))
               (let ((scaling (expt 10 decimal-places)))
                 (setq mmin (fix-coordinate (* scaling smin))
                       mmax (fix-coordinate (* scaling smax))
                       decimal-points decimal-places))))


        (assert (and (typep mmin '(signed-byte 32))
                     (typep mmax '(signed-byte 32))))

        (append (and show-value-p (list :show-value show-value-p))
		(and (stringp label) (list :title-string label))
		(list :minimum mmin
		      :maximum mmax
		      :decimal-points decimal-points)
		(and quanta
		     (list :scale-multiple
			   (max 1 (floor (- mmax mmin) quanta))))
		(and value
		     (list :value (fix-coordinate
				   (compute-symmetric-value
				    smin smax value mmin mmax)))))))))

(defmethod realize-mirror :around ((port motif-port) (pane motif-slider))
  (let* ((mirror (call-next-method))
	 (scale-orientation (gadget-orientation pane))
	 (ticks (slot-value pane 'silica::number-of-tick-marks))
	 (min-label (slot-value pane 'silica::min-label))
	 (max-label (slot-value pane 'silica::max-label))
	 (initargs (remove-keywords
		    (find-widget-resource-initargs-for-sheet port pane)
		    '(:font-list)))
	 (font-list (text-style-mapping
		     port
		     (or (slot-value pane 'silica::range-label-text-style)
			 (sheet-text-style port pane))))
	 (children nil)
	 (tick-long-dimension (tk::font-height font-list))
	 (tick-short-dimension 8)
	 tick-orientation tick-width tick-height
	 first-label second-label)
    (case scale-orientation
      (:vertical
       (setq tick-orientation :horizontal
	     tick-width tick-long-dimension
	     tick-height tick-short-dimension
	     first-label max-label second-label min-label))
      (:horizontal
       (setq tick-orientation :vertical
	     tick-width tick-short-dimension
	     tick-height tick-long-dimension
	     first-label min-label second-label max-label)))
    (when first-label
      (push (apply #'make-instance 'tk::xm-label
		   :parent mirror :managed nil
		   :label-string first-label
		   :font-list font-list
		   initargs)
	    children))
    (dotimes (i ticks)
      (push (apply #'make-instance 'tk::xm-separator
		   :orientation tick-orientation
		   :width tick-width :height tick-height
		   :parent mirror :managed nil
		   initargs)
	    children))
    (when second-label
      (push (apply #'make-instance 'tk::xm-label
		   :parent mirror :managed nil
		   :label-string second-label
		   :font-list font-list
		   initargs)
	    children))
    (tk::manage-children children)
    mirror))

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
    (let ((fudge 64))
      (ecase (gadget-orientation m)
        (:vertical
         (make-space-requirement :width swidth
                                 :min-height fudge
                                 :height (max fudge sheight)
                                 :max-height +fill+))
        (:horizontal
         (make-space-requirement :height sheight
                                 :min-width fudge
                                 :width (max fudge swidth)
                                 :max-width +fill+))))))

;;; Scroll-Bar


(defclass motif-scroll-bar (scroll-bar
                            motif-oriented-sliding-gadget
                            xt-leaf-pane)
          ())


(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-scroll-bar))
  'tk::xm-scroll-bar)


(defmethod find-widget-initargs-for-sheet ((port motif-port)
                                           (parent t)
                                           (sheet motif-scroll-bar))
  (let ((size (scroll-bar-size sheet))
        (increment (silica::scroll-bar-line-increment sheet)))
    (append `(:minimum 0 :maximum ,*scroll-bar-quantization*)
      (when increment
        `(:increment
          ,(silica::convert-scroll-bar-size-out sheet increment)))
      (when size
        (let ((internal-size (silica::convert-scroll-bar-size-out sheet size)))
          `(:slider-size ,internal-size
             ;; Set page increment too (alemmens, 2004-12-10).
             :page-increment ,internal-size)))
      ;; Set initial value (alemmens, 2004-12-10).
      (let ((value (gadget-value sheet)))
        (when value
          `(:value
            ,(silica::convert-scroll-bar-value-out sheet value)))))))

;;;--- We should use the motif functions for getting and changing the
;;;--- values

(defmethod change-scroll-bar-values ((sb motif-scroll-bar) &key
							   slider-size
							   value
							   line-increment
							   (page-increment slider-size))
  (let ((mirror (sheet-direct-mirror sb)))
    (when mirror
      (multiple-value-bind (value slider-size line-increment page-increment)
	  (compute-new-scroll-bar-values
	   sb value slider-size line-increment page-increment)
	(tk::set-values mirror
			:increment line-increment :page-increment page-increment
			:slider-size slider-size :value value)))))

(defmethod add-sheet-callbacks :after ((port motif-port) (sheet motif-scroll-bar) (widget t))
  (tk::add-callback widget
                    :value-changed-callback
                    'xm-scroll-bar-callback
                    sheet
		    'value-changed-callback)
  (tk::add-callback widget
		    :drag-callback
		    'xm-scroll-bar-callback
		    sheet
		    'drag-callback)
  ;; Callbacks below here are new.
  (tk::add-callback widget
		    :page-increment-callback
		    'xm-scroll-bar-callback
		    sheet
		    'silica::scroll-down-page-callback)
  (tk::add-callback widget
		    :page-decrement-callback
		    'xm-scroll-bar-callback
		    sheet
		    'silica::scroll-up-page-callback)
  (tk::add-callback widget
		    :increment-callback
		    'xm-scroll-bar-callback
		    sheet
		    'silica::scroll-down-line-callback)
  (tk::add-callback widget
		    :decrement-callback
		    'xm-scroll-bar-callback
		    sheet
		    'silica::scroll-up-line-callback)
  (tk::add-callback widget
		    :to-bottom-callback
		    'xm-scroll-bar-callback
		    sheet
		    'silica::scroll-to-bottom-callback)
  (tk::add-callback widget
		    :to-top-callback
		    'xm-scroll-bar-callback
		    sheet
		    'silica::scroll-to-top-callback)
  )

(defun xm-scroll-bar-callback (widget sheet callback-fn)
  (funcall callback-fn
	   sheet
	   (gadget-client sheet)
	   (gadget-id sheet)
	   (silica::convert-scroll-bar-value-in sheet
                                                (tk::get-values widget :value))))

(defmethod gadget-value ((gadget motif-scroll-bar))
  (let ((mirror (sheet-direct-mirror gadget)))
    (if mirror
	(silica::convert-scroll-bar-value-in gadget
                                             (tk::get-values mirror :value))
      (call-next-method))))

(defmethod (setf gadget-value) :after
	   (nv (gadget motif-scroll-bar) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((mirror (sheet-direct-mirror gadget)))
    (when mirror
      (tk::set-values mirror
		      :value (silica::convert-scroll-bar-value-out gadget nv)))))

(defmethod scroll-bar-size ((sheet motif-scroll-bar))
  (let ((mirror (sheet-direct-mirror sheet)))
    (if mirror
	(silica::convert-scroll-bar-size-in sheet
                                            (tk::get-values mirror :slider-size))
      (call-next-method))))

(defmethod (setf scroll-bar-size) :after (value (object motif-scroll-bar))
  (let ((mirror (sheet-direct-mirror object)))
    (when mirror
      ;; Set the XmNsliderSize and XmNpageIncrement resources so the correct
      ;; thing happens when the user presses the up-page/down-page buttons.
      ;; Apparently it is not sufficient just to set slider-size.  JPM.
      (let ((v (silica::convert-scroll-bar-size-out object value)))
	(tk::set-values mirror :slider-size v :page-increment v)))))

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

(defmethod (setf silica::scroll-bar-line-increment) :after
	   (value (object motif-scroll-bar))
  (let ((mirror (sheet-direct-mirror object)))
    (when mirror
      ;; Set the XmNincrement resource so the correct thing happens when
      ;; the user presses the up-line/down-line buttons.
      (tk::set-values mirror
		      :increment
		      (silica::convert-scroll-bar-size-out object value)))))

;; Should we stick in our preferred scroll-bar geometry here?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-widget-class-and-name-for-sheet
    ((port xt-port) (parent t) (sheet standard-sheet))
  'tk::xm-my-drawing-area)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass motif-top-level-sheet (xt-top-level-sheet)
          ())

(defmethod add-sheet-callbacks :after ((port motif-port)
                                       (sheet top-level-sheet)
                                       (widget tk::xm-my-drawing-area))
  (tk::add-callback widget
                    :resize-callback 'sheet-mirror-resized-callback
                    sheet))

(defparameter *xm-dialog-style*
    :primary-application-modal)

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet top-level-sheet))
  (let* ((frame (pane-frame sheet))
	 (name (and frame
		    (frame-name frame))))
    (values (if (popup-frame-p sheet)
		;;--- hack alert
		;; Seems that we need to use a bulletin board so that everything
		;; comes up in the right place.
		'tk::xm-bulletin-board
	      'tk::xm-my-drawing-area)
	     name)))

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet top-level-sheet))

  (append (when (popup-frame-p sheet)
	    ;; extra initargs for the bulletin board case
	    (list :dialog-style *xm-dialog-style*
		  ;; We specify NIL for accelerators otherwise the
		  ;; bulletin board messes with the event handling of
		  ;; its drawing area children
		  :accelerators nil
		  ;; Prevents buttons from deactivating dialog
		  :auto-unmanage nil))
	  (list :resize-policy :none
		:margin-width 0 :margin-height 0)))

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

;; text-fields no longer inherit from motif-value pane because we have
;; special value handling for password type entry fields. (cim 5/25/95)

(defclass motif-text-field (motif-losing-focus-callback-pane
			    motif-value-changed-callback-pane
                            motif-action-pane
                            text-field
                            xt-leaf-pane)
  ())

(defmethod text-field-echoed-value ((sheet motif-text-field) value)
  (let ((echo-character (text-field-echo-character sheet)))
    (if echo-character
	(make-string (length value) :initial-element echo-character)
      value)))

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-text-field))
  'tk::xm-text-field)

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet motif-text-field))
  (with-accessors ((editable gadget-editable-p)
		   (value gadget-value)) sheet
    (append (and (not editable) '(:cursor-position-visible nil))
	    `(:editable ,editable)
	    (and value `(:value
			 ,(text-field-echoed-value sheet value))))))

(defmethod gadget-current-selection ((tf motif-text-field))
  (let ((m (sheet-direct-mirror tf)))
    (and m
	 (let  ((x (tk::xm_text_field_get_selection m)))
	   (and (not (zerop x))
		(values (excl:native-to-string x)))))))

(defmethod (setf gadget-value) :after
    (nv (gadget motif-text-field) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((m (sheet-direct-mirror gadget))
	(ev (text-field-echoed-value gadget nv)))
    (when (and m (not (equal ev
			     (tk::get-values m :value))))
      (with-no-value-changed-callbacks
          (tk::set-values m :value ev)))))

(defmethod add-sheet-callbacks :after
	   ((port motif-port) (sheet motif-text-field) (widget t))
  (tk::add-callback widget
		    :modify-verify-callback
		    'text-field-modify-verify-callback
		    sheet))

(defmethod text-field-modify-verify-callback (widget callback-struct sheet)
  (declare (ignore widget))
  (unless *dont-invoke-callbacks*
    (let* ((text-block (tk::xm-text-field-callback-struct-text callback-struct))
	   (length (tk::xm-text-block-rec-length text-block))
	   (ptr (tk::xm-text-block-rec-ptr text-block))
	   (text (if (zerop length)
		     ""
		   (excl:native-to-string ptr)))
	   (start-pos (tk::xm-text-field-callback-struct-start-pos callback-struct))
	   (end-pos (tk::xm-text-field-callback-struct-end-pos callback-struct)))
      (with-slots (silica::value) sheet
	(setf silica::value
	  (concatenate
	      'string
	    (subseq silica::value 0 start-pos)
	    text
	    (subseq silica::value end-pos))))
      (unless (zerop length)
	(setf (tk::xm-text-block-rec-ptr text-block)
	  (clim-utils:string-to-foreign (text-field-echoed-value sheet text)))))))


;;; New definitions to support specifying width and height in terms of
;;; lines and characters.

;;;--- Implement caching
;;;--- Flush ncolumns, nrows but perhaps keep them of compatibility
;;;--- We are not supporting (pixels :relative)


(defmethod silica::normalize-space-requirement ((sheet motif-text-field) sr)
  (declare (ignore width height))
  (normalize-space-for-text-field-or-label
   sheet sr))

;;-- For label pane there are margin-{left,top,right,bottom} resources also
;;-- but they default to 0.

(defmethod silica::normalize-space-requirement ((sheet motif-label-pane) sr)
  (declare (ignore width height))
  (normalize-space-for-text-field-or-label
   sheet sr))

(defmethod silica::normalize-space-requirement ((sheet motif-push-button) sr)
  (declare (ignore width height))
  (normalize-space-for-text-field-or-label
   sheet sr))

(defun process-width-specification (sheet width)
  (when (numberp width) (return-from process-width-specification width))
  (let ((chars (etypecase width
		 (list
		  (assert (eq (second width) :character))
		  (first width))
		 (string (length width)))))
    (multiple-value-bind (font-list margin highlight shadow)
	(tk::get-values (sheet-direct-mirror sheet) :font-list
			:margin-width :highlight-thickness :shadow-thickness)
      (+ (* 2 (+ margin highlight shadow)) (* (font-list-max-width-and-height font-list) chars)))))

(defun text-gadget-margin-width (sheet)
  (multiple-value-bind (margin highlight shadow)
      (tk::get-values (sheet-direct-mirror sheet) :margin-width :highlight-thickness :shadow-thickness)
    (* 2 (+ margin highlight shadow))))

(defun font-list-max-width-and-height (font-list)
  (let* ((max-width most-negative-fixnum)
	 (max-ascent most-negative-fixnum)
	 (max-descent most-negative-fixnum))
    (assert font-list)
    (dolist (font font-list (values max-width (+ max-ascent max-descent)))
      (let ((font (second font)))
	(maxf max-ascent (tk::font-ascent font))
	(maxf max-descent (tk::font-descent font))
	(maxf max-width (tk::font-width font))))))

;;-- What should this do really?

(defun process-height-specification (sheet width)
  (when (numberp width) (return-from process-height-specification width))
  (let ((chars (etypecase width
		 (list
		  (assert (eq (second width) :line))
		  (first width))
		 (string (length width)))))
    (multiple-value-bind (font-list margin highlight shadow)
	(tk::get-values (sheet-direct-mirror sheet)
			:font-list :margin-height :highlight-thickness :shadow-thickness)
      (let ((font-height (nth-value 1 (font-list-max-width-and-height font-list))))
	(+ (* 2 (+ margin highlight shadow)) (* font-height chars))))))

(defun text-gadget-margin-height (sheet)
  (multiple-value-bind (margin highlight shadow)
      (tk::get-values (sheet-direct-mirror sheet) :margin-height :highlight-thickness :shadow-thickness)
    (* 2 (+ margin highlight shadow))))



;;;

(defclass motif-text-editor (motif-losing-focus-callback-pane
			     motif-value-changed-callback-pane
			     motif-value-pane
                             motif-action-pane
                             text-editor
                             xt-leaf-pane)
          ())

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-text-editor))
  'tk::xm-text)

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet motif-text-editor))
  (with-accessors ((value gadget-value)
                   (editable gadget-editable-p)
                   (ncolumns gadget-columns)
                   (nlines gadget-lines)
		   (word-wrap gadget-word-wrap)) sheet
    (let ((scroll-mode
	   (let ((p (sheet-parent sheet)))
	     (and (typep p 'motif-scroller-pane)
		  (scroller-pane-scroll-bar-policy p)))))
      (append `(:scroll-horizontal
		,(and (not word-wrap)
                      (member scroll-mode '(t :both :horizontal :dynamic)) t))
	      `(:scroll-vertical
		,(and (member scroll-mode '(t :both :vertical :dynamic)) t))
	      (and (not editable) '(:cursor-position-visible nil))
	      (list :edit-mode :multi-line)
	      (list :editable editable)
	      (and ncolumns (list :columns ncolumns))
	      (and nlines (list :rows nlines))
	      (and value `(:value ,value))
	      (and word-wrap `(:word-wrap t))))))

(defmethod gadget-current-selection ((tf motif-text-editor))
  (let ((m (sheet-direct-mirror tf)))
    (and m
	 (let  ((x (tk::xm_text_get_selection m)))
	   (and (not (zerop x))
		(values (excl:native-to-string x)))))))

(defmethod silica::set-selection ((tf motif-text-editor) startpos endpos)
  (let ((m (sheet-direct-mirror tf)))
    (when m
      (let* ((display (port-display (port tf)))
             (time (tk::xt-last-timestamp-processed display)))
        (tk::xm_text_set_selection m startpos endpos time)))))

(defmethod compose-space ((te motif-text-editor) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width (process-width-specification te`(,(gadget-columns te) :character))
			  :height (process-height-specification te `(,(gadget-lines te) :line))))

(defmethod silica::normalize-space-requirement ((sheet motif-text-editor) sr)
  (declare (ignore width height))
  (normalize-space-requirement-for-text-editor sheet sr))

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



;;; Toggle button

(defclass motif-toggle-button (motif-labelled-gadget
                               motif-value-changed-callback-pane
			       motif-arm-disarm-pane
                               toggle-button
                               xt-leaf-pane)
          ())

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-toggle-button))
  'xt::xm-toggle-button)

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet motif-toggle-button))
  (with-accessors ((set gadget-value)
                   (indicator-type gadget-indicator-type)) sheet
    (append (list :set set)
	    (ecase indicator-type
	      ;;--- How can we deal with this portably
	      ((nil) (list :indicator-on nil :shadow-thickness 2))
	      (:one-of (list :indicator-type :one-of-many))
	      (:some-of (list :indicator-type :n-of-many))))))

(defmethod gadget-value ((gadget motif-toggle-button))
  (let ((m (sheet-direct-mirror gadget)))
    (if m
	(plusp (tk::xm_toggle_button_get_state m))
      (call-next-method))))

(defmethod (setf gadget-value) :after
    (nv (gadget motif-toggle-button) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((m (sheet-direct-mirror gadget)))
    (when (and m (not (equal nv (plusp (tk::xm_toggle_button_get_state m)))))
      (with-no-value-changed-callbacks
	  ;;--- For some reason you loose if 1 is specified for the
	  ;;-- notify argument. Lisp dies.
	  (tk::xm_toggle_button_set_state m (if nv 1 0) 0))))
  nv)

#+ignore
(defmethod add-sheet-callbacks :after ((port motif-port)
                                       (sheet clim-stream-sheet)
                                       (widget tk::xm-my-drawing-area))
  ;;---- It would suprise me if we needed this.
  (tk::add-callback widget
                    :resize-callback
                    'sheet-mirror-resized-callback
                    sheet))

;; ignoring the following function - what does it do - no-one appears
;; to call it (cim 6/21/94)

#+ignore
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

(defmethod find-widget-initargs-for-sheet :around  ((port motif-port)
						    (parent t)
						    (sheet motif-row-column-gadget-mixin))
  (let ((initargs (call-next-method))
	(columns
	 (ecase (gadget-orientation sheet)
	   (:vertical (or (gadget-columns sheet)
			  (and (silica::gadget-rows sheet)
			       (ceiling (length (sheet-children sheet))
					(silica::gadget-rows sheet)))))
	   (:horizontal (or (silica::gadget-rows sheet)
			    (and (silica::gadget-columns sheet)
				 (ceiling (length (sheet-children sheet))
					  (silica::gadget-columns sheet)))))))
	(spacing (silica::gadget-spacing sheet)))
    (when columns
      (setf (getf initargs :num-columns) columns))
    (when spacing
      (setf (getf initargs :spacing) spacing))
    initargs))

;;;

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet xm-viewport))
  'tk::xm-my-drawing-area)

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet xm-viewport))
  `(:scrolling-policy :application-defined
		      :margin-width 0 :margin-height 0
		      :resize-policy :none
		      :scroll-bar-display-policy :static))


;;;

(defclass motif-radio-box (motif-row-column-gadget-mixin
			   motif-geometry-manager
                           mirrored-sheet-mixin
                           sheet-multiple-child-mixin
                           sheet-permanently-enabled-mixin
                           radio-box
                           ask-widget-for-size-mixin
                           xt-pane)
    ())

(defmethod sheet-adopt-child :after ((gadget motif-radio-box) child)
  (setf (gadget-client child) gadget))

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-radio-box))
  'tk::xm-radio-box)

(defclass motif-check-box (motif-row-column-gadget-mixin
			   motif-geometry-manager
                           mirrored-sheet-mixin
                           sheet-multiple-child-mixin
                           sheet-permanently-enabled-mixin
                           check-box
                           ask-widget-for-size-mixin
                           xt-pane)
    ())

(defmethod sheet-adopt-child :after ((gadget motif-check-box) child)
  (setf (gadget-client child) gadget))

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-check-box))
  'tk::xm-row-column)

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet motif-check-box))
  '(:packing :column))


;; Frame-viewport that we need because a sheet can have

(defclass xm-frame-viewport-mixin (sheet-with-resources-mixin
				   sheet-single-child-mixin
                                   sheet-permanently-enabled-mixin
                                   wrapping-space-mixin
                                   mirrored-sheet-mixin
                                   basic-pane)
          ())

(defmethod silica::always-propagate-region-changes-p ((sheet xm-frame-viewport-mixin))
  t)

(defclass xm-frame-viewport (xm-frame-viewport-mixin) ())

(defmethod find-widget-class-and-name-for-sheet ((port xt-port)
						 (parent t)
						 (sheet xm-frame-viewport-mixin))
  'tk::xm-my-drawing-area)


(defmethod find-widget-initargs-for-sheet ((port xt-port)
					   (parent t)
					   (sheet xm-frame-viewport-mixin))
  ;;---  These are duplicated
  (list :margin-width 0
	:resize-policy :none
	:margin-height 0))

;(defmethod allocate-space ((fr xm-frame-viewport-mixin) width height)
;  ;;-- Is this what wrapping space mixin should do???
;  (move-and-resize-sheet (sheet-child fr) 0 0 width height))


(defclass motif-frame-pane (sheet-with-resources-mixin
			    motif-geometry-manager
                            mirrored-sheet-mixin
                            sheet-single-child-mixin
                            sheet-permanently-enabled-mixin
                            layout-mixin
                            basic-pane)
  (
   ;;-- this is ignored
   (thickness :initform nil :initarg :thickness)
   (shadow-type :initarg :shadow-type :initform nil)))

(defmethod initialize-instance :after ((pane motif-frame-pane) &key
                                                               frame-manager frame
                                                               contents)
  (if (typep contents 'mirrored-sheet-mixin)
      (sheet-adopt-child pane contents)
    (let ((viewport (with-look-and-feel-realization (frame-manager frame)
                      (make-pane 'xm-frame-viewport))))
      (sheet-adopt-child pane viewport)
      (sheet-adopt-child viewport contents))))


(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-frame-pane))
  'tk::xm-frame)

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet motif-frame-pane))
  (with-slots (shadow-type) sheet
    (and shadow-type `(:shadow-type ,shadow-type))))

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

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet basic-motif-scrolling-window))
  'xt::xm-scrolled-window)

;; List-pane

(defclass motif-list-pane (list-pane xt-leaf-pane)
          ())


(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-list-pane))
  'xt::xm-list)


(defmethod clim-internals::gadget-state ((gadget motif-list-pane))
  ;; spr 30639 (alemmens, 2005-11-30)
  (and (sheet-direct-mirror gadget)
       (list :top-item-position
             (tk::get-values (sheet-direct-mirror gadget) :top-item-position))))


(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet motif-list-pane))
  (with-accessors ((items set-gadget-items)
                   (value gadget-value)
                   (value-key set-gadget-value-key)
                   (test set-gadget-test)
                   (mode list-pane-mode)
                   (visible-items gadget-visible-items)
                   (name-key set-gadget-name-key)
                   (top-item-position silica::list-pane-top-item-position)) sheet
    (multiple-value-bind (selected-items m n)
	(compute-list-pane-selected-items sheet value)
      (let ((scroll-mode
	     (let ((p (sheet-parent sheet)))
	       (and (typep p 'motif-scroller-pane)
		    (scroller-pane-scroll-bar-policy p)))))
	`(:list-size-policy
	  ,(case scroll-mode
	     ;; specifying the list-size-policy as :variable is the
	     ;; only way of getting only vertical scroll-bars
	     (:vertical :variable)
	     (t :constant))
	  :scroll-bar-display-policy
	  ,(case scroll-mode
	     ((:horizontal :vertical t :both) :static)
	     (t :as-needed))
	  ,@(and selected-items
		 `(:selected-item-count ,(length selected-items)
		   :selected-items ,selected-items
		   :top-item-position ,(or top-item-position (1+ (min m n)))))
          ;; spr 30639: restore top-item-position (alemmens, 2005-11-30)
          ,@(and (not selected-items) top-item-position
                 `(:top-item-position ,top-item-position))
          ;;
	  :selection-policy
	  ,(ecase mode
	     (:exclusive :browse-select)
	     (:nonexclusive :multiple-select))
	  ,@(and visible-items `(:visible-item-count ,visible-items))
	  :items ,(mapcar name-key items)
	  :item-count ,(length items))))))



(defmethod gadget-visible-items ((gadget motif-list-pane))
  (let ((m (sheet-direct-mirror gadget)))
    (if m
	(tk::get-values m :visible-item-count)
      (call-next-method))))

(defmethod (setf set-gadget-items) :after (items (gadget motif-list-pane))
  ;;---- What should this do about selected items and the value etc etc
  (let ((m (sheet-direct-mirror gadget)))
    (when m
      (with-accessors ((name-key set-gadget-name-key)) gadget
	(tk::set-values m :items (mapcar name-key items) :item-count
			(length items))))))

(defmethod (setf list-pane-mode) :after (mode (gadget motif-list-pane))
  (let ((m (sheet-direct-mirror gadget)))
    (when m
      (tk::set-values m :selection-policy
		      (ecase mode
			(:exclusive :browse-select)
			(:nonexclusive :multiple-select))))))

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


(defmethod add-sheet-callbacks
    ((port motif-port) (sheet motif-list-pane) (widget xt::xm-list))
  (tk::add-callback widget :browse-selection-callback
		    'list-pane-single-selection-callback sheet)
  (tk::add-callback widget :multiple-selection-callback
		    'list-pane-multiple-selection-callback sheet))

(defmethod (setf gadget-value) :after
    (nv (l motif-list-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((mirror (sheet-direct-mirror l)))
    (when mirror
      (multiple-value-bind (selected-items m n)
          (compute-list-pane-selected-items l nv)
        (declare (ignore m n))
        (let ((position (tk::get-values mirror :top-item-position)))
          (with-no-value-changed-callbacks
              (tk::set-values mirror
                              :top-item-position position
                              :selected-item-count (length selected-items)
                              :selected-items selected-items)))))))

;;; Option buttons

(defclass motif-option-pane (option-pane motif-labelled-gadget xt-leaf-pane)
          ((buttons :accessor motif-option-menu-buttons)))

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-option-pane))
  'xt::xm-option-menu)

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet motif-option-pane))
  (let ((pdm (make-instance 'xt::xm-pulldown-menu :managed nil :parent parent)))
    (update-option-menu-buttons port sheet pdm)
    `(:sub-menu-id ,pdm :margin-height 0)))

(defun update-option-menu-buttons-1-batch (sheet pdm initargs items)
  (with-accessors ((printer silica::option-pane-printer)
                   (name-key set-gadget-name-key)
                   (value-key set-gadget-value-key)) sheet
    (mapcar #'(lambda (item)
                (let* ((name (funcall name-key item))
                       (pixmap (unless (or (null printer) (eq printer #'write-token))
                                 (pixmap-from-menu-item
                                  (sheet-parent sheet) name printer nil
                                  :text-style (pane-text-style sheet)
                                  :background (pane-background sheet)
                                  :foreground (pane-foreground sheet))))
                       (button
                        (if (null pixmap)
                            (apply #'make-instance 'tk::xm-push-button
                                   :label-string name
                                   :parent pdm
                                   initargs)
                            (apply #'make-instance 'tk::xm-push-button
                                   :label-pixmap pixmap
                                   :label-type :pixmap
                                   :parent pdm
                                   initargs))))
                  (tk::add-callback
                   button
                   :activate-callback
                   'option-menu-callback-function
                   (funcall value-key item)
                   sheet
                   pdm)
                  button))
            items)))

(defparameter *items-per-batch* 30)

(defun update-option-menu-buttons (port sheet pdm)
  (let ((initargs (find-widget-resource-initargs-for-sheet port sheet))
        (items (set-gadget-items sheet)))
    (setf (motif-option-menu-buttons sheet)
          (loop for size = (min (length items) *items-per-batch*)
                for batch = (update-option-menu-buttons-1-batch
                                   sheet pdm initargs (subseq items 0 size))
                append batch
                while (and (not (zerop size))
                           (= size *items-per-batch*))
                do (setf items (subseq items size))
                when items
                  do (let* ((sub-pdm (make-instance 'xt::xm-pulldown-menu
                                        :managed nil :parent pdm))
                            (button (apply #'make-instance 'tk::xm-cascade-button
                                           :label-string "More"
                                           :sub-menu-id sub-pdm
                                           :parent pdm
                                           initargs)))
		       ;; [bug18430]
		       (declare (ignore button))
                       (setf pdm sub-pdm))))))

(defmethod (setf set-gadget-items) :after (items (gadget motif-option-pane))
  (declare (ignore items))
  ;;---- What should this do about selected items and the value etc etc
  (let ((m (sheet-direct-mirror gadget)))
    (when m
      (let ((old-pdm (tk::get-values m :sub-menu-id))
	    (pdm (make-instance 'xt::xm-pulldown-menu
		   :managed nil :parent (tk::widget-parent m))))
	(update-option-menu-buttons (port gadget) gadget pdm)
	(tk::set-values m :sub-menu-id pdm)
	(tk::destroy-widget old-pdm)))))

(defun option-menu-callback-function (widget count value sheet pdm)
  (declare (ignore count))
  (let ((shell (tk::widget-parent (tk::widget-parent pdm))))
    (loop
      (unless (typep shell 'tk::xm-menu-shell)
        (return))
      (tk::unmap-widget shell)
      (setf shell (tk::widget-parent shell))))
  (queue-value-changed-event widget sheet value))

(defmethod (setf gadget-value) :after
    (nv (gadget motif-option-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-no-value-changed-callbacks
      (set-option-menu-value gadget nv)))

(defmethod realize-mirror :around ((port motif-port) (pane motif-option-pane))
  (let ((mirror (call-next-method)))
    (set-option-menu-value pane (gadget-value pane))
    (tk::set-values (tk::intern-widget (tk::xm_option_label_gadget mirror))
		    :font-list
		    (getf (find-widget-resource-initargs-for-sheet port pane)
			  :font-list))
    mirror))

(defun set-option-menu-value (sheet nv)
  (with-accessors ((items set-gadget-items)
                   (value-key set-gadget-value-key)
                   (test set-gadget-test)
                   (printer silica::option-pane-printer)
                   (name-key set-gadget-name-key)) sheet
    (when (sheet-direct-mirror sheet)
      (let* ((x (position nv items :test test :key value-key))
             (widget (sheet-direct-mirror sheet)))
        (when x
          (tk::set-values widget :menu-history (nth x (motif-option-menu-buttons sheet)))
          (let* ((name (funcall name-key (nth x items)))
                 (pixmap (unless (or (null printer) (eq printer #'write-token))
                           (pixmap-from-menu-item
                            (sheet-parent sheet) name printer nil
			    :text-style (pane-text-style sheet)
			    :background (pane-background sheet)
			    :foreground (pane-foreground sheet))))
                 (widget (tk::intern-widget (tk::xm_option_button_gadget widget))))
            (if pixmap
                (tk::set-values widget :label-pixmap pixmap :label-type :pixmap)
              (tk::set-values widget :label-string name))))))))

(defun warp-pointer-to-dialog-box (dialog framem x-position y-position)
  ;; The purpose of this function is to ensure that the dialog box becomes
  ;; selected in the case that the selected sheet must be under the pointer.
  ;; This is simply to make it easy to say "OK" to the dialog box without
  ;; having to go to a lot of trouble to select the window.
  (let ((port (port framem)))
    (when (eq (silica:port-input-focus-selection port) :sheet-under-pointer)
      (unless (and x-position y-position)
	;; Figure out where the left,top of the dialog box is located.
	(multiple-value-setq (x-position y-position)
	  (tk::get-values (tk::widget-parent dialog) :x :y))
	;; Translate to screen coordinates.
	(tk::with-ref-par ((childreturn 0 :int)
			   (xreturn 0 :int)
			   (yreturn 0 :int))
	  (x11:xtranslatecoordinates (port-display port)
				     (tk::widget-window dialog)
				     (tk::display-root-window (port-display port))
				     x-position y-position
				     &xreturn &yreturn &childreturn)
	  (setq x-position xreturn y-position yreturn)))
      ;; Decide where to put the pointer.
      (incf x-position 30)
      (incf y-position 60)
      ;; Do it.
      (x11:xwarppointer
       (port-display port)
       0				; src
       (tk::widget-window dialog)	; dest
       0				; src-x
       0				; src-y
       0				; src-width
       0				; src-height
       (fix-coordinate x-position)
       (fix-coordinate y-position)))))

(defvar *warp-pointer-to-dialogs* nil)

(defmethod frame-manager-notify-user ((framem motif-frame-manager)
                                      message-string
                                      &key
                                      text-style
				      background
				      foreground
                                      (style :inform)
                                      (frame nil frame-p)
                                      (associated-window
				       (if frame-p
					   (frame-top-level-sheet frame)
					 (graft framem)))
                                      (title "Notify user")
                                      documentation
                                      (exit-boxes '(:exit :abort :help))
                                      (name :notify-user)
				      x-position
				      y-position
				      (warp-pointer *warp-pointer-to-dialogs*)
				      (abort-on-cancel-p nil))
  (let* ((initargs (find-widget-resource-initargs-for-sheet
		    (port framem) associated-window
		    :foreground foreground :background background
		    :text-style text-style))
	 (font-list (getf initargs :font-list))
	 (pixmap (when (typep style '(or string pixmap))
		   (prog1 style
		     (setq style :message))))
	 (dialog (apply #'make-instance (ecase style
					 (:inform 'tk::xm-information-dialog)
					 (:error 'tk::xm-error-dialog)
					 (:question 'tk::xm-question-dialog)
					 (:warning 'tk::xm-warning-dialog)
					 (:message 'tk::xm-message-dialog))
		       :dialog-style *xm-dialog-style*
		       :managed nil
		       :parent (if (typep associated-window 'xt::xt-root-class)
				   associated-window
				 (sheet-mirror associated-window))
		       :name name
		       :dialog-title title
		       :message-string message-string
		       :label-font-list font-list
		       :button-font-list font-list
		       :text-font-list font-list
		       (append
			(and pixmap `(:symbol-pixmap ,pixmap))
			initargs)))
	 (result nil))
    (multiple-value-bind
        (ok-button cancel-button help-button)
        (get-message-box-child dialog :ok :cancel :help)
      (flet ((set-it (widget r)
               (declare (ignore widget))
               (setq result (list r))))
        (tk::add-callback dialog :ok-callback #'set-it t)
        (tk::add-callback dialog :cancel-callback #'set-it nil)

	(process-exit-boxes framem
			    associated-window
			    documentation exit-boxes ok-button cancel-button
			    help-button dialog)

        (catch 'notify-user
	  (unwind-protect
	      (progn
		(tk::set-values (tk::widget-parent dialog) :mapped-when-managed nil)
		(tk::manage-child dialog)
		(when (and x-position y-position)
		  (tk::set-values dialog :x x-position :y y-position))

		(tk::set-values (tk::widget-parent dialog) :mapped-when-managed t)

		(when warp-pointer
		  (warp-pointer-to-dialog-box dialog framem x-position y-position))

		(with-toolkit-dialog-component (notify-user (list message-string :style))
		  (wait-for-callback-invocation
		   (port framem)
		   #'(lambda () (or result (not (tk::is-managed-p dialog))))
		   "Waiting for dialog")))
	    (tk::destroy-widget dialog))
	  (or (car result)
	      (and abort-on-cancel-p
		   (abort))))))))

(defun process-exit-boxes (framem
			   associated-window
			   documentation exit-boxes ok-button cancel-button
			   help-button &optional dialog)

  (flet ((display-help (widget ignore)
	   (declare (ignore widget ignore))
	   (frame-manager-notify-user
	    framem
	    documentation
	    :associated-window associated-window))
	 (set-button-state (name button type)
	   (let ((box (dolist (x exit-boxes nil)
			(cond ((eq x name) (return x))
			      ((atom x))
			      ((eq (car x) name)
			       (tk::set-values button :label-string (second x))
			       (return x))))))
	     (cond ((null box)
		    (tk::unmanage-child button))
		   ((and dialog
			 (eq box (car exit-boxes)))
		    (tk::set-values dialog :default-button-type type))))))
    (set-button-state :exit ok-button :ok)
    (set-button-state :abort cancel-button :cancel)
    (set-button-state :help help-button :help)

    (if documentation
	(tk::add-callback help-button :activate-callback #'display-help)
      (xt::set-sensitive help-button nil))))

(defun get-message-box-child (widget &rest children)
  (values-list
   (mapcar #'(lambda (child)
               (tk::convert-resource-in
                widget
                'tk::widget
                (tk::xm-message-box-get-child
                 widget
                 (tk::encode-box-child child))))
           children)))

(defun get-selection-box-child (widget &rest children)
  (values-list
   (mapcar #'(lambda (child)
               (tk::convert-resource-in
                widget
                'tk::widget
                (tk::xm-selection-box-get-child
                 widget
                 (tk::encode-box-child child))))
           children)))

;;;; Working

;;; File Selection

(defmethod frame-manager-select-file
    ((framem motif-frame-manager) &key (frame nil frame-p)
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
				  (name :select-file)
				  directory
				  pattern
				  default
				  background
				  foreground
				  text-style
				  x-position
				  y-position)

  (let* ((initargs (find-widget-resource-initargs-for-sheet
		    (port framem) associated-window
		    :foreground foreground :background background
		    :text-style text-style))
	 (font-list (getf initargs :font-list))
	 (dialog (apply #'make-instance
		       'tk::xm-file-selection-dialog
		       :dialog-style *xm-dialog-style*
		       :managed nil
		       :parent (sheet-mirror associated-window)
		       :name name
		       :dialog-title title
		       :label-font-list font-list
		       :button-font-list font-list
		       :text-font-list font-list
		       initargs))
	 (result nil))

    (multiple-value-bind
        (ok-button cancel-button help-button)
        (get-selection-box-child dialog :ok :cancel :help)
      (when directory
	(tk::set-values dialog :directory directory))

      (when pattern
	(tk::set-values dialog :pattern pattern))

      (when default
	(tk::set-values dialog :dir-spec default))

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
	(let ((str-pntr (xt::xm_string_create_l_to_r
			 (tk::get-values dialog :dir-mask)
			 "")))
	  (tk::xm_file_selection_do_search
	   dialog str-pntr)
	  (tk::add-widget-cleanup-function dialog
					   #'tk::destroy-generated-xm-string
					   str-pntr)))

      (process-exit-boxes framem
			  associated-window
			  documentation exit-boxes ok-button cancel-button
			  help-button)

      (flet ((set-it (widget r)
	       (declare (ignore widget))
	       (setq result (list r))))
	(tk::add-callback dialog :ok-callback #'set-it t)
	(tk::add-callback dialog :cancel-callback #'set-it nil)

	(unwind-protect
	    (progn
	      (tk::set-values (tk::widget-parent dialog) :mapped-when-managed nil)
	      (tk::manage-child dialog)
	      (when (and x-position y-position)
		(tk::set-values dialog :x x-position :y y-position))
	      (tk::set-values (tk::widget-parent dialog) :mapped-when-managed t)
	      (wait-for-callback-invocation
	       (port associated-window)
	       #'(lambda () (or result (not (tk::is-managed-p dialog))))
	       "Waiting for dialog")
	      (if (car result)
		  (tk::get-values dialog :dir-spec :directory)))
	  (tk::destroy-widget dialog))))))

(ff::defun-foreign-callable file-search-proc-callback ((widget :foreign-address)
						       (cb :foreign-address))
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

(defvar *file-search-proc-callback-address* (ff:register-foreign-callable 'file-search-proc-callback))

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


(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-paned-pane))
  'xt::xm-paned-window)

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet motif-paned-pane))
  '(:margin-height 0 :margin-width 0))


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

(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-rc-pane))
  'xt::xm-row-column)

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




(defmethod find-widget-class-and-name-for-sheet ((port motif-port)
						 (parent t)
						 (sheet motif-form-pane))
  'xt::xm-form)

(defmethod find-widget-initargs-for-sheet ((port motif-port)
					   (parent t)
					   (sheet motif-form-pane))
  (with-slots (fraction-base) sheet
    (and fraction-base (list :fraction-base fraction-base))))


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

(defclass motif-scroller-pane (scroller-pane basic-motif-scrolling-window)
          ())

(defmethod initialize-instance :after ((sp motif-scroller-pane) &key
                                                                scroll-bars contents
                                                                frame-manager frame)
  (if (setf (scroller-pane-gadget-supplies-scrolling-p sp)
        (gadget-supplies-scrolling-p contents))
      (sheet-adopt-child sp contents)
    (with-look-and-feel-realization (frame-manager frame)
      (when (member scroll-bars '(t :both :dynamic :vertical))
        (let ((sb (make-pane 'scroll-bar
                             :orientation :vertical :id :vertical
                             :client sp)))
          (setf (scroller-pane-vertical-scroll-bar sp) sb)
          (sheet-adopt-child sp sb)))
      (when (member scroll-bars '(t :both :dynamic :horizontal))
        (let ((sb (make-pane 'scroll-bar
                             :orientation :horizontal :id :horizontal
                             :client sp)))
          (setf (scroller-pane-horizontal-scroll-bar sp) sb)
          (sheet-adopt-child sp sb)))
      (sheet-adopt-child sp (setf (slot-value sp 'viewport)
                              (make-pane 'viewport :scroller-pane sp)))
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

;;; JPM: Note Tab is considered the completion character, and
;;; this code prevents it from being used as such.
(defmethod discard-accelerator-event-p ((port motif-port) event)
  (or (call-next-method)
      ;;-- There are a whole bunch of other keysyms that need to be
      ;;-- ignored too but there does not appear to be an easy way
      ;;-- of going from the osf name to the keysym that we need to ignore
      ;;--  osfMenuBar
      (member (keyboard-event-key-name event) '(:f10 #+ignore :tab))))


;; Support for help

(defmethod add-sheet-callbacks :after
           ((port motif-port) (sheet gadget) (widget xt::xm-primitive))
  (add-help-callback sheet widget))

(defmethod add-sheet-callbacks :after
           ((port motif-port) (sheet gadget) (widget xt::xm-manager))
  (add-help-callback sheet widget))

(defun add-help-callback (sheet widget)
  (when (silica::gadget-help-callback sheet)
    (tk::add-callback widget
		      :help-callback
		      'help-callback-function
		      sheet)))

(defun help-callback-function (widget sheet)
  (declare (ignore widget))
  (distribute-event
   (port sheet)
   (allocate-event 'gadget-help-event
		   :gadget sheet)))

(defclass gadget-help-event (gadget-event)
  ())

(defmethod handle-event ((sheet basic-gadget) (event gadget-help-event))
  (let ((help (silica::gadget-help-callback sheet)))
    (etypecase help
      (string (display-motif-help sheet (frame-manager sheet) help))
      (cons (apply (car help) sheet (cdr help))))))


(defun clim-internals::make-help-from-presentation-type (stream presentation-type)
  ;;-- We have to do it this way because describe methods might invoke
  ;;-- presentations etc etc
  (accepting-values (stream :exit-boxes '(:exit)
			    :label "Help"
			    :own-window t)
    (describe-presentation-type presentation-type stream)))


(defmethod silica::port-set-pane-text-style ((port motif-port) pane m text-style)
  (declare (ignore pane))
  (when (typep m 'xt::xt-root-class)
    (tk::set-values m :font-list (text-style-mapping port text-style))))

(defmethod silica::port-set-pane-background ((port motif-port) pane m (ink color))
  (when (typep m 'xt::xt-root-class)
    (with-sheet-medium (medium pane)
      ;; we save and restore the foreground because xm_change_color
      ;; will recalculate the foreground as well as the shadow colors
      ;; and we want the latter but not the former (cim 9/21/95)
      (let ((fg (tk::get-values m :foreground)))
	(tk::xm_change_color m (decode-color ink medium))
	(tk::set-values m :foreground fg)))))

(defmethod silica::port-set-pane-background :after
	   ((port motif-port) pane (m tk::xm-cascade-button) color)
  (let ((sub-menu (tk::get-values m :sub-menu-id)))
    (silica::port-set-pane-background port pane sub-menu color)))

(defmethod silica::port-set-pane-foreground :after
	   ((port motif-port) pane (m tk::xm-cascade-button) color)
  (let ((sub-menu (tk::get-values m :sub-menu-id)))
    (silica::port-set-pane-foreground port pane sub-menu color)))

(excl:ics-target-case
(:+ics

(defmethod gadget-needs-font-set-p ((sheet t))
  nil)

(defmethod gadget-needs-font-set-p ((sheet motif-text-field))
  t)

(defmethod gadget-needs-font-set-p ((sheet motif-text-editor))
  t)

)) ;; ics-target-case

(defmethod find-widget-resource-initargs-for-sheet :around
    ((port motif-port) (sheet t) &key text-style)
  (let* ((initargs (call-next-method))
	 (text-style (or text-style
			 (sheet-text-style port sheet)))
	 (font-list (text-style-mapping port text-style *all-character-sets*)))
    (excl:ics-target-case
     (:+ics (when (gadget-needs-font-set-p sheet)
	     (setq font-list
	       (font-set-from-font-list port font-list)))))
    `(:font-list ,font-list ,@initargs)))

(defmethod frame-manager-print-file
    ((framem motif-frame-manager) filename
     &key
     (frame nil frame-p)
     (associated-window
      (if frame-p
	  (frame-top-level-sheet frame)
	(graft framem)))
     &allow-other-keys)
  (declare (ignore filename associated-window))
  (error "not yet implemented for UNIX"))

#+(version>= 5 0)
(in-package :tk)

#+(version>= 5 0)
(defun reinitialize-silica-callbacks ()
  (xm-silica::setup-mda)
  (setq xm-silica::*file-search-proc-callback-address*
    (ff:register-foreign-callable
     'xm-silica::file-search-proc-callback :reuse)))

(in-package :tk-silica)

;;; spr35138: Expose the frame resizing functions as methods on the
;;; top level sheet of the frame.
(defmethod clim:move-sheet ((sheet tk-silica::motif-top-level-sheet)
                              x y)
    (with-slots ((frame silica::frame)) sheet
       (let ((port (slot-value (clim:frame-manager sheet) 'silica::port)))
         (cond
           ((and (clim-internals::sheet-enabled-p sheet)
                 (not (clim-internals::frame-resizable frame)))
            (silica:port-move-frame port frame x y)
            (setf (sheet-transformation sheet)
                  (make-translation-transformation x y)))
           (t (call-next-method))))))

(defmethod clim:resize-sheet ((sheet motif-top-level-sheet) width height)
  (with-slots ((frame silica::frame)) sheet
     (let ((port (slot-value (frame-manager sheet) 'silica::port)))
       (if (and (clim-internals::sheet-enabled-p sheet)
                (not (clim-internals::frame-resizable frame)))
           (silica:port-resize-frame port frame width height)
           (call-next-method)))))
