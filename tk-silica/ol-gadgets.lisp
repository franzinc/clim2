;; See the file LICENSE for the full license governing this code.
;;

(in-package :xm-silica)

(defmethod make-pane-class ((framem openlook-frame-manager) class &rest options)
  (declare (ignore options))
  (second (assoc class '(
			 ;; An experiment
			 (outlined-pane openlook-frame-pane)
			 ;;
			 (scroll-bar openlook-scroll-bar)
			 (slider openlook-slider)
			 (push-button openlook-push-button)
			 (label-pane openlook-label-pane)
			 (text-field openlook-text-field)
			 (text-editor openlook-text-editor)
			 (toggle-button openlook-toggle-button)
			 (menu-bar openlook-menu-bar)
			 (viewport ol-viewport)
			 (radio-box openlook-radio-box)
			 (check-box openlook-check-box)
			 (frame-pane openlook-frame-pane)
			 (top-level-sheet openlook-top-level-sheet)
			 (list-pane openlook-list-pane)
			 (option-pane openlook-option-pane)))))


;;;;;;;;;;;;;;;;;;;;

(defclass openlook-scroll-bar (scroll-bar
			       xt-oriented-gadget
			       xt-leaf-pane)
	  ())

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-scroll-bar))
  'tk::scrollbar)

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-scroll-bar))
  (let ((size (scroll-bar-size sheet)))
    (append '(:slider-min 0 :slider-max 1000)
	    (when size
	      `(:proportion-length
		,(convert-scroll-bar-value-out sheet
					       size))))))

(defmethod change-scroll-bar-values ((sb openlook-scroll-bar) &key
							      slider-size
							      value line-increment)
  (let ((mirror (sheet-direct-mirror sb)))
    (when mirror
      (multiple-value-bind
	(real-value real-size line-increment)
	  (compute-new-scroll-bar-values sb value slider-size line-increment)
      (tk::set-values mirror
		      :granularity line-increment
		      :proportion-length  real-size
		      :slider-value real-value)))))

(defmethod add-sheet-callbacks :after  ((port openlook-port) (sheet openlook-scroll-bar) (widget t))
  (tk::add-callback widget
		    :slider-moved
		    'ol-scroll-bar-callback
		    sheet))

(defun ol-scroll-bar-callback (widget value sheet)
  (declare (ignore widget))
  (value-changed-callback
   sheet
   (gadget-client sheet)
   (gadget-id sheet)
   (convert-scroll-bar-value-in sheet value)))

(defmethod gadget-value ((gadget openlook-scroll-bar))
  (let ((mirror (sheet-direct-mirror gadget)))
    (if mirror
	(convert-scroll-bar-value-in gadget
				     (tk::get-values mirror :slider-value))
      (call-next-method))))

(defmethod (setf gadget-value) :after
	   (nv (gadget openlook-scroll-bar) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((mirror (sheet-direct-mirror gadget)))
    (when mirror
      (tk::set-values mirror
		      :slider-value (convert-scroll-bar-value-out gadget nv)))))

(defmethod scroll-bar-size ((sheet openlook-scroll-bar))
  (let ((mirror (sheet-direct-mirror sheet)))
    (if mirror
	(convert-scroll-bar-value-in sheet
				     (tk::get-values mirror :proportion-length))
      (call-next-method))))

(defmethod (setf scroll-bar-size) :after (nv (sheet openlook-scroll-bar))
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror
      (tk::set-values mirror
		      :proportion-length
		      (convert-scroll-bar-value-out sheet nv)))))

(defmethod compose-space ((m openlook-scroll-bar) &key width height)
  (declare (ignore width height))
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

;;; Ol DrawArea Widgets require all of this

(ff:defun-c-callable ol-ignore-help-function (id-type id src-x src-y)
  (declare (ignore id-type id src-x src-y))
  nil)

(defvar *ol-ignore-help-function-address* (ff:register-foreign-callable 'ol-ignore-help-function))

(defmethod add-sheet-callbacks :after  ((port openlook-port) (sheet t) (widget tk::draw-area))
  (tk::add-callback widget
		    :expose-callback
		    'sheet-mirror-exposed-callback
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
			 1
			 'sheet-mirror-event-handler
			 sheet)
  (tk::ol_register_help
   tk::ol_widget_help
   widget
   "foo"
   tk::ol_transparent_source
   *ol-ignore-help-function-address*
   ))

;;; top level sheet

(defclass openlook-top-level-sheet (xt-top-level-sheet) ())

(defmethod add-sheet-callbacks :after ((port openlook-port)
				       (sheet openlook-top-level-sheet)
				       widget)
  (tk::add-callback widget
		    :resize-callback 'sheet-mirror-resized-callback
		    sheet))

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-top-level-sheet))
  'tk::draw-area)

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-top-level-sheet))
  (list :layout :ignore))

;;

(defmethod find-widget-class-and-name-for-sheet
    ((port openlook-port) (parent t) (sheet standard-sheet))
  'tk::draw-area)

(defmethod find-widget-initargs-for-sheet
    ((port openlook-port) (parent t) (sheet standard-sheet))
  (list :layout :ignore))


;;

(defclass openlook-frame-pane (sheet-single-child-mixin
			       sheet-permanently-enabled-mixin
			       wrapping-space-mixin
			       basic-pane)
	  ())

(defmethod initialize-instance :after ((fr openlook-frame-pane) &key
								frame frame-manager contents thickness
								background)
  (declare (ignore frame frame-manager thickness)
	   (ignore background))
  (sheet-adopt-child fr contents))

;; OpenLook viewport

(defclass ol-viewport
	  (mirrored-sheet-mixin
	   viewport)
    ())

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet ol-viewport))
  'tk::draw-area)

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet ol-viewport))
  '(:layout :ignore))

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

(defclass openlook-menu-bar (xt-leaf-pane xt-menu-bar menu-bar) ())

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-menu-bar))
  'tk::draw-area)

(defmethod compose-space ((mb openlook-menu-bar) &key width height)
  (declare (ignore height))
  (multiple-value-bind (max-width height)
        (map-over-menubar mb width)
    (make-space-requirement :width max-width :height height)))

(defmethod allocate-space ((mb openlook-menu-bar) width height)
  (declare (ignore height))
  (flet ((configure-child (child x y width height)
	   (tk::configure-widget child :x x :y y :width width :height height)))
    (declare (dynamic-extent #'configure-child))
    (map-over-menubar mb width #'configure-child)))

(defun map-over-menubar (mb width &optional fn)
  (let ((mirror (sheet-direct-mirror mb)))
    (let ((h-pad 4) (v-pad 4) (h-space 4) (v-space 4))
      ;; For some reason the mirror is a drawing area
      ;; so we cannot get the resources
      (let ((children (tk::widget-children mirror))
	    (y v-pad)
	    (max-width 0))
	(loop
	  (when (null children) (return nil))
	  (let ((x h-space)
		(max-height 0)
		(first t))
	    ;; Iterate over one row until we run out of space
	    (loop
	      (when (null children) (return nil))
	      (let ((child (car children)))
		(when (xt::is-managed-p child)
		  (multiple-value-bind
		      (ignore-x igore-y w h)
		      (xt::widget-best-geometry child)
		    (declare (ignore ignore-x igore-y))
		    (unless first (incf x v-space))
		    (when width
		      (unless (or first (<= (+ w x) (- width h-space)))
			(return nil)))
		    (when fn (funcall fn child x y w h))
		    (incf x w)
		    (maxf max-height h))
		  (setq first nil))
		(pop children)))
	    (maxf max-width (+ x h-pad))
	    (incf y max-height)
	    (when children (incf y v-space))))
	(incf y v-space)
	(values max-width y)))))



(defmethod realize-mirror :around ((port openlook-port) (sheet openlook-menu-bar))

  ;; This code fills the menu-bar. If top level items do not have
  ;; submenus then it creates one with a menu of its own

  (let* ((mirror (call-next-method))
	 (initargs (remove-keywords
		    (find-widget-resource-initargs-for-sheet port sheet)
		    '(:font)))
	 (menu-text-style (pane-text-style sheet))
	 (frame (pane-frame sheet)))
    (labels ((compute-options (item)
	       (let ((item-text-style (getf (command-menu-item-options item)
					    :text-style)))
		 (list* :font
			(text-style-mapping
			 port
			 (if item-text-style
			     (merge-text-styles item-text-style menu-text-style)
			   menu-text-style))
			initargs)))
	     (update-menu-item-sensitivity (widget frame commands)
	       (declare (ignore widget))
	       (dolist (cbs commands)
		 (tk::set-sensitive (second cbs)
				    (command-enabled
				     (car (second (car cbs)))
				     frame))))

	     (make-command-for-command-table-1 (mb item)
	       (let* ((menu-pane (tk::get-values mb :menu-pane))
		      (ct (find-command-table (second item)))
		      (tick (slot-value ct 'clim-internals::menu-tick))
		      (commands-and-buttons
		       (make-menu-for-command-table
			ct
			menu-pane
			nil)))

		 (tk::set-values menu-pane :background (getf initargs :background))

		 (when commands-and-buttons
		   ;; We might have add-callbacks for a different set
		   ;; of children which are now destroyed
		   (let ((shell (do ((widget menu-pane (tk::widget-parent widget)))
				    ((typep widget 'tk::shell)
				     widget)
				  (assert widget))))

		     #-ignore
		     (setf (tk::widget-create-popup-child-proc shell)
		       #'(lambda (shell)
			   (declare (ignore shell))
			   (let ((children
				  (tk::widget-children menu-pane)))
			     (when (or (null children)
				       (/= tick
					   (setq tick
					     (slot-value ct 'clim-internals::menu-tick))))
			       (tk::unmanage-children children)
			       #+should-we-do-this-since-olit-has-problems
			       (mapc #'tk::destroy-widget children)
			       (make-command-for-command-table-1 mb item)))))

		     (tk::remove-all-callbacks shell :popup-callback)
		     (tk::add-callback shell :popup-callback
				       #'update-menu-item-sensitivity
				       (pane-frame sheet)
				       commands-and-buttons)))

		 (set-button-mnemonic
		  sheet mb (getf
			    (command-menu-item-options item) :mnemonic))))

	     (make-menu-for-command-table (command-table parent top)
	       (let ((commands-and-buttons nil))
		 (map-over-command-table-menu-items
		  #'(lambda (menu keystroke item)
		      (let ((type (command-menu-item-type item)))
			(case type
			  (:divider
			   (unless top
			     (apply #'make-instance 'xt::static-text
					    :parent parent
					    :string (ecase (command-menu-item-value item)
						      ((nil :line) " ")
						      (:label menu))
					    (compute-options item))))
			  (:function
			   ;;--- Do this sometime
			   )
			  (:menu
			   (make-command-for-command-table-1
			    (let ((button
				   (apply #'make-instance 'tk::menu-button
					  :parent parent
					  :label menu
					  (compute-options item))))
			      (add-documentation-callbacks
			       frame button (getf (command-menu-item-options item) :documentation))
			      button)
			    item))

			  (t
			   (let ((button
				  (apply #'make-instance 'tk::oblong-button
						 :label menu
						 :managed t
						 :parent parent
						 :sensitive (command-enabled
							     (car (command-menu-item-value item))
							     (slot-value sheet 'silica::frame))
						 (compute-options item))))

			     (add-documentation-callbacks
			      frame button
			      (getf (command-menu-item-options item) :documentation))

			     (when top
			       (push (cons (car (command-menu-item-value item)) button)
				     (menu-bar-command-name-to-button-table sheet)))

			     (set-button-accelerator-from-keystroke
			      sheet button keystroke)

			     (set-button-mnemonic
			      sheet
			      button (getf (command-menu-item-options item) :mnemonic))

			     (push (list item button) commands-and-buttons)

			     (tk::add-callback
			      button
			      :select
			      'command-button-callback-ol
			      (slot-value sheet 'silica::frame)
			      command-table
			      item))))))
		  command-table)
 		 commands-and-buttons)))
      (make-menu-for-command-table
       (or (menu-bar-command-table sheet)
	   (frame-command-table (pane-frame sheet)))
       mirror
       t))
    mirror))

(defmethod set-button-accelerator-from-keystroke ((menubar openlook-menu-bar) button keystroke)
  (when keystroke
    (let ((modifiers (cdr keystroke)))
      (when modifiers (record-accelerator menubar keystroke))
      (multiple-value-bind (accel accel-text)
	  (get-accelerator-text keystroke t)
	(dolist (modifier modifiers)
	  (setq accel-text
	    (concatenate 'string
	      (ecase modifier
		(:control "Ctrl+")
		(:meta "Alt+")
		(:super "Super+")
		(:hyper "Hyper+"))
	      accel-text))
	  (setq accel
	    (concatenate 'string
	      (ecase modifier
		(:control "c")
		(:meta "a")
		(:super "Mod2")
		(:hyper "Mod3"))
	      accel)))
	(if modifiers
	    (tk::set-values button
			    :accelerator accel
			    :accelerator-text accel-text)
	  (tk::set-values button
			  :accelerator-text accel-text))))))

(defun command-button-callback-ol (button frame command-table item)
  (command-button-callback button nil frame command-table item))


;;; Label pane

(defclass openlook-label-pane (label-pane xt-leaf-pane)
	  ())

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-label-pane))
  'tk::static-text)

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-label-pane))
  (with-accessors ((label gadget-label)
		   (alignment gadget-alignment)) sheet
    (append (list :alignment
		  (ecase alignment
		    ((:left nil) :left)
		    (:center :center)
		    (:right :right)))
	    ;;-- icon label
	    (make-label-initarg label :string))))

(defmethod compose-space ((sheet openlook-label-pane) &key width height)
  (declare (ignore width height))
  (let ((label (gadget-label sheet)))
    (cond
     ((typep label 'xt::pixmap)
      (make-space-requirement :width (xt::pixmap-width label)
			      :height (xt::pixmap-height label)))
     ((typep label 'pattern)
      (make-space-requirement :width (pattern-width label)
			      :height (pattern-height label)))
     (t
      (call-next-method)))))

(defun make-label-initarg (label initarg)
  (etypecase label
    (null)
    (string (list initarg label))
    (xt::pixmap (list :background-pixmap label))))

(defmethod (setf gadget-label) :after (new-value (sheet openlook-label-pane))
  (let ((m (sheet-direct-mirror sheet)))
    (when m
      (tk::set-values m :string new-value))))
;;; Push button

(defclass openlook-push-button (push-button
				openlook-labelled-gadget
				openlook-action-pane
				xt-leaf-pane)
	  ())



(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-push-button))
  'tk::oblong-button)

;;


(defclass openlook-action-pane () ())

(defmethod add-sheet-callbacks :after ((port openlook-port) (sheet openlook-action-pane) (widget t))
  (tk::add-callback widget
		    :select
		    'queue-active-event-ol
		    sheet))

(defmethod queue-active-event-ol (widget (sheet openlook-action-pane))
  (declare (ignore widget))
  (distribute-event
   (port sheet)
   (allocate-event 'activate-gadget-event
     :gadget sheet)))


;;; Text field

(defclass openlook-text-field (text-field xt-leaf-pane)
	  ())

(defmethod initialize-mirror :after ((port openlook-port)
				     (parent t)
				     (parent-widget t)
				     (sheet openlook-text-field)
				     (widget t))
  (unless (gadget-editable-p sheet)
    (let ((te (tk-silica::openlook-text-field-edit-widget sheet widget)))
      (tk::set-values te :edit-type :text-read))))

(defmethod (setf gadget-editable-p) :after (nv (tf openlook-text-field))
  (let ((m (sheet-direct-mirror tf)))
    (when m (tk::set-values
	     (tk-silica::openlook-text-field-edit-widget tf m)
	     :edit-type (if nv :text-edit :text-read)))))

;;-- Extreme hack alert

(defmethod compose-space ((tf openlook-text-field) &key width height)
  (declare (ignore width height))
  (let ((m (sheet-direct-mirror tf)))
    (if (gadget-editable-p tf)
	(multiple-value-bind (font chars-visible)
	    (tk::get-values m :font :chars-visible)
	  (make-space-requirement :width (* chars-visible (tk::font-width font))
				  :height (+ 7 (tk::font-height font))))
      (multiple-value-bind (string font)
	  (tk::get-values m :string :font)
	(make-space-requirement :width (* (length string) (tk::font-width font))
				:height (+ 7 (tk::font-height font)))))))

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-text-field))
  'tk::text-field)


(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-text-field))
  (with-accessors ((value gadget-value)
		   (editable gadget-editable-p)
		   (foreground pane-foreground)) sheet
    (append (and foreground
		 (with-sheet-medium (medium sheet)
		   `(:font-color
		     ,(cadr (decode-gadget-foreground medium sheet foreground)))))
	    (and value `(:string ,value))
	    `(:chars-visible ,(max (length value) 10)))))

(defun openlook-text-field-edit-widget (tf &optional (mirror (sheet-direct-mirror tf)))
  (tk::get-values mirror :text-edit-widget))

;; we can't rely on OpenLook to genarate focus-out events in any
;; consistent manner. So instead we queue a losing focus event each
;; time the value changes. This is merly to  ensure that the
;; accepting-values field is kept up to date (cim 3/17/94)

(defun ol-text-field-value-changed (widget sheet)
  (queue-value-changed-event widget sheet)
  (queue-losing-focus-event widget sheet))

(defmethod add-sheet-callbacks :after ((port openlook-port)
				       (sheet openlook-text-field)
				       (widget t))
  (when (gadget-editable-p sheet)
    (let ((edit-widget (openlook-text-field-edit-widget sheet widget)))
      (tk::add-callback edit-widget
			:post-modify-notification
			'ol-text-field-value-changed
			sheet)
      #+ignore
      (tk::add-event-handler edit-widget
			     '(:focus-change)
			     1
			     'openlook-text-field-event-handler
			     sheet))))


#+ignore
(defun openlook-text-field-event-handler (widget event sheet)
  (case (tk::event-type event)
    (:focus-in (queue-gaining-focus-event widget sheet))
    (:focus-out (queue-losing-focus-event widget sheet))))


(defmethod gadget-value ((gadget openlook-text-field))
  (let ((m (sheet-direct-mirror gadget)))
    (if m
	(if (gadget-editable-p gadget)
	    (text-editor-text (openlook-text-field-edit-widget gadget))
	  (tk::get-values m :string))
      (call-next-method))))


(defmethod gadget-current-selection ((tf openlook-text-field))
  (and (sheet-direct-mirror tf)
       (text-editor-selection (openlook-text-field-edit-widget tf))))

(defmethod (setf gadget-value) :after
	   (nv (gadget openlook-text-field) &key invoke-callback)
  (declare (ignore invoke-callback))
  (when (sheet-direct-mirror gadget)
    (with-no-value-changed-callbacks
	(if (gadget-editable-p gadget)
	    (setf (text-editor-text (openlook-text-field-edit-widget gadget)) nv)
	  (tk::set-values (sheet-direct-mirror gadget) :string nv)))))

;;--- We need to implement the activate callback stuff so that when
;;--- the user hits return we invoke the callback. I guess we need to
;;-- look at whats been inserted.


;;;

(defclass openlook-labelled-gadget () ())

(defmethod find-widget-initargs-for-sheet
    :around ((port openlook-port)
	     (parent t)
	     (sheet openlook-labelled-gadget))
  (let ((initargs (call-next-method)))
    (with-accessors ((alignment gadget-alignment)
		     (label gadget-label)) sheet
      (etypecase label
	((or null string)
	 (unless (getf initargs :label)
	   (setf (getf initargs :label) label))
	 (unless (getf initargs :label-justify)
	   (setf (getf initargs :label-justify)
	     (ecase alignment
	       (:center :left)
	       ((:left :right)  alignment)))))
	((or tk::pixmap pattern)
	 (unless (getf initargs :label-image)
	   (when (typep label 'pattern)
	     (with-sheet-medium (medium sheet)
	       (setq label
		 (pixmap-from-pattern label medium :pixmap))))
	   (setf (getf initargs :label-image)
	     (tk::get-image label :format x11:xypixmap))
	   (setf (getf initargs :label-type) :image)))))
    initargs))

(defmethod (setf gadget-label) :after (nv (sheet openlook-labelled-gadget))
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) :label (or nv ""))))

(defmethod (setf gadget-alignment) :after (nv (sheet openlook-labelled-gadget))
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet)
		    :label-justify (ecase nv
				     (:center nv)
				     ((:left :right) nv))) ))

;; Toggle button

(defclass openlook-toggle-button (toggle-button
				  openlook-labelled-gadget
				  xt-leaf-pane)
	  ())

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-toggle-button))
  (typecase (sheet-parent sheet)
    (openlook-radio-box 'xt::rect-button)
    (openlook-check-box 'xt::rect-button)
    (t 'xt::check-box)))

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-toggle-button))
  (with-accessors ((set gadget-value)
		   (indicator-type gadget-indicator-type)
		   (label gadget-label)) sheet
    (append `(:set ,set
		   :position :right)
	    #+dunno
	    (list :indicator-type
		  (ecase indicator-type
		    (:one-of :one-of-many)
		    (:some-of :n-of-many))))))

;; check-box, rect  select, unselect callback, :set resource

(defmethod add-sheet-callbacks :after ((port openlook-port)
				       (sheet openlook-toggle-button)
				       (widget t))
  (tk::add-callback widget
		    :select
		    'queue-value-changed-event
		    sheet)
  (tk::add-callback widget
		    :unselect
		    'queue-value-changed-event
		    sheet))

(defmethod gadget-value ((gadget openlook-toggle-button))
  (if (sheet-direct-mirror gadget)
      (tk::get-values (sheet-mirror gadget) :set)
    (call-next-method)))

(defmethod (setf gadget-value) :after
	   (nv (gadget openlook-toggle-button) &key invoke-callback)
  (declare (ignore invoke-callback))
  (when (sheet-direct-mirror gadget)
    (handler-bind
	;;-- Nasty hack alert for
	((simple-warning #'(lambda (condition)
			     (muffle-warning condition))))
      (with-no-value-changed-callbacks
	  (tk::set-values (sheet-mirror gadget) :set nv)))))

;;

(defclass openlook-row-column-gadget-mixin ()
	  ())

(defmethod find-widget-initargs-for-sheet :around  ((port openlook-port)
						    (parent t)
						    (sheet openlook-row-column-gadget-mixin))
  (let ((initargs (call-next-method))
	(x (ecase (gadget-orientation sheet)
	     (:vertical (or (gadget-columns sheet)
			    (and (silica::gadget-rows sheet)
				 (ceiling (length (sheet-children sheet))
					  (silica::gadget-rows sheet)))))
	     (:horizontal (or (silica::gadget-rows sheet)
			      (and (silica::gadget-columns sheet)
				   (ceiling (length (sheet-children sheet))
					    (silica::gadget-columns sheet))))))))
    (when x
      (setf (getf initargs :measure) x))
    initargs))

;;

(defclass openlook-radio-box (openlook-row-column-gadget-mixin
			      openlook-geometry-manager
			      mirrored-sheet-mixin
			      sheet-multiple-child-mixin
			      sheet-permanently-enabled-mixin
			      radio-box
			      ask-widget-for-size-mixin
			      basic-pane)
	  ())

(defmethod sheet-adopt-child :after ((gadget openlook-radio-box) child)
  (setf (gadget-client child) gadget))

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-radio-box))
  'xt::exclusives)

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-radio-box))
  (with-accessors ((orientation gadget-orientation)) sheet
    (list :layout-type
	  (ecase orientation
	    (:horizontal :fixedrows)
	    (:vertical :fixedcols)))))

(defmethod value-changed-callback :around ((v basic-gadget)
					   (client openlook-radio-box)
					   (id t)
					   (value t))
  (when (eq value t)
    (setf (radio-box-current-selection client) v)
    (value-changed-callback client
			    (gadget-client client)
			    (gadget-id client)
			    v))
  (call-next-method))

(defmethod compose-space ((rb openlook-radio-box) &key width height)
  (declare (ignore width height))
  (compose-space-for-radio/check-box rb))

(defun compose-space-for-radio/check-box (rb &optional (spacing 0))
  (let ((mirror (sheet-direct-mirror rb)))
    (multiple-value-bind (orientation measure)
	(tk::get-values mirror :layout-type :measure)
      (let* ((max-w 0)
	     (max-h 0)
	     (all-children (tk::widget-children mirror))
	     (n (ceiling (length all-children) measure))
	     (max-column-widths (make-list n :initial-element 0)))

	(let ((children all-children)
	      child)
	  (loop
	    (unless children (return nil))

	    ;; Walk over one row/column

	    (let ((one-max-w 0)
		  (one-max-h 0))

	      (dotimes (i n)
		(unless children (return nil))
		(setq child (pop children))
		(multiple-value-bind
		    (ignore-x igore-y width height)
		    (xt::widget-best-geometry child)
		  (declare (ignore ignore-x igore-y))
		  (maxf one-max-h height)
		  (maxf one-max-w width)
		  (maxf (elt max-column-widths i)
			(case orientation
			  (:fixedrows width)
			  (:fixedcols height)))))

	      ;; If there is more then we need to add some spacing in

	      (ecase orientation
		(:fixedrows
		 ;; We have just done a row
		 (when children (incf one-max-h spacing))
		 (incf max-h one-max-h))
		(:fixedcols
		 ;; We have just done a colum
		 (when children (incf one-max-w spacing))
		 (incf max-w one-max-w))))))

	(ecase orientation
	  (:fixedrows (make-space-requirement
		       :width (+ (reduce #'+ max-column-widths)
				 (* spacing (1- n)))
		       :height max-h))
	  (:fixedcols (make-space-requirement
		       :width max-w
		       :height (+ (reduce #'+ max-column-widths)
				  (* spacing (1- n))))))))))

;;;


(defclass openlook-check-box (openlook-row-column-gadget-mixin
			      openlook-geometry-manager
			      mirrored-sheet-mixin
			      sheet-multiple-child-mixin
			      sheet-permanently-enabled-mixin
			      check-box
			      ask-widget-for-size-mixin
			      basic-pane)
	  ())

(defmethod sheet-adopt-child :after ((gadget openlook-check-box) child)
  (setf (gadget-client child) gadget))

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-check-box))
  'xt::nonexclusives)

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-check-box))

  (with-accessors ((orientation gadget-orientation)) sheet
    (list :layout-type
	  (ecase orientation
	    (:horizontal :fixedrows)
	    (:vertical :fixedcols)))))

(defmethod value-changed-callback :around ((v basic-gadget)
					   (client openlook-check-box)
					   (id t)
					   (value t))
  (if (eq value t)
      (push v (check-box-current-selection client))
    (setf (check-box-current-selection client)
      (delete v (check-box-current-selection client))))
  (value-changed-callback client
			  (gadget-client client)
			  (gadget-id client)
			  (check-box-current-selection client))
  (call-next-method))

(defmethod compose-space ((cb openlook-check-box) &key width height)
  (declare (ignore width height))
  ;; Where did this 15 come from. Its the space between the items
  (compose-space-for-radio/check-box cb 15))

;;

;; Open look-orriented-gadget


(defclass openlook-slider (#+ignore openlook-range-pane
			   xt-oriented-gadget
			   xt-leaf-pane
			   slider)
  ((slider-mirror :initform nil :accessor openlook-slider-mirror)
   (slider-label-mirror :initform nil :accessor openlook-slider-label-mirror)
   (slider-value-mirror :initform nil :accessor openlook-slider-value-mirror)
   (slider-initargs :accessor openlook-slider-initargs)))

;; This is kind of hacky but its the best way of gathering the
;; components

(defmethod find-widget-initargs-for-sheet :around ((port openlook-port)
						   (parent t)
						   (sheet openlook-slider))
  (let ((initargs (call-next-method)))
    (setf (openlook-slider-initargs sheet) initargs)
    nil))

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-slider))
  'tk::control)

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-slider))
  (with-accessors ((label gadget-label)
		   (show-value-p gadget-show-value-p)
		   (value gadget-value)
		   (editablep gadget-editable-p)) sheet
    (with-slots (silica::min-label silica::max-label) sheet
      (multiple-value-bind
	  (smin smax) (gadget-range* sheet)
	(let ((mmin 0)
	      (mmax 1000))
	  (append
	   (list :slider-class (if editablep 'tk::slider 'tk::gauge))
	   (list :show-value-p show-value-p)
	   (list :drag-c-b-type :release)
	   ;;-- icon label
	   (make-label-initarg label :label)
	   (list :slider-min mmin
		 :slider-max mmax)
	   (and silica::min-label
		(list :min-label silica::min-label))
	   (and silica::max-label
		(list :max-label silica::max-label))
	   (and value
		(list :slider-value
		      (fix-coordinate
		       (compute-symmetric-value
			smin smax value mmin mmax))))))))))

(defmethod realize-mirror :around ((port openlook-port) (sheet openlook-slider))
  (let ((control-area (call-next-method))
	(initargs (openlook-slider-initargs sheet)))
    (multiple-value-bind
        (smin smax) (gadget-range* sheet)
      (with-accessors ((slider-mirror openlook-slider-mirror)
		       (label-mirror openlook-slider-label-mirror )
		       (value-mirror openlook-slider-value-mirror)
		       (value gadget-value)
		       (decimal-places slider-decimal-places)) sheet
	(destructuring-bind (&key label slider-class show-value-p &allow-other-keys) initargs
	  (setf label-mirror
	    (make-instance 'tk::static-text
			   :string (or label "")
			   :managed (and label t)
			   :parent control-area))
	  (setf slider-mirror
	    (with-keywords-removed (bashed-initargs initargs '(:label
							       :show-value-p
							       :slider-class))
	      (apply #'make-instance slider-class :parent control-area
		     bashed-initargs)))

	  (when (gadget-editable-p sheet)
	    ;;--- Do we need to do this?
	    #+ignore
	    (tk::add-callback widget :drag-callback 'queue-drag-event sheet)
	    (tk::add-callback slider-mirror :slider-moved
			      'slider-changed-callback-1 sheet))

	  (setf value-mirror
	    (make-instance 'tk::static-text
			   :strip nil
			   :string (print-slider-value
				    decimal-places
				    value  smin smax)
			   :managed (and show-value-p t)
			   :parent control-area)))
	control-area))))

(defun print-slider-value (decimal-places value smin smax)
  (let ((n (+ decimal-places
	      (if (> decimal-places 0) 1 0)
	      (if (minusp smin) 1 0)
	      (1+ (ceiling (max (log (abs smax) 10)
				(log (abs smin) 10)))))))
    (if (> decimal-places 0)
	(format nil "~v,vf " n decimal-places value)
      (format nil "~vD " n (round value)))))

(defmethod gadget-value ((slider openlook-slider))
  (if (sheet-direct-mirror slider)
      (compute-slider-value slider)
    (call-next-method)))

(defmethod (setf gadget-value) :after  (nv (slider openlook-slider) &key invoke-callback)
  (declare (ignore invoke-callback))
  (when (sheet-direct-mirror slider)
    (with-no-value-changed-callbacks
	(set-slider-value slider nv))))

(defun compute-slider-value (sheet &optional value)
  (let ((widget (openlook-slider-mirror sheet)))
    (multiple-value-bind
	(smin smax) (gadget-range* sheet)
      (multiple-value-bind
	  (current-value mmin mmax)
	  (tk::get-values widget :slider-value :slider-min :slider-max)
	(compute-symmetric-value
			 mmin mmax (or value current-value) smin smax)))))


(defun set-slider-value (sheet nv)
  (let ((widget (openlook-slider-mirror sheet)))
    (multiple-value-bind
	(smin smax) (gadget-range* sheet)
      (multiple-value-bind
	  (mmin mmax)
	  (tk::get-values widget :slider-min :slider-max)
	(tk::set-values widget
			:slider-value
			(round
			 (compute-symmetric-value
			   smin smax  nv mmin mmax)))))))



(defmethod slider-changed-callback-1 ((widget t) value (sheet openlook-slider))
  (let ((real-value (compute-slider-value sheet value)))
    (queue-value-changed-event widget sheet real-value)
    (let ((text (openlook-slider-value-mirror sheet)))
      (when (xt::is-managed-p text)
	(multiple-value-bind
	    (smin smax) (gadget-range* sheet)
	  (tk::set-values text
			  :string
			  (print-slider-value
			   (slider-decimal-places sheet)
			   real-value  smin smax)))))))

(defmethod compose-space ((sheet openlook-slider) &key width height)
  (declare (ignore width height))

  (with-accessors ((slider-mirror openlook-slider-mirror)
		   (label-mirror openlook-slider-label-mirror)
		   (value-mirror openlook-slider-value-mirror)
		   (orientation gadget-orientation)) sheet

    (let ((h-pad 4)
	  (v-pad 4)
	  (h-space 4)
	  (v-space 4)
	  (height 0)
	  (max-height 0)
	  (width 0)
	  (max-width 0))

      (flet ((do-child (child &optional (min-width 0) (min-height 0))
	       (when (xt::is-managed-p child)
		 (multiple-value-bind
		     (ignore-x igore-y w h)
		     (xt::widget-best-geometry child)
		   (declare (ignore ignore-x igore-y))

		   (incf w h-space)
		   (incf h v-space)
		   (maxf h min-height)
		   (maxf w min-width)

		   (incf height h)
		   (maxf max-height h)
		   (incf width w)
		   (maxf max-width w)))))

	(do-child label-mirror)
	(do-child value-mirror)
	(ecase orientation
	  (:horizontal (do-child slider-mirror 100 0))
	  (:vertical (do-child slider-mirror 0 100)))

	(incf width (* 2 h-pad))
	(incf max-width (* 2 h-pad))

	(incf height (* 2 v-pad))
	(incf max-height (* 2 v-pad))

	(make-space-requirement :width width :height max-height)))))

(defmethod allocate-space ((sheet openlook-slider) width height)
  (declare (ignore width height))
  (with-accessors ((slider-mirror openlook-slider-mirror)
		   (label-mirror openlook-slider-label-mirror)
		   (value-mirror openlook-slider-value-mirror)
		   (orientation gadget-orientation)) sheet

    (let* ((h-pad 4)
	  (v-pad 4)
	  (h-space 4)
	  (v-space 4)
	  (x h-pad)
	  (y v-pad))

      (flet ((do-child (child &optional (min-width 0) (min-height 0))
	       (when (xt::is-managed-p child)

		 (multiple-value-bind
		     (ignore-x igore-y w h)
		     (xt::widget-best-geometry child)
		   (declare (ignore ignore-x igore-y))

		   (incf w h-space)
		   (incf h v-space)
		   (maxf h min-height)
		   (maxf w min-width)
		   (tk::set-values child :x x :y y :width w :height h)
		   (incf x (+ w h-space))))))

	(do-child label-mirror)
	(do-child value-mirror)
	(ecase orientation
	  (:horizontal (do-child slider-mirror 100 0))
	  (:vertical (do-child slider-mirror 0 100)))))))

#+dunno
(defmethod (setf gadget-show-value-p) :after (nv (sheet openlook-slider))
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) :show-value nv)))

#+dunno
(defmethod (setf gadget-label) :after (nv (sheet openlook-slider))
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) :title-string (or nv ""))))


;;; New definitions to support specifying width and height in terms of
;;; lines and characters.
;;;;---------------- This needs to be done for Openlook
;;;;---------------- Code currently is just a copy of the motif stuff

;;;--- Implement caching
;;;--- Flush ncolumns, nrows but perhaps keep them of compatibility
;;;--- We are not supporting (pixels :relative)


(defmethod silica::normalize-space-requirement ((sheet openlook-text-field) sr)
  (declare (ignore width height))
  (normalize-space-for-text-field-or-label
   sheet sr))

;;-- For label pane there are margin-{left,top,right,bottom} resources also
;;-- but they default to 0.

(defmethod silica::normalize-space-requirement ((sheet openlook-label-pane) sr)
  (declare (ignore width height))
  (normalize-space-for-text-field-or-label
   sheet sr))

(defmethod silica::normalize-space-requirement ((sheet openlook-push-button) sr)
  (declare (ignore width height))
  (normalize-space-for-text-field-or-label
   sheet sr))

(defmethod process-width-specification (sheet width)
  (when (numberp width) (return-from process-width-specification width))
  (let ((chars (etypecase width
		 (list
		  (assert (eq (second width) :character))
		  (first width))
		 (string (length width)))))
    (let ((font (tk::get-values (sheet-direct-mirror sheet) :font)))
      (let ((font-width (font-list-max-width-and-height font)))
	(* font-width chars)))))

(defun font-list-max-width-and-height (font)
  (values (tk::font-width font) (tk::font-height font)))

;;-- What should this do really?

(defun process-height-specification (sheet width)
  (when (numberp width) (return-from process-height-specification width))
  (let ((chars (etypecase width
		 (list
		  (assert (eq (second width) :line))
		  (first width))
		 (string (length width)))))
    (multiple-value-bind (font top-margin bottom-margin)
	(tk::get-values (sheet-direct-mirror sheet) :font :top-margin :bottom-margin)
      (let ((font-height (nth-value 1 (font-list-max-width-and-height font))))
	(+ top-margin bottom-margin (* font-height chars))))))


;;;

(defclass openlook-text-editor (text-editor xt-leaf-pane)
	  ())

(defmethod text-editor-text ((te openlook-text-editor))
  (let ((widget (sheet-direct-mirror te)))
    (text-editor-text widget)))

(defmethod text-editor-text ((widget tk::text-edit))
  (tk::with-ref-par ((end 0)
		     (string 0))
    (assert (not (zerop (tk::ol_text_edit_get_last_position widget end))))
    (let ((end (aref end 0)))
      (assert (not (zerop (tk::ol_text_edit_read_substring
			   widget string 0 end))))
      (let ((string (aref string 0)))
	(prog1
	    (subseq (excl:native-to-string string) 0 end)
	  (xt::xt_free string))))))


(defmethod  text-editor-selection ((widget openlook-text-editor))
  (text-editor-selection (sheet-direct-mirror widget)))

(defmethod text-editor-selection ((widget tk::text-edit))
  (multiple-value-bind (start end)
      (tk::get-values widget :select-start :select-end)
    (and (> end start)
	 (tk::with-ref-par ((string 0))
	   (assert (not (zerop (tk::ol_text_edit_read_substring
				widget string start end))))
	   (let ((string (aref string 0)))
	     (prog1
		 (excl:native-to-string string)
	       (xt::xt_free string)))))))

(defmethod gadget-current-selection ((tf openlook-text-editor))
  (and (sheet-direct-mirror tf) (text-editor-selection tf)))

(defmethod (setf text-editor-text) (nv (te openlook-text-editor))
  (let ((widget (sheet-direct-mirror te)))
    (setf (text-editor-text widget) nv)))

(defmethod (setf text-editor-text) (nv (widget tk::text-edit))
  (unless (equal nv (text-editor-text widget))
    (assert (not (zerop (tk::ol_text_edit_clear_buffer widget))))
    (assert (not (zerop (tk::ol_text_edit_insert widget nv (length nv)))))))


(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-text-editor))
  'tk::text-edit)

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-text-editor))
  (with-accessors ((value gadget-value)
		   (ncolumns gadget-columns)
		   (nlines gadget-lines)
		   (editable gadget-editable-p)
		   (word-wrap gadget-word-wrap)
		   (foreground pane-foreground)) sheet
    (append (and foreground
		 (with-sheet-medium (medium sheet)
		   `(:font-color
		     ,(cadr (decode-gadget-foreground medium
						      sheet
						      foreground)))))
	    (and (not editable) `(:edit-type :text-read))
	    (and ncolumns (list :chars-visible ncolumns))
	    (and nlines (list :lines-visible nlines))
	    (and value `(:source ,value))
	    (list :wrap-mode (if word-wrap :wrap-white-space :wrap-off)))))

(defmethod (setf gadget-editable-p) :after (nv (tf openlook-text-editor))
  (let ((m (sheet-direct-mirror tf)))
    (when m (tk::set-values m :edit-type (if nv :text-edit :text-read)))))

(defmethod process-width-specification ((sheet openlook-text-editor) width)
  (when (numberp width) (return-from process-width-specification width))
  (let ((chars (etypecase width
		 (list
		  (assert (eq (second width) :character))
		  (first width))
		 (string (length width)))))
    (multiple-value-bind (font left-margin right-margin)
	(tk::get-values (sheet-direct-mirror sheet) :font :left-margin :right-margin)
      (let ((font-width (font-list-max-width-and-height font)))
	(+ left-margin right-margin (* font-width chars))))))

(defmethod (setf gadget-word-wrap) :after (word-wrap (gadget openlook-text-editor))
  (tk::set-values (sheet-direct-mirror gadget)
		  :wrap-mode (if word-wrap :wrap-white-space :wrap-off)))

(defmethod add-sheet-callbacks :after ((port openlook-port)
				       (sheet openlook-text-editor)
				       (widget t))
  (tk::add-callback widget
		    :post-modify-notification
		    'ol-text-field-value-changed
		    sheet)

  #+ignore
  (tk::add-event-handler edit-widget
			 '(:focus-change)
			 1
			 'openlook-text-field-event-handler
			 sheet))

(defmethod compose-space ((te openlook-text-editor) &key width height)
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

(defmethod silica::normalize-space-requirement ((sheet openlook-text-editor) sr)
  (normalize-space-requirement-for-text-editor sheet sr))

(defmethod gadget-value ((gadget openlook-text-editor))
  (if (sheet-direct-mirror gadget)
      (text-editor-text gadget)
    (call-next-method)))

(defmethod (setf gadget-value) :after
	   (nv (gadget openlook-text-editor) &key invoke-callback)
  (declare (ignore invoke-callback))
  (when (sheet-direct-mirror gadget)
    (with-no-value-changed-callbacks
	(setf (text-editor-text gadget) nv))))


(defmethod gadget-supplies-scrolling-p ((contents openlook-text-editor))
  t)

;;;
;;;--- This code is so much like the motif one I think we could share
;;;--- code.

(defclass openlook-scrolling-window (scroller-pane
				     openlook-geometry-manager
				     ask-widget-for-size-mixin
				     mirrored-sheet-mixin
				     sheet-multiple-child-mixin
				     sheet-permanently-enabled-mixin
				     basic-pane)
	  ;;-- probably one of the options is whether to have vertical
	  ;;-- and/or horizontal scrollbars
	  ())


;;-- Very similar to the motif code.

(defmethod initialize-mirror :after ((port openlook-port)
				     (parent openlook-geometry-manager)
				     (parent-widget t)
				     (sheet t)
				     (widget t))
  ;; This is a pane in the butt since you only get configure-notify
  ;; events after you have been created
  ;; Really Xt should have a callback for this. Not the drawing area.
  (typecase widget
    (tk::draw-area
     (tk::add-callback widget
      :resize-callback
      'sheet-mirror-resized-callback
      sheet))
    (t
     (tk::add-event-handler widget
			    '(:structure-notify)
			    1
			    'sheet-mirror-event-handler
			    sheet))))


;;--- this is identical to the motif code

(defmethod initialize-instance :after ((sp openlook-scrolling-window)
                                       &key scroll-bars contents frame-manager frame)
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

(defmethod realize-widget-mirror ((port openlook-port)
				  (parent-sheet openlook-scrolling-window)
				  (parent-widget t)
				  (sheet openlook-scroll-bar))
  (let ((initargs (find-widget-initargs-for-sheet port parent-widget sheet))
	(widget (tk::get-values parent-widget
				(ecase (gadget-orientation sheet)
				  (:horizontal :h-scrollbar)
				  (:vertical :v-scrollbar)))))
    (apply #'tk::set-values widget initargs)
    widget))

(defmethod gadget-supplies-scrolling-p ((contents t))
  nil)

(defmethod make-pane-class ((framem openlook-frame-manager)
			    (class (eql 'scroller-pane))
			    &rest options
			    &key contents)
  (declare (ignore options))
  (if (gadget-includes-scrollbars-p contents)
      'openlook-frame-pane
    'openlook-scrolling-window))

(defmethod make-pane-arglist ((framem openlook-frame-manager)
			      (class (eql 'scroller-pane))
			      &rest options &key contents)
  (declare (ignore type))
  (declare (non-dynamic-extent options))
  ;; This is annoying. Because we are making a different class we
  ;; have to discard :scroll-bars
  (if (gadget-includes-scrollbars-p contents)
      (remove-keywords options '(:scroll-bars))
    options))

(defmethod gadget-includes-scrollbars-p ((pane t))
  nil)

(defmethod compose-space ((fr openlook-scrolling-window) &key width height)
  (declare (ignore width height))
  ;;--- This is not quite right because I think scrollbars are a bit
  ;;--- bigger than this. But atleast its a start
  ;;-- We check to see which scrollbars we have

  (let* ((fudge 8) 			; This was from trial and
					; error but at least 4 (1point) is from
					; the contents border
	 (spacing (+ fudge 0) #+ignore (tk::get-values (sheet-mirror fr) :spacing))
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
      (multiple-value-bind
	      (hb vb)
	      (tk::get-values (sheet-direct-mirror fr) :h-scrollbar :v-scrollbar)
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
      (make-space-requirement
	:width width :min-width min-width :max-width max-width
	:height height :min-height min-height :max-height max-height))))

(ff:defun-c-callable scrolling-window-geometry-function
    ((content :unsigned-natural)
     (geometries :unsigned-natural))
  (let* ((viewport (find-sheet-from-widget-address content))
	 (scrolling-window (sheet-parent viewport)))
    (multiple-value-bind
	(swidth sheight) (tk::get-values (sheet-direct-mirror
					  scrolling-window) :width :height)
      (let* ((fudge 8)
	     (vsbp
	      (let ((sb (scroller-pane-vertical-scroll-bar scrolling-window)))
		(and sb (sheet-enabled-p sb))))
	     (hsbp (let ((sb (scroller-pane-horizontal-scroll-bar scrolling-window)))
		     (and sb (sheet-enabled-p sb))))
	     (hsb-height (tk::ol-sw-geometries-hsb-height geometries))
	     (vsb-width (tk::ol-sw-geometries-vsb-width geometries))
	     (w (- swidth (if vsbp vsb-width 0) fudge))
	     (h (- sheight (if hsbp hsb-height 0) fudge)))
	(setf (tk::ol-sw-geometries-force-hsb geometries) (if hsbp 1 0))
	(setf (tk::ol-sw-geometries-force-vsb geometries) (if vsbp 1 0))
	(when (plusp w) (setf (tk::ol-sw-geometries-bbc-width geometries) w))
	(when (plusp h) (setf (tk::ol-sw-geometries-bbc-height geometries) h))
	(multiple-value-bind (extent-width extent-height)
	    (bounding-rectangle-size (viewport-contents-extent
				      viewport))
	  (fix-coordinates extent-width extent-height)
	  (setf (tk::ol-sw-geometries-bbc-real-width geometries) extent-width
		(tk::ol-sw-geometries-bbc-real-height geometries) extent-height))))))

(defvar *scrolling-window-geometry-function-address*
    (ff::register-foreign-callable 'scrolling-window-geometry-function))

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-scrolling-window))
  'xt::scrolled-window)

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-scrolling-window))
  (and (not (gadget-supplies-scrolling-p (pane-contents sheet)))
       (append
	(let ((scroll-bars (scroller-pane-scroll-bar-policy sheet)))
	  `(:force-vertical-s-b
	    ,(and (member scroll-bars '(t :both :dynamic :vertical))
		  t)
	    :force-horizontal-s-b
	    ,(and (member scroll-bars '(t :both :dynamic :horizontal))
		  t)))
	`(:h-auto-scroll nil
			 :v-auto-scroll nil
			 :compute-geometries
			 ,*scrolling-window-geometry-function-address*))))



;;;

(defclass openlook-list-pane (list-pane xt-leaf-pane)
	  ((token-list :accessor list-pane-token-list)
	   (current-tokens :initform nil :accessor list-pane-current-tokens)))


(defmethod gadget-includes-scrollbars-p ((gadget openlook-list-pane))
  gadget)

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-list-pane))
  'xt::ol-list)

(defmethod find-widget-initargs-for-sheet ((port openlook-port)
					   (parent t)
					   (sheet openlook-list-pane))
  (with-accessors ((visible-items gadget-visible-items)
		   (items set-gadget-items)) sheet
    `(:view-height ,(max 1 (or visible-items (length items))))))

(defmethod realize-mirror :around ((port openlook-port) (sheet openlook-list-pane))
  (let ((widget (call-next-method)))
    (add-items-to-list-pane-widget sheet widget)
    widget))


(defun add-items-to-list-pane-widget (sheet widget)
  (let (
	(token-list nil)
	selected-tokens
	(count 0)
	(add-item-fn (tk::get-values widget :appl-add-item)))
    (with-accessors ((items set-gadget-items)
		     (value gadget-value)
		     (value-key set-gadget-value-key)
		     (test set-gadget-test)
		     (mode list-pane-mode)
		     (name-key set-gadget-name-key)) sheet
      (dolist (item items)
	(let ((x (tk::make-ol-list-item :in-foreign-space t))
	      (selected-p (list-pane-selected-item-p sheet item)))
	  (setf (tk::ol-list-item-label-type x) tk::ol-string
		(tk::ol-list-item-label x) (clim-utils:string-to-foreign (funcall name-key item))
		(tk::ol-list-item-mnemonic x) 0
		(tk::ol-list-item-attr x)
		(dpb (if selected-p 1 0)
		     '#.tk::ol_b_list_attr_current
		     (dpb count '#.tk::ol_b_list_attr_appl 0)))
	  (let ((token
		 (tk::ol_appl_add_item
		  add-item-fn
		  widget
		  0
		  0
		  x)))
	    (push (list token count) token-list)
	    (when selected-p (push token selected-tokens)))
	  (incf count)))
      (setf (list-pane-token-list sheet) token-list
	    (list-pane-current-tokens sheet) selected-tokens))))

(defmethod (setf gadget-value) :after (new-value (gadget openlook-list-pane) &key invoke-callback)
  (declare (ignore new-value invoke-callback))
  (let ((widget (sheet-direct-mirror gadget))
	(items (set-gadget-items gadget))
	(selected-tokens nil))
    (when widget
      (dolist (token-and-position (list-pane-token-list gadget))
	(destructuring-bind (token position) token-and-position
	  (let* ((item (nth position items))
		 (selectedp (list-pane-selected-item-p gadget item))
		 (toolkit-item (find-list-pane-item-from-token token)))
	    (setf (tk::ol-list-item-attr toolkit-item)
	      (dpb (if selectedp 1 0)
		   '#.tk::ol_b_list_attr_current
		   (dpb position '#.tk::ol_b_list_attr_appl (tk::ol-list-item-attr toolkit-item))))
	    (touch-list-pane-item widget token)
	    (when selectedp (push token selected-tokens)))))
      (setf (list-pane-current-tokens gadget)
	selected-tokens))))


(defmethod (setf set-gadget-items) :after (items (gadget openlook-list-pane))
  (declare (ignore items))
  (let ((widget (sheet-direct-mirror gadget)))
    (when widget
      (let ((delete-item-fn (tk::get-values widget :appl-delete-item)))
	(dolist (token (list-pane-token-list gadget))
	  (tk::ol_appl_delete_item delete-item-fn widget (car token)))
	(add-items-to-list-pane-widget gadget widget)))))

(defmethod add-sheet-callbacks :after ((port openlook-port)
				       (sheet openlook-list-pane)
				       (widget t))
  (tk::add-callback widget
		    :user-make-current
		    'list-pane-value-changed
		    sheet))

(defun list-pane-value-changed (widget token sheet)
  ;; Need to decide between exclusive and nonexclusive etc etc
  (let ((item (find-list-pane-item-from-token token)))
    (with-accessors ((items set-gadget-items)
		     (value gadget-value)
		     (value-key set-gadget-value-key)
		     (test set-gadget-test)
		     (mode list-pane-mode)
		     (name-key set-gadget-name-key)) sheet
      (flet ((item-from-token (token)
	       (nth (second (assoc token (list-pane-token-list sheet))) items)))
	(ecase mode
	  (:exclusive
	   (unless (member token (list-pane-current-tokens sheet))
	     (let* ((old-token (car (list-pane-current-tokens sheet)))
		    (old-item (and old-token (find-list-pane-item-from-token old-token))))

	       (when old-item
		 (setf (tk::ol-list-item-attr old-item)
		   (dpb 0 '#.tk::ol_b_list_attr_current (tk::ol-list-item-attr old-item))))

	       (setf (tk::ol-list-item-attr item)
		 (dpb 1 '#.tk::ol_b_list_attr_current (tk::ol-list-item-attr item)))

	       (when old-token (touch-list-pane-item widget old-token))
	       (touch-list-pane-item widget token))
	     (setf (list-pane-current-tokens sheet) (list token)))
	   (queue-value-changed-event
	    widget sheet (funcall value-key
				  (item-from-token
				   (car (list-pane-current-tokens sheet))))))
	  (:nonexclusive
	   (if (member token (list-pane-current-tokens sheet))
	       (progn
		 (setf (list-pane-current-tokens sheet)
		   (delete token (list-pane-current-tokens sheet))
		   (tk::ol-list-item-attr item)
		   (dpb 0 '#.tk::ol_b_list_attr_current (tk::ol-list-item-attr item))))
	     (progn
	       (push token (list-pane-current-tokens sheet))
	       (setf (tk::ol-list-item-attr item)
		 (dpb 1 '#.tk::ol_b_list_attr_current
		      (tk::ol-list-item-attr item)))))
	   (touch-list-pane-item widget token)
	   (queue-value-changed-event
	    widget sheet (mapcar value-key (mapcar
					    #'item-from-token
					    (list-pane-current-tokens sheet))))))))))



(defun touch-list-pane-item (widget token)
  (tk::ol_appl_touch_item (tk::get-values widget :appl-touch-item) widget token))

(defun find-list-pane-item-from-token (token)
  (tk::ol_list_item_pointer token))

(defmethod compose-space ((sheet openlook-list-pane) &key width
							  height)
  (declare (ignore width height))
  ;;-- Fudge alert!
  (with-accessors ((items set-gadget-items)
		   (name-key set-gadget-name-key)
		   (visible-items gadget-visible-items)) sheet
    (let ((mirror (sheet-direct-mirror sheet)))
      (multiple-value-bind (font)
	  (tk::get-values (tk::get-values mirror :list-pane) :font)
	(let* ((scroll-bar-width 31)
	       ;; These constants were got from looking at the source
	       (vertical-margins (ceiling (* 2 4 (/ 90 72))))
	       (horizontal-margins (ceiling (* (+ 6 8) (/ 90 72))))
	       (vertical-spacing (ceiling (* 6 (/ 90 72))))
	       (fudge 6)	; This seems to be necessary to make
				; the right number of items visible
	       (item-max-width
		(let ((max 0))
		  (dolist (item items max)
		    (maxf max (let ((string (funcall name-key item)))
				(x11:xtextwidth
				 font string (length string)))))))
	       (height (+ (* (or visible-items (length items))
			     (+ vertical-spacing (tk::font-height font)))
			  vertical-margins
		       fudge))
	       (width (+ -5
			 horizontal-margins
			item-max-width
			scroll-bar-width)))
	  (make-space-requirement :width width :height height))))))

(defmethod change-widget-geometry :after (parent (child tk::ol-list) &key width)
  (declare (ignore parent))
  (tk::set-values (tk::get-values child :list-pane)
		  :pref-max-width width
		  :pref-min-width width))


(defclass openlook-option-pane (option-pane xt-leaf-pane)
  ((buttons :accessor option-menu-buttons)
   (abbrev-menu-button :accessor openlook-option-pane-abbrev-menu-button)))

(defmethod find-widget-class-and-name-for-sheet ((port openlook-port)
						 (parent t)
						 (sheet openlook-option-pane))
  'xt::control)

(defmethod realize-mirror ((port openlook-port) (sheet openlook-option-pane))
  (with-accessors ((label gadget-label)
		   (items set-gadget-items)
		   (name-key set-gadget-name-key)) sheet
    (let* ((control (call-next-method))
	   ;;-- icon label
	   (label (apply #'make-instance 'tk::static-text
			 :parent control
			 (append
			  (make-label-initarg label :string))))
	   (widget (apply #'make-instance
			  'xt::abbrev-menu-button
			  :parent control nil))
	   (preview (make-instance 'tk::static-text
				   :string (let ((big ""))
					     (dolist (item items big)
					       (let ((label (funcall name-key item)))
						 (when (> (length label) (length big))
						   (setq big label)))))
				   :recompute-size nil
				   :parent control)))
      (declare (ignore label))
      (setf (openlook-option-pane-abbrev-menu-button sheet) widget)
      (tk::set-values widget :preview-widget preview)
      (create-option-menu-buttons sheet widget)
      control)))

(defmethod compose-space ((sheet openlook-option-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (max-width height)
        (map-over-menubar sheet nil)
    (make-space-requirement :width max-width :height height)))

(defun create-option-menu-buttons (sheet widget)
  (let ((menu-pane (tk::get-values widget :menu-pane))
	(preview (tk::get-values widget :preview-widget)))
  (with-accessors ((items set-gadget-items)
		   (name-key set-gadget-name-key)
		   (value-key set-gadget-value-key)
		   (test set-gadget-test)) sheet
    (setf (option-menu-buttons sheet)
      (mapcar #'(lambda (item)
		  (let* ((currentp (funcall
				    test (funcall value-key item)
				    (gadget-value sheet)))
			 (label (funcall name-key item))
			 (button (make-instance 'tk::oblong-button
						:default currentp
						:label label
						:parent menu-pane)))
		    (tk::add-callback button
				      :select
				      #'(lambda (&rest ignore)
					  (declare (ignore ignore))
					  (queue-value-changed-event
					   widget sheet (funcall
							 value-key item))
					  (tk::set-values button :default t)
					  (tk::set-values preview :string label)))
		    (when currentp
		      (tk::set-values preview :string label))
		    (list button item)))
	      items)))))

(defmethod (setf set-gadget-items) :after (items (gadget openlook-option-pane))
  (declare (ignore items))
  ;;-- What to do about the preview widget
  (let ((m (sheet-direct-mirror gadget)))
    (when m
      (tk::unmanage-children (mapcar #'car (option-menu-buttons gadget)))
      (create-option-menu-buttons gadget
				  (openlook-option-pane-abbrev-menu-button gadget)))))

(defmethod (setf gadget-value) :after (nv (sheet openlook-option-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((widget (sheet-direct-mirror sheet)))
    (when widget
      (with-accessors ((items set-gadget-items)
		       (value-key set-gadget-value-key)
		       (test set-gadget-test)) sheet
	(let ((item (find nv items :test test :key value-key)))
	  (assert item)
	  (let ((button (car (find item (option-menu-buttons sheet) :key #'second))))
	    (with-no-value-changed-callbacks
		(tk::set-values (tk::get-values
				 (openlook-option-pane-abbrev-menu-button sheet)
				 :preview-widget)
				:string (tk::get-values button :label))
	      (tk::set-values button :default t))))))))

(defmethod discard-accelerator-event-p ((port openlook-port) (event t))
  (or (call-next-method)
      ;;-- There are a whole bunch of other keysyms that need to be ignored.
      ;;-- Perhaps in OLIT there is a way of getting the actual keysym.
      ;;-- Perhaps we need a way of representing them in the clim world
      (member (keyboard-event-key-name event) '(:tab))))

(defmethod frame-manager-notify-user ((framem openlook-frame-manager)
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
				      (exit-boxes
				       '(:exit
					 :abort
					 :help))
				      (name :notify-user)
				      x-position
				      y-position)
  (declare (ignore background foreground x-position y-position))
  (loop
    (let* ((shell (make-instance
		      'tk::notice-shell
		    :parent (sheet-shell associated-window))))
      (multiple-value-bind (text-area control-area)
	  (tk::get-values shell :text-area :control-area)
	(tk::set-values text-area :string message-string)
	(let ((done nil))
	  (flet ((done (widget value)
		   (declare (ignore widget))
		   (setq done (list value)))
		 (display-help (widget)
		   (declare (ignore widget))
		   (frame-manager-notify-user
		    framem
		    documentation
		    :associated-window associated-window)
		   (setq done (list :help))))
	    (dolist (exit-box exit-boxes)
	      (multiple-value-bind (name label)
		  (if (consp exit-box) (values (first exit-box)
					       (second exit-box)) exit-box)
		(let ((button (make-instance 'xt::oblong-button
				:parent control-area
				:label (or label (string name)))))
		  (case name
		    (:exit (tk::add-callback button :select #'done t))
		    (:abort (tk::add-callback button :select #'done nil))
		    (:help
		     (if documentation
			 (tk::add-callback button :select #'display-help)
		       (xt::set-sensitive button nil)))))))

	    (let ((result
		   (unwind-protect
		       (progn
			 (tk::popup shell)
			 (wait-for-callback-invocation
			  (port framem) #'(lambda () done))
			 (car done))
		     (unless done
		       (tk::popdown shell)))))
	      (unless (eq result :help)
		(return-from frame-manager-notify-user result)))))))))


;;--- We could export this to handle the default case.
;;--- It definitely needs work though.

(defmethod frame-manager-select-file
    ((framem openlook-frame-manager) &key (frame nil frame-p)
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
				     background
				     foreground
				     text-style
				     x-position
				     y-position)
  (declare (ignore directory pattern text-style background foreground
		   x-position y-position))
  (let ((pathname (and directory (pathname directory)))
	(stream (frame-top-level-sheet frame))
	(view '(text-field-view :width (60 :character))))
    (accepting-values (stream :own-window t :label title :exit-boxes exit-boxes)
	(setf pathname
	  (if pathname
	      (accept 'pathname :prompt "File" :default pathname
		      :view view
		      :stream stream)
	    (accept 'pathname :prompt "File"
		    :view view
		    :stream stream))))))

(defmethod silica::port-set-pane-text-style ((port openlook-port) pane m text-style)
  (when (typep m 'xt::xt-root-class)
    (tk::set-values m :font (text-style-mapping port text-style))))



(defmethod find-widget-resource-initargs-for-sheet :around
    ((port openlook-port) (sheet t) &key text-style)
  (let ((initargs (call-next-method))
	(text-style (or text-style
			(sheet-text-style port sheet))))
    `(:font ,(text-style-mapping port text-style)
	    ,@initargs)))


(defmethod find-application-resource-initargs :around
	   ((port openlook-port))
  (let ((initargs (call-next-method))
	(text-style (or (getf (get-application-resources port) :text-style)
			*default-text-style*)))
    `(:font ,(text-style-mapping port text-style) ,@initargs)))
