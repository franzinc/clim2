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
;; $fiHeader: ol-gadgets.lisp,v 1.12 92/05/13 17:11:12 cer Exp Locker: cer $


(in-package :xm-silica)

(defmethod make-pane-class ((framem openlook-frame-manager) class &rest options) 
  (declare (ignore options))
  (second (assoc class '((scroll-bar openlook-scroll-bar)
			 (slider openlook-slider)
			 (push-button openlook-push-button)
			 (label-pane openlook-label-pane)
			 (text-field openlook-text-field)
			 (text-editor openlook-text-editor)
			 (toggle-button openlook-toggle-button)
			 (menu-bar openlook-menu-bar)
			 (viewport ol-viewport)
			 (radio-box openlook-radio-box)
			 (frame-pane openlook-frame-pane)
			 (top-level-sheet openlook-top-level-sheet)
			 (list-pane openlook-list-pane)
			 (option-pane openlook-option-pane)))))


;;;;;;;;;;;;;;;;;;;;

(defclass openlook-scroll-bar (scroll-bar
			       xt-leaf-pane)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-scroll-bar))
  (with-accessors ((orientation gadget-orientation)) sheet
		  (values 'tk::scrollbar
			  (list :orientation orientation))))

(defmethod (setf silica::scroll-bar-size) (nv (sb openlook-scroll-bar))
  ;; (tk::set-values (sheet-direct-mirror sb) :slider-size nv)
  nv)

(defmethod (setf silica::scroll-bar-value) (nv (sb openlook-scroll-bar))
  (tk::set-values (sheet-direct-mirror sb) :slider-value nv)
  nv)


(defmethod change-scroll-bar-values ((sb openlook-scroll-bar) &key slider-size value)
  (let ((mirror (sheet-direct-mirror sb)))
    (multiple-value-bind
	(mmin mmax) (tk::get-values mirror :slider-min :slider-max)
      (multiple-value-bind
	  (real-value real-size) (compute-new-scroll-bar-values sb mmin mmax value slider-size)
	(tk::set-values
	 mirror
	 :proportion-length  real-size
	 :slider-value real-value)))))

(defmethod add-sheet-callbacks ((port openlook-port) (sheet openlook-scroll-bar) (widget t))
  (tk::add-callback widget
		    :slider-moved
		    'scroll-bar-changed-callback-1
		    sheet))

(defmethod scroll-bar-changed-callback-1 ((widget t) (sheet openlook-scroll-bar))
  (multiple-value-bind
      (smin smax) (gadget-range* sheet)
    (multiple-value-bind
	(value size mmin mmax)
	(tk::get-values widget :slider-value :proportion-length :slider-min :slider-max)
      (scroll-bar-value-changed-callback
       sheet
       (gadget-client sheet)
       (gadget-id sheet)
       (compute-symmetric-value
	mmin mmax value smin smax)
       (compute-symmetric-value
	mmin mmax size smin smax)))))

(defmethod compose-space ((m openlook-scroll-bar) &key width height)
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

(defmethod add-sheet-callbacks ((port openlook-port) (sheet t) (widget tk::draw-area))
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
			 sheet))

;;; top level sheet

(defclass openlook-top-level-sheet (top-level-sheet) ())

(defmethod add-sheet-callbacks :after ((port openlook-port) 
				       (sheet openlook-top-level-sheet)
				       widget)
  (tk::add-callback widget 
		    :resize-callback 'sheet-mirror-resized-callback
		    sheet))

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-top-level-sheet))
  (values 'tk::draw-area
	  (list :layout :ignore)))


;; OpenLook viewport

(defclass ol-viewport
	  (mirrored-sheet-mixin
	   viewport)
    ())

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet ol-viewport))
  (values 'tk::draw-area
	  '(:layout :ignore)))

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

(defclass openlook-menu-bar (xt-leaf-pane) 
	  ((command-table :initarg :command-table)))

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-menu-bar))
  (values 'tk::control nil))

(defmethod realize-mirror :around ((port openlook-port) (sheet openlook-menu-bar))

  ;; This code fills the menu-bar. If top level items do not have
  ;; submenus then it creates one with a menu of its own
  
  (let ((mirror (call-next-method)))
    (labels ((make-menu-for-command-table (command-table parent top)
	       (map-over-command-table-menu-items
		#'(lambda (menu keystroke item)
		    (declare (ignore keystroke))
		    (let ((type (command-menu-item-type item)))
		      (case type
			(:divider)
			(:function
			 ;;--- Do this sometime
			 )
			(:menu
			 (let* ((mb (make-instance 'tk::menu-button
						  :parent parent
						  :label menu)))
			   (make-menu-for-command-table
			    (find-command-table (second item))
			    (tk::get-values mb :menu-pane)
			    nil)))
			(t
			 (let ((button 
				(make-instance 'tk::oblong-button
					       :label menu
					       :managed t
					       :parent parent)))
			   (tk::add-callback
			    button
			    :select
			    'command-button-callback-ol
			    (slot-value sheet 'silica::frame)
			    item))))))
		command-table)))
      (make-menu-for-command-table
       (slot-value sheet 'command-table)
       mirror
       t))
    mirror))

(defun command-button-callback-ol (button frame item)
  (command-button-callback button nil frame item))


;;; Label pane

(defclass openlook-label-pane (label-pane xt-leaf-pane) 
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-label-pane))
  (with-accessors ((label gadget-label)
		   (alignment gadget-alignment)) sheet
    (values 'tk::static-text
	    (append
	     (list :alignment 
		   (ecase alignment
		     ((:left nil) :left)
		     (:center :center)
		     (:right :right)))
	     (and label (list :string label))))))
  
;;; Push button

(defclass openlook-push-button (push-button
				openlook-action-pane
				xt-leaf-pane) 
	  ())



(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-push-button))
  (with-accessors ((label gadget-label)) sheet
    (values 'tk::oblong-button 
	    (and label (list :label label)))))

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
   (make-instance 'activate-gadget-event
		  :gadget sheet)))


;;; Text field

(defclass openlook-text-field (openlook-value-pane 
			       openlook-action-pane
			       text-field
			       xt-leaf-pane)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-text-field))
  (with-accessors ((value gadget-value)) sheet
    (values 'tk::text
	    (append
	     (and value `(:string ,value))))))


;;; Value stuff
;;; I suspect that this is worthless

(defclass openlook-value-pane () ())

(defmethod add-sheet-callbacks :after ((port openlook-port) 
				       (sheet openlook-value-pane) (widget t))
  #+igore
  (tk::add-callback widget
		    :value-changed-callback
		    'queue-value-changed-event
		    sheet))

(defmethod gadget-value ((gadget openlook-value-pane))
  (if (sheet-direct-mirror gadget)
      (tk::get-values (sheet-mirror gadget) :value)
    (call-next-method)))

(defmethod (setf gadget-value) (nv (gadget openlook-value-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (when (sheet-mirror gadget)
    (tk::set-values (sheet-mirror gadget) :value nv)))

(defmethod queue-value-changed-event (widget sheet)
  (declare (ignore widget))
  (distribute-event
   (port sheet)
   (make-instance 'value-changed-gadget-event
		  :gadget sheet
		  :value (gadget-value sheet))))

;;;

(defclass openlook-labelled-gadget () ())

(defmethod find-widget-class-and-initargs-for-sheet
    :around ((port openlook-port)
	     (parent t)
	     (sheet openlook-labelled-gadget))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (with-accessors ((alignment gadget-alignment)
		     (label gadget-label)) sheet
      (when label
	(unless (getf initargs :label)
	  (setf (getf initargs :label) label)))
      (unless (getf initargs :label-justify)
	(setf (getf initargs :label-justify) 
	  (ecase alignment
	    (:center :left)
	    ((:left :right)  alignment)))))
    (values class initargs)))

(defmethod (setf gadget-label) :after (nv (sheet openlook-labelled-gadget))
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) :label (or nv ""))))

(defmethod (setf gadget-alignment) :after (nv (sheet openlook-labelled-gadget))
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) 
		    :label-justify (ecase nv
				     (:center nv)
				     ((:left :right) nv)))))

;; Toggle button

(defclass openlook-toggle-button (toggle-button
				openlook-labelled-gadget
				openlook-value-pane
				xt-leaf-pane)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-toggle-button))
  (with-accessors ((set gadget-value)
		   (indicator-type gadget-indicator-type)) sheet
    (values (typecase (sheet-parent sheet)
	      (openlook-radio-box 'xt::rect-button)
	      (openlook-check-box 'xt::rect-button)
	      (t 'xt::check-box))
	    (append (list :set set)
		    #+dunno
		    (list :indicator-type 
			  (ecase indicator-type
			    (:one-of :one-of-many)
			    (:some-of :n-of-many)))))))

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

(defmethod (setf gadget-value) (nv (gadget openlook-toggle-button) &key invoke-callback)
  (declare (ignore invoke-callback))
  (when (sheet-direct-mirror gadget)
    (tk::set-values (sheet-mirror gadget) :set nv)))




;;; 

;; Openlook-orriented-gadget

(defclass openlook-oriented-gadget () ())

(defmethod find-widget-class-and-initargs-for-sheet :around ((port openlook-port)
							     (parent t)
							     (sheet openlook-oriented-gadget))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (with-accessors ((orientation gadget-orientation)) sheet
      (unless (getf initargs :orientation)
	(setf (getf initargs :orientation) orientation)))
    (values class initargs)))

(defmethod (setf gadget-orientation) :after (nv (gadget openlook-oriented-gadget))
  (when (sheet-direct-mirror gadget)
    (tk::set-values (sheet-direct-mirror gadget) :orientation nv)))


;;

(defclass openlook-radio-box (openlook-geometry-manager 
			      mirrored-sheet-mixin
			      #+ignore ;;-- Need to decide
			      openlook-oriented-gadget
			      sheet-multiple-child-mixin
			      sheet-permanently-enabled-mixin
			      radio-box
			      ask-widget-for-size-mixin
			      pane)
	  ())

(defmethod sheet-adopt-child :after ((gadget openlook-radio-box) child)
  (setf (gadget-client child) gadget))

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-radio-box))
  (values 'xt::exclusives nil))

(defmethod value-changed-callback :around ((v gadget)
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
  (let ((w 0)
	(h 0))
    (dolist (child (tk::widget-children (sheet-direct-mirror rb)))
      (multiple-value-bind
	  (ignore-x igore-y width height)
	  (xt::widget-best-geometry child)
	(maxf h height)
	(incf w width)))
    (make-space-requirement :width w :height h)))
;;;


(defclass openlook-check-box (openlook-geometry-manager
			      mirrored-sheet-mixin
			      #+Dunno ;;--- what do do about tihs
			      openlook-oriented-gadget
			      sheet-multiple-child-mixin
			      sheet-permanently-enabled-mixin
			      check-box
			      ask-widget-for-size-mixin
			      pane)
	  ())

(defmethod sheet-adopt-child :after ((gadget openlook-check-box) child)
  (setf (gadget-client child) gadget))

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-check-box))
  
  (values 'xt::nonexclusives nil))

(defmethod value-changed-callback :around ((v gadget)
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

;;

;; Openlook-orriented-gadget


(defclass openlook-slider (#+ignore openlook-range-pane
			   #+ignore openlook-oriented-gadget
			   xt-leaf-pane
			   slider)
	  ())


(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-slider))
  (with-accessors ((label gadget-label)
		   (show-value-p gadget-show-value-p)
		   (value gadget-value)
		   (orientation gadget-orientation)) sheet
    (multiple-value-bind
	(smin smax) (gadget-range* sheet)
      (let ((mmin 0) 
	    (mmax 100))
	(values 'tk::slider
		(append
		 (list :orientation orientation)
		 #+dunno
		 (and label (list :title-string label))
		 (list :slider-min mmin
		       :slider-max mmax)
		 (and value 
		      (list :slider-value 
			    (fix-coordinate 
				    (compute-symmetric-value
				     smin smax value mmin
				     mmax))))))))))

(defmethod compose-space ((m openlook-slider) &key width height)
  (declare (ignore width height))
  (let ((sr (copy-space-requirement (call-next-method))))
    (ecase (gadget-orientation m)
      (:vertical
       (setf (space-requirement-max-height sr) +fill+))
      (:horizontal
       (setf (space-requirement-max-width sr) +fill+)))
    sr))

#+dunno
(defmethod (setf gadget-show-value-p) :after (nv (sheet openlook-slider)) 
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) :show-value nv)))

#+dunno 
(defmethod (setf gadget-label) :after (nv (sheet openlook-slider))
  (when (sheet-direct-mirror sheet)
    (tk::set-values (sheet-direct-mirror sheet) :title-string (or nv ""))))

;;; 

(defclass openlook-text-editor (openlook-value-pane 
				openlook-action-pane
				text-editor
				xt-leaf-pane)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet
						      openlook-text-editor))
  (with-accessors ((value gadget-value)
		   (ncolumns silica::gadget-columns)
		   (nlines silica::gadget-lines)) sheet
    (values 'tk::text
	    (append
	     (list :edit-mode :multi-line)
	     (and ncolumns (list :columns ncolumns))
	     (and nlines (list :rows nlines))
	     (and value `(:value ,value))))))

(defmethod compose-space ((te openlook-text-editor) &key width height)
  (declare (ignore width height))
  (let ((sr (call-next-method)))
    (setq sr (copy-space-requirement sr))
    ;;-- What it the correct thing to do???
    (setf (space-requirement-max-width sr) +fill+
	  (space-requirement-max-height sr) +fill+)
    sr))

(defmethod silica::gadget-supplied-scrolling (frame-manager frame 
					      (contents openlook-text-editor) 
					      &rest ignore)
  (declare (ignore ignore))
  (with-look-and-feel-realization (frame-manager frame)
    (make-pane 'openlook-scrolling-window :contents contents)))

;;;
;;;--- This code is so much like the motif one I think we could share
;;;--- code.

(defclass openlook-scrolling-window (openlook-geometry-manager
				  ask-widget-for-size-mixin
				  mirrored-sheet-mixin
				  sheet-single-child-mixin
				  sheet-permanently-enabled-mixin
				  pane)
	  ;;-- probably one of the options is whether to have vertical
	  ;;-- and/or horizontal scrollbars
	  ())

(defmethod initialize-instance :after ((pane openlook-scrolling-window) &key contents)
  (sheet-adopt-child pane contents))

(defmethod compose-space ((fr openlook-scrolling-window) &key width height)
  (declare (ignore width height))
  ;;--- This is not quite right because I think scrollbars are a bit
  ;;--- bigger than this. But atleast its a start
  (let ((fudge-factor (+ 16
			 #+ignore
			 (tk::get-values (sheet-mirror fr)
					 :spacing)))
	(sr (copy-space-requirement (compose-space (sheet-child fr)))))
    (incf (space-requirement-width sr) fudge-factor)
    (incf (space-requirement-height sr) fudge-factor)
    ;;--- Is this the correct thing to do???
    (setf (space-requirement-min-width sr) fudge-factor
	  (space-requirement-min-height sr) fudge-factor)
    sr))

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-scrolling-window))
  (values 'xt::scrolled-window nil))
