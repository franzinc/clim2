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
;; $fiHeader: ol-gadgets.lisp,v 1.4 92/02/14 18:57:38 cer Exp $


(in-package :xm-silica)

(defmethod realize-pane-class ((framem openlook-frame-manager) class &rest options) 
  (declare (ignore options))
  (second (assoc class '((scroll-bar openlook-scrollbar)
			 (slider openlook-slider)
			 (push-button openlook-push-button)
			 (text-field openlook-text-field)
			 (toggle-button openlook-toggle-button)
			 (menu-bar openlook-menu-bar)
			 (viewport ol-viewport)
			 (radio-box openlook-radio-box)
			 (frame-pane openlook-frame-pane)
			 (top-level-sheet openlook-top-level-sheet)
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


;;;;;;;;;;;;;;;;;;;;

(defclass openlook-scrollbar (xt-leaf-pane
			   silica::scrollbar)
	  ())

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
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
  (declare (ignore args))
  (tk::set-values
   (sheet-direct-mirror sb)
   :proportion-length slider-size
   :slider-value value))


(defmethod add-sheet-callbacks ((port openlook-port) (sheet openlook-scrollbar) (widget t))
  (tk::add-callback widget
		    :slider-moved
		    'scrollbar-changed-callback-1
		    sheet))

(defmethod scrollbar-changed-callback-1 ((widget t) (sheet openlook-scrollbar))
  (multiple-value-bind
      (value size)
      (tk::get-values widget :slider-value :proportion-length)
    (silica::scrollbar-value-changed-callback
     sheet
     (gadget-client sheet)
     (gadget-id sheet)
     value
     size)))

(defmethod compose-space ((m openlook-scrollbar) &key width height)
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
			 0
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

(warn "This is really bogus")

(defmethod stream-read-char-no-hang ((x openlook-top-level-sheet))
  nil)

(defmethod find-widget-class-and-initargs-for-sheet ((port openlook-port)
						     (parent t)
						     (sheet openlook-top-level-sheet))
  (values 'tk::draw-area
	  (list :layout :ignore)))


;; OpenLook viewport

(defclass ol-viewport
	  (viewport
	   mirrored-sheet-mixin)
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

