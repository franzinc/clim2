;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: xt-gadgets.lisp,v 1.48.28.2 2000/07/10 17:00:14 cley Exp $

(in-package :xm-silica)

(defclass ask-widget-for-size-mixin () ())

(defmethod compose-space ((pane ask-widget-for-size-mixin) &key width height)
  (multiple-value-bind
      (x y width height borderwidth
       care-x care-y care-width care-height care-borderwidth)
      (tk::widget-best-geometry (sheet-direct-mirror pane) :width
				width :height height)
    (declare (ignore x y borderwidth care-x care-y care-width
		     care-height care-borderwidth))
    (make-space-requirement :width width :height height)))


;;; Pane

(defmethod find-widget-class-and-name-for-sheet :around
	   ((port xt-port) (parent t)
	    (pane pane))
  ;; Include the name of the pane
  (multiple-value-bind
      (class name)
      (call-next-method)
    (values class
	    (or name
		(pane-name pane)))))

;; Background/foreground/text-style mixin

(defmethod find-widget-initargs-for-sheet :around
	   ((port xt-port) (parent t)
	    (sheet sheet-with-resources-mixin))
  (append (call-next-method)
	  (find-widget-resource-initargs-for-sheet port sheet)))

(defun ensure-color (c palette)
  (etypecase c
    (color c)
    (string (find-named-color c palette))
    (integer (make-device-color palette c))))

(defmethod find-widget-resource-initargs-for-sheet
    ((port xt-port) (sheet sheet-with-resources-mixin)
     &key background foreground)
  (let ((bg (or background (pane-background sheet) *default-pane-background*))
	(fg (or foreground (pane-foreground sheet) *default-pane-foreground*)))
    (with-sheet-medium (medium sheet)
      (let ((palette (medium-palette medium)))
	`(,@(when bg
	      (decode-gadget-background medium sheet
					(ensure-color bg palette)))
	  ,@(when fg
	      (decode-gadget-foreground medium sheet
					(ensure-color fg palette))))))))

(defmethod find-widget-resource-initargs-for-sheet
    ((port xt-port) (sheet t) &key background foreground)
  (let* ((resources (get-application-resources port))
	 (bg (or background (getf resources :background) *default-pane-background*))
	 (fg (or foreground (getf resources :foreground) *default-pane-foreground*))
	 (palette (port-default-palette port)))
    `(:background ,(decode-color-in-palette (ensure-color bg palette)
					    palette)
      :foreground ,(decode-color-in-palette (ensure-color fg palette)
					    palette))))

(defmethod decode-gadget-background (medium sheet ink)
  (declare (ignore sheet))
  (let ((pixel (decode-color ink medium)))
    (list :background pixel)))

(defmethod decode-gadget-background (medium sheet (ink pattern))
  (declare (ignore sheet))
  (let ((pixmap (pixmap-from-pattern ink medium :pixmap)))
    (list :background-pixmap pixmap)))

(defmethod decode-gadget-foreground (medium sheet ink)
  (declare (ignore sheet))
  (let ((pixel (decode-color ink medium)))
    (list :foreground pixel)))

(defmethod silica::port-set-pane-foreground ((port xt-port) pane m ink)
  (when (typep m 'xt::xt-root-class)
    (with-sheet-medium (medium pane)
      (apply #'tk::set-values m (decode-gadget-foreground medium pane ink)))))

(defmethod silica::port-set-pane-foreground :after
    ((port xt-port) pane (m tk::composite) ink)
  (dolist (child (tk::widget-children m))
    (silica::port-set-pane-foreground port pane child ink)))

(defmethod silica::port-set-pane-background ((port xt-port) pane m ink)
  (when (typep m 'xt::xt-root-class)
    (with-sheet-medium (medium pane)
      (apply #'tk::set-values m (decode-gadget-background medium pane ink)))))

(defmethod silica::port-set-pane-background :after
    ((port xt-port) pane (m tk::composite) ink)
  (dolist (child (tk::widget-children m))
    (silica::port-set-pane-background port pane child ink)))

(defmethod sheet-text-style :around ((port xt-port) (sheet t))
   (or (call-next-method)
       *default-text-style*))

(defmethod sheet-text-style ((port xt-port) (sheet sheet-with-resources-mixin))
  (pane-text-style sheet))

(defmethod sheet-text-style ((port xt-port) (sheet t))
  (getf (get-application-resources port) :text-style))



(defclass xt-pane (basic-pane)
	  ;;--- Is this useful a hack enabling things to be passed through
	  ;;--- to the mirror
	  ((silica::mirror-initargs  :initarg :mirror-initargs))
  (:default-initargs :mirror-initargs nil))

(defmethod find-widget-initargs-for-sheet :around ((port xt-port)
						   (parent t)
						   (sheet xt-pane))
  (append (slot-value sheet 'silica::mirror-initargs)
	  (call-next-method)))

(defmethod note-gadget-activated :after ((client t) (gadget xt-pane))
  (let (m)
    (when (setq m (sheet-direct-mirror gadget))
      (xt::set-sensitive m t))))

(defmethod note-gadget-deactivated :after ((client t) (gadget xt-pane))
  (let (m)
    (when (setq m (sheet-direct-mirror gadget))
      (xt::set-sensitive m nil))))

;(defclass xt-composite-pane () ())

(defclass xt-leaf-pane (sheet-permanently-enabled-mixin
			client-overridability-mixin
			mirrored-sheet-mixin
			ask-widget-for-size-mixin
			xt-pane)
	  ())

(defclass xt-top-level-sheet (top-level-sheet)
	  ((accelerator-gestures :initform nil :reader top-level-sheet-accelerator-gestures)))

;;-- Is this safe?

(defmethod sheet-transformation ((sheet xm-silica::xt-top-level-sheet))
  (let (m)
    (if (setq m (sheet-direct-mirror sheet))
	(multiple-value-bind (x y) (tk::get-values (tk::widget-parent m) :x :y)
	  (make-translation-transformation  x y))
      +identity-transformation+)))

;;; this needs to return region in co-ordinate system of parent.
;;; However CLIM's idea of parent is the graft, while Xt's is the
;;; shell. So we have to use the x and y from the shell to make things
;;; consistent (cim)

(defmethod mirror-region* ((port xt-port) (sheet xt-top-level-sheet))
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (multiple-value-bind (x y)
	  (get-values (tk::widget-parent mirror) :x :y)
	(multiple-value-bind (width height)
	    (get-values mirror :width :height)
	  (values (coordinate x) (coordinate y)
		  (coordinate (+ x width)) (coordinate (+ y height))))))))

(defmethod top-level-sheet-accelerator-gestures ((sheet top-level-sheet)) nil)

(defmethod sheet-disown-child :after ((sheet xt-top-level-sheet) (child basic-sheet))
  (setf (slot-value sheet 'accelerator-gestures) nil))

;;--- This isn't really right.  Genera and CLX ports have kludges
;;--- for the time being, too.
(defmethod sheet-shell (sheet)
  (do ((w (sheet-mirror sheet) (tk::widget-parent w)))
      ((typep w '(or tk::shell null))
       w)))

(defmethod add-sheet-callbacks :after
    ((port xt-port) (sheet xt-top-level-sheet) widget)
  (tk::add-event-handler (tk::widget-parent widget)
			 '(:structure-notify)
			 1
			 'sheet-mirror-event-handler
			 sheet))


;;; scroll bar utilities

(defvar *scroll-bar-quantization* most-positive-fixnum)

(defun convert-scroll-bar-value-out (scroll-bar value)
  (multiple-value-bind
      (smin smax) (gadget-range* scroll-bar)
    (max 0
	 (min *scroll-bar-quantization*
	      (floor
	       (compute-symmetric-value
		smin smax value 0 *scroll-bar-quantization*))))))

(defun convert-scroll-bar-value-in (scroll-bar value)
  (multiple-value-bind
      (smin smax) (gadget-range* scroll-bar)
    (max smin
	 (min smax
	      (compute-symmetric-value
	       0 *scroll-bar-quantization* value smin smax )))))

(defun compute-new-scroll-bar-values (scroll-bar value slider-size 
				      line-increment
				      &optional (page-increment slider-size))
  ;; It needs to be the case that (<= (+ val slider-size) <maximum-sv-value>).
  ;; This can fail to be true because of rounding errors when
  ;; converting from float to integer, which cause motif to whine I
  ;; assume the maximum sv value is *scroll-bar-quantization* (I'm not
  ;; sure if this is always true), and round the slider size down if
  ;; the contraint is violated.
  (let ((val (and value (convert-scroll-bar-value-out scroll-bar value)))
	(ss (and slider-size
		 (max 1 
		      (convert-scroll-bar-value-out scroll-bar slider-size)))))
    (values val (and ss			;what assumptions about nullness are
		     (if val		;safe?
			 (min ss (- *scroll-bar-quantization* val))
			 ss))
	    (and line-increment
		 (max 1 (convert-scroll-bar-value-out scroll-bar 
						      line-increment)))
	    (and page-increment
		 (max 1 (convert-scroll-bar-value-out scroll-bar 
						      page-increment))))))
    


(defun wait-for-callback-invocation (port predicate &optional (whostate "Waiting for callback"))
  (if (eq mp:*current-process* (port-process port))
      (progn
	(loop
	  (when (funcall predicate) (return nil))
	  (process-next-event port)))
    (mp:process-wait whostate predicate)))

;; accelerator and mnemonic support

(defun set-button-mnemonic (menubar button mnem)
  (when mnem
    (record-accelerator menubar (list mnem :meta))
    (tk::set-values button :mnemonic mnem)))

(defun record-accelerator (menubar gesture)
  (let ((sheet (frame-top-level-sheet (pane-frame menubar))))
    (push gesture (slot-value sheet 'accelerator-gestures))))

(defmethod distribute-event ((port xt-port) (event keyboard-event))
  (unless (discard-accelerator-event-p port event)
    (call-next-method)))

(defmethod discard-accelerator-event-p ((port xt-port) event)
  (let ((frame (pane-frame (event-sheet event))))
    (and frame
	 (some #'(lambda (gesture)
		   (clim-internals::keyboard-event-matches-gesture-name-p event gesture port))
	       (top-level-sheet-accelerator-gestures (frame-top-level-sheet frame))))))



;; Xt-orriented-gadget

(defclass xt-oriented-gadget () ())

(defmethod find-widget-initargs-for-sheet :around ((port xt-port)
						   (parent t)
						   (sheet xt-oriented-gadget))
  (let ((initargs (call-next-method)))
    (with-accessors ((orientation gadget-orientation)) sheet
      (unless (getf initargs :orientation)
        (setf (getf initargs :orientation) orientation)))
    initargs))

(defmethod (setf gadget-orientation) :after (nv (gadget xt-oriented-gadget))
  (when (sheet-direct-mirror gadget)
    (set-widget-orientation gadget nv)))

(defmethod set-widget-orientation ((gadget xt-oriented-gadget) nv)
  (tk::set-values (sheet-direct-mirror gadget) :orientation nv))

(defun normalize-space-for-text-field-or-label (sheet sr)
  (multiple-value-bind (width min-width max-width height min-height max-height)
      (space-requirement-components sr)
    (if (and (numberp width)
	     (numberp min-width)
	     (numberp max-width))
	sr
      (make-space-requirement
       :width (and width (process-width-specification sheet width))
       :min-width (and min-width (process-width-specification sheet min-width))
       :max-width (and max-width (process-width-specification sheet max-width))
       :height height
       :min-height min-height
       :max-height max-height))))


(defun normalize-space-requirement-for-text-editor (sheet sr)
  (multiple-value-bind (width min-width max-width height min-height max-height)
      (space-requirement-components sr)
    (if (and (numberp width)
	     (numberp min-width)
	     (numberp max-width)
	     (numberp height)
	     (numberp min-height)
	     (numberp max-height))
	sr
      (make-space-requirement
       :width (and width (process-width-specification sheet width))
       :min-width (and min-width (process-width-specification sheet min-width))
       :max-width (and max-width (process-width-specification sheet max-width))
       :height (and height (process-height-specification sheet height))
       :min-height (and min-height (process-height-specification sheet min-height))
       :max-height (and max-height (process-height-specification sheet max-height))))))

;; Patched from spr17194 to make backspace and arrow keys work
;; as accelerators.
(defparameter xm-silica::*funny-accelerator-characters*
    '(
      ((#\\ :\\) "backslash")
      ((#\space :\ ) "space")
      ((#\: :\: ) "colon")
      ((#\, :\, ) "comma")
      ((:left-arrow) "Left")
      ((:right-arrow) "Right")
      ((:up-arrow) "Up")
      ((:down-arrow) "Down")
      ((#\backspace) "BackSpace")
      ))

(defun xm-silica::get-accelerator-text (keystroke &optional olit)
  (let ((key (car keystroke)))
    (let ((x (assoc key xm-silica::*funny-accelerator-characters*
		    :test #'member)))
      (values (format nil
		      (if olit "<~A>" "<Key>~A")
		      (if x (second x) key))
	      (if x (second x)
		(format nil "~:@(~A~)" key))))))


