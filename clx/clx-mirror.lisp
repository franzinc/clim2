;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
;;; $fiHeader: clx-mirror.lisp,v 1.2 92/02/24 13:23:50 cer Exp $
 Portions copyright (c) 1991, 1992 International Lisp Associates."


(defmethod realize-mirror ((port clx-port) sheet)
  (with-slots (display screen) port
    (multiple-value-bind (left top right bottom)
	(sheet-native-region* sheet)
      (integerize-coordinates left top right bottom)
      (let* ((clx-parent (sheet-mirror sheet))
	     (mirror
	       (xlib:create-window 
		 :parent clx-parent
		 :x left :y top
		 :width (- right left) :height (- bottom top)
		 :background (xlib:screen-white-pixel screen)
		 :event-mask 
		 #.(xlib:make-event-mask 
		     :button-press :button-release
		     :key-press :key-release 
		     :pointer-motion :pointer-motion-hint
		     :enter-window :leave-window 
		     :structure-notify
		     :exposure
		     ;; Needed for popup menus
		     :owner-grab-button
		     ))))
	(setf (sheet-native-transformation sheet) +identity-transformation+)
	mirror))))

(defmethod destroy-mirror ((port clx-port) sheet)
  (ignore-errors			
    (xlib:destroy-window (sheet-mirror sheet))
    (port-force-output port)))

(defmethod enable-mirror ((port clx-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (with-slots (display) port
      (xlib:map-window mirror)
      (setf (xlib:window-priority mirror) :top-if)
      (xlib:display-force-output display))))

(defmethod disable-mirror ((port clx-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (with-slots (display) port
      (xlib:unmap-window mirror)
      (port-force-output port))))

;;--- Is this the same as WINDOW-STACK-ON-TOP?
(defmethod raise-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (setf (xlib:window-priority window) :above)
    (port-force-output port)))

;;--- Is this the same as WINDOW-STACK-ON-BOTTOM?
(defmethod bury-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (setf (xlib:window-priority window) :below)
    (port-force-output port)))

(defmethod realize-graft ((port clx-port) graft)
  (with-slots (silica::pixels-per-point silica::pixel-width silica::pixel-height
	       silica::mm-width silica::mm-height silica::units) graft
    (let ((screen (slot-value port 'screen))
	  (window (slot-value port 'window)))
      (setf silica::pixel-width  (xlib:screen-width screen)
	    silica::pixel-height (xlib:screen-height screen)
	    ;;--- Bogus numbers
	    silica::mm-width     360.0
	    silica::mm-height	 280.0
	    silica::pixels-per-point 1)
      (setf (sheet-region graft)
	    (ecase silica::units
	      ((:device :pixel)
	       (make-bounding-rectangle
		 0 0 
		 silica::pixel-width silica::pixel-height))))
      (setf (sheet-native-transformation graft) +identity-transformation+)
      (setf (sheet-direct-mirror graft) window)
      (update-mirror-transformation port graft))))

(defmethod mirror-region* ((port clx-port) sheet)
  (let* ((mirror (sheet-mirror sheet))
	 (x (and mirror (xlib:drawable-x mirror)))
	 (y (and mirror (xlib:drawable-y mirror))))
    (when mirror
      (values x y 
	      (+ x (xlib:drawable-width mirror))
	      (+ y (xlib:drawable-height mirror))))))

(defmethod mirror-region ((port clx-port) sheet)
  (multiple-value-call #'make-bounding-rectangle
    (mirror-region* port sheet)))

(defmethod clx-mirror-native-edges* ((port clx-port) sheet &optional mirror)
  (let ((mirror (or mirror (sheet-mirror sheet))))
    (multiple-value-bind (width height)
	(scl:send mirror :size)
      (multiple-value-bind (xoff yoff)
	  (tv:sheet-calculate-offsets mirror (tv:sheet-screen mirror))
	(values xoff
		yoff
		(+ xoff width)
		(+ yoff height))))))

;;--- Is this really the same as MIRROR-INSIDE-EDGES*?
(defmethod mirror-inside-region* ((port clx-port) sheet)
  (mirror-inside-edges* port sheet))

;;--- Is this really the same as MIRROR-REGION*?
(defmethod mirror-native-edges* ((port clx-port) sheet)
  (mirror-region* port sheet))

(defmethod mirror-inside-edges* ((port clx-port) sheet)
  (let* ((mirror (sheet-mirror sheet))
	 (parent-mirror (sheet-mirror (sheet-parent sheet)))
	 ;; Is this really the only way to get parent?
	 (x-parent (multiple-value-bind (windows parent root)
		       (xlib:query-tree mirror)
		     (declare (ignore windows root))
		     parent))
	 (x (xlib::drawable-x mirror))
	 (y (xlib::drawable-y mirror))
	 (w (xlib::drawable-width mirror))
	 (h (xlib::drawable-height mirror)))
    ;; Can deal with reparenting window managers
    (when (not (eq parent-mirror x-parent))
      (multiple-value-setq (x y)
	(xlib:translate-coordinates
	  x-parent x y parent-mirror)))
    (values x y
	    (+ x w) (+ y h))))
 
(defmethod set-sheet-mirror-edges* ((port clx-port) sheet 
				    left top right bottom)
  (let* ((mirror (sheet-mirror sheet))
	 (display (port-display port))
	 (w (integerize-coordinate (- right left)))
	 (h (integerize-coordinate (- bottom top)))
	 (x (integerize-coordinate left))
	 (y (integerize-coordinate top)))
    (xlib:with-display (display)
      (setf (xlib:drawable-x mirror) x
	    (xlib:drawable-y mirror) y
	    (xlib:drawable-width mirror) w
	    (xlib:drawable-height mirror) h))
    (port-force-output port)))


(defmethod process-next-event ((port clx-port)
			       &key (timeout nil) (wait-function nil)
				    (state "X Event"))
  (declare (optimize (speed 3) (safety 0) (compilation-speed 0)))
  (let ((display (port-display port)))
    (multiple-value-bind (nevents outcome) 
	(xlib:event-listen display timeout)
      (declare (ignore nevents))
      (unless (eq outcome :timeout)
	(xlib:event-case (display :force-output-p nil :discard-p t
				  :timeout nil)
	  ;; Device Events
	  ((:motion-notify :enter-notify :leave-notify)
	   (event-window time) 
	   (multiple-value-bind (x y same-screen-p child state)
	       (xlib:query-pointer event-window)
	     (declare (ignore same-screen-p child))
	     (let ((event-key :pointer-motion)
		   (sheet (mirror->sheet port event-window)))
	       (distribute-event
		 (port sheet)
		 (make-instance 'pointer-motion-event
				:x mouse-x
				:y mouse-y
				:native-x native-x
				:native-y native-y
				:modifiers (current-modifier-state
					     (make-state-from-buttons mouse-buttons)
					     (tv:sheet-mouse mouse-window))
				:sheet sheet)))
	     t))
	  ((:button-press :button-release) 
	   (event-key event-window x y state time code)
	   (let ((sheet (mirror->sheet port event-window))
		 click-type)
	     (distribute-event
	       (port sheet)
	       (make-instance (if (eq event-key :button-press)
				  'pointer-button-press-event
				  'pointer-button-release-event)
			      :x mouse-x
			      :y mouse-y
			      :native-x native-x
			      :native-y native-y
			      :button button
			      :modifiers (current-modifier-state
					   0 (tv:sheet-mouse window))
			      :sheet sheet))
	     t))
	  ((:key-press :key-release)
	   (event-key event-window x y state time code)
	   (let* ((keysym (clx-keysym->keysym
			    (xlib:keycode->keysym 
			      display code
			      (xlib:default-keysym-index display code state))))
		  (modifier-state (state->modifier-state state))
		  ;; Canonicalize the only interesting key right here.
		  ;; If we get a key labelled "Return", we canonicalize it
		  ;; into #\Newline.
		  ;; This may be misguided, but it'll almost certainly help us
		  ;; in the short run.
		  (char (cond ((and (eq keysym ':return)
				    (or (zerop modifier-state)
					(= modifier-state (make-modifier-state :shift))))
			       #\Newline)
			      (t (xlib:keycode->character display code state)))))
	     (distribute-event
	       (port sheet)
	       (make-instance (if (eq event-key :key-press)
				  'key-press-event
				  'key-release-event)
			      :key-name keysym
			      :character char
			      :modifiers (current-modifier-state
					   0 (tv:sheet-mouse window))
			      :sheet sheet))
	     t))
	  ;; window oriented events.
	  (:exposure (event-window x y width height)
	   (let ((sheet (mirror->sheet port event-window)))
	     (when sheet
	       (multiple-value-bind (min-x min-y max-x max-y)
		   (untransform-rectangle*
		     (sheet-native-transformation sheet) 
		     x y (+ x width) (+ y height))
		 (queue-repaint sheet
				(make-rectangle* min-x min-y max-x max-y)))))
	   t)
	  (:map-notify (event-window)
	   (let ((sheet (mirror->sheet port event-window)))
	     (when sheet 
	       ;;--- Used to pass :PORT-TRIGGER T, better check it out
	       (setf (sheet-enabled-p sheet) t))
	     t))
	  (:unmap-notify (event-window)
	   (let ((sheet (mirror->sheet port event-window)))
	     (when sheet
	       ;;--- Used to pass :PORT-TRIGGER T, better check it out
	       (setf (sheet-enabled-p sheet) nil))
	     t))
	  (:configure-notify (event-window ;x y width height
			       #+ignore send-event-p)
	   (let ((sheet (mirror->sheet port event-window)))
	     (when sheet
	       (xlib:with-state (event-window)
		 (mirror-region-updated port sheet)))
	     t))
	  (:destroy-notify ()
	   t)
	  ((:reparent-notify :no-exposure ) () 
	   t)
	  (otherwise (event-key)
	   t))))))
