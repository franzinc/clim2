;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: clx-mirror.lisp,v 1.5 92/05/22 19:27:32 cer Exp $

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 International Lisp Associates."


;;--- This isn't really right, figure out what to do
#-Genera (defmethod sheet-shell (sheet) sheet)


(defmethod realize-mirror ((port clx-port) sheet)
  (with-slots (display screen) port
    (multiple-value-bind (left top right bottom)
	(sheet-native-region* sheet)
      (fix-coordinates left top right bottom)
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
    (xlib:display-force-output (port-display port))))

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
      (xlib:display-force-output (port-display port)))))

;;--- Is this the same as WINDOW-STACK-ON-TOP?
(defmethod raise-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (setf (xlib:window-priority window) :above)
    (xlib:display-force-output (port-display port))))

;;--- Is this the same as WINDOW-STACK-ON-BOTTOM?
(defmethod bury-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (setf (xlib:window-priority window) :below)
    (xlib:display-force-output (port-display port))))

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

#+ignore	;--- nobody seems to use this
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
	 (w (fix-coordinate (- right left)))
	 (h (fix-coordinate (- bottom top)))
	 (x (fix-coordinate left))
	 (y (fix-coordinate top)))
    (xlib:with-display (display)
      (setf (xlib:drawable-x mirror) x
	    (xlib:drawable-y mirror) y
	    (xlib:drawable-width mirror) w
	    (xlib:drawable-height mirror) h))
    (xlib:display-force-output (port-display port))))


;; Chords aren't supported yet
(defmacro x-button-code-to-event-button (x-code)
  `(case ,x-code
     (1 256)
     (2 512)
     (3 1024)))

(defmacro x-button-code-to-event-button (x-code)
  `(case ,x-code
     (1 256)
     (2 512)
     (3 1024)))

(defmethod process-next-event ((port clx-port)
			       &key (timeout nil) (wait-function nil)
				    (state "X Event"))
  ;; In LCL compiling with this higher safety will add fatal type-checking.
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
	   (event-window) 
	   (multiple-value-bind (x y same-screen-p child state root-x root-y)
	       (xlib:query-pointer event-window)
	     (declare (ignore same-screen-p child))

	     (let ((sheet (mirror->sheet port event-window))
		   (modifier-state (state-mask->modifier-state state display)))
	       (distribute-event
		 port				;(EQ PORT (PORT-SHEET)) ==> T
		 (make-instance 'pointer-motion-event
		   :x x
		   :y y
		   :native-x root-x
		   :native-y root-y
		   :button nil			;--- fill this in
		   :modifiers modifier-state
		   :pointer (port-pointer port)
		   :sheet sheet)))
	     t))
	  ((:button-press :button-release) 
	   (event-key event-window code)
	   (multiple-value-bind (x y same-screen-p child state root-x root-y)
	       (xlib:query-pointer event-window)
	     (declare (ignore same-screen-p child))
	     (let ((sheet (mirror->sheet port event-window))
		   (modifier-state (state-mask->modifier-state state display)))
	       (distribute-event
		 port 
		 (make-instance (if (eq event-key :button-press)
				    'pointer-button-press-event
				    'pointer-button-release-event)
		   :x x
		   :y y
		   :native-x root-x
		   :native-y root-y
		   :button (x-button-code-to-event-button code)
		   :modifiers modifier-state
		   :pointer (port-pointer port)
		   :sheet sheet)))
	     t))
	  ((:key-press :key-release)
	   (event-key event-window x y state code)
	   (let* ((keysym (clx-keysym->keysym
			    (xlib:keycode->keysym 
			      display code
			      (xlib:default-keysym-index display code state))))
		  (sheet (mirror->sheet port event-window))
		  (modifier-state (state-mask->modifier-state state display))
		  ;; Canonicalize the only interesting key right here.
		  ;; If we get a key labelled "Return", we canonicalize it
		  ;; into #\Newline.
		  ;; This may be misguided, but it'll almost certainly help us
		  ;; in the short run.
		  (char (cond ((and (eq keysym ':return)
				    (or (zerop modifier-state)
					(= modifier-state #.(make-modifier-state :shift))))
			       #\Newline)
			      (t (xlib:keycode->character display code state)))))
	     (when (characterp char)
	       (distribute-event
		 port
		 (make-instance (if (eq event-key :key-press)
				    'key-press-event
				    'key-release-event)
		   :key-name keysym
		   :character char
		   :modifiers modifier-state
		   :sheet sheet)))
	     t))
	  ;; window oriented events.
	  ((:exposure :graphics-exposure)
	   (event-window x y width height)
	   (let ((sheet (mirror->sheet port event-window)))
	     (when sheet
	       (multiple-value-bind (min-x min-y max-x max-y)
		   (untransform-rectangle*
		     (sheet-native-transformation sheet) 
		     x y (+ x width) (+ y height))
		 (queue-repaint
		   sheet
		   (make-instance 'window-repaint-event
		     :native-region (sheet-native-region sheet)
		     :region (make-bounding-rectangle min-x min-y max-x max-y)
		     :sheet sheet))))
	     t))
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
			       #+++ignore send-event-p)
	   (let ((sheet (mirror->sheet port event-window)))
	     (when sheet
	       (xlib:with-state (event-window)
		 (mirror-region-updated port sheet)))
	     t))
	  (:destroy-notify ()
	   t)
	  ((:reparent-notify :no-exposure ) () 
	   t)
	  
	  (:client-message (type data event-window)
	   (case type
	     (:wm_protocols
	       (let ((atom (xlib:atom-name x-display (aref data 0))))
		 (case atom
		   (:wm_delete_window
		     (let* ((sheet (mirror->sheet port event-window))
			    (frame (pane-frame sheet)))
		       (if frame
			   (disown-frame (frame-manager frame) frame)
			   (format *error-output*
			       "CLX delete window message for non-frame sheet ~S"
			     sheet))))
		   (t (format *error-output*
			  "Unknown CLX ~S client message data ~S"
			':wm_protocols atom))))))
	   t))))))
