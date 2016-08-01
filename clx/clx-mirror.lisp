;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clx-clim)

;;;"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1991, 1992 International Lisp Associates."


;;--- This isn't really right, figure out what to do
#-Genera (defmethod sheet-shell (sheet) sheet)


;; In the raw CLX port, we only create top-level windows.  All the other
;; "windows" in a CLIM frame are managed by CLIM itself.
(defmethod realize-mirror ((port clx-port) sheet)
  (with-slots (display screen) port
    (multiple-value-bind (left top right bottom)
	(sheet-native-region* sheet)
      (fix-coordinates left top right bottom)
      (let* ((clx-parent (sheet-mirror sheet))
	     (frame (pane-frame sheet))
	     (save-under (and frame (getf (frame-properties frame) :save-under)))
	     (mirror
	       (xlib:create-window 
		 :parent clx-parent
		 :x left :y top
		 :width (- right left) :height (- bottom top)
		 :bit-gravity :north-west
		 :save-under (if save-under :on nil)
		 :event-mask 
		   #.(xlib:make-event-mask 
		       :button-press :button-release
		       :key-press :key-release 
		       :pointer-motion :pointer-motion-hint
		       :enter-window :leave-window 
		       :structure-notify
		       :exposure
		       ;; Needed for popup menus
		       :owner-grab-button))))
	(setf (sheet-native-transformation sheet) +identity-transformation+)
	(xlib:set-wm-properties mirror
	  :input :on
	  :initial-state :normal
	  ;; pre-X11R5 doesn't hack the next two
	  :program-specified-position-p t
	  :program-specified-size-p t
	  ;; pre-X11R5 uses "user" instead of "program"
	  ;;--- We should get these from the top-level sheet
	  :user-specified-size-p t 
	  :user-specified-position-p t
	  ;; Some X11R3 servers want the next four
	  :x left :y top
	  :width (- right left) :height (- bottom top)
	  ;; Sigh, keep us out of the debugger
	  :allow-other-keys t)
	(setf (xlib:wm-protocols mirror) '(:wm_delete_window))
	(setf (xlib:wm-client-machine mirror) (short-site-name))
	;; Doing the following means that users may add lines to a resources
	;; file (such as .Xdefaults) to control what the window manager does
	;; with clim windows, e.g.,
	;;   olwm.MinimalDecor: clim
	;; Also, users can write standard XLIB calls in C that do interesting
	;; things to CLIM windows, such as changing the mouse cursor of CLIM
	;; windows to a wristwatch during GC operations.
	(xlib:set-wm-class mirror "clim" "clim")
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
    (with-slots (display screen) port
      ;; WITHDRAW-WINDOW isn't in X11R3, so use UNMAP-WINDOW instead
      (if (fboundp 'xlib::withdraw-window)
	  (xlib::withdraw-window mirror screen)
	  (xlib:unmap-window mirror))
      (xlib:display-force-output display))))

(defmethod raise-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (setf (xlib:window-priority window) :above)
    (xlib:display-force-output (port-display port))))

(defmethod bury-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (setf (xlib:window-priority window) :below)
    (xlib:display-force-output (port-display port))))

(defmethod mirror-visible-p ((port clx-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    ;;--- This costs a round trip to the display server.  A better
    ;;--- scheme would be to have the map/unmap-notify set a flag
    ;;--- that we could simply read here, as in CLIM 1.1
    (not (eq (xlib:window-map-state mirror) :unmapped))))

(defmethod realize-graft ((port clx-port) graft)
  (with-slots (silica::pixels-per-point silica::pixel-width silica::pixel-height
	       silica::mm-width silica::mm-height silica::units) graft
    (let ((screen (slot-value port 'screen))
	  (window (slot-value port 'window)))
      (setf silica::pixel-width  (xlib:screen-width screen)
	    silica::pixel-height (xlib:screen-height screen)
	    silica::mm-width     (xlib:screen-width-in-millimeters screen)
	    silica::mm-height	 (xlib:screen-height-in-millimeters screen))
      (let* ((ppp
	       ;; Geometric mean in case the pixels aren't square.  Sigh.
	       (* (sqrt (* (/ silica::pixel-width silica::mm-width)
			   (/ silica::pixel-height silica::mm-height)))
		  (/ 25.4s0 72)))
	     (rounded-ppp (round ppp)))
	(setf silica::pixels-per-point 
	      (if (< (abs (- ppp rounded-ppp)) .1s0) rounded-ppp ppp)))
      (setf (sheet-region graft)
	    (ecase silica::units
	      ((:device :pixel)
	       (make-bounding-rectangle
		 0 0 
		 silica::pixel-width silica::pixel-height))))
      (setf (sheet-native-transformation graft) +identity-transformation+)
      (setf (sheet-direct-mirror graft) window)
      (update-mirror-transformation port graft))))

(defmethod clx-mirror-native-edges* ((port clx-port) sheet &optional mirror)
  (let* ((mirror (or mirror (sheet-mirror sheet)))
	 (x (xlib::drawable-x mirror))
	 (y (xlib::drawable-y mirror))
	 (width (xlib::drawable-width mirror))
	 (height (xlib::drawable-height mirror))
	 (parent-mirror (sheet-mirror (sheet-parent sheet)))
	 ;; Is this really the only way to get parent?
	 (x-parent (multiple-value-bind (windows parent root)
		       (xlib:query-tree mirror)
		     (declare (ignore windows root))
		     parent)))
    ;;--- Need this to deal with reparenting window managers
    #+++ignore
    (when (not (eq parent-mirror x-parent))
      (multiple-value-setq (x y)
	(xlib:translate-coordinates
	  x-parent x y parent-mirror)))
    (values (coordinate x) (coordinate y)
	    (coordinate (+ x width)) (coordinate (+ y height)))))

;; Returns left,top,right,bottom
(defmethod mirror-region* ((port clx-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (clx-mirror-native-edges* port sheet mirror))))

;; Returns x,y,width,height
(defmethod mirror-inside-region* ((port clx-port) sheet)
  (multiple-value-bind (left top right bottom)
      (mirror-region* port sheet)
    (values (coordinate 0) (coordinate 0)
	    (- right left) (- bottom top))))

;;--- Shouldn't this be the same as MIRROR-REGION*?
(defmethod mirror-native-edges* ((port clx-port) sheet)
  (let ((mirror (sheet-direct-mirror sheet)))
    (clx-mirror-native-edges* port sheet mirror)))

(defmethod mirror-inside-edges* ((port clx-port) sheet)
  (multiple-value-bind (left top right bottom)
      (mirror-native-edges* port sheet)
    (values (coordinate 0) (coordinate 0)
	    (- right left) (- bottom top))))
 
(defmethod set-sheet-mirror-edges* ((port clx-port) sheet 
				    left top right bottom)
  (let* ((mirror (sheet-direct-mirror sheet))
	 (display (port-display port))
	 (width (fix-coordinate (- right left)))
	 (height (fix-coordinate (- bottom top)))
	 (x (fix-coordinate left))
	 (y (fix-coordinate top)))
    (xlib:with-display (display)
      (setf (xlib:drawable-x mirror) x
	    (xlib:drawable-y mirror) y
	    (xlib:drawable-width mirror) width
	    (xlib:drawable-height mirror) height))
    (xlib:display-force-output (port-display port))))


;; Chords aren't supported yet
(defmacro x-button-code->event-button (x-code)
  `(case ,x-code
     (1 +pointer-left-button+)
     (2 +pointer-middle-button+)
     (3 +pointer-right-button+)))

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
	(xlib:event-case (display :force-output-p nil
				  :discard-p t
				  :timeout nil)
	  ;; Device Events
	  ((:motion-notify)
	   (event-window) 
	   (multiple-value-bind (x y same-screen-p child state root-x root-y)
	       (xlib:query-pointer event-window)
	     (declare (ignore same-screen-p child))
	     (let ((sheet (mirror->sheet port event-window))
		   (modifier-state (state-mask->modifier-state state display))
		   (pointer (port-pointer port)))
	       ;;--- This should probably set the native position of the pointer
	       (when sheet
		 (distribute-event
		   port				;(EQ PORT (PORT-SHEET)) ==> T
		   (allocate-event 'pointer-motion-event
		     :x root-x
		     :y root-y
		     :native-x x
		     :native-y y
		     :modifier-state modifier-state
		     :pointer pointer
		     :sheet sheet))))
	     t))
	  ((:enter-notify :leave-notify)
	   (event-window event-key state kind x y)
	   (let ((sheet (mirror->sheet port event-window))
		 (modifier-state (state-mask->modifier-state state display))
		 (pointer (port-pointer port)))
	     ;;--- This should probably set the native position of the pointer
	     (when sheet
	       (distribute-event
		 port				;(EQ PORT (PORT-SHEET)) ==> T
		 (allocate-event (case event-key
				   (:enter-notify 'pointer-enter-event)
				   (:leave-notify 'pointer-exit-event))
		   :native-x x
		   :native-y y
		   :kind kind
		   :modifier-state modifier-state
		   :pointer pointer
		   :sheet sheet))))
	   t)
	  ((:button-press :button-release) 
	   (event-window event-key state code x y)
	   (let ((sheet (mirror->sheet port event-window))
		 (modifier-state (state-mask->modifier-state state display))
		 (button (x-button-code->event-button code))
		 (pointer (port-pointer port)))
	     (when sheet
	       (distribute-event
		 port 
		 (allocate-event (if (eq event-key :button-press)
				     'pointer-button-press-event
				     'pointer-button-release-event)
		   :native-x x
		   :native-y y
		   :button button
		   :modifier-state modifier-state
		   :pointer pointer
		   :sheet sheet))))
	   t)
	  ((:key-press :key-release)
	   (event-key event-window state code)
	   (let* ((sheet (mirror->sheet port event-window))
		  (keysym (clx-keysym->keysym
			    (xlib:keycode->keysym 
			      display code
			      (xlib:default-keysym-index display code state))))
		  (keysym-shift-mask
		    (if (typep keysym 'modifier-keysym)
			(make-modifier-state
			  (case keysym
			    ((:left-shift :right-shift) :shift)
			    ((:left-control :right-control) :control)
			    ((:left-meta :right-meta) :meta)
			    ((:left-super :right-super) :super)
			    ((:left-hyper :right-hyper) :hyper)))
			0))
		  (modifier-state 
		    (if (eq event-key :key-press)
			(logior (state-mask->modifier-state state display)
				keysym-shift-mask)
			(logandc2 (state-mask->modifier-state state display)
				  keysym-shift-mask)))
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
	     (when sheet
	       (distribute-event
		 port
		 (allocate-event (if (eq event-key :key-press)
				     'key-press-event
				     'key-release-event)
		   :key-name keysym
		   :character (and (characterp char) char)
		   :modifier-state modifier-state
		   :sheet sheet)))
	     t))
	  ;; window oriented events.
	  ((:exposure :graphics-exposure)
	   (event-window x y width height)
	   (let ((sheet (mirror->sheet port event-window)))
	     (when sheet
	       (handle-event
		 sheet
		 (allocate-event 'window-repaint-event
		   ;;--- Should this be (MIRROR-REGION PORT SHEET), as
		   ;;--- it is in the :CONFIGURE-NOTIFY case?
		   :native-region (sheet-native-region sheet)
		   :region (make-bounding-rectangle x y (+ x width) (+ y height))
		   :sheet sheet)))
	     t))
	  ((:map-notify) (event-window)
	   (let ((sheet (mirror->sheet port event-window)))
	     (when sheet 
	       ;;--- Used to pass :PORT-TRIGGER T, better check it out
	       (setf (sheet-enabled-p sheet) t))
	     t))
	  ((:unmap-notify) (event-window)
	   (let ((sheet (mirror->sheet port event-window)))
	     (when sheet
	       ;;--- Used to pass :PORT-TRIGGER T, better check it out
	       (setf (sheet-enabled-p sheet) nil))
	     t))
	  ((:mapping-notify) (request start count)
	   (xlib:mapping-notify display request start count)
	   (when (eql request :modifier)
	     (fill-keycode->modifier-state display)
	     (fill-clim-modifier-key-index->x-state display))
	   t)
	  ((:reparent-notify) () 
	   t)
	  ((:configure-notify) (event-window)
	   (let ((sheet (mirror->sheet port event-window)))
	     (when sheet
	       (dispatch-event
		 sheet
		 (let ((region (mirror-region port sheet)))
		   (allocate-event 'window-configuration-event
		     :native-region region
		     :region (untransform-region
			       (sheet-native-transformation sheet) region)
		     :sheet sheet))))
	     t))
	  ((:destroy-notify) ()
	   t)
	  ((:no-exposure) ()
	   t)
	  ((:client-message) (type data event-window)
	   (case type
	     (:wm_protocols
	       (let ((atom (xlib:atom-name display (aref data 0))))
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
