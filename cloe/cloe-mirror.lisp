;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: cloe-mirror.lisp,v 1.1 92/10/01 10:03:54 cer Exp $

(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;--- This isn't really right, figure out what to do
(defmethod sheet-shell (sheet) sheet)

(defmethod realize-graft ((port cloe-port) graft)
  (with-slots (silica::pixels-per-point silica::pixel-width silica::pixel-height
	       silica::mm-width silica::mm-height silica::units) graft
    (multiple-value-bind (screen-width screen-height)
	(win::get-screen-size)
      (let ((logpixelsx (win::get-device-caps *dc* 90))
	    (logpixelsy (win::get-device-caps *dc* 90)))
	(setf silica::pixel-width  screen-width
	      silica::pixel-height screen-height
	      silica::mm-width     (round (* screen-width 25.4s0) logpixelsx)
	      silica::mm-height	   (round (* screen-height 25.4s0) logpixelsy))
	(let* ((ppp (/ logpixelsy 72s0))
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
	(setf (sheet-direct-mirror graft) *dc*)
	(update-mirror-transformation port graft)))))

(defmethod realize-mirror ((port cloe-port) sheet)
  (multiple-value-bind (left top right bottom)
      (sheet-native-region* sheet)
    (fix-coordinates left top right bottom)
    (let ((window
	    (let* ((parent (sheet-mirror sheet))
		   (frame (pane-frame sheet))
		   (save-under (and frame (getf (frame-properties frame) :save-under))))
	      (win::create-window
		"Vanilla" "CLIM"
		(logior win::ws_clipchildren	;new, helps refresh
			(cond ((not (eq parent *dc*))
			       (logior win::ws_child
				       win::ws_clipsiblings))
			      (save-under
			       (logior win::ws_popup
				       win::ws_border))
			      (t
			       (logior win::ws_overlapped
				       win::ws_caption
				       win::ws_thickframe
				       win::ws_sysmenu
				       win::ws_minimizebox
				       win::ws_maximizebox)))
			)
		left top (- right left) (- bottom top)
		(if (eql parent *dc*) 0 parent) 0 0 "arg"))))
      (setf (sheet-native-transformation sheet) +identity-transformation+)
      window)))

(defmethod destroy-mirror ((port cloe-port) sheet)
  (win::destroy-window (sheet-mirror sheet)))

(defmethod enable-mirror ((port cloe-port) sheet)
  (win::show-window (sheet-mirror sheet) :type :show))

(defmethod disable-mirror ((port cloe-port) sheet)
  (win::show-window (sheet-mirror sheet) :type :hide))

(defmethod raise-mirror ((port cloe-port) (sheet mirrored-sheet-mixin))
  (win::set-window-position (sheet-mirror sheet) 0 0 0 0 0
			    (logior win::swp_noactivate
				    win::swp_nomove
				    win::swp_nosize)))

(defmethod bury-mirror ((port cloe-port) (sheet mirrored-sheet-mixin))
  (win::set-window-position (sheet-mirror sheet) 1 0 0 0 0
			    (logior win::swp_noactivate
				    win::swp_nomove
				    win::swp_nosize)))

(defmethod mirror-visible-p ((port cloe-port) sheet)
  ;;(win::is-window-visible (sheet-mirror sheet))
  t)

(defmethod cloe-mirror-native-edges* ((port cloe-port) mirror)
  (multiple-value-bind (wleft wtop wright wbottom)
      (win::get-window-rectangle mirror)
    (values (coordinate wleft) (coordinate wtop)
	    (coordinate wright) (coordinate wbottom))))

;; Returns left,top,right,bottom
(defmethod mirror-region* ((port cloe-port) sheet)
  (cloe-mirror-native-edges* port (sheet-mirror sheet)))

;; Returns x,y,width,height
(defmethod mirror-inside-region* ((port cloe-port) sheet)
  (multiple-value-bind (cleft ctop cright cbottom)
      (win::get-client-rectangle (sheet-mirror window))
    (values (coordinate cleft) (coordinate ctop)
	    (coordinate cright) (coordinate cbottom))))

;;--- Shouldn't this be the same as MIRROR-REGION*?
(defmethod mirror-native-edges* ((port cloe-port) sheet)
  (cloe-mirror-native-edges* port (sheet-direct-mirror sheet)))

(defmethod mirror-inside-edges* ((port cloe-port) sheet)
  (multiple-value-bind (cleft ctop cright cbottom)
      (win::get-client-rectangle (sheet-mirror sheet))
    (values (coordinate cleft) (coordinate ctop)
	    (coordinate cright) (coordinate cbottom))))
 
(defmethod set-sheet-mirror-edges* ((port cloe-port) sheet left top right bottom)
  (fix-coordinates left top right bottom)
  (win::set-window-position (sheet-mirror sheet) 0
			    left top (- right left) (- bottom top)
			    (logior win::swp_noactivate win::swp_nozorder)))

;;;

(defun-inline sign-extend-16 (n)
  (dpb n (byte 15 0) (- (ldb (byte 1 15) n))))

(defmethod note-pointer-motion ((port cloe-port) sheet x y)
  (with-slots (pointer-sheet pointer-x pointer-y) port
    (setf pointer-sheet sheet)
    (setf pointer-x x)
    (setf pointer-y y)))

(defmethod flush-pointer-motion ((port cloe-port))
  (with-slots (event-queue pointer-sheet pointer-x pointer-y) port
    (when pointer-sheet
      (let ((pointer (port-pointer port)))
	(when pointer
	  (queue-put event-queue
		     (allocate-event 'pointer-motion-event
		       :native-x pointer-x
		       :native-y pointer-y
		       :modifier-state (port-modifier-state port)
		       :pointer pointer
		       :sheet pointer-sheet))))
      (setf pointer-sheet nil))))

;;; Convert a MS Windows shift mask into a CLIM modifier-state
(defun windows-mask->modifier-state (mask)
  (if (logtest win::mk_shift mask)
      (if (logtest win::mk_control mask)
	  (make-modifier-state :shift :control)
	  (make-modifier-state :shift))
      (if (logtest win::mk_control mask)
	  (make-modifier-state :control)
	  (make-modifier-state))))

(defmethod event-handler ((port cloe-port) args)
  (with-slots (event-queue pointer-sheet pointer-x pointer-y) port
    (let ((sheet (mirror->sheet port (win::get-16bit args 0)))
	  (message (win::get-16bit args 2)))
      (declare (fixnum message))
      (when sheet
	(cond ((eql message win::wm_mousemove)
	       (note-pointer-motion port sheet (win::get-16bit args 6) (win::get-16bit args 8)))

	      ((eql message win::wm_paint)
	       (let ((ileft (win::get-16bit args 4))
		     (itop (win::get-16bit args 6))
		     (iright (win::get-16bit args 8))
		     (ibottom (win::get-16bit args 10)))
		 (unless (or (= ileft iright) (= itop ibottom))	;seems to happen alot
		   (queue-put event-queue
			      (allocate-event 'window-repaint-event
				;;--- Should this be (MIRROR-REGION PORT SHEET), as
				;;--- it is in the :CONFIGURE-NOTIFY case?
				:native-region (sheet-native-region sheet)
				:region (make-bounding-rectangle ileft itop iright ibottom)
				:sheet sheet)))))

	      #||
	      ;; scrolling
	      ((or (eql message win::wm_hscroll) (eql message win::wm_vscroll))
	       (let ((type (win::get-16bit args 4))
		     (position (win::get-16bit args 6))
		     (message (cond ((eql message win::wm_hscroll) :x)
				    ((eql message win::wm_vscroll) :y))))
		 (declare (fixnum type position))
		 (multiple-value-bind (type position)
		     (cond ((eql type win::sb_lineup)
			    (values :relative-jump -1))
			   ((eql type win::sb_linedown)
			    (values :relative-jump +1))
			   ((eql type win::sb_pageup)
			    (values :screenful -1))
			   ((eql type win::sb_pagedown)
			    (values :screenful +1))
			   ((eql type win::sb_thumbposition)
			    (values :percentage position))
			   ((eql type win::sb_top)
			    (values :percentage 0))
			   ((eql type win::sb_bottom)
			    (values :percentage 100)))
		   (when type
		     (queue-put pending-scrolls (list message type position))
		     (pushnew stream *cloe-windows-with-deferred-events*)))))
	      ||#

	      ;; resizing
	      ((or (eql message win::wm_move)
		   (eql message win::wm_size))
	       (queue-put event-queue
			  (allocate-event 'window-configuration-event
			    :sheet sheet)))

	      ;; character typed
	      ((eql message win::wm_char)
	       (let ((char (aref args 4)))
		 (flush-pointer-motion port)
		 (queue-put event-queue
			    (allocate-event 'key-press-event
			      :key-name nil
			      :character char
			      :modifier-state (port-modifier-state port)
			      :sheet sheet))))

	      #||
	      ((or (eql message win::wm_keydown)
		   (eql message win::wm_keyup)
		   (eql message win::wm_syskeyup)
		   (eql message win::wm_syskeydown))
	       (let ((vk (char-code (aref args 4))))
		 (flush-pointer-motion port)
		 (queue-put event-queue
			    (allocate-event
			      (cond ((or (eql message win::wm_keydown)
					 (eql message win::wm_syskeydown))
				     'key-press-event)
				    ((or (eql message win::wm_keyup)
					 (eql message win::wm_syskeyup))
				     'key-release-event))
			      :key-name (vk->keysym vk)
			      :character nil
			      :modifier-state (port-modifier-state port)
			      :sheet sheet))))
	      ||#

	      ;; button press or release
	      ((or (eql message win::wm_lbuttondown)
		   (eql message win::wm_rbuttondown)
		   (eql message win::wm_mbuttondown)
		   (eql message win::wm_lbuttonup)
		   (eql message win::wm_rbuttonup)
		   (eql message win::wm_mbuttonup))
	       (let ((modifier-state (windows-mask->modifier-state (win::get-16bit args 4)))
		     (pointer (port-pointer port)))
		 (when pointer
		   (flush-pointer-motion port)
		   (setf (port-modifier-state port) modifier-state)
		   (multiple-value-bind (key button)
		       (cond ((eql message win::wm_lbuttondown)
			      (values 'pointer-button-press-event +pointer-left-button+))
			     ((eql message win::wm_mbuttondown)
			      (values 'pointer-button-press-event +pointer-middle-button+))
			     ((eql message win::wm_rbuttondown)
			      (values 'pointer-button-press-event +pointer-right-button+))
			     ((eql message win::wm_lbuttonup)
			      (values 'pointer-button-release-event +pointer-left-button+))
			     ((eql message win::wm_mbuttonup)
			      (values 'pointer-button-release-event +pointer-middle-button+))
			     ((eql message win::wm_rbuttonup)
			      (values 'pointer-button-release-event +pointer-right-button+)))
		     (queue-put event-queue
				(allocate-event key
				  :native-x (win::get-16bit args 6)
				  :native-y (win::get-16bit args 8)
				  :button button
				  :modifier-state modifier-state
				  :pointer pointer
				  :sheet sheet))))))
	      )))))

(defun win::vanilla-event (code length args)
  (declare (ignore code length))		;because I don't know what they are!
  (event-handler *cloe-port* args))

;;;

(defmethod process-next-event ((port cloe-port)
			       &key (timeout nil) (wait-function nil)
			       (state "Windows Event"))
  (with-slots (event-queue) port
    (let ((end-time (and timeout (+ (get-internal-real-time)
				    (* internal-time-units-per-second timeout)))))
      (win::await-response -1 nil nil)
      (loop
	(flush-pointer-motion port)
	(let ((event (queue-get event-queue)))
	  (when event
	    (distribute-event port event)
	    (return t)))
	(when (and end-time (> (get-internal-real-time) end-time))
	  (return nil))
	(win::await-response -1 t t)))))

(defmethod distribute-event :around ((port cloe-port) (event window-configuration-event))
  (let* ((sheet (event-sheet event))
	 (mirror (sheet-mirror sheet)))
    (unless (win::is-iconic mirror)
      (let ((native-region (mirror-region port sheet)))
	(setf (slot-value event 'pyrex::native-region) native-region)
	(setf (slot-value event 'pyrex::region)
	      (untransform-region (sheet-native-transformation sheet) 
				  native-region)))
      (call-next-method))))
