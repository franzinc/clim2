;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;--- This isn't really right, figure out what to do
(defmethod sheet-shell (sheet) sheet)

(defmethod realize-graft ((port cloe-port) graft)
  (with-slots (silica::pixels-per-point silica::pixel-width silica::pixel-height
	       silica::mm-width silica::mm-height silica::units) graft
    (multiple-value-bind (screen-width screen-height)
	(win::get-screen-size)
      (let ((logpixelsx (win::get-device-caps *dc* win::logpixelsx))
	    (logpixelsy (win::get-device-caps *dc* win::logpixelsy)))
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
	(setf (sheet-direct-mirror graft) 0)
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
			(cond ((not (eql parent 0))
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
		parent 0 0 "arg"))))
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
  (win::is-window-visible (sheet-mirror sheet)))

;; Returns left,top,right,bottom
(defmethod mirror-region* ((port cloe-port) sheet)
  (multiple-value-bind (cleft ctop cright cbottom)
      (win::get-client-rectangle (sheet-mirror window))
    (values (coordinate cleft) (coordinate ctop)
	    (coordinate cright) (coordinate cbottom))))

;; Returns x,y,width,height
(defmethod mirror-inside-region* ((port cloe-port) sheet)
  (multiple-value-bind (cleft ctop cright cbottom)
      (win::get-client-rectangle (sheet-mirror window))
    (values (coordinate cleft) (coordinate ctop)
	    (coordinate cright) (coordinate cbottom))))

;;--- Shouldn't this be the same as MIRROR-REGION*?
(defmethod mirror-native-edges* ((port cloe-port) sheet)
  (multiple-value-bind (cleft ctop cright cbottom)
      (win::get-client-rectangle (sheet-direct-mirror sheet))
    (values (coordinate cleft) (coordinate ctop)
	    (coordinate cright) (coordinate cbottom))))

(defmethod mirror-inside-edges* ((port cloe-port) sheet)
  (multiple-value-bind (cleft ctop cright cbottom)
      (win::get-client-rectangle (sheet-mirror sheet))
    (values (coordinate cleft) (coordinate ctop)
	    (coordinate cright) (coordinate cbottom))))
 
(defmethod set-sheet-mirror-edges* ((port cloe-port) sheet left top right bottom)
  (fix-coordinates left top right bottom)
  (win::set-window-position (sheet-mirror sheet) 0
			    left top (- right left) (- bottom top)
			    (logior win::swp_nomove	;???
				    win::swp_noactivate
				    win::swp_nozorder)))
