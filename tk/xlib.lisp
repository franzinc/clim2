(in-package :tk)

;;; Pathetic clos interface to Xlib

(defclass screen (handle-class display-object) ())

(defun screen-gcontext (screen)
  (let ((gc (x-screen-gc (object-handle screen))))
    (or (find-object-from-address gc nil)
	(register-address
	 (make-instance 'gcontext
			:display (object-display screen)
			:handle gc)))))




(defclass window (display-object handle-class)
	  ())



(defun-c-callable x-error-handler (display (x :unsigned-long))
  (error "x-error:~S" 
	 (get-error-text (x11:xerrorevent-error-code x))))

(defun-c-callable x-io-error-handler (x)
  (error "x-io-error:~S" x))



(defun get-error-text (code)
  (let ((s (string-to-char* (make-string 1000))))
    (x11::xgeterrortext (display-handle display) code s 1000)
    (char*-to-string s)))
      
(x11:xseterrorhandler (register-function 'x-error-handler))
(x11:xsetioerrorhandler (register-function 'x-io-error-handler))


(defmethod display-root-window (display)
  (intern-object-xid
   (x11:xdefaultrootwindow (display-handle display))
   'window
   :display display))



(defclass colormap (handle-class display-object) ())

(defun default-colormap (display &optional (screen 0))
  (intern-object-xid
   (x11:xdefaultcolormap (display-handle display)
			 screen)
   'colormap
   :display 
   display))

(defclass color (handle-class) ())

(defmethod initialize-instance :after ((x color) &key handle red green
				       blue)
  (unless handle
    (setq handle (x11::make-xcolor))
    (setf (x11::xcolor-red handle) red
	  (x11:xcolor-green handle) green
	  (x11:xcolor-blue handle) blue
	  (x11:xcolor-flags handle) 255)
    (setf (slot-value x 'handle) handle)))

(defmethod print-object ((o color) s)
  (print-unreadable-object 
   (o s :type t :identity t)
   (let ((cm (object-handle o)))
     (format s "~D,~D,~D"
	     (x11:xcolor-red cm)
	     (x11:xcolor-green  cm)
	     (x11:xcolor-blue cm)))))

(defun lookup-color (colormap color-name)
  (let ((exact (x11::make-xcolor))
	(closest (x11::make-xcolor)))
    (if (zerop (x11:xlookupcolor
		(display-handle (object-display colormap))
		(object-handle colormap)
		(string-to-char* color-name)
		exact
		closest))
	(error "Could not find ~S in colormap ~S" color-name colormap)
      (values (make-instance 'color :handle exact)
	      (make-instance 'color :handle closest)))))

(defun allocate-color (colormap x)
  (let ((y (x11::make-xcolor))
	(x (object-handle x)))
    (setf (x11:xcolor-red y) (x11:xcolor-red x)
	  (x11:xcolor-green y) (x11:xcolor-green x)
	  (x11:xcolor-blue y) (x11:xcolor-blue x))
    (let ((z (x11:xalloccolor
	      (display-handle (object-display colormap))
	      (object-handle colormap)
	      y)))
      (when (zerop z)
	(error "Could not allocate color: ~S" x))
      (values (x11:xcolor-pixel y)
	      (make-instance 'color :handle y)))))



(defun default-screen (display)
  (intern-object-address
   (x11:xdefaultscreenofdisplay
    (display-handle display))
   'screen
   :display display))

(defun query-pointer (window)
  (with-ref-par 
   ((root 0)
    (child 0)
    (root-x 0)
    (root-y 0)
    (x 0)
    (y 0)
    (mask 0))
   (let ((display (object-display window)))
     (if (x11:xquerypointer
	  (display-handle display)
	  (object-handle window)
	  root
	  child
	  root-x
	  root-y
	  x
	  y
	  mask)
	 (values
	  t
	  (intern-object-xid
   (aref root 0) 
	   'window
	   :display display)
	  (intern-object-xid
	   (aref child 0) 
	   'window
	   :display display)
	  (aref root-x 0)
	  (aref root-y 0)
	  (aref x 0)
	  (aref y 0)
	  (aref mask 0))))))
	


(defconstant *event-masks* 
    '(:key-press
      :key-release
      :button-press
      :button-release
      :enter-window
      :leave-window
      :pointer-motion
      :pointer-motion-hint
      :button1-motion
      :button2-motion
      :button3-motion
      :button4-motion
      :button5-motion
      :button-motion
      :keymap-state
      :exposure
      :visibility-change
      :structure-notify
      :resize-redirect
      :substructure-notify
      :substructure-redirect
      :focus-change
      :property-change
      :colormap-change
      :owner-grab-button
      ))

(defconstant *event-types*
  '(
    nil
    nil
    :key-press
    :key-release
    :button-press
    :button-release
    :motion-notify
    :enter-notify
    :leave-notify
    :focus-in
    :focus-out
    :keymap-notify
    :expose
    :graphics-expose
    :no-expose
    :visibility-notify
    :create-notify
    :destroy-notify
    :unmap-notify
    :map-notify
    :map-request
    :reparent-notify
    :configure-notify
    :configure-request
    :gravity-notify
    :resize-request
    :circulate-notify
    :circulate-request
    :property-notify
    :selection-clear
    :selection-request
    :selection-notify
    :colormap-notify
    :client-message
    :mapping-notify
    ))



(defun event-type (event)
  (elt *event-types* (x11::xevent-type event)))

(defun encode-event-mask (mask)
  (cond ((integerp mask)
	 mask)
	((listp mask)
	 (let ((r 0))
	   (dolist (x mask)
	     (setq r (logior (encode-event-mask x) r)))
	   r))
	(t
	 (let ((x (position mask *event-masks*)))
	   (if x
	       (ash 1 x)
	     (error "cannot encode event-mask ~S" mask))))))


(defun lookup-string (event)
  (let ((buffer (string-to-char* (make-string 20 :initial-element #\null))))
    (with-ref-par 
	((keysym 0) (compose-status 0))
      (values
       (x11:xlookupstring
	event
	buffer
	20
	keysym
	compose-status)
       (char*-to-string buffer)
       (aref keysym 0)))))
