;; -*- mode: common-lisp; package: tk -*-
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
;; $fiHeader: xlib.lisp,v 1.11 92/03/09 17:41:03 cer Exp Locker: cer $

(in-package :tk)

;;; Pathetic clos interface to Xlib

(defclass screen (display-object) ()
	  (:metaclass standard-class-wrapping-foreign-address))

#+obsolete
(defun screen-gcontext (screen)
  (let ((gc (x-screen-gc screen)))
    (or (find-object-from-address gc nil)
	(register-address
	 (make-instance 'gcontext
			:display (object-display screen)
			:foreign-address gc)))))




(defclass drawable (display-object)
  ()
  (:metaclass standard-class-wrapping-foreign-address))
	  
(defclass window (drawable)
  ()
  (:metaclass standard-class-wrapping-foreign-address))

;; Ugh.  This should be part of a general purpose mechanism
(defmethod drawable-depth ((window window))
  (let ((attrs (x11:make-xwindowattributes)))
    (x11:xgetwindowattributes (object-display window)
			      window
			      attrs)
    (x11:xwindowattributes-depth attrs)))
    
(defmethod (setf drawable-save-under) (nv (window window))
  (let ((attrs (x11:make-xsetwindowattributes)))
    (setf (x11:xsetwindowattributes-save-under attrs) (if nv 1 0))
    (x11:xchangewindowattributes (object-display window)
				 window
				 x11::cwsaveunder ;; 
				 attrs)
    nv))

(defmethod drawable-save-under ((window window))
  (let ((attrs (x11:make-xwindowattributes)))
    (x11:xgetwindowattributes (object-display window)
			      window
			      attrs)
    (not (zerop (x11:xwindowattributes-save-under attrs)))))


(defmethod (setf drawable-backing-store) (nv (window window))
  (let ((attrs (x11:make-xsetwindowattributes)))
    (setf (x11:xsetwindowattributes-backing-store attrs) (if nv 1 0))
    (x11:xchangewindowattributes (object-display window)
				 window
				 x11::cwbackingstore ;; 
				 attrs)
    nv))

(defmethod drawable-backing-store ((window window))
  (let ((attrs (x11:make-xwindowattributes)))
    (x11:xgetwindowattributes (display-handle (object-display window))
			      (object-handle window)
			      attrs)
    (not (zerop (x11:xwindowattributes-backing-store attrs)))))

  
(defclass pixmap (drawable)
  ((width :initarg :width :reader pixmap-width)
   (height :initarg :height :reader pixmap-height)
   (depth :initarg :depth :reader drawable-depth))
  (:metaclass standard-class-wrapping-foreign-address))

(defmethod initialize-instance :after
	   ((p pixmap) &key foreign-address width height depth drawable)
  (with-slots (display) p
    (setf display (object-display drawable))
    (unless foreign-address
      (setf (foreign-pointer-address p)
	(x11:xcreatepixmap
	 display
	 drawable
	 width
	 height
	 depth))
      (register-xid p))))

(defun-c-callable x-error-handler ((display :unsigned-long) (x :unsigned-long))
  (error "x-error:~S" 
	 (get-error-text (x11:xerrorevent-error-code x) display)))

(defun-c-callable x-io-error-handler (x)
  (error "x-io-error:~S" x))



(defun get-error-text (code display-handle)
  (let ((s (string-to-char* (make-string 1000))))
    (x11::xgeterrortext display-handle code s 1000)
    (char*-to-string s)))

(defun setup-error-handlers ()
  (x11:xseterrorhandler (register-function 'x-error-handler))
  (x11:xsetioerrorhandler (register-function 'x-io-error-handler)))

(eval-when (load)
  (setup-error-handlers))

(defmethod display-root-window (display)
  (intern-object-xid
   (x11:xdefaultrootwindow display)
   'window
   :display display))

(defmethod display-screen-number (display)
  (x11:xdefaultscreen display))


(defclass colormap (display-object) ()
  (:metaclass standard-class-wrapping-foreign-address))

(defun default-colormap (display &optional (screen 0))
  (intern-object-xid
   (x11:xdefaultcolormap display
			 screen)
   'colormap
   :display 
   display))

(defclass color () ()
  (:metaclass standard-class-wrapping-foreign-address))

(defmethod initialize-instance :after ((x color) &key foreign-address red green
				       blue)
  (unless foreign-address
    (setq foreign-address (x11::make-xcolor))
    (setf (x11::xcolor-red foreign-address) red
	  (x11:xcolor-green foreign-address) green
	  (x11:xcolor-blue foreign-address) blue
	  (x11:xcolor-flags foreign-address) 255)
    (setf (foreign-pointer-address x) foreign-address)))

(defmethod print-object ((o color) s)
  (print-unreadable-object 
   (o s :type t :identity t)
   (let ((cm o))
     (format s "~D,~D,~D"
	     (x11:xcolor-red cm)
	     (x11:xcolor-green  cm)
	     (x11:xcolor-blue cm)))))

(defun lookup-color (colormap color-name)
  (let ((exact (x11::make-xcolor))
	(closest (x11::make-xcolor)))
    (if (zerop (x11:xlookupcolor
		(object-display colormap)
		colormap
		(string-to-char* color-name)
		exact
		closest))
	(error "Could not find ~S in colormap ~S" color-name colormap)
      (values (make-instance 'color :foreign-address exact)
	      (make-instance 'color :foreign-address closest)))))

(defun allocate-color (colormap x)
  (let ((y (x11::make-xcolor))
	(x x))
    (setf (x11:xcolor-red y) (x11:xcolor-red x)
	  (x11:xcolor-green y) (x11:xcolor-green x)
	  (x11:xcolor-blue y) (x11:xcolor-blue x))
    (let ((z (x11:xalloccolor
	      (object-display colormap)
	      colormap
	      y)))
      (when (zerop z)
	(error "Could not allocate color: ~S" x))
      (values (x11:xcolor-pixel y)
	      (make-instance 'color :foreign-address y)))))



(defun default-screen (display)
  (intern-object-address
   (x11:xdefaultscreenofdisplay
    display)
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
	   display
	   window
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
	    (sys:memref-int (foreign-pointer-address root) 0 0 :signed-long)
	    'window
	    :display display)
	   (intern-object-xid
	    (sys:memref-int (foreign-pointer-address child) 0 0 :signed-long)
	    'window
	    :display display)
	   (sys:memref-int (foreign-pointer-address root-x) 0 0 :signed-long)
	   (sys:memref-int (foreign-pointer-address root-y) 0 0 :signed-long)
	   (sys:memref-int (foreign-pointer-address x) 0 0 :signed-long)
	   (sys:memref-int (foreign-pointer-address y) 0 0 :signed-long)
	   (sys:memref-int (foreign-pointer-address mask) 0 0 :signed-long))))))
	


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
  (with-ref-par 
      ((keysym 0)
       (buffer 0))
    (values
     (x11:xlookupstring
      event
      buffer
      2 
      keysym
      0)
     (char*-to-string (foreign-pointer-address buffer))
     (sys:memref-int (foreign-pointer-address keysym) 0 0 :signed-long))))

(defclass image ()
  ((width :reader image-width :initarg :width)
   (height :reader image-height :initarg :height)
   (data :reader image-data :initarg :data)
   (depth :reader image-depth :initarg :depth)
   (realized-displays :initform nil :accessor realized-displays))
  (:metaclass standard-class-wrapping-foreign-address))
  

(defmethod realize-image (image display)
  (when (not (member display (realized-displays image)))
    (let ((width (image-width image))
	  (height (image-height image))
	  (data (image-data image))
	  (depth (image-depth image))
	  v
	  format
	  (bytes-per-line 0)
	  (bitmap-pad 8))		; Why is this 8 - if 0 get signal 8!
					;  -- RTFM, dude.  (jdi)
      (ecase depth
	(8 
	 (setq format x11:zpixmap)
	 (setq v (excl::malloc (* width height)))
	 (dotimes (h height)
	   (dotimes (w width)
	     (setf (sys::memref-int v (+ (* h width) w) 0 :unsigned-byte)
	       (aref data h w)))))
	(1
	 (setq format x11:xybitmap)
	 (setq bytes-per-line (ceiling (/ width bitmap-pad)))
	 (setq v (excl::malloc (* bytes-per-line height)))))
      (let* ((visual (x11:screen-root-visual
		      (x11:xdefaultscreenofdisplay display)))
	     (offset 0)
	     (x (x11:xcreateimage
		 display
		 visual
		 depth
		 format
		 offset
		 v
		 width
		 height
		 bitmap-pad
		 bytes-per-line)))
	(case depth
	  (1
	   (dotimes (h height)
	     (dotimes (w width)
	       (x11:xputpixel x w h (aref data h w))))))
	(setf (foreign-pointer-address image) x)))
    (pushnew display (realized-displays image))))

(defmethod put-image (pixmap gc image
		      &key (src-x 0) (src-y 0) (dest-x 0) (dest-y 0))
  (let ((display (object-display pixmap)))
    (realize-image image display)
    (x11:xputimage
     display
     pixmap
     gc
     image
     src-x
     src-y
     dest-x
     dest-y
     (image-width image)
     (image-height image))))
