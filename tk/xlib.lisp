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
;; $fiHeader: xlib.lisp,v 1.22 92/07/01 15:44:38 cer Exp Locker: cer $

(in-package :tk)

(deftype card32 () '(unsigned-byte 32))

(deftype card29 () '(unsigned-byte 29))

(deftype card24 () '(unsigned-byte 24))

(deftype card8 () '(unsigned-byte 8))

(deftype int32 () '(signed-byte 32))

(deftype int16 () '(signed-byte 16))

(deftype int8 () '(signed-byte 8))

;;; Pathetic clos interface to Xlib

(defclass display-object (ff:foreign-pointer)
  ((display :initarg :display
	    :fixed-index 0)))

(defclass screen (display-object) ())

(defclass drawable (display-object) ())
	  
(defclass window (drawable) ())

(eval-when (compile load eval)
  (defconstant *window-set-attributes-bit-mask*
      '(background-pixmap background-pixel border-pixmap border-pixel
	bit-gravity win-gravity backing-store backing-planes backing-pixel
	override-redirect save-under event-mask do-not-propogate-mask
	colormap cursor))
  ;; Not really supported yet:
  (defconstant *window-configure-bit-mask*
      '(x y width height border-width sibling stacking-mode)))

(eval-when (compile eval)
  (defmacro define-window-reader (name &optional decoder &rest args)
    `(defmethod ,(intern (format nil "~A-~A" 'window name)) ((window window))
       ,(let ((body
	       `(let ((attrs (x11:make-xwindowattributes)))
		  (x11:xgetwindowattributes (object-display window)
					    window
					    attrs)
		  (,(intern (format nil "~a-~a" 'xwindowattributes name) :x11)
		   attrs))))
	  (if decoder
	      `(,decoder ,body ,@args)
	    body))))
  (defmacro define-window-writer (name &optional encoder &rest args)
    `(progn
       (defmethod (setf ,(intern (format nil "~A-~A" 'window name)))
	   (nv (window window))
	 (let ((attrs (x11::make-xsetwindowattributes)))
	   (setf (,(intern (format nil "~a-~a" 'xsetwindowattributes name) :x11)
		  attrs)
	     ,(if encoder
		  `(,encoder nv ,@args)
		`nv))
	   (x11:xchangewindowattributes
	    (object-display window)
	    window
	    ,(ash 1 (or (position name *window-set-attributes-bit-mask*)
			(error "Cannot find ~S in window attributes" name)))
	    attrs)
	   nv))))
  (defmacro define-window-accessor (name (&optional encoder decoder) &rest args)
    `(progn
       (define-window-reader ,name ,decoder ,@args)
       (define-window-writer ,name ,encoder ,@args)
       ',name)))

(define-window-accessor backing-store (encode-backing-store decode-backing-store))

(defun encode-backing-store (x)
  (declare (optimize (speed 3) (safety 0)))
  (ecase x
    ((t :always) 2)
    (:when-mapped 1)
    ((nil :not-useful) 0)))
    
(defun decode-backing-store (x)
  (declare (optimize (speed 3) (safety 0)))
  (ecase x
    (2 :always)
    (1 :when-mapped)
    (0 :not-useful)))
    
(defun encode-save-under (x)
  (declare (optimize (speed 3) (safety 0)))
  (ecase x
    ((t :on) 1)
    ((nil :off) 0)))
    
(defun decode-save-under (x)
  (declare (optimize (speed 3) (safety 0)))
  (ecase x
    (1 t)
    (0 nil)))

(define-window-accessor save-under (encode-save-under decode-save-under))

(define-window-reader width)
(define-window-reader height)
(define-window-reader depth)
(define-window-reader map-state decode-window-map-state)

(defun decode-window-map-state (x)
  (ecase x
    (0 :unmapped)
    (1 :unviewable)
    (2 :viewable)))

(defmethod drawable-width ((window window))
  (window-width window))
(defmethod drawable-height ((window window))
  (window-height window))
(defmethod drawable-depth ((window window))
  (window-depth window))

  
(defclass pixmap (drawable)
  ((width :initarg :width :reader pixmap-width)
   (height :initarg :height :reader pixmap-height)
   (depth :initarg :depth :reader pixmap-depth)))

(defmethod drawable-width ((pixmap pixmap))
  (pixmap-width pixmap))
(defmethod drawable-height ((pixmap pixmap))
  (pixmap-height pixmap))
(defmethod drawable-depth ((pixmap pixmap))
  (pixmap-depth pixmap))

(defun destroy-pixmap (pixmap)
  (let ((display (object-display pixmap)))
    (unregister-xid pixmap display)
    (x11:xfreepixmap display pixmap)))
  
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
      (register-xid p display))))

(defun-c-callable x-error-handler ((display :unsigned-long) (x :unsigned-long))
  (error "x-error:~S" 
	 (get-error-text (x11:xerrorevent-error-code x) display)))

(defun-c-callable x-io-error-handler (x)
  (error "x-io-error:~S" x))



(defun get-error-text (code display-handle)
  (let ((s (make-string 1000)))
    (x11::xgeterrortext display-handle code s 1000)
    (let ((n (position (cltl1::int-char 0) s)))
      (when n (setq s (subseq s 0 n))))
    s))
      
(defun setup-error-handlers ()
  (x11:xseterrorhandler (register-function 'x-error-handler))
  (x11:xsetioerrorhandler (register-function 'x-io-error-handler)))

(eval-when (load)
  (setup-error-handlers))


(defmethod display-root-window (display)
  (intern-object-xid
   (x11:xdefaultrootwindow display)
   'window
   display
   :display display))

(defmethod display-screen-number (display)
  (x11:xdefaultscreen display))


(defclass colormap (display-object) ())

(defun default-colormap (display &optional (screen 0))
  (intern-object-xid
   (x11:xdefaultcolormap display
			 screen)
   'colormap
   display
   :display 
   display))

(defclass color (ff:foreign-pointer) ())

(defmethod initialize-instance :after ((x color) &key foreign-address red green blue)
  (unless foreign-address
    (setq foreign-address (x11::make-xcolor :in-foreign-space t))
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
	     (x11:xcolor-green cm)
	     (x11:xcolor-blue cm)))))

(defun lookup-color (colormap color-name)
  (let ((exact (x11::make-xcolor :in-foreign-space t))
	(closest (x11::make-xcolor :in-foreign-space t)))
    (if (zerop (x11:xlookupcolor
		(object-display colormap)
		colormap
		color-name
		exact
		closest))
	(error "Could not find ~S in colormap ~S" color-name colormap)
      (values (make-instance 'color :foreign-address exact)
	      (make-instance 'color :foreign-address closest)))))

(defun allocate-color (colormap x)
  (let ((y (x11::make-xcolor :in-foreign-space t))
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


(defun query-color (colormap x)
  ;;--- Resource time
  (let ((y (x11::make-xcolor)))
    (setf (x11:xcolor-pixel y) x)
    (x11:xquerycolor
     (object-display colormap)
     colormap
     y)
    (values 
     (x11:xcolor-red y)
     (x11:xcolor-green y)
     (x11:xcolor-blue y))))

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
	    (aref root 0) 
	    'window
	    display
	    :display display)
	   (intern-object-xid
	    (aref child 0) 
	    'window
	    display
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


(defvar *lookup-string-buffers* nil)

(defun lookup-string (event)
  (declare (optimize (speed 3) (safety 0)))
  (let ((buffer (or (pop *lookup-string-buffers*)
		    (excl::malloc 256))))
    (declare (type (unsigned-byte 32) buffer))
    (with-ref-par ((keysym 0))
      (let* ((nchars (x11:xlookupstring event buffer 256 keysym 0))
	     (result (make-string nchars)))
	(declare (fixnum nchars)
		 (simple-string result))
	(dotimes (i nchars)
	  (declare (fixnum i))
	  (setf (schar result i)
	    (code-char (sys:memref-int buffer 0 i :unsigned-byte))))
	(push buffer *lookup-string-buffers*)
	(values result
		(aref keysym 0))))))

(defclass image (ff:foreign-pointer)
  ((width :reader image-width :initarg :width)
   (height :reader image-height :initarg :height)
   (data :reader image-data :initarg :data)
   (depth :reader image-depth :initarg :depth)
   (realized-displays :initform nil :accessor realized-displays)))
  

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

(defun image-from-pixmap (pixmap)
  (x11:xgetimage
   (object-display pixmap)
   pixmap
   0 
   0 
   (pixmap-width pixmap)
   (pixmap-height pixmap)
   #xff					; plane-mask
   x11:xypixmap))

(defun destroy-image (image)
  (x11:xdestroyimage image))

