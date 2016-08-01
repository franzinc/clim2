;; See the file LICENSE for the full license governing this code.
;;

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
	    excl::fixed-index 0)))

(defclass screen (display-object) ())

(defclass drawable (display-object) ())

(defclass window (drawable)
  ((plist :accessor window-property-list :initform nil)))

(eval-when (compile load eval)
  (defconstant *window-set-attributes-bit-mask*
      '(background-pixmap background-pixel border-pixmap border-pixel
	bit-gravity win-gravity backing-store backing-planes backing-pixel
	override-redirect save-under event-mask do-not-propogate-mask
	colormap cursor))
  ;; Not really supported yet:
  (defconstant *window-configure-bit-mask*
      '(x y width height border-width sibling stacking-mode)))

(defun make-xsetwindowattributes ()
  (clim-utils::allocate-cstruct 'x11::xsetwindowattributes :initialize t))

(defun make-xwindowattributes ()
  (clim-utils::allocate-cstruct 'x11::xwindowattributes :initialize t))

(eval-when (compile eval)
  (defmacro define-window-reader (name &optional decoder &rest args)
    `(defmethod ,(intern (format nil "~A-~A" 'window name)) ((window window))
       ,(let ((body
	       `(let ((attrs (make-xwindowattributes)))
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
	 (let ((attrs (make-xsetwindowattributes)))
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

#+obsolete
(define-window-accessor cursor nil)

(defun decode-window-map-state (x)
  (declare (optimize (speed 3) (safety 0)))
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
  (assert (and (plusp width) (plusp height))
      () "Width and height must be greater than zero")
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

(defclass resource-database (ff:foreign-pointer) ())

(defmethod initialize-instance :after
	   ((db resource-database) &key foreign-address)
  (unless foreign-address
    (setf (foreign-pointer-address db) (x11:xrmgetstringdatabase ""))))

(defun make-xrmvalue ()
  (clim-utils::allocate-cstruct 'x11::xrmvalue :initialize t))

(defun get-resource (db name class)
  (with-ref-par ((type 0 *))
    (let ((xrmvalue (make-xrmvalue)))
      (unless (zerop (x11:xrmgetresource db
					 (lisp-string-to-string8 name)
					 (lisp-string-to-string8 class)
					 &type xrmvalue))
	(let ((type (excl:native-to-string type)))
	  (values
	   (cond
	    ((equal type "String")
	     (excl:native-to-string (x11:xrmvalue-addr xrmvalue)))
	    ((equal type "Pixel")
	     (sys::memref-int (x11:xrmvalue-addr xrmvalue) 0 0
			      :unsigned-natural))
	    (t
	     (error "Unknown resource type: ~A" type)))
	   type))))))

(defun convert-string (widget string to-type)
  (let ((from (make-xrmvalue))
	(to-in-out (make-xrmvalue)))
    (setf (x11:xrmvalue-size from) (1+ (length string))
	  (x11:xrmvalue-addr from)
	  (note-malloced-object
	   (clim-utils:string-to-foreign string))
	  (x11:xrmvalue-addr to-in-out) 0)
    (unless (zerop (xt::xt_convert_and_store widget "String" from to-type
				      to-in-out))
      (values (x11:xrmvalue-size to-in-out)
	      (x11:xrmvalue-addr to-in-out)))))

;; --
;; Error handling

(define-condition x-error (error)
  ((display :reader x-error-display :initarg :display)
   (error-code :reader x-error-error-code :initarg :error-code)
   (request-code :reader x-error-request-code :initarg :request-code)
   (minor-code :reader x-error-minor-code :initarg :minor-code)
   (serial :reader x-error-serial :initarg :serial)
   (resourceid :reader x-error-resourceid :initarg :resourceid)
   (current-serial :reader x-error-current-serial :initarg :current-serial)
   (asynchronous :reader x-error-asynchronous :initarg :asynchronous))
  (:report report-x-error))

(defun report-x-error (condition stream)
  (let ((display (x-error-display condition))
	(error-code (x-error-error-code condition))
	(request-code (x-error-request-code condition))
	(minor-code (x-error-minor-code condition))
	(serial (x-error-serial condition))
	(resourceid (x-error-resourceid condition))
	(current-serial (x-error-current-serial condition)))
    (format stream "~@<Xlib: ~;~a ~:_in ~:[request ~d (last request was ~d)~;current request~2*~], ~:_Code: ~d.~d~@[ [~a]~], ~:_ResourceID: ~d~:>"
	    (get-error-text error-code display)
	    (= serial current-serial)
	    serial current-serial request-code minor-code
            nil
	    #+want-segfaults(when (< 1 request-code 128)
	      (let ((request-cstring (princ-to-string request-code))
		    (s (x11:system-malloc 1000))) ; bug16585
		(x11:xgeterrordatabasetext display "XRequest" request-cstring
					   0 s 1000)
		(prog1
		    (excl:native-to-string s)
		  (clim-utils::system-free s))))
	    resourceid)))

(defun-foreign-callable x-error-handler ((display :foreign-address)
					 (event :foreign-address))
  (error 'x-error
	 :display display
	 :error-code (x11:xerrorevent-error-code event)
	 :request-code (x11:xerrorevent-request-code event)
	 :minor-code (x11:xerrorevent-minor-code event)
	 :resourceid (x11:xerrorevent-resourceid event)
	 :serial (x11:xerrorevent-serial event)
	 :current-serial (x11:display-request display)))

(define-condition x-connection-lost (error)
  ((display :reader x-error-display :initarg :display))
  (:report report-x-connection-lost))

(defun report-x-connection-lost (condition stream)
    (let ((display (x-error-display condition)))
      (format stream "Xlib: Connection to X11 server '~a' lost"
	      (excl:native-to-string (x11:display-display-name display)))))

(defvar *x-io-error-hook* nil)

(defun-foreign-callable x-io-error-handler ((display :foreign-address))
  (when *x-io-error-hook*
    (funcall *x-io-error-hook* display))
  (error 'x-connection-lost :display display))

(defun get-error-text (code display-handle)
  (let ((s (x11:system-malloc 1000)))	; bug16585
    (x11::xgeterrortext display-handle code s 1000)
    (prog1
	(excl:native-to-string s)
      (clim-utils::system-free s))))

(defvar *x-error-handler-address* nil)
(defvar *x-io-error-handler-address* nil)

(defun setup-error-handlers ()
  (x11:xseterrorhandler (or *x-error-handler-address*
			    (setq *x-error-handler-address*
			      (register-foreign-callable 'x-error-handler))))
  (x11:xsetioerrorhandler (or *x-io-error-handler-address*
			      (setq *x-io-error-handler-address*
				(register-foreign-callable 'x-io-error-handler)))))

(eval-when (load)
  (setup-error-handlers))

(define-condition x-colormap-full (serious-condition) ())

;;--

(defmethod display-root-window (display)
  (intern-object-xid
   (x11:xdefaultrootwindow display)
   'window
   display
   :display display))

(defmethod display-screen-number (display)
  (x11:xdefaultscreen display))


(defclass colormap (display-object) ())


(define-window-accessor colormap ())

(defun create-colormap (display &key (screen (display-screen-number display)))
  (intern-object-xid
   (x11:xcreatecolormap  display
                         (x11:xrootwindow display screen)
                         (x11:xdefaultvisual display screen)
                         x11:allocnone)
   'colormap
   display
   :display display))


(defun default-colormap (display &optional (screen (display-screen-number display)))
  (intern-object-xid
   (x11:xdefaultcolormap display screen)
   'colormap
   display
   :display display))

(defclass color (ff:foreign-pointer) ())

(defun make-xcolor (&key in-foreign-space (number 1))
  (declare (ignore in-foreign-space))
  (clim-utils::allocate-cstruct 'x11::xcolor
				:number number :initialize t))

(defmethod initialize-instance :after
	   ((x color) &key foreign-address (in-foreign-space t) red green blue (pixel 0))
  (unless foreign-address
    (setq foreign-address (make-xcolor :in-foreign-space in-foreign-space))
    (setf (x11::xcolor-red foreign-address) red
	  (x11:xcolor-green foreign-address) green
	  (x11:xcolor-blue foreign-address) blue
	  (x11:xcolor-flags foreign-address) 255
	  (x11:xcolor-pixel foreign-address) pixel)
    (setf (foreign-pointer-address x) foreign-address)))

(defmethod print-object ((o color) s)
  (print-unreadable-object
   (o s :type t :identity t)
   (let ((cm o))
     (format s "~D:~D,~D,~D"
	     (x11:xcolor-pixel cm)
	     (x11:xcolor-red cm)
	     (x11:xcolor-green cm)
	     (x11:xcolor-blue cm)))))

(defun lookup-color (colormap name)
  (let ((exact (make-xcolor))
	(closest (make-xcolor)))
    (unless (zerop (x11:xlookupcolor
		    (object-display colormap)
		    colormap
		    (lisp-string-to-string8 name)
		    exact
		    closest))
      (values (make-instance 'color :foreign-address exact)
	      (make-instance 'color :foreign-address closest)))))

(defun parse-color (colormap name)
  (let ((exact (make-xcolor :in-foreign-space t)))
    (unless (zerop (x11:xparsecolor
		    (object-display colormap)
		    colormap
		    (lisp-string-to-string8 name)
		    exact))
      (values (make-instance 'color :foreign-address exact)))))

(defun allocate-color (colormap x)
  (let ((y (make-xcolor))
	(x x))
    (setf (x11:xcolor-red y) (x11:xcolor-red x)
	  (x11:xcolor-green y) (x11:xcolor-green x)
	  (x11:xcolor-blue y) (x11:xcolor-blue x))
    (let ((z (x11:xalloccolor
	      (object-display colormap)
	      colormap
	      y)))
      ;;-- This needs to be handled intelligently
      ;;-- Perhaps the colormap code should enable the user to
      ;;-- intercept this or we should just resort to some kind of stippling
      (if (zerop z)
	  (error 'x-colormap-full)
	(values (x11:xcolor-pixel y)
		(make-instance 'color
		  :in-foreign-space t
		  :foreign-address y))))))

(defun free-color-cells (colormap pixel planes)
  (with-unsigned-long-array (pixels 1)
    (setf (unsigned-long-array pixels 0) pixel)
    (x11:xfreecolors
     (object-display colormap)
     colormap
     pixels
     1
     planes)))

(defun allocate-named-color (colormap name)
  (let ((exact (make-xcolor))
	(closest (make-xcolor)))
    (if (zerop (x11:xallocnamedcolor
		(object-display colormap)
		colormap
		name
		closest
		exact))
	(error "Could not find ~S in colormap ~S" name colormap)
      (x11:xcolor-pixel closest))))

(defun query-color (colormap x)
  ;;--- Resource time
  (let ((y (make-xcolor)))
    (setf (x11:xcolor-pixel y) x)
    (x11:xquerycolor
     (object-display colormap)
     colormap
     y)
    (values
     (x11:xcolor-red y)
     (x11:xcolor-green y)
     (x11:xcolor-blue y))))

(defun store-color (colormap color)
  (x11:xstorecolor
   (object-display colormap)
   colormap
   color))

(defun store-named-color (colormap name pixel flags)
  (x11:xstorenamedcolor
   (object-display colormap)
   colormap
   name
   pixel
   flags))

#+ignore ;; moved to ./macros.lisp
(defmacro def-foreign-array-resource (name constructor)
  `(progn
     (clim-sys:defresource ,name (n)
       :constructor (cons n (,constructor :number n))
       :matcher (not (< (car ,name) n)))
     (defmacro ,(intern (format nil "~A-~A" 'with name))
	 ((var n) &body body)
       `(clim-sys:using-resource (,var ,',name ,n)
	  (let ((,var (cdr ,var)))
	    ,@body)))))

(defun make-xcolor-array (&key (number 1) in-foreign-space (initialize t))
  (declare (ignore in-foreign-space))
  (clim-utils::allocate-cstruct 'x11::xcolor-array
				:number number :initialize initialize))

(defun make-xsegment-array (&key (number 1) in-foreign-space (initialize t))
  (declare (ignore in-foreign-space))
  (clim-utils::allocate-cstruct 'x11::xsegment-array
				:number number :initialize initialize))

(defun make-xpoint-array (&key (number 1) in-foreign-space (initialize t))
  (declare (ignore in-foreign-space))
  (clim-utils::allocate-cstruct 'x11::xpoint-array
				:number number :initialize initialize))

(defun make-xrectangle-array (&key (number 1) in-foreign-space (initialize t))
  (declare (ignore in-foreign-space))
  (clim-utils::allocate-cstruct 'x11::xrectangle-array
				:number number :initialize initialize))

(defun make-xarc-array (&key (number 1) in-foreign-space (initialize t))
  (declare (ignore in-foreign-space))
  (clim-utils::allocate-cstruct 'x11::xarc-array
				:number number :initialize initialize))

(def-foreign-array-resource xcolor-array make-xcolor-array)
(def-foreign-array-resource xsegment-array make-xsegment-array)
(def-foreign-array-resource xpoint-array make-xpoint-array)
(def-foreign-array-resource xrectangle-array make-xrectangle-array)
(def-foreign-array-resource xarc-array make-xarc-array)

(defun store-colors (colormap colors ncolors)
  (x11:xstorecolors
   (object-display colormap)
   colormap
   colors
   ncolors))

(defun alloc-color-cells (colormap ncolors nplanes)
  (with-unsigned-long-array (masks nplanes)
    (with-unsigned-long-array (pixels ncolors)
      (let ((z (x11:xalloccolorcells
		(object-display colormap)
		colormap
		0
		masks
		nplanes
		pixels
		ncolors)))
	(when (zerop z)
	  (error 'x-colormap-full))
	(values pixels masks)))))


(defun default-screen (display)
  (intern-object-address
   (x11:xdefaultscreenofdisplay
    display)
   'screen
   :display display))

(defun get-input-focus (display)
  (with-ref-par
      ((focus 0 :unsigned-long)
       (revert-to 0 :int))
    (x11:xgetinputfocus display &focus &revert-to)
    (values focus revert-to)))

(defun set-input-focus (display focus &optional (revert-to 0) (time 0))
  (x11:xsetinputfocus display focus revert-to time))


(defun query-pointer (window)
  (with-ref-par
      ((root 0 :unsigned-long)
       (child 0 :unsigned-long)
       (root-x 0 :int)
       (root-y 0 :int)
       (x 0 :int)
       (y 0 :int)
       (mask 0 :unsigned-int))
    (let ((display (object-display window)))
      (if (x11:xquerypointer
	   display
	   window
	   &root
	   &child
	   &root-x
	   &root-y
	   &x
	   &y
	   &mask)
	  (values
	   t
	   (intern-object-xid
	    root
	    'window
	    display
	    :display display)
	   (intern-object-xid
	    child
	    'window
	    display
	    :display display)
	   root-x
	   root-y
	   x
	   y
	   mask)))))



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
  (declare (optimize (speed 3) (safety 0)))
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

;; image support

(defclass image (display-object)
  ((width :reader image-width :initarg :width)
   (height :reader image-height :initarg :height)
   (depth :reader image-depth :initarg :depth)
   (format :reader image-format :initform x11:zpixmap :initarg :format)))

;; I don't believe that this should be hard-wired
(defconstant bitmap-pad 8)

(defmethod initialize-instance :after
	   ((image image) &key foreign-address data)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (display format depth width height) image
    (unless foreign-address
      (let* ((bits-per-pixel (x11:_xgetbitsperpixel display depth))
	     (bits-per-line (* width bits-per-pixel))
	     (padded-bits-per-line (* bitmap-pad
				      (ceiling bits-per-line bitmap-pad)))
	     (bytes-per-line (/ padded-bits-per-line 8))
	     (v (x11::system-malloc (* bytes-per-line height)))
	     (visual (x11:screen-root-visual
		      (x11:xdefaultscreenofdisplay display)))
	     (offset 0)
	     (x (x11:xcreateimage display
				  visual
				  depth
				  format
				  offset
				  v
				  width
				  height
				  bitmap-pad
				  bytes-per-line)))
	(setf (foreign-pointer-address image) x)
	(when data
	  (case depth
	    (8
	     (let ((u 0))
	       (dotimes (h height)
		 (dotimes (w width)
		   (setf (sys::memref-int v (+ u w) 0 :unsigned-byte)
		     (aref data h w)))
		 (incf u bytes-per-line))))
	    (t
	     (dotimes (h height)
	       (dotimes (w width)
		 (x11:xputpixel x w h (aref data h w)))))))))))

(defmethod put-image (pixmap gc image
		      &key (src-x 0) (src-y 0) (dest-x 0) (dest-y 0))
  (let ((display (object-display image)))
    (unless (eq display (object-display pixmap))
      (error "~S and ~S must be on the same display" image pixmap))
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

(defmethod get-image (pixmap &key (x 0) (y 0)
				  (width (pixmap-width pixmap))
				  (height (pixmap-height pixmap))
				  (format x11:zpixmap))
  (let* ((display (object-display pixmap))

	 ;; from pnc patch p8j010.001
	 ;;     Fix tk::get-image to properly return colors.
	 ;;     Error if color-depth is unhandled.
	 (depth (pixmap-depth pixmap))
	 (plane-mask (cond ((= depth 1)
			    1)
			   ((= depth 8)
			    #xff)
			   ((= depth 16)
			    #xffff) 
			   ((= depth 24)
			    #xffffff)
			   (t
			    (error "get-image(): pixmap-depth not handled: "
				   depth)))))
    (make-instance 'image
      :display display
      :width width
      :height height
      :depth depth
      :format format
      :foreign-address (x11:xgetimage display
				      pixmap
				      x
				      y
				      width
				      height
				      plane-mask
				      format))))

(defun get-pixel (image x y)
  (x11:xgetpixel image x y))

(defun put-pixel (image x y pixel)
  (x11:xputpixel image x y pixel))

(defun destroy-image (image)
  (x11:xdestroyimage image))

;;

(defmacro with-server-grabbed ((display) &body body)
  `(let ((.display. ,display))
     (unwind-protect
	 (let ((result nil))
	  (x11:xgrabserver .display.)
	  (x11:xflush .display.)
	  (excl:errorset (setq result (multiple-value-list (progn ,@body))) t)
	  (values-list (cdr result)))
       (x11:xungrabserver .display.)
       (x11:xflush .display.))))

;;; Visuals

(defun screen-root-visual-class (screen)
  (ecase (x11::visual-class (x11::screen-root-visual screen))
    (0 :static-gray)
    (2 :static-color)
    (4 :true-color)
    (1 :gray-scale)
    (3 :pseudo-color)
    (5 :direct-color)))

;;; limited cut buffer support

(defun store-cut-buffer (display string)
  ;; clear any existing selection
  (x11:xsetselectionowner
   display
   x11:xa-primary
   x11:none
   x11:currenttime)
  (x11:xchangeproperty
   display
   (x11:xdefaultrootwindow display)
   x11:xa-cut-buffer0
   x11:xa-string
   8
   x11:propmodereplace
   (excl::string-to-native string) ;; spr25829
   (length string)))

(defun get-cut-buffer (display)
  (let (#+ignore (string (make-string 1024)))
    (with-ref-par
	((actual-type 0 :unsigned-long)
	 (actual-format 0 :int)
	 (nitems 0 :unsigned-long)
	 (bytes-after 0 :unsigned-long)
	 (prop 0 *))
      (x11:xgetwindowproperty
       display
       (x11:xdefaultrootwindow display)
       x11:xa-cut-buffer0
       0
       256
       0
       x11:anypropertytype
       &actual-type
       &actual-format
       &nitems
       &bytes-after
       &prop)
      (values (excl:native-to-string prop)))))
