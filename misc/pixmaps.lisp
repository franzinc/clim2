;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T; Lowercase: Yes -*-
;;; Patch file for Private version 0.0
;;; Reason: Add support for pixmaps and pixmap streams to CLIM.
;;; 
;;; Function CLIM:WITH-OUTPUT-TO-PIXMAP-STREAM:  New, creates a pixmap stream.
;;; 
;;; DEFOPERATION CLIM:COPY-AREA:  Replacement for COPY-AREA-INTERNAL.
;;; Function (CLOS:METHOD CLIM::WINDOW-SHIFT-VISIBLE-REGION (CLIM::WINDOW-MIXIN T T T T T T T T)):
;;;   Use it.
;;; CLOS class CLIM::BASIC-PIXMAP:  The basic pixmap class, with PIXMAP-WIDTH and PIXMAP-HEIGHT
;;;   readers for the allocated width and height.
;;; 
;;; The methods for Genera sheets.
;;; Function (CLOS:METHOD CLIM:COPY-AREA (CLIM::SHEET-IMPLEMENTATION-MIXIN T T T T T T)):  ..
;;; CLOS class CLIM::GENERA-PIXMAP:  ..
;;; Function (CLOS:METHOD CLIM:ALLOCATE-PIXMAP (CLIM::SHEET-IMPLEMENTATION-MIXIN T T)):  ..
;;; Function (CLOS:METHOD CLIM:DEALLOCATE-PIXMAP (CLIM::GENERA-PIXMAP)):  ..
;;; Function (CLOS:METHOD CLIM:COPY-TO-PIXMAP (CLIM::SHEET-IMPLEMENTATION-MIXIN T T T T)):  ..
;;; Function (CLOS:METHOD CLIM:COPY-FROM-PIXMAP (CLIM::GENERA-PIXMAP T T T T CLIM::SHEET-IMPLEMENTATION-MIXIN T T)):  ..
;;; CLOS class CLIM::GENERA-PIXMAP-STREAM:  ..
;;; function (CLOS:METHOD CLOS:INITIALIZE-INSTANCE (CLIM::GENERA-PIXMAP-STREAM) :AFTER):  ..
;;; Function (CLOS:METHOD (SETF CLIM:MEDIUM-FOREGROUND) (T CLIM::GENERA-PIXMAP-STREAM) :AFTER):  ..
;;; Function (CLOS:METHOD (SETF CLIM:MEDIUM-BACKGROUND) (T CLIM::GENERA-PIXMAP-STREAM) :AFTER):  ..
;;; Function (CLOS:METHOD CLIM::INVOKE-WITH-OUTPUT-TO-PIXMAP (CLIM::SHEET-IMPLEMENTATION-MIXIN T)):  ..
;;; 
;;; The methods for CLX windows.
;;; Function (CLOS:METHOD CLIM:COPY-AREA (CLIM::CLX-WINDOW T T T T T T)):  ..
;;; CLOS class CLIM::CLX-PIXMAP:  ..
;;; Function (CLOS:METHOD CLIM:ALLOCATE-PIXMAP (CLIM::CLX-WINDOW T T)):  ..
;;; Function (CLOS:METHOD CLIM:DEALLOCATE-PIXMAP (CLIM::CLX-PIXMAP)):  ..
;;; Function (CLOS:METHOD CLIM:COPY-TO-PIXMAP (CLIM::CLX-WINDOW T T T T)):  ..
;;; Function (CLOS:METHOD CLIM:COPY-FROM-PIXMAP (CLIM::CLX-PIXMAP T T T T CLIM::CLX-WINDOW T T)):  ..
;;; CLOS class CLIM::CLX-PIXMAP-STREAM:  ..
;;; Function (CLOS:METHOD CLOS:INITIALIZE-INSTANCE (CLIM::CLX-PIXMAP-STREAM) :AFTER):  ..
;;; function (CLOS:METHOD (SETF CLIM:MEDIUM-FOREGROUND) (T CLIM::CLX-PIXMAP-STREAM) :AFTER):  ..
;;; Function (CLOS:METHOD (SETF CLIM:MEDIUM-BACKGROUND) (T CLIM::CLX-PIXMAP-STREAM) :AFTER):  ..
;;; function (CLOS:METHOD CLIM::INVOKE-WITH-OUTPUT-TO-PIXMAP (CLIM::CLX-WINDOW T)):  ..
;;; Written by SWM, 18-Oct-91 18:09:01
;;; while running on Evening Grosbeak from FEP0:>dSCRC-439-25-from-IH-Sys439-25.load.1
;;; with Experimental System 439.45, Experimental CLOS 424.5, Experimental RPC 428.1,
;;; Experimental Embedding Support 420.0, Experimental MacIvory Support 434.0,
;;; Experimental UX Support 429.0, Experimental Development Utilities 424.4,
;;; Experimental Old TV 422.0, Experimental Zwei 422.5, Experimental Utilities 432.0,
;;; Experimental RPC Development 423.0, Experimental MacIvory Development 422.0,
;;; Experimental UX Development 428.0, Experimental Server Utilities 430.1,
;;; Experimental Serial 423.1, Experimental Hardcopy 433.0, Experimental Zmail 430.0,
;;; Experimental LMFS Defstorage 406.0, Experimental SCSI 419.0,
;;; Experimental Tape 432.1, Experimental LMFS 431.2, Experimental NSage 428.0,
;;; Experimental Extended Help 429.0, Experimental CL Developer 416.0,
;;; Experimental Documentation Database 429.13, Experimental IP-TCP 438.2,
;;; Experimental IP-TCP Documentation 411.0, Experimental CLX 434.0,
;;; Experimental X Remote Screen 432.0, Experimental X Documentation 410.0,
;;; Experimental NFS Client 428.0, Experimental NFS Documentation 412.0,
;;; Experimental DNA 427.0, Experimental Metering 432.0,
;;; Experimental Metering Substrate 432.0, Experimental Conversion Tools 423.0,
;;; Experimental Hacks 428.0, Experimental Mac Dex 421.0,
;;; Experimental HyperCard/MacIvory 421.0, Experimental Statice Runtime 412.10,
;;; Experimental Statice 433.0, Experimental Statice Browser 409.0,
;;; Experimental Statice Documentation 415.0, Experimental CLIM 33.21,
;;; Experimental CLIM Documentation 33.1, Experimental CLIM Demo 33.2,
;;; Experimental Lock Simple 425.0, Version Control 404.4, Compare Merge 403.0,
;;; VC Documentation 401.0, Symbolics In-House 431.0,
;;; Symbolics In-House Documentation 413.6, SCRC 429.0, Weather User 413.0,
;;; Logical Pathnames Translation Files NEWEST, cold load 2, microcode 3645-MIC 430,
;;; FEP 127, FEP0:>V127-lisp.flod(64), FEP0:>V127-loaders.flod(64),
;;; FEP0:>V127-info.flod(64), FEP0:>V127-debug.flod(38), 1067x752 B&W Screen,
;;; Machine serial number 10129,


#+Genera
(SYSTEM-INTERNALS:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:CLIM;CLIM;CLX-IMPLEMENTATION.LISP.236"
  "SYS:CLIM;CLIM;GENERA-IMPLEMENTATION.LISP.285"
  "SYS:CLIM;CLIM;WINDOW-PROTOCOL.LISP.76"
  "SYS:CLIM;CLIM;STREAM-DEFPROTOCOLS.LISP.71"
  "SYS:CLIM;CLIM;CLIM-DEFS.LISP.149"
  "SYS:WINDOW;BITMAP-SCREEN.LISP.37")


;;--- Make DRAGGING-OUTPUT-RECORD use pixmaps when possible


;=====================================
#+Genera (SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
#+Genera (SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "SYS:CLIM;CLIM;CLIM-DEFS.LISP.149")
#+Genera (SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-")

(eval-when (compile load eval)
  (scl:export 'clim::(with-output-to-pixmap-stream) 'clim))

(defmacro with-output-to-pixmap-stream ((stream) &body body)
  (default-output-stream stream with-output-to-pixmap-stream)
  `(flet ((with-output-to-pixmap-body (,stream) ,@body))
     (declare (dynamic-extent #'with-output-to-pixmap-body))
     (invoke-with-output-to-pixmap ,stream #'with-output-to-pixmap-body)))


;=====================================
#+Genera (SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
#+Genera (SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "SYS:CLIM;CLIM;STREAM-DEFPROTOCOLS.LISP.71")
#+Genera (SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-")

(eval-when (compile load eval)
  (scl:export 'clim::(copy-area
		      allocate-pixmap deallocate-pixmap
		      copy-to-pixmap copy-from-pixmap
		      pixmap-width pixmap-height)))

;; Replaces COPY-AREA-INTERNAL
(scl:fundefine 'copy-area-internal)
(defoperation copy-area window-mixin
  ((window window-mixin) from-x from-y width height to-x to-y))


;=====================================
#+Genera (SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
#+Genera (SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "SYS:CLIM;CLIM;WINDOW-PROTOCOL.LISP.76")
#+Genera (SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-")

;; This is called by output-recording-mixin's whopper on set-viewport-position*.
;; It shifts a region of the "host screen" that's visible to some other visible
;; location.  It does NOT do any cleaning up after itself.  It does not side-effect
;; the output history of the window.  It calls COPY-AREA-INTERNAL whose contract is to 
;; do the above, the whole above, and nothing but the above.
(defmethod window-shift-visible-region ((window window-mixin)
					old-left old-top old-right old-bottom
					new-left new-top new-right new-bottom)
  (declare (fixnum new-left new-top new-right new-bottom))
  (declare (ignore old-right old-bottom new-right new-bottom))
  (let ((delta-x (the fixnum (- old-left new-left)))
	(delta-y (the fixnum (- old-top new-top))))
    (multiple-value-bind (stream-width stream-height)
	(window-inside-size window)
      (declare (fixnum stream-width stream-height))
      (let (from-x from-y)
	(cond ((and (>= delta-x 0)
		    (>= delta-y 0))
	       ;; shifting down and to the right
	       (setq from-x 0
		     from-y 0))
	      ((and (>= delta-x 0)
		    (<= delta-y 0))
	       ;; shifting up and to the right
	       (setq from-x 0
		     from-y (abs delta-y)))
	      ((>= delta-y 0)
	       ;; shifting down and to the left
	       (setq from-x (abs delta-x)
		     from-y 0))
	      (t
	       ;; shifting up and to the left
	       (setq from-x (abs delta-x)
		     from-y (abs delta-y))))
	(let ((width (the fixnum (- stream-width (abs (the fixnum delta-x)))))
	      (height (the fixnum (- stream-height (abs (the fixnum delta-y))))))
	  (multiple-value-bind (ml mt) (window-margins window)
	    (declare (fixnum ml mt))
	    (translate-fixnum-positions ml mt from-x from-y))
	  (let ((to-x (the fixnum (+ from-x delta-x)))
		(to-y (the fixnum (+ from-y delta-y))))
	    (copy-area window from-x from-y width height to-x to-y)))))))


;;; Basic pixmaps

(defclass basic-pixmap ()
  ((pixmap :initarg :pixmap)
   (screen :initarg :screen)
   (width :initarg :width :reader pixmap-width)
   (height :initarg :height :reader pixmap-height)))

(defmethod copy-to-pixmap ((stream encapsulating-stream-mixin)
			   window-x window-y width height
			   &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (copy-to-pixmap (slot-value stream 'stream)
		  window-x window-y width height
		  pixmap pixmap-x pixmap-y))

(defmethod copy-from-pixmap (pixmap pixmap-x pixmap-y width height
			     (stream encapsulating-stream-mixin) window-x window-y)
  (copy-from-pixmap pixmap pixmap-x pixmap-y width height
		    (slot-value stream 'stream) window-x window-y))


;=====================================
#+Genera (SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
#+Genera (SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "SYS:CLIM;CLIM;GENERA-IMPLEMENTATION.LISP.285")
#+Genera (SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-")

;;; Pixmaps

;; Replaces COPY-AREA-INTERNAL
(defmethod copy-area ((stream sheet-implementation-mixin)
		      from-x from-y width height to-x to-y)
  ;; Coords in "host" coordinate system
  (when (window-drawing-possible stream)
    (fix-points from-x from-y width height to-x to-y)
    (with-slots (window) stream
      (when (>= to-x from-x)
	;; Shifting to the right
	(setq width (- (abs width))))
      (when (>= to-y from-y)
	(setq height (- (abs height))))
      (scl:send window :bitblt-within-sheet 
		       tv:alu-seta width height from-x from-y to-x to-y))))


(defclass genera-pixmap (basic-pixmap))

(defmethod allocate-pixmap ((stream sheet-implementation-mixin) width height)
  (with-slots (window) stream
    (let* ((root (window-root stream))
	   (screen (slot-value root 'window))
	   (pixmap (tv:allocate-bitmap-stream :for-stream window
					      :width width :height height
					      :host-allowed t)))
      (make-instance 'genera-pixmap 
		     :pixmap pixmap :screen screen
		     :width width :height height))))

(defmethod deallocate-pixmap ((pixmap genera-pixmap))
  (with-slots (pixmap) pixmap
    (tv:deallocate-bitmap-stream pixmap)
    (setq pixmap nil)))

(defmethod copy-to-pixmap ((stream sheet-implementation-mixin)
			   window-x window-y width height
			   &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (fix-points window-x window-y width height pixmap-x pixmap-y)
  (with-slots (window) stream
    (cond ((null pixmap)
	   (setq pixmap (allocate-pixmap stream width height)))
	  (t
	   (check-type pixmap genera-pixmap)
	   (let* ((root (window-root stream))
		  (screen (slot-value root 'window)))
	     (assert (eql screen (slot-value pixmap 'screen)) ()
		     "You can only use a pixmap on the screen for which it was allocated"))))
    (scl:send (slot-value pixmap 'pixmap) :bitblt-from-sheet
	      tv:alu-seta width height pixmap-x pixmap-y
	      window window-x window-y))
  pixmap)

(defmethod copy-from-pixmap ((pixmap genera-pixmap) pixmap-x pixmap-y width height
			     (stream sheet-implementation-mixin) window-x window-y)
  (fix-points window-x window-y width height pixmap-x pixmap-y)
  (let* ((root (window-root stream))
	 (screen (slot-value root 'window)))
    (assert (eql screen (slot-value pixmap 'screen)) ()
	    "You can only use a pixmap on the screen for which it was allocated"))
  (with-slots (window) stream
    (scl:send (slot-value pixmap 'pixmap) :bitblt-to-sheet
	      tv:alu-seta width height pixmap-x pixmap-y
	      window window-x window-y))
  pixmap)


(defclass genera-pixmap-stream
	  (sheet-implementation-mixin window-stream)
    ()
  (:default-initargs :display-device-type nil
		     :text-cursor nil))

(defmethod initialize-instance :after ((stream genera-pixmap-stream) &key parent pixmap)
  (assert (not (null parent)) ()
	  "You must provide a root window for Genera pixmap streams")
  (with-slots (display-device-type) stream
    (unless display-device-type
      (setf display-device-type (slot-value parent 'display-device-type))))
  (let ((root-sheet (slot-value (window-root parent) 'window))
	(parent-sheet (slot-value parent 'window))
	(sheet (slot-value pixmap 'pixmap)))
    (with-slots (left top right bottom) stream
      (multiple-value-bind (width height) (scl:send parent-sheet :size)
	(setf left 0
	      top  0
	      right  width
	      bottom height)))
    (setf (slot-value stream 'window) sheet)
    (setf (slot-value stream 'color-p) (color:color-stream-p root-sheet))
    (setf (slot-value stream 'embedded-p)
	  (typep (tv:sheet-screen sheet) 'tv:basic-remote-screen))
    (setf (medium-foreground stream) (medium-foreground parent))
    (setf (medium-background stream) (medium-background parent))
    (associate-clim-window-with-host-window sheet stream)))

(defmethod (setf medium-foreground) :after (ink (stream genera-pixmap-stream))
  (with-slots (window) stream	   
    (scl:send window :set-char-aluf (sheet-decode-color ink stream nil))))

(defmethod (setf medium-background) :after (ink (stream genera-pixmap-stream))
  (with-slots (window) stream	   
    (scl:send window :set-erase-aluf (sheet-decode-color ink stream nil))))

(defmethod invoke-with-output-to-pixmap ((stream sheet-implementation-mixin) continuation)
  (let* ((pixmap (allocate-pixmap stream nil nil))
	 (stream (make-instance 'genera-pixmap-stream 
				:parent stream :pixmap pixmap)))
    (funcall continuation stream)
    pixmap))


;=====================================
#+Genera (SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
#+Genera (SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "SYS:CLIM;CLIM;CLX-IMPLEMENTATION.LISP.236")
#+Genera (SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-")

;;; Pixmaps

;; Replaces COPY-AREA-INTERNAL
(defmethod copy-area ((stream clx-window) from-x from-y width height to-x to-y)
  ;; Coords in "host" coordinate system
  (fix-points from-x from-y width height to-x to-y)
  (with-slots (window copy-gc) stream
    (xlib:copy-area window copy-gc from-x from-y width height window to-x to-y))
  (clx-force-output-if-necessary stream))


(defclass clx-pixmap (basic-pixmap))

(defmethod allocate-pixmap ((stream clx-window) width height)
  (with-slots (window) stream
    (let* ((root (window-root stream))
	   (screen (slot-value root 'window))
	   (pixmap (xlib:create-pixmap :drawable window
				       :width width :height height
				       :depth (xlib:drawable-depth window))))
      (make-instance 'clx-pixmap 
		     :pixmap pixmap :screen screen
		     :width width :height height))))

(defmethod deallocate-pixmap ((pixmap clx-pixmap))
  (with-slots (pixmap) pixmap
    (xlib:free-pixmap pixmap)
    (setq pixmap nil)))

(defmethod copy-to-pixmap ((stream clx-window) window-x window-y width height
			   &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (fix-points window-x window-y width height pixmap-x pixmap-y)
  (with-slots (window copy-gc) stream
    (cond ((null pixmap)
	   (setq pixmap (allocate-pixmap stream width height)))
	  (t
	   (check-type pixmap clx-pixmap)
	   (let* ((root (window-root stream))
		  (screen (slot-value root 'window)))
	     (assert (eql screen (slot-value pixmap 'screen)) ()
		     "You can only use a pixmap on the screen for which it was allocated"))))
    (xlib:copy-area window copy-gc window-x window-y width height
		    (slot-value pixmap 'pixmap) pixmap-x pixmap-y))
  pixmap)

(defmethod copy-from-pixmap ((pixmap clx-pixmap) pixmap-x pixmap-y width height
			     (stream clx-window) window-x window-y)
  (fix-points window-x window-y width height pixmap-x pixmap-y)
  (let* ((root (window-root stream))
	 (screen (slot-value root 'window)))
    (assert (eql screen (slot-value pixmap 'screen)) ()
	    "You can only use a pixmap on the screen for which it was allocated"))
  (with-slots (window copy-gc) stream
    (xlib:copy-area (slot-value pixmap 'pixmap) copy-gc pixmap-x pixmap-y width height
		    window window-x window-y))
  (clx-force-output-if-necessary stream)
  pixmap)


(defclass clx-pixmap-stream
	  (clx-window)
    ()
  (:default-initargs :display-device-type nil
		     :text-cursor nil))

(defmethod initialize-instance :after ((stream clx-pixmap-stream) &key parent pixmap)
  (assert (not (null parent)) ()
	  "You must provide a root window for CLX pixmap streams")
  (with-slots (display-device-type) stream
    (unless display-device-type
      (setf display-device-type (slot-value parent 'display-device-type))))
  (let ((sheet (slot-value pixmap 'pixmap)))
    (with-slots (root screen window left top right bottom
		 color-p white-pixel black-pixel copy-gc points-to-pixels) stream
      (setf root (clx-stream-root parent))
      (setf screen (clx-stream-screen parent))
      (setf window sheet)
      (setf left 0)
      (setf top 0)
      (setf right (xlib:screen-width screen))
      (setf bottom (xlib:screen-height screen))
      (setf color-p (color-stream-p stream))
      (setf color-p (color-stream-p stream)) 
      (setf black-pixel (xlib:screen-black-pixel screen))
      (setf copy-gc (xlib:create-gcontext :drawable window :exposures nil))
      (setf points-to-pixels (* (sqrt (* (/ (xlib:screen-width screen)
					    (xlib:screen-width-in-millimeters screen))
					 (/ (xlib:screen-height screen)
					    (xlib:screen-height-in-millimeters screen))))
				(/ 25.4s0 72))) 
      (let ((rounded (round points-to-pixels)))
	(when (< (abs (- points-to-pixels rounded)) .1s0)
	  (setf points-to-pixels rounded)))
      (clx-recompute-gcs stream)
      (setf (getf (xlib:window-plist window) 'stream) stream)))
  (setf (medium-foreground stream) (medium-foreground parent))
  (setf (medium-background stream) (medium-background parent)))

(defmethod (setf medium-foreground) :after (ink (stream clx-pixmap-stream))
  (declare (ignore ink))
  (clx-recompute-gcs stream))

(defmethod (setf medium-background) :after (ink (stream clx-pixmap-stream))
  (declare (ignore ink))
  (clx-recompute-gcs stream))

(defmethod invoke-with-output-to-pixmap ((stream clx-window) continuation)
  (let* ((pixmap (allocate-pixmap stream nil nil))
	 (stream (make-instance 'clx-pixmap-stream 
				:parent stream :pixmap pixmap)))
    (funcall continuation stream)
    pixmap))

