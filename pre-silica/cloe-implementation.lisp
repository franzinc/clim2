;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; Support for the shared class DC.

(defvar *cloe-device*)
(defvar *dc*)

(defvar *current-pen*)
(defvar *current-brush*)
(defvar *current-rop2*)
(defvar *current-background-color*)
(defvar *current-text-color*)
(defvar *current-font*)

;;; Stock objects

(defvar *null-pen*)
(defvar *black-pen*)
(defvar *white-pen*)

(defvar *null-brush*)
(defvar *black-brush*)
(defvar *white-brush*)

;;; Original objects

(defvar *color-to-image* (make-hash-table))
(defvar *black-image*)
(defvar *white-image*)

(defvar *ink-to-image* (make-hash-table))

(defstruct (dc-image (:predicate nil))
  (bitmap nil)
  solid-1-pen
  (pen-table (make-hash-table))
  brush
  (rop2 win::r2_copypen)
  text-color
  background-color)

;;;

(eval-when (compile load eval)
	   
;; Define the new key chars for Cloe CLIM.  Regular Cloe defines 0-127, we define
;; 128-139 as the F-keys (F1 thru F12), 140 for c-sh-A, and 141 as c-sh-V
(sys::define-character-name "F1" 128)
(sys::define-character-name "F2" 129)
(sys::define-character-name "F3" 130)
(sys::define-character-name "F4" 131)
(sys::define-character-name "F5" 132)
(sys::define-character-name "F6" 133)
(sys::define-character-name "F7" 134)
(sys::define-character-name "F8" 135)
(sys::define-character-name "F9" 136)
;; Note windows traps F10 as alt-space. Why?
(sys::define-character-name "F10" 137)
(sys::define-character-name "F11" 138)
(sys::define-character-name "F12" 139)
(sys::define-character-name "Arglist" 140)
(sys::define-character-name "ShowValue" 141)

)	;eval-when


;;;

(defun initialize-dc ()
  ;; Dummy window to represent class.
  (setf *dc* (win::create-window "Vanilla" "CLIM" win::ws_popup 0 0 0 0 0 0 0 "arg"))

  ;; Stock objects
  (setf *null-pen* (win::get-stock-object win::null_pen))
  (setf *black-pen* (win::get-stock-object win::black_pen))
  (setf *white-pen* (win::get-stock-object win::white_pen))
  (setf *current-pen* *black-pen*)

  (setf *null-brush* (win::get-stock-object win::null_brush))
  (setf *black-brush* (win::get-stock-object win::black_brush))
  (setf *white-brush* (win::get-stock-object win::white_brush))
  (setf *current-brush* *white-brush*)

  (setf *current-rop2* win::r2_copypen)
  (setf *current-background-color* #xffffff)
  (setf *current-text-color* #x000000)
  (setf *current-font* nil)

  (setf *black-image*
	(make-dc-image :solid-1-pen *black-pen* :brush *black-brush*
		       :text-color #x000000 :background-color nil))
  (setf (gethash #x000000 *color-to-image*) *black-image*)
  (setf *white-image*
	(make-dc-image :solid-1-pen *white-pen* :brush *white-brush*
		       :text-color #xffffff :background-color nil))
  (setf (gethash #xffffff *color-to-image*) *white-image*)
  )

(defun select-pen (pen)
  (unless (eql *current-pen* pen)
    (win::select-pen *dc* pen)
    (setf *current-pen* pen))
  pen)

(defun select-brush (brush)
  (unless (eql *current-brush* brush)
    (win::select-brush *dc* brush)
    (setf *current-brush* brush))
  brush)

(defun select-rop2 (rop2)
  (unless (eql *current-rop2* rop2)
    (win::set-rop2 *dc* rop2)
    (setf *current-rop2* rop2))
  rop2)

(defun select-background-color (color)
  (unless (eql *current-background-color* color)
    (cond ((null color)
	   #||(win::set-background-mode *dc* win::transparent)||#)
	  (t
	   #||(when (null *current-background-color*)
		(win::set-background-mode *dc* win::opaque))||#
	   (win::set-background-color *dc* color)
    (setf *current-background-color* color))))
  color)

(defun select-text-color (color)
  (unless (eql *current-text-color* color)
    (win::set-text-color *dc* color)
    (setf *current-text-color* color))
  color)

(defun select-font (font)
  (unless (eql *current-font* font)
    (win::select-font *dc* font)
    (setf *current-font* font))
  font)

;;;

(defun set-dc-for-drawing (image line-style)
  (let ((dashes (line-style-dashes line-style)))
    (select-pen
      (let* ((thickness (max 1 (round (line-style-thickness line-style))))
	     (code (if dashes (- thickness) thickness)))
	(declare (fixnum thickness code))
	(if (= code 1)
	    (dc-image-solid-1-pen image)
	    (or (gethash code (dc-image-pen-table image))
		(setf (gethash code (dc-image-pen-table image))
		      (win::create-pen (if dashes win::ps_dash win::ps_solid)
				       thickness
				       (dc-image-text-color image)))))))
    (when dashes
      (select-background-color nil)))
  (select-brush *null-brush*)
  (select-rop2 (dc-image-rop2 image))
  image)

(defun set-dc-for-filling (image)
  (select-pen *null-pen*)
  (select-brush (dc-image-brush image))
  (let ((background-color (dc-image-background-color image)))
    (when background-color
      (select-background-color background-color)))
  (select-rop2 (dc-image-rop2 image)))

(defun set-dc-for-ink (stream ink line-style)
  (let ((image (dc-image-for-ink stream ink)))
    (if line-style
	(set-dc-for-drawing image line-style)
	(set-dc-for-filling image))))

(defun set-dc-for-text (stream ink font)
  (select-text-color (dc-image-text-color (dc-image-for-ink stream ink)))
  (select-background-color nil)
  (select-font font))



(defclass cloe-root-window
	  (window-stream)
    ())

(defmethod initialize-instance :before ((stream cloe-root-window) &key)
  (unless win::*windows-channel*
    (win::connect-to-winfe))
  (with-slots (left top right bottom) stream
    (setf left 0 top 0)
    (multiple-value-setq (right bottom)
      (win::get-screen-size)))
  (initialize-dc)
  (setf (slot-value stream 'display-device-type)
	(make-instance 'cloe-display-device
		       :logpixelsy (win::get-device-caps *dc* 90))))

(defmethod initialize-instance :after ((stream cloe-root-window) &key)
  (setf (stream-pointers stream)
	(list (make-instance 'standard-pointer :root stream))))

(defmethod window-margins ((stream cloe-root-window))
  (values 0 0 0 0))

(defmethod host-window-margins ((stream cloe-root-window))
  (values 0 0 0 0))

(defmethod window-stream-class-name ((stream cloe-root-window))
  'cloe-window-stream)

(defmethod menu-class-name ((stream cloe-root-window))
  'cloe-window-stream)

(defmethod close ((stream cloe-root-window) &key abort)
  (declare (ignore abort))
  nil)

(defvar *cloe-root-window*)
(defvar *cloe-pointer*)

(defun create-cloe-root-window ()
  (unless (boundp '*cloe-root-window*)
    (setf *cloe-root-window* (make-instance 'cloe-root-window))
    (setf *cloe-pointer* (stream-primary-pointer *cloe-root-window*)))
  *cloe-root-window*)

(define-implementation :cloe 'create-cloe-root-window)



(defclass cloe-window-stream
	  (window-stream)
    ((window :initarg :window)
     (move-pending :initform nil)
     (event-width) (event-height)
     (event-left) (event-top)
     (margin-width) (margin-height)
     (h-scroll-pos :initform nil)
     (v-scroll-pos :initform nil)
     (pending-scrolls :initform (make-queue))
     (background-dc-image)
     (foreground-dc-image)
     (modifier-state :initform 0 :reader window-modifier-state)))

(defmethod initialize-instance :before ((stream cloe-window-stream) &key parent)
  (setf (slot-value stream 'display-device-type)
	(slot-value parent 'display-device-type)))

(defmethod initialize-instance :after ((stream cloe-window-stream) &key parent
				       label (scroll-bars :both) (borders t) save-under)
  (with-slots (window left top right bottom margin-width margin-height
		      event-left event-top event-width event-height
		      foreground-dc-image background-dc-image)
	      stream
    (let ((top-level (not (slot-exists-p parent 'window))))
      (setf window (win::create-window
		     "Vanilla" (or label "CLIM")
		     (logior  win::ws_clipchildren	;new, helps refresh
			      (if top-level
				  (if save-under
				      (logior win::ws_popup
					      win::ws_border)
				      (logior win::ws_overlapped
					      win::ws_caption
					      win::ws_thickframe
					      win::ws_sysmenu
					      win::ws_minimizebox
					      win::ws_maximizebox))
				  (logior win::ws_child
					  win::ws_clipsiblings
					  (if borders win::ws_border 0)))
			      (if (member scroll-bars '(:horizontal :both))
			          (progn (setf h-scroll-pos 0)
				  	 win::ws_hscroll)
				  0)
			      (if (member scroll-bars '(:vertical :both))
			          (progn (setf v-scroll-pos 0)
				  	 win::ws_vscroll)
				  0)
			      )
		     left top (- right left) (- bottom top)
		     (if top-level 0 (slot-value parent 'window)) 0 0 "arg")))
    (associate-clim-window-with-host-window window stream)
    (setf foreground-dc-image (dc-image-for-ink stream (medium-foreground stream)))
    (setf background-dc-image (dc-image-for-ink stream (medium-background stream)))
    (multiple-value-bind (cleft ctop cright cbottom)
	(win::get-client-rectangle window)
      (multiple-value-bind (wleft wtop wright wbottom)
	  (win::get-window-rectangle window)
	(setf margin-width (- (- wright wleft) (- cright cleft)))
	(setf margin-height (- (- wbottom wtop) (- cbottom ctop))))
      (setf event-left (+ left cleft))
      (setf event-top (+ top ctop))
      (setf event-width (- cright cleft))
      (setf event-height (- cbottom ctop))
      (setf right (+ left event-width margin-width))
      (setf bottom (+ top event-height margin-height)))
    nil))

(defmethod implementation-pixels-per-point ((stream cloe-window-stream))
  1)

(defmethod window-beep ((stream cloe-window-stream))
  (win::message-beep))

(defmethod close ((stream cloe-window-stream) &key abort)
  (declare (ignore abort))
  (with-slots (window) stream
    (win::destroy-window window)))

;;; After changing the background color, refresh the window.
(defmethod (setf medium-background) :after (new-background (stream cloe-window-stream))
  (with-slots (background-dc-image) stream
    (setf background-dc-image (dc-image-for-ink stream new-background)))
  (window-refresh stream))

(defmethod (setf medium-foreground) :after (new-foreground (stream cloe-window-stream))
  (with-slots (foreground-dc-image) stream
    (setf foreground-dc-image (dc-image-for-ink stream new-foreground)))
  (window-refresh stream))

(defmethod window-erase-viewport ((stream cloe-window-stream))
  (when (window-drawing-possible stream)
    (multiple-value-bind (width height)
	(window-inside-size stream)
      (window-clear-area stream 0 0 width height))))

(defmethod (setf window-visibility) :after (visibility (stream cloe-window-stream))
  (labels ((window-set-visibility-1 (stream visibility)
	     (win::show-window (slot-value stream 'window)
			       :type (if visibility :show-no-activate :hide))
	     (dolist (child (window-children stream))
	       (when (window-visibility child)
		 (window-set-visibility-1 child visibility)))))
    (window-set-visibility-1 stream visibility))
  (handle-generated-paints)
  visibility)

 (defmethod wait-for-window-exposed ((window cloe-window-stream))
  (loop
    (when (window-visibility window)
      (return))
    (process-yield)))

(defmethod window-drawing-possible ((stream cloe-window-stream))
  (window-visibility stream))

(defmethod window-stack-on-top ((stream cloe-window-stream))
  (with-slots (window) stream
    (win::set-window-position window 0 0 0 0 0
			      (logior win::swp_noactivate
				      win::swp_nomove
				      win::swp_nosize)))
  (handle-generated-paints))

(defmethod window-stack-on-bottom ((stream cloe-window-stream))
  (with-slots (window) stream
    (win::set-window-position window 1 0 0 0 0
			      (logior win::swp_noactivate
				      win::swp_nomove
				      win::swp_nosize)))
  (handle-generated-paints))

(defmethod bounding-rectangle-set-edges :after
	   ((stream cloe-window-stream) left top right bottom)
  (with-slots (window) stream
    (win::move-window window left top (- right left) (- bottom top) t))
  (handle-generated-paints))
  
(defmethod bounding-rectangle-set-position* :after
	   ((stream cloe-window-stream) new-left new-top)
  (multiple-value-bind (width height) (bounding-rectangle-size stream)
    (with-slots (window) stream
      (win::move-window window new-left new-top width height t)))
  (handle-generated-paints))

(defmethod bounding-rectangle-set-size :after
	   ((stream cloe-window-stream) width height)
  (multiple-value-bind (left top) (bounding-rectangle-position* stream)
    (with-slots (window) stream
      (win::move-window window left top width height t)))
  (handle-generated-paints))

(defmethod stream-force-output ((stream cloe-window-stream))
  (declare (ignore stream))
  )

(defmethod stream-clear-output ((stream cloe-window-stream))
  (declare (ignore stream))
  )

(defmethod stream-finish-output ((stream cloe-window-stream))
  (declare (ignore stream))
  )

(defmethod window-margins ((stream cloe-window-stream))
  (declare (ignore stream))
  (values 0 0 0 0))

(defmethod host-window-margins ((stream cloe-window-stream))
  (with-slots (margin-width margin-height) stream
    (values 0 0 margin-width margin-height)))

(defmethod window-flush-update-region ((stream cloe-window-stream))
  (with-slots (update-region) stream
    (setf update-region nil)))

;;;changed to fix the scrolling problem
(defmethod window-process-update-region ((stream cloe-window-stream))
  (with-slots (window update-region background-dc-image) stream
    (when update-region
      (set-dc-for-filling background-dc-image)
      (multiple-value-bind (dx dy)
	  (window-viewport-position* stream)
	(declare (type coordinate dx dy))
	(dolist (region update-region)
	  (with-bounding-rectangle* (left top right bottom) region
	    (win::rectangle window (- left dx) (- top dy)
				   (- right dx) (- bottom dy))))))))

;;; Postpone better update region stuff.
(defmethod window-shift-visible-region :after ((stream cloe-window-stream)
					       old-left old-top old-right old-bottom
					       new-left new-top new-right new-bottom)

  (setf (slot-value stream 'update-region)
	(ltrb-difference new-left new-top new-right new-bottom
			 old-left old-top old-right old-bottom)))

;; Does this want to be an :after method?  Will the primary method on
;; window-stream ever want to do anything else?
(defmethod stream-set-input-focus ((stream cloe-window-stream))
  (with-slots (window) stream
    (win::set-focus window)))

(defmethod stream-restore-input-focus ((stream cloe-window-stream) old-focus)
  (declare (ignore stream))
  (win::set-focus old-focus))

(defmethod stream-has-input-focus ((stream cloe-window-stream))
  (with-slots (window) stream
    (= window  (win::get-focus))))

(defmethod notify-user-1 ((stream cloe-window-stream) frame format-string &rest format-args)
  (declare (ignore frame) (dynamic-extent format-args))
  (let ((formatted-string (apply #'format nil format-string format-args)))
    (clos:with-slots (window) stream
      (win::notify-user window formatted-string))))

;;; Draw a rectangle in the background color.
(defmethod window-clear-area ((stream cloe-window-stream) left top right bottom)
  (with-slots (window background-dc-image) stream
    (set-dc-for-filling background-dc-image)
    (win::rectangle window left top right bottom)))

(defmethod copy-area ((stream cloe-window-stream)
		      from-left from-top from-right from-bottom
		      to-left to-top)
  (fix-points from-left from-top from-right from-bottom to-left to-top)
  (let ((x-delta (- to-left from-left))		; can be negative
	(y-delta (- to-top from-top)))		; can be negative
    (multiple-value-bind (width height)
	(window-inside-size stream)
      (with-slots (window) stream
	(win::scroll-dc window
			x-delta y-delta
			from-left from-top 
			;; --- from-bottom or 1+?
			from-right from-bottom	;rectangles are different...
			0 0 width height)))))

;;; Scrolling support.
(defmethod cloe-recompute-scroll-bars ((stream cloe-window-stream))
  ;; don't update the scroll bars if they didn't change.
  (multiple-value-bind (new-h-pos new-v-pos)
      (let ((history (and (output-recording-stream-p stream)
			  (stream-output-history stream))))
	(when history
	  (multiple-value-bind (width height) (bounding-rectangle-size history)
	    (with-bounding-rectangle* (vleft vtop vright vbottom) (window-viewport stream)
	      (values (and (< (- vright vleft) width)
			   (round (* vleft 100)
				  (- width (- vright vleft))))
		      (and (< (- vbottom vtop) height)
			   (round (* vtop 100)
				  (- height (- vbottom vtop)))))))))
    (with-slots (window h-scroll-pos v-scroll-pos) stream
      (unless (eql new-h-pos h-scroll-pos)
	(cond ((null new-h-pos)
	       ;;(win::show-scroll-bar window win::sb_horz nil)
	       (win::set-scroll-position window win::sb_horz new-h-pos))
	      (t
	       (win::set-scroll-position window win::sb_horz new-h-pos)
	       #||(when (null h-scroll-pos)
		 (win::show-scroll-bar window win::sb_horz t))||#))
	(setf h-scroll-pos new-h-pos))
      (unless (eql new-v-pos v-scroll-pos)
	(cond ((null new-v-pos)
	       ;;(win::show-scroll-bar window win::sb_vert nil)
	       (win::set-scroll-position window win::sb_vert new-v-pos))
	      (t
	       (win::set-scroll-position window win::sb_vert new-v-pos)
	       #||(when (null v-scroll-pos)
		 (win::show-scroll-bar window win::sb_vert t))||#))
	(setf v-scroll-pos new-v-pos)))))

(defmethod window-set-viewport-position* :after ((stream cloe-window-stream) new-x new-y)
  (cloe-recompute-scroll-bars stream))



(defmethod invoke-with-clipping-region ((stream cloe-window-stream) continuation region)
  (funcall continuation stream))

(defmethod draw-point-internal ((stream cloe-window-stream)
				x-offset y-offset x y ink line-style)
  (with-slots (window) stream
    (fix-points x y)
    (translate-positions x-offset y-offset x y)
    (set-dc-for-ink stream ink line-style)
    (win::rectangle window x y x y)))

(defmethod draw-line-internal ((stream cloe-window-stream) x-offset y-offset
			       start-x start-y end-x end-y ink line-style)
  (with-slots (window) stream
    (fix-points start-x start-y end-x end-y)
    (translate-positions x-offset y-offset start-x start-y end-x end-y)
    (set-dc-for-ink stream ink line-style)
    (win::move-to window start-x start-y)
    (win::line-to window end-x end-y)))

(defmethod draw-rectangle-internal ((stream cloe-window-stream) x-offset y-offset
				    left top right bottom ink line-style)
  (with-slots (window) stream
    (fix-points left top right bottom)
    (translate-positions x-offset y-offset left top right bottom)
    (set-dc-for-ink stream ink line-style)
    (win::rectangle window left top right bottom)))

(defmethod draw-polygon-internal ((stream cloe-window-stream) x-offset y-offset
				  list-of-x-and-ys closed ink line-style)
  (with-slots (window) stream
    (set-dc-for-ink stream ink line-style)
    (if (null line-style)
	(win::polygon window list-of-x-and-ys x-offset y-offset)
	(win::polyline window list-of-x-and-ys closed x-offset y-offset))))

(defvar *ft* .0001)
(defun fl-= (x y)
  (< (abs (- x y)) *ft*))

(defmethod draw-ellipse-internal ((stream cloe-window-stream) x-offset y-offset
				  center-x center-y
				  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				  start-angle end-angle ink line-style)
  (fix-points center-x center-y)
  (translate-positions x-offset y-offset center-x center-y)
  (when (null start-angle)
    (setq start-angle 0.0
	  end-angle 2pi))
  (with-slots (window) stream
    (set-dc-for-ink stream ink line-style)
    (setf radius-1-dx (round radius-1-dx))
    (setf radius-1-dy (round radius-1-dy))
    (setf radius-2-dx (round radius-2-dx))
    (setf radius-2-dy (round radius-2-dy))
    (multiple-value-bind (x-radius y-radius)
	(cond ((and (= radius-1-dx 0) (= radius-2-dy 0))
	       (values (abs radius-2-dx) (abs radius-1-dy)))
	      ((and (= radius-2-dx 0) (= radius-1-dy 0))
	       (values (abs radius-1-dx) (abs radius-2-dy)))
	      (t (nyi)))
      (let (left top right bottom)
	(setq left (- center-x x-radius)
	      right (+ center-x x-radius)
	      top (- center-y y-radius)
	      bottom (+ center-y y-radius))
	(cond ((or (fl-= (- end-angle start-angle) 2pi)
		   (fl-= (- end-angle start-angle) 0))
	       #+Ignore
	       (and (= start-angle 0)
		    (= end-angle 2pi))
	       ;; drawing a full ellipse
	       (win::ellipse window left top right bottom))
	      ((null line-style)
	       ;; drawing a pie slice
	       (win::pie window left top right bottom
			 (round (+ (* (cos end-angle) x-radius) center-x))
			 (round (+ (* (sin end-angle) y-radius) center-y))
			 (round (+ (* (cos start-angle) x-radius) center-x))
			 (round (+ (* (sin start-angle) y-radius) center-y))))
	      (t
	       ;; drawing an arc
	       (win::arc window left top right bottom
			 (round (+ (* (cos end-angle) x-radius) center-x))
			 (round (+ (* (sin end-angle) y-radius) center-y))
			 (round (+ (* (cos start-angle) x-radius) center-x))
			 (round (+ (* (sin start-angle) y-radius) center-y)))))))))



(defgeneric dc-image-for-ink (stream ink))

(defmethod dc-image-for-ink (stream (ink (eql +foreground-ink+)))
  (slot-value stream 'foreground-dc-image))

(defmethod dc-image-for-ink (stream (ink (eql +background-ink+)))
  (slot-value stream 'background-dc-image))

(defmethod dc-image-for-ink (stream (ink (eql +black+)))
  (declare (ignore stream))
  *black-image*)

(defmethod dc-image-for-ink (stream (ink (eql +white+)))
  (declare (ignore stream))
  *white-image*)

(defmethod dc-image-for-ink (stream (ink color))
  (declare (ignore stream))
  (multiple-value-bind (red green blue)
      (color-rgb ink)
    (let ((color (dpb (round (* 255 blue)) (byte 8 16)
		      (dpb (round (* 255 green)) (byte 8 8)
			   (round (* 255 red))))))
      (declare (fixnum color))
      (case color
	((#x000000) *black-image*)
	((#xffffff) *white-image*)
	(otherwise
	  (or (gethash color *color-to-image*)
	      (setf (gethash color *color-to-image*)
		    (make-dc-image :solid-1-pen (win::create-pen win::ps_solid 1 color)
				   :brush (win::create-solid-brush color)
				   :text-color color :background-color nil))))))))

(defmethod dc-image-for-ink (stream (ink rectangular-tile))
  ;; The only case we handle right now is stipples
  (or (gethash ink *ink-to-image*)
      (setf (gethash ink *ink-to-image*)
	    (multiple-value-bind (array width height)
		(decode-tile-as-stipple ink)
	      (unless array
		(error "Rectangular tiles other than stipples are not supported yet."))
	      (let ((into (make-array 8 :element-type '(unsigned-byte 16)))
		    (dc-image (copy-dc-image (slot-value stream 'foreground-dc-image))))
		(macrolet ((collect-byte (row-no)
			     `(let ((result 0))
				(dotimes (j 8)
				  (setq result
					(dpb (aref array (mod ,row-no width) (mod j height))
					     (byte 1 (- 7 j)) result)))
				(logxor result #2r11111111))))
		  (dotimes (i 8)
		    (setf (aref into i) (collect-byte i))))
		(let ((bitmap (win::create-bitmap into)))
		  (setf (dc-image-bitmap dc-image) bitmap)
		  (setf (dc-image-brush dc-image) (win::create-pattern-brush bitmap))
		  (setf (dc-image-background-color dc-image)
			(dc-image-text-color (slot-value stream 'background-dc-image))))
		dc-image)))))

(defmethod dc-image-for-ink (stream (ink flipping-ink))
  (declare (ignore stream))
  (or (gethash ink *ink-to-image*)
      (setf (gethash ink *ink-to-image*)
	    (multiple-value-bind (ink1 ink2)
		(decode-flipping-ink ink)
	      (let* ((image1 (dc-image-for-ink stream ink1))
		     (image2 (dc-image-for-ink stream ink2))
		     (color (logxor (dc-image-text-color image1)
				    (dc-image-text-color image2))))
		(unless (and (eql (dc-image-rop2 image1) win::r2_copypen)
			     (eql (dc-image-rop2 image2) win::r2_copypen)
			     (null (dc-image-bitmap image1))
			     (null (dc-image-bitmap image2)))
		  (nyi))
		(make-dc-image :solid-1-pen (win::create-pen win::ps_solid 1 color)
			       :brush (win::create-solid-brush color)
			       :rop2 win::r2_xorpen
			       :text-color color :background-color nil))))))

(defmethod dc-image-for-ink (stream (ink contrasting-ink))
  (dc-image-for-ink stream (make-color-for-contrasting-ink ink)))



(defstruct (cloe-font)
  index
  height
  ascent
  descent
  internal-leading
  external-leading
  average-character-width
  maximum-character-width
  weight
  italic
  underlined
  struckout
  first-character
  last-character
  default-character
  break-character
  character-set
  overhang
  pitch
  family
  font-width-table)

;;; Fill up the font width table for a variable width font
;;; by asking windows the width of each character.
;;; This doesn't quite work in the face of kerning.
(defun initialize-font-width-table (font window)
  ;; only need to run this on variable-width fonts.
  (select-font (cloe-font-index font))
  (let ((array (make-array (1+ (- 126 32)))))
    (with-temporary-string (string :length 1)
      (setf (fill-pointer string) 1)
      (loop for i from 32 to 126 do
	(setf (aref string 0) (code-char i))
	(multiple-value-bind (width height)
	    (win::get-text-extent window string)
	  (declare (ignore height ))
	  (setf (aref array (- i 32)) width))))
    (setf (cloe-font-font-width-table font) array)))

(defmethod stream-glyph-for-character ((stream cloe-window-stream) character style
				       &optional our-font)
  (let ((display-device-type (slot-value stream 'display-device-type))
	(window (slot-value stream 'window)))
    (multiple-value-bind (character-set index)
	(char-character-set-and-index character)
      (let* ((cloe-font (or our-font
			    (text-style-mapping
			      display-device-type style character-set window)))
	     (origin-x 0)
	     (origin-y (cloe-font-ascent cloe-font))
	     (average-w (cloe-font-average-character-width cloe-font))
	     (max-w (cloe-font-maximum-character-width cloe-font))
	     (fixed-width-p (= max-w average-w))
	     (escapement-x (if fixed-width-p
			       average-w
			       (aref (cloe-font-font-width-table cloe-font) index)))
	     (escapement-y 0)
	     (bb-y (+ (cloe-font-height cloe-font)
		      (cloe-font-external-leading cloe-font)))
	     (bb-x escapement-x))
	(values index cloe-font escapement-x escapement-y
		origin-x origin-y bb-x bb-y
		fixed-width-p)))))

;;; Really want to set the text color to color.  Talk to Bob.
(defmethod stream-write-string-1 ((stream cloe-window-stream)
				  glyph-buffer start end font ink x y)
  (with-slots (window) stream
    (set-dc-for-text stream ink (cloe-font-index font))
    (with-temporary-string (string :length (- end start))
      (dovector (index glyph-buffer :start start :end end)
	(vector-push (int-char index) string))
      (win::text-out window x y string))))

(defmethod stream-write-char-1 ((stream cloe-window-stream) index font ink x y)
  (with-slots (window) stream
    (set-dc-for-text stream ink (cloe-font-index font))
    (with-temporary-string (string :length 1)
      (vector-push (int-char index) string)
      (win::text-out window x y string))))

(defmethod draw-string-internal ((stream cloe-window-stream)
				 x-offset y-offset string x y
				 start end align-x align-y text-style ink)
  (unless end (setq end (length string)))
  (fix-points x y)
  (translate-positions x-offset y-offset x y)
  (with-temporary-substring (substring string start end)
    (with-slots (window display-device-type) stream
      (let* ((font (text-style-mapping 
		     display-device-type text-style *standard-character-set* window))
	     (height (cloe-font-height font))
             (descent (cloe-font-descent font))
             (ascent (cloe-font-ascent font)))
	(incf x (compute-text-x-adjustment
		  align-x stream string text-style start end))
	(incf y (compute-text-y-adjustment
		  align-y descent ascent height))
	(decf y ascent)		;text is positioned by its top left on CLOE
	(set-dc-for-text stream ink (cloe-font-index font))
	(win::text-out window x y substring)))))

(defmethod draw-character-internal ((stream cloe-window-stream) x-offset y-offset 
				    character x y align-x align-y text-style ink)
  (fix-points x y)
  (translate-positions x-offset y-offset x y)
  (with-slots (window display-device-type) stream
    (let* ((font (text-style-mapping 
		   display-device-type text-style *standard-character-set* window))
	   (height (cloe-font-height font))
	   (descent (cloe-font-descent font))
	   (ascent (cloe-font-ascent font)))
      (incf x (compute-text-x-adjustment
		align-x stream character text-style))
      (incf y (compute-text-y-adjustment
		align-y descent ascent height))
      (decf y ascent)		;text is positioned by its top left on CLOE
      (set-dc-for-text stream ink (cloe-font-index font))
      (with-temporary-string (string :length 1)
	(vector-push character string)
	(win::text-out window x y string)))))


(defmethod text-style-height ((text-style standard-text-style) (stream cloe-window-stream))
  (with-slots (window display-device-type) stream
    (let ((font (text-style-mapping 
		  display-device-type text-style *standard-character-set* window)))
      (cloe-font-height font))))

(defmethod text-style-width ((text-style standard-text-style) (stream cloe-window-stream))
  (with-slots (window display-device-type) stream
    (let ((font (text-style-mapping 
		  display-device-type text-style *standard-character-set* window)))
      (cloe-font-average-character-width font))))

(defmethod text-style-ascent ((text-style standard-text-style) (stream cloe-window-stream))
  (with-slots (window display-device-type) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      (cloe-font-ascent font))))

(defmethod text-style-descent ((text-style standard-text-style) (stream cloe-window-stream))
  (with-slots (window display-device-type) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      (cloe-font-descent font))))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (stream cloe-window-stream))
  (with-slots (window display-device-type) stream
    (let ((font (text-style-mapping
		  display-device-type text-style *standard-character-set* window)))
      ;; Really disgusting, but probably OK
      (= (cloe-font-average-character-width font)
	 (cloe-font-maximum-character-width font)))))


(defparameter *cloe-logical-size-alist*
	      '((:tiny       6)
		(:very-small 7)
		(:small	     8)
		(:normal     10)
		(:large	     12)
		(:very-large 18)))

(defclass cloe-display-device ()
    ((text-style->cloe-font-mapping :initform (make-hash-table))
     (logpixelsy :initform 72 :initarg :logpixelsy)))

(defmethod text-style-mapping
	   ((device cloe-display-device) character-set (style text-style) window)
  (declare (ignore character-set))
  (let ((hash-table (slot-value device 'text-style->cloe-font-mapping)))
    (or (gethash style hash-table)
	(setf (gethash style hash-table)
	      (multiple-value-bind (weight italic)
		  (let ((face (text-style-face style)))
		    (typecase face
		      (cons
			(values (if (member :bold face) 700 400)
				(member :italic face)))
		      (otherwise
			(case face
			  (:roman (values 400 nil))
			  (:bold (values 700 nil))
			  (:italic (values 400 t))
			  (otherwise (values 400 nil))))))
		(multiple-value-bind (family face-name)
		    (case (text-style-family style)
		      (:fix (values #x35 nil))
		      (:serif (values #x16 nil))
		      (:sans-serif (values #x26 nil))
		      (otherwise (values 0 nil)))
		  (let ((point-size
			  (let ((size (text-style-size style)))
			    (typecase size
			      (number
				size)
			      (otherwise
				(or (second (assoc size *cloe-logical-size-alist*)) 12))))))
		    (make-windows-font 
		      window 
		      (- (round (* point-size (slot-value device 'logpixelsy))
				72))
		      :weight weight :italic italic
		      :pitch-and-family family :face face-name))))))))

(defmethod text-style-mapping
	   ((device cloe-display-device) character-set (style device-font) window)
  (declare (ignore character-set))
  (unless (eql device (device-font-display-device style))
    (error "An attempt was made to map device font ~S on device ~S, ~@
	    but it is defined for device ~S"
	   style device (device-font-display-device style)))
  (let ((hash-table (slot-value device 'text-style->cloe-font-mapping)))
    (or (gethash style hash-table)
	(setf (gethash style hash-table)
	      (let ((args (device-font-name style)))
		(apply #'make-windows-font window 
		       (- (round (* (pop args) (slot-value device 'logpixelsy))
				 72))
		       args))))))

(defun make-windows-font
       (window height &key (width 0) (escapement 0) (orientation 0)
			   (weight 400) (italic nil) (underline nil) (strikeout nil)
			   (charset 0) (output-precision 0) (clip-precision 0)
			   (quality 2) (pitch-and-family 0) (face nil))
  (let ((win-font
	  (win::create-font height width escapement orientation weight
			    (if italic 1 0) (if underline 1 0)
			    (if strikeout 1 0) charset
			    output-precision clip-precision quality
			    pitch-and-family (or face ""))))
    (select-font win-font)
    (multiple-value-bind (height ascent descent
			  internal-leading external-leading
			  average-character-width maximum-character-width
			  weight italic underlined struckout
			  first-character last-character default-character
			  break-character p&f character-set overhang
			  aspect-x aspect-y)
	(win::get-text-metrics window)
      (declare (ignore p&f aspect-x aspect-y))
      (make-cloe-font
	:index win-font :height height :ascent ascent :descent descent
	:internal-leading internal-leading :external-leading external-leading
	:average-character-width average-character-width
	:maximum-character-width maximum-character-width
	:weight weight :italic italic 
	:underlined underlined :struckout struckout
	:first-character first-character :last-character last-character
	:default-character default-character :break-character break-character
	:overhang overhang
	:font-width-table (and (/= average-character-width
				   maximum-character-width)
			       (let ((array (make-array (1+ last-character))))
				 (with-temporary-string (string :length 1)
				   (setf (fill-pointer string) 1)
				   (loop for i from first-character to last-character do
				     (setf (aref string 0) (code-char i))
				     (multiple-value-bind (width height)
					 (win::get-text-extent window string)
				       (declare (ignore height))
				       (setf (aref array i) width))))
				 array))))))


;;; These should really be defined in CL-STREAM, but doing it causes CLOE to
;;; go south during the build, my guess is the regular streams get trashed.
;;; So anyway we now do it here.

(clos:defmethod stream-read-byte ((stream t)) 
  (read-byte stream nil :eof))

(clos:defmethod stream-read-char ((stream t)) 
  (read-char stream nil :eof))

(clos:defmethod stream-unread-char ((stream t) character) 
  (unread-char character stream))

(clos:defmethod stream-read-char-no-hang ((stream t)) 
  (read-char-no-hang stream nil :eof))

(clos:defmethod stream-peek-char ((stream t)) 
  (peek-char nil stream nil :eof))

(clos:defmethod stream-listen ((stream t)) 
  (listen stream))

(clos:defmethod stream-read-line ((stream t)) 
  (read-line stream nil :eof))

(clos:defmethod stream-clear-input ((stream t)) 
  (clear-input stream))

(clos:defmethod stream-write-byte ((stream t) integer) 
  (write-byte integer stream))

(clos:defmethod stream-write-char ((stream t) character) 
  (write-char character stream))

(clos:defmethod stream-write-string ((stream t) string &optional (start 0) end) 
  (write-string string stream 
		:start start 
		:end end))

(clos:defmethod stream-terpri ((stream t)) (terpri stream))


(clos:defmethod stream-fresh-line ((stream t)) 
  (fresh-line stream))

(clos:defmethod stream-force-output ((stream t)) 
  (force-output stream))

(clos:defmethod stream-finish-output ((stream t)) 
  (finish-output stream))

(clos:defmethod stream-clear-output ((stream t)) 
  (clear-output stream))
