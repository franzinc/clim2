;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: db-scroll.lisp,v 1.9 92/03/24 19:36:28 cer Exp Locker: cer $

"Copyright (c) 1991, 1992 by Franz, Inc.  All rights reserved.
 Portions copyright(c) 1991, 1992 International Lisp Associates.
 Portions copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)


;;--- Need to be able to specify horizontal and vertical separately
;;--- What do we need from CLIM 0.9's DB-NEW-SCROLL?
(defclass scroller-pane (client-space-requirement-mixin
			 wrapping-space-mixin
			 layout-pane)
	  (vertical-scrollbar
	   horizontal-scrollbar
	   contents
	   viewport))

(defmethod pane-viewport ((x sheet))
  (and (typep (sheet-parent x) 'viewport)
       (sheet-parent x)))

(defmethod pane-viewport-region ((x sheet))
  (let ((vp (pane-viewport x)))
    (and vp
	 (viewport-viewport-region vp))))

(defmethod pane-scroller ((x sheet))
  (and (pane-viewport x)
       (sheet-parent (sheet-parent x))
       (typep (sheet-parent (sheet-parent (sheet-parent x)))
	      'scroller-pane)
       (sheet-parent (sheet-parent (sheet-parent x)))))

(defmethod initialize-instance :after ((pane scroller-pane) 
				       &key contents frame-manager frame
					    (scroll-bars :both))
  (with-slots (vertical-scrollbar horizontal-scrollbar (c contents) viewport) pane
    (with-look-and-feel-realization (frame-manager frame)
      (setf vertical-scrollbar (realize-pane 'scroll-bar 
					     :orientation :vertical
					     :client pane
					     :id :vertical)
	    horizontal-scrollbar (realize-pane 'scroll-bar 
					       :id :horizontal
					       :client pane
					       :orientation :horizontal)
	    c contents
	    viewport (realize-pane 'viewport))
      (sheet-adopt-child pane
			 (tabling ()
			   (viewport vertical-scrollbar)
			   (horizontal-scrollbar nil))))
    (sheet-adopt-child viewport c)
    ;; Add callbacks
    ))

(defun update-scrollbars (vp)
  (with-bounding-rectangle* (minx miny maxx maxy)
    (let ((c (sheet-child vp)))
      (if (typep c 'clim-internals::output-recording-mixin)
	  (stream-output-history c)
	c))
    (with-bounding-rectangle* (vminx vminy vmaxx vmaxy)
	(viewport-viewport-region vp)
      (with-slots (horizontal-scrollbar vertical-scrollbar)
		  (sheet-parent (sheet-parent vp))
	(update-scrollbar vertical-scrollbar
			  miny maxy vminy vmaxy)
	(update-scrollbar horizontal-scrollbar
			  minx maxx vminx vmaxx)))))


;;;--- In the case where the viewport is bigger than the window this
;;;code gets things kind a wrong. Checkout the thinkadot puzzl. Its
;;;cos (- (--) (- vminx)) is -ve

(defun update-scrollbar (scrollbar min max vmin vmax)
  (declare (optimize (safety 0) (speed 3)))
  (with-slots (current-size current-value) scrollbar
    ;; Kinda bogus benchmark optimization -- if the scrollbar was full size
    ;; before, and the viewport is bigger than the extent, don't bother with
    ;; the fancy math.
    (if (and (eql (the (integer 0 100) current-size) 100)
	     (> (- vmax vmin) (- max min)))
	(return-from update-scrollbar))
    (let* ((size (the fixnum (truncate (* 100.0
					  (the single-float
					    (if (zerop (- max min))
						1.0
					      (min 1.0 (the single-float
							 (/ (float (- vmax vmin) 0.0s0)
							    (float (- max min) 0.0s0))))))))))
	   (pos (the single-float
		  (min 1.0
		       (the single-float
			 (max 0.0
			      (the single-float
				(if (zerop (- (- max min) (- vmax vmin)))
				    0.0
				  (the single-float 
				    (/ (float (- vmin min) 0.0s0)
				       (float (- (- max min) (- vmax vmin)) 0.0s0))))))))))
	   (value (min (the fixnum (- 100 size))
		       (the fixnum (truncate (* 100 pos))))))
      (declare (fixnum size value)
	       (type single-float pos))
      (unless (and current-size
		   current-value
		   (eq current-size size)
		   (eq current-value value))
	(setf current-size size
	      current-value value)
	(change-scrollbar-values scrollbar 
				 :slider-size size
				 :value value)))))


(defmethod scrollbar-value-changed-callback :before
	   ((sheet scrollbar)
	    (client scroller-pane) 
	    id value
	    size)
  (declare (ignore id))
  (with-slots (current-size current-value) sheet
    (setf current-size (truncate size)
	  current-value (truncate value))))
  

(defmethod scrollbar-value-changed-callback (sheet 
					      (client scroller-pane) 
					      id value
					      size)
  (declare (ignore sheet))
  (with-slots (viewport contents) client
    (let* ((extent (if (typep contents 'clim-internals::output-recording-mixin)
		       (stream-output-history contents)
		     contents))
	   (vp viewport)
	   (viewport (viewport-viewport-region vp)))
      (case id
	(:vertical
	  (scroll-extent
	    contents
	    :x (bounding-rectangle-min-x viewport)
	    :y (truncate
		 (+ (bounding-rectangle-min-y extent)
		    (* (max 0 (- (bounding-rectangle-height extent)
				 (bounding-rectangle-height viewport)))
		       (if (= size 100)
			   0
			   (/ value (- 100 size))))))))
	(:horizontal
	  (scroll-extent
	    contents
	    :x (truncate
		 (* (max 0 (- (bounding-rectangle-width extent)
			      (bounding-rectangle-width viewport)))
		    (if (= size 100)
			0
			(/ value (- 100 size)))))
	    :y (bounding-rectangle-min-y viewport)))))))

(defun update-region (stream nminx nminy nmaxx nmaxy &key no-repaint)
  ;;-- I suspect that we should pass in mins and maxs since this does
  ;;-- assume that the window origin is 0,0 and I think that this
  ;;-- causes the compass menu test to fail since there are graphics at
  ;;-- -ve coordinates.
  #-ignore
  (with-bounding-rectangle* 
      (l tt r b) stream
      (when (or (< nminx l)
		(< nminy tt)
		(> nmaxx r)
		(> nmaxy b))
	(setf (sheet-region stream)
	  (make-bounding-rectangle  (min nminx l)
				    (min nminy tt)
				    (max nmaxx r)
				    (max nmaxy b)))))
  #+ignore
  (let ((width (- nmaxx nminx))
	(height (- nmaxy nminy)))
    (when (or (> width (bounding-rectangle-width stream))
	      (> height (bounding-rectangle-height stream)))
      (setf (sheet-region stream)
	(make-bounding-rectangle  0 0 
				  (max (bounding-rectangle-width stream) width)
				  (max (bounding-rectangle-height
					stream) height))))))


(defun scroll-extent (stream &key (x 0) (y 0))
  (let ((vp (pane-viewport stream)))
    (when vp
      (with-bounding-rectangle* (left top right bottom) 
	  (pane-viewport-region stream)
	;;;---- This should actually bash the sheet-transformation
	(setf (sheet-transformation stream)
	      (make-translation-transformation (- x) (- y)))
	(bounding-rectangle-set-position* (viewport-viewport-region vp) x y)
	;;--- Is this the correct place
	(update-scrollbars vp)
	(with-bounding-rectangle* (nleft ntop nright nbottom) 
	  (pane-viewport-region stream)
	  ;;-- Do we really want to do this here??
	  (update-region stream nleft ntop nright nbottom)
	  (cond
	   ;; if some of the stuff that was previously on display is still on display
	   ;; bitblt it into the proper place and redraw the rest.
	   ((ltrb-overlaps-ltrb-p left top right bottom
				  nleft ntop nright nbottom)
	    ;; move the old stuff to the new position
	    #+ignore
	    (break "before shifting ~S"
		   (list (list left top right bottom)
			 (list nleft ntop nright nbottom)))
	    (window-shift-visible-region stream 
					 left top right bottom
					 nleft ntop nright nbottom)
	    (let ((rectangles (ltrb-difference nleft ntop nright nbottom
					       left top right
					       bottom)))
	      #+ignore
	      (break "done shifting")
	      (with-sheet-medium (medium stream)
		(dolist (region rectangles)
		  (multiple-value-call #'draw-rectangle*
		    medium
		    (bounding-rectangle* region)
		    :ink +background-ink+ :filled t)
		  #+ignore
		  (break "cleared region ~s" region)
		  (when (typep stream 'clim-internals::output-recording-mixin)
		    (replay (stream-output-history stream) stream
			    region))
		  #+ignore
		  (break "done replay")))))
	   ;; otherwise, just redraw the whole visible viewport
	   ;; adjust for the left and top margins by hand so clear-area doesn't erase
	   ;; the margin components.
	   ((typep stream 'clim-internals::output-recording-mixin)
	    (let ((region (viewport-viewport-region vp)))
	      ;;---- we should make the sheet-region bigger at this point
	      ;; perhaps we do a union of the sheet-region and the viewport
	      (with-sheet-medium (medium vp)
		(draw-rectangle* medium
				 0 0 
				 (bounding-rectangle-width region)
				 (bounding-rectangle-height region)
				 :ink +background-ink+ :filled t))
	      (replay (stream-output-history stream) stream region)))))))))



;;; home-grown scrollbars

;;--- orientation, min-value, max-value, unit-increment, page-increment.
(defclass scroll-bar-pane
	  (scrollbar
	   pane
	   ;;--- add immediate-sheet-input-mixin so that scroll bars
	   ;;--- get handled immediately by the event process?
	   wrapping-space-mixin
	   sheet-permanently-enabled-mixin
	   sheet-mute-input-mixin
	   sheet-multiple-child-mixin
	   mute-repainting-mixin
	   space-requirement-mixin)
    ((shaft-thickness :initarg :shaft-thickness)
     (min-target-pane :initform nil)
     (max-target-pane :initform nil)
     (shaft-pane :initform nil))
  (:default-initargs :value 0
		     :shaft-thickness 10))

;;--- there should really be a small border around scroll bars so that
;;--- they don't butt right up against the viewport's drawing area
(defmethod initialize-instance :after ((pane scroll-bar-pane)
				       &key orientation shaft-thickness
					    frame-manager frame)
  (with-slots (min-target-pane max-target-pane shaft-pane) pane
    (with-look-and-feel-realization (frame-manager frame)
      (let ((inferiors
	      (ecase orientation
		(:vertical
		  (spacing (:thickness 1)
		    (vertically ()
		      (setq min-target-pane
			    (realize-pane 'scroll-bar-target-pane
					  :scroll-bar pane
					  :end :less-than
					  :width shaft-thickness
					  :height shaft-thickness))
		      (setq shaft-pane 
			    (realize-pane 'scroll-bar-shaft-pane 
					  :scroll-bar pane
					  :width shaft-thickness
					  :height 0
					  :max-height +fill+))
		      (setq max-target-pane
			    (realize-pane 'scroll-bar-target-pane
					  :scroll-bar pane
					  :end :greater-than
					  :width shaft-thickness
					  :height shaft-thickness)))))
		(:horizontal
		  (spacing (:thickness 1)
		    (horizontally ()
		      (setq min-target-pane
			    (realize-pane 'scroll-bar-target-pane
					  :scroll-bar pane
					  :end :less-than
					  :width shaft-thickness
					  :height shaft-thickness))
		      (setq shaft-pane 
			    (realize-pane 'scroll-bar-shaft-pane
					  :scroll-bar pane
					  :width 0
					  :max-width +fill+
					  :height shaft-thickness))
		      (setq max-target-pane
			    (realize-pane 'scroll-bar-target-pane
					  :scroll-bar pane
					  :end :greater-than
					  :width shaft-thickness
					  :height shaft-thickness))))))))
	(sheet-adopt-child pane inferiors)))))

(defmethod change-scrollbar-values ((scrollbar scroll-bar-pane) &rest args 
					    &key slider-size value)
  (declare (ignore slider-size value))
  )


(defclass scroll-bar-target-pane 
	  (leaf-pane
	   sheet-permanently-enabled-mixin
	   mute-repainting-mixin
	   space-requirement-mixin)
    ((end :initarg :end)
     (scroll-bar :initarg :scroll-bar)
     (coord-cache :initform nil)))

(defmethod initialize-instance :after ((pane scroll-bar-target-pane) &key end)
  (let* ((scroll-bar (slot-value pane 'scroll-bar))
	 (orientation (gadget-orientation scroll-bar)))
    (setf (sheet-cursor pane)
	  (ecase end
	    (:less-than 
	      (ecase orientation
		(:horizontal :scroll-left)
		(:vertical :scroll-up)))
	    (:greater-than
	      (ecase orientation
		(:horizontal :scroll-right)
		(:vertical :scroll-down)))))))

(defmethod sheet-region-changed :after ((pane scroll-bar-target-pane) &key)
  (setf (slot-value pane 'coord-cache) nil))

#+if-we-had-cheap-xforms...
(defmethod sheet-region-changed :after ((pane scroll-bar-target-pane) &key)
  (let ((cursor (sheet-cursor pane)))
    (ecase cursor
      ((:scroll-up :scroll-down) nil)
      (:scroll-left
	(let ((xform +identity-transformation+))
	  (setq xform (compose-with-rotation xform pi/2 :reuse xform))
	  (setq xform (compose-with-translation xform (bounding-rectangle-width pane) 0 
						:reuse xform))
	  (setf (sheet-transformation pane) xform)))
      (:scroll-right
	(let ((xform +identity-transformation+))
	  (setq xform (compose-with-rotation xform (- pi/2) :reuse xform))
	  (setq xform (compose-with-translation xform 0 (bounding-rectangle-height pane)
						:reuse xform))
	  (setf (sheet-transformation pane) xform))))))

(defmethod repaint-sheet ((pane scroll-bar-target-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (multiple-value-call #'draw-rectangle*
      medium (bounding-rectangle* (sheet-region pane))
      :filled nil)
    (draw-target pane medium)))

;;; you can pass :filled t to this in order to highlight the target when clicked on...
(defmethod draw-target ((pane scroll-bar-target-pane) medium
			&key filled (ink +foreground-ink+))
  (let ((coord-cache (slot-value pane 'coord-cache)))
    (unless coord-cache
      (let ((identity (sheet-cursor pane)))
	(setq coord-cache
	      (setf (slot-value pane 'coord-cache)
		    (with-bounding-rectangle* (minx miny maxx maxy) (sheet-region pane)
		      (ecase identity
			(:scroll-up
			  (list minx miny
				(+ minx (/ (- maxx minx) 2)) maxy
				maxx miny))
			(:scroll-down
			  (list minx maxy
				(+ minx (/ (- maxx minx) 2)) miny
				maxx maxy))
			(:scroll-left
			  (list maxx miny
				minx (/ (+ miny maxy) 2)
				maxx maxy))
			(:scroll-right
			  (list minx miny
				maxx (/ (+ miny maxy) 2)
				minx maxy))))))))
    (draw-polygon* medium coord-cache :filled filled :ink ink)))

(defclass scroll-bar-shaft-pane
	  (leaf-pane
	   sheet-permanently-enabled-mixin
	   mute-repainting-mixin
	   space-requirement-mixin)
    ((scroll-bar :initarg :scroll-bar)
     (needs-erase :initform nil)))

(defmethod initialize-instance :after ((pane scroll-bar-shaft-pane) &key)
  (let* ((scroll-bar (slot-value pane 'scroll-bar))
	 (orientation (gadget-orientation scroll-bar)))
    (setf (sheet-cursor pane)
	  (ecase orientation
	    (:horizontal ':horizontal-scroll)
	    (:vertical ':vertical-scroll)))))

(defmethod repaint-sheet ((pane scroll-bar-shaft-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (multiple-value-call #'draw-rectangle*
      medium (bounding-rectangle* (sheet-region pane))
      :filled nil)
    (draw-thumb pane medium)))

(defvar +33%-gray+ (make-gray-color 1/3))

(defmethod draw-thumb ((pane scroll-bar-shaft-pane) medium
		       &key (ink +foreground-ink+))
  (let ((needs-erase (slot-value pane 'needs-erase)))
    (setf (slot-value pane 'needs-erase) (not (eq ink +background-ink+)))
    (when (and (not needs-erase)
	       (eq ink +background-ink+))
      (return-from draw-thumb (values))))
  (let* ((scroll-bar (slot-value pane 'scroll-bar))
	 (current-value (gadget-value scroll-bar))
	 (max-value #---ignore 100 #+++ignore (scroll-bar-max-value scroll-bar))
	 (min-value #---ignore   0 #+++ignore (scroll-bar-min-value scroll-bar))
	 (identity (sheet-cursor pane)))
    (setf (slot-value pane 'needs-erase) (not (eq ink +background-ink+)))
    (flet ((draw-car (medium left top right bottom)
	     (draw-rectangle* medium left top right bottom
			      :filled t
			      :ink (if (eq ink +foreground-ink+) +33%-gray+ ink))
	     (draw-rectangle* medium left top right bottom
			      :filled nil
			      :ink ink)))
      (declare (dynamic-extent #'draw-car))
      (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
	(let ((height (- y2 y1))
	      (width (- x2 x1 2)))
	  (let* ((elevator-top (compute-symmetric-value 
				 min-value max-value current-value 0
				 (ecase identity
				   (:vertical-scroll height)
				   (:horizontal-scroll width))))
		 (elevator-bottom (+ elevator-top 20)))
	    (ecase identity
	      (:vertical-scroll
		(draw-car medium
			  (1+ x1)           (- y2 elevator-top)
			  (+ x1 width 1) (- y2 elevator-bottom)))
	      (:horizontal-scroll
		(draw-car medium
			  elevator-top      (+ y1 2)
			  elevator-bottom   (+ y1 height 1))))))))))

(defun compute-symmetric-value (min1 max1 value1 min2 max2)
  (declare (values value2))
  (let* ((distance1 (- max1 min1))
	 (fraction1 (if (zerop distance1) 0 (/ (- value1 min1) distance1))))
    (+ min2 (* (- max2 min2) fraction1))))

(defmethod handle-event :around ((pane scroll-bar-target-pane) 
				 (event pointer-button-press-event))
  (with-sheet-medium (medium pane)
    (draw-target pane medium :filled T :ink +foreground-ink+)
    (call-next-method)
    (draw-target pane medium :filled T :ink +background-ink+)
    (draw-target pane medium :filled nil)))

;;; The interface to our scroll bars is that if you click left in the targets
;;; you scroll up/down by a page.  If you click right you scroll up/down by a line.
;;; If you click middle, you scroll to top/bottom.
;;; This needs more complexity in that as long as the button is held down
;;; we want to smoothly continue to scroll by lines or pages.  Later.
(defmethod handle-event ((pane scroll-bar-target-pane) 
			 (event pointer-button-press-event))
  (let* ((scroll-bar (slot-value pane 'scroll-bar))
	 (client (gadget-client scroll-bar))
	 (id (gadget-id scroll-bar)))
    (case (pointer-event-button event)
      (#.+pointer-left-button+
       (ecase (slot-value pane 'end)
	 (:greater-than
	   ;; --- how to figure out how much to scroll...
	   (scroll-down-page-callback scroll-bar client id))
	 (:less-than
	   ;; --- how to figure out how much to scroll...
	   (scroll-up-page-callback scroll-bar client id))))
      (#.+pointer-middle-button+
       (ecase (slot-value pane 'end)
	 (:greater-than
	   (scroll-to-bottom-callback scroll-bar client id))
	 (:less-than
	   (scroll-to-top-callback scroll-bar client id))))
      (#.+pointer-right-button+
       (ecase (slot-value pane 'end)
	 (:greater-than
	   ;; --- how to figure out how much to scroll...
	   (scroll-down-line-callback scroll-bar client id))
	 (:less-than
	   ;; --- how to figure out how much to scroll...
	   (scroll-up-line-callback scroll-bar client id)))))))

(defmethod handle-event ((pane scroll-bar-shaft-pane) 
			 (event pointer-button-press-event))
  (case (pointer-event-button event)
    (#.+pointer-left-button+
     (let ((x (pointer-event-x event))
	   (y (pointer-event-x event))
	   (scroll-bar (slot-value pane 'scroll-bar)))
       (with-bounding-rectangle* (minx miny maxx maxy) (sheet-region pane)
	 (ecase (gadget-orientation scroll-bar)
	   (:vertical
	     ;; this (- maxy y) is here because we want y=0 to be at the bottom.
	     (update-scroll-bar-value pane scroll-bar (- maxy y) miny maxy))
	   (:horizontal
	     (update-scroll-bar-value pane scroll-bar x minx maxx))))))))

(defmethod handle-event ((pane scroll-bar-shaft-pane) (event pointer-enter-event))
  ;;--- Change the mouse cursor
  )

(defmethod handle-event ((pane scroll-bar-shaft-pane) (event pointer-exit-event))
  ;;--- Change the mouse cursor
  )

(defmethod update-scroll-bar-value ((pane scroll-bar-shaft-pane) scroll-bar coord min max)
  (let* ((max-value #---ignore 100 #+++ignore (slot-value scroll-bar 'max-value))
	 (min-value #---ignore   0 #+++ignore (slot-value scroll-bar 'min-value))
	 (value (compute-symmetric-value min max coord min-value max-value)))
    (setf (gadget-value scroll-bar) value)))

;;;-- This looks like a value gadget to me

(defmethod (setf gadget-value) (nv (pane scroll-bar-pane) &key)
  (with-slots (value) pane
    (setf value nv)))

(defmethod (setf gadget-value) :around (nv (pane scroll-bar-pane) &key)
  (declare (ignore nv))
  (if (port pane)
      (let ((shaft (slot-value pane 'shaft-pane)))
	(with-sheet-medium (medium pane)
	  (draw-thumb shaft medium :ink +background-ink+)
	  (call-next-method)
	  (draw-thumb shaft medium :ink +foreground-ink+)))
      (call-next-method)))
