;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: db-scroll.lisp,v 1.29 92/08/19 18:04:15 cer Exp Locker: cer $

"Copyright (c) 1991, 1992 by Franz, Inc.  All rights reserved.
 Portions copyright(c) 1991, 1992 International Lisp Associates.
 Portions copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)


;; The abstract scroller pane class
;;--- Need to be able to specify horizontal and vertical separately
(defclass scroller-pane ()
    ((scroll-bars :initarg :scroll-bars
		  :reader scroller-pane-scroll-bar-policy)
     viewport
     (contents :initarg :contents :accessor pane-contents)
     (vertical-scroll-bar :initform nil 
			  :accessor scroller-pane-vertical-scroll-bar)
     (horizontal-scroll-bar :initform nil
			    :accessor scroller-pane-horizontal-scroll-bar)
     (scrolling-supplied-by-gadget :initform nil
				   :accessor scroller-pane-gadget-supplies-scrolling-p))
  (:default-initargs :scroll-bars :both))

;; An implementation of a scroller pane
(defclass generic-scroller-pane (scroller-pane 
				 client-space-requirement-mixin
				 wrapping-space-mixin
				 layout-pane)
    ())

(defmethod handle-event :after ((pane generic-scroller-pane) (event pointer-motion-event))
  (deallocate-event event))

(defmethod pane-viewport ((sheet sheet))
  (let ((parent (sheet-parent sheet)))
    (and (viewportp parent) parent)))

(defmethod pane-viewport-region ((sheet sheet))
  (let ((vp (pane-viewport sheet)))
    (and vp (viewport-viewport-region vp))))

(defmethod pane-scroller ((sheet sheet))
  (let ((viewport (pane-viewport sheet)))
    (and viewport (viewport-scroller-pane viewport))))

(defmethod gadget-supplied-scrolling (frame-manager frame contents &rest ignore)
  (declare (ignore frame-manager frame ignore contents))
  nil)

(defmethod allocate-space :before ((scroller generic-scroller-pane) width height)
  (declare (ignore width height))
  ;; Adjust the status of the scrollbars
  (multiple-value-bind (changedp 
			hscroll-bar hscroll-bar-enabled-p
			vscroll-bar vscroll-bar-enabled-p)
      (compute-dynamic-scroll-bar-values scroller)
    (when changedp
      (update-dynamic-scroll-bars scroller changedp
				  hscroll-bar hscroll-bar-enabled-p
				  vscroll-bar vscroll-bar-enabled-p))))

(defmethod compose-space ((scroller generic-scroller-pane) &key width height)
  (declare (ignore width height))
  (let ((sr (call-next-method)))
    (multiple-value-bind (width min-width max-width
			  height min-height max-height)
	(space-requirement-components sr)
      (if (or (< width 50) (< height 50))
	  ;; Make sure the scroller pane is big enough to hold something
	  (make-space-requirement
	    :width (max width 50)
	    :min-width (max min-width 50)
	    :max-width (max max-width 50)
	    :height (max height 50)
	    :min-height (max min-height 50)
	    :max-height (max max-height 50))
	  sr))))

;;--- Ideally we should use a toolkit scrolling window. This will look
;;--- exactly right and will deal with user specified placement of scroll-bars.
;;--- However the geometry management problems are quite huge.
(defmethod initialize-instance :after ((pane generic-scroller-pane) 
				       &key contents frame-manager frame
					    scroll-bars)
  (let ((scroller (gadget-supplied-scrolling frame-manager frame contents
					     :scroll-bars scroll-bars)))
    (if scroller
	(progn
	  (sheet-adopt-child pane scroller)
	  (setf (slot-value pane 'scrolling-supplied-by-gadget) t)
	  scroller)
	(progn
	  (check-type scroll-bars
		      (member :both :dynamic :vertical :horizontal))
	  (with-slots (vertical-scroll-bar horizontal-scroll-bar (c contents) viewport) pane
	    (with-look-and-feel-realization (frame-manager frame)
	      (let ((verticalp
		      (member scroll-bars '(:both :dynamic :vertical)))
		    (horizontalp
		      (member scroll-bars '(:both :dynamic :horizontal))))
		(setf vertical-scroll-bar 
		      (and verticalp
			   (make-pane 'scroll-bar 
			     :orientation :vertical
			     :id :vertical
			     :client pane))
		      horizontal-scroll-bar 	    
		      (and horizontalp
			   (make-pane 'scroll-bar 
			     :orientation :horizontal
			     :id :horizontal
			     :client pane))
		      c contents
		      viewport (make-pane 'viewport :scroller-pane pane))
		(sheet-adopt-child
		  pane
		  (cond ((and horizontalp verticalp)
			 (tabling ()
			   (viewport vertical-scroll-bar)
			   (horizontal-scroll-bar nil)))
			(verticalp
			 (horizontally ()
			   viewport
			   vertical-scroll-bar))
			(horizontalp
			 (vertically ()
			   viewport
			   horizontal-scroll-bar))
			(t (error "Internal error laying out scroll bars"))))
		(sheet-adopt-child viewport c)
		;;--- Add callbacks
		)))))))

(defun update-scroll-bars (viewport)
  ;;--- This is not the most efficient thing in the world
  (let ((scroller (viewport-scroller-pane viewport)))
    (multiple-value-bind (changedp
			  hscroll-bar hscroll-bar-enabled-p
			  vscroll-bar vscroll-bar-enabled-p)
	(compute-dynamic-scroll-bar-values scroller)
      (update-dynamic-scroll-bars
	scroller changedp
	hscroll-bar hscroll-bar-enabled-p 
	vscroll-bar vscroll-bar-enabled-p t))
    (with-bounding-rectangle* (left top right bottom) 
	(viewport-contents-extent viewport)
      (with-bounding-rectangle* (vleft vtop vright vbottom)
	  (viewport-viewport-region viewport)
	(let* ((vertical-scroll-bar (scroller-pane-vertical-scroll-bar scroller))
	       (horizontal-scroll-bar (scroller-pane-horizontal-scroll-bar scroller)))
	  (when vertical-scroll-bar
	    (update-scroll-bar vertical-scroll-bar
			       top bottom vtop vbottom))
	  (when horizontal-scroll-bar
	    (update-scroll-bar horizontal-scroll-bar
			       left right vleft vright)))))))

;;--- In the case where the viewport is bigger than the window this
;;--- code gets things wrong. Check out the thinkadot demo.  It's
;;--- because (- (--) (- vmin)) is negative.

(defmethod note-sheet-grafted :before ((sheet scroll-bar))
  (setf (scroll-bar-current-size sheet) nil))

(defun update-scroll-bar (scroll-bar min max vmin vmax)
  (declare (optimize (safety 0) (speed 3)))
  (let ((current-size (scroll-bar-current-size scroll-bar))
	(current-value (scroll-bar-current-value scroll-bar)))
    ;; Kinda bogus benchmark optimization -- if the scroll-bar was full size
    ;; before, and the viewport is bigger than the extent, don't bother with
    ;; the fancy math.
    (let* ((gmin (float (gadget-min-value scroll-bar) 0s0))
	   (gmax (float (gadget-max-value scroll-bar) 0s0))
	   (range (- gmax gmin)))
      (declare (type single-float range gmin gmax))
      (when (and (and current-size (= (the single-float current-size) range))
		 (> (- vmax vmin) (- max min)))
	(return-from update-scroll-bar))
      ;; The elevator size in 01 units - calculated from the contents
      (let* ((contents-range (float (- max min) 0.0s0))
	     (viewport-range (float (- vmax vmin) 0.0s0))
	     (size (the single-float
		     (* range
			(the single-float
			  (if (= max min)
			      1.0
			      (min 1.0 (the single-float
					 (/ viewport-range contents-range))))))))
	     (pos (the single-float
		    (min 1.0s0 (max 0.0s0
				    (if (<= contents-range viewport-range)
					0.0
					(/ (float (- vmin min) 0.0s0) 
					   ;;--- Uh-oh, the home-grown scroll bars
					   ;;--- seem to have a different contract
					   ;;--- from Motif/OpenLook.  Fix them!
					   #+Allegro (- contents-range viewport-range)
					   #-Allegro contents-range)))))))
	(declare (type single-float pos size))
	(unless (and current-size
		     current-value
		     (= current-size size)
		     (= current-value pos))
	  (setf (scroll-bar-current-value scroll-bar) pos)
	  (setf (scroll-bar-current-size scroll-bar) size)
	  (change-scroll-bar-values scroll-bar 
				    :slider-size size
				    :value pos))))))

(defmethod scroll-bar-value-changed-callback
	   (sheet (client scroller-pane) id value size)
  (with-slots (viewport contents) client
    (let* ((extent (viewport-contents-extent viewport))
	   (region (viewport-viewport-region viewport)))
      (case id
	(:vertical
	  (scroll-extent
	    contents
	    :x (bounding-rectangle-min-x region)
	    :y (+ (bounding-rectangle-min-y extent)
		  (* (max 0 (- (bounding-rectangle-height extent)
			       (bounding-rectangle-height region)))
		     (if (= size (gadget-range sheet))
			 0
			 (/ (- value (gadget-min-value sheet))
			    (- (gadget-range sheet) size)))))))
	(:horizontal
	  (scroll-extent
	    contents
	    :x (* (max 0 (- (bounding-rectangle-width extent)
			    (bounding-rectangle-width region)))
		  (if (= size (gadget-range sheet))
		      0
		      (/ (- value (gadget-min-value sheet))
			 (- (gadget-range sheet) size))))
	    :y (bounding-rectangle-min-y region)))))))

(defun update-region (sheet nleft ntop nright nbottom &key no-repaint)
  ;;--- I suspect that we should pass in mins and maxs since this does
  ;;--- assume that the window origin is 0,0 and I think that this
  ;;--- causes the compass menu test to fail since there are graphics at
  ;;--- negative coordinates.
  #---ignore
  (with-bounding-rectangle* (left top right bottom) sheet
    (when (or (< nleft left)
	      (< ntop  top)
	      (> nright  right)
	      (> nbottom bottom))
      ;; It should be safe to modify the sheet's region
      (let ((region (sheet-region sheet)))
	(setf (slot-value region 'left) (min nleft left)
	      (slot-value region 'top)  (min ntop  top)
	      (slot-value region 'right)  (max nright  right)
	      (slot-value region 'bottom) (max nbottom bottom))
	(note-sheet-region-changed sheet))))
  #+++ignore
  (let ((width  (- nright  nleft))
	(height (- nbottom ntop)))
    (when (or (> width  (bounding-rectangle-width  sheet))
	      (> height (bounding-rectangle-height sheet)))
      ;; It should be safe to modify the sheet's region
      (let ((region (sheet-region sheet)))
	(setf (slot-value region 'left) 0
	      (slot-value region 'top)  0
	      (slot-value region 'right)  (max (bounding-rectangle-width  sheet) width)
	      (slot-value region 'bottom) (max (bounding-rectangle-height sheet) height))))))

(defun scroll-extent (sheet &key (x 0) (y 0))
  ;;--- CER says that this really isn't right...
  (fix-coordinates x y)
  (let ((viewport (pane-viewport sheet)))
    (when viewport
      (with-bounding-rectangle* (left top right bottom) 
	  (pane-viewport-region sheet)
	;; Optimize this case, since the rest of this code can be
	;; quite expensive, especially on servers that require COPY-AREA
	;; to be synchronous
 	(unless (and (= x left) (= y top))
	  ;;--- This should actually bash the sheet-transformation
	  (setf (sheet-transformation sheet)
		(make-translation-transformation (- x) (- y)))
	  (bounding-rectangle-set-position (viewport-viewport-region viewport) x y)
	  ;; Must go after bounding-rectangle-set-position
	  (update-scroll-bars viewport)
	  (with-bounding-rectangle* (nleft ntop nright nbottom) 
	      (pane-viewport-region sheet)
	    ;;--- Do we really want to do this here??
	    (update-region sheet nleft ntop nright nbottom)
	    (cond
	      ;; If some of the stuff that was previously on display is still on
	      ;; display, BITBLT it into the proper place and redraw the rest.
	      ((ltrb-overlaps-ltrb-p left top right bottom
				     nleft ntop nright nbottom)
	       ;; Move the old stuff to the new position
	       (window-shift-visible-region sheet 
					    left top right bottom
					    nleft ntop nright nbottom)
	       (let ((rectangles (ltrb-difference nleft ntop nright nbottom
						  left top right bottom)))
		 (with-sheet-medium (medium sheet)
		   (dolist (region rectangles)
		     (with-medium-clipping-region (medium region)
		       (multiple-value-call #'medium-clear-area
					    medium (bounding-rectangle* region))
		       (when (output-recording-stream-p sheet)
			 (replay (stream-output-history sheet) sheet region)))))))
	      ;; Otherwise, just redraw the whole visible viewport
	      ;; adjust for the left and top margins by hand so clear-area doesn't erase
	      ;; the margin components.
	      ((output-recording-stream-p sheet)
	       (let ((region (viewport-viewport-region viewport)))
		 ;;--- We should make the sheet-region bigger at this point.
		 ;;--- Perhaps we do a union of the sheet-region and the viewport.
		 (with-sheet-medium (medium sheet)
		   (multiple-value-call #'medium-clear-area
					medium (bounding-rectangle* region)))
		 (replay (stream-output-history sheet) sheet region)))))
	  (when (and (/= left x) (/= top y))
	    (note-viewport-position-changed (pane-frame sheet) sheet)))))))

(defmethod note-viewport-position-changed (frame pane)
  nil)


;;; Home-grown scroll bars

(defparameter *scroll-shaft-thickness-ink* 12)

;;--- unit-increment, page-increment.
(defclass scroll-bar-pane
	  (scroll-bar
	   ;;--- Add IMMEDIATE-SHEET-INPUT-MIXIN so that scroll bars
	   ;;--- get handled immediately by the event process?
	   wrapping-space-mixin
	   sheet-permanently-enabled-mixin
	   sheet-mute-input-mixin
	   sheet-multiple-child-mixin
	   space-requirement-mixin
	   pane)
    ((shaft-thickness :initarg :shaft-thickness)
     (min-target-pane :initform nil)
     (max-target-pane :initform nil)
     (shaft-pane :initform nil))
  (:default-initargs :value 0
		     :shaft-thickness *scroll-shaft-thickness-ink*))

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
			    (make-pane 'scroll-bar-target-pane
			      :scroll-bar pane
			      :end :less-than
			      :width shaft-thickness
			      :height shaft-thickness))
		      (setq shaft-pane 
			    (make-pane 'scroll-bar-shaft-pane 
			      :scroll-bar pane
			      :width shaft-thickness
			      :height 0
			      :max-height +fill+))
		      (setq max-target-pane
			    (make-pane 'scroll-bar-target-pane
			      :scroll-bar pane
			      :end :greater-than
			      :width shaft-thickness
			      :height shaft-thickness)))))
		(:horizontal
		  (spacing (:thickness 1)
		    (horizontally ()
		      (setq min-target-pane
			    (make-pane 'scroll-bar-target-pane
			      :scroll-bar pane
			      :end :less-than
			      :width shaft-thickness
			      :height shaft-thickness))
		      (setq shaft-pane 
			    (make-pane 'scroll-bar-shaft-pane
			      :scroll-bar pane
			      :width 0
			      :max-width +fill+
			      :height shaft-thickness))
		      (setq max-target-pane
			    (make-pane 'scroll-bar-target-pane
			      :scroll-bar pane
			      :end :greater-than
			      :width shaft-thickness
			      :height shaft-thickness))))))))
	(sheet-adopt-child pane inferiors)))))

(defmethod handle-event :after ((pane scroll-bar-pane) (event pointer-event))
  (deallocate-event event))


(defmethod contents-range ((scroller scroller-pane) orientation)
  (with-slots (viewport) scroller
    (with-bounding-rectangle* (left top right bottom) 
	(viewport-contents-extent viewport)
      (ecase orientation
	(:horizontal (- right left))
	(:vertical (- bottom top))))))

(defmethod viewport-range ((scroller scroller-pane) orientation)
  (with-slots (viewport) scroller
    (with-bounding-rectangle* (left top right bottom) 
	(viewport-viewport-region viewport)
      (ecase orientation
	(:horizontal (- right left))
	(:vertical (- bottom top))))))

(defmethod line-scroll-amount ((pane scroller-pane) orientation direction)
  (declare (ignore direction))
  (with-slots (contents) pane
    (let ((style (medium-text-style contents)))
      ;; --- These are the stubs for user specified line scroll amounts
      ;; req by graphic panes. I haven't enabled them because I want to
      ;; check with SWM for the MAKE-PANE protocol for these. Davo 6/30/92
      (ecase orientation
	(:horizontal (or #+++ignore horizontal-line-scroll-amount
			 (text-style-width style contents)))
	(:vertical (or #+++ignore vertical-line-scroll-amount
		       ;; --- This should check the top or bottom line
		       ;; of the viewport for its line height to
		       ;; determine the scroll amount - this is the
		       ;; reason for the direction arg. Davo 6/30/92
		       (+ (text-style-height style contents)
			  (if (extended-output-stream-p contents)
			      (stream-vertical-spacing contents) 
			      0))))))))

(defmethod scroll-up-line-callback ((scroll-bar scroll-bar-pane) scroller-pane orientation)
  (with-slots (current-size current-value port) scroll-bar
    (with-slots (viewport contents) scroller-pane
      (let* ((contents-range (contents-range scroller-pane orientation))
	     (line-value (if (= contents-range 0)
			     0
			     (the single-float
			       (/ (line-scroll-amount scroller-pane orientation :up)
				  (float contents-range  0.0s0)))))
	     (new-value (max 0.0 (- current-value line-value))))	
	(scroll-bar-value-changed-callback
	  scroll-bar scroller-pane orientation new-value current-size)))))

(defmethod scroll-down-line-callback ((scroll-bar scroll-bar-pane) scroller-pane orientation)
  (with-slots (current-size current-value port) scroll-bar
    (with-slots (viewport contents) scroller-pane
      (let* ((contents-range (contents-range scroller-pane orientation))
	     (line-value (if (= contents-range 0)
			     0
			     (the single-float
			       (/ (line-scroll-amount scroller-pane orientation :down)
				  (float contents-range  0.0s0)))))
	     (new-value (min (- 1.0 current-size) (+ current-value line-value))))
	(scroll-bar-value-changed-callback
	  scroll-bar scroller-pane orientation new-value current-size)))))

(defmethod scroll-up-page-callback ((scroll-bar scroll-bar-pane) scroller-pane orientation)
  (with-slots (current-size current-value) scroll-bar
    (with-slots (viewport contents) scroller-pane
      (let* ((contents-range (contents-range scroller-pane orientation))
	     (viewport-range (bounding-rectangle-max-y viewport))
	     new-value)
	(if (zerop contents-range)
	    (setq new-value current-value)
	    (let ((page-value (the single-float 
				(/ viewport-range (float contents-range 0.0s0)))))
	      (setq new-value (max 0.0 (- current-value page-value)))))
	(scroll-bar-value-changed-callback
	  scroll-bar scroller-pane orientation new-value current-size)))))

(defmethod scroll-down-page-callback ((scroll-bar scroll-bar-pane) scroller-pane orientation)
  (with-slots (current-size current-value) scroll-bar
    (with-slots (viewport contents) scroller-pane
      (let* ((contents-range (contents-range scroller-pane orientation))
	     (viewport-range (bounding-rectangle-max-y viewport))
	     new-value)
      (if (zerop contents-range)
	  (setq new-value current-value)
	  (let ((page-value (the single-float 
			      (/ viewport-range (float contents-range 0.0s0)))))
	    (setq new-value (min (- 1.0 current-size) (+ current-value page-value)))))
      (scroll-bar-value-changed-callback
	scroll-bar scroller-pane orientation new-value current-size)))))

(defmethod scroll-to-top-callback ((scroll-bar scroll-bar-pane) client id)
  (with-slots (current-size current-value) scroll-bar
    (scroll-bar-value-changed-callback scroll-bar client id 0 current-size)))

(defmethod scroll-to-bottom-callback ((scroll-bar scroll-bar-pane) client id)
  (with-slots (current-size current-value) scroll-bar
    (scroll-bar-value-changed-callback
      scroll-bar client id (- 1.0 current-size) current-size)))

(defmethod scroll-line-to-top-callback ((scroll-bar scroll-bar-pane)
					scroller-pane id orientation x y)
  (with-slots (current-size current-value) scroll-bar
    (with-slots (viewport contents) scroller-pane
      ;; --- scroll-bar may not be the right thing if it's not the same
      ;; size as the contents pane - Davo 6/30/92.
      (with-bounding-rectangle* (left top right bottom)
	  (sheet-region scroll-bar)
	(with-bounding-rectangle* (vleft vtop vright vbottom)
	    (viewport-viewport-region viewport) 
	  (declare (ignore vright vbottom))
	  (with-bounding-rectangle* (cleft ctop cright cbottom)
	      (viewport-contents-extent viewport) 
	    (declare (ignore cright cbottom))
	    (let ((contents-range (contents-range scroller-pane orientation)))
	      (unless (zerop contents-range)
		(let* ((viewport-range (viewport-range scroller-pane orientation))
		       (contents-min
			 (ecase orientation (:horizontal cleft) (:vertical ctop)))
		       (viewport-min
			 (ecase orientation (:horizontal vleft) (:vertical vtop)))
		       (mouse-offset
			 (ecase orientation
			   (:vertical (/ (- y top) (float (- bottom top) 0.0s0)))
			   (:horizontal (/ (- x left) (float (- right left) 0.0s0)))))
		       (pos (/ (- (+ viewport-min (* viewport-range mouse-offset))
				  contents-min)
			       contents-range)))
		  (scroll-bar-value-changed-callback
		    scroll-bar scroller-pane id
		    (min (- 1.0 current-size) pos) current-size))))))))))

(defmethod scroll-line-to-bottom-callback ((scroll-bar scroll-bar-pane)
					   scroller-pane id orientation x y)
  (with-slots (current-size current-value) scroll-bar
    (with-slots (viewport contents) scroller-pane
      ;; --- scroll-bar may not be the right thing if its not the same
      ;; size as the contents pane - Davo 6/30/92.
      (with-bounding-rectangle* (left top right bottom)
	  (sheet-region scroll-bar)
	(with-bounding-rectangle* (vleft vtop vright vbottom)
	    (viewport-viewport-region viewport) 
	  (declare (ignore vright vbottom))
	  (with-bounding-rectangle* (cleft ctop cright cbottom)
	      (viewport-contents-extent viewport) 
	    (declare (ignore cright cbottom))
	    (let ((contents-range (contents-range scroller-pane orientation)))
	      (unless (zerop contents-range)
		(let* ((viewport-range (viewport-range scroller-pane orientation))
		       (contents-min
			 (ecase orientation (:horizontal cleft) (:vertical ctop)))
		       (viewport-min
			 (ecase orientation (:horizontal vleft) (:vertical vtop)))
		       (mouse-offset
			 (ecase orientation
			   (:vertical (/ (- y top) (float (- bottom top) 0.0s0)))
			   (:horizontal (/ (- x left) (float (- right left) 0.0s0)))))
		       (pos (/ (- (+ viewport-min (* viewport-range mouse-offset))
				  viewport-range contents-min)
			       contents-range)))
		  (scroll-bar-value-changed-callback
		    scroll-bar scroller-pane id (max 0 pos) current-size))))))))))

(defmethod scroll-elevator-callback ((scroll-bar scroll-bar-pane)
				     scroller-pane id orientation x y)
  (with-slots (current-size current-value) scroll-bar
    (with-slots (viewport contents) scroller-pane
      ;; --- scroll-bar may not be the right thing if its not the same
      ;; size as the contents pane - Davo 6/30/92.
      (with-bounding-rectangle* (left top right bottom)
	  (sheet-region scroll-bar)
	(let* ((mouse-offset
		 (- (ecase orientation
		      (:vertical (/ (- y top) (float (- bottom top) 0.0s0)))
		      (:horizontal (/ (- x left) (float (- right left) 0.0s0))))
		    (/ current-size 2.0s0))))
	  (scroll-bar-value-changed-callback
	    scroll-bar scroller-pane id (min 1.0s0 (max 0.0s0 mouse-offset)) current-size))))))

;;; Set the indicator to the proper size and location (size and value are between 0 and 1)
(defmethod change-scroll-bar-values ((scroll-bar scroll-bar-pane)
				     &key slider-size value)
  (setf (gadget-value scroll-bar) value)
  (setf (scroll-bar-current-size scroll-bar) slider-size))


(defclass scroll-bar-target-pane 
	  (space-requirement-mixin
	   sheet-single-child-mixin
	   leaf-pane)
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

(defmethod note-sheet-region-changed :after ((pane scroll-bar-target-pane)
					     &key &allow-other-keys)
  (setf (slot-value pane 'coord-cache) nil))

#+If-we-had-cheap-xforms...
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
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (draw-rectangle* medium left top (1- right) (1- bottom)
		       :filled nil))
    (draw-target pane medium)))

;;; You can pass :FILLED T to this in order to highlight the target when clicked on...
(defmethod draw-target ((pane scroll-bar-target-pane) medium
			&key filled (ink +foreground-ink+))
  (let ((coord-cache (slot-value pane 'coord-cache)))
    (unless coord-cache
      (let ((identity (sheet-cursor pane)))
	(setq coord-cache
	      (setf (slot-value pane 'coord-cache)
		    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
		      ;; This seems to be a kludge to deal with roundoff?
		      (decf right) (decf bottom)
		      ;; Indent the target arrows a little for aesthetic purposes
		      (incf left 2) (decf right 2)
		      (incf top 2)  (decf bottom 2)
		      (ecase identity
			(:scroll-up
			  (list left bottom
				(+ left (/ (- right left) 2)) top
				right bottom))
			(:scroll-down
			  (list left top
				(+ left (/ (- right left) 2)) bottom
				right top))
			(:scroll-left
			  (list right top
				left (/ (+ top bottom) 2)
				right bottom))
			(:scroll-right
			  (list left top
				right (/ (+ top bottom) 2)
				left bottom))))))))
    (draw-polygon* medium coord-cache :filled filled :ink ink)))

(defclass scroll-bar-shaft-pane
	  (space-requirement-mixin
	   sheet-single-child-mixin
	   leaf-pane)
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
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (draw-rectangle* medium left top (1- right) (1- bottom)
		       :filled nil))
    (draw-thumb pane medium)))

(defparameter *scroll-bar-thumb-ink* (make-gray-color 2/3))

(defmethod draw-thumb ((pane scroll-bar-shaft-pane) medium
		       &key (ink +foreground-ink+))
  (let ((needs-erase (slot-value pane 'needs-erase)))
    (setf (slot-value pane 'needs-erase) (not (eq ink +background-ink+)))
    (when (and (not needs-erase)
	       (eq ink +background-ink+))
      (return-from draw-thumb (values))))
  (let* ((scroll-bar (slot-value pane 'scroll-bar))
	 (scroll-value (gadget-value scroll-bar))
	 (min-value (gadget-min-value scroll-bar))
	 (max-value (gadget-max-value scroll-bar))
	 (gadget-size (or (scroll-bar-current-size scroll-bar) 0))
	 (gadget-range (abs (- max-value min-value)))
	 (identity (sheet-cursor pane)))
    (flet ((draw-car (medium left top right bottom which)
	     (decf right) (decf bottom)
	     (draw-rectangle* medium left top right bottom
			      :filled t
			      :ink (if (eq ink +foreground-ink+) *scroll-bar-thumb-ink* ink))
	     (case which
	       (:vertical-scroll
		 (draw-line* medium left top right top :ink ink)
		 (draw-line* medium left bottom right bottom :ink ink))
	       (:horizontal-scroll
		 (draw-line* medium left bottom left top :ink ink)
		 (draw-line* medium right bottom right top :ink ink)))))
      (declare (dynamic-extent #'draw-car))
      (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
	(let* ((height (- y2 y1))
	       (width (- x2 x1))
	       (scroll-range (ecase identity
			       (:vertical-scroll height)
			       (:horizontal-scroll width)))
	       (car-size (max 10 (* scroll-range (/ gadget-size gadget-range))))
	       (elevator-top (compute-symmetric-value 
			       min-value max-value scroll-value 0
			       scroll-range))
	       (elevator-bottom (+ elevator-top car-size)))
	  (ecase identity
	    (:vertical-scroll
	      (draw-car medium
			(+ x1 1)           (+ y1 elevator-top)
			(+ x1 width -1) (+ y1 elevator-bottom)
			identity))
	    (:horizontal-scroll
	      (draw-car medium
			elevator-top      (+ y1 1)
			elevator-bottom   (+ y1 height -1)
			identity))))))))


;;; Given min1, max1 and value1 which is between them it will return
;;; value2 which would be proportionally between min2 and max2.
(defun compute-symmetric-value (min1 max1 value1 min2 max2)
  (declare (values value2))
  (let* ((distance1 (- max1 min1))
	 (fraction1 (if (zerop distance1) 0 (/ (- value1 min1) distance1))))
    (let ((x (+ min2 (* (- max2 min2) fraction1))))
      (if (integerp x) x (float x)))))

(defmethod handle-event :around ((pane scroll-bar-target-pane) 
				 (event pointer-button-press-event))
  (with-sheet-medium (medium pane)
    (draw-target pane medium :filled t :ink +foreground-ink+)
    (call-next-method)
    (draw-target pane medium :filled t :ink +background-ink+)
    (draw-target pane medium :filled nil)))


;;; This implements genera style scroll bars.
;;; Vertical Scroll bars
;;;    Top Target
;;;       Left: Next Line   Middle: First Page   Right: Previous Line
;;;    Bottom Target
;;;       Left: Next Page   Middle: Last Page    Right: Previous Page
(defmethod handle-event ((pane scroll-bar-target-pane) 
			 (event pointer-button-press-event))
  (let* ((scroll-bar (slot-value pane 'scroll-bar))
	 (client (gadget-client scroll-bar))
	 (id (gadget-id scroll-bar)))
    (case (pointer-event-button event)
      (#.+pointer-left-button+
       (ecase (slot-value pane 'end)
	 (:greater-than
	   (scroll-down-page-callback scroll-bar client id))
	 (:less-than
	   (scroll-down-line-callback scroll-bar client id))))
      (#.+pointer-middle-button+
       (ecase (slot-value pane 'end)
	 (:greater-than
	   (scroll-to-bottom-callback scroll-bar client id))
	 (:less-than
	   (scroll-to-top-callback scroll-bar client id))))
      (#.+pointer-right-button+
       (ecase (slot-value pane 'end)
	 (:greater-than
	   (scroll-up-page-callback scroll-bar client id))
	 (:less-than
	   (scroll-up-line-callback scroll-bar client id)))))))

(defmethod handle-event ((pane scroll-bar-shaft-pane) 
			 (event pointer-button-press-event))
  (let* ((x (pointer-event-x event))
	 (y (pointer-event-y event))
	 (scroll-bar (slot-value pane 'scroll-bar))
	 (client (gadget-client scroll-bar))
	 (id (gadget-id scroll-bar))
	 (orientation (gadget-orientation scroll-bar)))
    (case (pointer-event-button event)
      (#.+pointer-left-button+
       (scroll-line-to-top-callback
	 scroll-bar client id orientation x y))
      (#.+pointer-right-button+
       (scroll-line-to-bottom-callback
	 scroll-bar client id orientation x (- (bounding-rectangle-height scroll-bar) y)))
      (#.+pointer-middle-button+
       (scroll-elevator-callback
	 scroll-bar client id orientation x y)))))

(defmethod handle-event ((pane scroll-bar-shaft-pane) (event pointer-enter-event))
  ;;--- Change the mouse cursor and set the mouse string
  )

(defmethod handle-event ((pane scroll-bar-shaft-pane) (event pointer-exit-event))
  ;;--- Change the mouse cursor and set the mouse string
  )

(defmethod handle-event ((pane scroll-bar-target-pane) (event pointer-enter-event))
  ;;--- Change the mouse cursor and set the mouse string
  )

(defmethod handle-event ((pane scroll-bar-target-pane) (event pointer-exit-event))
  ;;--- Change the mouse cursor and set the mouse string
  )

(defmethod handle-event :after ((pane scroll-bar-shaft-pane) (event pointer-event))
  (deallocate-event event))

(defmethod handle-event :after ((pane scroll-bar-target-pane) (event pointer-event))
  (deallocate-event event))

(defmethod update-scroll-bar-value ((pane scroll-bar-shaft-pane) scroll-bar coord min max)
  (let* ((min-value (gadget-min-value scroll-bar))
	 (max-value (gadget-max-value scroll-bar))
	 (value (compute-symmetric-value min max coord min-value max-value)))
    (setf (gadget-value scroll-bar) value)))

(defmethod (setf gadget-value) (value (pane scroll-bar-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (setf (slot-value pane 'value) value))

(defmethod (setf gadget-value) :around (value (pane scroll-bar-pane) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (if (port pane)
      (let ((shaft-pane (slot-value pane 'shaft-pane)))
	(with-sheet-medium (medium shaft-pane)
	  (draw-thumb shaft-pane medium :ink +background-ink+)
	  (call-next-method)
	  (draw-thumb shaft-pane medium :ink +foreground-ink+)))
      (call-next-method)))
