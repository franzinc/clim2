;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements MS-Windows native scrolling for Clim 2.0.            *
*                                                                            *
*                                                                            *
*                                                                            *
****************************************************************************|#


(in-package :silica)

(eval-when (compile load eval)

(define-event-class scrollbar-event (event) 
  ((sheet :reader event-sheet :initarg :sheet)
   (orientation :initarg :orientation :reader scrollbar-event-orientation)
   (action :initarg :action :reader scrollbar-event-action)
   (amount :initarg :amount :reader scrollbar-event-amount)))
) ;; eval-when

(define-event-resource scrollbar-event 10)

(defmethod distribute-event ((port acl-clim::acl-port) (event scrollbar-event))
  (let ((sheet (event-sheet event)))
    (if sheet (handle-event sheet event) (deallocate-event event))
  ))


(defclass mswin-scroller-pane (mirrored-sheet-mixin
			       permanent-medium-sheet-output-mixin
			       layout-pane
			       scroller-pane
			       range-gadget-mixin)
    ((current-vertical-value :initform nil
			     :accessor scroller-current-vertical-value)
     (current-horizontal-value :initform nil
			       :accessor scroller-current-horizontal-value)
     (current-horizontal-size :initform nil
			      :accessor scroller-current-horizontal-size)
     (current-vertical-size :initform nil
			    :accessor scroller-current-vertical-size)
     ))


(defmethod initialize-instance :after ((pane mswin-scroller-pane) 
				       &key contents frame-manager frame
					    scroll-bars)
  (check-type scroll-bars
      (member t nil :both :dynamic :vertical :horizontal))
  (if (setf (scroller-pane-gadget-supplies-scrolling-p pane)
        (gadget-supplies-scrolling-p contents))
      (sheet-adopt-child pane contents)
    (with-slots (vertical-scroll-bar horizontal-scroll-bar
		 (c contents) viewport foreground background)
	pane
      (with-look-and-feel-realization (frame-manager frame)
	(setf c contents
	      viewport (make-pane 'viewport :scroller-pane pane))
	(sheet-adopt-child pane viewport)
	(sheet-adopt-child viewport c)
	(when (member scroll-bars '(t :both :dynamic :vertical))
	  (setf vertical-scroll-bar pane))
	(when (member scroll-bars '(t :both :dynamic :horizontal))
	  (setf horizontal-scroll-bar pane))
	;;--- Add callbacks
	))))

(defmethod gadget-supplies-scrolling-p ((sheet t)) nil)
(defmethod gadget-supplies-scrolling-p ((sheet hlist-pane)) t)
(defmethod gadget-supplies-scrolling-p ((sheet acl-clim::acl-text-editor-pane)) t)

(defmethod handle-event ((pane mswin-scroller-pane) 
						 (event scrollbar-event))
  (with-slots (orientation action amount) event
	(case action
	  (:relative-jump
		(case amount
		  (-1  (scroll-up-line pane orientation))
		  (1 (scroll-down-line pane orientation))))
	  (:screenful
		(case amount
		  (-1  (scroll-up-page pane orientation))
		  (1 (scroll-down-page pane orientation))))
	  (:percentage
		(scroll-to-position pane orientation amount)))))

(defmethod scroll-bar-value-changed-callback
  (sheet (client scroller-pane) id value size)
  (with-slots (viewport contents) client
    (let* ((extent (viewport-contents-extent viewport))
           (region (viewport-viewport-region viewport)))
      (case id
	(:vertical
	 (let ((amount
		(+ (bounding-rectangle-min-y extent)
		   (* (bounding-rectangle-height extent)
		      (if (= size (gadget-range sheet))
			  0
			(/ (- value (gadget-min-value sheet))
			   (gadget-range sheet)))))))
	   (if (zerop amount) 
	       (let ((flag (case id
			     (:vertical win:sb_vert)
			     (:horizontal win:sb_horz)))
		     (window (sheet-mirror client)))
		 (win::setScrollPos window flag 0 1))
	     (scroll-extent
	      contents
	      (bounding-rectangle-min-x region)
	      amount))))
	(:horizontal
	 (let ((amount 
		(+ (bounding-rectangle-min-x extent)
		   (* (bounding-rectangle-width extent)
		      (if (= size (gadget-range sheet))
			  0
			(/ (- value (gadget-min-value sheet))
			   (gadget-range sheet)))))))
	   (if (zerop amount) 
	       (let ((flag (case id
			     (:vertical win:sb_vert)
			     (:horizontal win:sb_horz)))
		     (window (sheet-mirror client)))
		 (win::setScrollPos window flag 0 1))
	     (scroll-extent
	      contents
	      amount
	      (bounding-rectangle-min-y region)))))
	;;-- Yuck
	(clim-internals::maybe-redraw-input-editor-stream
	 contents
	 (pane-viewport-region contents))))))


;;--- In the case where the viewport is bigger than the window this
;;--- code gets things wrong.  Check out the thinkadot demo.  It's
;;--- because (- (--) (- vmin)) is negative.
(defun update-mswin-sbar (scroll-bar min max vmin vmax orientation)
  (declare (optimize (safety 0) (speed 3)))
  ;;-- Is this really the right thing to do?
  ;;-- If in an interactor some draws at -ve coordinates but the
  ;;window is large enough no one changes the viewport but we cannot scroll-either
  ;(maxf max vmax)
  ;(minf min vmin)
  (let ((current-size
		  (case orientation
			(:vertical (scroller-current-vertical-size scroll-bar))
			(:horizontal (scroller-current-horizontal-size scroll-bar))))
		(current-value
		  (case orientation
			(:vertical (scroller-current-vertical-value scroll-bar))
			(:horizontal (scroller-current-horizontal-value scroll-bar)))))
	;; Kinda bogus benchmark optimization -- if the scroll-bar was full size
	;; before, and the viewport is bigger than the extent, don't bother with
	;; the fancy math.
	(let* ((gmin (float (gadget-min-value scroll-bar) 0s0))
		   (gmax (float (gadget-max-value scroll-bar) 0s0))
		   (range (- gmax gmin)))
	  (declare (type single-float range gmin gmax))
	  (when (and (and current-size (= (the single-float current-size) range))
				 (> (- vmax vmin) (- max min)))
		(return-from update-mswin-sbar))
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
			 ;;-- This does not scale by the range
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
		  (case orientation
			(:vertical
			  (setf (scroller-current-vertical-size scroll-bar) size)
			  (setf (scroller-current-vertical-value scroll-bar) pos))
			(:horizontal
			  (setf (scroller-current-horizontal-size scroll-bar) size)
			  (setf (scroller-current-horizontal-value scroll-bar) pos)
			  ))
		  ;;-- It would be nice if we could do this at the point of scrolling
		  #+foo (cerror "foo" "pos=~a size=~a min=~a max=~a vmin=~a vmax=~a contents-range=~a"
						pos size min max vmin vmax contents-range)
		  ;(cerror "never mind" "size=~a pos=~a" size pos) pos
		  #+ignore
		  (let* ((line-scroll1 (line-scroll-amount
									 scroll-bar orientation :down)
							   #||(line-scroll-amount (slot-value scroll-bar 'client)
				 								   orientation nil)||#)
				 (line-scroll (if (zerop contents-range)
								  0 ;-- Who knows
								  (* range (/ line-scroll1 contents-range)))))
			(change-scroll-bar-values scroll-bar 
									  :slider-size size
									  :value pos
									  :line-increment line-scroll)))))))


(defmethod contents-range ((scroller mswin-scroller-pane) orientation)
  (with-slots (viewport) scroller
    (with-bounding-rectangle* (left top right bottom) 
	(viewport-contents-extent viewport)
      (ecase orientation
	(:horizontal (- right left))
	(:vertical (- bottom top))))))

(defmethod viewport-range ((scroller mswin-scroller-pane) orientation)
  (with-slots (viewport) scroller
    (with-bounding-rectangle* (left top right bottom) 
	(viewport-viewport-region viewport)
      (ecase orientation
	(:horizontal (- right left))
	(:vertical (- bottom top))))))

(defmethod scroll-up-line ((scroller-pane mswin-scroller-pane) orientation)
  (with-slots (current-vertical-size current-vertical-value
									 current-horizontal-size current-horizontal-value
									 viewport contents) scroller-pane
	(let* ((window (sheet-mirror scroller-pane))
		   (flag (case orientation
				   (:vertical win:sb_vert)
				   (:horizontal win:sb_horz)))
		   (current-value (/ (win::getScrollPos window flag) 100.0))
		   (current-size (case orientation
						   (:horizontal current-horizontal-size)
						   (:vertical  current-vertical-size)))
		   (contents-range (contents-range scroller-pane orientation))
		   (line-value (if (= contents-range 0)
						   0
						   (the single-float
								(/ (line-scroll-amount scroller-pane orientation :up)
								   (float contents-range  0.0s0)))))
		   (new-value (max 0.0 (- current-value line-value))))
	  (win::setScrollPos window flag (floor (* 100 new-value)) 1)
	  (scroll-bar-value-changed-callback
		scroller-pane scroller-pane orientation new-value current-size))))

 #||(case orientation
	  (:horizontal current-horizontal-value)
	  (:vertical  current-vertical-value))||#
 
(defmethod scroll-down-line ((scroller-pane mswin-scroller-pane) orientation)
  (with-slots (current-vertical-size current-vertical-value
									 current-horizontal-size current-horizontal-value
									 viewport contents) scroller-pane
	(let* ((flag (case orientation
				   (:vertical win:sb_vert)
				   (:horizontal win:sb_horz)))
		   (window (sheet-mirror scroller-pane))
		   (current-value (/ (win::getScrollPos window flag) 100.0))
		   (current-size (case orientation
						   (:horizontal current-horizontal-size)
						   (:vertical  current-vertical-size)))
		   (contents-range (contents-range scroller-pane orientation))
		   (line-value (if (= contents-range 0)
						   0
						   (the single-float
								(/ (line-scroll-amount
									 scroller-pane orientation :down)
								   (float contents-range  0.0s0)))))
		   (new-value (+ current-value line-value)))
	  (win::setScrollPos window flag (floor (* 100 new-value)) 1)
	  (scroll-bar-value-changed-callback
		scroller-pane scroller-pane orientation new-value current-size)
	  )))

(defmethod scroll-up-page ((scroller-pane mswin-scroller-pane) orientation)
  (with-slots (current-vertical-size current-vertical-value
									 current-horizontal-size current-horizontal-value
									 viewport contents) scroller-pane
	(let* ((flag (case orientation
				   (:vertical win:sb_vert)
				   (:horizontal win:sb_horz)))
		   (window (sheet-mirror scroller-pane))
		   (current-value (/ (win::getScrollPos window flag) 100.0))
		   (current-size (case orientation
						   (:horizontal current-horizontal-size)
						   (:vertical  current-vertical-size)))
		   (contents-range (contents-range scroller-pane orientation))
		   (viewport-range (bounding-rectangle-max-y viewport))
		   new-value)
	  (if (zerop contents-range)
		  (setq new-value current-value)
		  (let ((page-value (the single-float 
								 (/ viewport-range (float contents-range 0.0s0)))))
			(setq new-value (max 0.0 (- current-value page-value)))))
	  (win::setScrollPos window flag (floor (* 100 new-value)) 1)
	  (scroll-bar-value-changed-callback
		scroller-pane scroller-pane orientation new-value current-size))))

(defmethod scroll-down-page  ((scroller-pane mswin-scroller-pane) orientation)
  (with-slots (current-vertical-size current-vertical-value
									 current-horizontal-size current-horizontal-value
									 viewport contents) scroller-pane
	(let* ((flag (case orientation
				   (:vertical win:sb_vert)
				   (:horizontal win:sb_horz)))
		   (window (sheet-mirror scroller-pane))
		   (current-value (/ (win::getScrollPos window flag) 100.0))
		   (current-size (case orientation
						   (:horizontal current-horizontal-size)
						   (:vertical  current-vertical-size)))
		   (contents-range (contents-range scroller-pane orientation))
		   (viewport-range (bounding-rectangle-max-y viewport))
		   new-value)
	  (if (zerop contents-range)
		  (setq new-value current-value)
		  (let ((page-value (the single-float 
								 (/ viewport-range (float contents-range 0.0s0)))))
			(setq new-value (+ current-value page-value))))
	  (win::setScrollPos window flag (floor (* 100 new-value)) 1)
	  (scroll-bar-value-changed-callback
		scroller-pane scroller-pane orientation new-value current-size))))

(defmethod scroll-to-position ((scroller-pane mswin-scroller-pane)
			       orientation pos)
  (with-slots (current-vertical-size current-vertical-value
	       current-horizontal-size current-horizontal-value
	       viewport contents) scroller-pane
    (let* ((flag (case orientation
		   (:vertical win:sb_vert)
		   (:horizontal win:sb_horz)))
	   (current-value (case orientation
			    (:horizontal current-horizontal-value)
			    (:vertical  current-vertical-value)))
	   (current-size (case orientation
			   (:horizontal current-horizontal-size)
			   (:vertical  current-vertical-size)))
	   (contents-range (contents-range scroller-pane orientation))
	   (viewport-range (bounding-rectangle-max-y viewport))
	   (new-value (/ pos 100)))
      (scroll-bar-value-changed-callback
       scroller-pane scroller-pane orientation new-value current-size))))

(defmethod handle-event :after ((pane mswin-scroller-pane) 
				(event scrollbar-event))
  (deallocate-event event))

(defvar *win-scroll-thick*  18) ; thick 24 none 16
(defvar *win-border-thick* 2)   ; thick 8 none 0

;;; silica\db-layout
(defmethod compose-space ((pane mswin-scroller-pane) &key width height)
  (let* ((child (sheet-child pane))
	 (scroll (and (silica::scroller-pane-scroll-bar-policy pane)
		      (not (scroller-pane-gadget-supplies-scrolling-p pane))))
	 (vscroll (if (member scroll '(t :both :dynamic :vertical))
		      *win-scroll-thick* *win-border-thick*))
	 (hscroll (if (member scroll '(t :both :dynamic :horizontal))
		      *win-scroll-thick* *win-border-thick*))
	 (sr (compose-space child :width width :height height))
	 (nr (make-instance 'general-space-requirement
	       :width (+ (space-requirement-width sr) vscroll hscroll)
	       :min-width (+ (space-requirement-min-width sr) vscroll hscroll)
	       :max-width (min most-positive-fixnum
			       (+ (space-requirement-max-width sr) vscroll))
	       :height (+ (space-requirement-height sr) hscroll vscroll)
	       :min-height (+ (space-requirement-min-height sr) hscroll vscroll)
	       :max-height (min most-positive-fixnum
				(+ (space-requirement-max-height sr)
				   hscroll)))))
    nr))


(defmethod allocate-space ((pane mswin-scroller-pane) width height)
  (let* ((scroll (and (silica::scroller-pane-scroll-bar-policy pane)
		      (not (scroller-pane-gadget-supplies-scrolling-p pane))))
	 (vscroll (member scroll '(t :both :dynamic :vertical)))
	 (hscroll (member scroll '(t :both :dynamic :horizontal)))
	 (nwidth (when width
		   (- width 
		      (if vscroll *win-scroll-thick* *win-border-thick*))))
	 (nheight (when height
		    (- height
		       (if hscroll *win-scroll-thick* *win-border-thick*))))
	 (child (sheet-child pane)))
    (resize-sheet child nwidth nheight)	;;; get the viewport sized correctly!
    (allocate-space child nwidth nheight)))

