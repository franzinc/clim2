;;; -*- Mode: Lisp; Package: silica -*-
;; See the file LICENSE for the full license governing this code.
;;

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements MS-Windows native scrolling for Clim 2.0.            *
*                                                                            *
*                                                                            *
*                                                                            *
****************************************************************************|#


;; support for variable thumb size

(in-package :silica)

(eval-when (compile load eval)
  (define-event-class scrollbar-event (event) 
    ((sheet :reader event-sheet :initarg :sheet)
     (orientation :initarg :orientation :reader scrollbar-event-orientation)
     (action :initarg :action :reader scrollbar-event-action)
     (amount :initarg :amount :reader scrollbar-event-amount)
     ;; Added the following slot to distinguish between dragging the slug
     ;; and releasing the slug after dragging (alemmens, 2004-12-24).
     (dragging-p :initarg :dragging-p :initform nil :reader scroll-bar-event-dragging-p
                 :documentation "If true, the VALUE-CHANGED-CALLBACK won't be called."))))

(defmethod handle-event ((pane mswin-scroll-bar) (event scrollbar-event))
  ;; Added callback invocation (alemmens, 2004-12-24).
  (with-slots (orientation action amount dragging-p) event
    (assert (eql orientation (gadget-orientation pane)))
    (multiple-value-bind (min max) (gadget-range* pane)
      ;; SCROLL-BAR-SIZE is less than or equal to the range.
      (multiple-value-bind (tentative-new-value callback)
          (case action
            ;; Press arrows
            (:relative-jump
             ;; AMOUNT is plus or minus one
             (values (+ (gadget-value pane) 
                        ;; Replaced size by line-increment (alemmens, 2004-12-17)
                        (* (scroll-bar-line-increment pane) amount))
                     (if (plusp amount) 'scroll-down-line-callback 'scroll-up-line-callback)))
            ;; Click near thumb
            (:screenful
             ;; AMOUNT is plus or minus one
             (values (+ (gadget-value pane) (* (scroll-bar-size pane) amount))
                     (if (plusp amount) 'scroll-down-page-callback 'scroll-up-page-callback)))
            ;; Drag thumb
            (:percentage
             ;; AMOUNT is absolute position in the scroll range
             ;; (from 0 to *win-scroll-grain*).
             (values (convert-scroll-bar-value-in pane amount)
                     'drag-callback)))
        (let ((new-value (max min 
                              (min (- max (scroll-bar-size pane)) tentative-new-value)))
              (client (gadget-client pane))
              (id (gadget-id pane)))
          (unless (= new-value (gadget-value pane))
            (funcall callback pane client id new-value))
          (when (and (eql action :percentage) (not dragging-p))
            ;; The thumb was released: we can call the value-changed callback.
            (value-changed-callback pane client id new-value)))))))


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

(defmethod acl-clim::sheet-wants-default-pointer 
    ((object silica::mswin-scroller-pane))
  (acl-clim::sheet-wants-default-pointer (slot-value object 'contents)))

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
  (cond ((scroller-pane-gadget-supplies-scrolling-p client) 
	 ;; FIXME - child is a text-editor
	 nil)
	(t
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
		  ;;--- used to skip this if (zerop amount)
		  (scroll-extent
		   contents
		   (bounding-rectangle-min-x region)
		   amount)))
	       (:horizontal
		(let ((amount 
		       (+ (bounding-rectangle-min-x extent)
			  (* (bounding-rectangle-width extent)
			     (if (= size (gadget-range sheet))
				 0
			       (/ (- value (gadget-min-value sheet))
				  (gadget-range sheet)))))))
		  (scroll-extent
		   contents
		   amount
		   (bounding-rectangle-min-y region))))
	       ;;-- Yuck
	       (clim-internals::maybe-redraw-input-editor-stream
		contents
		(pane-viewport-region contents))))))))


;;--- In the case where the viewport is bigger than the window this
;;--- code gets things wrong.  Check out the thinkadot demo.  It's
;;--- because (- (--) (- vmin)) is negative.
(defmethod update-scroll-bar ((scroll-bar silica::mswin-scroller-pane) 
			      min max vmin vmax orientation)
  (declare (optimize (safety 0) (speed 3)))
  ;;-- Is this really the right thing to do?
  ;;-- If in an interactor some draws at -ve coordinates but the
  ;;window is large enough no one changes the viewport but we cannot scroll-either
  ;;(maxf max vmax)
  ;;(minf min vmin)
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
      (when (and current-size
		 (= (the single-float current-size) range)
		 (= min vmin)
		 (> (- vmax vmin) (- max min)))
	(return-from update-scroll-bar))
      ;; The elevator size in 01 units - calculated from the contents
      (let* ((contents-range (float (- max min) 0.0s0))
	     (viewport-range (float (- vmax vmin) 0.0s0))
	     (viewport-past-contents-p (and (> vmax max) (> vmin min)))
	     (corrected-contents-range (+ contents-range
					  (if viewport-past-contents-p
					      (- vmax max)
					    0)))
	     (size (the single-float
		     (* range
			(the single-float
			  (if (= max min)
			      1.0
			    (min 1.0 (the single-float
				       (/ viewport-range
					  corrected-contents-range))))))))
	     ;;-- This does not scale by the range
	     (pos (the single-float
		    (min 1.0s0 
			 (max 0.0s0
			      (if (<= corrected-contents-range viewport-range)
				  0.0
				(/ (float (- vmin min) 0.0s0) 
				   ;;--- Uh-oh, the home-grown scroll bars
				   ;;--- seem to have a different contract
				   ;;--- from Motif/OpenLook.  Fix them!
				   corrected-contents-range)))))))
	(declare (type single-float pos size))
	(unless (and current-size
		     current-value
		     (= current-size size)
		     (= current-value pos))
	  (let ((mirror (sheet-mirror scroll-bar))
		(struct (ct:ccallocate win:scrollinfo))
		(bar (if (eq orientation :vertical) 
			 win:SB_VERT win:SB_HORZ))
		(page (floor (* size (acl-clim::win-scroll-grain acl-clim::*acl-port*))))
		(position (floor (* pos (acl-clim::win-scroll-grain acl-clim::*acl-port*)))))
	    (ct:csets win:scrollinfo struct
		      cbSize (ct:sizeof win:scrollinfo)
		      fMask win:SIF_ALL
		      nMin 0
		      nMax (acl-clim::win-scroll-grain acl-clim::*acl-port*)
		      nPage page
		      nPos position)
	    (win:SetScrollInfo mirror bar struct 1))
	  (case orientation
	    (:vertical
	     (setf (scroller-current-vertical-size scroll-bar) size)
	     (setf (scroller-current-vertical-value scroll-bar) pos))
	    (:horizontal
	     (setf (scroller-current-horizontal-size scroll-bar) size)
	     (setf (scroller-current-horizontal-value scroll-bar) pos)))
	  )))))

(defmethod contents-range ((scroller mswin-scroller-pane) orientation)
  (cond ((scroller-pane-gadget-supplies-scrolling-p scroller) 
	 ;; FIXME - child is a text-editor
	 0)
	(t
	 (with-slots (viewport) scroller
	   (with-bounding-rectangle* (left top right bottom) 
	       (viewport-contents-extent viewport)
	     (ecase orientation
	       (:horizontal (- right left))
	       (:vertical (- bottom top))))))))

(defmethod viewport-range ((scroller mswin-scroller-pane) orientation)
  (cond ((scroller-pane-gadget-supplies-scrolling-p scroller) 
	 ;; FIXME - child is a text-editor
	 0)
	(t
	 (with-slots (viewport) scroller
	   (with-bounding-rectangle* (left top right bottom) 
	       (viewport-viewport-region viewport)
	     (ecase orientation
	       (:horizontal (- right left))
	       (:vertical (- bottom top))))))))

(defmethod scroll-up-line ((scroller-pane mswin-scroller-pane) orientation)
  (with-slots (current-vertical-size current-vertical-value
	       current-horizontal-size current-horizontal-value
	       contents) scroller-pane
    (let* ((window (sheet-mirror scroller-pane))
	   (flag (case orientation
		   (:vertical win:SB_VERT)
		   (:horizontal win:SB_HORZ)))
	   (current-value (/ (win:GetScrollPos window flag)
			     (float (acl-clim::win-scroll-grain acl-clim::*acl-port*))))
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
      (scroll-bar-value-changed-callback
       scroller-pane scroller-pane orientation new-value current-size))))

#||(case orientation
	  (:horizontal current-horizontal-value)
	  (:vertical  current-vertical-value))||#
 
(defmethod scroll-down-line ((scroller-pane mswin-scroller-pane) orientation)
  (with-slots (current-vertical-size current-vertical-value
	       current-horizontal-size current-horizontal-value
	       contents) scroller-pane
    (let* ((flag (case orientation
		   (:vertical win:SB_VERT)
		   (:horizontal win:SB_HORZ)))
	   (window (sheet-mirror scroller-pane))
	   (current-value (/ (win:GetScrollPos window flag)
			     (float (acl-clim::win-scroll-grain acl-clim::*acl-port*))))
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
	   (new-value (min (- (/ (1+ (acl-clim::win-scroll-grain acl-clim::*acl-port*)) ;; max size
				 (acl-clim::win-scroll-grain acl-clim::*acl-port*)) ;; max pos
			      current-size)
			   (+ current-value line-value))))
      (scroll-bar-value-changed-callback
       scroller-pane scroller-pane orientation new-value current-size)
      )))

(defmethod scroll-up-page ((scroller-pane mswin-scroller-pane) orientation)
  (with-slots (current-vertical-size current-vertical-value
	       current-horizontal-size current-horizontal-value
	       viewport contents) scroller-pane
    (let* ((flag (case orientation
		   (:vertical win:SB_VERT)
		   (:horizontal win:SB_HORZ)))
	   (window (sheet-mirror scroller-pane))
	   (current-value (/ (win:GetScrollPos window flag)
			     (float (acl-clim::win-scroll-grain acl-clim::*acl-port*))))
	   (current-size (case orientation
			   (:horizontal current-horizontal-size)
			   (:vertical  current-vertical-size)))
	   (contents-range (contents-range scroller-pane orientation))
	   (viewport-range (bounding-rectangle-max-y viewport))
	   new-value)
      (if (zerop contents-range)
	  (setq new-value current-value)
	(let ((page-value 
	       (the single-float 
		 (/ viewport-range (float contents-range 0.0s0)))))
	  (setq new-value (max 0.0 (- current-value page-value)))))
      (scroll-bar-value-changed-callback
       scroller-pane scroller-pane orientation new-value current-size))))

(defmethod scroll-down-page  ((scroller-pane mswin-scroller-pane) orientation)
  (with-slots (current-vertical-size current-vertical-value
	       current-horizontal-size current-horizontal-value
	       viewport contents) scroller-pane
    (let* ((flag (case orientation
		   (:vertical win:SB_VERT)
		   (:horizontal win:SB_HORZ)))
	   (window (sheet-mirror scroller-pane))
	   (current-value (/ (win:GetScrollPos window flag)
			     (float (acl-clim::win-scroll-grain acl-clim::*acl-port*))))
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
	  (setq new-value (min (- (/ (1+ (acl-clim::win-scroll-grain acl-clim::*acl-port*))
				     (acl-clim::win-scroll-grain acl-clim::*acl-port*))
				  current-size)
			       (+ current-value page-value)))))
      (scroll-bar-value-changed-callback
       scroller-pane scroller-pane orientation new-value current-size))))

(defmethod scroll-to-position ((scroller-pane mswin-scroller-pane)
			       orientation pos)
  (with-slots (current-vertical-size current-vertical-value
	       current-horizontal-size current-horizontal-value
	       contents) scroller-pane
    (let* ((current-size (case orientation
			   (:horizontal current-horizontal-size)
			   (:vertical  current-vertical-size)))
	   (new-value (/ pos (float (acl-clim::win-scroll-grain acl-clim::*acl-port*)))))
      (scroll-bar-value-changed-callback
       scroller-pane scroller-pane orientation new-value current-size))))

(defmethod handle-event :after ((pane mswin-scroller-pane) 
				(event scrollbar-event))
  (deallocate-event event))

;; The way this is used, it has to be the width of two borders.
;; If it is too small, then when you scroll, you will get 
;; garbage pixels.  JPM.
(defparameter *win-border-thick* 4)

;; this used to be hardwired to 18 -tjm 17Jul97
(defun win-scroll-thick (x-or-y)
  (+ *win-border-thick*
     (win:GetSystemMetrics 
      (if (eq x-or-y :x)
	  win:SM_CXHTHUMB
	win:SM_CYVTHUMB))))

(defun needs-scroller-pane-adjustment-p (pane)
  ;; Some widgets come with built-in scrolling. Since we're using
  ;; scroller panes for them too, we need to compensate for their
  ;; built-in need to scroll:
  (or (not (scroller-pane-gadget-supplies-scrolling-p pane))
      (gadget-supplies-scrolling-p (sheet-child pane))))

;;; silica\db-layout
(defmethod compose-space ((pane mswin-scroller-pane) &key width height)
  (let* ((child (sheet-child pane))
	 (scroll (and (needs-scroller-pane-adjustment-p pane)
		      (silica::scroller-pane-scroll-bar-policy pane)))
	 (vscroll (if (member scroll '(t :both :dynamic :vertical))
		      (win-scroll-thick :y) *win-border-thick*))
	 (hscroll (if (member scroll '(t :both :dynamic :horizontal))
		      (win-scroll-thick :x) *win-border-thick*))
	 (sr (compose-space child :width width :height height))
	 (nr (make-instance 'general-space-requirement
	       :width (+ (space-requirement-width sr) vscroll)
	       :min-width (+ (space-requirement-min-width sr) vscroll)
	       :max-width (min most-positive-fixnum
			       (+ (space-requirement-max-width sr) vscroll))
	       ;; now assumes menu is one line thick, but in practice it
	       ;; can be more, and some windows have no menu at all
	       :height (+ (space-requirement-height sr) hscroll)
	       :min-height (+ (space-requirement-min-height sr) hscroll)
	       :max-height (min most-positive-fixnum
				(+ (space-requirement-max-height sr)
				   hscroll)))))
    nr))

(defmethod allocate-space ((pane mswin-scroller-pane) width height)
  (let* ((scroll (and (needs-scroller-pane-adjustment-p pane)
		      (silica::scroller-pane-scroll-bar-policy pane)))
	 (vscroll (member scroll '(t :both :dynamic :vertical)))
	 (hscroll (member scroll '(t :both :dynamic :horizontal)))
	 (nwidth (when width
		   (- width 
		      (if vscroll (win-scroll-thick :x) *win-border-thick*))))
	 ;; does this need to take *win-menu-line-height* into account?
	 (nheight (when height
		    (- height
		       (if hscroll (win-scroll-thick :y) *win-border-thick*))))
	 (child (sheet-child pane)))
    (when (< nwidth 1)
      (warn "Scroller-pane width too small, compensating")
      (setq nwidth 1))
    (when (< nheight 1)
      (warn "Scroller-pane height too small, compensating")
      (setq nheight 1))
    (resize-sheet child nwidth nheight)	;;; get the viewport sized correctly!
    (allocate-space child nwidth nheight)))

(eval-when (compile load eval) 
  ;; Turn this on as long as it gets turned off in pkgdcl.lisp
  (setq excl:*enable-package-locked-errors* common-lisp-user::*lock-preference*)
  (provide :climnt))
