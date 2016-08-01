;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements the CLIM mirroring of sheets and panes.  In the      *
*  current deep mirrored implementation, it is still the case that only      *
*  top-level sheets of frames, scroller panes and win-widget controls        *
*  are mirrored.  Here also is support for Grafts.                           *
*                                                                            *
*                                                                            *
****************************************************************************|#


(in-package :acl-clim)

;;--- This isn't really right, all ports only have kludges for this
(defmethod sheet-shell (sheet) sheet)

(defun win-full-screen-area ()
  "Get the area of the screen that can be occupied by windows (screen - task/tool bars)."
  (let ((rect (ct:ccallocate win:rect)))
    (win:SystemParametersInfo win:SPI_GETWORKAREA 0 rect 0)
    (values (coordinate (ct:cref win:rect rect win::left))
            (coordinate (ct:cref win:rect rect win::top))
            (coordinate (ct:cref win:rect rect win::right))
            (coordinate (ct:cref win:rect rect win::bottom)))))

(defmethod realize-graft ((port acl-port) graft)
  (let ((screen (slot-value (screen-device *acl-port*) 'device-handle1)))
    (setf (port-screen port) screen)
    (init-cursors)
    (with-slots (silica::pixels-per-point
                 silica::pixel-width
                 silica::pixel-height
                 silica::mm-width
                 silica::mm-height
                 silica::units) graft
       (with-dc (screen dc)
         (multiple-value-bind (screen-left screen-top screen-right screen-bottom)
             (win-full-screen-area)
           (let ((screen-width (- screen-left screen-right))
                 (screen-height (- screen-bottom screen-top))
                 (logpixelsx (win:GetDeviceCaps dc win:LOGPIXELSX))
                 (logpixelsy (win:GetDeviceCaps dc win:LOGPIXELSY)))
             (setf silica::pixel-width  (- screen-left screen-right)
                   silica::pixel-height (- screen-bottom screen-top)
                   silica::mm-width     (round (* screen-width 25.4s0) logpixelsx)
                   silica::mm-height       (round (* screen-height 25.4s0) logpixelsy))
             (let* ((ppp (/ logpixelsy 72s0))
                    (rounded-ppp (round ppp)))
               (setf silica::pixels-per-point 
                     (if (< (abs (- ppp rounded-ppp)) .1s0) rounded-ppp ppp)))
             
             (setf (sheet-region graft)
                   (ecase silica::units
                     ((:device :pixel)
                      (make-bounding-rectangle
                       screen-left screen-top screen-right screen-bottom))))
             (setf (sheet-native-transformation graft) +identity-transformation+)
             (setf (sheet-direct-mirror graft) screen)
             (update-mirror-transformation port graft)))))))

(defmethod modal-frame-p ((frame t)) nil)
(defmethod modal-frame-p ((frame clim-internals::accept-values-own-window)) t)
(defmethod modal-frame-p ((frame clim-internals::menu-frame)) t)

(defmethod scroller-pane-p ((pane t)) nil)
(defmethod scroller-pane-p ((pane silica::scroller-pane)) t)

(defmethod list-pane-p ((pane t)) nil)
(defmethod list-pane-p ((pane list-pane)) t)

(defmethod combo-box-p ((pane t)) nil)
(defmethod combo-box-p ((pane option-pane)) t)

(defmethod realize-mirror ((port acl-port) sheet)
  (multiple-value-bind (left top right bottom)
      (sheet-native-region* sheet)
    (fix-coordinates left top right bottom)
    (let* ((parent (sheet-mirror sheet))
	   (parent2 (sheet-mirror (sheet-parent sheet)))
           (nullparent (ct:null-handle win:hwnd))
	   (frame (pane-frame sheet))
	   (pretty-name (frame-pretty-name frame))
	   (save-under (and frame
			    (getf (frame-properties frame) :save-under)))
           (window nil)
	   (scroll nil)
	   (msscrollwin nil)
	   (childwin nil)
	   (control nil)
	   (value nil)
	   (items nil)
	   (width (- right left))
	   (height (- bottom top))
	   gadget-id)
      (assert (eq parent parent2) () "parents don't match!")
      ;; Wouldn't this COND work better as a pile of methods, or at least a
      ;; typecase? JPM 14Aug97
      (cond ((scroller-pane-p sheet)
	     (setq msscrollwin t)
	     (setq scroll (and (not (scroller-pane-gadget-supplies-scrolling-p sheet))
			       (silica::scroller-pane-scroll-bar-policy sheet))))
	    ((list-pane-p sheet)
      	     (setq control :hlist)
	     ;;mm: allocate gadget-id per parent
             (setq gadget-id (silica::allocate-gadget-id sheet))
	     (setq value (slot-value sheet 'silica::value))
	     (setq items (slot-value sheet 'silica::items))
	     ;; added value-key (cim 9/18/96)
	     ;; moving this into hlist-open so it can do the right
	     ;; thing with nonexclusive list-panes (cim 9/20/96)
	     #+ignore
	     (when value
	       (setf value 
		 (position value items :test #'equal
			   :key (slot-value sheet 'silica::value-key)))))
            ((combo-box-p sheet)
      	     (setq control :hcombo)
	     ;;mm: allocate gadget-id per parent
             (setq gadget-id (silica::allocate-gadget-id sheet))
	     (setq value (slot-value sheet 'silica::value))
	     (setq items (slot-value sheet 'silica::items))
	     ;; added value-key (cim 9/18/96)
	     ;; added set-gadget-test (cim 9/20/96)
	     ;; removed the "(when value" because nil is a valid value 
	     ;; (cim 9/20/96)
	     (setf value 
	       (position value items 
			 :key (set-gadget-value-key sheet)
			 :test (set-gadget-test sheet)))))
      (setq window
	(cond ((eq control :hlist)
	       (setq childwin t)
	       (hlist-open parent gadget-id
			       0 0 0 0	; resize left top width height 
					; :label (slot-value sheet 'silica::label)
			       :items items
			       :value value
			       :name-key
			       (set-gadget-name-key sheet)
			       :value-key
			       (set-gadget-value-key sheet)
			       :test
			       (set-gadget-test sheet)
			       :mode (slot-value sheet 'silica::mode)
			       :scroll-mode 
			       (let ((p (sheet-parent sheet)))
				 (and (scroller-pane-p p)
				      (silica::scroller-pane-scroll-bar-policy p)))
			       :horizontal-extent
			       (silica::compute-set-gadget-dimensions sheet)
			       ))
	      ((eq control :hcombo)
	       (setq childwin t)
	       (hcombo-open parent gadget-id
			    0 0 0 0	; left top width height 
			    :items items
			    :value value
			    ;; Give vertical scroll bars in case
			    ;; its a long list.
			    :scroll-mode :vertical
			    :name-key
			    (slot-value sheet 'silica::name-key)))
	      ((not (eql parent (port-screen port)))
	       (setq childwin t)
	       (create-child-window
		parent pretty-name scroll left top width height))
	      (save-under
	       (create-pop-up-window
		nullparent 
		pretty-name scroll left top width height
		(not (modal-frame-p frame))))
	      (t 
	       (create-overlapped-window
		(if (modal-frame-p frame) 
		    (let* ((parent *application-frame*)
			   (sheet (when parent (frame-top-level-sheet parent)))
			   (mirror (when sheet (sheet-mirror sheet))))
		      mirror)
		  nullparent)
		pretty-name scroll 
		win:CW_USEDEFAULT
		win:CW_USEDEFAULT
		width height
		(and (getf (frame-properties frame) :native-menu)
		     (slot-value frame 'menu-bar))
		(modal-frame-p frame)
		))))
      (setf (sheet-native-transformation sheet)
        (if childwin
          (sheet-native-transformation (sheet-parent sheet))
          +identity-transformation+))
      ;;added first clause in OR below to prevent bashing a shared event
      ;;queue for activities. -- Kalman
      (unless (or (sheet-event-queue sheet) control msscrollwin save-under) ; tryit
        (setf (sheet-event-queue sheet) (make-acl-event-queue)))
      (when control (setf (silica::gadget-id->window sheet gadget-id) window)) 
      (unless (or control msscrollwin)
        (setf (current-window port) window)
	(unless (dc-initialized port) (initialize-dc))
	(with-dc (window dc)
          (with-slots (logpixelsy) port
            (setf logpixelsy (win:GetDeviceCaps dc win:LOGPIXELSY)))))

      ;; added (cim 10/11/96)
      (when control
	(let ((text-style (pane-text-style sheet)))
	  (when text-style
	    (let ((font (text-style-mapping port text-style)))
	      (frame-send-message
	       (pane-frame sheet)
	       window win:WM_SETFONT 
			       (acl-font-index font) 0)))))
      (when (and childwin (sheet-enabled-p sheet))
	;; It's too soon for this.  Need to do this later, 
	;; after the layout has been processed, but where?
	(win:ShowWindow window win:SW_SHOW))
      window)))

(defmethod realize-mirror :around ((port acl-port) (sheet basic-gadget))
  (let ((window (call-next-method))
	(false 0))
    (unless (gadget-active-p sheet)
      (win:EnableWindow window false)
      (check-last-error "EnableWindow" :action :warn))
    window))

(defmethod destroy-mirror ((port acl-port) sheet)
  ;; "A WIN32 thread cannot use DestroyWindow to destroy a window
  ;; created by a different thread."
  ;;
  ;; Called by note-sheet-degrafted, which is
  ;; called by (setf port) which is called by sheet-disown-child.
  ;; This is applied to frame panes the frame layout changes and
  ;; when frame-exit occurs.
  ;;
  ;; It is not called on the top-level-sheet upon frame-exit,
  ;; presumably in case you want to reuse the associated frame.
  ;; It is called on the top-level-sheet when you call disown-frame
  ;; or destroy-frame.
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror 
      (let ((menuhand (win:GetMenu mirror)))
	(when (and menuhand (plusp menuhand))
	  (delete-menu-bar nil menuhand nil)))
      (win:DestroyWindow mirror)
      ;; DestroyWindow is supposed to destroy everything, including
      ;; the window's menu and the window's child windows.  It is
      ;; also supposed to flush the thread message queue.
      (setf (sheet-direct-mirror sheet) nil))))

(defvar *in-layout-avp* nil)

(defmethod enable-mirror ((port acl-port) sheet)
  (let ((window (sheet-mirror sheet)))
    (unless *in-layout-avp*
      (win:ShowWindow window win:SW_SHOW) ; returns NIL if it was already visible.
      #+ignore
      (frame-update-window (pane-frame sheet) window)
      )))

(defmethod disable-mirror ((port acl-port) sheet)
  (win:ShowWindow (sheet-mirror sheet) win:SW_HIDE))

(defmethod raise-mirror ((port acl-port) (sheet mirrored-sheet-mixin))
  ;; On Windows95, SetForegroundWindow often fails (returns NIL).
  ;; But the menu shows up after a few seconds anyway.  So that is
  ;; why it would be bad to check the return status of this guy.
  ;;
  ;; We are supposed to avoid calling this function.  "Let the
  ;; user control which window is the foreground window."
  (win:SetForegroundWindow (sheet-mirror sheet))
  t)

(defmethod bury-mirror ((port acl-port) (sheet mirrored-sheet-mixin))
  (win:SetWindowPos (sheet-mirror sheet) 
		    win:HWND_BOTTOM
		    0 0 0 0		; x y width height
		    (logior win:SWP_NOACTIVATE
			    win:SWP_NOMOVE ; IGNORE X Y
			    win:SWP_NOSIZE ; IGNORE WIDTH HEIGHT
			    )))

(defmethod mirror-visible-p ((port acl-port) sheet)
  (win:IsWindowVisible (sheet-mirror sheet)))

(defmethod mirror-client-region-internal* ((port acl-port) mirror target)
  (let ((wrect (ct:ccallocate win:rect))
	(topleft (ct:ccallocate win:point))
	(botright (ct:ccallocate win:point)))
    (win:GetWindowRect mirror wrect)
    (setf (ct:cref win:point topleft win::x) (ct:cref win:rect wrect win::left))
    (setf (ct:cref win:point topleft win::y) (ct:cref win:rect wrect win::top))
    (setf (ct:cref win:point botright win::x) (ct:cref win:rect wrect win::right))
    (setf (ct:cref win:point botright win::y) (ct:cref win:rect wrect win::bottom))
    (win:ScreenToClient target topleft)
    (win:ScreenToClient target botright)
    (let ((wleft (ct:cref win:point topleft win::x))
	  (wtop (ct:cref win:point topleft win::y))
	  (wright (ct:cref win:point botright win::x)) 
	  (wbottom (ct:cref win:point botright win::y)))      
      (values (coordinate wleft) (coordinate wtop)
	      (coordinate wright) (coordinate wbottom)))))

(defmethod mirror-region* ((port acl-port) sheet)
  (let ((mirror (sheet-mirror sheet))
        (target (sheet-mirror (sheet-parent sheet))))
    (mirror-client-region-internal* port mirror target)))

(defmethod mirror-inside-region* ((port acl-port) sheet)
  (multiple-value-bind (minx miny maxx maxy)
      (mirror-region* port sheet)
    ;; Returns 0,0,width,height
    (values (coordinate 0) (coordinate 0)
	    (- maxx minx) (- maxy miny))))

(defmethod mirror-native-edges* ((port acl-port) sheet)
  (let ((wrect (ct:ccallocate win:rect)))
    (win:GetWindowRect (sheet-direct-mirror sheet) wrect)
    (let ((wleft (ct:cref win:rect wrect win::left))
          (wtop (ct:cref win:rect wrect win::top))
	  (wright (ct:cref win:rect wrect win::right)) 
	  (wbottom (ct:cref win:rect wrect win::bottom)))      
      (values (coordinate wleft) (coordinate wtop)
	      (coordinate wright) (coordinate wbottom)))))

(defmethod mirror-inside-edges* ((port acl-port) sheet)
  (multiple-value-bind (a b c d)
      (mirror-native-edges* port sheet)
    ;; Returns 0,0,width,height
    (values (coordinate 0) (coordinate 0)
	    (- c a) (- d b))))

(defvar *setting-sheet-mirror* nil)

(defmethod set-sheet-mirror-edges* ((port acl-port) sheet
				    left top right bottom)
  ;; unspecialized (not top)
  (fix-coordinates left top right bottom)
  (unless (eq sheet *setting-sheet-mirror*) ; avoid recursion bug
    (let ((*setting-sheet-mirror* sheet))
      (win:SetWindowPos (sheet-mirror sheet)
			0		; we really want win:HWND_TOP
			left top (- right left) (- bottom top)
			(logior win:SWP_NOACTIVATE
				win:SWP_NOZORDER)))))

(defvar *port-mirror-sheet-alist* nil)

(defun mirror->sheet (port mirror)
  (declare (ignore port))
  (cdr (assoc mirror *port-mirror-sheet-alist* 
	      :test #'equal)))

(defun (setf mirror->sheet) (sheet port mirror)
  (declare (ignore port))
  (push (cons mirror sheet) *port-mirror-sheet-alist*)
  sheet)

(defmethod sheet-native-transformation ((sheet basic-sheet))
  (compose-transformations 
    (sheet-transformation sheet)
    (sheet-native-transformation (sheet-parent sheet))))

(defmethod clim-utils::stream-encapsulates-stream-p (s1 s2) 
  (declare (ignore s1 s2))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; A TOP-LEVEL-SHEET FOR WINDOWS
;;;

(defclass acl-top-level-sheet (top-level-sheet)
  ((min-width :accessor acl-top-min-width :initform nil)
   (min-height :accessor acl-top-min-height :initform nil)
   (accelerator-gestures :initform nil :reader top-level-sheet-accelerator-gestures)
   (sheet-thread :initform nil :accessor clim-internals::sheet-thread)
   (tooltip-control :initform nil :accessor tooltip-control)
   (maximized-p :initform nil :accessor maximized-p)
   (minimized-p :initform nil :accessor minimized-p)))

(defmethod istoplevel ((object t)) nil)
(defmethod istoplevel ((object acl-top-level-sheet)) t)

(defmethod initialize-instance :after ((sheet acl-top-level-sheet) &key background)
  ;; to cause make-instance to accept :background initializer
  (declare (ignore background))
  nil)

(defmethod repaint-sheet :around ((sheet acl-top-level-sheet) region)
  (declare (ignore region) (special silica::*clim-icon-pattern*))
  (when (port sheet)
    (call-next-method)
    (let ((mirror (sheet-direct-mirror sheet))
	  (pattern silica::*clim-icon-pattern*)
	  frame icon)
      (when (and mirror (win:IsIconic mirror))
	(setf frame (pane-frame sheet))
	(setf icon (clim-internals::frame-icon frame))
	(with-sheet-medium (medium sheet)
	  (when icon
	    (destructuring-bind (&key name pixmap clipping-mask) icon
	      (declare (ignore name clipping-mask))
	      (setf pattern pixmap)))
	  (when pattern
	    (clim:draw-pattern* medium pattern 3 3)))
	#+ignore
	(multiple-value-bind (left top right bottom)
	    (mirror-native-edges* *acl-port* sheet)
	  (declare (special *clim-icon*))
	  (let ((dc (GetDC mirror)))
	    (unless (zerop dc)
	      (win:DrawIcon dc 0 0 *clim-icon*)
	      (ReleaseDC mirror dc))))
	))))

(defmethod realize-mirror :around ((port acl-port) 
				   (sheet acl-top-level-sheet))
  ;; added this method so that the default positioning 
  ;; of frames works (cim 10/3/96)
  (let ((mirror (call-next-method)))
    (setf (clim-internals::sheet-thread sheet) (current-process))
    (with-slots ((uspp silica::user-specified-position-p)) sheet
      (when (eq uspp :unspecified)
	(multiple-value-bind (left top)
	    (mirror-region* port sheet)
	  (move-sheet sheet left top))))
    mirror))

(defmethod note-sheet-tree-grafted ((port acl-port) 
				    (sheet acl-top-level-sheet))
  ;; This method is invoked when the sheet and its descendents have
  ;; been mirrored
  (let* ((frame (pane-frame sheet))
	 (native (getf (frame-properties frame) :native-menu))
	 (command-table (frame-command-table frame)))
    (when (and native command-table)
      (compute-msmenu-bar-pane frame sheet command-table))))

(defmethod mirror-region* ((port acl-port) (sheet acl-top-level-sheet))
  ;; Get the "inside" size of a top-level sheet.
  (let ((wrect (ct:ccallocate win:rect))
	(topleft (ct:ccallocate win:point))
	(botright (ct:ccallocate win:point))
	(handle (sheet-direct-mirror sheet)))
    ;;mm: Use the CLient values and map to screen coordinates
    (win:GetClientRect handle wrect)
    (setf (ct:cref win:point topleft win::x) (ct:cref win:rect wrect win::left))
    (setf (ct:cref win:point topleft win::y) (ct:cref win:rect wrect win::top))
    (setf (ct:cref win:point botright win::x) (ct:cref win:rect wrect win::right))
    (setf (ct:cref win:point botright win::y) (ct:cref win:rect wrect win::bottom))
    (win:ClientToScreen handle topleft)
    (win:ClientToScreen handle botright)
    (let ((wleft (ct:cref win:point topleft win::x))
	  (wtop (ct:cref win:point topleft win::y))
	  (wright (ct:cref win:point botright win::x)) 
	  (wbottom (ct:cref win:point botright win::y)))      
      (values (coordinate wleft) (coordinate wtop)
	      (coordinate wright) (coordinate wbottom)))))

(defun get-nonclient-deltas (sheet)
  "Calculates the difference between the window edges and the 'client' area"
  ;; Gives you sizes of title bar and window handles.
  (let ((wrect (ct:ccallocate win:rect))
	(crect (ct:ccallocate win:rect))
	(topleft (ct:ccallocate win:point))
	(botright (ct:ccallocate win:point))
        (handle (sheet-direct-mirror sheet)))
    (unless (and handle (win:IsWindow handle))
      ;; No need to plunge ahead in this case...
      (error "Sheet no longer has a valid window handle: ~S" handle))
    (win:GetWindowRect handle wrect)	; in screen coordinates
    (win:GetClientRect handle crect)	; 0 0 width height
    (setf (ct:cref win:point topleft win::x) (ct:cref win:rect crect win::left))
    (setf (ct:cref win:point topleft win::y) (ct:cref win:rect crect win::top))
    (setf (ct:cref win:point botright win::x) (ct:cref win:rect crect win::right))
    (setf (ct:cref win:point botright win::y) (ct:cref win:rect crect win::bottom))
    (win:ClientToScreen handle topleft)
    (win:ClientToScreen handle botright)

    (let ((ctop (ct:cref win:point topleft win::y)) 
	  (cleft (ct:cref win:point topleft win::x))
          (cright (ct:cref win:point botright win::x)) 
	  (cbottom (ct:cref win:point botright win::y)) 
	  (wleft (ct:cref win:rect wrect win::left))
          (wtop (ct:cref win:rect wrect win::top))
	  (wright (ct:cref win:rect wrect win::right)) 
	  (wbottom (ct:cref win:rect wrect win::bottom)))
      (values 
       (- wleft cleft) (- wtop ctop)
       (- (- wright wleft) (- cright cleft))
       (- (- wbottom wtop) (- cbottom ctop))
       ))))

(defmethod set-sheet-mirror-edges* ((port acl-port)
				    (sheet acl-top-level-sheet)
				    left top right bottom)
  ;; Set the "inside" size of a top-level sheet.
  ;; Top-level sheet has to account for sizes of window decorations.
  (fix-coordinates left top right bottom)
  (multiple-value-bind (wleft wtop wright wbottom) (mirror-region* port sheet)
    (when (and (= left wleft) (= top wtop)
	       (= right wright) (= bottom wbottom))
      ;; We seem to get infinite recursion if we don't check for this.
      (return-from set-sheet-mirror-edges* t)))
  (multiple-value-bind (dl dt dw dh) (get-nonclient-deltas sheet)
    ;;mm: map the client coordinates to frame coordinates
    (let* ((winleft   (+ left dl))
	   (wintop    (+ top dt))
	   (winwidth  (+ (- right left) dw))
	   (winheight (+ (- bottom top) dh)))
      (win:SetWindowPos (sheet-mirror sheet) 0
			winleft	wintop winwidth	winheight
			(logior win:SWP_NOACTIVATE
				win:SWP_NOZORDER)))))

(defmethod silica::sheet-flags ((sheet acl-top-level-sheet))
  `(,@(when (maximized-p sheet) (list :maximized))
      ,@(when (minimized-p sheet) (list :minimized))))

(defmethod (setf silica::sheet-flags) (new-flags (sheet acl-top-level-sheet))
  (let ((mirror (sheet-mirror sheet)))
   (cond
     ((member :maximized new-flags)
      (win:ShowWindow mirror win:SW_MAXIMIZE))
     ((member :minimized new-flags)
      (win:ShowWindow mirror win:SW_MINIMIZE)))))

(defmethod resize-sheet :around ((sheet acl-top-level-sheet) width height)
  "Restrict sheet sizes to the actual available client area."
  (multiple-value-bind (-left -top dw dh)
      (get-nonclient-deltas sheet)
    (declare (ignore -left -top))
    (let* ((graft-br (sheet-region (graft sheet)))
           (gw (bounding-rectangle-width graft-br))
           (gh (bounding-rectangle-height graft-br))) 
      (call-next-method sheet (min (- gw dw) width) (min (- gh dh) height)))))

