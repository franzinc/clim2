;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: acl-mirror.lisp,v 1.7 1998/10/08 18:36:21 layer Exp $

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

(defvar *screen* nil)

;;--- This isn't really right, all ports only have kludges for this
(defmethod sheet-shell (sheet) sheet)

(defmethod realize-graft ((port acl-port) graft)
  (let ((handle (slot-value *screen-device* 'device-handle1)))
   (setq *screen* handle)
   (init-cursors)
   (with-slots (silica::pixels-per-point
	        silica::pixel-width
	        silica::pixel-height
	        silica::mm-width
	        silica::mm-height
	        silica::units) graft
   (with-dc (*screen* dc) 
    (let ((screen-width (win:GetSystemMetrics win:SM_CXSCREEN)) 
	  (screen-height (win:GetSystemMetrics win:SM_CYSCREEN))
	  (logpixelsx (win:getDeviceCaps dc win:logpixelsx))
	  (logpixelsy (win:getDeviceCaps dc win:logpixelsy)))
	(setf silica::pixel-width  screen-width
	      silica::pixel-height screen-height
	      silica::mm-width     (round (* screen-width 25.4s0) logpixelsx)
	      silica::mm-height	   (round (* screen-height 25.4s0) logpixelsy))
	(let* ((ppp (/ logpixelsy 72s0))
	       (rounded-ppp (round ppp)))
	  (setf silica::pixels-per-point 
		(if (< (abs (- ppp rounded-ppp)) .1s0) rounded-ppp ppp)))
	(setf (sheet-region graft)
	      (ecase silica::units
		((:device :pixel)
		 (make-bounding-rectangle
		   0 0 
		   silica::pixel-width silica::pixel-height))))
	(setf (sheet-native-transformation graft) +identity-transformation+)
	(setf (sheet-direct-mirror graft) handle)
	(update-mirror-transformation port graft))))))

(defmethod modal-frame-p ((frame t)) nil)
(defmethod modal-frame-p ((frame clim-internals::accept-values-own-window)) t)
(defmethod modal-frame-p ((frame clim-internals::menu-frame)) t)

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
           (buttonstyle nil)
           (editstyle nil)
	   (value nil)
	   (items nil)
	   (width (- right left))
	   (height (- bottom top))
	   gadget-id)
      ;;mm: defined in acl-widg.lsp later
      (declare (special silica::*hbutton-width* silica::*hbutton-height*))
      (assert (eq parent parent2) () "parents don't match!")
      ;; Wouldn't this COND work better as a pile of methods, or at least a
      ;; typecase? JPM 14Aug97
      (cond ((typep sheet 'silica::scroller-pane)
	     (setq msscrollwin t)
	     (setq scroll (and (not (scroller-pane-gadget-supplies-scrolling-p sheet))
			       (silica::scroller-pane-scroll-bar-policy sheet))))
	    ((typep sheet 'silica::hlist-pane)
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
            ((typep sheet 'silica::mswin-combo-box-pane)
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
			 :test (set-gadget-test sheet))))
	    ((typep sheet 'silica::hpbutton-pane)
	     
	     (setq control :hbutt)
	     ;;mm: allocate gadget-id per parent
             (setq gadget-id (silica::allocate-gadget-id sheet))
	     (setf buttonstyle
	       (if (push-button-show-as-default sheet)
			     win:BS_DEFPUSHBUTTON
			     win:BS_PUSHBUTTON)))
	    ((typep sheet 'silica::mswin-text-edit)
	     (setq control :hedit)
	     ;;mm: allocate gadget-id per parent
             (setq gadget-id (silica::allocate-gadget-id sheet))
	     (setq value (slot-value sheet 'silica::value))
	     (setf editstyle
	       (logior win:ES_AUTOHSCROLL win:ES_LEFT win:WS_BORDER))
	     (unless (typep sheet 'silica::mswin-text-field)
               (setf editstyle 
		 (logior editstyle win:ES_MULTILINE win:ES_AUTOVSCROLL))))
	    ((typep sheet 'silica::hbutton-pane)
	     (setq control :hbutt)
	     ;;mm: allocate gadget-id per parent
             (setq gadget-id (silica::allocate-gadget-id sheet))
	     (setq value (slot-value sheet 'silica::value))
	     (setq buttonstyle
	       (ecase (gadget-indicator-type sheet)
		 (:one-of win:BS_RADIOBUTTON)
		 (:some-of win:BS_CHECKBOX)))))
      (when (or (eq control :hbutt) (eq control :hedit))
	(multiple-value-bind (cwidth cheight)
              (compute-gadget-label-size sheet)
	  (setq top (+ top (* gadget-id 25)))
	  (setq left (+ left 50))
          (setq width (+ cwidth (* 2 silica::*hbutton-width*)))
	  (setq height (max cheight (* 1 silica::*hbutton-height*)))))
      (setq window
	(cond ((eq control :hbutt)
	       (setq childwin t)
	       (let ((label (slot-value sheet 'silica::label)))
		 (typecase label
		   ((or acl-pixmap pattern)
		    (setf (slot-value sheet 'silica::pixmap)
		      (if (typep label 'pattern)
			  (with-sheet-medium (medium sheet)
			    (with-output-to-pixmap 
				(stream medium
					:width (pattern-width label)
					:height (pattern-height label))
			      (draw-pattern* stream label 0 0)))
			label))
		    (setq buttonstyle win:BS_OWNERDRAW ;; pnc Aug97 for clim2bug740
			  label nil)))
		 (hbutton-open parent gadget-id
				   left top width height 
				   :buttonstyle buttonstyle
				   :value value
				   :label label)))
	      ((eq control :hedit)
	       (setq childwin t)
	       (hedit-open parent gadget-id
			       left top width height 
			       :editstyle editstyle
			       :value value
			       :scroll-mode 
			       (let ((p (sheet-parent sheet)))
				 (and (typep p 'silica::scroller-pane)
				      (silica::scroller-pane-scroll-bar-policy p)))
			       )
	       )
	      ((eq control :hlist)
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
				 (and (typep p 'silica::scroller-pane)
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
	      ((not (eql parent *screen*))
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
		win:cw_usedefault
		win:cw_usedefault
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
        (setq *current-window* window)
	(unless *dc-initialized* (initialize-dc))
	(with-dc (window dc)
          (with-slots (logpixelsy) port
            (setf logpixelsy (win:getDeviceCaps dc win:logpixelsy)))))

      ;; added (cim 10/11/96)
      (when control
	(let ((text-style (pane-text-style sheet)))
	  (when text-style
	    (let ((font (text-style-mapping port text-style)))
	      (win:sendmessage window win:wm_setfont 
			       (acl-font-index font) 0)))))
      (sheet-region sheet) ;;+++ debug only
      (when (and childwin (sheet-enabled-p sheet))
	(win:showWindow window win:sw_show))
      window)))

;; added this method so that the default positioning of frames works
;; properly (cim 10/3/96)

(defmethod realize-mirror :around ((port acl-port) (sheet top-level-sheet))
  (let ((mirror (call-next-method)))
    (setf (clim-internals::sheet-thread sheet) (current-process))
    (with-slots ((uspp silica::user-specified-position-p)) sheet
      (when (eq uspp :unspecified)
	(multiple-value-bind (left top)
	    (mirror-region* port sheet)
	  (move-sheet sheet left top))))
    mirror))

(defmethod realize-mirror :around ((port acl-port) (sheet basic-gadget))
  (let ((window (call-next-method))
	(false 0))
    (unless (gadget-active-p sheet)
      (win:enablewindow window false)
      (check-last-error "EnableWindow" :action :warn))
    window))

(defmethod destroy-mirror ((port acl-port) sheet)
  ;; "A WIN32 thread cannot use DestroyWindow to destroy a window
  ;; created by a different thread."
  ;;
  ;; Called by note-sheet-degrafted, which is
  ;; called by (setf port) which is called by sheet-disown-child.
  ;; This is used when the frame layout changes or by disown-frame,
  ;; among other times.
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror
      (win:destroyWindow mirror))))

(defmethod note-sheet-tree-grafted ((port acl-port) (sheet top-level-sheet))
  ;; This method is invoked when the sheet and its descendents have
  ;; been mirrored
  (let* ((frame (pane-frame sheet))
	 (native (getf (frame-properties frame) :native-menu))
	 (command-table (frame-command-table frame)))
    (when (and native command-table)
      (acl-clim::compute-msmenu-bar-pane frame sheet command-table))))

(defvar *in-layout-avp* nil)

(defmethod enable-mirror ((port acl-port) sheet)
  (let ((window (sheet-mirror sheet)))
    (unless *in-layout-avp*
      (win:showWindow window win:sw_show) ; returns NIL if it was already visible.
      (or (win:updateWindow window)	; send a WM_PAINT message
	  (check-last-error "UpdateWindow")))))

(defmethod disable-mirror ((port acl-port) sheet)
  (win:showWindow (sheet-mirror sheet) win:sw_hide))

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
  (win:setWindowPos (sheet-mirror sheet) 
		    win:HWND_BOTTOM
		    0 0 0 0		; x y width height
		    (logior win:swp_noactivate
			    win:swp_nomove ; ignore x y
			    win:swp_nosize ; ignore width height
			    )))

(defmethod mirror-visible-p ((port acl-port) sheet)
  (win:isWindowVisible (sheet-mirror sheet)))

;; specialized Returns left,top,right,bottom
(defmethod mirror-region* ((port acl-port) (sheet silica::top-level-sheet))
  (let ((wrect (ct:ccallocate win:rect))
	(topleft (ct:ccallocate win:point))
	(botright (ct:ccallocate win:point))
	(handle (sheet-direct-mirror sheet))
	)
    ;;mm: Use the CLient values and map to screen coordinates
    (win:getClientRect handle wrect)
    (setf (ct:cref win:point topleft win::x) (ct:cref win:rect wrect win::left))
    (setf (ct:cref win:point topleft win::y) (ct:cref win:rect wrect win::top))
    (setf (ct:cref win:point botright win::x) (ct:cref win:rect wrect win::right))
    (setf (ct:cref win:point botright win::y) (ct:cref win:rect wrect win::bottom))
    (win:clientToScreen handle topleft)
    (win:clientToScreen handle botright)

    (let ((wleft (ct:cref win:point topleft win::x))
	  (wtop (ct:cref win:point topleft win::y))
	  (wright (ct:cref win:point botright win::x)) 
	  (wbottom (ct:cref win:point botright win::y)))      
      (values (coordinate wleft) (coordinate wtop)
	      (coordinate wright) (coordinate wbottom)))))


;;; unspecialized (not top)
(defmethod mirror-region* ((port acl-port) sheet)
  (let ((mirror (sheet-mirror sheet))
        (target (sheet-mirror (sheet-parent sheet))))
    (mirror-client-region-internal* port mirror target)))


;; Returns x,y,width,height
(defmethod mirror-inside-region* ((port acl-port) sheet)
  (let ((crect (ct:ccallocate win:rect)))
    (win:getClientRect (sheet-mirror sheet) crect) ; 0 0 width height
    (let ((cwidth (ct:cref win:rect crect win::right)) 
	  (cheight (ct:cref win:rect crect win::bottom)))      
      (values (coordinate 0) (coordinate 0)
	      (coordinate cwidth)
	      (coordinate cheight)))))

;;--- Shouldn't this be the same as MIRROR-REGION*?
(defmethod mirror-native-edges* ((port acl-port) sheet)
  (let ((wrect (ct:ccallocate win:rect)))
    (win:getWindowRect (sheet-direct-mirror sheet) wrect)
    (let ((wleft (ct:cref win:rect wrect win::left))
          (wtop (ct:cref win:rect wrect win::top))
	  (wright (ct:cref win:rect wrect win::right)) 
	  (wbottom (ct:cref win:rect wrect win::bottom)))      
      (values (coordinate wleft) (coordinate wtop)
	      (coordinate wright) (coordinate wbottom)))))

(defmethod mirror-inside-edges* ((port acl-port) sheet)
  (let ((crect (ct:ccallocate win:rect)))
    (win:getClientRect (sheet-mirror sheet) crect) ; 0 0 width height
    (let ((cwidth (ct:cref win:rect crect win::right)) 
	  (cheight (ct:cref win:rect crect win::bottom)))      
      (values (coordinate 0) (coordinate 0)
	      (coordinate cwidth)
	      (coordinate cheight)))))

;;mm: compute the offset and size deltas between frame and client area.
(defun get-nonclient-deltas (sheet)
  (let ((wrect (ct:ccallocate win:rect))
	(crect (ct:ccallocate win:rect))
	(topleft (ct:ccallocate win:point))
	(botright (ct:ccallocate win:point))
        (handle (sheet-direct-mirror sheet))
        )
    (unless (and handle (win:iswindow handle))
      ;; No need to plunge ahead in this case...
      (error "Sheet no longer has a valid window handle: ~S" handle))
    (win:getWindowRect handle wrect)
    (win:getClientRect handle crect)	; 0 0 width height
    (setf (ct:cref win:point topleft win::x) (ct:cref win:rect crect win::left))
    (setf (ct:cref win:point topleft win::y) (ct:cref win:rect crect win::top))
    (setf (ct:cref win:point botright win::x) (ct:cref win:rect crect win::right))
    (setf (ct:cref win:point botright win::y) (ct:cref win:rect crect win::bottom))
    (win:clientToScreen handle topleft)
    (win::clientToScreen handle botright)

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


(defun clim-internals::bounding-rectangle-inside-size (top-sheet)
  (let ((crect (ct:ccallocate win:rect)))
    (win:getClientRect (sheet-mirror top-sheet) crect) ; 0 0 width height
    (let ((cwidth (ct:cref win:rect crect win::right)) 
	  (cheight (ct:cref win:rect crect win::bottom)))      
      (values (coordinate cwidth)
	      (coordinate cheight)))))

(defmethod mirror-client-region-internal* ((port acl-port) mirror target)
  (let ((wrect (ct:ccallocate win:rect))
	(topleft (ct:ccallocate win:point))
	(botright (ct:ccallocate win:point))
	)
    (win:getWindowRect mirror wrect)
    (setf (ct:cref win::point topleft win::x) (ct:cref win:rect wrect win::left))
    (setf (ct:cref win:point topleft win::y) (ct:cref win:rect wrect win::top))
    (setf (ct:cref win:point botright win::x) (ct:cref win:rect wrect win::right))
    (setf (ct:cref win:point botright win::y) (ct:cref win:rect wrect win::bottom))
    (win:screenToClient target topleft)
    (win:screenToClient target botright)
    (let ((wleft (ct:cref win:point topleft win::x))
	  (wtop (ct:cref win:point topleft win::y))
	  (wright (ct:cref win:point botright win::x)) 
	  (wbottom (ct:cref win:point botright win::y)))      
      (values (coordinate wleft) (coordinate wtop)
	      (coordinate wright) (coordinate wbottom)))))

(defvar *setting-sheet-mirror-edges* nil)

(declaim (special *use-native-menubar*))

;;; specialized
;;; aclpc\acl-mirr
(defmethod set-sheet-mirror-edges* ((port acl-port)
				    (sheet silica::top-level-sheet)
				    left top right bottom)
  (fix-coordinates left top right bottom)
  (multiple-value-bind (dl dt dw dh) (get-nonclient-deltas sheet)
    ;;mm: map the client coordinates to frame coordinates
    (let* ((pldl (+ left dl))
	   (ptdt (+ top dt)))
      ;; rl: +++kludge, why doesn't it know the size of the menu bar
      ;;     in the first pass?, so add 19 for now
      (win:setWindowPos (sheet-mirror sheet)
		      #+aclpc (ct:null-handle win:hwnd) ; we really want win:HWND_TOP
		      #+acl86win32 0
		      pldl
		      ptdt
		      (+ (- right left) dw)
		      (+ (- bottom top) dh
			 ;; kludge added back in -tjm Aug97
			 (if (and *use-native-menubar*
				  ;; is this how to verify that sheet's frame has a
				  ;; menu bar? -tjm
				  (slot-value (slot-value sheet 'silica::frame) 'silica::menu-bar)
				  (zerop (+ (win:GetSystemMetrics win:SM_CYCAPTION)
					    (win:GetSystemMetrics win:SM_CYFRAME)
					    dt)))
			     (win:GetSystemMetrics win:SM_CYMENU) 0)
			 ;; got rid of the menu-bar kludge factor -
			 ;; doesn't seem to be needed anymore
			 ;; (cim 10/3/96)  
			 #+ignore 
			 (if (and (minusp ptdt) (> ptdt -30)) 19 0))
		      (logior
		       win:swp_noactivate
		       win:swp_nozorder)))))

;;; unspecialized (not top)
(defmethod set-sheet-mirror-edges* ((port acl-port) sheet
				    left top right bottom)
  (fix-coordinates left top right bottom)
  (win:setWindowPos (sheet-mirror sheet)
		    (ct:null-handle win:hwnd) ; we really want win:HWND_TOP
		    left top
		    (- right left)
		    (- bottom top)
		    (logior win:swp_noactivate
			    win:swp_nozorder)))

(defmethod set-sheet-mirror-edges* :around ((port acl-port)
					     sheet 
					    left top right bottom)
  (declare (ignore bottom right top left))
  (let ((*setting-sheet-mirror-edges* sheet))
    (call-next-method)))


(in-package :silica)

;;; from silica\mirror.lsp
(defvar *port-mirror-sheet-alist* nil)

(eval-when (compile load eval)
  (proclaim '(notinline mirror->sheet)))

(defun mirror->sheet (port mirror)
  (declare (ignore port))
  (cdr (assoc mirror *port-mirror-sheet-alist* 
	      :test #'equal)))

(defun (setf mirror->sheet) (sheet port mirror)
  (declare (ignore port))
  (push (cons mirror sheet) *port-mirror-sheet-alist*)
  sheet)

;;; silica\mirror ; ignored before, but we need it with deep mirroring
(defmethod sheet-native-transformation ((sheet basic-sheet))
  (compose-transformations 
    (sheet-transformation sheet)
    (sheet-native-transformation (sheet-parent sheet))))


;;;  problem with interactor pane as first argument
(defmethod clim-utils::stream-encapsulates-stream-p (s1 s2) 
  (declare (ignore s1 s2))
  nil)


;;; silica\medium.lsp  ;;; to make aclpc accept :background initializer


(defmethod initialize-instance :after ((sheet top-level-sheet) &key background)
  (declare (ignore background))
  nil)

(declaim (special *clim-icon-pattern*))

(defmethod repaint-sheet :around ((sheet top-level-sheet) region)
  (declare (ignore region))
  (when (port sheet)
    (call-next-method)
    (when (typep sheet 'top-level-sheet)
      (let ((mirror (sheet-direct-mirror sheet))
	    (pattern *clim-icon-pattern*)
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
	      (mirror-native-edges* acl-clim::*acl-port* sheet)
	    (let ((dc (GetDc mirror)))
	      (win:DrawIcon dc 0 0 *clim-icon*)
	      (win:releaseDC mirror dc)))
	  )))))
