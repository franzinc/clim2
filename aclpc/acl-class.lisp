;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements the lowest levels of Windows support, registering    *
*  the window class, creating windows, defining the message loop.            *
*                                                                            *
*                                                                            *
****************************************************************************|#

(in-package :acl-clim)

(defvar *acl-port* nil)

;; this will grow indefinitely as items are never removed (cim 9/10/96)
(defvar *popup-menu->command-table* (make-hash-table))

(defstruct modstate
  (numlock nil)
  (shift nil)
  (control nil)
  (meta nil))

(defvar *maybe-format* nil)

(defun mformat (&rest args)
  (if *maybe-format* (apply #'format args)))


(defun msg-name (msg)
  (let ((x (and (< msg 4096) ;; pr Aug97
		(svref (msg-names *acl-port*) msg))))
    (unless (cdr x)
      (setq x (car x)))
    (or x msg)))

(defun modstate->modifier (state)
  (if (modstate-shift state)
    (if (modstate-control state)
      (if (modstate-meta state)
        (make-modifier-state :shift :control :meta)
        (make-modifier-state :shift :control))
      (if (modstate-meta state)
        (make-modifier-state :shift :meta)
        (make-modifier-state :shift)))
    (if (modstate-control state)
      (if (modstate-meta state)
        (make-modifier-state :control :meta)
        (make-modifier-state :control))
      (if (modstate-meta state)
        (make-modifier-state :meta)
        (make-modifier-state)))))

;;;;; window procedure args, rebound in each call to window proc
;;;(defvar *hwnd* 0)

(defun init-cursors ()
  ;; put back old cursor mechanism for the moment (cim 9/13/96)
  (setf (arrow-cursor *acl-port*) (win:LoadCursor 0 win:IDC_ARROW))
  (when (zerop (arrow-cursor *acl-port*))
    (check-last-error "LoadCursor" :action :warn))
  ;; this just does the icon now - the cursor stuff is all handled in
  ;; realize-cursor methods in acl-port (cim 9/12/96)
  (setf (application-icon *acl-port*) (win:LoadIcon 0 win:IDI_APPLICATION))
  (when (zerop (application-icon *acl-port*))
    (check-last-error "LoadIcon" :action :warn))
  t)

;;;(defvar *clim-wproc-arg-struct* nil)
;;;(defvar *clim-ctrl-arg-struct* nil)
;;;(defvar *tooltip-relay-struct* nil)

(defun init-clim-win-proc (wproc-address arg-struct)
  (declare (ignore wproc-address arg-struct))
;;;  (setf *clim-wproc-arg-struct* arg-struct)
  ;; [rfe4951]:
  (ff:register-foreign-callable 'clim-wind-proc :reuse :return-value))

(defun init-tooltip-relay (wproc-address arg-struct)
  (declare (ignore wproc-address arg-struct))
;;;  (setf *tooltip-relay-struct* arg-struct)
  ;; [rfe4951]:
  (ff:register-foreign-callable 'tooltip-relay :reuse :return-value))

(defun init-clim-ctrl-proc (wproc-address arg-struct)
  (declare (ignore wproc-address arg-struct))
;;;  (setf *clim-ctrl-arg-struct* arg-struct)
  ;; [rfe4951]:
  (ff:register-foreign-callable 'clim-ctrl-proc :reuse :return-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; callback for the windowproc for all CLIM windows.

(defun set-cursor (sheet cursor)
  (let ((wincursor (realize-cursor (port sheet) cursor)))
    ;; SetCursor doesn't seem to be the right thing.
    ;; Each time the mouse moves, Windows sets the cursor back
    ;; to the default for the class and then sends a WM_SETCURSOR
    ;; message where we get a chance to SetCursor again.  
    (win:SetClassLong (sheet-mirror sheet) 
		      -12		; GCL_HCURSOR
		      wincursor)
    (win:SetCursor wincursor)
    t))

(defmethod sheet-wants-default-pointer ((object t)) nil)

(defmethod maybe-set-cursor (sheet)
  ;; This has a bug that it doesn't call defwindowproc
  ;; when the sheet is a text field.  This should get you the ibeam
  ;; cursor because that's the cursor for that class
  (let* ((cursor (or (port-grab-cursor *acl-port*)
		     (sheet-pointer-cursor sheet)
		     ))
	 (parent (sheet-parent sheet)))
    (cond (cursor
	   (set-cursor sheet cursor))
	  ((sheet-wants-default-pointer sheet) nil)
	  ((and parent (maybe-set-cursor parent)))
	  (t
	   (and (setq cursor (pointer-cursor (port-pointer (port sheet))))
		(set-cursor sheet cursor))))))

(defvar *level* 0)

(defmacro clear-winproc-result (res)
  (declare (ignore res))
  nil)

(defun loword (long)
  (declare (optimize (speed 3) (safety 0)))
  (ldb (byte 16 0) long))

(defun hiword (long)
  (declare (optimize (speed 3) (safety 0)))
  (ldb (byte 16 16) long))

;; Process WM_MOUSEMOVE
(defun onmousemove (window msg wparam lparam)
  (let ((mx (loword lparam))
	(my (hiword lparam))
	(keys wparam)
	(sheet (mirror->sheet *acl-port* window)))
    (declare (ignore keys))
    (setf (win-result *acl-port*)
      (if (or (not sheet) 
	      (note-pointer-motion *acl-port* sheet mx my))
	  (win:DefWindowProc window msg wparam lparam)
	win:FALSE))))

;; Process WM_SETCURSOR
(defun onsetcursor (window msg wparam lparam)
  ;; There is a bug that you get these messages even
  ;; when the cursor is not moving.  JPM 5/98.
  (let* ((hit-code (loword lparam)))
    (cond ((eql hit-code win:HTCLIENT)
	   (setf (win-result *acl-port*) 
	     (if (maybe-set-cursor (mirror->sheet *acl-port* window))
		 win:TRUE win:FALSE)))
	  (t 
	   (setf (pointer-cursor (port-pointer *acl-port*)) :default)
	   ;; If the hit-code is not HTCLIENT, then its not CLIM's problem.
	   ;; If it is in the client area, then DefWindowProc is supposed
	   ;; to (1) send WM_SETCURSOR to the parent window to see if it
	   ;; wants to set the cursor, and if not, (2) sets the cursor
	   ;; to the arrow.
	   (message-default window msg wparam lparam)))))

;; Process WM_PAINT
;; Note: this can be quite excessive when "Show window contents while dragging"
;; is turned on in the Display control panel.  The top-level sheet gets
;; completely repainted every time the mouse moves.
(defun onpaint (window msg wparam lparam)
  (declare (ignore wparam msg lparam))
  (let ((sheet (mirror->sheet *acl-port* window)))
    (let* ((udrect (ff:allocate-fobject 'win:rect :foreign-static-gc nil))
	   (berase 0)
	   (update (win:GetUpdateRect window udrect berase))
	   ;; +++rl added to validate everything because of multiple
	   ;; (continuous) repaints with maximized window
	   (vdrect (ff:allocate-fobject 'win:rect :foreign-static-gc nil)))
      ;; +++rl same as comment above
      (win:GetClientRect window vdrect)
      (if update
	  (let ((ilef (ct:cref win:rect udrect left))
		(itop (ct:cref win:rect udrect top))
		(irig (ct:cref win:rect udrect right))
		(ibot (ct:cref win:rect udrect bottom)))
	    (win:ValidateRect window vdrect) ; needed?
	    #+debug
	    (format *trace-output* "~A ~A ~A ~A ~A~%" 
		    ilef itop irig ibot sheet)
	    (unless (or (= ilef irig) (= itop ibot)) ;does this happen alot?
	      (when sheet
		(handle-event
		 sheet
		 (allocate-event 
		  'window-repaint-event
		  :native-region (sheet-native-region sheet)
		  :region (make-bounding-rectangle ilef itop irig ibot)
		  :sheet sheet))
		)))))))

;; Process WM_DRAWITEM
(defun ondrawitem (window msg wparam lparam)
  (declare (ignore wparam msg window)
           (optimize (speed 3) (safety 0) (debug 0)))
  (macrolet ((lp-slot (name)
               `(ff:fslot-value-typed 'drawitemstruct :c lparam ',name))
             (lp-addr (name)
               `(ff:fslot-address-typed 'drawitemstruct :c lparam ',name)))
   (let* ((hwnd (lp-slot hwnditem))
          (sheet (mirror->sheet *acl-port* hwnd)))
     (when sheet
       (silica::draw-picture-button sheet (lp-slot itemstate) (lp-slot hdc) (lp-addr rcitem))))))

(defmethod isa-pushbutton ((object t)) nil)
(defmethod isa-pushbutton ((object push-button)) t)

;; Process WM_CTLCOLOREDIT
(defun onctlcoloredit (window msg wparam lparam)
  (declare (ignore msg window))
  (let ((hwnd (ct:ccallocate win:hwnd))
	(hdc (ct:ccallocate win:hdc)))
    (setf (ct:handle-value win:hwnd hwnd) lparam
	  (ct:handle-value win:hdc hdc) wparam)
    (let ((sheet (mirror->sheet *acl-port* hwnd)))
      (when sheet
         (when (and (isa-pushbutton sheet)
                    (slot-value sheet 'silica::pixmap))
           (let ((rect (ct:ccallocate win:rect)))
	     (win:GetClientRect hwnd rect)
	     (silica::draw-picture-button sheet 0 hdc rect)))
	 
	 (setf (win-result *acl-port*) (adjust-gadget-colors sheet hdc))))))

;; Process WM_COMMAND.
;; This message is sent when the user selects a command item
;; from a menu, when a control sends a notification message to
;; its parent window, or when an accelerator key is translated.

(defun oncommand (window msg wparam lparam)
  (declare (ignore msg))
  (let* ((port *acl-port*)
	 (parent (mirror->sheet port window))
	 (control (not (zerop lparam))))
    (flush-pointer-motion port)
    (cond ((not control)
	   ;; User selected a command from the menu bar.
	   (let* ((frame (pane-frame parent))
		  (ID (loword wparam))
		  (command (cdr (aref (menu-id->command-table port) ID))))
	     
	     ;;mm bug12977
	     (handler-case
		 ;; set the focus to make sure mswin-text-field instances get updated
		 (win:SetFocus window)
	       (clim-internals::synchronous-command-event
		   (c)
		 (let ((command (clim-internals::synchronous-command-event-command c)))
		   ;; must do the command right now to do commands in
		   ;; the right order
		   (execute-command-in-frame frame command))))
	     
	     (when command
	       (with-slots (clim-internals::disabled-commands) frame
		 (if (member (car command) clim-internals::disabled-commands)
		     (win:MessageBeep 200)
		   (execute-command-in-frame frame command))))))
	  (parent
	   ;; User clicked on a control.
	   (let ((gadget (mirror->sheet port lparam)))
	     (when gadget
	       (command-event gadget port parent wparam lparam)))))
    (clear-winproc-result (win-result *acl-port*))
        0))


(defmethod command-event ((gadget t) port sheet wparam lparam)
  "Gadget just issued a WM_COMMAND event."
  (declare (ignore wparam))
  (let ((pointer (port-pointer port))
	(modifier-state (make-modifier-state)))
    (with-slots (event-queue) port
      (multiple-value-bind (left top right bottom)
	  (mirror-client-region-internal*
	   port lparam (sheet-mirror sheet))
	(declare (ignore right bottom))
	(queue-put event-queue
		   (allocate-event 
		    'silica::window-change-event
		    :native-x (+ left 1)
		    :native-y (+ top 1)
		    :button +pointer-left-button+
		    :modifier-state modifier-state
		    :pointer pointer
		    :sheet sheet ;;gadget
		    :mswin-control gadget))))))

;; Process WM_SYSCOMMAND.
;; This message is sent when the user selects something in
;; the window menu like minimize or restore.
(defun onsyscommand (window msg wparam lparam)
  ;; spr24753
  ;; Call note-frame-iconified and note-frame-deiconified
  ;; rather than simply setting the frame-state to :shrunk
  ;; and :enabled respectively.  (This happens on the
  ;; respective :after methods in silica/framem.lisp.)
  ;; Note that according to the documentation, calling
  ;; these methods also directly iconify/deiconify the
  ;; frame (by calling the functions win:CloseWindow
  ;; and win:OpenIcon --see aclpc/acl-frames.lisp)
  ;; We are depending on the fact that the windows-system
  ;; won't try to re-iconify an already iconified window, etc.
  (case wparam
    (#.win:SC_MINIMIZE			; iconify
     (let* ((sheet (mirror->sheet *acl-port* window))
	    (frame (when sheet (pane-frame sheet))))
       (when frame 
	 (let ((framem (clim::frame-manager frame)))
	   (note-frame-iconified framem frame)) 
	 #+old (when frame (setf (frame-state frame) :shrunk))
	 )))
    (#.win:SC_RESTORE			; deiconify
     (let* ((sheet (mirror->sheet *acl-port* window))
	    (frame (when sheet (pane-frame sheet))))
       (when frame
	 (let ((framem (clim::frame-manager frame)))
	   (note-frame-deiconified framem frame))
	 #+old (when frame (setf (frame-state frame) :enabled))
	 )))
    )
  (message-default window msg wparam lparam))


;;
;; Process WM_VSCROLL
;;

(defun onvscroll (window msg wparam lparam)
  (let* ((scrollcode (loword wparam))
         (position (hiword wparam))
         (hwnd (if (zerop lparam) window lparam)) ; JPM for rfe353
         (message (cond ((eql msg win:WM_HSCROLL) :horizontal)
                        ((eql msg win:WM_VSCROLL) :vertical)))
         (sheet (mirror->sheet *acl-port* hwnd))
         (bar (cond ((gadgetp sheet) win:SB_CTL)
                    ((eql msg win:WM_HSCROLL) win:SB_HORZ)
                    ((eql msg win:WM_VSCROLL) win:SB_VERT)))
         (redraw 1))
    (declare (fixnum position))
    (assert bar)
    ;; By convention, scroll bar values should always have a range of 
    ;; 0 to *win-scroll-grain*.
    (multiple-value-bind (action amount)
        (case scrollcode
          (#.win:SB_BOTTOM
           ;; Scroll to lower right
           (win:SetScrollPos hwnd bar (win-scroll-grain *acl-port*) redraw)
           (values :percentage (win-scroll-grain *acl-port*)))
          ((#.win:SB_LINEDOWN #.win:SB_LINERIGHT)
           ;; Scroll down/right one line
           (move-slug sheet hwnd bar +1 #'silica::scroll-bar-line-increment)
           (values :relative-jump +1))
          ((#.win:SB_LINEUP #.win:SB_LINELEFT)
           ;; Scroll up/left one line
           (move-slug sheet hwnd bar -1 #'silica::scroll-bar-line-increment)
           (values :relative-jump -1))
          ((#.win:SB_PAGEDOWN #.win:SB_PAGERIGHT)
           ;; Scroll down/right by slug size
           (move-slug sheet hwnd bar +1 #'silica::scroll-bar-size)
           (values :screenful +1))
          ((#.win:SB_PAGEUP #.win:SB_PAGELEFT)
           ;; Scroll up/left by slug size
           (move-slug sheet hwnd bar -1 #'silica::scroll-bar-size)
           (values :screenful -1))
          (#.win:SB_THUMBPOSITION
           ;; Scroll to absolute position.
           (when (gadgetp sheet)
             (win:SetScrollPos hwnd bar position redraw))
           (values :percentage position))
          (#.win:SB_THUMBTRACK
           ;; Scroll to absolute position.
           (when (gadgetp sheet)
             (win:SetScrollPos hwnd bar position redraw))
           (values :percentage position))
          (#.win:SB_TOP
           ;; Scroll to upper left
           (when (gadgetp sheet)
             (win:SetScrollPos hwnd bar 0 redraw))
           (values :percentage 0))
          )
      (when (and action sheet)
        (with-slots (event-queue) *acl-port*
          (handle-event sheet
                        (allocate-event 'silica::scrollbar-event
                                        :orientation message
                                        :action action
                                        :dragging-p (= scrollcode #.win:SB_THUMBTRACK)
                                        :amount amount
                                        :sheet sheet)))))
    (clear-winproc-result (win-result *acl-port*))
    (win-result *acl-port*)))

(defun move-slug (scroll-bar hwnd win-code direction delta-function)
  ;; Only move slug for independent scroll bar gadgets.
  ;; Scroller panes already take care of moving the slug.
  ;; (alemmens, 2004-12-24)
  (when (gadgetp scroll-bar)
    (let* ((delta (funcall delta-function scroll-bar))
           (old-position (win:GetScrollPos hwnd win-code))
           (tentative-new-position 
            (+ old-position
              (* direction (silica::convert-scroll-bar-size-out scroll-bar delta))))
           (new-position (max 0 (min (win-scroll-grain *acl-port*) tentative-new-position)))
           (redraw 1))
      (unless (= old-position new-position)
        (win:SetScrollPos hwnd win-code new-position redraw)))))

;;

(defun frame-initialization-time (frame)
  ;; We need to know when the frame is not fully initialized.
  (or (not frame) (not (frame-top-level-sheet frame))))

;; Process WM_MOVE
(defun onmove (window msg wparam lparam)
  (let ((sheet (mirror->sheet *acl-port* window)))
    (if (and sheet
	     (not (win:IsIconic window))
	     (not (frame-initialization-time (pane-frame sheet))))
	(progn
	  (handle-event
	   sheet
	   (allocate-event 'window-configuration-event :sheet sheet))
	  ;; set return value to 0
	  (clear-winproc-result (win-result *acl-port*)))
      (setf (win-result *acl-port*) (win:DefWindowProc window msg wparam lparam)))
    (win-result *acl-port*)))

#| WM_SIZE message constants |#

(defconstant SIZE_MAXIMIZED 2)
(defconstant SIZE_MINIMIZED 1)
(defconstant SIZE_RESTORED  0)

;; Process WM_SIZE
(defun onsize (window msg wparam lparam)
  (let* ((sheet (mirror->sheet *acl-port* window)))
    (if (and sheet
	     (not (win:IsIconic window))
	     (not (frame-initialization-time (pane-frame sheet))))
	(progn
	  (handle-event
	   sheet
	   (allocate-event 'window-configuration-event :sheet sheet))
	  ;; set return value to 0
	  (clear-winproc-result (win-result *acl-port*)))
        (setf (win-result *acl-port*) (win:DefWindowProc window msg wparam lparam)))
    (when (typep sheet 'acl-top-level-sheet)
      (setf (maximized-p sheet) (= wparam SIZE_MAXIMIZED))
      (setf (minimized-p sheet) (= wparam SIZE_MINIMIZED)))
    (win-result *acl-port*)))

;; Process WM_GETMINMAXINFO
(defun ongetminmaxinfo (window msg wparam lparam)
  (let ((sheet (mirror->sheet *acl-port* window)))
    (if (istoplevel sheet)
	(let ((min-width (acl-top-min-width sheet))
	      (min-height (acl-top-min-height sheet)))
	  ;; someone who knows how to use the pc ff interface should get
	  ;; rid of all these memrefs! (cim 10/4/96)
	  (when (and min-width min-height)
	    (setf (sys:memref-int lparam 0 24 :unsigned-long) min-width
		  (sys:memref-int lparam 0 28 :unsigned-long) min-height)
	    (clear-winproc-result (win-result *acl-port*))))
      (setf (win-result *acl-port*) (win:DefWindowProc window msg wparam lparam)))
    (win-result *acl-port*)))

;; PROCESS EN_UPDATE
(defun onupdatetext (window msg wparam lparam)
  ;; An edit control is about to display modified text.
  ;; Resize the edit control, if necessary.
  (declare (ignore msg wparam lparam))
  (let ((sheet (mirror->sheet *acl-port* window)))
    (when sheet
      (with-slots (event-queue) *acl-port*
	;; queue-put event-queue
	;; handle-event
	;; sheet
	;;  (allocate-event 'window-change-event :sheet sheet)
	))
    (clear-winproc-result (win-result *acl-port*))
    (win-result *acl-port*)))

(defun update-acl-port-modifier-state ()
  (let* ((capstate (win:GetKeyState win:VK_CAPITAL))
         (numstate (win:GetKeyState win:VK_NUMLOCK))
         (shiftstate (win:GetKeyState win:VK_SHIFT))
         (controlstate (win:GetKeyState win:VK_CONTROL))
         (metastate (win:GetKeyState win:VK_MENU)))
    (setf (modstate-control (modstate *acl-port*))
          (or (minusp controlstate) (not (zerop (ash controlstate -15)))))
    (setf (modstate-meta (modstate *acl-port*))
          ;; whoops! this was controlstate!?
          (or (minusp metastate) (not (zerop (ash metastate -15)))))
    (setf (modstate-numlock (modstate *acl-port*))
          (and (plusp numstate) (zerop (ash numstate -15))))
    (setf (modstate-shift (modstate *acl-port*))
          (or (and (or (minusp shiftstate) 
                       (not (zerop (ash shiftstate -15))))
                   (zerop capstate))
              (and (not (or (minusp shiftstate) 
                            (not (zerop (ash shiftstate -15)))))
                   (and (plusp capstate) (zerop (ash capstate -15)))))))
  (setf (port-modifier-state *acl-port*)
        (modstate->modifier (modstate *acl-port*))))

;; Process WM_KEYDOWN
;;; SPR25900 --pnc
;;; The port slot modstate is used to specify the alt/meta
;;; key on mouse-gestures.  See the functions onbuttondown
;;; and windows-mask->modifier-state+ (in aclpc/acl-port.lisp).
(defun onkeydown (window msg wparam lparam)
  (flush-pointer-motion *acl-port*)
  (let* ((code wparam)
	 (pass nil)
	 (vk (ldb (byte 8 0) code)))
    (when (or (eql code win:VK_CAPITAL)
	      (eql code win:VK_NUMLOCK)
	      (eql code win:VK_SHIFT)
	      (eql code win:VK_CONTROL)
	      (eql code win:VK_MENU))
      (setq pass t))
    (update-acl-port-modifier-state)
    (if pass
	(progn
	  (setq pass nil)
	  (setf (win-result *acl-port*) (win:DefWindowProc window msg wparam lparam))
	  ;;added this so that port modifier state is always updated, 
	  ;; even on passed characters. -- KR
	  (setf (port-modifier-state *acl-port*)
	    (modstate->modifier (modstate *acl-port*)))
	  (win-result *acl-port*))
      (progn
	(when (and (or (<= #x30 vk #x5a)
		       (<= #xba vk #xc0)
		       (<= #xdb vk #xdf))
		   (modstate-shift (modstate *acl-port*)))
	  (setf code (logior #x100 code)))
	;; NUM LOCK
	(when (and (<= #x60 vk #x69)
		   (not (modstate-numlock (modstate *acl-port*))))
	  (setf code (logior #x100 code)))
	(with-slots (event-queue vk->keysym) *acl-port*
	  (let ((keysym (or (gethash (ldb (byte 9 0) code) vk->keysym)
			    (gethash (ldb (byte 8 0) code) vk->keysym)))
		(char nil)
		(modstate (modstate->modifier (modstate *acl-port*)))
		(sheet (mirror->sheet *acl-port* window)))
	    (when (consp keysym)
	      (setf keysym (first keysym)))
	    (when (characterp keysym)
	      (when (zerop (ldb (byte 2 9) code))
		(setf char keysym))
	      (setf keysym (char->keysym keysym)))
	    (let ((frame (pane-frame sheet))
		  (command nil))
	      (cond ((and (or (eql msg win:WM_KEYDOWN)
			      (eql msg win:WM_SYSKEYDOWN)); Alt keyboard accelerator
			  (setq command 
			    (lookup-accelerator frame keysym modstate)))
		     (execute-command-in-frame frame command))
		    ((and (or (eql msg win:WM_KEYDOWN)
			      (eql msg win:WM_SYSKEYDOWN))
			  (eql keysym :newline)
			  (find-default-gadget frame))
		     (activate-default-gadget frame))
		    (t
		     (setf (port-modifier-state *acl-port*) modstate)
		     (queue-put event-queue
				(allocate-event
				 (cond ((or (eql msg win:WM_KEYDOWN)
					    (eql msg win:WM_SYSKEYDOWN))
					'key-press-event)
				       ((or (eql msg win:WM_KEYUP)
					    (eql msg win:WM_SYSKEYUP))
					'key-release-event))
				 :key-name keysym
				 :character char
				 :modifier-state modstate
				 :sheet sheet))
		     ;;added this so that port modifier state is always 
		     ;; updated, even on passed characters. -- KR
		     (setf (port-modifier-state *acl-port*)
		       (modstate->modifier (modstate *acl-port*)))
		     )))))
	;; set return value to 0
	(clear-winproc-result (win-result *acl-port*))
	(win-result *acl-port*)))))

(defun find-default-gadget (frame)
  ;; Look for a button that is "show-as-default"
  (let* ((gadget nil)
	 (input-context *input-context*)
	 (this-context (first input-context))
	 (context-type (when this-context (input-context-type this-context))))
    ;; If we are not accepting a command, then forget about
    ;; activating the default gadget.  JPM 6/30/98.
    (when (and (consp context-type)
	       (eq (first context-type) 'command-name))
      (flet ((look (s)
	       (when (and (isa-pushbutton s)
			  (push-button-show-as-default s))
		 (setq gadget s))))
	(declare (dynamic-extent #'look))
	(map-over-sheets #'look
			 (frame-top-level-sheet frame))
	gadget))))

(defun activate-default-gadget (frame)
  ;; Look for a button that is "show-as-default"
  ;; and activate it.
  (let ((gadget (find-default-gadget frame)))
    (if gadget
	(with-slots (event-queue) *acl-port*
	  (queue-put event-queue
		     (allocate-event 
		      'silica::window-change-event 
		      :native-x 0
		      :native-y 0
		      :button +pointer-left-button+
		      :modifier-state 0
		      :pointer (port-pointer *acl-port*)
		      :sheet gadget
		      :mswin-control gadget)))
      (clim:beep))))

;; Process WM_LBUTTONDOWN
(defun onbuttondown (window msg wparam lparam)
  ;; added the following so that clicking on a blank area will
  ;; move the focus away from any text-fields and cause their
  ;; value to be correctly updated  - this was copied from the
  ;; handle-event on key-press-event for mswin-text-edit in
  ;; acl-widg (cim 9/17/96)
  (win:SetFocus (win:GetActiveWindow))
  (let* ((pointer (port-pointer *acl-port*))
	 (sheet (mirror->sheet *acl-port* window))
	 ;; SPR25900--
	 ;; Make modifier deal with windows Alt key.
	 (modifier-state (windows-mask->modifier-state+ wparam)))
    (when pointer
      (flush-pointer-motion *acl-port*)
      (setf (port-modifier-state *acl-port*) modifier-state)
      (multiple-value-bind (key button)
	  (cond ((eql msg win:WM_LBUTTONDOWN)
		 (values 'pointer-button-press-event
			 +pointer-left-button+))
		((eql msg win:WM_MBUTTONDOWN)
		 (values 'pointer-button-press-event
			 +pointer-middle-button+))
		((eql msg win:WM_RBUTTONDOWN)
		 (values 'pointer-button-press-event
			 +pointer-right-button+))
		((eql msg win:WM_LBUTTONUP)
		 (values 'pointer-button-release-event
			 +pointer-left-button+))
		((eql msg win:WM_MBUTTONUP)
		 (values 'pointer-button-release-event
			 +pointer-middle-button+))
		((eql msg win:WM_RBUTTONUP)
		 (values 'pointer-button-release-event
			 +pointer-right-button+)))
	(with-slots (event-queue) *acl-port*
	  (queue-put event-queue
		     (allocate-event key
				     :native-x (loword lparam)
				     :native-y (hiword lparam)
				     :button button
				     :modifier-state modifier-state
				     :pointer pointer
				     :sheet sheet))))))
  (clear-winproc-result (win-result *acl-port*))
  (win-result *acl-port*))

;; Process WM_LBUTTONDBLCLK
(defun onbuttondblclk (window msg wparam lparam)
  ;; A double click will actually generate four events:
  ;; down, up, dblclk, up.  This function handles the 
  ;; double click, the other three are handled by OnButtonDown.
  (win:SetFocus (win:GetActiveWindow))
  (let* ((pointer (port-pointer *acl-port*))
	 (sheet (mirror->sheet *acl-port* window))
	 ;; SPR25900--
	 ;; Make modifier deal with windows Alt key.
	 (modifier-state (windows-mask->modifier-state+ wparam t)))
    (when pointer
      (flush-pointer-motion *acl-port*)
      (setf (port-modifier-state *acl-port*) modifier-state)
      (multiple-value-bind (key button)
	  (cond ((eql msg win:WM_LBUTTONDBLCLK)
		 (values 'pointer-button-press-event
			 +pointer-left-button+))
		((eql msg win:WM_MBUTTONDBLCLK)
		 (values 'pointer-button-press-event
			 +pointer-middle-button+))
		((eql msg win:WM_RBUTTONDBLCLK)
		 (values 'pointer-button-press-event
			 +pointer-right-button+)))
	(with-slots (event-queue) *acl-port*
	  (queue-put event-queue
		     (allocate-event key
				     :native-x (loword lparam)
				     :native-y (hiword lparam)
				     :button button
				     :modifier-state modifier-state
				     :pointer pointer
				     :sheet sheet))))))
  (clear-winproc-result (win-result *acl-port*))
  (win-result *acl-port*))

;; Process WM_ACTIVATE
(defun onactivate (window msg wparam lparam)
  ;; A window is being activated or deactivated.
  ;; This message is not sent when interacting with the
  ;; individual controls in an active window.
  ;; If user is activating a window with a mouse click, 
  ;; OnMouseActivate is also called, and ACTIVE will be
  ;; 2 (WA_CLICKACTIVE).
  (let ((sheet (mirror->sheet *acl-port* window))
	(WA_INACTIVE 0)
	(active (loword wparam)))
    (update-acl-port-modifier-state)
    (when (and sheet (not (= active WA_INACTIVE)))
      (setf (acl-port-mirror-with-focus *acl-port*) window))
    (setf (win-result *acl-port*) (win:DefWindowProc window msg wparam lparam))))  

;; Process WM_KILLFOCUS
(defun onkillfocus (window msg wparam lparam)
  (declare (ignore lparam wparam))
  (let* ((sheet (mirror->sheet *acl-port* window))
	 (frame (when sheet (pane-frame sheet)))
	 (menup (when frame (getf (frame-properties frame) :menu-frame))))
    (cond ((not sheet))			; should this happen?
	  ((and frame menup)
	   (let ((menu (slot-value frame 'clim-internals::menu)))
	     (setf (window-visibility menu) nil)))
	  ((eql msg win:WM_CLOSE)
	   (with-slots (event-queue) *acl-port*
	     (queue-put event-queue
			(allocate-event 'silica::window-close-event
					:sheet sheet)))))
    ;; set return value to 0
    (clear-winproc-result (win-result *acl-port*))
    (win-result *acl-port*)))

;; Process WM_INITMENUPOPUP
(defun oninitmenupopup (window msg wparam lparam)
  ;; Sent when a drop-down menu or submenu is about to become active.
  ;; We use it to activate/deactivate menu items and to update
  ;; menus in response to add/remove menu-item-from-command-table.
  (declare (ignore msg))
  (let ((sheet (mirror->sheet *acl-port* window))
	(system-menu (plusp (hiword lparam)))
	(index (loword lparam))
	(menu-handle wparam))
    (unless system-menu
      (update-menu-contents sheet menu-handle index)
      (update-menu-item-sensitivities menu-handle)
      ;; set return value to 0
      (clear-winproc-result (win-result *acl-port*))
      (win-result *acl-port*))))

;; Process WM_NCHITTEST
(defun onnchittest (window msg wparam lparam)
  ;; Called on the object that contains the cursor
  ;; every time the mouse moves.
  (setf (win-result *acl-port*) (win:DefWindowProc window msg wparam lparam)))

;; Allow Windows to provide default message processing.
(defun message-default (window msg wparam lparam)
  (clear-winproc-result (win-result *acl-port*))
  (setf (win-result *acl-port*) (win:DefWindowProc window msg wparam lparam))
  (win-result *acl-port*))

;; Refer to the windows documentation on Tooltip controls.
;; The message stream needs to be relayed to the tooltip 
;; control for it to know when and where to display tool tips.
;; [rfe4951]:
(ff:defun-foreign-callable tooltip-relay ((window win:hwnd)
					  (msg win:uint)
					  (wparam win:wparam)
					  (lparam win:lparam))
  (declare (:convention :stdcall) (:unwind 0)
	   (optimize (safety 0) (speed 3)))
  (case msg
    ((#.win:WM_MOUSEMOVE 
      #.win:WM_LBUTTONDOWN
      #.win:WM_LBUTTONUP
      #.win:WM_RBUTTONDOWN
      #.win:WM_RBUTTONUP)
     (let* ((sheet (mirror->sheet *acl-port* window))
	    (frame (pane-frame sheet))
	    (top (frame-top-level-sheet frame))
	    (tt (tooltip-control top))
	    (ttm_relayevent #.(+ win:WM_USER 7))
	    (relay (wmsg *acl-port*)))
       (when tt
	 (ct:csets win:msg relay
		   lparam lparam
		   wparam wparam
		   message msg
		   hwnd window)
	 (or (plusp (win:SendMessage tt ttm_relayevent 0 relay))
	     (check-last-error "TTM_RELAYEVENT" :Action :warn))))))
  #+optional
  (callnexthookex (next-windows-hook *acl-port*) msg wparam lparam))

;;; Windows messages get printed to the value of this, if
;;; *maybe-format* is non-NIL
(defvar *windows-message-trace-output* excl:*initial-terminal-io*)

;; CLIM-WIND-PROC
;; is the function called by the Windows operating system when
;; a message is to be delivered to a clim window.  Its function
;; address is associated with the "clim" class of windows via
;; the call to RegisterClassEx.  For most messages, clim-wind-proc
;; creates an event object and puts it into the event queue of
;; the target frame.  For a few messages, this function processes
;; the message immediately.  This function is supposed to return
;; a 32-bit "LRESULT" value to the caller.  The nature of the
;; return value depends on the message.
;; [rfe4951]:
(ff:defun-foreign-callable clim-wind-proc ((window win:hwnd)
					   (msg win:uint)
					   (wparam win:wparam)
					   (lparam win:lparam))
  (declare (:convention :stdcall) (:unwind 0)
	   (optimize (safety 0) (speed 3)))
  (let ((result 0)
	(*level* (1+ (the fixnum *level*))))
;;;    (setf *hwnd* window)
    ;; FYI: Spy++ does a better job of tracing messages,
    ;; though it doesn't report everything.
    (mformat *windows-message-trace-output*
	     "~A In clim-wind-proc msg=~a sheet=~s lparam=~a~%"
	     *level*
	     (msg-name msg) 
	     window
	     lparam)
    (when (> *level* 40)
      (warn "clim-wind-proc: too deep!"))
    (case msg
      (#.win:WM_MOUSEMOVE
       (onmousemove window msg wparam lparam))
      (#.win:WM_SETCURSOR
       (onsetcursor window msg wparam lparam))
      (#.win:WM_PAINT
       (onpaint window msg wparam lparam))
      (#.win:WM_DRAWITEM
       (ondrawitem window msg wparam lparam))
      ((#.win:WM_CTLCOLOREDIT
	#.win:WM_CTLCOLORLISTBOX
	#.win:WM_CTLCOLORBTN
	;; couldn't get the colors to change for the following
	;; wm_ctlcolorxx messages -  so we're not using them for the
	;; moment (cim 10/11/96)
	;; #.win:wm_ctlcolormsgbox
	;; #.win:wm_ctlcolordlg
	;; #.win:wm_ctlcolorscrollbar
	;; #.win:wm_ctlcolorstatic
	)
       (onctlcoloredit window msg wparam lparam))
      (#.win:WM_COMMAND
       (oncommand window msg wparam lparam))
      (#.win:WM_SYSCOMMAND
       (onsyscommand window msg wparam lparam))
      ((#.win:WM_HSCROLL #.win:WM_VSCROLL)
       (onvscroll window msg wparam lparam))
      (#.win:WM_MOVE
       (onmove window msg wparam lparam))
      (#.win:WM_SIZE
       (onsize window msg wparam lparam))
      (#.win:WM_GETMINMAXINFO
       (ongetminmaxinfo window msg wparam lparam))
      (#.win:EN_UPDATE
       (onupdatetext window msg wparam lparam))
      ;; character typed
      ((#.win:WM_KEYDOWN 
	#.win:WM_SYSKEYDOWN
	#.win:WM_KEYUP
	#.win:WM_SYSKEYUP)
       (onkeydown window msg wparam lparam))
      ((#.win:WM_LBUTTONDOWN
	#.win:WM_RBUTTONDOWN
	#.win:WM_MBUTTONDOWN
	#.win:WM_LBUTTONUP
	#.win:WM_RBUTTONUP
	#.win:WM_MBUTTONUP)
       (onbuttondown window msg wparam lparam))
      ((#.win:WM_LBUTTONDBLCLK
	#.win:WM_LBUTTONDBLCLK
	#.win:WM_LBUTTONDBLCLK)
       (onbuttondblclk window msg wparam lparam))
      (#.win:WM_ACTIVATE
       (onactivate window msg wparam lparam))
      ((#.win:WM_KILLFOCUS
	#.win:WM_CLOSE)
       (onkillfocus window msg wparam lparam))
      (#.win:WM_INITMENUPOPUP
       (oninitmenupopup window msg wparam lparam))
      (#.win:WM_NCHITTEST
       (onnchittest window msg wparam lparam))
      (#.win:WM_NOTIFY
       (message-default window msg wparam lparam))
      (otherwise
       (message-default window msg wparam lparam)))
    (setf result (win-result *acl-port*))
    (mformat *windows-message-trace-output*
	     "~A Out clim-wind-proc msg=~a sheet=~s result=~s~%"
	       *level*
	       (msg-name msg) 
	       window
	       result)
    result))


;;; [bug13094]   Set this var to nil if this hack seems to be interfering
;;;   with normal dialog accelarator keystrokes.
(defvar *ignore-getdlgcode-in-acl-text-editor-pane* t)   

;;; [spr31884]
(defun win-catch-command (body sheet)
  ;;mm: spr27890 - we need to catch some commands triggered by 
  ;;               Windows events
  ;;    AND prevent throw out of WindowProc
  (unwind-protect

      (handler-case
       (funcall body sheet)

       (clim-internals::synchronous-command-event
	(c)
	(let ((command
	       (clim-internals::synchronous-command-event-command c)))
	  ;; must do the command right now to do commands in
	  ;; the right order
	  (execute-command-in-frame (pane-frame sheet) command))))

    ;; we should not throw out of a WindowProc!
    (return-from win-catch-command nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implements the window proc for the subclassed controls (presently
;;; only the edit control).

;; [rfe4951]
(ff:defun-foreign-callable clim-ctrl-proc ((window win:hwnd)
					   (msg win:uint)
					   (wparam win:wparam)
					   (lparam win:lparam))
  (declare (:convention :stdcall) (:unwind 0))
  (mp:without-scheduling
    (let ((result 0)
          (sheet (mirror->sheet *acl-port* window)))
      (mformat excl:*initial-terminal-io*
               "In clim-ctrl-proc msg=~a sheet=~s~%"
               (msg-name msg) sheet)
      (cond ((and (typecase sheet
                    (silica::mswin-text-field (slot-value sheet 'ignore-wm-char)))
		  (eql msg win:WM_CHAR))
	     (mformat excl:*initial-terminal-io* "~&;;ignoring WM_CHAR~%")
	     (setf (slot-value sheet 'ignore-wm-char) nil)
	     (return-from clim-ctrl-proc 0)))

      (cond
       ;; character typed
       ((or (eql msg win:WM_KEYDOWN)
	    (eql msg win:WM_SYSKEYDOWN)
	    (eql msg win:WM_KEYUP)
	    (eql msg win:WM_SYSKEYUP))
	;; JPM: This ought to be merged with onkeydown().
	(flush-pointer-motion *acl-port*)
	(let* ((code wparam)
	       (pass t)			; !!!
	       (vk (ldb (byte 8 0) code)))
	  (when (or (eql code win:VK_CAPITAL)
		    (eql code win:VK_NUMLOCK)
		    (eql code win:VK_SHIFT)
		    (eql code win:VK_CONTROL)
		    (eql code win:VK_MENU))
	    (setq pass t))
	  (let* ((capstate (win:GetKeyState win:VK_CAPITAL))
		 (numstate (win:GetKeyState win:VK_NUMLOCK))
		 (shiftstate (win:GetKeyState win:VK_SHIFT))
		 (controlstate (win:GetKeyState win:VK_CONTROL))
		 (metastate (win:GetKeyState win:VK_MENU)))
	    (setf (modstate-control (ctlmodstate *acl-port*))
	      (or (minusp controlstate) (not (zerop (ash controlstate -15)))))
	    (setf (modstate-meta (ctlmodstate *acl-port*))
	      (or (minusp metastate) (not (zerop (ash metastate -15)))))
	    (setf (modstate-numlock (ctlmodstate *acl-port*))
	      (and (plusp numstate) (zerop (ash numstate -15))))
	    (setf (modstate-shift (ctlmodstate *acl-port*))
	      (or (and (or (minusp shiftstate) 
			   (not (zerop (ash shiftstate -15))))
		       (zerop capstate))
		  (and (not (or (minusp shiftstate) 
				(not (zerop (ash shiftstate -15)))))
		       (and (plusp capstate) (zerop (ash capstate -15)))))))

	  (when (and (or (<= #x30 vk #x5a)(<= #xba vk #xc0)(<= #xdb vk #xdf))
		     (modstate-shift (ctlmodstate *acl-port*)))
	    (setf code (logior #x100 code)))
	  ;; NUM LOCK
	  (when (and (<= #x60 vk #x69)
		     (not (modstate-numlock (ctlmodstate *acl-port*))))
	    (setf code (logior #x100 code)))
	  (with-slots (event-queue vk->keysym) *acl-port*
	    (let ((keysym (or (gethash (ldb (byte 9 0) code) vk->keysym)
			      (gethash (ldb (byte 8 0) code) vk->keysym)))
		  (char nil)
		  (modstate (modstate->modifier (ctlmodstate *acl-port*)))
		  ;;(sheet (mirror->sheet *acl-port* window))
		  )
	      (when (consp keysym)
		(setf keysym (first keysym)))
	      (when (characterp keysym)
		(when (zerop (ldb (byte 2 9) code))
		  (setf char keysym))
         (setf keysym (char->keysym keysym)))
	      #+ignore
	      (format *standard-output* "keysym=~a char=~a modstate=~a~%"
		      keysym char modstate)
       (if (and (silica::isa-textfield sheet)
                ;;mm: spr27890 look at :newline or :tab ourselves
                (case keysym 
                  (:newline (eql modstate 0))
                  (:tab (or (eql modstate 0) (modstate-shift (ctlmodstate
							      *acl-port*))))))
           (setq pass nil)) ;;; Don't pass along the character.
       (if pass
           (progn
             (setq pass nil)
             (setq 
              ;;mm (win-result *acl-port*) 
              result
               (win:CallWindowProc (std-ctrl-proc-address *acl-port*)
                                   window msg wparam lparam))
             (win-result *acl-port*))
         (let ((sheet (mirror->sheet *acl-port* window)))
           ;; We have decided to handle this character ourselves!
           
           ;;mm: spr27890 
           (when (eql msg win:WM_KEYDOWN)
             (setf (slot-value sheet 'ignore-wm-char) t))
           (win-catch-command
            #'(lambda (sheet)
                (handle-event 
                 sheet
                 (allocate-event
                  (cond ((or (eql msg win:WM_KEYDOWN)
                             (eql msg win:WM_SYSKEYDOWN))
                         'key-press-event)
                        ((or (eql msg win:WM_KEYUP)
                             (eql msg win:WM_SYSKEYUP))
                         'key-release-event))
                  :key-name keysym
                  :character char
                  :modifier-state (setf (port-modifier-state *acl-port*)
                                    modstate)
                  :sheet sheet))) sheet)

		  ;; set return value to 0
		  ;;(clear-winproc-result (win-result *acl-port*))
		  (setq result 0)))))))

       ;; 17-Mar-03  mm [bug13094]      
       ((and *ignore-getdlgcode-in-acl-text-editor-pane*
	     (eql msg win:WM_GETDLGCODE)
	     (typep (mirror->sheet *acl-port* window)
		    'acl-text-editor-pane))
	;; When this message arrives for an acl-text-editor-pane
	;; it seems to be coming because of the IsDialogMessage call.
	;; We return 0 to pervent special (dialog accelerator) handling
	;; for any of the keys that should be translated to WM_CHAR messages.
	(clear-winproc-result (win-result *acl-port*)) 
	(setf (win-result *acl-port*) 0)
	(win-result *acl-port*))

       (t
	;; This is where we let the control do its own thing.  Most
	;; messages come through here.
	;;(clear-winproc-result (win-result *acl-port*))
	(setq ;;(win-result *acl-port*)
	 result
	 (win:CallWindowProc (std-ctrl-proc-address *acl-port*)
					       window msg wparam lparam))
        (win-result *acl-port*)))
      ;;(setf result (win-result *acl-port*))
      result)))


(defun initialize-clim (&optional (mp t))
  (declare (ignore mp))
  (warn "~s deprecated - CLIM is automatically initialized"
	'initialize-clim))

;; CLIM makes one of these, gets the slot value, and then
;; throws the rest of it away.
(defclass windows-screen-device ()
  ((device-handle1 :initarg :device-handle1
		   :initform 0)
   (device-handle2 :initarg :device-handle1
		   :initform  (excl:with-native-string (d "DISPLAY")
			       (win:CreateDC d ct:hnull ct:hnull ct:hnull)))))

(defun initialize-cg ()
  (let* ((dataobj
	  (make-array
	   4 ;; make sure this stays in sync with cl/src/c/dllmain.c
	   :element-type 'signed-nat)))
    (win:GetWinMainArgs dataobj)
    (setf (hinst *acl-port*)         (aref dataobj 0)
	  ;;(hprevinst *acl-port*)   (aref dataobj 1)
	  (lpcmdline *acl-port*)     (aref dataobj 2)
	  (screen-device *acl-port*) (make-instance 'windows-screen-device))))

(defun make-cstructure (type length)
  ;; create and return a region of memory of the
  ;; given length.
  (declare (ignore type))
  (ff:allocate-fobject `(:array :unsigned-char ,length) :foreign-static-gc))

(defun ensure-clim-initialized (*acl-port*)
  (unless (acl-clim-initialized-p *acl-port*)
    (initialize-cg)
    (setf (clim-window-proc-address *acl-port*) 
	  (init-clim-win-proc (if (slot-boundp *acl-port*
					       'clim-window-proc-address)
				  (clim-window-proc-address *acl-port*)
				  nil)
			      (make-cstructure 0 16)))
    (setf (clim-ctrl-proc-address *acl-port*) 
	  (init-clim-ctrl-proc (if (slot-boundp *acl-port*
						'clim-ctrl-proc-address)
				   (clim-ctrl-proc-address *acl-port*)
				   nil)
			       (make-cstructure 0 16)))
    #+not-yet
    (setf (tooltip-relay-address *acl-port*) 
	  (init-tooltip-relay
	   (if (slot-boundp *acl-port*
			    'tooltip-relay-address)
	       (tooltip-relay-address *acl-port*)
	       nil)
	   (make-cstructure 0 16)))
    (setf (clim-initialized-p *acl-port*) t)))

(defun acl-clim::register-window-class (hcursor)
  ;; This is called by initialize-instance of acl-port.
  ;; It creates a (single) Windows window class for all clim windows.
  (unless (wndclass-registered *acl-port*)
    (init-clim-win-proc (clim-window-proc-address *acl-port*)
			(make-cstructure 0 16))
    (let ((class (ff:allocate-fobject 'win:wndclassex 
				      :foreign-static-gc nil))
	  (icon (win:LoadIcon 0 win:IDI_APPLICATION)) ; (get-clim-icon)
	  (reg nil))
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :cbSize)
	(ff:sizeof-fobject 'win:wndclassex))
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :style)
	(logior win:CS_DBLCLKS
		win:CS_BYTEALIGNCLIENT
		win:CS_BYTEALIGNWINDOW))
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :lpfnWndProc)
	    (clim-window-proc-address *acl-port*))
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :cbClsExtra) 
	0)
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :cbWndExtra) 
	0)
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :hInstance) 
	(hinst *acl-port*))
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :hIcon) 
	icon)
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :hCursor)
	hcursor)
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :hbrBackground)
	(1+ win:COLOR_BTNFACE))
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :lpszMenuName) 
	0)
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :lpszClassName) 
	(string-to-foreign (acl-clim::clim-class *acl-port*)))
	  
      (setf (ff:fslot-value-typed 'win:wndclassex 
				  :foreign class :hIconSm) 
	icon)
      (setq reg (win:RegisterClassEx class))
      (when (zerop reg)
	(check-last-error "RegisterClassEx"))
      (setf (wndclass-registered *acl-port*) t)
      (when (tooltip-relay-address *acl-port*)
	(setf (next-windows-hook *acl-port*)
	  (SetWindowsHookEx win:WH_GETMESSAGE 
			    (tooltip-relay-address *acl-port*)
			    0 0))
	(when (zerop (next-windows-hook *acl-port*))
	  (check-last-error "SetWindowsHookEx")))
      t)))

;;; "CreateWindowEx"
;;;(dword lpctstr lpctstr dword int int int int hwnd hmenu handle lpstr)
;;; "CreateWindow"
;;;(      lpctstr lpctstr dword int int int int hwnd hmenu handle lpstr) hwnd 351 %oscall)

(defun create-overlapped-window (parent pretty scroll
				 left top width height menubar 
				 &optional modal)
  ;; Most frames come in here.
  (let ((winstyle 
	 (logior 
	  win:WS_CLIPSIBLINGS
	  win:WS_CAPTION
	  win:WS_SYSMENU
	  win:WS_OVERLAPPED
	  (if (or (eql scroll :both) (eql scroll :vertical))
	      win:WS_VSCROLL 0)
	  (if (or (eql scroll :both) (eql scroll :horizontal))
	      win:WS_HSCROLL 0)
	  win:WS_CLIPCHILDREN
	  ))
	(exstyle
	 (logior
	  win:WS_EX_LEFT
	  win:WS_EX_LTRREADING
	  win:WS_EX_RIGHTSCROLLBAR
	  win:WS_EX_WINDOWEDGE
	  win:WS_EX_CONTROLPARENT	; TAB BTWN CONTROLS
	  ))
        (win-name (acl-clim::win-name *acl-port*))
	(menu
	 (if menubar (win:CreateMenu) 0))
	(window nil))
    (when pretty
      (setq win-name pretty))
    (cond (modal
	   ;; Accepting-values comes in here.  We need
	   ;; thickframe, even though that is not normally
	   ;; used for modal dialog boxes, so that AVV is
	   ;; sizeable.
	   (setq winstyle
	     (logior winstyle
		     win:WS_THICKFRAME
		     win:WS_POPUP
		     (if parent win:DS_MODALFRAME 0)
		     win:DS_3DLOOK
		     win:WS_CLIPCHILDREN))
	   (setq exstyle
	     (logior exstyle
		     win:WS_EX_DLGMODALFRAME
		     )))
	  (t
	   (setq winstyle
	     (logior winstyle 
		     win:WS_THICKFRAME
		     win:WS_MINIMIZEBOX
		     win:WS_MAXIMIZEBOX))))
    (setq window
      (excl:with-native-string (clim-class (acl-clim::clim-class *acl-port*))
	(excl:with-native-string (win-name win-name)
	  (win:CreateWindowEx exstyle
			      clim-class
			      win-name
			      winstyle
			      left top width height
			      (or parent 0)
			      menu
			      (hinst *acl-port*)
			      (win-x *acl-port*) )))) 
    (when (zerop window)
      (or (check-last-error "CreateWindowEx")
	  (error "CreateWindowEx: unknown error")))
    window))

(defun create-pop-up-window (parent pretty scroll left top width height 
			     ovl &optional modal)
  (declare (ignore modal))
  ;; MENU-FRAME comes in here.
  (let* ((overlap (if ovl
		      (logior win:WS_CAPTION win:WS_SYSMENU) ; NOT A MENU
		    (logior win:WS_THICKFRAME win:WS_DLGFRAME) ; its a menu
		    ))
         (winstyle (logior overlap
			   win:WS_POPUP
			   (if (or (eql scroll :both)(eql scroll :vertical))
			       win:WS_VSCROLL 0)
			   (if (or (eql scroll :both)(eql scroll :horizontal))
			       win:WS_HSCROLL 0)
			   win:WS_CLIPCHILDREN
			   ))
         (win-name (acl-clim::win-name *acl-port*)))
    (when pretty
      (setq win-name pretty))
    (let ((window
	   (excl:with-native-string (clim-class (acl-clim::clim-class *acl-port*))
	     (excl:with-native-string (win-name win-name)
	       (win:CreateWindowEx
		(if ovl 0 win:WS_EX_TOOLWINDOW)
		clim-class
		win-name
		winstyle
		left top width height
		parent
		0
		(hinst *acl-port*)
		(win-x *acl-port*))))))
      (when (zerop window)
	(check-last-error "CreateWindowEx"))
      window)))

(defun create-child-window (parent pretty scroll left top width height)
  ;; Application pane comes in here.
  (let ((winstyle (logior win:WS_CLIPCHILDREN
                          win:WS_CHILD
			  (if (member scroll '(t :both :dynamic :vertical))
			    win:WS_VSCROLL 0)
			  (if (member scroll '(t :both :dynamic :horizontal))
			    win:WS_HSCROLL 0)
			  win:WS_CLIPSIBLINGS))
	(exstyle (logior win:WS_EX_LEFT
			 win:WS_EX_LTRREADING
			 win:WS_EX_RIGHTSCROLLBAR
			 win:WS_EX_CONTROLPARENT ; tab btwn controls
			 ;; You get an "edge" if you use (outlining () ...)
			 ;; OR if you have scroll bars.  This one is purely
			 ;; aesthetic but I think it's almost always appropriate.
			 ;; JPM 6/98.
			 (if scroll win:WS_EX_CLIENTEDGE 0)
			 ))
	;; You can ask for a menu bar on a child window,
	;; but Windows will not give you one.  
	(menu 0)
        (win-name (acl-clim::win-name *acl-port*))
	(window nil))
    (when pretty
      (setq win-name pretty))
    (setq window
      (excl:with-native-string (clim-class (acl-clim::clim-class *acl-port*))
	(excl:with-native-string (win-name win-name)
	  (win:CreateWindowEx exstyle
			      clim-class
			      win-name
			      winstyle
			      left top width height
			      parent
			      menu
			      (hinst *acl-port*)
			      (win-x *acl-port*)))))
    (when (zerop window)
      (check-last-error "CreateWindowEx"))
    (if (or (eql scroll :both)(eql scroll :vertical))
      (win:SetScrollRange window win:SB_VERT 0 (win-scroll-grain *acl-port*) 1))
    (if (or (eql scroll :both)(eql scroll :horizontal))
      (win:SetScrollRange window win:SB_HORZ 0 (win-scroll-grain *acl-port*) 1))
    window))

;; wres and wmsg move up in file to remove compile-time warning

(defun wait-for-event ()
  (when (prog1
            (win:PeekMessage (wmsg *acl-port*) 0 0 0
			     (logior win:PM_NOYIELD win:PM_NOREMOVE))
	  (not (and (zerop (hiword (wres *acl-port*)))
		    (zerop (loword (wres *acl-port*))))))
    t))

;; This is the prototypical way to get and process 
;; a windows message from the windows message queue. 
;; Note that TAB will not select the next control in
;; a group of controls if we never call IsDialogMessage.
#+ignore
(defun process-pending-messages (nonblocking hwnd)
  (declare (optimize (speed 3) (safety 0)))
  ;;(when hwnd (assert (win:IsWindow hwnd)))
  (loop
    (let ((gotmessage
	   (if nonblocking
	       (win:PeekMessage (msg *acl-port*) 0 0 0 win:PM_REMOVE) 
	     (win:GetMessage (msg *acl-port*) 0 0 0))))
      (cond ((not gotmessage) (return t))
	    ((and hwnd (win:IsDialogMessage hwnd (msg *acl-port*))))
	    (t 
	     (win:TranslateMessage (msg *acl-port*))
	     (win:DispatchMessage (msg *acl-port*))))
      (when nonblocking (return t))
      )))

;; This code below is identical to the default
;; process-messages-in-interrupt function
;; in /fi/cl/6.2/src/cl/src/code/threads.cl
;; so it is only left here as a comment in case we figure out
;; when and how to call IsDialogMessage correctly.

;;(defvar *pmif* nil)
;;(defvar *last-pmif* 0)
#+ignore
(defun clim-message-interrupt-function (&aux hwnd done)
  (declare (optimize (speed 3) (safety 0)))

  ;; copied from process-pending-messages

  (ff:with-stack-fobject
   (lmsg 'win:msg)
   (prog1
       (loop
	(let ((gotmessage (win:PeekMessage lmsg 0 0 0 win:PM_REMOVE)))
	  (cond ((not gotmessage) (return done))

		;; This looks well intentioned but it does not work.
		;; It does not work in 6.2 either because PeekMessage
		;;  always returns false when this function was invoked.
		;;  The messages had already been removed by the
		;;  process-messages-in-interrupt function.
		;; Calling IsDialogMessage throws the thread into a loop
		;;  of endless GETDLGCODE messages.
		#+ignore
		((and (multiple-value-bind (frame boundp)
			  (mp:symeval-in-process '*application-frame*
						 sys:*current-process*)
			(case boundp
			  ((nil :unbound) (setf hwnd nil))
			  (otherwise
			   (let ((sheet (frame-top-level-sheet frame)))
			     (if sheet
				 (setf hwnd (sheet-mirror sheet))
			       (setf hwnd nil))))))
		      hwnd
		      (win:IsDialogMessage hwnd lmsg)))

		(t  (win:TranslateMessage lmsg)
		    (win:DispatchMessage lmsg)))
	  (setf done t)
	  ))
     (when (numberp user::*pmif*)
       (let* ((now (get-internal-real-time))
	      (delta (- now *last-pmif*)))
	 (when (> delta user::*pmif*)
	   (format excl:*initial-terminal-io* "~&PMIF delta=~S~%" delta))
	 (setf *last-pmif* now)))
     )))

;; This can be activated if the above function is ever revived.
#+ignore
(setf sys::*default-message-interrupt-function*
  #'clim-message-interrupt-function)

;;; Stock objects
(defvar *black-pen*)
(defvar *ltgray-pen*)
(defvar *null-brush*)
(defvar *black-brush*)
(defvar *ltgray-brush*)
(defvar *blank-image*)
(defvar *ltgray-image*)

(defvar *background-brush* nil)

;;; spr25546
;;; Re-initialize a number of state-variables/pointers associated
;;; with the acl-port.
;;; In short, this is a collection of all the defvars and defparameters
;;; defined in the file aclpc/acl-class.lisp.
;;; This is here mainly to be called by (destroy-port :after) on acl-port.
(defun reset-aclpc-clim (&key (destroy-frames t) (destroy-port t))
  
  (when (and destroy-frames acl-clim::*acl-port*)
    (clim:map-over-frames #'(lambda (x) (clim:destroy-frame x))))
  (when (and destroy-port acl-clim::*acl-port*)
    (clim:destroy-port acl-clim::*acl-port*))


  (setq *acl-port* nil
	*maybe-format* nil
	*level* 0
	*windows-message-trace-output* excl:*initial-terminal-io*)
  
;;;  (setq *hwnd* 0)
;;;  (setq *clim-wproc-arg-struct* nil)
;;;  (setq *clim-ctrl-arg-struct* nil)
;;;  (setq *tooltip-relay-struct* nil)
  
  )

(push #'reset-aclpc-clim excl::*restart-actions*)
