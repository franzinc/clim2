;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-
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
;; $Id: acl-class.lisp,v 1.7.8.8 1998/07/20 21:57:15 layer Exp $

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

;; this will grow indefinitely as menu-ids are never removed (cim 9/10/96)
(defvar *menu-id->command-table*
  (make-array 256 :element-type t :adjustable t :fill-pointer 0 :initial-element nil))

;; this will grow indefinitely as items are never removed (cim 9/10/96)
(defvar *popup-menu->menu-item-ids*
    (make-hash-table))

(defstruct modstate
  (numlock nil)
  (shift nil)
  (control nil)
  (meta nil))

(defvar *maybe-format* nil)

(defun mformat (&rest args)
  (if *maybe-format* (apply #'format args)))

(defvar *msg-names* nil)

(defun init-msg-names ()
  (setq *msg-names* (make-array 4096))
  (dolist (x (remove-duplicates
	      (apropos-list 'wm_ (find-package :win))))
    (push x (svref *msg-names* (symbol-value x)))))

(defun msg-name (msg)
  (unless *msg-names* (init-msg-names))
  (let ((x (and (< msg 4096) ;; pr Aug97
		(svref *msg-names* msg))))
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

(defvar *modstate* (make-modstate))
(defvar *ctlmodstate* (make-modstate))
(defvar *win-result* 0)

(defparameter arrow-cursor (ct:callocate win:hcursor))
(defparameter application-icon (ct:callocate win:hicon))

;; window procedure unwinding state
(defvar *window-proc-return-reason* nil)
(defvar *window-proc-return-tag* nil)
(defvar *window-proc-return-value* nil)
(defvar *window-proc-return-other-values* nil)
;; window procedure args, rebound in each call to window proc
(defvar *hwnd* (ct:null-handle hwnd))
(defvar *msg* win:WM_NULL)
(defvar *wparam* 0)
(defvar *lparam* (ct:callocate :long))
;; Window procedure result variable, returned to Windows.
;; Set to 0 initially in each call to window proc and smashed by anyone.
(defvar *window-proc-result* 0)
;; to avoid garbage we reuse window procedure args from queue
(defvar *window-proc-args-queue* nil)

(defun init-cursors ()
  ;; put back old cursor mechanism for the moment (cim 9/13/96)
  (setf arrow-cursor (win:LoadCursor 0 win:IDC_ARROW))
  (when (zerop arrow-cursor)
    (check-last-error "LoadCursor" :action :warn))
  ;; this just does the icon now - the cursor stuff is all handled in
  ;; realize-cursor methods in acl-port (cim 9/12/96)
  (setf application-icon (win:LoadIcon 0 win:IDI_APPLICATION))
  (when (zerop application-icon)
    (check-last-error "LoadIcon" :action :warn))
  t)

;;; Gather up the argument information and invoke the window procedure.
;;; +++ at some point merge this in with the cg mechanism for lisp
;;; windows.
(defun clim-window-proc (x)
  (declare (ignore x))
  ;; always return *window-proc-result* to the kernel as a cpointer
  (let ((result 0))
    (setf result *window-proc-result*)
    ;; return result
    result))

(defvar *clim-wproc-arg-struct* nil)
(defvar *clim-ctrl-arg-struct* nil)

(defstruct (PCCStructure 
	    (:print-function print-pccstructure)
	    (:predicate PCCStructurep))
  (type-tag 0)
  (data-length 0)
  (data-pointer 0)
  )

(defmacro long-ref (p i) 
  `(sys::memref-int ,p 0 
		    (the fixnum (* 4 (the fixnum ,i)))
		    :unsigned-long))

(defmacro signed-long-ref (p i) 
  `(sys::memref-int ,p 0 
		    (the fixnum (* 4 (the fixnum ,i)))
		    :signed-long))

(ff:defun-c-callable wproc-clim-wrapper (hwnd message wparam lparam)
  (let* ((s *clim-wproc-arg-struct*)
	 (d (pccstructure-data-pointer s)))
    (setf (long-ref d 0) hwnd
	  (long-ref d 1) message
	  (long-ref d 2) wparam
	  (signed-long-ref d 3) lparam)
    (clim-window-proc s)
    ))

(defun init-clim-win-proc (wproc-address arg-struct)
  (declare (ignore wproc-address))
  (setf *clim-wproc-arg-struct* arg-struct)
  (ff:register-function 'clim-wind-proc :reuse :return-value))

(defun init-clim-ctrl-proc (wproc-address arg-struct)
  (declare (ignore wproc-address))
  (setf *clim-ctrl-arg-struct* arg-struct)
  (ff:register-function 'clim-ctrl-proc :reuse :return-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; callback for the windowproc for all CLIM windows.

(defun set-cursor (sheet cursor)
  (let ((wincursor (realize-cursor (port sheet) cursor)))
    ;; SetCursor doesn't seem to be the right thing.
    ;; Each time the mouse moves, Windows sets the cursor back
    ;; to the default for the class and then sends a WM_SETCURSOR
    ;; message where we get a chance to SetCursor again.  
    (win:setClassLong (sheet-mirror sheet) 
		      -12		; GCL_HCURSOR
		      wincursor)
    (win:setcursor wincursor)
    t))

(defun maybe-set-cursor (sheet)
  ;; This has a bug that it doesn't call defwindowproc
  ;; when the sheet is a text field.  This should get you the ibeam
  ;; cursor because that's the cursor for that class
  (let* ((cursor (or (port-grab-cursor *acl-port*)
		     (sheet-pointer-cursor sheet)
		     ;;(pointer-cursor (port-pointer *acl-port*))
		     )))
    (if cursor
	(set-cursor sheet cursor)
      (let ((parent (sheet-parent sheet)))
	(or (and parent (maybe-set-cursor parent))
	    (and (setq cursor (pointer-cursor (port-pointer (port sheet))))
		 (set-cursor sheet cursor)))))))

(defvar *level* 0)

(defvar *realtime-scrollbar-tracking* t)
(defvar *win-scroll-grain* 1000)

(defmacro clear-winproc-result (res)
  (declare (ignore res))
  nil)

(defun loword (long)
  (etypecase long
    (integer (logand long #xffff))
    #+broken
    (word-vector (word-vector-ref long 1)))
  )

(defun hiword (long)
  (etypecase long
    (integer (logand #xffff (ash long -16)))
    #+broken
    (word-vector (word-vector-ref long 0)))
  )

;; Process WM_MOUSEMOVE
(defun onmousemove (window msg wparam lparam)
  (let ((mx (loword lparam))
	(my (hiword lparam))
	(keys wparam)
	(sheet (mirror->sheet *acl-port* window)))
    (declare (ignore keys))
    (setq *win-result*
      (if (note-pointer-motion *acl-port* sheet mx my)
	  (win:defwindowproc window msg wparam lparam)
	win:false))))

;; Process WM_SETCURSOR
(defun onsetcursor (window msg wparam lparam)
  ;; There is a bug that you get these messages even
  ;; when the cursor is not moving.  JPM 5/98.
  (let* ((hit-code (loword lparam)))
    (cond ((eql hit-code win:htclient)
	   (maybe-set-cursor (mirror->sheet *acl-port* window))
	   (setq *win-result* win:true))
	  (t 
	   (setf (pointer-cursor (port-pointer *acl-port*)) :default)
	   ;; If the hit-code is not HTCLIENT, then its not CLIM's problem.
	   ;; If it is in the client area, then DefWindowProc is supposed
	   ;; to (1) send WM_SETCURSOR to the parent window to see if it
	   ;; wants to set the cursor, and if not, (2) sets the cursor
	   ;; to the arrow.
	   (message-default window msg wparam lparam)))))

;; Process WM_PAINT
(defun onpaint (window msg wparam lparam)
  (declare (ignore wparam msg lparam))
  (let ((sheet (mirror->sheet *acl-port* window)))
    (let* ((udrect (ct:ccallocate win:rect))
	   (berase 0)
	   (update (win:getUpdateRect window udrect berase))
	   ;; +++rl added to validate everything because of multiple
	   ;; (continuous) repaints with maximized window
	   (vdrect (ct:ccallocate win:rect)))
      ;; +++rl same as comment above
      (win:getClientRect window vdrect)
      (if update
	  (let ((ilef (ct:cref win:rect udrect left))
		(itop (ct:cref win:rect udrect top))
		(irig (ct:cref win:rect udrect right))
		(ibot (ct:cref win:rect udrect bottom)))
	    (win:validateRect window vdrect) ; needed?
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
  (declare (ignore wparam msg window))
  ;; someone who knows how to use the pc ff interface should get
  ;; rid of all these memrefs! (cim 10/4/96)
  (let ((hwnd (ct:ccallocate win:hwnd))
	(hdc (ct:ccallocate win:hdc))
	(state (sys:memref-int lparam 0 16 :unsigned-long))
	(rect (+ lparam 28)))
    (setf (ct:handle-value win:hwnd hwnd)
      (sys:memref-int lparam 0 20 :unsigned-long))
    (setf (ct:handle-value win:hdc hdc)
      (sys:memref-int lparam 0 24 :unsigned-long))
    (let ((sheet (mirror->sheet *acl-port* hwnd)))
      (when sheet
	(silica::draw-picture-button (mirror->sheet *acl-port* hwnd)
				     state hdc rect)))))

;; Process WM_CTLCOLOREDIT
(defun onctlcoloredit (window msg wparam lparam)
  (setq *win-result* (message-default window msg wparam lparam))
  (let ((hwnd (ct:ccallocate win:hwnd))
	(hdc (ct:ccallocate win:hdc)))
    (setf (ct:handle-value win:hwnd hwnd) lparam
	  (ct:handle-value win:hdc hdc) wparam)
    (let ((sheet (mirror->sheet *acl-port* hwnd)))
      (when sheet
         (when (and (typep sheet 'silica::hpbutton-pane)
                    (slot-value sheet 'silica::pixmap))
           (let ((rect (ct:ccallocate win:rect)))
	     (win:getclientrect hwnd rect)
	     (silica::draw-picture-button sheet 0 hdc rect)))
	 
	 (setf *win-result* (adjust-gadget-colors sheet hdc))))))

;; Process WM_COMMAND
(defun oncommand (window msg wparam lparam)
  (declare (ignore msg))
  (let ((wloword (loword wparam))
	(whiword (hiword wparam))
	(sheet (mirror->sheet *acl-port* window))
	(pointer (port-pointer *acl-port*))
	(modifier-state (make-modifier-state))
	(gadget nil)
	(hwnd (ct:ccallocate win:hwnd)))
    ;;mm: defined in acl-mirr.lsp later
    (declare (special *gadget-id->window*))
    (when pointer
      (flush-pointer-motion *acl-port*))
    (cond ((and (zerop lparam)		; menu item
		(zerop whiword))	; otherwise control (or accelerator)
	   (let* ((frame (pane-frame sheet))
		  (command (cdr (aref *menu-id->command-table* wloword))))
	     ;; pr Aug97
	     (with-slots (clim-internals::disabled-commands) frame
	       (if (member (car command) clim-internals::disabled-commands)
		   (win:messagebeep 200)
		 #+old
		 (queue-put (slot-value *acl-port* 'event-queue)
			    (allocate-event 
			     'presentation-event
			     :frame frame
			     :sheet (frame-top-level-sheet frame)
			     :presentation-type
			     `(command :command-table ,command-table)
			     :value command))
		 (execute-command-in-frame 
		  frame command)
		 ))))
	  (t
	   (setf (ct:handle-value win:hwnd hwnd) lparam)
	   ;;mm: for the moment, the following seems superfluous
	   ;;(setf hwndid (silica::gadget-id->window sheet wloword))
	   (setf gadget (mirror->sheet *acl-port* hwnd))
	   (cond ((typep gadget 'silica::mswin-combo-box-pane)
		  (let ((sheet (mirror->sheet *acl-port* window)))
		    (when (and sheet (= whiword win:cbn_closeup))
		      (with-slots (event-queue) *acl-port*
			(queue-put
			 event-queue
			 (multiple-value-bind (left top right bottom)
			     (mirror-client-region-internal* *acl-port* hwnd window)
			   (declare (ignore right bottom))
			   (allocate-event 'silica::window-change-event
					   :native-x (+ left 1)
					   :native-y (+ top 1)
					   :button +pointer-left-button+
					   :modifier-state 0
					   :pointer pointer
					   :sheet sheet
					   :mswin-control gadget)))))))
		 ((typep gadget 'silica::mswin-text-edit)
		  (let ((sheet (mirror->sheet *acl-port* window)))
		    (when (and sheet (= whiword win:en_killfocus))

		      (with-slots (event-queue) *acl-port*
					;handle-event
					;  gadget
			(multiple-value-bind (left top right bottom)
			    (mirror-client-region-internal* *acl-port* hwnd window)
			  (declare (ignore right bottom))
			  (queue-put event-queue
				     (allocate-event 
				      'silica::window-change-event
				      :native-x (+ left 1)
				      :native-y (+ top 1)
				      :button +pointer-left-button+
				      :modifier-state 0
				      :pointer pointer
				      :sheet sheet
				      :mswin-control gadget)))))))
		 ((or (not (typep gadget 'silica::hlist-pane))
		      (= whiword hln_selchange))
		  (when (typep gadget 'silica::hlist-pane)
		    (win:setfocus window))
		  (with-slots (event-queue) *acl-port*

		    (multiple-value-bind (left top right bottom)
			(mirror-client-region-internal* *acl-port* hwnd window)
		      (declare (ignore right bottom))
		      (queue-put event-queue
				 (allocate-event 
				  'silica::window-change-event
				  :native-x (+ left 1)
				  :native-y (+ top 1)
				  :button +pointer-left-button+
				  :modifier-state modifier-state
				  :pointer pointer
				  :sheet sheet
				  :mswin-control gadget))))))))
    (clear-winproc-result *win-result*)
    *win-result*))  

;; Process WM_VSCROLL
(defun onvscroll (window msg wparam lparam)
  (let* ((type (loword wparam))
	 (position (hiword wparam))
	 (hwnd (if (zerop lparam) window lparam)) ; JPM for rfe353
	 (message (cond ((eql msg win:wm_hscroll) :horizontal)
			((eql msg win:wm_vscroll) :vertical)))
	 (flag (cond ((eql msg win:wm_hscroll) win:sb_horz)
		     ((eql msg win:wm_vscroll) win:sb_vert)))
	 (sheet (mirror->sheet *acl-port* hwnd)))
    (declare (fixnum action position))
    (multiple-value-bind (action amount)
	(cond ((eql type win:sb_lineup)
	       (win:setScrollPos window flag
				  (- (win:getScrollPos window flag) 1) 1)
	       (values :relative-jump -1))
	      ((eql type win:sb_linedown)
	       (win:setScrollPos window flag
				  (+ (win:getScrollPos window flag) 1) 1)
	       (values :relative-jump +1))
	      ((eql type win:sb_pageup)
	       (win:setScrollPos window flag
				  (- (win:getScrollPos window flag) 1) 1)
	       (values :screenful -1))
	      ((eql type win:sb_pagedown)
	       (win:setScrollPos window flag
				  (+ (win:getScrollPos window flag) 1) 1)
	       (values :screenful +1))
	      ((eql type win:sb_thumbposition)
	       (win:setScrollPos window flag position 1)
	       (values :percentage position))
	      ((and *realtime-scrollbar-tracking*
		    (eql type win:sb_thumbtrack))
	       (win:setScrollPos window flag position 1)
	       (values :percentage position))
	      ((eql type win:sb_top)
	       (win:setScrollPos window flag 0 1)
	       (values :percentage 0))
	      ((eql type win:sb_bottom)
	       (win:setScrollPos window flag *win-scroll-grain* 1)
	       (values :percentage 100)))
      (when (and action sheet)
	(with-slots (event-queue) *acl-port*
	  ;;(queue-put event-queue
	  (handle-event
	   sheet
	   (allocate-event 'silica::scrollbar-event
			   :orientation message
			   :action action
			   :amount amount
			   :sheet sheet)))))
    (clear-winproc-result *win-result*)
    *win-result*))

(declaim (special *setting-sheet-mirror-edges*))

;; Process WM_MOVE
(defun onmove (window msg wparam lparam)
  (let ((sheet (mirror->sheet *acl-port* window)))
    (if (and sheet
	     (not (eq sheet *setting-sheet-mirror-edges*))
	     (not (win:IsIconic window)))
	(progn
	  (handle-event
	   sheet
	   (allocate-event 'window-configuration-event :sheet sheet))
	  ;; set return value to 0
	  (clear-winproc-result *win-result*))
      (setq *win-result* (win:defwindowproc window msg wparam lparam)))
    *win-result*))  

;; Process WM_GETMINMAXINFO
(defun ongetminmaxinfo (window msg wparam lparam)
  #+aclpc ;; pnc Aug97 for clim2bug740
  (let ((sheet (mirror->sheet *acl-port* window)))
    (if (typep sheet 'acl-top-level-sheet)
	(let ((min-width (acl-top-min-width sheet))
	      (min-height (acl-top-min-height sheet)))
	  ;; someone who knows how to use the pc ff interface should get
	  ;; rid of all these memrefs! (cim 10/4/96)
	  (when (and min-width min-height)
	    (set-measureitemstruct-width-and-height lparam min-width
						    min-height)
	    #+ignore
	    (setf (sys:memref-int lparam 0 24 win:uint) min-width
		  (sys:memref-int lparam 0 28 win:uint) min-height)
	    (clear-winproc-result *win-result*)))
      #+acl86win32
      (setq *win-result* (win:defwindowproc window msg wparam lparam))
      #+aclpc
      (ct:%set-long *win-result* 4 0
		     (win:defwindowproc window msg wparam lparam)))
    *win-result*)
  #-aclpc				; +++ fix this for aclpc +++
  (let ((sheet (mirror->sheet *acl-port* window)))
    (if (typep sheet 'acl-top-level-sheet)
	(let ((min-width (acl-top-min-width sheet))
	      (min-height (acl-top-min-height sheet)))
	  ;; someone who knows how to use the pc ff interface should get
	  ;; rid of all these memrefs! (cim 10/4/96)
	  (when (and min-width min-height)
	    (setf (sys:memref-int lparam 0 24 :unsigned-long) min-width
		  (sys:memref-int lparam 0 28 :unsigned-long) min-height)
	    (clear-winproc-result *win-result*)))
      #+acl86win32
      (setq *win-result* (win:defwindowproc window msg wparam lparam))
      #+aclpc
      (ct:%set-long *win-result* 4 0
		     (win:defwindowproc window msg wparam lparam)))
    *win-result*))

;; PROCESS EN_UPDATE
(defun onupdate (window msg wparam lparam)
  (declare (ignore msg wparam lparam))
  (let ((sheet (mirror->sheet *acl-port* window)))
    (when sheet
      (with-slots (event-queue) *acl-port*
	;; queue-put event-queue
	;; handle-event
	;; sheet
	;;  (allocate-event 'window-change-event :sheet sheet)
	))
    (clear-winproc-result *win-result*)
    *win-result*))

;; Process WM_KEYDOWN
(defun onkeydown (window msg wparam lparam)
  (flush-pointer-motion *acl-port*)
  (let* ((code wparam)
	 (pass nil)
	 (vk (ldb (byte 8 0) code)))
    (when (or (eql code win:vk_capital)
	      (eql code win:vk_numlock)
	      (eql code win:vk_shift)
	      (eql code win:vk_control)
	      (eql code win:vk_menu))
      (setq pass t))
    (let* ((capstate (win:getKeyState win:vk_capital))
	   (numstate (win:getKeyState win:vk_numlock))
	   (shiftstate (win:getKeyState win:vk_shift))
	   (controlstate (win:getKeyState win:vk_control))
	   (metastate (win:getKeyState win:vk_menu)))
      #+debugg
      (format *standard-output* 
	      "caps=~a num=~a shift=~a ctrl=~a meta=~a code=~a~%"
	      capstate numstate shiftstate controlstate metastate code)

      (setf (modstate-control *modstate*)
	(or (minusp controlstate) (not (zerop (ash controlstate -15)))))
      (setf (modstate-meta *modstate*)
					; whoops! this was controlstate!?
	(or (minusp metastate) (not (zerop (ash metastate -15)))))
      (setf (modstate-numlock *modstate*)
	(and (plusp numstate) (zerop (ash numstate -15))))
      (setf (modstate-shift *modstate*)
	(or (and (or (minusp shiftstate) (not (zerop (ash shiftstate -15))))
		 (zerop capstate))
	    (and (not (or (minusp shiftstate) (not (zerop (ash shiftstate -15)))))
		 (and (plusp capstate) (zerop (ash capstate -15)))))))
    (if pass
	(progn
	  (setq pass nil)
	  (setq *win-result* (win:defwindowproc window msg wparam lparam))
	  ;;added this so that port modifier state is always updated, 
	  ;; even on passed characters. -- KR
	  (setf (port-modifier-state *acl-port*)
	    (modstate->modifier *modstate*))
	  *win-result*)
      (progn
	(when (and (or (<= #x30 vk #x5a)
		       (<= #xba vk #xc0)
		       (<= #xdb vk #xdf))
		   (modstate-shift *modstate*))
	  (setf code (logior #x100 code)))
	;; NUM LOCK
	(when (and (<= #x60 vk #x69)
		   (not (modstate-numlock *modstate*)))
	  (setf code (logior #x100 code)))
	(with-slots (event-queue vk->keysym) *acl-port*
	  (let ((keysym (or (gethash (ldb (byte 9 0) code) vk->keysym)
			    (gethash (ldb (byte 8 0) code) vk->keysym)))
		(char nil)
		(modstate (modstate->modifier *modstate*))
		(sheet (mirror->sheet *acl-port* window)))
	    (when (consp keysym)
	      (setf keysym (first keysym)))
	    (when (characterp keysym)
	      (when (zerop (ldb (byte 2 9) code))
		(setf char keysym))
	      (setf keysym (char->keysym keysym)))
	    (let ((frame (pane-frame sheet))
		  (command nil))
	      (cond ((and (eql msg win:wm_keydown)
			  (setq command 
			    (lookup-accelerator frame keysym modstate)))
		     #+old
		     (queue-put 
		      event-queue
		      (allocate-event 'presentation-event
				      :frame frame
				      :sheet (frame-top-level-sheet frame)
				      :presentation-type
				      `(command :command-table ,command-table)
				      :value command))
		     (execute-command-in-frame
		      frame command))
		    ((and (eql msg win:wm_keydown)
			  (eql keysym :newline)
			  (find-default-gadget frame))
		     (activate-default-gadget frame))
		    (t
		     (queue-put event-queue
				(allocate-event
				 (cond ((or (eql msg win:wm_keydown)
					    (eql msg win:wm_syskeydown))
					'key-press-event)
				       ((or (eql msg win:wm_keyup)
					    (eql msg win:wm_syskeyup))
					'key-release-event))
				 :key-name keysym
				 :character char
				 :modifier-state 
				 (setf (port-modifier-state *acl-port*)
				   modstate)
				 :sheet sheet))
		     ;;added this so that port modifier state is always 
		     ;; updated, even on passed characters. -- KR
		     (setf (port-modifier-state *acl-port*)
		       (modstate->modifier *modstate*))
		     )))))
	;; set return value to 0
	(clear-winproc-result *win-result*)
	*win-result*))))

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
	       (when (and (typep s 'push-button)
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

;; Process WM_BUTTONDOWN
(defun onbuttondown (window msg wparam lparam)
  ;; added the following so that clicking on a blank area will
  ;; move the focus away from any text-fields and cause their
  ;; value to be correctly updated  - this was copied from the
  ;; handle-event on key-press-event for mswin-text-edit in
  ;; acl-widg (cim 9/17/96)
  (win:setfocus (win:getactivewindow) #-acl86win32 :static)

  (let ((modifier-state
	 (windows-mask->modifier-state wparam))
	(pointer (port-pointer *acl-port*)))
    (when pointer
      (flush-pointer-motion *acl-port*)
      (setf (port-modifier-state *acl-port*) modifier-state)
      (multiple-value-bind (key button)
	  (cond ((eql msg win:wm_lbuttondown)
		 (values 'pointer-button-press-event
			 +pointer-left-button+))
		((eql msg win:wm_mbuttondown)
		 (values 'pointer-button-press-event
			 +pointer-middle-button+))
		((eql msg win:wm_rbuttondown)
		 (values 'pointer-button-press-event
			 +pointer-right-button+))
		((eql msg win:wm_lbuttonup)
		 (values 'pointer-button-release-event
			 +pointer-left-button+))
		((eql msg win:wm_mbuttonup)
		 (values 'pointer-button-release-event
			 +pointer-middle-button+))
		((eql msg win:wm_rbuttonup)
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
				     :sheet (mirror->sheet *acl-port* window)))))))
  (clear-winproc-result *win-result*)
  *win-result*)  

;; Process WM_ACTIVATE
(defun onactivate (window msg wparam lparam)
  (declare (ignore msg lparam))
  (let ((sheet (mirror->sheet *acl-port* window))
	(flag (loword wparam)))
    (when (and sheet (> flag 0))
      (setf (acl-port-mirror-with-focus *acl-port*) window))
    ;; set return value to 0
    (clear-winproc-result *win-result*)
    *win-result*))  

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
	  ((eql msg win:wm_close)
	   (with-slots (event-queue) *acl-port*
	     (queue-put event-queue
			(allocate-event 'silica::window-close-event
					:sheet sheet)))))
    ;; set return value to 0
    (clear-winproc-result *win-result*)
    *win-result*))

;; Process WM_INITMENUPOPUP
(defun oninitmenupopup (window msg wparam lparam)
  (declare (ignore msg window lparam))
  (update-menu-item-sensitivities wparam)
  *win-result*)

;; Process WM_NCHITTEST
(defun onnchittest (window msg wparam lparam)
  (setq *win-result* (win:defwindowproc window msg wparam lparam)))

;; Allow Windows to provide default message processing.
(defun message-default (window msg wparam lparam)
  (clear-winproc-result *win-result*)
  (setq *win-result* (win:defwindowproc window msg wparam lparam))
  *win-result*)

(defvar *trace-messages* nil)

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
(ff:defun-c-callable clim-wind-proc (window msg wparam lparam)
  (declare (:convention :stdcall) (:unwind 0)
	   (optimize (safety 0) (speed 3)))
  (let ((result 0)
	(*level* (1+ (the fixnum *level*))))
    (setf *hwnd* window)
    ;; FYI: Spy++ does a better job of tracing messages,
    ;; though it doesn't report everything.
    (when *maybe-format*
      (mformat excl:*initial-terminal-io*
	       "~A In clim-wind-proc msg=~a sheet=~s lparam=~a~%"
	       *level*
	       (msg-name msg) 
	       window
	       lparam))
    (when (> *level* 40)
      (break "too deep!"))
    (case msg
      (#.win:wm_mousemove
       (onmousemove window msg wparam lparam))
      (#.win:wm_setcursor
       (onsetcursor window msg wparam lparam))
      (#.win:wm_paint
       (onpaint window msg wparam lparam))
      (#.win:wm_drawitem
       (ondrawitem window msg wparam lparam))
      ((#.win:wm_ctlcoloredit
	#.win:wm_ctlcolorlistbox
	#.win:wm_ctlcolorbtn
	;; couldn't get the colors to change for the following
	;; wm_ctlcolorxx messages -  so we're not using them for the
	;; moment (cim 10/11/96)
	;; #.win:wm_ctlcolormsgbox
	;; #.win:wm_ctlcolordlg
	;; #.win:wm_ctlcolorscrollbar
	;; #.win:wm_ctlcolorstatic
	)
       (onctlcoloredit window msg wparam lparam))
      (#.win:wm_command
       (oncommand window msg wparam lparam))
      ((#.win:wm_hscroll #.win:wm_vscroll)
       (onvscroll window msg wparam lparam))
      ((#.win:wm_move #.win:wm_size)
       (onmove window msg wparam lparam))
      (#.win:wm_getminmaxinfo
       (ongetminmaxinfo window msg wparam lparam))
      (#.win:en_update
       (onupdate window msg wparam lparam))
      ;; character typed
      ((#.win:wm_keydown 
	#.win:wm_syskeydown
	#.win:wm_keyup
	#.win:wm_syskeyup)
       (onkeydown window msg wparam lparam))
      ((#.win:wm_lbuttondown
	#.win:wm_rbuttondown
	#.win:wm_mbuttondown
	#.win:wm_lbuttonup
	#.win:wm_rbuttonup
	#.win:wm_mbuttonup)
       (onbuttondown window msg wparam lparam))
      (#.win:wm_activate
       (onactivate window msg wparam lparam))
      ((#.win:wm_killfocus
	#.win:wm_close)
       (onkillfocus window msg wparam lparam))
      (#.win:wm_initmenupopup
       (oninitmenupopup window msg wparam lparam))
      (#.win:wm_nchittest
       (onnchittest window msg wparam lparam))
      (otherwise
       (message-default window msg wparam lparam)))
    (setf result *win-result*)
    (when *maybe-format*
      (mformat excl:*initial-terminal-io*
	       "~A Out clim-wind-proc msg=~a sheet=~s result=~s~%"
	       *level*
	       (msg-name msg) 
	       window
	       result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implements the window proc for the subclassed controls (presently
;;; only the edit control).

(ff:defun-c-callable clim-ctrl-proc (window msg wparam lparam)
  (declare (:convention :stdcall) (:unwind 0))
  (mp:without-scheduling
    (setf *hwnd* window)
    (mformat excl:*initial-terminal-io*
	     "In clim-ctrl-proc msg=~a sheet=~s~%"
	     (msg-name msg) (mirror->sheet *acl-port* window))
    ;;(setq *args* (list window msg wparam lparam))
    (let ((result 0))
      (cond
       ;; character typed
       ((or (eql msg win:wm_keydown)
	    (eql msg win:wm_syskeydown)
	    (eql msg win:wm_keyup)
	    (eql msg win:wm_syskeyup)
	    (eql msg win:wm_char))
	(flush-pointer-motion *acl-port*)
	(let* ((code wparam)
	       (pass t)			; !!!
	       (vk (ldb (byte 8 0) code)))
	  (when (or (eql code win:vk_capital)
		    (eql code win:vk_numlock)
		    (eql code win:vk_shift)
		    (eql code win:vk_control)
		    (eql code win:vk_menu))
	    (setq pass t))
	  (let* ((capstate (win:getKeyState win:vk_capital))
		 (numstate (win:getKeyState win:vk_numlock))
		 (shiftstate (win:getKeyState win:vk_shift))
		 (controlstate (win:getKeyState win:vk_control))
		 (metastate (win:getKeyState win:vk_menu)))
	    (setf (modstate-control *ctlmodstate*)
	      (or (minusp controlstate) (not (zerop (ash controlstate -15)))))
	    (setf (modstate-meta *ctlmodstate*)
	      (or (minusp metastate) (not (zerop (ash metastate -15)))))
	    (setf (modstate-numlock *ctlmodstate*)
	      (and (plusp numstate) (zerop (ash numstate -15))))
	    (setf (modstate-shift *ctlmodstate*)
	      (or (and (or (minusp shiftstate) (not (zerop (ash shiftstate -15))))
		       (zerop capstate))
		  (and (not (or (minusp shiftstate) (not (zerop (ash shiftstate -15)))))
		       (and (plusp capstate) (zerop (ash capstate -15))))))
	    )

	  (when (and (or (<= #x30 vk #x5a)(<= #xba vk #xc0)(<= #xdb vk #xdf))
		     (modstate-shift *ctlmodstate*))
	    (setf code (logior #x100 code)))
	  ;; NUM LOCK
	  (when (and (<= #x60 vk #x69)
		     (not (modstate-numlock *ctlmodstate*)))
	    (setf code (logior #x100 code)))
	  (with-slots (event-queue vk->keysym) *acl-port*
	    (let ((keysym (or (gethash (ldb (byte 9 0) code) vk->keysym)
			      (gethash (ldb (byte 8 0) code) vk->keysym)))
		  (char nil)
		  (modstate (modstate->modifier *ctlmodstate*))
		  (sheet (mirror->sheet *acl-port* window)))
	      (when (consp keysym)
		(setf keysym (first keysym)))
	      (when (characterp keysym)
		(when (zerop (ldb (byte 2 9) code))
		  (setf char keysym))
		(setf keysym (char->keysym keysym)))
	      #+ignore
	      (format *standard-output* "keysym=~a char=~a modstate=~a~%"
		      keysym char modstate)
	      (if (and (or (eql keysym :end)
			   (and (typep sheet 'silica::mswin-text-field)
				(eql keysym :newline)))
		       (eql modstate 0))
		  (setq pass nil)) ;;; pass along the end character.
	      (if pass
		  (progn
		    (setq pass nil)
		    #+acl86win32
		    (setq *win-result* (win:callwindowproc std-ctrl-proc-address
							   window msg wparam lparam))
		    #+aclpc
		    (ct:%set-long *win-result* 4 0
				   (win:callwindowproc std-ctrl-proc-address
						       window msg wparam lparam))
		    *win-result*)
		(progn
		  ;; We have decided to handle this character ourselves!
		  (handle-event
		   sheet
		   (allocate-event
		    (cond ((or (eql msg win:wm_keydown)
			       (eql msg win:wm_syskeydown)
			       (eql msg win:wm_char))
			   'key-press-event)
			  ((or (eql msg win:wm_keyup)
			       (eql msg win:wm_syskeyup))
			   'key-release-event))
		    :key-name keysym
		    :character char
		    :modifier-state (setf (port-modifier-state *acl-port*)
				      modstate)
		    :sheet sheet))
		  ;; set return value to 0
		  (clear-winproc-result *win-result*)
		  *win-result*))))))
       (t
	;; This is where we let the control do its own thing.  Most
	;; messages come through here.
	(clear-winproc-result *win-result*)
	(setq *win-result* (win:callwindowproc std-ctrl-proc-address
					       window msg wparam lparam))
        *win-result*))
      (setf result *win-result*)
      result)))

(defvar *clim-class* "ClimClass")
(defvar *win-name* "CLIM")
(defvar *menu-name* "ClimMenu")
(defvar *win-x* "x")
(defvar *wndclass-registered* nil)
(defvar clim-window-proc-address nil)
(defvar clim-ctrl-proc-address nil)
(defvar std-ctrl-proc-address nil)
(defvar *clim-initialized* nil)
(defvar lpcmdline "")
(defvar *hinst* 0) 
(defvar *hprevinst* 0) 
(defvar *screen-device* nil)


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
		   :initform (win:CreateDC "DISPLAY" ct:hnull ct:hnull ct:hnull))))

(defun initialize-cg ()
  (let* ((dataobj (make-array 3 :element-type '(signed-byte 32))))
    (win:GetWinMainArgs dataobj)
    (setq *hinst*      (aref dataobj 0)
	  *hprevinst*  (aref dataobj 1)
	  lpcmdline    (aref dataobj 2)))
  (setq *screen-device*
    (make-instance 'windows-screen-device)))

(eval-when (compile load eval)
  (defun make-cstructure (type length)
    ;; create and return a region of memory of the
    ;; given length.
    ;; 
    ;; in the mm implementation this was in malloc space, but since
    ;; it is referenced using #. in files like message.cl, we better
    ;; us a lisp structure so it will exist when the definition
    ;; is fasled in.
    ;;
    ;; we've got to look into this later
    ;;
    ;;
    (declare (ignore type))
  
    (ff:allocate-fobject `(:array :unsigned-char ,length)
		      #-allegro-v4.3 :lisp)))

(defun ensure-clim-initialized ()
  (unless *clim-initialized*
    (initialize-cg)
    (setf clim-window-proc-address 
      (init-clim-win-proc clim-window-proc-address #.(make-cstructure 0 16)))
    (setf clim-ctrl-proc-address 
      (init-clim-ctrl-proc clim-ctrl-proc-address #.(make-cstructure 0 16)))
    (setq *clim-initialized* t)))

(defun acl-clim::register-window-class (hcursor)
  ;; This is called by initialize-instance of acl-port.
  ;; It creates a (single) Windows window class for all clim windows.
  (unless *wndclass-registered*
    (init-clim-win-proc clim-window-proc-address #.(make-cstructure 0 16))
    (let ((class (ct:ccallocate win:wndclassex))
	  (icon (win:LoadIcon 0 win:IDI_APPLICATION)) ; (get-clim-icon)
	  (reg nil))
      (ct:csets win:wndclassex class
                 win::cbSize (ct:sizeof win:wndclassex)
                 win::style (logior win:CS_DBLCLKS
				    win:CS_BYTEALIGNCLIENT
				    win:CS_BYTEALIGNWINDOW
				    ;; win:CS_SAVEBITS ; Can we afford this?
				    ;; win:CS_OWNDC
				    )
                 win::lpfnwndproc clim-window-proc-address 
                 win::cbClsExtra 0
                 win::cbWndExtra 0
                 win::hinstance  *hinst*
                 win::hicon icon 
                 win::hcursor hcursor
                 win::hbrbackground (1+ win:color_btnface)
                 win::lpszmenuname ct:hnull ;*menu-name*
                 win::lpszclassname (ff:string-to-char* *clim-class*)
                 win::hIconSm icon)
      (setq reg (win:registerclassex class))
      (when (zerop reg)
	(check-last-error "RegisterClassEx"))
      (setq *wndclass-registered* t))
    ))

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
	  win:ws_clipsiblings
	  win:ws_caption
	  win:ws_sysmenu
	  win:ws_overlapped
	  (if (or (eql scroll :both) (eql scroll :vertical))
	      win:ws_vscroll 0)
	  (if (or (eql scroll :both) (eql scroll :horizontal))
	      win:ws_hscroll 0)
	  win:ws_clipchildren
	  ))
	(exstyle
	 (logior
	  win:ws_ex_left
	  win:ws_ex_ltrreading
	  win:ws_ex_rightscrollbar
	  win:ws_ex_windowedge
	  win:ws_ex_controlparent	; tab btwn controls
	  ))
        (*win-name* *win-name*)
	(menu
	 (if menubar (win:CreateMenu) (ct:null-handle win:hmenu)))
	(window nil))
    (when pretty
      (setq *win-name* pretty))
    (cond (modal
	   (setq winstyle
	     (logior winstyle
		     win:ws_popup
		     (if parent win:ds_modalframe 0)
		     win:ds_3dlook
		     win:ws_clipchildren))
	   (setq exstyle
	     (logior exstyle
		     win:ws_ex_dlgmodalframe
		     win:ws_ex_controlparent
		     )))
	  (t
	   (setq winstyle
	     (logior winstyle 
		     win:ws_thickframe
		     win:ws_minimizebox
		     win:ws_maximizebox))))
    (setq window
      (win:createWindowEx exstyle
			  *clim-class*
			  *win-name*
			  winstyle
			  left top width height
			  (or parent 0)
			  menu
			  *hinst*
			  *win-x* )) 
    (when (zerop window)
      (or (check-last-error "CreateWindowEx")
	  (error "CreateWindowEx: unknown error")))
    window))

(defun create-pop-up-window (parent pretty scroll left top width height 
			     ovl &optional modal)
  (declare (ignore modal))
  ;; MENU-FRAME comes in here.
  (let* ((overlap (if ovl
		      (logior win:ws_caption win:ws_sysmenu) ; not a menu
		    (logior win:ws_thickframe win:ws_dlgframe) ; its a menu
		    ))
         (winstyle (logior overlap
			   win:ws_popup
			   (if (or (eql scroll :both)(eql scroll :vertical))
			       win:ws_vscroll 0)
			   (if (or (eql scroll :both)(eql scroll :horizontal))
			       win:ws_hscroll 0)
			   win:ws_clipchildren
			   ))
         (*win-name* *win-name*))
    (when pretty
      (setq *win-name* pretty))
    (let ((window (win:createWindowEx
		   (if ovl 0 win:ws_ex_toolwindow)
		   *clim-class*
		   *win-name*
		   winstyle
		   left top width height
		   parent
		   (ct:null-handle win:hmenu)
		   *hinst*
		   *win-x*)))
      (when (zerop window)
	(check-last-error "CreateWindowEx"))
      window)))

(defun create-child-window (parent pretty scroll left top width height)
  ;; Application pane comes in here.
  (let ((winstyle (logior win:ws_clipchildren
                          win:ws_child
			  (if (member scroll '(t :both :dynamic :vertical))
			    win:ws_vscroll 0)
			  (if (member scroll '(t :both :dynamic :horizontal))
			    win:ws_hscroll 0)
			  win:ws_clipsiblings))
	(exstyle (logior win:ws_ex_left
			 win:ws_ex_ltrreading
			 win:ws_ex_rightscrollbar
			 win:ws_ex_controlparent ; tab btwn controls
			 ;; You get an "edge" if you use (outlining () ...)
			 ;; OR if you have scroll bars.  This one is purely
			 ;; aesthetic but I think it's almost always appropriate.
			 ;; JPM 6/98.
			 (if scroll win:ws_ex_clientedge 0)
			 ))
	;; You can ask for a menu bar on a child window,
	;; but Windows will not give you one.  
	(menu (ct:null-handle win:hmenu))
        (*win-name* *win-name*)
	(window nil))
    (when pretty
      (setq *win-name* pretty))
    (setq window
	  (win:createWindowEx exstyle
		*clim-class*
		*win-name*
		winstyle
		left top width height
		parent
		menu
		*hinst*
		*win-x*))
    (when (zerop window)
      (check-last-error "CreateWindowEx"))
    (if (or (eql scroll :both)(eql scroll :vertical))
      (win:setScrollRange window win:SB_VERT 0 *win-scroll-grain* 1))
    (if (or (eql scroll :both)(eql scroll :horizontal))
      (win:setScrollRange window win:SB_HORZ 0 *win-scroll-grain* 1))
    window))

(defvar wres  (ct:callocate :long))
(defvar wmsg  (ct:ccallocate win:msg))

(defun wait-for-event ()
  (when (prog1
            (win:peekMessage wmsg (ct:null-handle win:hwnd) 0 0
			     (logior win:PM_NOYIELD win:PM_NOREMOVE)
			     #+acl86win32x wres)
	  (not (and (zerop (hiword wres)) (zerop (loword wres)))))
    t))

(defvar msg (ct:ccallocate win:msg))
(defvar res (ct:callocate :long))

;;--- this never gets called on NT because we do a
;;--- sys::process-pending-events instead.
(defun await-response (waitp)
  (if waitp
      (progn
        (win:getMessage msg (ct:null-handle win:hwnd) 0 0)
        (win:TranslateMessage msg)
        (win:dispatchMessage msg)
	msg)
    (let* ((ret (win:peekMessage msg (ct:null-handle win:hwnd) 
				 0 0 win:PM_REMOVE))
	   (do-it ret))
      (when do-it
	(win:TranslateMessage msg)
	(win:dispatchMessage msg)))))
