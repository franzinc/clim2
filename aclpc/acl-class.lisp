;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements the lowest levels of Windows support, registering    *
*  the window class, creating windows, defining the message loop.            *
*                                                                            *
*                                                                            *
****************************************************************************|#

(in-package :windows)

#|| ;; this is no longer how it is done
#+acl86win32 (eval-when (compile load eval) 
 	     (if* (equal (short-site-name) "ultra")
 		then (warn "not doing defapi without mainsoft")
		else (setf *defapientry-allowed* t)))

#+acl86win32
(defapientry wprocsetter2 "WProcSetter2" (lresult) void :runtime)
#+acl86win32
(defapientry wprocinit2 "WProcInit2" (long) long :runtime)
#+acl86win32
(defapientry wfontsetter2 "WFontSetter2" (int) void :runtime)
#+acl86win32
(defapientry wfontinit2 "WFontInit2" (long) long :runtime)
#+acl86win32
(defapientry wprocsetter3 "WProcSetter3" (lresult) void :runtime)
#+acl86win32
(defapientry wprocinit3 "WProcInit3" (long) long :runtime)
#+acl86win32
(defapientry wfontsetter3 "WFontSetter3" (int) void :runtime)
#+acl86win32
(defapientry wfontinit3 "WFontInit3" (long) long :runtime)
#+acl86win32 (eval-when (compile load eval) (setf *defapientry-allowed* nil))
||#


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

#+acl86win32x
(cg::defbvar *win-result* (ct::callocate :long))
#+acl86win32
(defvar *win-result* 0)
#+aclpc
(defvar *win-result* (ct::callocate :long))

(defparameter arrow-cursor (ct::callocate win::hcursor))
(defparameter application-icon (ct::callocate win::hicon))

#+acl86win32
(progn
   ;; window procedure unwinding state
   (cg::defbvar *window-proc-return-reason* nil)
   (cg::defbvar *window-proc-return-tag* nil)
   (cg::defbvar *window-proc-return-value* nil)
   (cg::defbvar *window-proc-return-other-values* nil)
   ;; window procedure args, rebound in each call to window proc
   (cg::defbvar *hwnd* (cg::null-handle hwnd))
   (cg::defbvar *msg* win:WM_NULL)
   (cg::defbvar *wparam* 0)
   (cg::defbvar *lparam* (cg::callocate :long))
   ;; Window procedure result variable, returned to Windows.
   ;; Set to 0 initially in each call to window proc and smashed by anyone.
#+acl86win32x   (cg::defbvar *window-proc-result* (cg::callocate :long))
   (defvar *window-proc-result* 0)
   ;; to avoid garbage we reuse window procedure args from queue
   (cg::defbvar *window-proc-args-queue* nil)
)

(defun init-cursors ()
  ;; put back old cursor mechanism for the moment (cim 9/13/96)
  (setf arrow-cursor
        (win::LoadCursor 
	  (ct::null-handle win::hinst)
	  (ct::ccallocate (char *) :initial-value win::IDC_ARROW)))
  ;; this just does the icon now - the cursor stuff is all handled in
  ;; realize-cursor methods in acl-port (cim 9/12/96)
  (setf application-icon
	(win::LoadIcon 
	  (ct::null-handle win::hinst)
	  (ct::ccallocate (char *) :initial-value win:IDI_APPLICATION))))

#+(or aclpc acl86win32)
(eval-when (compile load eval)
   ;;mm: 11Jan95 - this is defined later in  ???
   (unless (ignore-errors (find-class 'silica::hlist-pane))
      (defclass silica::hlist-pane () ())))

;;; Gather up the argument information and invoke the window procedure.
;;; +++ at some point merge this in with the cg mechanism for lisp
;;; windows.
#+acl86win32
(defun clim-window-proc (x)
  #||
  ;; bind message variables 
  (multiple-value-bind
    (queue-entry *window-proc-result* *hwnd* *msg* *wparam* *lparam*)
      (cg::get-window-proc-args)
  (let ((d (cg::pccstructure-data-pointer x)))
    (setf *hwnd* (cg::long-ref d 0))
    (setf *msg* (cg::long-ref d 1))
    (setf *wparam* (cg::long-ref d 2))
    (setf *lparam* (cg::signed-long-ref d 3)))
   
  ;; set return value to 0 
  (setf (cg::be *window-proc-result* 0) 0)
  (setf (cg::be *window-proc-result* 1) 0)
  ;; dispatch 
   
  (clim-wind-proc *hwnd* *msg* *wparam* *lparam*))
  ||#
  ;; always return *window-proc-result* to the kernel as a cpointer 
  (let ((result #-acl86win32 (cg::ccallocate (:void *)) #+acl86win32 0))
    (setf #-acl86win32 (pc::cpointer-value result) #+acl86win32 result
          #-acl86win32 (cg::word-vector-long-ref *window-proc-result* 0)
          #+acl86win32 *window-proc-result*)
    ;; release queue vector 
    ;(cg::release-window-proc-args queue-entry)+++seems OK uncomment.
    ;; return result 
    result))

#+acl86win32
(defvar *clim-wproc-arg-struct* nil)

#+acl86win32
(defvar *clim-ctrl-arg-struct* nil)

#+acl86win32
(ff:defun-c-callable wproc-clim-wrapper (hwnd message wparam lparam)
  (let* ((s *clim-wproc-arg-struct*)
	 (d (cg::pccstructure-data-pointer s)))
    (setf (cg::long-ref d 0) hwnd
	  (cg::long-ref d 1) message
	  (cg::long-ref d 2) wparam
	  (cg::signed-long-ref d 3) lparam)
    ;(cg::wprocsetter2
      (clim-window-proc s)
      ));)

#+acl86win32
(defun init-clim-win-proc (wproc-address arg-struct)
  ;(setf (pc::cpointer-value wproc-address)
        ;(cg::wprocinit2 
            ;(ff:register-function 'clim-wind-proc))
  ;)
  (setf *clim-wproc-arg-struct* arg-struct)
  (ff:register-function 'clim-wind-proc :reuse :return-value))

#+acl86win32
(defun init-clim-ctrl-proc (wproc-address arg-struct)
  ;(setf (pc::cpointer-value wproc-address)
        ;(cg::wprocinit3 
          ;(ff:register-function 'clim-ctrl-proc);)
        ;)
  (setf *clim-ctrl-arg-struct* arg-struct)
  (ff:register-function 'clim-ctrl-proc :reuse :return-value))

#+aclpc
(defun init-clim-win-proc (wproc-address arg-struct)
  (setf (pc::cpointer-value wproc-address)
        (ct:get-callback-procinst 'clim-wind-proc))
  (setf *clim-wproc-arg-struct* arg-struct))

#+aclpc
(defun init-clim-ctrl-proc (wproc-address arg-struct)
  (setf (pc::cpointer-value wproc-address)
        (ct:get-callback-procinst 'clim-ctrl-proc))
  (setf *clim-ctrl-arg-struct* arg-struct))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; callback for the windowproc for all CLIM windows.

(defun innermost-sheet-pointer-cursor (sheet)
  (and sheet
       (or (sheet-pointer-cursor sheet)
	   (innermost-sheet-pointer-cursor (sheet-parent sheet)))))

(defvar *cursor-cache* nil)

(defun maybe-set-cursor (window)
  (let* ((sheet (mirror->sheet *acl-port* window))
	 (cursor (or (port-grab-cursor *acl-port*)
		     (innermost-sheet-pointer-cursor sheet)
		     (pointer-cursor (port-pointer *acl-port*)))))
    (when (and cursor
	       (not (eq cursor :default))
	       (not (eq cursor *cursor-cache*)))
      (setq *cursor-cache* cursor)
      (win::setcursor (realize-cursor *acl-port* cursor))
      t)))

(defvar *level* 0)

(defvar *realtime-scrollbar-tracking* t)
(defvar *win-scroll-grain* 1000)

(defmacro clear-winproc-result (res)
  #+acl86win32x
  `(setf (cg::word-vector-long-ref ,res 0) 0)
  #+acl86win32x
  `(setf ,res 0)
  #+aclpc
  `(progn
	 (setf (acl::be ,res 0) 0)
	 (setf (acl::be ,res 1) 0)))

#+aclpc
(defvar win::cbn_closeup 8)

;;; begin new support for drawing on gadgets pnc Aug97 for clim2bug740
;;; need to do equivalent for NT too.

#+aclpc
(eval-when (load compile eval)
  (ct::defcstruct DrawItemStruct
    ((CtlType pc::uint)
     (CtlID pc::uint)
     (itemID pc::uint)
     (itemAction pc::uint)
     (itemState pc::uint)
     (hwndItem pc::hwnd)
     (hDC pc::HDC)
     #+ignore (rcItem pc::RECT)
     (rcItem pc::uint)
     (itemData pc::dword)))
  (ct::defcstruct MeasureItemStruct
    ((CtlType pc::uint)		; type of control 
     (CtlID pc::uint)		; combo box, list box, or button identifier 
     (itemID pc::uint)		; menu item, variable-height list box, or combobox identifier 
     (itemWidth pc::uint)	; width of menu item, in pixels 
     (itemHeight pc::uint)	; height of single item in list box menu, in pixels 
     (itemData pc::dword))))	; application-defined 32-bit value

#+aclpc
(defun drawitemstruct-info (lparam)
  (let ((pointer (ct::ccallocate (:void *)))
        (drawitemstruct (ct::ccallocate DrawItemStruct)))
    (setf (ct::cpointer-value pointer) lparam)
    (ct::far-peek drawitemstruct pointer 0 (ct::sizeof drawitemstruct))
    (values (ct::cref DrawItemStruct drawitemstruct CtlType)
	    (ct::cref DrawItemStruct drawitemstruct CtlID)
	    (ct::cref DrawItemStruct drawitemstruct itemID)
	    (ct::cref DrawItemStruct drawitemstruct itemAction)
	    (ct::cref DrawItemStruct drawitemstruct itemState)
	    (ct::cref DrawItemStruct drawitemstruct hwndItem)
	    (ct::cref DrawItemStruct drawitemstruct hdc)
	    (ct::cref DrawItemStruct drawitemstruct rcItem)
	    (ct::cref DrawItemStruct drawitemstruct itemData))))

#+aclpc
(defun set-measureitemstruct-width-and-height (lparam width height)
  (let ((pointer (ct::ccallocate (:void *)))
        (measureitemstruct (ct::ccallocate measureitemstruct)))
    (setf (ct::cpointer-value pointer) lparam)
    (ct::far-peek measureitemstruct pointer 0 (ct::sizeof measureitemstruct))
    (when width
      (setf (ct::cref measureitemstruct measureitemstruct itemWidth) width))
    (when height
      (setf (ct::cref measureitemstruct measureitemstruct itemHeight) height))
    (ct::far-poke measureitemstruct pointer 0 (ct::sizeof measureitemstruct))
    (values (ct::cref measureitemstruct measureitemstruct CtlType)
	    (ct::cref measureitemstruct measureitemstruct CtlID)
	    (ct::cref measureitemstruct measureitemstruct itemID)
	    (ct::cref measureitemstruct measureitemstruct itemWidth)
	    (ct::cref measureitemstruct measureitemstruct itemHeight)
	    (ct::cref measureitemstruct measureitemstruct itemData))))

#+aclpc (defvar *drawitem-started* 0)
#+aclpc (defvar *buttons-drawn* 0)

;;; end new support for drawing on gadgets pnc Aug97 for clim2bug740

(#+aclpc      ct:defun-callback
 #+acl86win32 ff:defun-c-callable
              clim-wind-proc
 #+aclpc      ((window win:hwnd) (msg win:uint) (wparam win:wparam) (lparam win:lparam))
 #+acl86win32 (window msg wparam lparam)
 #+acl86win32 (declare (:convention :stdcall) (:unwind 0))
 (#+acl86win32 mp:without-scheduling
  #-acl86win32 progn
  #+acl86win32 (setf *hwnd* window)
  (mformat #+acl86win32 excl:*initial-terminal-io* 
	   #+aclpc *standard-output*
	   "In clim-wind-proc msg=~a sheet=~s lparam=~a~%"
	   (msg-name msg) (mirror->sheet *acl-port* window) lparam)
  (let ((result #+aclpc (ct::ccallocate (win:void *)) #-aclpc 0)
        (*level* (1+ *level*)))
    (when (> *level* 40)
      (break "too deep!"))
    (cond
     ((eql msg win:wm_mousemove)
      ;; (maybe-set-cursor window)
      (let ((mx (pc::loword lparam))
	    (my (pc::hiword lparam))
	    (sheet (mirror->sheet *acl-port* window)))
	(note-pointer-motion *acl-port* sheet mx my)))
     
     ((eql msg win:wm_setcursor)
      (let ((hit-code (pc::loword lparam)))
	(unless (and (eql hit-code win::htclient)
		     (maybe-set-cursor window))
	  (setq *cursor-cache* nil)
	  (setf (pointer-cursor (port-pointer *acl-port*)) :button) 
	  #+acl86win32
	  (setq *win-result* (win::defwindowproc window msg wparam lparam))
	  #+aclpc
	  (ct::%set-long *win-result* 4 0 (win::defwindowproc window msg wparam lparam)))))
     
     ((eql msg win:wm_paint)
      (let* ((udrect (ct::ccallocate win:rect))
	     (berase 0)			;  (ct::ccallocate win:bool)
	     (update (win::getUpdateRect window udrect berase))
	     (sheet (mirror->sheet *acl-port* window))
                   ;;; +++rl added to validate everything because of multiple
                   ;;; (continuous) repaints with maximized window
	     (vdrect (ct::ccallocate win:rect)))
               ;;; +++rl same as comment above
	(win::getClientRect window vdrect)
	(if update
	    (let ((ilef (ct::cref win::rect udrect win::left))
		  (itop (ct::cref win::rect udrect win::top)) 
		  (irig (ct::cref win::rect udrect win::right))
		  (ibot (ct::cref win::rect udrect win::bottom))
		  (done-val (win::validateRect window vdrect)))
	      
	      (unless (or (= ilef irig) (= itop ibot)) ;does this happen alot?
		(when sheet
		  #-ignore
		  (handle-event
		   sheet
		   (allocate-event 'window-repaint-event
				   :native-region (sheet-native-region sheet)
				   :region (make-bounding-rectangle ilef itop irig ibot)
				   :sheet sheet))
		  #+ignore
		  (with-slots (event-queue) *acl-port*
		    (queue-put 
		     event-queue
		     (allocate-event
		      'window-repaint-event
		      :native-region (sheet-native-region sheet)
		      :region (make-bounding-rectangle ilef itop irig ibot)
		      :sheet sheet)))))))))
     #+aclpc ;; pnc Aug97 for clim2bug740
     ((eql msg win::wm_drawitem) 
      (incf *drawitem-started*)
      (multiple-value-bind (ctltype ctlid itemid itemaction itemstate 
			    hwnditem hdcitem rcitem itemdata)
	  (drawitemstruct-info lparam)
	(let ((hwnd (ct::ccallocate win::hwnd))
	      (hdc (ct::ccallocate win::hdc))
	      (state itemstate)
	      (rect rcitem))
	  (setf (pc::handle-value win::hwnd hwnd) hwnditem)
	  (setf (pc::handle-value win::hdc hdc) hdcitem)
	  (let ((sheet (mirror->sheet *acl-port* hwnd)))
	    (when sheet (incf *buttons-drawn*)
		  (silica::draw-picture-button (mirror->sheet *acl-port* hwnd)
					       state hdc rect)))))
      *win-result*)
     #-aclpc				;+++ fix this for ACLPC +++
     ((eql msg win::wm_drawitem)
      ;; someone who knows how to use the pc ff interface should get
      ;; rid of all these memrefs! (cim 10/4/96)
      (let ((hwnd (ct::ccallocate win::hwnd))
	    (hdc (ct::ccallocate win::hdc))
	    (state (sys:memref-int lparam 0 16 :unsigned-long))
	    (rect (+ lparam 28)))
	(setf (pc::handle-value win::hwnd hwnd)
	  (sys:memref-int lparam 0 20 :unsigned-long))
	(setf (pc::handle-value win::hdc hdc)
	  (sys:memref-int lparam 0 24 :unsigned-long))
	(let ((sheet (mirror->sheet *acl-port* hwnd)))
	  (when sheet
	    (silica::draw-picture-button (mirror->sheet *acl-port* hwnd)
					 state hdc rect)))))
     
     ((or (eql msg win::wm_ctlcoloredit)
	  (eql msg win::wm_ctlcolorlistbox)
	  (eql msg win::wm_ctlcolorbtn)
	  ;; couldn't get the colors to change for the following
	  ;; wm_ctlcolorxx messages -  so we're not using them for the
	  ;; moment (cim 10/11/96)
	  #+ignore (eql msg win::wm_ctlcolormsgbox)
	  #+ignore (eql msg win::wm_ctlcolordlg)
	  #+ignore (eql msg win::wm_ctlcolorscrollbar)
	  #+ignore (eql msg win::wm_ctlcolorstatic))
      (let ((hwnd (ct::ccallocate win::hwnd))
	    (hdc (ct::ccallocate win::hdc)))
	(setf (pc::handle-value win::hwnd hwnd) lparam
	      (pc::handle-value win::hdc hdc) wparam)
	(let ((sheet (mirror->sheet *acl-port* hwnd)))
	  (when sheet
	    #+acl86win32		;+++ aclpc
	    (setf *win-result* (adjust-gadget-colors sheet hdc))))))
     
     ((eql msg win::wm_command)
      (let ((wloword (pc::loword wparam))
	    (whiword (pc::hiword wparam))
	    (lloword (pc::loword lparam))
	    (lhiword (pc::hiword lparam))
	    (sheet (mirror->sheet *acl-port* window))
	    (pointer (port-pointer *acl-port*))
	    (modifier-state (make-modifier-state))
	    (gadget nil)
	    (hwndid nil)
	    (hwnd (ct::ccallocate win::hwnd)))
	;;mm: defined in acl-mirr.lsp later
	(declare (special *gadget-id->window*))
	(when pointer
	  (flush-pointer-motion *acl-port*))
	
	(if (and (= lparam 0)		; menu item
		 (= whiword 0))		; otherwise control (or accelerator)
	    (let* ((frame (pane-frame sheet))
		   (command-table (frame-command-table frame))
		   (command (cdr (aref *menu-id->command-table* wloword))))
	      ;; pr Aug97
	      (with-slots (clim-internals::disabled-commands) frame
		(if (member (car command) clim-internals::disabled-commands)
		    (win::messagebeep 200)
		  (queue-put (slot-value *acl-port* 'event-queue)
			     (allocate-event 'presentation-event
					     :frame frame
					     :sheet (frame-top-level-sheet frame)
					     :presentation-type
					     `(command :command-table ,command-table)
					     :value command)))))
	  (progn 
	    (setf (pc::handle-value win::hwnd hwnd) lparam)
	    ;;mm: for the moment, the following seems superfluous
	    ;;(setf hwndid (silica::gadget-id->window sheet wloword))
	    (setf gadget (mirror->sheet *acl-port* hwnd))
	    (if (typep gadget 'silica::mswin-combo-box-pane)
		(let ((sheet (mirror->sheet *acl-port* window)))
		  (when (and sheet (= whiword win::cbn_closeup))
		    (with-slots (event-queue) *acl-port*
		      (queue-put
		       event-queue
		       (multiple-value-bind
			   (left top right bottom)
			   (mirror-client-region-internal* *acl-port* hwnd window)
                         (allocate-event 'silica::window-change-event
					 :native-x (+ left 1)
					 :native-y (+ top 1)
					 :button +pointer-left-button+
					 :modifier-state 0
					 :pointer pointer
					 :sheet sheet
					 :mswin-control gadget))))))
	      ;; else
	      (if (typep gadget 'silica::mswin-text-edit)
		  (let ((sheet (mirror->sheet *acl-port* window)))
		    (when (and sheet (= whiword pc::en_killfocus))
		      
		      (with-slots (event-queue) *acl-port*
					;handle-event
					;  gadget
			(multiple-value-bind
			    (left top right bottom)
			    (mirror-client-region-internal* *acl-port* hwnd window)
			  (queue-put event-queue
				     (allocate-event 'silica::window-change-event
						     :native-x (+ left 1)
						     :native-y (+ top 1)
						     :button +pointer-left-button+
						     :modifier-state 0
						     :pointer pointer
						     :sheet sheet
						     :mswin-control gadget))))))
		;; else
		(when (or (not (typep gadget 'silica::hlist-pane))
			  (= whiword pc::hln_selchange))
		  (when (typep gadget 'silica::hlist-pane)
		    (win::setfocus window))
		  (with-slots (event-queue) *acl-port*
		    
		    (multiple-value-bind (left top right bottom)
			(mirror-client-region-internal* *acl-port* hwnd window)
		      #+ignore
		      (queue-put event-queue
				 (allocate-event 'pointer-button-press-event
						 :native-x (+ left 1) ; nc=4
						 :native-y (+ top 1) ; nc=23
						 :button +pointer-left-button+
						 :modifier-state modifier-state
						 :pointer pointer
						 :sheet sheet))
		      #+ignore
		      (queue-put event-queue
				 (allocate-event 'pointer-button-release-event
						 :native-x (+ left 1)
						 :native-y (+ top 1)
						 :button +pointer-left-button+
						 :modifier-state modifier-state
						 :pointer pointer
						 :sheet sheet))
		      (queue-put event-queue
				 (allocate-event 'silica::window-change-event
						 :native-x (+ left 1)
						 :native-y (+ top 1)
						 :button +pointer-left-button+
						 :modifier-state modifier-state
						 :pointer pointer
						 :sheet sheet
						 :mswin-control gadget)))))))))
	(clear-winproc-result *win-result*)
	*win-result*))
     ((or (eql msg win::wm_hscroll)
	  (eql msg win::wm_vscroll))
      (let ((type (pc::loword wparam))
	    (position (pc::hiword wparam))
	    (message (cond ((eql msg win::wm_hscroll) :horizontal)
			   ((eql msg win::wm_vscroll) :vertical)))
	    (flag (cond ((eql msg win::wm_hscroll) win:sb_horz)
			((eql msg win::wm_vscroll) win:sb_vert)))
	    (event nil)
	    (sheet (mirror->sheet *acl-port* window)))
	(declare (fixnum action position))
	(multiple-value-bind (action amount)
	    (cond ((eql type win::sb_lineup)
		   (win::setScrollPos window flag
				      (- (win::getScrollPos window flag) 1) 1)
		   (values :relative-jump -1))
		  ((eql type win::sb_linedown)
		   (win::setScrollPos window flag
				      (+ (win::getScrollPos window flag) 1) 1)
		   (values :relative-jump +1))
		  ((eql type win::sb_pageup)
		   (win::setScrollPos window flag
				      (- (win::getScrollPos window flag) 1) 1)
		   (values :screenful -1))
		  ((eql type win::sb_pagedown)
		   (win::setScrollPos window flag
				      (+ (win::getScrollPos window flag) 1) 1)
		   (values :screenful +1))
		  ((eql type win::sb_thumbposition)
		   (win::setScrollPos window flag position 1)
		   (values :percentage position))
		  ((and *realtime-scrollbar-tracking*
			(eql type win::sb_thumbtrack))
		   (win::setScrollPos window flag position 1)
		   (values :percentage position))
		  ((eql type win::sb_top)
		   (win::setScrollPos window flag 0 1)
		   (values :percentage 0))
		  ((eql type win::sb_bottom)
		   (win::setScrollPos window flag *win-scroll-grain* 1)
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
     ((or (eql msg win:wm_move)
	  (eql msg win:wm_size))
      (let ((sheet (mirror->sheet *acl-port* window))
	    (lorw (pc::loword lparam))
	    (torh (pc::hiword lparam)))
	(if (and sheet
		 (not (eq sheet *setting-sheet-mirror-edges*))
		 (not (win::IsIconic window)))
	    (progn
	      #-ignore
	      (handle-event
	       sheet
	       (allocate-event 'window-configuration-event :sheet sheet))
	      #+ignore
	      (with-slots (event-queue) *acl-port*
		(queue-put event-queue
			   (allocate-event 'window-configuration-event
					   :sheet sheet)))
	      ;; set return value to 0 
	      (clear-winproc-result *win-result*))
	  #+acl86win32
	  (setq *win-result* (win::defwindowproc window msg wparam lparam))
	  #+aclpc
	  (ct::%set-long *win-result* 4 0
			 (win::defwindowproc window msg wparam lparam)))
	*win-result*))
     #+aclpc ;; pnc Aug97 for clim2bug740
     ((eql msg win::wm_getminmaxinfo)
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
		(setf (sys:memref-int lparam 0 24 pc::uint) min-width
		      (sys:memref-int lparam 0 28 pc::uint) min-height)
		(clear-winproc-result *win-result*)))
	  #+acl86win32
	  (setq *win-result* (win::defwindowproc window msg wparam lparam))
	  #+aclpc
	  (ct::%set-long *win-result* 4 0
			 (win::defwindowproc window msg wparam lparam)))
	*win-result*))
     #-aclpc				; +++ fix this for aclpc +++
     ((eql msg win::wm_getminmaxinfo)
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
	  (setq *win-result* (win::defwindowproc window msg wparam lparam))
	  #+aclpc
	  (ct::%set-long *win-result* 4 0
			 (win::defwindowproc window msg wparam lparam)))
	*win-result*))
     ((eql msg win::en_update)
      (let ((sheet (mirror->sheet *acl-port* window)))
	(when sheet
	  (with-slots (event-queue) *acl-port*
	    ;; queue-put event-queue
	    ;; handle-event
	    ;; sheet
	    ;;  (allocate-event 'window-change-event :sheet sheet)
	    )))
      (clear-winproc-result *win-result*)
      *win-result*)
     ;; character typed
     ((or (eql msg win:wm_keydown)
	  (eql msg win:wm_syskeydown)
	  (eql msg win:wm_keyup)
	  (eql msg win:wm_syskeyup))
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
	(let* ((capstate (win::getKeyState win:vk_capital))
	       (numstate (win::getKeyState win:vk_numlock))
	       (shiftstate (win::getKeyState win:vk_shift))
	       (controlstate (win::getKeyState win:vk_control))
	       (metastate (win::getKeyState win:vk_menu)))
	  #+debugg
	  (format *standard-output* "caps=~a num=~a shift=~a ctrl=~a meta=~a code=~a~%"
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
	      #+acl86win32
	      (setq *win-result* (win::defwindowproc window msg wparam lparam))
	      #+aclpc
	      (ct::%set-long *win-result* 4 0
			     (win::defwindowproc window msg wparam lparam))
	      *win-result*)
	  (progn
	    (when (and (or (<= #x30 vk #x5a)(<= #xba vk #xc0)(<= #xdb vk #xdf))
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
			    :modifier-state (setf (port-modifier-state *acl-port*)
					      modstate)
			    :sheet sheet))
		))
	    ;; set return value to 0 
	    (clear-winproc-result *win-result*)
	    *win-result*))))
     ((or (eql msg win:wm_lbuttondown)
	  (eql msg win:wm_rbuttondown)
	  (eql msg win:wm_mbuttondown)
	  (eql msg win:wm_lbuttonup)
	  (eql msg win:wm_rbuttonup)
	  (eql msg win:wm_mbuttonup))
      
      ;; added the following so that clicking on a blank area will
      ;; move the focus away from any text-fields and cause their
      ;; value to be correctly updated  - this was copied from the
      ;; handle-event on key-press-event for mswin-text-edit in
      ;; acl-widg (cim 9/17/96)
      (win::setfocus (win::getactivewindow) #-acl86win32 :static)
      
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
					 :native-x (pc::loword lparam)
					 :native-y (pc::hiword lparam)
					 :button button
					 :modifier-state modifier-state
					 :pointer pointer
					 :sheet (mirror->sheet *acl-port* window)))))))
      (clear-winproc-result *win-result*)
      *win-result*)
     ((eql msg win:wm_activate)
      (let ((sheet (mirror->sheet *acl-port* window))
	    (flag (pc::loword wparam)))
	(when (and sheet (> flag 0))
	  (setf (acl-port-mirror-with-focus *acl-port*) window))
	;; set return value to 0 
	(clear-winproc-result *win-result*)
	*win-result*))
     ((or (eql msg win:wm_killfocus)
	  (eql msg win:wm_close))
      (let* ((sheet (mirror->sheet *acl-port* window))
	     (frame (pane-frame sheet))
	     (menup (getf (frame-properties frame) :menu-frame)))
	(if (and frame menup)
	    (let ((menu (slot-value frame 'clim-internals::menu)))
	      (setf (window-visibility menu) nil))
	  (when (eql msg win:wm_close)
	    (with-slots (event-queue) *acl-port*
	      (queue-put event-queue
			 (allocate-event 'silica::window-close-event
					 :sheet sheet)))))
	;; set return value to 0 
	(clear-winproc-result *win-result*)
	*win-result*))
     ;; the following has been moved into above clause with
     ;; wm_killfocus so as to do the right thing with menu frames
     ;; (cim 9/12/96)
     #+ignore
     ((eql msg win:wm_close)
      (let* ((sheet (mirror->sheet *acl-port* window))
	     (frame (pane-frame sheet)))
	(with-slots (event-queue) *acl-port*
	  (queue-put event-queue
		     (allocate-event 'silica::window-close-event
				     :sheet sheet)))
	;; set return value to 0 
	(clear-winproc-result *win-result*)
	*win-result*))
     #|| We can't do it this way because the frame-event gets run in the scheduler
     process and activities depend on being able to throw to a catch on the
     application stack.
     ((eql msg win:wm_close)
      (let* ((sheet (mirror->sheet *acl-port* window))
	     (frame (pane-frame sheet)))
	(when frame
	  (frame-exit frame))
	(destroy-mirror *acl-port* sheet)
	;; set return value to 0 
	(clear-winproc-result *win-result*)
	*win-result*))
     ||#
     
     ((eql msg win:wm_initmenupopup)
      (update-menu-item-sensitivities wparam)
      *win-result*)
     (t
      (clear-winproc-result *win-result*)
      #+acl86win32
      (setq *win-result* (win::defwindowproc window msg wparam lparam))
      #+aclpc
      (ct::%set-long *win-result* 4 0
		     (win::defwindowproc window msg wparam lparam))
      *win-result*))
    #+acl86win32
    (setf result *win-result*)
    #+acl86win32
    (mformat #+acl86win32 excl:*initial-terminal-io* #+aclpc *standard-output*
	     "Out clim-wind-proc msg=~a sheet=~s result=~s~%"
	     (msg-name msg) (mirror->sheet *acl-port* window) result)
    #+acl86win32
    result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implements the window proc for the subclassed controls (presently
;;; only the edit control).

(#+aclpc ct:defun-callback #+acl86win32 ff:defun-c-callable clim-ctrl-proc
  #+aclpc ((window win:hwnd)(msg win:uint)(wparam win:wparam)(lparam win:lparam))
  #+acl86win32 (window msg wparam lparam)
  #+acl86win32 (declare (:convention :stdcall) (:unwind 0))
  (#+acl86win32 mp:without-scheduling #-acl86win32 progn
  #+acl86win32 (setf *hwnd* window)
  #+acl86win32
  (mformat #+acl86win32 excl:*initial-terminal-io* #+aclpc *standard-output*
	   "In clim-ctrl-proc msg=~a sheet=~s~%"
	   (msg-name msg) (mirror->sheet *acl-port* window))
  (setq *args* (list window msg wparam lparam))
  ;(break "look at *args*")
  (let ((result #+aclpc (ct::ccallocate (win:void *)) #+acl86win32 0))
    (cond
      ;; character typed
      ((or (eql msg win:wm_keydown)
		   (eql msg win:wm_syskeydown)
		   (eql msg win:wm_keyup)
		   (eql msg win:wm_syskeyup)
		   (eql msg win:wm_char))
	   (flush-pointer-motion *acl-port*)
	   (let* ((code wparam)
			  (pass t) ; !!!
			  (vk (ldb (byte 8 0) code)))
		 (when (or (eql code win:vk_capital)
				   (eql code win:vk_numlock)
				   (eql code win:vk_shift)
				   (eql code win:vk_control)
				   (eql code win:vk_menu))
		   (setq pass t))
		 (let* ((capstate (win::getKeyState win:vk_capital))
				(numstate (win::getKeyState win:vk_numlock))
				(shiftstate (win::getKeyState win:vk_shift))
				(controlstate (win::getKeyState win:vk_control))
				(metastate (win::getKeyState win:vk_menu)))
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
				   (setq *win-result* (pc::callwindowproc std-ctrl-proc-address
														  window msg wparam lparam))
				   #+aclpc
				   (ct::%set-long *win-result* 4 0
								  (pc::callwindowproc std-ctrl-proc-address
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
		#+acl86win32
 	    (setq *win-result* (pc::callwindowproc std-ctrl-proc-address
			 				  window msg wparam lparam))
		#+aclpc
		(ct::%set-long *win-result* 4 0
					   (pc::callwindowproc std-ctrl-proc-address
										   window msg wparam lparam))
        *win-result*))
#+acl86win32
    (setf result *win-result*)
#+acl86win32
      result
    ))
)

(defvar *clim-class* "ClimClass")

#+aclpc
(defvar *win-name* (ct::ccallocate win:lpstr :size 5))
#+aclpc
(ct:cset (:char *) *win-name* nil "CLIM" (string 4))
#+aclpc
(defvar *menu-name* (ct::ccallocate win:lpstr :initial-value 0)) ; "ClimMenu"

#+acl86win32
(defvar *win-name* "CLIM")
#+acl86win32
(defvar *menu-name* "ClimMenu")

(defvar *win-arg* (ct::ccallocate win::lpvoid))
(defvar *win-x* "x")

(defvar *wndclass-registered* nil)

#+aclpc
(defun acl-clim::register-window-class ()
  (let ((err1 nil)
        (err2 nil)
	(reg nil)
	(wndclass nil)
	(clim-proc-address (ct:get-callback-procinst 'clim-wind-proc)))
    (setq wndclass
	  (ct::callocate win::wndclass
			win::style         win::CS_DBLCLKS
			win::lpfnwndproc   clim-proc-address ; cg::window-proc-address
			win::cbclsextra    0
			win::cbwndextra    0
			win::hinstance     pc::*hinst*
			win::hicon         (ct::null-handle win::hicon)
			win::hcursor       (ct::null-handle win::hcursor)
			win::hbrbackground (ct::null-handle win::hbrush)
					; (win::GetStockObject win::WHITE_BRUSH)
			win::lpszmenuname *menu-name*
			win::lpszclassname *clim-class*))
    (unless *wndclass-registered*
      (setq err1 (win::getLastError))
      (setq reg (win::registerclass wndclass))
      (setq err2 (win::getLastError))
      (setq *wndclass-registered* t)
      )))

#+acl86win32
;(cg::defbvar clim-window-proc-address (cg::callocate (:void *)))
(defvar clim-window-proc-address nil)

#+acl86win32
;(cg::defbvar clim-ctrl-proc-address (cg::callocate (:void *)))
(defvar clim-ctrl-proc-address nil)

#+acl86win32
;(cg::defbvar std-ctrl-proc-address (cg::callocate (:void *)))
(defvar std-ctrl-proc-address nil)

#+aclpc
(defvar clim-window-proc-address (ct::callocate (:void *)))

#+aclpc
(defvar clim-ctrl-proc-address (ct::callocate (:void *)))

#+aclpc
(defvar std-ctrl-proc-address (ct::callocate (:void *)))

#+acl86win32
(defun initialize-clim (&optional (mp t))
  (declare (ignore mp))
  (warn "~s deprecated - CLIM is automatically initialized"
	'initialize-clim)) 

#+acl86win32
(defvar *clim-initialized* nil)

#+aclpc 
(defvar *clim-initialized* nil)

(in-package :pc)
#+acl86win32
(defun initialize-cg ()
  (let* ((dataobj (make-array 3 :element-type '(signed-byte 32))))
	  (win::GetWinMainArgs dataobj)
	  (setq pc::*hinst*      (aref dataobj 0)
		    pc::*hprevinst*  (aref dataobj 1)
			pc::lpcmdline    (aref dataobj 2)))
  (setq pc::*command-line* (ff::char*-to-string pc::lpcmdline))
  (let ((class (ccallocate wndclassex))) ; <11>
    (csets wndclassex class ; <11>
	   cbSize (sizeof wndclassex) ; <11>
	   style #.(logior CS_OWNDC CS_DBLCLKS)
	   lpfnwndproc 0 ;window-proc-address
	   cbClsExtra 0
	   cbWndExtra DLGWINDOWEXTRA
	   hinstance  *hinst*
	   hicon (if (pre-windows4-p) ;; <13>
		     (ccallocate ct::lhandle :initial-value 0) ;; <13>
		   (LoadIcon *hinst* "ICON_LISP")) ;cac 14mar96
	   hcursor (null-handle hcursor)
	   hbrbackground (null-handle hbrush)
	    ;; the next two are overwritten by str-registerclassex
	   lpszmenuname ct::hnull
	   lpszclassname lisp-window-class-name
	   ;; until we get a small icon in acl5
	   
	   hIconSm
	   #+aclmerge 0
	   #+aclpc (if (pre-windows4-p) ;; <13>
		       (ccallocate ct::lhandle :initial-value 0) ;; <13>
		     (loadimage *hinst* "ICON_LISP" 
				1	; image_icon
				16 16 0)) ; <11>
	   )
    (if*  (zerop (str_RegisterClassEx class 
				    lisp-window-class-name
				    0
				    )) ; <11>
       then (error "Lisp window class 1 could not be registered. Aborting Lisp."))
    (csets  wndclassex  class ; <11>
	    cbSize (sizeof wndclassex) ; <11>
	    style #.(logior CS_OWNDC CS_DBLCLKS CS_HREDRAW CS_VREDRAW)
	    lpfnwndproc 0 ;window-proc-address
	    cbClsExtra 0
	    cbWndExtra DLGWINDOWEXTRA
	    hinstance  *hinst*
	    hicon (if (pre-windows4-p) ;; <13>
		      (ccallocate ct::lhandle :initial-value 0) ;; <13>
		    (LoadIcon *hinst* "ICON_LISP")) ;cac 14mar96
	    hcursor (null-handle hcursor)
	    hbrbackground (null-handle hbrush)
	    ;; the next two are overwritten by str-registerclassex
	    lpszmenuname ct::hnull
	    lpszclassname lisp-resizing-window-class-name
	    hIconSm
	    #+aclmerge 0
	    
	    #+aclpc 
	    (if (pre-windows4-p) ;; <13>
		(ccallocate ct::lhandle :initial-value 0) ;; <13>
	      (loadimage *hinst* "ICON_LISP"
			 1 ; image_icon
			 16 16 0)) ; <11>
	    )
    (if* (zerop (str_RegisterClassEx class 
				   lisp-resizing-window-class-name
				   0
				   )) ; <11>
       then (error "Lisp window class 2 could not be registered. Aborting Lisp.")))
  (setq *screen*
    (pc::open-stream 'pc::windows-screen-device
	 	         'pc::windows-screen-location :io
		         :name :screen))
  )

(in-package :acl-clim)

#+acl86win32
(defun ensure-clim-initialized ()
  (unless *clim-initialized*
    #+acl86win32x (cg::do-init-windows :topwin nil)
    #+acl86win32 (cg::initialize-cg)

  ;; The following turns off a 330ms timer that cg starts up. This
  ;; makes debugging of mp event bugs harder and CLIM shouldn't need
  ;; this anyway. When the CLIM and CG event loops are made to live
  ;; together in the same image this will have to change (cim 9/16/96) 
    (cg::kill-acl-timer-exit-fn nil)
    
    (setf clim-window-proc-address (init-clim-win-proc clim-window-proc-address #.(cg::make-cstructure 0 16)))
    (setf clim-ctrl-proc-address (init-clim-ctrl-proc clim-ctrl-proc-address #.(cg::make-cstructure 0 16)))
    (setq *clim-initialized* t)))

#+aclpc
(defun ensure-clim-initialized ()
  (unless *clim-initialized*
    (init-clim-win-proc clim-window-proc-address #.(cg::make-cstructure 0 16))
    (init-clim-ctrl-proc clim-ctrl-proc-address #.(cg::make-cstructure 0 16))
    (setq *clim-initialized* t)))

#+acl86win32
(defun acl-clim::register-window-class ()
  ;; initialise window procedure
  (unless *wndclass-registered*
    (init-clim-win-proc clim-window-proc-address #.(cg::make-cstructure 0 16))
    (let ((class (cg::ccallocate win::wndclassex)))
      (ct::csets win::wndclassex class
                 win::cbSize (cg::sizeof win::wndclassex)
                 win::style win::CS_DBLCLKS
                 win::lpfnwndproc clim-window-proc-address ;cg::window-proc-address
                 win::cbClsExtra 0
                 win::cbWndExtra 0
                 win::hinstance  pc::*hinst*
                 win::hicon (win::LoadIcon pc::*hinst* "ICON_LISP") ;(ct::null-handle win::hicon)
                 win::hcursor (ct::null-handle win::hcursor)
                 win::hbrbackground (ct::null-handle win::hbrush)
                 win::lpszmenuname ct::hnull ;*menu-name*
                 win::lpszclassname *clim-class*
                 win::hIconSm 0
                 )
      (setq err1 (win::getLastError))
      (if (zerop (setq reg (win::str_registerclassex class *clim-class* 0)))
        (error "CLIM window class could not be registered."))
      (setq err2 (win::getLastError))
      (setq *wndclass-registered* t))    
    ))

;;; "CreateWindowEx"
;;;(dword lpctstr lpctstr dword int int int int hwnd hmenu handle lpstr)
;;; "CreateWindow"
;;;(      lpctstr lpctstr dword int int int int hwnd hmenu handle lpstr) hwnd 351 %oscall)

(defun create-overlapped-window (parent pretty scroll
					left top width height native)
  (let ((winstyle (logior win::ws_overlapped
			  (if (or (eql scroll :both)(eql scroll :vertical))
			    win::ws_vscroll 0)
			  (if (or (eql scroll :both)(eql scroll :horizontal))
			    win::ws_hscroll 0)
			  win::ws_clipchildren
			  win::ws_caption
			  win::ws_thickframe
			  win::ws_sysmenu
			  win::ws_minimizebox
			  win::ws_maximizebox))
        (*win-name* *win-name*)
        (cstr (ct::callocate (:char *) :size 256))
	(subsize (when pretty (length pretty)))
	(menu
	  (if native
	    (win::CreateMenu)
	    (ct::null-handle win::hmenu)))
	(window nil))
    (when pretty
	  #+aclpc
      (dotimes (i subsize)
        (ct:cset (:char 256) cstr ((fixnum i)) (char-int (char pretty i))))
	  #+aclpc
      (setq *win-name* cstr)
	  #+acl86win32
      (setq *win-name* pretty))
    (setq window
          (#+acl86win32 win::createWindowEx #+acl86win32 0
		   #+aclpc win::createWindow
		*clim-class*
		*win-name*
		winstyle
		left top width height
		parent
		menu
		pc::*hinst*
        #+acl86win32			 
		*win-x* ;(setq *win-x* (symbol-name (gensym)))
		#+aclpc *win-arg*)) ;*win-x*
    #+acl86win32
    (if (cg::null-handle-p window window)
        (error "in create-overlapped-window: (createWindowEx 0 ~s ~s ~a ~a ~a ~a ~a ~a ~a ~a ~s) Failed~%"
               *clim-class*
               *win-name*
               winstyle
               left top width height
               parent
               menu
               pc::*hinst*
               *win-x*))
    window))

(defun create-pop-up-window (parent pretty scroll left top width height ovl)
  (let* ((overlap (if ovl
		    (logior ; win::ws_overlapped
			    win::ws_caption win::ws_sysmenu
			    ; win::ws_minimizebox win::ws_maximizebox
			    ; win::ws_thickframe
			    win::ws_border
			    )
		    win::ws_border))
         (winstyle (logior overlap
			  win::ws_popup
			  (if (or (eql scroll :both)(eql scroll :vertical))
			    win::ws_vscroll 0)
			  (if (or (eql scroll :both)(eql scroll :horizontal))
			    win::ws_hscroll 0)
			  win::ws_clipchildren
			  ))
         (*win-name* *win-name*)
         (cstr (ct::callocate (:char *) :size 256))
	 (subsize (when pretty (length pretty)))
	 (window nil))
    (when pretty
      #+aclpc
	  (dotimes (i subsize)
        (ct:cset (:char 256) cstr ((fixnum i)) (char-int (char pretty i))))
	  #+aclpc
      (setq *win-name* cstr)
	  #+acl86win32
      (setq *win-name* pretty))
    (let ((window (#+acl86win32 win::createWindowEx #+acl86win32 0
 		           #+aclpc win::createWindow
                  *clim-class*
                  *win-name*
                  winstyle
                  left top width height
                  parent
                  (ct::null-handle win::hmenu)
                  pc::*hinst*
                  #+acl86win32 *win-x* ;(setq *win-x* (symbol-name (gensym)))
				  #+aclpc *win-arg*)))
	  #+acl86win32
      (if (cg::null-handle-p window window)
          (error "in create-pop-up-window: (createWindowEx 0 ~s ~s ~a ~a ~a ~a ~a ~a ~a ~a ~a) Failed~%"
                 *clim-class*
                 *win-name*
                 winstyle
                 left top width height
                 parent
                 (ct::null-handle win::hmenu)
                 pc::*hinst*
                 *win-x*))
      window)))


(defun create-child-window (parent pretty scroll left top width height)
  (let ((winstyle (logior win::ws_clipchildren
                          win::ws_child
			  win::ws_border
			  ; win::ws_thickframe repaint prob + 2 much realestate
			  (if (member scroll '(t :both :dynamic :vertical))
			    win::ws_vscroll 0)
			  (if (member scroll '(t :both :dynamic :horizontal))
			    win::ws_hscroll 0)
			  win::ws_clipsiblings))
        (*win-name* *win-name*)
        (cstr (ct::callocate (:char *) :size 256))
	(subsize (when pretty (length pretty)))
	(window nil))
    (when pretty
      #+aclpc
	  (dotimes (i subsize)
        (ct:cset (:char 256) cstr ((fixnum i)) (char-int (char pretty i))))
	  #+aclpc
      (setq *win-name* cstr)
	  #+acl86win32
      (setq *win-name* pretty))
    (setq window
	  (#+acl86win32 win::createWindowEx #+acl86win32 0
		   #+aclpc win::createWindow
		*clim-class*
		*win-name*
		winstyle
		left top width height
		parent
		(ct::null-handle win::hmenu)
		pc::*hinst*
		#+acl86win32
		*win-x* ;(setq *win-x* (symbol-name (gensym)))
		#+aclpc *win-arg*))
	#+acl86win32
    (if (cg::null-handle-p window window)
        (error "in create-child-window (createWindowEx 0 ~s ~s ~a ~a ~a ~a ~a ~a ~a ~a ~a) Failed~%"
               *clim-class*
               *win-name*
               winstyle
               left top width height
               parent
               (ct::null-handle win::hmenu)
               pc::*hinst*
               *win-x*))
    (if (or (eql scroll :both)(eql scroll :vertical))
      (win::setScrollRange window win::SB_VERT 0 *win-scroll-grain* 1))
    (if (or (eql scroll :both)(eql scroll :horizontal))
      (win::setScrollRange window win::SB_HORZ 0 *win-scroll-grain* 1))
    window))

(defvar wres  (ct::callocate :long))
(defvar wmsg  (ct::ccallocate win::msg)) 

(defun wait-for-event ()
  (let (;(msg (ct::ccallocate win::msg))
        ;(res (cg::callocate :long))
        )
    (when (prog1
            (win::peekMessage wmsg (ct::null-handle win::hwnd) 0 0
                              (logior win::PM_NOYIELD win::PM_NOREMOVE) wres)
            (not (and (zerop (cg::hiword wres)) (zerop (cg::loword wres)))))
      t)))

(defvar msg (ct::ccallocate win::msg))
(defvar res (ct::callocate :long))

;;--- this never gets called on NT because we do a
;;--- sys::process-pending-events instead.
#+acl86win32
(defun await-response (waitp)
  (if waitp
      (progn
        (win::getMessage msg (ct::null-handle win::hwnd) 0 0 #+acl86win32x res)
        (win::TranslateMessage msg #+acl86win32x :static)
        (win::dispatchMessage msg)
	msg)
    (let* ((ret (win::peekMessage msg (ct::null-handle win::hwnd) 0 0 win::PM_REMOVE
				  #+acl86win32x res))
	   (do-it #-acl86win32x ret
		  #+acl86win32x (not (and (zerop (cg::hiword res))
					  (zerop (cg::loword res))))))
      (when do-it
	(win::TranslateMessage msg #+acl86win32x :static)
	(win::dispatchMessage msg)))))

#+aclpc
(defun await-response (waitp)
  (let ((msg (ct::ccallocate win::msg)))
    (if waitp
	(progn 
	  (win::getMessage msg (ct::null-handle win::hwnd) 0 0)
	  (win::TranslateMessage msg)
	  (win::dispatchMessage msg)
	  msg)
      (when (win::peekMessage msg (ct::null-handle win::hwnd) 0 0 win::PM_REMOVE)
        (win::TranslateMessage msg)
        (win::dispatchMessage msg)))))


