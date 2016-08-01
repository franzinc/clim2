;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements frame manager specialization for the ACL for Windows *
*  port.  It supplies support for using menu bars and pointer documentation  *
*  panes, as well as supplying file and notification dialogs.                *
*                                                                            *
*                                                                            *
****************************************************************************|#

;; DO: This file contains several IN-PACKAGE forms.  That's very confusing
;; and we should get rid of them. (alemmens, 2005-11-30)

(in-package :acl-clim)

(defvar *use-native-menubar* t)
(defvar *in-layout-frame* nil)

(defclass acl-frame-manager (standard-frame-manager)
    ((msmenubart :initarg :msmenubart :reader msmenubart))
  (:default-initargs :dialog-view +gadget-dialog-view+
		     :msmenubart t))

(defmethod make-frame-manager ((port acl-port) &key palette)
  (make-instance 'acl-frame-manager
		 :port port
		 :palette palette
		 :msmenubart *use-native-menubar*))

(defmethod frame-wrapper ((framem acl-frame-manager)
			  (frame standard-application-frame) pane)
  (let ((menu-bar (slot-value frame 'menu-bar))
	(native *use-native-menubar* #+ignore (slot-value frame 'msmenubart))
	(pointer-doc-pane nil)
	(pointer-options
	 (clim-internals::frame-pointer-documentation-p frame)))
    (when				;mm: was: (eq menu-bar 't)
        (silica::default-command-table-p menu-bar)
      (setq menu-bar (frame-command-table frame)))
    (when native (setf (getf (frame-properties frame) :native-menu) t))
    (when pointer-options
      (setq pointer-doc-pane
	(with-look-and-feel-realization (framem frame)
	  (apply #'make-pane
		 'clim-internals::pointer-documentation-pane
		 :name :pointer-documentation
		 (append (and (listp pointer-options) pointer-options)
			 `(:max-width ,+fill+
				      ;; commented out hard-wired
				      ;; white background (cim 10/9/96)
				      #| :background ,+white+ |#
				      :min-height (1 :line)
				      :max-height (1 :line)
				      :height (1 :line)))))))
    #+ignore ;; rl: wanted to do the menu bar here, but can't
    (when (and native menu-bar)
      (compute-msmenu-bar-pane frame menu-bar))
    (with-look-and-feel-realization (framem frame)
      (cond ((and (not native) menu-bar pointer-doc-pane)
	     (vertically ()
	       (compute-menu-bar-pane frame menu-bar)
	       pane
	       pointer-doc-pane))
	    ((and (not native) menu-bar)
	     (vertically () (compute-menu-bar-pane frame menu-bar) pane))
	    (pointer-doc-pane
	     (vertically () pane pointer-doc-pane))
	    (t pane)))))

(defun record-accelerator (frame keysym command &optional sheet)
  (unless sheet
    (setq sheet (frame-top-level-sheet frame)))
  (pushnew (cons keysym command)
	   (slot-value sheet 'accelerator-gestures)
	   :test #'equal))

(defun keysymeql (keysyma keysymb)
  (or (eql keysyma keysymb)
      (cond ((and (keywordp keysyma) (characterp keysymb))
	     (eql keysyma (char->keysym keysymb))
	     )
	    ((and (keywordp keysymb) (characterp keysyma))
	     (eql keysymb (char->keysym keysyma)))
	    (t nil))))

(defun modstateeql (a b) (eql a b))

(defun lookup-accelerator (frame keysym modstate)
  (when clim-internals::*input-buffer-empty* 
    ;; Ensure that command-accelerators are only enabled
    ;; when we are not in the middle of an input-editor context.
    ;; spr18494
    (let* ((sheet (frame-top-level-sheet frame))
	   (gestures (top-level-sheet-accelerator-gestures sheet)))
      (loop with gkeysym and gmodstate
	  for gesture-and-command in gestures
	  for (gesture) = gesture-and-command
	  do (multiple-value-setq (gkeysym gmodstate)
	       (parse-gesture-spec gesture))
	  when (and (keysymeql keysym gkeysym)
		    (modstateeql modstate gmodstate))
	  return (cdr gesture-and-command)))))

(defmethod update-frame-settings ((framem acl-frame-manager) 
				  (frame t))
  (let* ((sheet (frame-top-level-sheet frame)))
    (when sheet
      (let* ((sr (compose-space sheet))
	     (width (space-requirement-min-width sr))
	     (height (space-requirement-min-height sr)))
	(clim-internals::limit-size-to-graft width height (graft framem))
	(multiple-value-bind (dl dt dw dh) (get-nonclient-deltas sheet)
	  (declare (ignore dl dt))
	  (setf (acl-top-min-width sheet) 
	    (fix-coordinate (+ width dw)))
	  (setf (acl-top-min-height sheet)
	    (fix-coordinate (+ height dh))))
	(values width height)))))

;; added the following two methods so that the default labels are the
;; same as for the Xt port (cim 9/25/96)

(defmethod frame-manager-exit-box-labels ((framem acl-frame-manager) frame view)
  (declare (ignore frame view))
  '((:exit  "OK" :documentation "Exit from dialog" :show-as-default t)
    (:abort  "Cancel" :documentation "Cancel dialog")))

(defmethod frame-manager-default-exit-boxes ((framem acl-frame-manager))
  '((:exit) (:abort)))

;; Note: items appear never to be removed from menu-id->command-table
;; so this will grow indefinitely (cim 9/9/96). There really should be
;; a global id counter and the association list should be kept on a
;; per application-frame basis. 

(defun assign-command-menu-item-id (command frame)
  ;; We should probably allow COMMAND to be NIL.
  ;; It is allowed for the UNIX port, apparently.
  (when (atom command) (setq command (list command)))
  (prog1
    (fill-pointer (menu-id->command-table *acl-port*))
    (vector-push-extend (cons frame command) (menu-id->command-table *acl-port*)
			256)))

(defun find-command-menu-item-id (command frame)
  (position-if
   #'(lambda (x)
       (when x
	 (let ((f (first x))
	       (c (second x)))
	   (and (eq f frame)
		(eq c command)))))
   (menu-id->command-table *acl-port*)))

(defun map-command-menu-ids (frame func &rest args)
  (dotimes (id (fill-pointer (menu-id->command-table *acl-port*)))
    (let ((x (aref (menu-id->command-table *acl-port*) id)))
      (when x
	(let ((f (first x)))
	  (if (eq f frame) (apply func id args)))))))

;;; The following two methods are called as a pair (see
;;; CLIM-INTERNALS::WITH-MENU-DISABLED).
;;;
;;; In DISABLE-ALL-MENU-ITEMS:
;;;  1] Remember the previously disabled-commands;
;;;  2] Disable all the defined commands;
;;;  3] Return the list of the previously disabled-commands.
;;; 
;;; In RE-ENABLE-MENU-ITEMS:
;;;  1] Record the currently disabled-commands;
;;;  2] re-Disable all the commands that were previously
;;;     disabled if and only if they are still disabled.
;;;
;;; This last condition worries about any commands that
;;; have been actively enabled since the corresponding
;;; call to DISABLE-ALL-MENU-ITEMS.

(defun clim-internals::disable-all-menu-items (frame)
  ;;; Called before a command is executed.
  ;;;
  ;;; Push all the commands on top of the 
  ;;; previously-disabled commands.
  ;;;
  ;;; In particular note that any previously-disabled
  ;;; command will appear in the list of disabled-commands
  ;;; two (or possibly more) times.
  ;;;
  ;;; See the subequent clean-up below in 
  ;;; clim-internals::re-enable-menu-items.
  (map-command-menu-ids
   frame
   #'(lambda (menuid)
       (let ((command-name (second (aref (menu-id->command-table *acl-port*)
					 menuid))))
	 (with-slots (clim-internals::disabled-commands) frame
	   (push command-name clim-internals::disabled-commands)))))
  )

;;; See the comment for DISABLE-ALL-MENU-ITEMS
(defun clim-internals::re-enable-menu-items (frame)
  (let ((new-disabled-commands nil))
    (with-slots (clim-internals::disabled-commands) frame
      
      ;;; Called after a command has finished executing.
      ;;; (See set-up above in clim-internals::disable-all-menu-items.)
      ;;;
      ;;; Re-Disable a command if and only if it appears
      ;;; in the list of disabled-commands twice.
      ;;; 
      ;;; 1] If a command has been actively enabled during
      ;;;    the execution of the present-command, all 
      ;;;    appearances in the list of disabled-commands
      ;;;    will have been removed.
      ;;; 2] If a command has been actively disabled during
      ;;;    the execution of the present-command, it
      ;;;    will appear in the list two (or more) times.
      ;;; 3] If a command was disabled before the 
      ;;;    presently-exectuted command began, it should
      ;;;    appear in the list two (or more) time.
      ;;;
      ;;; Note that this scheme loses if the presently-executing
      ;;; command first actively enables a command and then
      ;;; actively disables it (i.e. at the end the command
      ;;; will appear in the list only once, and so not be
      ;;; disabled here).
      ;;;
      ;;; Note also this assume that the commands are being
      ;;; en/dis-abled throught the standard (setf command-enabled)
      ;;; mechanism.  I.e. the user's code hasn't been
      ;;; directly munging the disabled-command slot.
      (loop for (com . rest) on clim-internals::disabled-commands
	  do (when (member com rest)
	       (pushnew com new-disabled-commands)))
      
      (setq clim-internals::disabled-commands
	new-disabled-commands))))      

;;; Either of these would be nicer, but redisplay of the menu bar causes them to not
;;; always get repainted in their ungrayed state at the end.  pr Aug97

;(setf (command-enabled command-name frame) enablep)
;(win:EnableMenuItem menu menuid (if enablep win:MF_ENABLED win:MF_GRAYED))

(defun keysym->char (keysym)
  (if (characterp keysym)
      keysym
  (let ((entry (rassoc keysym *keysym-alist*))
	(code (position keysym *char->keysym*)))
    (if entry
	(car entry)
      (when code
	(code-char code))))))

(defun gesture-spec-for-mswin (gesture-spec)
  (let* ((alist '((:shift   "SHIFT+")
		  (:control "CTRL+")
		  (:meta    "ALT+")))
	 (keypress (first gesture-spec))

	 ;; bug13218 Menu accelerator is not always a character
	 (char (keysym->char keypress))
	 (nlist (list (if char
                          (format nil "~:C" (if (member :shift gesture-spec)
					    (keysym->char keypress)
					  (char-downcase (keysym->char keypress))))
			keypress)))
	 
	 )
    (dolist (shift alist)
      (when (member (first shift) (rest gesture-spec))
	(push (second shift) nlist)))
    (format nil "~A~{~A~}" #\tab nlist)))

(defun strlen (str)
  ;; compute length of null terminated string in a big buffer
  (typecase str
    (string (dotimes (i (length str) (length str))
	      (when (eq #\null (aref str i)) (return i))))
    (otherwise (error "???"))))

(defun set-strlen (stringvar newlen)
  ;;mm: Add a terminating zero-char for C code.
  (setf (aref stringvar newlen) #\null)
  (setf (fill-pointer stringvar) newlen))

(eval-when (compile load eval)
  (defconstant *nstringify-buffer-default-size* 2048)
  (defvar nstringify-buffer 
      (make-array *nstringify-buffer-default-size* :fill-pointer t 
		  :element-type 'character))
  (defvar control-text-buffer
      (make-array 2048 :fill-pointer t :element-type 'character)))

(defun nstringify (x)
  ;; This function is now a misnomer.  It is not destructive.
  (typecase x
    (simple-string x)
    (string (coerce x 'simple-string))
    (null "")
    (t (prin1-to-string x))))

(defun nstringify-for-control (x)
  ;; This function is now a misnomer.  It is not destructive.
  (typecase x
    ((not symbol)
     (nstringify x))
    (null "")
    (t
     (string-upcase (nstringify x)))))

(defun schar-byte (string index)
  (if (< index (length string))
      ;; Use aref so it will work on adjustable arrays
      (char-int (aref string index))
    0))

(defun set-schar-byte (string index new)
  (if (< index (length string))
      (etypecase string
	((simple-array character (*))
	 (setf (schar string index) (code-char new)))
	(array
	 (setf (aref string index) (code-char new)))
	))
  new)

(defun make-control-text (x)
  (let ((string (nstringify-for-control x)))
    ;;mm: This is not MP-safe ???
    ;;mm: allow access to the whole buffer 
    #+aclmerge (setf (fill-pointer control-text-buffer) 2047)
    (set-strlen 
     control-text-buffer
     ;; copy characters across and return final length
     (block nil				; macroexpanded the FOR loop to get
					; this... jpm.
       (let* ((to-index 0) (from-index 0) (char-byte nil) (last-char-byte nil))
	 (declare (fixnum to-index) (fixnum from-index))
	 (tagbody
	  for-loop
	   (when (or (>= from-index #.(length nstringify-buffer)) 
		     (>= to-index #.(length control-text-buffer))) 
	     (go for-exit))
	   (setq last-char-byte char-byte)
	   (setq char-byte (schar-byte string from-index))
	   (set-schar-byte control-text-buffer to-index char-byte)
	   (case char-byte
	     (0 (go for-exit))
	     (#.(char-int #\&)
		(when (< from-index #.(1- (length nstringify-buffer)))
		  (set-schar-byte control-text-buffer 
				  (incf to-index) 
				  #.(char-int #\&))))
	     (#.(char-int #\~)
		(if (eql last-char-byte #.(char-int #\~))
		    (progn (set-schar-byte control-text-buffer
					   (decf to-index) 
					   #.(char-int #\~))
			   (setf char-byte 0))
		  (set-schar-byte control-text-buffer to-index 
				  #.(char-int #\&)))))
	   (incf to-index 1)
	   (incf from-index 1)
	   (go for-loop)
	  for-exit)
	 to-index)))
    ;; return the control text
    control-text-buffer))

(defun make-menu-text (text keystroke item)
  (let* ((mnemonic (getf (command-menu-item-options item) :mnemonic))
	 (pos (position mnemonic text))
	 (newtext (if pos
		      (format nil "~A~~~A"
			      (subseq text 0 pos)
			      (subseq text pos))
		    text)))
    (make-control-text (if keystroke
			   (format nil "~A~A"
				   newtext
				   (gesture-spec-for-mswin keystroke))
			 newtext))))

(defun delete-menu-bar (frame menuhandle &optional redraw)
  (let ((count (win:GetMenuItemCount menuhandle)))
    (when (plusp count)
      (dotimes (position count)
	;; Use position = 0 every time because the menu items get
	;; renumbered each time through this loop.
	(win:DeleteMenu menuhandle 0 win:MF_BYPOSITION))
      (when redraw
	(win:DrawMenuBar (sheet-mirror (frame-top-level-sheet frame)))
	))))

(defun make-menu-for-command-table (command-table menuhand frame 
				    &optional top-level-sheet top-level-p)
  (assert (valid-handle menuhand))
  (unless top-level-sheet
    (setq top-level-sheet (frame-top-level-sheet frame)))
  ;; First, delete any pre-existing menu items.
  (delete-menu-bar frame menuhand t)
  (setf (gethash menuhand (popup-menu->menu-item-ids *acl-port*)) nil) 
  (setf (gethash menuhand *popup-menu->command-table*) 
    (list command-table 
	  (slot-value (find-command-table command-table)
		      'clim-internals::menu-tick)))
  ;; Make the menu
  (map-over-command-table-menu-items
   #'(lambda (menu keystroke item)
       (let* ((type (command-menu-item-type item))
	      (value (command-menu-item-value item))
	      (menu-item-available-p 
	       (or (not (eq type :command))
		   (command-enabled (car value) frame)))
	      (menu-item-selected-p nil)
	      (acckey keystroke)
	      (flags (logior
		      win:MF_STRING
		      (if menu-item-available-p
			  win:MF_ENABLED win:MF_GRAYED)
		      (if menu-item-selected-p
			  win:MF_CHECKED win:MF_UNCHECKED)))
	      (smflags (logior flags win:MF_POPUP)))
	 (case type
	   (:command
	    (when acckey
	      (record-accelerator frame acckey value top-level-sheet))
	    (let ((menu-item-id (assign-command-menu-item-id value frame)))
	      (excl:with-native-string (m (make-menu-text
					   menu 
					   ;; Don't display the accelerator key if
					   ;; the command is going to land on the
					   ;; menu bar itself,
					   ;; Windows will not correctly display
					   ;; such text.
					   (if top-level-p nil acckey)
					   item))
		(win:AppendMenu
		 menuhand
		 flags
		 menu-item-id
		 m))
	      (push menu-item-id (gethash menuhand (popup-menu->menu-item-ids *acl-port*)))))
	   (:function
	    (warn ":function not yet implemented in menu bars")
	    )
	   (:menu
	    (let* ((submenu (win:CreatePopupMenu))
		   (submenu-handle (ct:handle-value 'win:hmenu submenu))
		   (menutext (make-menu-text menu acckey item)))
	      (excl:with-native-string (menutext menutext)
		(win:AppendMenu menuhand
				smflags
				submenu-handle
				menutext))
	      (make-menu-for-command-table value submenu-handle frame top-level-sheet)))
	   (:divider
	    (excl:with-native-string (x "x")
	      (win:AppendMenu menuhand
			      win:MF_SEPARATOR
			      0
			      x))
	    ))))
   command-table))

(defun compute-msmenu-bar-pane (frame top command-table)
  (let* ((mirror (sheet-mirror top))
	 (menu-handle (win:GetMenu mirror)) 
	 (command-table
	  (if (listp command-table) (car command-table) command-table)))
    (when (silica::default-command-table-p command-table)
      ;; command-table arg comes from menu-bar slot of frame
      ;; and may be NIL T=menu-hbox-pane command-table-arg
      (setq command-table (frame-command-table frame)))
    (when (valid-handle menu-handle)
      (make-menu-for-command-table command-table menu-handle frame top t))))

(defun update-menu-contents (sheet menuhand index)
  ;; Called just before making a menu active.  If the menu is associated
  ;; with a command table, make sure to rebuild the menu if the command
  ;; table has changed recently.  Returns T if the menu was rebuilt.
  (declare (ignore index))
  (let* ((frame (pane-frame sheet))
	 (pair (gethash menuhand *popup-menu->command-table*))
	 (command-table (first pair))
	 (tickthen (second pair))
	 (ticknow (when tickthen (slot-value (find-command-table command-table)
					     'clim-internals::menu-tick))))
    (when (and tickthen ticknow (not (= tickthen ticknow)))
      (make-menu-for-command-table command-table menuhand frame 
				   (frame-top-level-sheet frame) t)
      t)))

(defun update-menu-item-sensitivities (hmenu)
  ;; Called just before making a menu active.  If the menu is associated
  ;; with a command table, make sure to enable or gray the menu
  ;; items appropriately.
  (dolist (menu-item-id (gethash hmenu (popup-menu->menu-item-ids *acl-port*)))
    (let* ((item (aref (menu-id->command-table *acl-port*) menu-item-id))
	   (frame (first item))
	   (command (second item))
	   (top (frame-top-level-sheet frame))
	   (mirror (sheet-mirror top))
	   (menu-handle (win:GetMenu mirror)))
      (win:EnableMenuItem menu-handle menu-item-id
			   (if (command-enabled command frame)
			       win:MF_ENABLED
			     win:MF_GRAYED)))))

(defmethod frame-send-message (frame a b c d)
  (let* ((me mp:*current-process*)
	 (sheet (frame-top-level-sheet frame))
	 (him (when sheet (clim-internals::sheet-thread sheet))))
    (unless (eq me him)
      ;; SendMessage will block, awaiting the response from the other thread.
      ;; If the other thread sends a message to me, then you have a deadlock.
      ;; So we have to have this restriction.
      (error "SendMessage: attempt to send a message to a window in another thread"))
    (win:SendMessage a b c d)))

(defmethod frame-update-window (frame handle)
  (let* ((me mp:*current-process*)
	 (sheet (frame-top-level-sheet frame))
	 (him (when sheet (clim-internals::sheet-thread sheet))))
    (unless (eq me him)
      ;; SendMessage will block, awaiting the response from the other thread.
      ;; If the other thread sends a message to me, then you have a deadlock.
      ;; So we have to have this restriction.
      (error "UpdateWindow: attempt to send a message to a window in another thread"))
    ;; Don't signal errors for UpdateWindow, just warn. JPM 3/19/99.
    (or (win:UpdateWindow handle)
	(acl-clim::check-last-error "UpdateWindow" :action :warn))))

(defmethod initialize-tooltips ((frame standard-application-frame))
  ;; Create a tooltip control associated with this frame.
  ;; Note that this control won't do anything unless you
  ;; set up a message relay to evesdrop on the messages being
  ;; sent to other controls.
  (let* ((sheet (frame-top-level-sheet frame))
	 (tooltip-control (tooltip-control sheet))
	 (toolinfo (ct:ccallocate toolinfo))
	 (TTM_ADDTOOL #+ics #.(+ win:WM_USER 50) ;; TTM_ADDTOOLW
		      #-ics #.(+ win:WM_USER 4)  ;; TTM_ADDTOOLA
		      )
	 (TTM_ACTIVATE #.(+ win:WM_USER 1))
	 (TTF_IDISHWND 1)
	 (TTS_ALWAYSTIP 1)
	 (status nil))
    (unless tooltip-control
      (win:InitCommonControls)
      (setq tooltip-control
	(excl:with-native-string (s "tooltips_class32")
	  (win:CreateWindow s
			    0
			    TTS_ALWAYSTIP
			    win:CW_USEDEFAULT win:CW_USEDEFAULT
			    win:CW_USEDEFAULT win:CW_USEDEFAULT 
			    0 0
			    (hinst *acl-port*) 0)))
      (setf (tooltip-control sheet) tooltip-control)
      (when (zerop tooltip-control)
	(check-last-error "CreateWindow" :action :warn)
	(return-from initialize-tooltips nil))
      (frame-send-message
       (pane-frame sheet)
       tooltip-control TTM_ACTIVATE 1 0)
      (flet ((tip (s)
	       ;; I never got tool tips to work, so I didn't
	       ;; really finish this part.  JPM 8/98.
	       (ct:csets 
		toolinfo toolinfo
		cbsize (ct:sizeof toolinfo)
		uflags TTF_IDISHWND
		hwnd (sheet-mirror sheet)
		uid (sheet-mirror s)
		;;rect 0
		hinst (hinst *acl-port*)
		lpsztext -1 
		#+ign
		(lisp-string-to-scratch-c-string 
		 (princ-to-string label)))
	       (setq status
		 (frame-send-message frame
				     tooltip-control
				     TTM_ADDTOOL 0 toolinfo))
	       (when (zerop status)
		 (return-from initialize-tooltips nil))))
	(declare (dynamic-extent #'tip))
	(map-over-sheets #'tip sheet)
	tooltip-control))))

(defmethod redisplay-frame-panes :around ((frame standard-application-frame)
					  &key force-p)
  (declare (ignore force-p))
  (call-next-method)
  ;;; ensure that the top-level-sheet is visible, mostly for avp frames
  #-ignore ;;-- thought adding this unless would fix growing pains but it didn't.
  (unless clim-internals::*sizing-application-frame*
    (setf (sheet-enabled-p (frame-top-level-sheet frame)) t))
  #+ignore
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) t))

(defmethod run-frame-top-level :before ((frame standard-application-frame)
					&key &allow-other-keys)
  (initialize-tooltips frame)
  (let* ((sheet (frame-top-level-sheet frame))
	 (thread (when sheet (clim-internals::sheet-thread sheet))))
    (unless (eq thread (current-process))
      ;; Lisp may hang badly if you proceed.
      (cerror "I don't care if the application crashes or hangs" 
              "An attempt was made to display a window in thread ~S,
which is not the thread in which the window was created.
Windows does not allow a window created in one thread 
to be run from another."
	     thread))))


;; Following var macro and function are part of bug12221 mods.
(defvar +batch-menubar-refresh+ nil)
;;; An effeciency work-around.
;;; Refresh the menubar once (rather than each time
;;; the command-button is enabled/disabled).
(defmacro with-batch-menubar-refresh ((frame) &body body)
  `(let ((+batch-menubar-refresh+ t))
     (progn ,@body)
     (menubar-refresh-1 ,frame)
     ))
(defun menubar-refresh-1 (frame)
  (let* ((top (frame-top-level-sheet frame))
	 (mirror (sheet-mirror top))
	 (menu-handle (win:GetMenu mirror)))
    (when menu-handle
      (win:DrawMenuBar mirror))))



;;--- Should "ungray" the command button, if there is one
(defmethod note-command-enabled ((framem acl-frame-manager) frame command)
  (when (consp command) (setf command (car command)))
  (let* ((top (frame-top-level-sheet frame))
         (mirror (sheet-mirror top))
         (menu-handle (win:GetMenu mirror))
         (command-id (find-command-menu-item-id command frame))
         (flag win:MF_ENABLED))
    (when menu-handle
      (win:EnableMenuItem menu-handle command-id flag)

      ;; bug12221/spr24998
      ;; If this is a button, make sure the "ungray"
      ;; is immediately seen.
      
;;; Note from docs for Windows function DrawMenuBar.
;;; The DrawMenuBar function redraws the menu bar of the specified
;;; window. If the menu bar changes after Windows has created the
;;; window, this function must be called to draw the changed menu bar.
      
      ;; (Unfortunately, there doesn't seem to be any
      ;; simpler way than refreshing the entire menubar.)
      (when (not +batch-menubar-refresh+)
	(win:DrawMenuBar mirror))

      )))

;;--- Should "gray" the command button, if there is one
(defmethod note-command-disabled ((framem acl-frame-manager) frame command)
  (when (consp command) (setf command (car command)))
  (let* ((top (frame-top-level-sheet frame))
         (mirror (sheet-mirror top))
         (menu-handle (win:GetMenu mirror))
         (command-id (find-command-menu-item-id command frame))
         (flag win:MF_GRAYED))
    (when menu-handle
      (win:EnableMenuItem menu-handle command-id flag)
      
      ;; bug12221/spr24998
      ;; If this is a button, make sure the "ungray"
      ;; is immediately seen.
      ;; (Unfortunately, there doesn't seem to be any
      ;; simpler way than refreshing the entire menubar.)
      (when (not +batch-menubar-refresh+)
	(win:DrawMenuBar mirror))
      
      )))

(defmethod note-frame-enabled ((framem acl-frame-manager) frame)
  (declare (ignore frame))
  nil)

(defmethod note-frame-disabled ((framem acl-frame-manager) frame)
  (declare (ignore frame))
  nil)

(defmethod note-frame-iconified ((framem acl-frame-manager) frame)
  (win:CloseWindow (sheet-mirror (frame-top-level-sheet frame))))

(defmethod note-frame-deiconified ((framem acl-frame-manager) frame)
  (win:OpenIcon (sheet-mirror (frame-top-level-sheet frame))))

(defmethod accept-values-pane-p ((object t)) nil)
(defmethod accept-values-pane-p ((object accept-values-pane)) t)

(defmethod note-frame-enabled :around ((framem acl-frame-manager) frame)
  (call-next-method)
  (let ((*in-layout-avp* *in-layout-avp*)
	(sheet (frame-top-level-sheet frame))
	(avp nil))
    (when sheet
      (map-over-sheets #'(lambda (sheet)
			   (when (accept-values-pane-p sheet)
			     (setf avp t)))
		       sheet)
      (setf *in-layout-avp* avp)
      (setf (sheet-enabled-p sheet) t)
      (raise-sheet sheet))))

(defmethod note-frame-enabled :after ((framem acl-frame-manager) frame)
  (update-frame-settings framem frame)
  ;;--- Perhaps we want to resize the top level sheet if there is one
  (let ((avp nil)
        (*in-layout-frame* *in-layout-frame*))    
    (when (frame-top-level-sheet frame)
      (map-over-sheets #'(lambda (sheet)
                           (when (accept-values-pane-p sheet)
                             (setf avp t)))
                       (frame-top-level-sheet frame))
      (setf *in-layout-frame* avp)
      (setf (sheet-enabled-p (frame-top-level-sheet frame)) t))))

(defmethod note-frame-layout-changed :after ((framem acl-frame-manager) frame)
  ;; added this to workaround some bugs with new layouts not being
  ;; correctly redisplayed - in particular problems with label-panes
  ;; - this should be viewed as a temporary fix (cim 10/14/96) 
  (repaint-sheet (frame-top-level-sheet frame) +everywhere+)
  ;; spr16580.
  ;; Added this next one to fix problem with distribute-event
  ;; after changing the layout of the frame.  Need to clear
  ;; port-trace-thing, otherwise buttons may "go dead" due to a failure
  ;; in event distribution. What I don't understand is why Motif doesn't
  ;; have to do this. jpm 12/12/97.
  (setf (fill-pointer (port-trace-thing (port (frame-manager frame)))) 0)
  )

(defmethod frame-manager-note-pretty-name-changed
    ((framem acl-frame-manager)
     (frame standard-application-frame))
  (let ((name (frame-pretty-name frame))
        (sheet (frame-top-level-sheet frame)))
    (when name
      (or (excl:with-native-string (cstr name)
	    (win:SetWindowText (sheet-mirror sheet) cstr))
	  (check-last-error "SetWindowText" :action :warn)))))

(defun select-messagebox-icon (style)
  ;; Decides which Windows icon matches this (standardized) style. 
  (if (member style '#.`(,win:MB_ICONINFORMATION
			 ,win:MB_ICONQUESTION
			 ,win:MB_ICONEXCLAMATION
			 ,win:MB_ICONSTOP
			 ,win:MB_ICONHAND ; ?
			 ,win:MB_ICONASTERISK ; ?
			 ))
      style				; user knows what they want
    (case style
      (:message win:MB_ICONINFORMATION)
      (:inform win:MB_ICONINFORMATION)
      (:question win:MB_ICONQUESTION)
      ((:warn :warning) win:MB_ICONEXCLAMATION)
      (:error win:MB_ICONSTOP)
      (otherwise win:MB_ICONINFORMATION))))

(defun select-messagebox-buttons (exit-boxes)
  ;; Decides which predefined button collection to use.
  ;; You take a big performance hit here over using the constant,
  ;; but hey, this is CLIM, we're minimizing developers' time.
  ;; Return NIL if nothing seems to match, and the caller should
  ;; fall back on the more general, but less pretty, accepting values.
  (if (atom exit-boxes)
      (when (member exit-boxes '#.`(,win:MB_OK
				    ,win:MB_OKCANCEL
				    ,win:MB_YESNO
				    ,win:MB_RETRYCANCEL
				    ,win:MB_YESNOCANCEL
				    ,win:MB_ABORTRETRYIGNORE))
	exit-boxes)			; user knows what they want
    (flet ((find-label (text)
	     (dolist (box exit-boxes)
	       (when (consp box)
		 (when (search text (second box) :test #'char-equal)
		   ;; Use search rather than string-equal because
		   ;; people tend to pad their labels with whitespace.
		   (return-from find-label t))))
	     nil)
	   (find-naked-key (symbol)
	     (dolist (box exit-boxes)
	       (when (and (atom box) (eql box symbol))
		 (return-from find-naked-key t)))))
      (let ((number (length exit-boxes)))
	(cond ((= number 3)
	       (cond ((and (find-label "Yes")
			   (find-label "No")
			   (find-label "Cancel"))
		      win:MB_YESNOCANCEL)
		     ((and (find-label "Abort")
			   (find-label "Retry")
			   (find-label "Ignore"))
		      win:MB_ABORTRETRYIGNORE)))
	      ((= number 2)
	       (cond ((or (find-naked-key :abort)
			  (find-label "Cancel"))
		      (cond ((or (find-naked-key :exit)
				 (find-label "OK"))
			     win:MB_OKCANCEL)
			    ((find-label "Retry")
			     win:MB_RETRYCANCEL)))
		     ((and (find-label "Yes")
			   (find-label "No"))
		      win:MB_YESNO)))
	      ((= number 1)
	       (when (or (find-naked-key :exit)
			 (find-label "OK"))
		 win:MB_OK)))))))

(defun select-messagebox-result (code button-style exit-boxes)
  ;; Most ports assume the notify-user exit boxes are limited
  ;; to :exit and :abort. This function tries to imagine which 
  ;; one the user picked.
  (when (zerop code)
    (error "Not enough memory for MessageBox operation."))
  (flet ((find-label (text)
	   (dolist (box exit-boxes)
	     (when (consp box)
	       (when (search text (second box) :test #'char-equal)
		 ;; Use search rather than string-equal because
		 ;; people tend to pad their labels with whitespace.
		 (return-from find-label (first box)))))))
    (cond ((= button-style win:MB_OK)
	   (or (find-label "OK") :exit))
	  ((= button-style win:MB_YESNO)
	   (cond ((= code win:IDYES) (or (find-label "Yes") :exit))
		 ((= code win:IDNO) (or (find-label "No") :abort))))
	  ((= button-style win:MB_RETRYCANCEL)
	   (cond ((= code win:IDRETRY) (or (find-label "Retry") :exit))
		 ((= code win:IDCANCEL) (or (find-label "Cancel") :abort))))
	  ((= button-style win:MB_OKCANCEL)
	   (cond ((= code win:IDOK) (or (find-label "OK") :exit))
		 ((= code win:IDCANCEL) (or (find-label "Cancel") :abort))))
	  ((= button-style win:MB_YESNOCANCEL)
	   (cond ((= code win:IDYES) (or (find-label "Yes") :exit))
		 ((= code win:IDNO) (or (find-label "No") :no))
		 ((= code win:IDCANCEL) (or (find-label "Cancel") :abort))))
	  ((= button-style win:MB_ABORTRETRYIGNORE)
	   (cond ((= code win:IDABORT) (or (find-label "Abort") :abort))
		 ((= code win:IDRETRY) (or (find-label "Retry") :retry))
		 ((= code win:IDIGNORE) (or (find-label "Ignore") :exit)))))))

(defun message-box (hwnd message-string name &optional icon)
  (excl:with-native-string (m message-string)
    (excl:with-native-string (n name)
      (win:MessageBox hwnd m n
		      (or icon 
			  (logior win:MB_ICONSTOP 
				  win:MB_TASKMODAL))))))
 
(defmethod frame-manager-notify-user
    ((framem acl-frame-manager) message-string
     &key (style :inform)
	  (frame nil frame-p)
	  (associated-window
	   (if frame-p
	       (frame-top-level-sheet frame)
	     (graft framem)))
	  (title "Notification") 
	  documentation
	  (exit-boxes '(:exit :abort))
	  (name title)
	  text-style
     &allow-other-keys)
  (declare (ignore documentation))	; FIXME
  ;; Uses MessageBox() to put up a simple dialog box,
  ;; unless the user has specified some fancy options, in
  ;; which case accepting-values will have to suffice.
  (let ((icon (select-messagebox-icon style))
	(buttons (select-messagebox-buttons exit-boxes)))
    (if (and icon buttons (not text-style))
	(let* ((hwnd (sheet-mirror associated-window))
	       (code (message-box
		      hwnd
		      (coerce message-string 'simple-string) 
		      (coerce name 'simple-string)
		      (logior win:MB_SYSTEMMODAL icon buttons)))
	       (symbol (select-messagebox-result code buttons exit-boxes)))
	  ;; Notify-user is supposed to return T or NIL:
	  (case symbol
	    (:exit t)
	    (:abort nil)
	    (otherwise symbol)))
      (let ((stream associated-window))
	(accepting-values (stream :exit-boxes exit-boxes
				  :scroll-bars :both
				  :label name
				  :own-window t)
	  (with-text-style (stream text-style)
	    (write-string message-string stream)))))))

(defun do-one-menu-item (popmenu item printer tick alist submenus)
  (let ((*print-circle* nil))
    (flet ((print-item (item)
	     (silica::xlat-newline-return 
	      (with-output-to-string (stream)
		;; The click-right menu uses PRESENT at this point to
		;; get the menu text, using the stream-default-view, which needs to be
		;; set to textual-menu-view.
		(letf-globally (((stream-default-view stream) +textual-menu-view+))
		  (funcall (or printer #'print-menu-item) item stream))))))
      (declare (dynamic-extent #'print-item))
      (incf tick)
      (ecase (clim-internals::menu-item-type item)
	(:divider
	 (win:AppendMenu popmenu win:MF_SEPARATOR tick 0))
	(:label
	 (excl:with-native-string (p-i (print-item item))
	   (win:AppendMenu popmenu win:MF_DISABLED tick 
			   p-i)))
	(:item
	 (if (clim-internals::menu-item-items item)
	     (let ((submenu (win:CreatePopupMenu)))
	       (push submenu submenus)
	       ;; submenu
	       (excl:with-native-string (p-i (print-item item))
		 (win:AppendMenu popmenu win:MF_POPUP submenu
				 p-i))
	       (map nil
		 #'(lambda (it)
		     (multiple-value-setq (tick alist submenus)
		       (do-one-menu-item submenu it printer
					 tick alist submenus)))
		 (clim-internals::menu-item-items item)))
	   (progn
	     (push (list tick (menu-item-value item) 
			 item) ;; spr25894 -- Third item is the menu-item itself
		   alist)
	     
	     (excl:with-native-string (p-i (print-item item))
	       (if (clim-internals::menu-item-active item)
		   (win:AppendMenu popmenu win:MF_ENABLED tick 
				   p-i)
		;;; Use win:MF_GRAYED rather than win:MF_DISABLED.
		;;; The latter will also disable the command, but
		;;; doesn't seem to affect the appearance.	       
		 (win:AppendMenu popmenu win:MF_GRAYED tick 
				 p-i)))))))
      (values tick alist submenus))))


;; Gets rid of scroll bars if possible.

(defmethod frame-manager-menu-choose
    ((framem acl-frame-manager) items &rest keys
     &key printer
	  presentation-type
	  (associated-window (frame-top-level-sheet *application-frame*))
	  text-style label
	  foreground background
	  cache
	  (unique-id items)
	  (id-test 'equal)
	  (cache-value items)
	  (cache-test #'equal)
	  (gesture :select)
	  row-wise
	  n-columns
	  n-rows
	  x-position
	  y-position
	  scroll-bars
	  
	  default-item ;; bug12221/spr25238
	  )
  ;; The basic theory of ignoring is that we ignore arguments
  ;; that don't contribute functionality and just bring up
  ;; the native menu without any fluff.
  (declare (ignore text-style cache
		   cache-test cache-value
		   id-test unique-id
		   foreground background))
  (if (or presentation-type ;; foreground background  
	  row-wise n-columns n-rows scroll-bars label)
      (call-next-method)
    #+simple-but-sure
    (apply #'call-next-method framem items :scroll-bars nil keys)
    (let ((popmenu (win:CreatePopupMenu))
	  (submenus nil)
	  (flags (logior win:TPM_RETURNCMD ; return the selection
			 win:TPM_NONOTIFY ; don't notify clim
			 (if (eq gesture :menu) 
			     win:TPM_RIGHTBUTTON
			   win:TPM_LEFTBUTTON)))
	  (rect 0)
	  (tick 0)
	  (alist nil)
	  (code 0))
      (when (zerop popmenu)
	(check-last-error "CreatePopupMenu"))
      (unless (and x-position y-position)
	;; Get screen coordinates of pointer.
	(let ((point (ct:ccallocate win:point)))
	  (or (win:GetCursorPos point)
	      (check-last-error "GetCursorPos"))
	  (setq x-position (ct:cref win:point point x))
	  (setq y-position (ct:cref win:point point y))

	  ;; spr25238
	  (multiple-value-setq (x-position y-position)
	    (calculate-mswin-menu-pos framem
				x-position y-position
				items
				default-item)
	    )))

      (setq x-position (truncate x-position))
      (setq y-position (truncate y-position))
      (map nil #'(lambda (item)
		   (multiple-value-setq (tick alist submenus)
		     (do-one-menu-item popmenu item printer 
				       tick alist submenus)))
	   items)
      ;; Bug here, exhibited by CAD Demo Create, that menu
      ;; is sometimes never exposed.  TrackPopupMenu returns 0.  
      ;; But most of the time this seems to work... 5/98 JPM.
      (setq code
	(win:TrackPopupMenu
	 popmenu flags x-position y-position 
	 0				; reserved, must be zero
	 (sheet-mirror associated-window) rect))
      (win:DestroyMenu popmenu)
      (dolist (submenu submenus) (win:DestroyMenu submenu))
      (cond ((zerop code)		; no item is selected
	     nil)
	    (t
	     (let ((x (assoc code alist)))
	       (values (second x) (third x) :unknown-gesture))
	     )))))

;;; bug12221/spr25238
;;; New method:  Make native menus appear with pointer on :default-item.
(defun calculate-mswin-menu-pos (framem 
				 init-cursor-x init-cursor-y			   
				 items
				 default-item)
  (let ((menu-left init-cursor-x)
	(menu-top init-cursor-y))

    (let ((default-item-pos (and default-item
				 (position default-item items))))
      (when default-item-pos
	(let ((line-hei 17) ;; A good guess at the height of a menu-item
	      (bottom-buffer 10) ;; Put a buffer at the bottom, in case the line-hei is not perfect.
	      (cursor-x nil)
	      (cursor-y nil))
	  (multiple-value-bind (graft-width graft-height)
	      (bounding-rectangle-size (graft framem))
	    graft-width
	    (let ((item-offset-x 3)
		  (item-offset-y (+ (* default-item-pos line-hei) 5))
		  (menu-hei (* (length items) line-hei)))
	      (cond ((< (- graft-height bottom-buffer)
			menu-hei)
		     ;; The menu is taller than the screen
		     ;; First, shift the offset, because of the 
		     ;; scroll-gadget at the top of the menu.
		     (setq item-offset-y (+ item-offset-y line-hei))
		     ;; Now, place the menu against the top of the screen...
		     (setq menu-left init-cursor-x
			   menu-top 0)
		     ;; ... and try to place the pointer on the default-item.
		     (setq cursor-x (+ init-cursor-x item-offset-x)
			   cursor-y (min (+ menu-top item-offset-y)
					 (- graft-height bottom-buffer))))
		    ((< (- graft-height bottom-buffer) 
			(+ (- init-cursor-y item-offset-y) menu-hei))
		     ;; We are bumping against the bottom.
		     ;; So place the menu at the bottom of the screen...
		     (setq menu-left init-cursor-x
			   menu-top (- (- graft-height bottom-buffer) menu-hei))
		     ;; ... and place the pointer on the default-item.
		     (setq cursor-x (+ init-cursor-x item-offset-x)
			   cursor-y (+ menu-top item-offset-y))
		     )
		    ((< init-cursor-y item-offset-y)
		     ;; We are bumping against the top of the screen.
		     ;; So place the menu at the top...
		     (setq menu-left init-cursor-x
			   menu-top 0)
		     ;; ... and place the pointer on the default-item.
		     (setq cursor-x (+ init-cursor-x item-offset-x)
			   cursor-y (+ menu-top item-offset-y))) 
		    (t
		     ;; The normal case-- everything fits on the screen
		     ;; So position the menu so the the default item
		     ;; comes up under the pointer.
		     (setq menu-left (- init-cursor-x item-offset-x)
			   menu-top  (- init-cursor-y item-offset-y))
		     ;; Warp the pointer slightly, 
		     ;; so that the item highlights properly 
		     (setq cursor-x (+ init-cursor-x 1)
			   cursor-y (+ init-cursor-y 1))))

	      ;; Finally, warp the pointer
	      (when (and cursor-x cursor-y) 
		(win:SetCursorPos cursor-x 
				  (max 0
				       (min cursor-y
					    (- graft-height bottom-buffer))))))))))
    (values  menu-left
	     menu-top)))



(defun make-filter-string (dotted-pair)
  (let ((*print-circle* nil))
    (format nil "~a (~a)~a~a~a"
	    (car dotted-pair) (cdr dotted-pair) (code-char 0)
	    (cdr dotted-pair) (code-char 0))))

(eval-when (compile load eval) 
  ;; All pathnames returned by SELECT-FILE must fit in the scratch string,
  ;; so make it pretty big.
  (defconstant *scratch-string-length* 2048)
  )

(defparameter *scratch-lisp-string*
    (make-string *scratch-string-length*))

(defparameter *scratch-c-string*
  (ff:allocate-fobject-c `(:array :char ,*scratch-string-length*)))

(eval-when (compile eval load)
  ;; this type useful since we don't open code anonymous types well yet:
  (ff:def-foreign-type foreign-string (:array :char 1))
  )

(defun lisp-string-to-scratch-c-string (lisp-string)
  (let ((length (min (length lisp-string)
		     (1- *scratch-string-length*))))
    (dotimes (i length 
	       ;; null term
	       (setf (ff:fslot-value-typed 'acl-clim::foreign-string
					   :c
					   *scratch-c-string*
					   length)
		 0))
      (setf (ff:fslot-value-typed 'acl-clim::foreign-string
				  :c
				  *scratch-c-string*
				  i)
	(char-int (aref lisp-string i))))
    *scratch-c-string*))

(defun scratch-c-string-to-lisp-string ()
  (values (excl:native-to-string *scratch-c-string*)))

(defun pathnames-from-directory-and-filenames (filename-list)
  ;; Takes a list consisting of a directory namestring followed
  ;; by a set of filenames relative to that directory.
  ;; This is the sort of list returned by the common dialog
  ;; when multiple choices are allowed.
  ;; Returns a list of complete pathnames.
  (if (eq (length filename-list) 1)
      ;; If only one choice, no separate directory is returned
      ;; by the GetOpenFileName call.
      filename-list
    (let ((directory (car filename-list)))
      ;; Windows doesn't stick a backslash on the end of the dir.
      (unless (eql (aref directory (1- (length directory))) #\\)
	(setf directory (concatenate 'string directory "\\")))
      (mapcar #'(lambda (filename)
		  ;; cac removed call to namestring to have this function
		  ;; return pathnames instead of strings.
		  ;; 5-apr-94
		  (merge-pathnames filename directory))
	      (cdr filename-list)))))

(defun delimited-string-to-list (string delimiter-char-or-string)
  "Returns a list of substrings of STRING, separating it at DELIMETER-CHAR-OR-STRING"
  (do* ((stringp (stringp delimiter-char-or-string))
	(delimiter-length (if stringp
                              (length delimiter-char-or-string)
			    1))
	(s string (subseq s (+ index delimiter-length)))
	(index
	 (if stringp
	     (search delimiter-char-or-string s)
	   (position delimiter-char-or-string s))
	 (if stringp
	     (search delimiter-char-or-string s)
	   (position delimiter-char-or-string s)))
	(list
	 (list (subseq s 0 index))
	 (nconc list (list (subseq s 0 index)))))
      ((null index)
       list)))

(defun spaced-string-to-list (string) ;; <27>
  (delimited-string-to-list string #\space))

(cl:defparameter common-dialog-errors
 '((#xffff . cderr_dialogfailure)
   (#x0000 . cderr_generalcodes)
   (#x0001 . cderr_structsize)
   (#x0002 . cderr_initialization)
   (#x0003 . cderr_notemplate)
   (#x0004 . cderr_nohinstance)
   (#x0005 . cderr_loadstrfailure)
   (#x0006 . cderr_findresfailure)
   (#x0007 . cderr_loadresfailure)
   (#x0008 . cderr_lockresfailure)
   (#x0009 . cderr_memallocfailure)
   (#x000a . cderr_memlockfailure)
   (#x000b . cderr_nohook)
   (#x000c . cderr_registermsgfail)
   (#x1000 . pderr_printercodes)
   (#x1001 . pderr_setupfailure)
   (#x1002 . pderr_parsefailure)
   (#x1003 . pderr_retdeffailure)
   (#x1004 . pderr_loaddrvfailure)
   (#x1005 . pderr_getdevmodefail)
   (#x1006 . pderr_initfailure)
   (#x1007 . pderr_nodevices)
   (#x1008 . pderr_nodefaultprn)
   (#x1009 . pderr_dndmmismatch)
   (#x100a . pderr_createicfailure)
   (#x100b . pderr_printernotfound)
   (#x100c . pderr_defaultdifferent)
   (#x2000 . cferr_choosefontcodes)
   (#x2001 . cferr_nofonts)
   (#x2002 . cferr_maxlessthanmin)
   (#x3000 . fnerr_filenamecodes)
   (#x3001 . fnerr_subclassfailure)
   (#x3002 . fnerr_invalidfilename)
   (#x3003 . fnerr_buffertoosmall)
   (#x4000 . frerr_findreplacecodes)
   (#x4001 . frerr_bufferlengthzero)
   (#x5000 . ccerr_choosecolorcodes)
   ))


(defun get-pathname-flags (save-p multiple-p warn-if-exists-p)
  (logior
   (if multiple-p win:OFN_ALLOWMULTISELECT 0)
   (if save-p 0 win:OFN_FILEMUSTEXIST)
   ;; This is only relevant if save-p:
   (if warn-if-exists-p win:OFN_OVERWRITEPROMPT 0)
   win:OFN_NOCHANGEDIR
   win:OFN_HIDEREADONLY))

(defun get-pathname (prompt directory stream allowed-types initial-name
		     save-p multiple-p warn-if-exists-p)
  (let ((open-file-struct (ct:ccallocate win:openfilename)))
    (excl:with-native-string
	(file-filter-string
	 (apply #'concatenate 'string
		(mapcar #'make-filter-string allowed-types)))
      (let ((s1 (lisp-string-to-scratch-c-string (or initial-name ""))))
	(excl:with-native-string (initial-dir-string (string directory))
	  (excl:with-native-string (prompt-string (string prompt))

	    (ct:csets win:openfilename open-file-struct
		      lStructSize (ct:sizeof win:openfilename)
		      hwndOwner (or (and stream (sheet-mirror stream))
				    0)
		      hInstance 0	; no custom dialog
		      lpstrFilter file-filter-string
		      lpstrCustomFilter 0 
		      nMaxCustFilter 0 ;; length of custom filter string
		      nFilterIndex 0	; zero means use custom-filter if supplied
					; otherwise the first filter in the list
		      lpstrFile s1
		      nMaxFile *scratch-string-length*
		      lpstrFileTitle 0 
		      nMaxFileTitle 0
		      lpstrInitialDir initial-dir-string
		      lpstrTitle prompt-string
		      Flags (get-pathname-flags save-p multiple-p warn-if-exists-p)
		      nFileOffset 0
		      nFileExtension 0 
		      lpstrDefExt 0
		      lCustData 0
		      lpfnHook 0
		      lpTemplateName 0)))))
    (let* ((result 
	    (if save-p
		(win:GetSaveFileName open-file-struct)
	      (win:GetOpenFileName open-file-struct))))
      (if result ;; t means no errors and user said "OK"
	  (if multiple-p
	      (pathnames-from-directory-and-filenames
	       (spaced-string-to-list
		(scratch-c-string-to-lisp-string)))
	    (pathname
	     (scratch-c-string-to-lisp-string)))
	(let ((error-code (win:CommDlgExtendedError)))
	  (and (plusp error-code) ;; zero means cancelled, so return NIL
	       (error (format nil 
			      "Common dialog error ~a."
			      (or (cdr (assoc error-code
					      common-dialog-errors))
				  error-code)))))))))

#|
;; This is the old version of frame-manager-select-file.  It doesn't work well
;; if you want to ask for a directory instead of a file.  So I replaced it
;; by the code at the end of this file. See spr30571. (alemmens, 2005-11-29).
;; Let's remove this once we're sure that the new version is better than the
;; old one.

(defun get-directory (sheet title)
  (let* ((info (ct:ccallocate browseinfo)))
    (ct:csets browseinfo info
	      hwndOwner (or (and sheet (sheet-mirror sheet)) 0)
	      pidlRoot 0
	      pszDisplayName 0
	      lpszTitle (string-to-foreign title)
	      ulflags 0
	      lpfn 0
	      lparam 0
	      iImage 0)
    (let ((result (SHBrowseForFolder info)))
      (when (plusp result)
	;; To do: parse the result.
	result))))  

(defmethod frame-manager-select-file
    ((framem acl-frame-manager)
     &key (default nil)
	  (frame nil frame-p)
	  (associated-window
	   (if frame-p
	       (frame-top-level-sheet frame)
	     nil))
	  (title "Select a file")
	  documentation
	  file-search-proc
	  directory-list-label
	  file-list-label
	  (exit-boxes '(:exit :abort :help))
	  (name title)
	  directory
	  pattern
	  ;; These are all peculiar to this frame manager
	  (dialog-type :open)
	  (file-types '(("All Files" . "*.*")))
	  (multiple-select nil)
	  (warn-if-exists-p t)
     &allow-other-keys)
  (declare (ignore name exit-boxes file-list-label directory-list-label
		   file-search-proc documentation))
  (unless pattern 
    (setq pattern ""))
  (when (pathnamep default)
    (let ((name (pathname-name default))
	  (type (pathname-type default))
	  (dir (pathname-directory default))
	  (device (pathname-device default)))
      (cond ((and name type)
	     (setq pattern (format nil "~A.~A" name type)))
	    (name
	     (setq pattern name))
	    (type
	     (setq pattern (format nil "*.~A" type))))
      (when (or directory device)
	(setq directory 
	  (namestring (make-pathname :name nil
				     :directory dir
				     :device device))))))
  ;; Massage the directory to make sure, in particular,
  ;; that it has a device.  Expensive, but worth it to 
  ;; avoid a segmentation violation.  JPM.
  (if directory
      (setq directory
	(namestring (merge-pathnames (pathname directory)
				     (excl:current-directory))))
    (setq directory (namestring (excl:current-directory))))
  (let* ((stream associated-window)
	 (save-p nil)
	 (directory-p nil))
    (ecase dialog-type 
      (:open (setq save-p nil))
      (:save (setq save-p t))
      (:directory (setq directory-p t)))
    (if directory-p
	(get-directory stream title)
      (get-pathname title 
		    directory 
		    stream
		    file-types
		    pattern
		    save-p
		    multiple-select
		    warn-if-exists-p))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Progress notification code.

(define-application-frame nt-working-dialog ()
  ((note :initform "" :accessor work-note :initarg :note)
   (fraction :initform 0.0 :accessor fraction
	     :initarg :fraction)
   (cancellable :initform t :accessor cancellable
		:initarg :cancellable)
   (cancel-button :accessor cancel-button)
   (thermopane :initform nil :accessor thermopane)
   (notepane :initform nil :accessor notepane)
   (process :accessor work-process))
  (:panes
   (display 
    (setf (notepane *application-frame*)
      (make-pane 'application-pane
		 :text-style '(:sans-serif :roman :small)
		 :display-function 'display-note
		 :initial-cursor-visibility nil
		 )))
   (thermometer
    (setf (thermopane *application-frame*)
      (make-pane 'application-pane
		 :min-width 250 :width 250
		 :display-function 'display-thermometer
		 :initial-cursor-visibility nil
		 :record-p nil		;performance hack
		 :foreground +blue+
		 )))
   (cancel
    (setf (cancel-button *application-frame*)
      (make-pane 'push-button
		 :label "Cancel"
		 :align-x :center
		 :align-y :center
		 :show-as-default (cancellable *application-frame*)
		 :active (cancellable *application-frame*)
		 :activate-callback
		 #'(lambda (button)
		     (let ((frame (pane-frame button))
			   (process nil))
		       (when (and frame (cancellable frame))
			 (setq process (work-process frame))
			 (if process 
			     (mp:process-interrupt process 'abort)
			   (beep)))))))))
  (:menu-bar nil)
  (:layouts (main 
	     (spacing (:thickness 10)
	       (vertically () 
		 display
		 (25 (horizontally ()
		       (vertically ()
			 (15 (outlining () thermometer))
			 :fill)
		       :fill
		       cancel)))))))

;; Perhaps we hang if we set this to T.  I guess that
;; means we aren't doing application-modal dialog boxes
;; correctly yet.  10/98 JPM.
(defmethod modal-frame-p ((frame nt-working-dialog)) nil)

(defmethod display-note ((frame nt-working-dialog) stream)
  "The pane display function for the DISPLAY pane."
  (window-clear stream)
  (stream-set-cursor-position stream 0 40)
  (write-string (work-note frame) stream)
  (force-output stream))

(defmethod display-thermometer ((frame nt-working-dialog) stream)
  (let ((fraction (fraction frame)))
    (with-bounding-rectangle* (left top right bottom) (sheet-region stream)
      (medium-draw-rectangle* stream left top 
			      (+ left (* (- right left) fraction)) bottom
			      t))))

(defvar *working-dialog* nil)
(defvar *noting-progress-enable-cancel* t); set/bind to nil to disable cancel

(defmethod clim-internals::frame-manager-invoke-with-noting-progress
    ((framem acl-frame-manager)
     note
     continuation)
  (if *working-dialog*
      (let ((old-string (work-note *working-dialog*))
	    (old-value (fraction *working-dialog*)))
	(unwind-protect
	    (progn
	      (clim-internals::frame-manager-display-progress-note
	       framem note)
	      (funcall continuation note))
	  (setf (work-note *working-dialog*) old-string)
	  (setf (fraction *working-dialog*) old-value)
	  (redisplay-frame-panes *working-dialog*)))
    (let ((frame nil)
	  (cancellable *noting-progress-enable-cancel*)
	  (parent-frame *application-frame*)
	  (*working-dialog* nil)
	  (worker (current-process))
	  (waiter nil))
      (unwind-protect
	  (progn
	    (setq waiter
	      (mp:process-run-function
	       "Progress Note"
	       #'(lambda ()
		   (let ((*application-frame* parent-frame)) ; for application-modal
		     (setq frame (make-application-frame
				  'nt-working-dialog
				  :cancellable cancellable
				  :note (string (progress-note-name note))
				  :pretty-name ""
				  :left 200 :top 200
				  :width 400
				  :height 125))
		     (setf (work-process frame) worker)
		     (run-frame-top-level frame)))))
	    (process-wait
	     "Synchronize In"
	     #'(lambda () 
		 (and frame 
		      (eq (frame-state frame) :enabled))))
	    (setq *working-dialog* frame)
	    (clim-internals::frame-manager-invoke-with-noting-progress
	     framem note continuation))
	(when waiter
	  (mp:process-interrupt waiter #'frame-exit frame)
	  ;; It is possible to hang all of Lisp if you don't
	  ;; slow down here.  Don't ask me why.  Perhaps
	  ;; it also helps if FRAME is not modal-frame-p. 
	  ;; JPM 10/98.
	  (mp:process-allow-schedule waiter)
	  (process-wait
	   "Synchronize Out"
	   #'(lambda () 
	       (and frame 
		    (not (eq (frame-state frame) :enabled)))))
	  (setq *working-dialog* nil))))))

(defmethod clim-internals::frame-manager-display-progress-note
    ((framem acl-frame-manager) note)
  (when *working-dialog*
    (let ((old (fraction *working-dialog*))
	  (new (/ (float (slot-value note 'clim-internals::numerator))
		  (float (slot-value note 'clim-internals::denominator)))))
      (unless (= old new)
	(setf (fraction *working-dialog*) new)
	(display-thermometer *working-dialog* (thermopane *working-dialog*))))
    (let ((old (work-note *working-dialog*))
	  (new (string (progress-note-name note))))
      (unless (string= old new)
	(setf (work-note *working-dialog*) new)
	(display-note *working-dialog* (notepane *working-dialog*))))))

(defun wait-demo (&key (time 10.0) (N 100))
  (let ((*application-frame* (car (frame-manager-frames 
				   (find-frame-manager))))
	(note (format nil "Men at work. ~%Please wait."))
	(wtime (/ time N)))
    (with-simple-restart (abort "Abort Wait Demo")
      (clim-internals::frame-manager-invoke-with-noting-progress
       (find-frame-manager)
       (clim-internals::add-progress-note note t)
       #'(lambda (note)
	   (let ((*current-progress-note* note))
	     (dotimes (i N)
	       (sleep wtime)
	       (note-progress i N))))))))

(defmethod port-move-frame ((port acl-port) frame x y)
  (let ((sheet (frame-top-level-sheet frame)))
    (fix-coordinates x y)
    (or (win:SetWindowPos (sheet-mirror sheet)
			  (ct:null-handle win:hwnd) ; we really want win:HWND_TOP
			  x y 0 0
			  (logior win:SWP_NOACTIVATE
				  win:SWP_NOZORDER
				  win:SWP_NOSIZE))
	(acl-clim::check-last-error "SetWindowPos"))))

(defmethod accept-values-frame-p ((object t)) nil)
(defmethod accept-values-frame-p ((object clim-internals::accept-values-own-window)) t)

(defmethod handle-event ((pane sheet) (event silica::window-close-event))
  (let ((frame (pane-frame pane)))
    (cond ((not frame))
	  ((accept-values-frame-p frame)
	   ;; A window-close-event is supposed to equate to a "cancel" gesture.
	   (abort))
	  (t (frame-exit frame)))))

(defmethod distribute-event-1 ((port basic-port) (event silica::window-close-event))
  (declare (optimize (speed 3)))
  (dispatch-event (window-event-mirrored-sheet event) event))

(in-package :silica)

(defparameter *mswin-pane-classes*
    '((scroll-bar mswin-scroll-bar)
      (scroller-pane silica::mswin-scroller-pane)
      (viewport viewport)
      (menu-bar mswin-menu-bar-pane)
      (menu-bar-button-logic mswin-menu-bar-button)
      (pull-down-button-logic mswin-pull-down-menu-button)
      (push-button hpbutton-pane)
      (toggle-button hbutton-pane)
      (radio-box radio-box-pane)
      (check-box check-box-pane)
      (slider slider-pane)
      (top-level-sheet acl-clim::acl-top-level-sheet)
      (frame-pane frame-pane)
      (label-pane generic-label-pane)
      (text-field mswin-text-field)
      ;;mm: interpose a pane class to manipulate initargs
      (text-editor acl-clim::acl-text-editor-pane)
      (list-pane hlist-pane)
      (option-pane mswin-combo-box-pane)
      (outlined-pane acl-clim::mswin-outlined-pane)
      ;;--- Need to do these
      (horizontal-divider-pane)
      (vertical-divider-pane)
      ))

(defmethod make-pane-class ((framem acl-clim::acl-frame-manager) class
			    &rest options)
  (declare (ignore options))
  (second (assoc class *mswin-pane-classes*)))

(in-package :clim-internals)

;; TO DO: Check for the case of a frame enabled in one thread and
;; then run-frame-top-level in another thread.  Signal an error,
;; otherwise Lisp will probably hang when the first thread ever exits,
;; because the OS considers the first thread to "own" the window.

(defmethod enable-frame :before ((frame standard-application-frame))
  (let* ((sheet (frame-top-level-sheet frame))
	 (mirror (when sheet (sheet-direct-mirror sheet))))
    (when mirror
      ;; Validate the window handle to give a better error message.
      (or (win:IsWindow mirror)
	  (error "The window handle ~S is not valid.  Frame
~S cannot be enabled.  It is likely that
Windows has destroyed it automatically as a
result of the exit of the thread that created it:
~S
This typically happens when you attempt to reuse a disabled CLIM frame 
in a second Lisp process.  This frame cannot be reused."
		 mirror frame (when sheet (clim-internals::sheet-thread sheet))))
      )))

(defmethod layout-frame :around ((frame standard-application-frame)
				 &optional width height)
  (let ((panes (frame-panes frame))
	(*application-frame* frame)
	(native (getf (frame-properties frame) :native-menu)))
    (when (and (not native) panes)
       ;;mm: Adjust the space requirements of the menu-bar pane depending on
       ;;    the number of buttons present
       (let* ((mb (slot-value frame 'menu-bar))
              (bt (when mb (sheet-children mb)))
              br)
          (when mb
             (with-slots (space-requirement) mb
                (with-slots (clim-silica::min-height clim-silica::height
                             clim-silica::max-height)
                   space-requirement
                   (if bt
                      (setq br nil)
                      (setq br 1))
                   (setf clim-silica::min-height br
                         clim-silica::height br
                         clim-silica::max-height br)
                   )))))
    (when panes
      ;; we need to recompute the frame-settings (min allowable width
      ;; and height for resizing) when we re-layout the frame because
      ;; narrowing a window can cause the native menu-bar to be spread
      ;; over multiple-lines invalidating the previous calculations
      ;; (because the client deltas have changed) (cim 10/8/96)
      (when native
	(multiple-value-bind (min-width min-height)
	    (update-frame-settings (frame-manager frame) frame)
	  (when width (maxf width min-width))
	  (when height (maxf height min-height))))
      (call-next-method frame width height)
      (let ((wrect (ct:ccallocate win:rect))
	    (handle (sheet-direct-mirror (frame-top-level-sheet frame))))
	;;; The code below makes sure that the frame grows or shrinks
	;;; when the user resizes the frame window.
	;; ----
	;; Actually, the code below repaints the entire frame. 
	;; Shouldn't we free wrect? JPM.
	(or (win:GetClientRect handle wrect)
	    (acl-clim::check-last-error "GetClientRect"))
	(or (win:InvalidateRect handle wrect 1)
	    (acl-clim::check-last-error "InvalidateRect"))
	#+ignore ; not needed and may be implicated in a race condition.
	(acl-clim::frame-update-window frame handle)))))

;; Obsolete I think.
(defun clean-frame (frame)
  (declare (ignore frame))
  ;; (disable-frame frame)
  ;; (enable-frame frame)
  nil)

;; Obsolete I think.
(defun frame-find-position (frame)
  (when frame 
    (let* ((wrect (ct:ccallocate win:rect))
	   (sheet (frame-top-level-sheet frame))
	   (handle (when sheet (sheet-mirror sheet))))
      (when handle
	(win:GetWindowRect handle wrect)
	(values (ct:cref win:rect wrect left) 
		(ct:cref win:rect wrect top))))))

;; Obsolete I think.
(defun frame-set-position (frame x y)
  (win:SetWindowPos (sheet-mirror (frame-top-level-sheet frame))
     0					; we really want win:HWND_TOP
     x y 0 0
     (logior win:SWP_NOACTIVATE
	     win:SWP_NOZORDER
	     win:SWP_NOSIZE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printer support

(defun setup-escape-buffer (buffer code string)
  (let ((length (length string)))
    (assert (< length 1024))
    ;; The header is the size of a "word"
    (setf (ff:fslot-value-typed 'acl-clim::foreign-string :foreign buffer 0) 
      (ldb (byte 8 0) code))
    (setf (ff:fslot-value-typed 'acl-clim::foreign-string :foreign buffer 1) 
      (ldb (byte 8 8) code))
    (setf (ff:fslot-value-typed 'acl-clim::foreign-string :foreign buffer 2) 
      (ldb (byte 8 16) code))
    (setf (ff:fslot-value-typed 'acl-clim::foreign-string :foreign buffer 3) 
      (ldb (byte 8 24) code))
    ;; The rest is the actual string.
    (dotimes (i length)
      (setf (ff:fslot-value-typed 'acl-clim::foreign-string :foreign
				  buffer (+ i 4))
	(char-code (char string i))))
    buffer))

(defun print-postscript (filename printer)
  ;; Print a file as postscript.
  ;; 'printer' is an hDC
  (assert (stringp filename))
  (assert (acl-clim::valid-handle printer))
  (let ((code 1)
	(buffer (ff:allocate-fobject-c '(:array :char 1024)))
	(prcode 4115 #+ig win:POSTSCRIPT_PASSTHROUGH)
	(isa-psprinter t)
	(docinfo (ff:allocate-fobject 'win:docinfo :foreign-static-gc)))

    (setf (ct:cref win:docinfo docinfo cbSize) (ct:sizeof win:docinfo))
    #+ignore
    (excl:with-native-string (cstr filename)
      (setf (ct:cref win:docinfo docinfo lpszDocName) cstr))
    (setf (ct:cref win:docinfo docinfo lpszDocName) 
      (excl:string-to-native filename))
    (setf (ct:cref win:docinfo docinfo lpszOutput) 0)
    (setf (ct:cref win:docinfo docinfo lpszDatatype) 0)
    (setf (ct:cref win:docinfo docinfo fwType) 0)
    
    (setup-escape-buffer buffer prcode "")
    (setq isa-psprinter
      (plusp (win:Escape printer win:QUERYESCSUPPORT 
			 4 buffer 0)))
    (unless isa-psprinter
      (return-from print-postscript :error-postscript-not-supported))
    (or (plusp (win:StartDoc printer docinfo))
	(acl-clim::check-last-error "StartDoc"))
    (with-open-file (stream filename :direction :input)
      (loop 
	(let ((line (read-line stream nil nil nil)))
	  (unless line (return))
	  (setup-escape-buffer buffer (length line) line)
	  (win:Escape printer prcode (+ (length line) 4) buffer 0))))
    (setq code (win:EndDoc printer))
    (cond ((plusp code) nil)
	  (t (acl-clim::check-last-error "EndDoc")
	     :error))))

(defun print-ascii (filename printer &key (xmargin 5) (ymargin 5))
  ;; Print a file as plain ascii text.
  ;; 'printer' is an hDC
  (assert (stringp filename))
  (assert (acl-clim::valid-handle printer))
  (let ((code 0)
	(x xmargin)
	(y ymargin)
	(currentline 0)
	(lines-per-page nil)
	(pagesize (win:GetDeviceCaps printer win:VERTRES))
	(line-height 12)
	(char-width 12)
	(textmetric (ct:ccallocate win:textmetric))
	(docinfo (ff:allocate-fobject 'win:docinfo :foreign-static-gc)))
    (or (win:GetTextMetrics printer textmetric)
	(acl-clim::check-last-error "GetTextMetrics"))
    (setq line-height 
      (+ (ct:cref win:textmetric textmetric tmHeight)
	 (ct:cref win:textmetric textmetric tmExternalLeading)))
    (setq char-width (ct:cref win:textmetric textmetric tmMaxCharWidth))
    (setq ymargin (* ymargin line-height))
    (setq xmargin (* xmargin char-width))
    (setq x xmargin y ymargin)

    (setf (ct:cref win:docinfo docinfo cbSize) (ct:sizeof win:docinfo))
    #+ignore
    (excl:with-native-string (cstr filename)
      (setf (ct:cref win:docinfo docinfo lpszDocName) cstr))
    (setf (ct:cref win:docinfo docinfo lpszDocName) 
      (excl:string-to-native filename))
    (setf (ct:cref win:docinfo docinfo lpszOutput) 0)
    (setf (ct:cref win:docinfo docinfo lpszDatatype) 0)
    (setf (ct:cref win:docinfo docinfo fwType) 0)
    
    (decf pagesize (+ ymargin ymargin))    
    (setq lines-per-page (1- (truncate pagesize line-height)))
    
    (or (plusp (win:StartDoc printer docinfo))
	(acl-clim::check-last-error "StartDoc"))
    (progn
      (win:StartPage printer)
      (with-open-file (stream filename :direction :input)
	(loop 
	  (let ((line (read-line stream nil nil nil)))
	    (unless line (return))
	    (multiple-value-bind (string i)
		(silica::xlat-newline-return line)
	      (excl:with-native-string (cstr string)
		(win:TextOut printer x y cstr i)))
	    (incf currentline)
	    (incf y line-height)
	    (when (> currentline lines-per-page)
	      (win:EndPage printer)
	      (win:StartPage printer)
	      (setq currentline 1)
	      (setq y ymargin))
	    )))
      (win:EndPage printer))
    (setq code (win:EndDoc printer))
    (cond ((plusp code) nil)
	  (t (acl-clim::check-last-error "EndDoc")
	     :error))))

(defun determine-print-file-type (filename)
  (unless (probe-file filename)
    (return-from determine-print-file-type :none))
  (with-open-file (stream filename :direction :input)
    (let ((char1 (read-char stream nil nil nil))
	  (char2 (read-char stream nil nil nil)))
      (unless char1
	(return-from determine-print-file-type :empty))
      (cond ((and char2 (char= char1 #\%) (char= char2 #\!))
	     :postscript)
	    (t :ascii)))))

(defmethod frame-manager-print-file
    ((framem acl-clim::acl-frame-manager) filename
     &key 	  
     (frame nil frame-p)
     (associated-window
      (if frame-p
	  (frame-top-level-sheet frame)
	(graft framem)))
     from-page to-page min-page max-page
     ncopies collate-p 
     print-to-file-p disable-print-to-file (hide-print-to-file t)
     nopagenums noselection selection
     nowarning nodialog)
  (let* ((printdlg (ct:ccallocate win:printdlg))
	 (hwnd (if associated-window (sheet-mirror associated-window) 0)))
    (ct:csets win:printdlg printdlg
	      lStructSize (ct:sizeof win:printdlg)
	      hwndOwner hwnd
	      hDevMode 0
	      hDevNames 0        
	      hDC 0
	      Flags (logior 
		     ;; check the collate box
		     (if collate-p win:PD_COLLATE 0)
		     ;; disable the printtofile check box
		     (if disable-print-to-file win:PD_DISABLEPRINTTOFILE 0)
		     ;; hide the printtofile check box
		     (if hide-print-to-file win:PD_HIDEPRINTTOFILE 0)
		     ;; disable the pages radio button
		     (if nopagenums win:PD_NOPAGENUMS 0)
		     ;; disable the selection radio button
		     (if noselection win:PD_NOSELECTION 0)
		     ;; Prevents warning message from being displayed
		     ;; when there is no default printer.
		     (if nowarning win:PD_NOWARNING 0)
		     ;; selects the pages radio button
		     (if (or from-page to-page) win:PD_PAGENUMS win:PD_ALLPAGES)
		     ;; selects the printtofile check box
		     (if print-to-file-p win:PD_PRINTTOFILE 0)
		     ;; suppress the dialog box
		     (if nodialog win:PSD_RETURNDEFAULT 0)
		     ;; selects the selection radio button
		     (if selection win:PD_SELECTION 0)
		     ;; return a device context to the printer
		     win:PD_RETURNDC)
	      nFromPage (or from-page #xffff)
	      nToPage (or to-page #xffff)
	      nMinPage (or min-page 1)
	      nMaxPage (or max-page #xffff)
	      nCopies (or ncopies 1))
    (cond ((not (win:PrintDlg printdlg))
	   ;; User cancelled, or there was an error.
	   (let ((code (win:CommDlgExtendedError)))
	     (if (zerop code) 
		 nil			
	       ;; Code will be among CDERR_* or PDERR_* families
	       (error "PrintDlg failed with error code ~A" code))))
	  (t
	   (let ((hdc (ct:cref win:printdlg printdlg hDC)))
	     (unwind-protect
		 (ecase (determine-print-file-type filename)
		   (:postscript 
		    (case (print-postscript filename hdc)
		      (:error-postscript-not-supported
		       (frame-manager-notify-user
			framem
			"Printing Error: Printer does not support postscript"
			:style :error))
		      (:error
		       (frame-manager-notify-user
			framem
			"Printing Error: Cannot print file"
			:style :error)
		       )))
		   (:ascii 
		    (case (print-ascii filename hdc)
		      (:error
		       (frame-manager-notify-user 
			framem
			"Printing Error: Cannot print file"
			:style :error))))
		   (:none
		    (frame-manager-notify-user 
		     framem
		     "Printing Error: File does not exist"
		     :style :error))
		   (:empty
		    (frame-manager-notify-user 
		     framem
		     "Printing Error: File is empty"
		     :style :error)))
	       (win:DeleteDC hdc)
	       ))))))

#+ignore
(defun tester ()
  (with-open-file (s "c:/tester.ps" :direction :output
		   :if-exists :supersede)
    (clim:with-output-to-postscript-stream (stream s)
      ;(clim:draw-line* stream 0 0 100 100)
      ;(clim:draw-line* stream 0 100 100 0)
      (clim:draw-text* stream "Tester" 50 50))))



(defmethod popup-frame-p ((frame application-frame))
  (typep frame '(or clim-internals::menu-frame
		 clim-internals::accept-values-own-window)))

;;; New method aclpc/acl-frames.lisp
;;; bug12221/spr24998 -pnc
;;; Make pop-ups appear relative to the their calling frame.
;;; Motif seems to do this automatically.  Windows does
;;; it relative to the main-screen.  So, here we just
;;; do it all by hand.
(defmethod clim-internals::frame-manager-position-dialog 
  ((framem acl-clim::acl-frame-manager)
   frame
   own-window-x-position
   own-window-y-position)
  ;; This definition overrides the more general method in clim/accept-values.lisp
  (let ((calling-frame nil) calling-frame-top-level-sheet frame-top-level-sheet)
    (multiple-value-bind (x y) 
	(cond ((and own-window-x-position own-window-y-position)
	       ;; If value is specified, use that.
	       (values own-window-x-position own-window-y-position))
	      ((and (setq calling-frame (frame-calling-frame frame))
		    (popup-frame-p frame)
		    ;; Make sure these values exist before using them [bug14769]
	            (setq calling-frame-top-level-sheet
			  (frame-top-level-sheet calling-frame))
                    (setq frame-top-level-sheet (frame-top-level-sheet frame))
		    )
	       ;; If frame is a designated pop-up, 
	       ;; try to center over the calling frame.
	       (let ()
		 (multiple-value-bind (calling-frame-left calling-frame-top
							  calling-frame-width)
		     (bounding-rectangle* calling-frame-top-level-sheet)
		   (let ((frame-width (bounding-rectangle-size frame-top-level-sheet)))
		     (let ((offset (/ (- calling-frame-width frame-width) 2))) 
		       (multiple-value-bind (new-x new-y) 
			   (transform-position (sheet-delta-transformation
                                                calling-frame-top-level-sheet nil) 
					       calling-frame-left calling-frame-top) 
			 (values (+ new-x offset)
				 (+ new-y 10))))))))
	      (t
	       ;; Otherwise, just pick a default.
	       (values clim-internals::+frame-manager-position-dialog-default-x+
		       clim-internals::+frame-manager-position-dialog-default-y+)))
      (position-sheet-carefully
       (frame-top-level-sheet frame) x y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; frame-manager-select-file [spr30571]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :acl-clim)

;;;
;;; Windows foreign functions and constants
;;;

;; 260 is win:MAX_PATH, but for some reason that's not available yet
;; when we build CLIM from scratch.
(defparameter *max-path* 260)

(defparameter *max-file-selection-buffer-size* (* 16 *max-path*))

(ff:def-foreign-call IMallocFree
    ((this :nat)
     (pv win:pvoid))
  :returning :void
  :convention :stdcall
  :method-index 5
  :release-heap :when-ok
  :arg-checking nil
  :strings-convert nil)

(ff:def-foreign-call IMallocRelease ()
  :method-index 2
  :convention :stdcall
  :release-heap :when-ok
  :arg-checking nil
  :returning :unsigned-long
  :strings-convert nil)

(ff:def-foreign-call IShellFolderParseDisplayName
    ((this :nat)
     (hwnd win:hwnd)
     (pbc win:lpstr)
     (pwszDisplayName win:lpstr)
     (pchEaten win:ulong_ptr)
     (ppidl (* win:lpcitemidlist))
     (pdwAttributes win:ulong_ptr))
  :method-index 3
  :convention :stdcall
  :release-heap :when-ok
  :arg-checking nil
  :returning win:hresult
  :strings-convert nil)

(ff:def-foreign-call (MultiByteToWideChar "MultiByteToWideChar")
    ((code-page :unsigned-int)
     (flags win:dword)      ; character-type options
     (string win:lpcstr)    ; address of string to map
     (nr-bytes :int)        ; number of bytes in string
     (buffer win:lpwstr)    ; LPWSTR, address of wide-character buffer
     (buffer-size :int)     ; size of buffer (in wide characters)
     )
  :returning :int
  :convention :stdcall
  :arg-checking nil
  :release-heap :when-ok
  :strings-convert nil)




;; WINSHELLAPI HRESULT WINAPI SHGetDesktopFolder(LPSHELLFOLDER *ppshf)
(ff:def-foreign-call (SHGetDesktopFolder "SHGetDesktopFolder")
    ((folder (* :nat)))
  :returning win:hresult
  :release-heap :when-ok)

;; browseinfo moved to winwidgh.lisp

;; SHBrowseForFolder moved to winwidgh.lisp

;; HRESULT SHGetMalloc(LPMALLOC *ppMalloc);
(ff:def-foreign-call (SHGetMalloc "SHGetMalloc")
    ((pointer (* :nat)))
  :returning win:hresult
  :release-heap :when-ok)


;; WINSHELLAPI BOOL WINAPI SHGetPathFromIDList(
;;    LPCITEMIDLIST pidl,
;;    LPSTR pszPath)
(ff:def-foreign-call (SHGetPathFromIDList "SHGetPathFromIDList")
    ((pidl win:lpcitemidlist)
     (path win:lpstr))
  :returning win:bool
  :convention :stdcall
  :arg-checking nil
  :release-heap :when-ok
  :strings-convert nil)


;;;
;;; Little helpers
;;;

(defun allocate-pointer (type &optional (size 1))
  (ff:allocate-fobject `(:array ,type ,size)))

(defun pointer-value (type object)
  (ff:fslot-value-typed `(:array ,type 1) nil object 0))

(defmacro fill-fslots (type object &rest slots)
  (declare (ignore type))
  (let ((object-var (gensym "OBJECT")))
    `(let ((,object-var ,object))
      ,@(loop for (slot-name slot-value) on slots by #'cddr
              collect `(setf (ff:fslot-value ,object-var ',slot-name)
                             ,slot-value)))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; spr17917b
(defvar *directory-selection-buffer* nil)

(defun directory-selection-buffer ()
  (or *directory-selection-buffer*
      (make-directory-selection-buffer)))

(defun make-directory-selection-buffer ()
  (setq *directory-selection-buffer*
        (ff:allocate-fobject `(:array char ,*max-file-selection-buffer-size*)
                             :c)))

(defun lisp-string-to-directory-selection-buffer (lisp-string)
  (win:string-to-lptstr lisp-string
                        :address (directory-selection-buffer)))

;;;
;;; Callback function (necessary to set initial selection to
;;  a user-specified initial directory)
;;;

;; Callback messages from file browser
(defconstant BFFM_INITIALIZED        1)
(defconstant BFFM_SELCHANGED         2)
(defconstant BFFM_VALIDATEFAILEDA    3)
(defconstant BFFM_VALIDATEFAILEDW    4)

;; Messages to file browser
(defconstant BFFM_SETSTATUSTEXTA     (+ win:WM_USER 100))
(defconstant BFFM_ENABLEOK           (+ win:WM_USER 101))
(defconstant BFFM_SETSELECTIONA      (+ win:WM_USER 102))
(defconstant BFFM_SETSELECTIONW      (+ win:WM_USER 103))
(defconstant BFFM_SETSTATUSTEXTW     (+ win:WM_USER 104))
(defconstant BFFM_SETSTATUSTEXT      BFFM_SETSTATUSTEXTW)
(defconstant BFFM_SETSELECTION       BFFM_SETSELECTIONW)

;; The the docs for the `BrowseCallbackProc' function.
(ff:defun-foreign-callable browse-callback-proc
    ((hwnd        win:hwnd)
     (message     win:uint)
     (lparam      win:lparam)
     (lparam-data win:lparam))
  (declare (:convention :stdcall) (:unwind 0)
           (ignore lparam))
  (when (and (= message BFFM_INITIALIZED)
             (/= lparam-data 0))
    ;; LPARAM-DATA is the raw item-id-list corresponding to the
    ;; specified initial-directory (0 if no initial-directory was
    ;; specified).
    ;; This is executed when the browse dialog box has finished
    ;; initializing. 
    (win:SendMessage hwnd BFFM_SETSELECTION win:FALSE lparam-data))
  0)

(defparameter *clim-browse-callback-proc-address* nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pathname-to-item-id-list (pathname)
  ;; Convert the given directory pathname to an ITEMIDLIST using the
  ;; IShellFolder::ParseDisplayName API. Returns the ITEMIDLIST
  ;; corresponding to PATHNAME (signals an error on failure).  This
  ;; ITEMIDLIST needs to be freed using the IMalloc allocator returned
  ;; from SHGetMalloc().
  (let ((desktop-folder (allocate-pointer :nat))
        (ole-path (allocate-pointer :unsigned-long (+ 2 (* 2 *max-path*)))))
    (unless (= (SHGetDesktopFolder desktop-folder) win:NOERROR)
      (error "Error in ask-user-for-directory: SHGetDesktopFolder failed."))
    ;; Convert pathname to C string, and then to a 'wide character
    ;; string'.  (IShellFolder::ParseDisplayName requires the file
    ;; name to be in Unicode.)
    (MultiByteToWideChar win:CP_ACP
                         win:MB_PRECOMPOSED 
                         (lisp-string-to-directory-selection-buffer 
                          (namestring pathname))
                         -1 ole-path *max-path*)
    ;; Let IShellFolder::ParseDisplayName turn the directory
    ;; into an ITEMIDLIST.
    (let ((pidl (allocate-pointer 'win:lpcitemidlist))
          (cheaten (allocate-pointer :unsigned-long))
          (attributes (allocate-pointer :unsigned-long)))
      (unless (= (IShellFolderParseDisplayName
		  (pointer-value :nat desktop-folder)
		  0 0 ole-path cheaten pidl
		  attributes)
                 win:NOERROR)
        (error "Error in ask-user-for-directory: ParseDisplayName failed."))
      ;; Return that ITEMIDLIST (actually, a foreign-object
      ;; containing the ITEMIDLIST).
      pidl)))


(defun item-id-list-to-pathname (item-id-list)
  ;; Converts an ITEMIDLIST to a Lisp pathname
  (let (result)
    ;; Convert item-id-list back to a Lisp string.
    (when (SHGetPathFromIDList item-id-list (directory-selection-buffer))
      (setq result (win:lptstr-to-string (directory-selection-buffer)))
      ;; And convert the Lisp string back to a pathname.
      (if (excl:probe-directory result)
          (excl:pathname-as-directory result)
          (pathname result)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ask-user-for-directory (&key (prompt "Select a directory.")
                               associated-window root initial-directory
                               edit-box dont-go-below-domains
				    include-files)
  ;; Ensure that the callback procedure is registered.
  (unless (and (clim-initialized-p *acl-port*)
	       *clim-browse-callback-proc-address*)
    (setf *clim-browse-callback-proc-address*
          (ff:register-foreign-callable 'browse-callback-proc
                                        :reuse :return-value)))
  ;;
  (let* ((malloc (allocate-pointer :nat))
         (root-item-id-list nil)
         (initial-directory-item-id-list nil))
    (unless (eql (SHGetMalloc malloc) windows:NOERROR)
      (error "Error in ask-user-for-directory: SHGetMalloc failed."))
    ;; Convert root and/or initial-directory to ITEMIDLISTs.
    (when root
      (setq root-item-id-list (pathname-to-item-id-list root)))
    (when initial-directory
      (setq initial-directory-item-id-list
            (pathname-to-item-id-list initial-directory)))
    ;;
    (unwind-protect
         (ff:with-stack-fobject (browse-info 'browseinfo)
           ;; The real work: callSHBrowseForFolder.
           (excl:with-native-string (win-prompt prompt)
             (fill-fslots browseinfo browse-info
                          hwndOwner (or (and associated-window
                                             (sheet-mirror associated-window))
                                        0)
                          pidlRoot (if root
                                       (pointer-value :nat root-item-id-list)
                                       0)
                          pszDisplayName 0
                          lpszTitle win-prompt
                          ulFlags (logior win:BIF_RETURNONLYFSDIRS
                                          (if include-files
                                              win:BIF_BROWSEINCLUDEFILES
                                              0)
                                          (if edit-box win:BIF_EDITBOX 0)
                                          (if dont-go-below-domains
                                              win:BIF_DONTGOBELOWDOMAIN
                                              0))
                          lpfn *clim-browse-callback-proc-address*
                          lParam (if initial-directory
                                     (pointer-value
				      :nat initial-directory-item-id-list)
                                     0)
                          iImage 0))
           (let ((item-id-list-out (SHBrowseForFolder browse-info)))
             (when (> item-id-list-out 0)
               (unwind-protect (item-id-list-to-pathname item-id-list-out)
                 ;; Free item-id-list
                 (IMallocFree (pointer-value :nat malloc) item-id-list-out)))))
      ;; Free the item-id-lists returned from pathname-to-item-id-list.
      (when root
        (IMallocFree (pointer-value :nat malloc)
		     (pointer-value :nat root-item-id-list)))
      (when initial-directory
        (IMallocFree (pointer-value :nat malloc)
		     (pointer-value :nat initial-directory-item-id-list)))
      ;;
      (IMallocRelease (pointer-value :nat malloc)))))



;;;
;;; The main function
;;;

(defmethod frame-manager-select-file
    ((framem acl-frame-manager)
     &key (default nil)
	  (frame nil frame-p)
	  (associated-window
	   (if frame-p
	       (frame-top-level-sheet frame)
	     nil))
	  (title "Select a file")
	  documentation
	  file-search-proc
	  directory-list-label
	  file-list-label
	  (exit-boxes '(:exit :abort :help))
	  (name title)
	  directory
	  pattern
	  ;; These are all peculiar to this frame manager
	  (dialog-type :open)
	  (file-types '(("All Files" . "*.*")))
	  (multiple-select nil)
	  (warn-if-exists-p t)
     &allow-other-keys)
  (declare (ignore name exit-boxes file-list-label directory-list-label
		   file-search-proc documentation))
  (unless pattern 
    (setq pattern ""))
  (when (pathnamep default)
    (let ((name (pathname-name default))
	  (type (pathname-type default))
	  (dir (pathname-directory default))
	  (device (pathname-device default)))
      (cond ((and name type)
	     (setq pattern (format nil "~A.~A" name type)))
	    (name
	     (setq pattern name))
	    (type
	     (setq pattern (format nil "*.~A" type))))
      (when (or directory device)
	(setq directory 
	  (namestring (make-pathname :name nil
				     :directory dir
				     :device device))))))
  ;; Massage the directory to make sure, in particular,
  ;; that it has a device.  Expensive, but worth it to 
  ;; avoid a segmentation violation.  JPM.
  (setq directory
        (namestring
         (if directory
             (merge-pathnames (pathname directory) (excl:current-directory))
             (excl:current-directory))))
  (ecase dialog-type 
    ((:open :save)
     (get-pathname title 
                   directory 
                   associated-window
                   file-types
                   pattern
                   (eql dialog-type :save)
                   multiple-select
                   warn-if-exists-p))
    (:directory
     (ask-user-for-directory :associated-window associated-window
                             :prompt title
                             :initial-directory directory))))

