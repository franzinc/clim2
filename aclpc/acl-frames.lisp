;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements frame manager specialization for the ACL for Windows *
*  port.  It supplies support for using menu bars and pointer documentation  *
*  panes, as well as supplying file and notification dialogs.                *
*                                                                            *
*                                                                            *
****************************************************************************|#


(in-package :acl-clim)

(defvar *use-native-menubar* t)

(defclass acl-frame-manager (standard-frame-manager)
    ((msmenubart :initarg :msmenubart :reader msmenubart))
  (:default-initargs :dialog-view #+ignore +textual-dialog-view+
		     +gadget-dialog-view+
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
    (when ;mm: was: (eq menu-bar 't)
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
      (outlining ()
	(cond ((and (not native) menu-bar pointer-doc-pane)
	       (vertically ()
		 (compute-menu-bar-pane frame menu-bar)
		 pane
		 pointer-doc-pane))
	      ((and (not native) menu-bar)
	       (vertically () (compute-menu-bar-pane frame menu-bar) pane))
	      (pointer-doc-pane
	       (vertically () pane pointer-doc-pane))
	      (t pane))))))

(defclass acl-top-level-sheet (top-level-sheet)
  ((min-width :accessor acl-top-min-width :initform nil)
   (min-height :accessor acl-top-min-height :initform nil)
   (accelerator-gestures :initform nil :reader top-level-sheet-accelerator-gestures)
   ))

(defun record-accelerator (sheet keysym command)
  (push (cons keysym command)
	(slot-value sheet 'accelerator-gestures)))

(defun keysymeql (keysyma keysymb)
  (or (eql keysyma keysymb)
      (cond ((and (keywordp keysyma) (typep keysymb 'character))
	     (eql keysyma (char->keysym keysymb))
	     )
	    ((and (keywordp keysymb) (typep keysyma 'character))
	     (eql keysymb (char->keysym keysyma)))
	    (t nil))))

(defun modstateeql (a b) (eql a b))

(defun lookup-accelerator (frame keysym modstate)
  (let* ((sheet (frame-top-level-sheet frame))
	 (gestures (top-level-sheet-accelerator-gestures sheet)))
    (loop with gkeysym and gmodstate
	for gesture-and-command in gestures
	for (gesture) = gesture-and-command
	do (multiple-value-setq (gkeysym gmodstate)
	     (parse-gesture-spec gesture))
	when (and (keysymeql keysym gkeysym)
		  (modstateeql modstate gmodstate))
	return (cdr gesture-and-command))))

(defmethod update-frame-settings ((framem acl-frame-manager) 
				  (frame t))
  (let* ((sheet (frame-top-level-sheet frame))
	 (sr (compose-space sheet))
	 (width (space-requirement-min-width sr))
	 (height (space-requirement-min-height sr)))
    (clim-internals::limit-size-to-graft width height (graft framem))
    (multiple-value-bind (dl dt dw dh) (get-nonclient-deltas sheet)
      (declare (ignore dl dt))
      (setf (acl-top-min-width sheet) (fix-coordinate (+ width dw))
	    (acl-top-min-height sheet) (fix-coordinate (+ height
							  dh))))
    (values width height)))

;; added the following two methods so that the default labels are the
;; same as for the Xt port (cim 9/25/96)

(defmethod frame-manager-exit-box-labels ((framem acl-frame-manager) frame view)
  (declare (ignore frame view))
  '((:exit  "OK" :documentation "Exit from dialog" :show-as-default t)
    (:abort  "Cancel" :documentation "Cancel dialog")))

(defmethod frame-manager-default-exit-boxes ((framem acl-frame-manager))
  '((:exit) (:abort)))

;; Note: items appear never to be removed from *menu-id->command-table*
;; so this will grow indefinitely (cim 9/9/96). There really should be
;; a global id counter and the association list should be kept on a
;; per application-frame basis. 

(defun get-command-menu-item-id (command frame)
  (prog1
    (fill-pointer *menu-id->command-table*)
    #+ignore
    (when (= (fill-pointer *menu-id->command-table*)
	     (array-total-size *menu-id->command-table*))
      (cerror "Continue" "Too many menu items"))
    (vector-push-extend (cons frame command) *menu-id->command-table*
			256)))

(defun find-command-menu-item-id (command frame)
  (position-if
   #'(lambda (x)
       (when x
	 (destructuring-bind (f c &rest args)
	     x
	   (declare (ignore args))
	   (and (eq f frame)
		(eq c command)))))
   *menu-id->command-table*))

;; pr Aug97
(defun map-command-menu-ids (frame func &rest args)
  (dotimes (id (fill-pointer *menu-id->command-table*))
    (let ((x (aref *menu-id->command-table* id)))
      (when x
	(destructuring-bind (f c &rest xargs) x
	  (declare (ignore xargs c))
	  (if (eq f frame) (apply func id args)))))))

;;; Disable all menu items.
(defun clim-internals::enable-menu-items (frame enablep)
  (let* ((sheet (frame-top-level-sheet frame))
	 (mirror (sheet-mirror sheet))
	 (menu (win::getmenu mirror)))
    (map-command-menu-ids
     frame
     #'(lambda (menuid)
	 (let ((command-name (second (aref *menu-id->command-table* menuid))))
	   (with-slots (clim-internals::disabled-commands) frame
	     (if enablep
		 (setf clim-internals::disabled-commands
		   (delete command-name clim-internals::disabled-commands))
	       (push command-name clim-internals::disabled-commands))))))))

;;; Either of these would be nicer, but redisplay of the menu bar causes them to not
;;; always get repainted in their ungrayed state at the end.  pr Aug97

;(setf (command-enabled command-name frame) enablep)
;(win::EnableMenuItem menu menuid (if enablep pc::MF_ENABLED pc::MF_GRAYED))

(defun keysym->char (keysym)
  (if (typep keysym 'character)
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
	 (nlist (list (format nil "~:C" (if (member :shift gesture-spec)
					    (keysym->char keypress)
					  (char-downcase (keysym->char keypress)))))))
    (dolist (shift alist)
      (when (member (first shift) (rest gesture-spec))
	(push (second shift) nlist)))
    (format nil "~A~{~A~}" #\tab nlist)))

(defun make-menu-text (text keystroke item)
  (let* ((mnemonic (getf (command-menu-item-options item) :mnemonic))
	 (pos (position mnemonic text))
	 (newtext (if pos
		      (format nil "~A~~~A"
			      (subseq text 0 pos)
			      (subseq text pos))
		    text)))
    (pc::make-control-text (if keystroke
			       (format nil "~A~A"
				       newtext
				       (gesture-spec-for-mswin keystroke))
			     newtext))))

(defun compute-msmenu-bar-pane (frame top command-table)
  (let* ((text-style
	   (and (listp command-table)
		(getf (cdr command-table) :text-style)
		`(:text-style ,(getf (cdr command-table) :text-style))))
	 (mirror (sheet-mirror top))
	 (menu-handle (win::GetMenu mirror))
	 (command-table
	   (if (listp command-table) (car command-table) command-table)))
    (when
        ;; command-table arg comes from menu-bar slot of frame
        ;; and may be NIL T=menu-hbox-pane command-table-arg
        (silica::default-command-table-p command-table)
      (setq command-table (frame-command-table frame)))
    (with-look-and-feel-realization ((frame-manager frame) frame)
      (labels
	((make-command-table-buttons (command-table menuhand top-level)
	   (let ((menu-item-ids nil))
	     (map-over-command-table-menu-items
	      #'(lambda (menu keystroke item)
		  (let* ((type (command-menu-item-type item))
			 (value (command-menu-item-value item))
			 (menu-item-available-p 
			  (or (not (eq type :command))
			      (command-enabled (car value) frame)))
			 (menu-item-selected-p nil)
			 (acckey (and (not top-level) keystroke))
			 (flags (pc::ilogior
				 pc::MF_STRING
				 (if menu-item-available-p
				     pc::MF_ENABLED pc::MF_GRAYED)
				 (if menu-item-selected-p
				     pc::MF_CHECKED pc::MF_UNCHECKED)))
			 (smflags (pc::ilogior flags pc::MF_POPUP)))
		    (case type
		      (:command
		       (when acckey
			 (record-accelerator top acckey value))
		       (let ((menu-item-id (get-command-menu-item-id value frame)))
			 (win::AppendMenu
			  menuhand
			  flags
			  menu-item-id
			  (make-menu-text menu acckey item))
			 (push menu-item-id menu-item-ids)))
		      (:function
		       ;; do something here
		       )
		      (:menu
		       (let* ((popmenu (win::CreatePopupMenu))
			      (hmenu (pc::handle-value 'win::hmenu popmenu))
			      (menutext (make-menu-text menu acckey item)))
			 (win::AppendMenu menuhand
					  smflags
					  hmenu
					  menutext)
			 (setf (gethash hmenu *popup-menu->menu-item-ids*)
			   (make-command-table-buttons value popmenu nil))))
		      (:divider
		       (unless top-level
			 (win::AppendMenu menuhand
					  pc::MF_SEPARATOR
					  0
					  "x")
			 )))))
	      command-table)
	     menu-item-ids)))
	(make-command-table-buttons command-table menu-handle t)))))

(defun update-menu-item-sensitivities (hmenu)
  (dolist (menu-item-id (gethash hmenu *popup-menu->menu-item-ids*))
    (destructuring-bind (frame command &rest args)
	(aref *menu-id->command-table* menu-item-id)
      (declare (ignore args))
      (let* ((top (frame-top-level-sheet frame))
	     (mirror (sheet-mirror top))
	     (menu-handle (win::GetMenu mirror)))
	(win::EnableMenuItem menu-handle menu-item-id
			     (if (command-enabled command frame)
				 pc::MF_ENABLED
			       pc::MF_GRAYED))))))

(defmethod redisplay-frame-panes :around ((frame standard-application-frame)
					  &key force-p)
  (call-next-method)
  ;;; ensure that the top-level-sheet is visible, mostly for avp frames
  #-ignore ;;-- thought adding this unless would fix growing pains but it didn't.
  (unless clim-internals::*sizing-application-frame*
    (setf (sheet-enabled-p (frame-top-level-sheet frame)) t))
  #+ignore
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) t))

;;--- Should "ungray" the command button, if there is one
(defmethod note-command-enabled ((framem acl-frame-manager) frame command)
  (when (consp command) (setf command (car command)))
  (let* ((top (frame-top-level-sheet frame))
         (mirror (sheet-mirror top))
         (menu-handle (win::GetMenu mirror))
         (command-id (find-command-menu-item-id command frame))
         (flag pc::MF_ENABLED))
    (when menu-handle
      (win::EnableMenuItem menu-handle command-id flag))))

;;--- Should "gray" the command button, if there is one
(defmethod note-command-disabled ((framem acl-frame-manager) frame command)
  (when (consp command) (setf command (car command)))
  (let* ((top (frame-top-level-sheet frame))
         (mirror (sheet-mirror top))
         (menu-handle (win::GetMenu mirror))
         (command-id (find-command-menu-item-id command frame))
         (flag pc::MF_GRAYED))
    (when menu-handle
      (win::EnableMenuItem menu-handle command-id flag))))

;; moved the SetForegroundWindow call from an around method on
;; realize-mirror to the following method to stop the focus moving
;; when the window wasn't yet visible (cim 10/3/96) 

#+acl86win32
(eval-when (load compile eval)
  (load "user32.dll"))

#+acl86win32
(ff:defforeign 'win::setforegroundwindow :entry-point "SetForegroundWindow"
  :arguments '(integer) :return-type :integer)

;;; pclhandle-value not on aclpc+++
#+acl86win32
(defmethod note-frame-enabled :around ((framem acl-frame-manager) frame)
  (call-next-method)
  (let ((*in-layout-avp* *in-layout-avp*)
	(sheet (frame-top-level-sheet frame))
	(avp nil))
    (when sheet
      (map-over-sheets #'(lambda (sheet)
			   (when (typep sheet 'accept-values-pane)
			     (setf avp t)))
		       sheet)
      (setf *in-layout-avp* avp)
      (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)
      (win::setforegroundwindow 
       #+aclpc (win::pclhandle-value (sheet-mirror sheet))
	   #+acl86win32 (sheet-mirror sheet)))))

(defmethod note-frame-layout-changed :after ((framem acl-frame-manager) frame)
  ;; added this to workaround some bugs with new layouts not being
  ;; correctly redisplayed - in particular problems with label-panes
  ;; - this should be viewed as a temporary fix (cim 10/14/96) 
  (repaint-sheet (frame-top-level-sheet frame) +everywhere+)
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
      (let ((win (sheet-mirror sheet))
            (cstr (ct::callocate (:char *) :size 256))
	    (subsize (length name)))
        (dotimes (i subsize)
          (ct::cset (:char 256) cstr ((fixnum i)) (char-int (char name i))))
      (ct::cset (:char 256) cstr ((fixnum subsize)) #-aclpc (char-int #\NULL) #+aclpc 0)
      (win::SetWindowText win cstr)))))

;;; focus setting could be better, and support for exit box choices should
;;; be added
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
		 (exit-boxes '(:exit :abort :help))
		 (name title)
		 text-style
		 &allow-other-keys
		  )
  (let ((stream associated-window)
	(icon (case style
		(:inform cg::information-icon)
		((:warn :warning) cg:warning-icon)
		(:error cg:error-icon))))
    #+(or aclpc acl86win32x)
    (progn
      (setq message-string (pc::cleanup-button-label message-string))
      (cg:pop-up-message-dialog cg::*screen* title message-string
				icon "OK")
      (pc::clear-focus (pc::get-focus cg::*screen*))
      (win::setFocus (sheet-direct-mirror stream) #-acl86win32 :static)
      (clim-internals::stream-set-input-focus stream)
      (enable-mirror *acl-port* stream))
    #-(or aclpc acl86win32x)
    (accepting-values (stream :exit-boxes exit-boxes :label title
			      :own-window t)
      (with-text-style (stream text-style)
	(write-string message-string stream)))
    ))

(defmethod frame-manager-select-file
  ((framem acl-frame-manager)
   &key (default nil default-p)
   (frame nil frame-p)
   (associated-window
	 (if frame-p
		 (frame-top-level-sheet frame)
		 (graft framem)))
   (title "Select a file")
   documentation
   file-search-proc
   directory-list-label
   file-list-label
   (exit-boxes '(:exit :abort :help))
   (name title)
   directory
   pattern
   &allow-other-keys)
  (let* ((stream associated-window)
		 (path-string (or pattern (and default (namestring default))))
		 (dir (or directory
				  (and path-string
					   (subseq path-string 0
							   (position #\\ path-string :from-end t)))
				  (namestring *default-pathname-defaults*))))
	(get-pathname title dir stream '(("All Files" . "*.*")) path-string nil nil nil nil)
	))

;;; ms-windows style command menu support

(define-command-table mswin-file-commands
  :menu (("Exit" :command (com-exit))))

(define-command-table mswin-edit-commands
  :menu (("Copy" :command (com-copy-object))
	 ("Paste" :command (com-paste))))

(define-command-table mswin-help-commands
  :menu (("About" :command (com-about))))

(define-command (com-exit :name "Exit"
			  :command-table mswin-file-commands
			  :menu ("Exit" :documentation "Quit application"))
  ()
  #+ignore
  (format *terminal-io* "~%Quitting ~S" clim:*application-frame*)
  (clim:frame-exit clim:*application-frame*))

;;; gadget switching support

(defvar *generic-gadgets* nil)

(in-package :silica)

(defmethod handle-event ((pane sheet) (event window-close-event))
  (let* ((frame (pane-frame pane)))
    (when frame
      (frame-exit frame))
    ;; this was breaking things when closing a menu - instead now deal
    ;; with closing of menus in the windows event handler directly
    ;; (cim 9/12/96)
    #+ignore
    (destroy-mirror acl-clim::*acl-port* pane)))

(defmethod distribute-event-1 ((port basic-port) (event window-close-event))
  (declare (optimize (speed 3)))
  (with-slots (mirrored-sheet) event
    (dispatch-event mirrored-sheet event)))

(defmethod make-pane-class ((framem acl-clim::acl-frame-manager) class
			    &rest options)
  (declare (ignore options))
  (if acl-clim::*generic-gadgets*
    (second (assoc class '((scroll-bar scroll-bar-pane)
			 (scroller-pane generic-scroller-pane)
			 (viewport viewport)
			 (menu-bar menu-bar-pane)
			 (menu-bar-button-logic menu-bar-button)
			 (pull-down-button-logic pull-down-menu-button)
			 (push-button push-button-pane)
			 (toggle-button toggle-button-pane)
			 (radio-box radio-box-pane)
			 (check-box check-box-pane)
			 (slider slider-pane)
			 (top-level-sheet top-level-sheet)
			 (frame-pane frame-pane)
			 (label-pane generic-label-pane)
			 (text-field text-field-pane)
			 (text-editor text-editor-pane)
			 (list-pane generic-list-pane)
			 (option-pane generic-option-pane)
			 ;;--- Need to do these
			 (horizontal-divider-pane)
			 (vertical-divider-pane)
			 )))
    (second (assoc class '((scroll-bar mswin-scroll-bar)
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
			 ;;--- Need to do these
			 (horizontal-divider-pane)
			 (vertical-divider-pane)
			 )))))

;; modulor the frame-background/foreground hacks this is identical to
;; the default method for standard-frame-manager in silica/framem -
;; the bg/fg hacks should no longer be necessary so I'm commenting
;; this out (cim 10/11/96)

#+ignore
(defmethod adopt-frame ((framem acl-clim::acl-frame-manager) frame)
  (generate-panes framem frame)
  (unless (frame-background frame)
    (setf (frame-background frame) +white+)) ;  added this
  (when (frame-panes frame)
    (let* ((top-pane (frame-panes frame))
	   (sheet (with-look-and-feel-realization (framem frame)
		    (make-pane 'top-level-sheet
			       :event-queue (frame-input-buffer frame)
			       :user-specified-position-p (frame-user-specified-position-p frame)
			       :user-specified-size-p (frame-user-specified-size-p frame)
			       :region (multiple-value-bind (width height)
					   (bounding-rectangle-size top-pane)
					 (make-bounding-rectangle 0 0 width height))
			       :background (frame-background frame)))))
      (sheet-adopt-child (find-graft :port (port frame)) sheet)
      (setf (frame-top-level-sheet frame) sheet
	    (frame-shell frame) (sheet-shell sheet))
	    (sheet-adopt-child sheet top-pane))))


(defmethod port-move-frame ((port acl-clim::acl-port) frame x y)
  (let ((sheet (frame-top-level-sheet frame)))
    (fix-coordinates x y)
    (win::setWindowPos (sheet-mirror sheet)
		       (ct::null-handle win::hwnd) ; we really want win::HWND_TOP
		       x y 0 0
		    (logior win::swp_noactivate
			    win::swp_nozorder
			    win::swp_nosize))))

(in-package :clim-internals)

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
      (let ((wrect (ct::ccallocate win::rect))
	    (handle (sheet-direct-mirror (frame-top-level-sheet frame))))
	;;; +++rl don't show the window here
	;;; the code below makes sure that the frame grows or shrinks
	;;; when the user resizes the frame window
	#+ignore (win::showWindow handle win::sw_show)
	(win::getClientRect handle wrect)
	(win::InvalidateRect handle wrect 1)
	(win::UpdateWindow handle)
	))))

