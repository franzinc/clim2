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
   (sheet-thread :initform nil :accessor clim-internals::sheet-thread)
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
  (map-command-menu-ids
   frame
   #'(lambda (menuid)
       (let ((command-name (second (aref *menu-id->command-table* menuid))))
	 (with-slots (clim-internals::disabled-commands) frame
	   (if enablep
	       (setf clim-internals::disabled-commands
		 (delete command-name clim-internals::disabled-commands))
	     (push command-name clim-internals::disabled-commands)))))))

;;; Either of these would be nicer, but redisplay of the menu bar causes them to not
;;; always get repainted in their ungrayed state at the end.  pr Aug97

;(setf (command-enabled command-name frame) enablep)
;(win::EnableMenuItem menu menuid (if enablep win:MF_ENABLED win:MF_GRAYED))

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
  (typecase x
    (simple-string x)
    (string 
     (let ((new-length 
	    (min (length x) (length nstringify-buffer))))
       (ncopy-vector nstringify-buffer x 0 0 new-length)
       (set-strlen nstringify-buffer new-length)
       nstringify-buffer))
    (null "")
    (t (nprin1-to-string x))))

(defun nstringify-for-control (x)
  (typecase x
    ((not symbol)
     (nstringify x))
    (null "")
    (t
     (let* ((symbol-name (symbol-name x))
	    (new-length 
	     (min (length symbol-name) #.(length nstringify-buffer))))
       (ncopy-vector nstringify-buffer symbol-name 0 0 new-length)
       (set-strlen nstringify-buffer new-length)
       (nstring-capitalize nstringify-buffer :end new-length)))))

(defun schar-byte (string index)
  (if (< index (length string))
      ;; Use aref so it will work on adjustable arrays
      (char-int (aref string index))
    0))

(defun set-schar-byte (string index new)
  (if (< index (length string))
      (etypecase string
	((simple-array character (*))
	 (setf (schar string index) (cltl1:int-char new)))
	(array
	 (setf (aref string index) (cltl1:int-char new)))
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

(defun compute-msmenu-bar-pane (frame top command-table)
  (let* (#+ignore
	 (text-style
	   (and (listp command-table)
		(getf (cdr command-table) :text-style)
		`(:text-style ,(getf (cdr command-table) :text-style))))
	 (mirror (sheet-mirror top))
	 (menu-handle (win:GetMenu mirror))
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
			 (record-accelerator top acckey value))
		       (let ((menu-item-id (get-command-menu-item-id value frame)))
			 (win:AppendMenu
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
			      (hmenu (ct:handle-value 'win::hmenu popmenu))
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
					  win:MF_SEPARATOR
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
				 win:MF_ENABLED
			       win:MF_GRAYED))))))

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
  (let* ((sheet (frame-top-level-sheet frame))
	 (thread (when sheet (clim-internals::sheet-thread sheet))))
    (unless (eq thread (current-process))
      ;; Lisp may hang badly if you proceed.
      (cerror "I don't care if the application crashes or hangs" 
	      "This window was created in thread ~S,
which is not its creator.
Windows does not allow a window created in one thread 
to be run from another."
	     thread))))

;;--- Should "ungray" the command button, if there is one
(defmethod note-command-enabled ((framem acl-frame-manager) frame command)
  (when (consp command) (setf command (car command)))
  (let* ((top (frame-top-level-sheet frame))
         (mirror (sheet-mirror top))
         (menu-handle (win::GetMenu mirror))
         (command-id (find-command-menu-item-id command frame))
         (flag win:MF_ENABLED))
    (when menu-handle
      (win::EnableMenuItem menu-handle command-id flag))))

;;--- Should "gray" the command button, if there is one
(defmethod note-command-disabled ((framem acl-frame-manager) frame command)
  (when (consp command) (setf command (car command)))
  (let* ((top (frame-top-level-sheet frame))
         (mirror (sheet-mirror top))
         (menu-handle (win::GetMenu mirror))
         (command-id (find-command-menu-item-id command frame))
         (flag win:MF_GRAYED))
    (when menu-handle
      (win::EnableMenuItem menu-handle command-id flag))))

;; moved the SetForegroundWindow call from an around method on
;; realize-mirror to the following method to stop the focus moving
;; when the window wasn't yet visible (cim 10/3/96) 

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
      ;; On Windows95, this call often fails when enabling a menu-frame.
      ;; But the menu shows up after a few seconds anyway.  So that is
      ;; why it would be bad to check the return status of this guy.
      (win:SetForegroundWindow (sheet-mirror sheet)))))

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
	(ct::cset (:char 256) cstr ((fixnum subsize)) 
		  #-aclpc (char-int #\NULL) #+aclpc 0)
      (or (win:SetWindowText win cstr)
	  (error "SetWindowText: system error ~s" (win:getlasterror)))))))

(defun select-messagebox-icon (style)
  ;; Decides which Windows icon matches this (standardized) style. 
  (if (member style '#.`(,win:mb_iconinformation
			 ,win:mb_iconquestion
			 ,win:mb_iconexclamation
			 ,win:mb_iconstop
			 ,win:mb_iconhand ; ?
			 ,win:mb_iconasterisk ; ?
			 ))
      style				; user knows what they want
    (case style
      (:message win:mb_iconinformation)
      (:inform win:mb_iconinformation)
      (:question win:mb_iconquestion)
      ((:warn :warning) win:mb_iconexclamation)
      (:error win:mb_iconstop)
      (otherwise win:mb_iconinformation))))

(defun select-messagebox-buttons (exit-boxes)
  ;; Decides which predefined button collection to use.
  ;; You take a big performance hit here over using the constant,
  ;; but hey, this is CLIM, we're minimizing developers' time.
  ;; Return NIL if nothing seems to match, and the caller should
  ;; fall back on the more general, but less pretty, accepting values.
  (if (atom exit-boxes)
      (when (member exit-boxes '#.`(,win:mb_ok
				    ,win:mb_okcancel
				    ,win:mb_yesno
				    ,win:mb_retrycancel
				    ,win:mb_yesnocancel
				    ,win:mb_abortretryignore))
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
		      win:mb_yesnocancel)
		     ((and (find-label "Abort")
			   (find-label "Retry")
			   (find-label "Ignore"))
		      win:mb_abortretryignore)))
	      ((= number 2)
	       (cond ((or (find-naked-key :abort)
			  (find-label "Cancel"))
		      (cond ((or (find-naked-key :exit)
				 (find-label "OK"))
			     win:mb_okcancel)
			    ((find-label "Retry")
			     win:mb_retrycancel)))
		     ((and (find-label "Yes")
			   (find-label "No"))
		      win:mb_yesno)))
	      ((= number 1)
	       (when (or (find-naked-key :exit)
			 (find-label "OK"))
		 win:mb_ok)))))))

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
    (cond ((= button-style win:mb_ok)
	   (or (find-label "OK") :exit))
	  ((= button-style win:mb_yesno)
	   (cond ((= code win:idyes) (or (find-label "Yes") :exit))
		 ((= code win:idno) (or (find-label "No") :abort))))
	  ((= button-style win:mb_retrycancel)
	   (cond ((= code win:idretry) (or (find-label "Retry") :exit))
		 ((= code win:idcancel) (or (find-label "Cancel") :abort))))
	  ((= button-style win:mb_okcancel)
	   (cond ((= code win:idok) (or (find-label "OK") :exit))
		 ((= code win:idcancel) (or (find-label "Cancel") :abort))))
	  ((= button-style win:mb_yesnocancel)
	   (cond ((= code win:idyes) (or (find-label "Yes") :exit))
		 ((= code win:idno) (or (find-label "No") :no))
		 ((= code win:idcancel) (or (find-label "Cancel") :abort))))
	  ((= button-style win:mb_abortretryignore)
	   (cond ((= code win:idabort) (or (find-label "Abort") :abort))
		 ((= code win:idretry) (or (find-label "Retry") :retry))
		 ((= code win:idignore) (or (find-label "Ignore") :exit)))))))

(defun message-box (hwnd message-string name &optional icon)
  (win:messagebox hwnd
		  (coerce message-string 'simple-string)
		  (coerce name 'simple-string)
		  (or icon 
		      (logior win:MB_ICONSTOP 
			      win:MB_TASKMODAL))))
 
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
		      (logior win:MB_TASKMODAL icon buttons)))
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
  (flet ((print-item (item)
	   (with-output-to-string (stream)
	     (funcall (or printer #'print-menu-item) item stream))))
    (declare (dynamic-extent #'print-item))
    (incf tick)
    (ecase (clim-internals::menu-item-type item)
      (:divider
       (win:appendmenu popmenu win:MF_SEPARATOR tick 0))
      (:label
       (win:appendmenu popmenu win:MF_DISABLED tick 
		       (print-item item)))
      (:item
       (if (clim-internals::menu-item-items item)
	   (let ((submenu (win:createpopupmenu)))
	     (push submenu submenus)
	     ;; submenu
	     (win:appendmenu popmenu win:MF_POPUP submenu
			     (print-item item))
	     (map nil
	       #'(lambda (it)
		   (multiple-value-setq (tick alist submenus)
		     (do-one-menu-item submenu it printer
				       tick alist submenus)))
	       (clim-internals::menu-item-items item)))
	 (progn
	   (push (list tick (menu-item-value item))
		 alist)
	   (win:appendmenu popmenu win:MF_ENABLED tick 
			   (print-item item))))))
    (values tick alist submenus)))

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
	  scroll-bars)
  (declare (ignore text-style
		   gesture cache-test cache-value
		   id-test unique-id label))
  ;; What we oughta do here is implement a real Win32 menu.
  ;; Think about calling CreatePopupMenu
  (if (or presentation-type foreground background cache row-wise n-columns n-rows scroll-bars)
      (call-next-method)
    #+simple-but-sure
    (apply #'call-next-method framem items :scroll-bars nil keys)
    (let ((popmenu (win::CreatePopupMenu))
	  (submenus nil)
	  (flags (logior win:tpm_returncmd win:tpm_nonotify
			 win:tpm_leftbutton win:tpm_rightbutton))
	  (tick 0)
	  (alist nil)
	  (code 0))
      (when (zerop popmenu)
	(error "CreatePopupMenu -- system error ~A" (win:getlasterror)))
      ;; These are probably the wrong coordinates.
      (unless (and x-position y-position)
	(multiple-value-setq (x-position y-position)
	  (stream-pointer-position associated-window)))
      (setq x-position (truncate x-position))
      (setq y-position (truncate y-position))
      (map nil #'(lambda (item)
		   (multiple-value-setq (tick alist submenus)
		     (do-one-menu-item popmenu item printer 
				       tick alist submenus)))
	   items)
      (setq code
	(win:trackpopupmenu
	 popmenu flags x-position y-position 0 
	 (sheet-mirror associated-window) 0))
      (win:destroymenu popmenu)
      (dolist (submenu submenus) (win:destroymenu submenu))
      (second (assoc code alist)))))

(defun make-filter-string (dotted-pair)
  (format nil "~a (~a)~a~a~a"
	  (car dotted-pair) (cdr dotted-pair) (cltl1:int-char 0)
	  (cdr dotted-pair) (cltl1:int-char 0)))

(eval-when (compile load eval) 
  (defconstant *scratch-string-length* 256)
  )

(defparameter *scratch-lisp-string*
    (make-string *scratch-string-length*))

(defparameter *scratch-c-string*
  (ff::allocate-fobject-c `(:array :char ,*scratch-string-length*)))

(defun lisp-string-to-scratch-c-string (lisp-string)
  (let ((length (min (length lisp-string)
		     (1- *scratch-string-length*))))
    (dotimes (i length 
	       ;; null term
	       (setf (ff::fslot-value-c '(:array :char 1)
					*scratch-c-string*
					length) 0))
      (setf (ff::fslot-value-c '(:array char 1) *scratch-c-string*
			       i)
	(char-int (aref lisp-string i))))
    *scratch-c-string*
    ))

(defun scratch-c-string-to-lisp-string ()
  (ff:char*-to-string *scratch-c-string*))

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
      #+(or (not aclmerge) (and 386 (not UNIX)))
      (unless (eql (aref directory (1- (length directory)))
		   #\\)
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

(defun get-pathname
    (prompt host stream allowed-types initial-name
     save-p multiple-p change-current-directory-p
     warn-if-exists-p)
  (flet ((fill-c-string (string)
	   (let ((c-string (ff::allocate-fobject-c '(:array :char 256)))
		 (length (length string)))
	     (dotimes (i length (setf (ff::fslot-value-c '(:array :char 1)
							 c-string
							 length) 0))
	       (setf (ff:fslot-value-c '(:array char 1) c-string i)
		 (char-int (aref string i))))
	     c-string)))
    (let* ((open-file-struct (ct:ccallocate win:openfilename))
	   (file-filter-string (fill-c-string
				(apply #'concatenate 'string
				       (mapcar #'make-filter-string allowed-types))))
	   (initial-dir-string (fill-c-string
				(or host (namestring *default-pathname-defaults*))))
	   (prompt-string (fill-c-string prompt)))
      (ct:csets win:fi-openfilename open-file-struct
	     struct-size (ct:sizeof win:openfilename)
	     owner (clim::sheet-mirror stream)
	     hinst *hinst*
	     file-filter file-filter-string
	     custom-filter (ct:ccallocate (:void *) :initial-value 0)
	     max-custom-filter 0 ;; length of custom filter string
	     filter-index 0		; zero means use custom-filter if supplied
					; otherwise the first filter in the list
	     selected-file (lisp-string-to-scratch-c-string (or initial-name ""))
	     max-file 256
	     file-title (ct:ccallocate (:void *) :initial-value 0)
	     max-file-title 0
	     initial-dir initial-dir-string
	     window-title prompt-string
	     flags (logior
		    (if multiple-p win:ofn_allowmultiselect 0)
		    (if save-p 0 win:ofn_filemustexist)
		    (if warn-if-exists-p win:ofn_overwriteprompt 0)
		    (if change-current-directory-p
			0 win:ofn_nochangedir)
		    win:ofn_hidereadonly
		    )
	     default-extension (ct:ccallocate (:void *) :initial-value 0)
	     custom-data 0 ;; would be passed to the callback
	     ;; callback ;; ignored since we don't pass that flag
	     ;; template-name ;; ignored
	     )
      (let* ((error-code (if save-p
			     (win:GetSaveFileName open-file-struct)
			   (win:GetOpenFileName open-file-struct))))
	(dolist (c-string (list file-filter-string initial-dir-string prompt-string))
	  (ff::free-fobject-c c-string))
	(if error-code ;; t means it worked
	    (if multiple-p
		(pathnames-from-directory-and-filenames
		 (spaced-string-to-list
		  (string-downcase
		   (scratch-c-string-to-lisp-string))))
	      (pathname
	       (string-downcase
		(scratch-c-string-to-lisp-string))))
	  (let ((error-code (win:CommDlgExtendedError)))
	    (and (plusp error-code) ;; zero means cancelled, so return NIL
		 (error (format nil 
				"Common dialog error ~a."
				(or (cdr (assoc error-code
						common-dialog-errors))
				    error-code))))))))))

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
  (declare (ignore name exit-boxes file-list-label directory-list-label
		   file-search-proc documentation default-p))
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
    (or (win:setWindowPos (sheet-mirror sheet)
			  (ct::null-handle win::hwnd) ; we really want win::HWND_TOP
			  x y 0 0
			  (logior win:swp_noactivate
				  win:swp_nozorder
				  win:swp_nosize))
	(error "SetWindowPos: system error ~s" (win:getlasterror)))))

(in-package :clim-internals)

;; TO DO: Check for the case of a frame enabled in one thread and
;; then run-frame-top-level in another thread.  Should be an error.
;; Lisp will hang.

(defmethod enable-frame :before ((frame standard-application-frame))
  (let* ((sheet (frame-top-level-sheet frame))
	 (mirror (when sheet (sheet-direct-mirror sheet))))
    (when mirror
      ;; Validate the window handle to give a better error message.
      (or (win:iswindow mirror)
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
      (let ((wrect (ct::ccallocate win::rect))
	    (handle (sheet-direct-mirror (frame-top-level-sheet frame))))
	;;; +++rl don't show the window here
	;;; the code below makes sure that the frame grows or shrinks
	;;; when the user resizes the frame window
	#+ignore (win::showWindow handle win::sw_show)
	(or (win:getClientRect handle wrect)
	    (error "GetClientRect: system error ~s" (win:getlasterror)))
	(or (win:InvalidateRect handle wrect 1)
	    (error "InvalidateRect: system error ~s" (win:getlasterror)))
	(or (win:UpdateWindow handle)
	    (error "UpdateWindow: system error ~s" (win:getlasterror)))
	))))

(defun clean-frame (frame)
  (declare (ignore frame))
  ;; (disable-frame frame)
  ;; (enable-frame frame)
  nil)

(defun frame-find-position (frame)
  (when frame 
    (let* ((wrect (ct::ccallocate win::rect))
	   (sheet (frame-top-level-sheet frame))
	   (handle (when sheet (sheet-mirror sheet))))
      (when handle
	(win::GetWindowRect handle wrect)
	(values (ct::cref win::rect wrect win::left) 
		(ct::cref win::rect wrect win::top))))))

(defun frame-set-position (frame x y)
  (win:setWindowPos (sheet-mirror (frame-top-level-sheet frame))
     (ct::null-handle win::hwnd) ; we really want win::HWND_TOP
     x y 0 0
     (logior win:swp_noactivate
	     win:swp_nozorder
	     win:swp_nosize)))