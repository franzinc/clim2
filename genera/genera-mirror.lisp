;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: genera-mirror.lisp,v 1.4 92/05/07 13:13:28 cer Exp $

(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 International Lisp Associates."


(defvar *clim-window-count* 0)
(defvar *temporary-genera-mirror-desired* nil)

(defmethod realize-mirror ((port genera-port) sheet)
  (with-slots (screen height-pixels) port
    (multiple-value-bind (left top right bottom)
	(sheet-native-region* sheet)
      (fix-coordinates left top right bottom)
      (let* ((genera-parent (sheet-mirror sheet))
	     (name (format nil "CLIM window ~D" (incf *clim-window-count*)))
	     (mirror
	       (tv:make-window
		 (if *temporary-genera-mirror-desired*
		     'temporary-genera-window
		     'genera-window)
		 ;; probably the screen
		 :superior genera-parent
		 :name name
		 :sheet sheet
		 :save-bits t
		 :deexposed-typeout-action :permit
		 :label nil  ;;name
		 ;; Want inside/outside coord system to match if possible
		 :borders nil
		 :x left :y top
		 :width (- right left) :height (- bottom top))))
	(multiple-value-bind (mleft mtop mright mbottom)
	    (scl:send mirror :margins)
	  (decf left mleft)
	  (decf top mtop)
	  (incf right mright)
	  (incf bottom mbottom))
	(multiple-value-bind (clip-left clip-top clip-right clip-bottom)
	    (scl:send (tv:sheet-superior mirror) :inside-edges)
	  (multiple-value-bind (left top right bottom)
	      (fit-region*-in-region* left top right bottom
				      clip-left clip-top clip-right clip-bottom)
	    (scl:send mirror :set-edges left top right bottom)))
	;;--- Do we need to do this?
	(setf (sheet-native-transformation sheet) +identity-transformation+)
	mirror))))

(defmethod realize-mirror :around ((port genera-port) (sheet top-level-sheet))
  (let ((*temporary-genera-mirror-desired* 
	  (getf (frame-properties (pane-frame sheet)) :save-under)))
    (call-next-method)))

(defmethod destroy-mirror ((port genera-port) sheet)
  (let ((window (sheet-direct-mirror sheet)))
    (when window
      (scl:send window :kill))))

(defmethod enable-mirror ((port genera-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (scl:send mirror :expose)))

(defmethod disable-mirror ((port genera-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (scl:send mirror :deactivate)))

;;--- Is this the same as WINDOW-STACK-ON-TOP?
(defmethod raise-mirror ((port genera-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (unless (scl:send window :exposed-p)
      (scl:send window :expose))
    (let ((screen (tv:sheet-screen window))
	  (console (tv:sheet-console window))
	  (mouse (tv:sheet-mouse window)))
      ;; SCREEN-MANAGE-AUTOEXPOSE-INFERIORS doesn't reliably select a window
      ;; that fills the entire screen
      (let ((sw (tv:console-selected-window console)))
	(unless (and sw (atom (tv:sheet-lock sw)))
	  (scl:send window :select)))
      (scl:send screen :screen-select)
      (unless (eq screen (tv:sheet-screen (tv:mouse-sheet mouse)))
	(tv:mouse-set-sheet screen)))))

;;--- Is this the same as WINDOW-STACK-ON-BOTTOM?
(defmethod bury-mirror ((port genera-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (scl:send window :bury)))

(defmethod realize-graft ((port genera-port) graft)
  (with-slots (silica::pixels-per-point silica::pixel-width silica::pixel-height
	       silica::mm-width silica::mm-height silica::units) graft
    (let ((screen (slot-value port 'screen)))
      (setf silica::pixel-width  (scl:send screen :inside-width)
	    silica::pixel-height (scl:send screen :inside-height)
	    ;;--- Bogus numbers
	    silica::mm-width     360.0
	    silica::mm-height	 280.0
	    silica::pixels-per-point 1)
      (setf (sheet-region graft)
	    (ecase silica::units
	      ((:device :pixel)
	       (make-bounding-rectangle
		 0 0 
		 silica::pixel-width silica::pixel-height))))
      (setf (sheet-native-transformation graft) +identity-transformation+)
      (setf (sheet-direct-mirror graft) screen)
      (update-mirror-transformation port graft))))

(defmethod genera-mirror-native-edges* ((port genera-port) sheet &optional mirror)
  (let ((mirror (or mirror (sheet-mirror sheet))))
    (multiple-value-bind (width height)
	(scl:send mirror :size)
      (multiple-value-bind (xoff yoff)
	  (tv:sheet-calculate-offsets mirror (tv:sheet-screen mirror))
	(values xoff
		yoff
		(+ xoff width)
		(+ yoff height))))))

(defmethod mirror-region* ((port genera-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (genera-mirror-native-edges* port sheet mirror))))

(defmethod mirror-region ((port genera-port) sheet)
  (multiple-value-call #'make-bounding-rectangle
    (mirror-region* port sheet)))

(defmethod mirror-inside-region* ((port genera-port) sheet)
  (scl:send (sheet-mirror sheet) :inside-edges))

;;--- Shouldn't this be the same as MIRROR-REGION*?
(defmethod mirror-native-edges* ((port genera-port) sheet)
  (let ((mirror (sheet-direct-mirror sheet)))
    (genera-mirror-native-edges* port sheet mirror)))

;;--- Shouldn't this be the same as MIRROR-INSIDE-REGION*?
(defmethod mirror-inside-edges* ((port genera-port) sheet)
  (multiple-value-bind (left top right bottom)
      (mirror-native-edges* port sheet)
    (values (coordinate 0) (coordinate 0) 
	    (- right left) (- bottom top))))
 
(defmethod set-sheet-mirror-edges* ((port genera-port) sheet 
				    left top right bottom)
  (let ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (mleft mtop mright mbottom)
	(scl:send mirror :margins)
      (decf left mleft)
      (decf top mtop)
      (incf right mright)
      (incf bottom mbottom))
    (multiple-value-bind (clip-left clip-top clip-right clip-bottom)
	(scl:send (tv:sheet-superior mirror) :inside-edges)
      (multiple-value-bind (left top right bottom)
	  (fit-region*-in-region* left top right bottom
				  clip-left clip-top clip-right clip-bottom)
	(scl:send mirror :set-edges (fix-coordinate left)  (fix-coordinate top)
				    (fix-coordinate right) (fix-coordinate bottom))))))


;; Port trigger not nil means that the port is triggering the
;; enabling/disabling.  This allows port enabled/disabled methods
;; to distinguish which way the flow of control is going.
(defvar *port-trigger* nil)

(scl:defflavor genera-window
	((sheet nil)
	 (mouse-x nil) 
	 (mouse-y nil)
	 (mouse-moved nil)
	 (blinker-table nil)
	 (selected-inferior nil))
	(dw::margin-mixin			;scroll bars, labels, borders
	 tv:no-screen-managing-mixin		;don't auto-select a pane
	 tv:changeable-name-mixin
	 tv:window)
  (:initable-instance-variables sheet)
  (:writable-instance-variables sheet)
  (:default-init-plist :borders nil
		       :save-bits nil		;adjusted later if top-level
		       :deexposed-typeout-action :permit
		       :blinker-p nil
		       :label nil))

(scl:defflavor temporary-genera-window
	()
	(tv:temporary-window-mixin genera-window))

;; All the sheet windows in a given activity (top level window) share the same io-buffer, and
;; CLIM decides which CLIM window to give the input to.
;; A top-level window should have a bit save array if we want the screen manager
;; to do what users expect.
(scl:defmethod (:init genera-window :before) (args)
  (declare (ignore args))
  (let ((top-level (not (typep tv:superior 'genera-window))))
    (unless tv:io-buffer
      (setq tv:io-buffer (if top-level
			     (tv:make-io-buffer 100)
			     (scl:send tv:superior :io-buffer))))
    (when top-level
      ;; This is like :SAVE-BITS :DELAYED
      (setf (tv:sheet-save-bits) 1))))

(scl:defmethod (:init temporary-genera-window :after) (args)
  (declare (ignore args))
  (setf (tv:sheet-save-bits) 0))

;;; The window manager should manipulate the highest "CLIM sheet" when invoked
;;; on an inferior.
(scl:defmethod (:alias-for-selected-windows genera-window) ()
  (if (typep tv:superior 'genera-window)
      (scl:send tv:superior :alias-for-selected-windows)
      scl:self))

(scl:defmethod (:name-for-selection genera-window) ()
  (or (let ((frame (pane-frame sheet)))
	(and frame (frame-pretty-name frame)))
      (let ((label (scl:send scl:self :label)))
	(and label (scl:string-search-not-char #\sp label) label))
      tv:name))

;;; A combination of the methods of pane-mixin and basic-frame
(scl:defwhopper (:select genera-window) (&rest args)
  (when (scl:send tv:superior :inferior-select scl:self)
    (cond ((and selected-inferior
		(scl:send selected-inferior :exposed-p))
	   (scl:lexpr-send selected-inferior :select args))
	  ((and (null selected-inferior)
		tv:inferiors
		(let ((frame (window-stream-to-frame sheet)))
		  (when frame
		    ;; This is a frame whose input focus has not yet been directed to any pane
		    ;; Choose the interactor pane by default
		    (let ((stream (frame-query-io frame)))
		      (when stream
			(scl:lexpr-send (slot-value (sheet-medium stream) 'window)
					:select args)
			t))))))
	  (t
	   (scl:lexpr-continue-whopper args)))))

;;; As for basic-frame
(scl:defmethod (:inferior-select genera-window) (pane)
  (setq selected-inferior pane)
  (scl:send tv:superior :inferior-select scl:self))

;;; As for basic-frame
(scl:defwhopper (:select-relative genera-window) ()
  (if (and selected-inferior
	   (scl:send selected-inferior :exposed-p))
      (scl:send selected-inferior :select-relative)
      (scl:continue-whopper)))

;;; As for pane-mixin
;;; "When selecting a pane with the mouse, pass the selection request to the frame."
(scl:defwhopper (:mouse-select genera-window) (&rest args)
  (if (typep tv:superior 'genera-window)
      (scl:lexpr-send tv:superior ':mouse-select args)
      (scl:lexpr-continue-whopper args)))

;; When the window is first activated ("mapped") run the repaint handler
(scl:defmethod (:expose genera-window :after) (&rest args)
  (declare (ignore args))
  (scl:send tv:superior :inferior-expose scl:self)
  (unless *port-trigger*
    (let ((*port-trigger* t))
      (setf (sheet-enabled-p sheet) t))))

(scl:defmethod (:inferior-expose genera-window) (inferior)
  (when (eq inferior selected-inferior)
    (scl:send inferior :select-relative)))

(scl:defwhopper (:set-reverse-video-p genera-window) (reverse-video-p)
  (let ((old-reverse-video-p (scl:send scl:self :reverse-video-p)))
    (multiple-value-prog1
      (scl:continue-whopper reverse-video-p)
      (unless (eq reverse-video-p old-reverse-video-p)
	(labels ((switch-inks (sheet)
		   (let ((medium (sheet-medium sheet)))
		     ;; Inhibit refreshing until we're done
		     (rotatef (slot-value medium 'foreground)
			      (slot-value medium 'background)))
		   (dolist (sheet (sheet-children sheet))
		     (switch-inks sheet))))
	  (declare (dynamic-extent #'switch-inks))
	  (switch-inks sheet)
	  (scl:send scl:self :refresh))))))

(scl:defmethod (:refresh genera-window :before) (&optional (type :complete-redisplay))
  (declare (ignore type))
  ;; Invalidate all cached inks, so they'll be redecoded taking into account any
  ;; changed parameters such as reversed video.
  (let* ((medium (sheet-medium sheet))
	 (cache (and medium (slot-value medium 'ink-cache))))
    (when cache
      (dotimes (i (array-total-size cache))
	(setf (aref cache i) nil)))
    (let ((foreground (genera-decode-color (medium-foreground medium) medium nil))
	  (background (genera-decode-color (medium-background medium) medium nil)))
      (funcall scl:self :set-char-aluf foreground)
      (funcall scl:self :set-erase-aluf background))))

(scl:defmethod (:refresh genera-window :after) (&optional (type ':complete-redisplay))
  (declare (ignore type))
  (unless *port-trigger*
    (let ((*port-trigger* t))
      (with-sheet-medium (medium sheet)
	(handle-repaint sheet medium (sheet-region sheet))))))

(scl:defmethod (tv:refresh-rectangle genera-window) (left top right bottom)
  #+++ignore (setf (slot-value sheet 'highlighted-presentation) nil)
  (frame-replay *application-frame*
		sheet (or (pane-viewport-region sheet)
			  (sheet-region sheet))))

;;; Called on position changes as well as size?
(scl:defmethod (:change-of-size-or-margins genera-window :after) (&rest options)
  (declare (ignore options))
  (let ((*port-trigger* t)
	(port (port sheet)))
    (dispatch-event
      sheet
      (let ((region (mirror-region port sheet)))
	(make-instance 'window-configuration-event
		       :native-region region
		       :region (untransform-region (sheet-native-transformation sheet) region)
		       :sheet sheet)))))

;;--- Problem: we disable the viewport that corresponds to the Genera
;;--- window, but that leaves the window stream enabled.  Should en/disable
;;--- on the viewport forward to the drawing-surface?
(scl:defmethod (:deexpose genera-window :after) (&rest ignore)
  (unless *port-trigger*
    (let ((*port-trigger* t))
      ;;--- Want to call shrink-sheet, but that doesn't appear to be supported
      #+++ignore 
      (disable-sheet sheet))))

(scl:defmethod (:ensure-blinker-for-cursor genera-window) (cursor)
  (unless blinker-table (setq blinker-table (make-hash-table)))
  (or (gethash cursor blinker-table)
      (let ((blinker (tv:make-blinker scl:self 'tv:rectangular-blinker :follow-p t)))
	(setf (gethash cursor blinker-table) blinker)
	(scl:send blinker :set-visibility nil)
	blinker)))


;;; Save this state (rather than using (mouse-x mouse)) 'cause the coords
;;; are in window coordinates already.
(defvar *mouse-moved* nil)
(defvar *mouse-x* nil)
(defvar *mouse-y* nil)
(defvar *mouse-buttons* 0)
(defvar *mouse-window* nil)
(defvar *mouse-button-released* nil)

;; So we don't generate spurious motion events
(defvar *old-mouse-x* nil)
(defvar *old-mouse-y* nil)

;;; Really should be per console, as with all of these special variables.
(defvar *old-mouse-chord-shifts* 0)

(defun-inline buttons-up (old-buttons new-buttons)
  #+Genera (declare (values buttons-up buttons-down))
  (values (boole boole-andc2 old-buttons new-buttons)
	  (boole boole-andc2 new-buttons old-buttons)))

(defun invoke-on-genera-shift-keysyms (genera-shift-mask continuation)
  (declare (dynamic-extent continuation))
  (macrolet ((do-shift (shift shift-name)
	       `(when (si:bit-test (si:name-bit ,shift) genera-shift-mask)
		  (funcall continuation ,shift-name))))
    ;; why is SHIFT different from sys:%kbd-shifts-shift
    (when (ldb-test (byte 1 4) genera-shift-mask)
      (funcall continuation :left-shift))
    (do-shift :control :left-control)
    (do-shift :meta :left-meta)
    (do-shift :super :left-super)
    (do-shift :hyper :left-hyper))
  nil)

(defmacro map-over-genera-shift-keysyms ((keysym-var genera-shift-mask) &body body)
  `(flet ((map-over-genera-shift-keysyms
	     (,keysym-var)
	    ,@body))
     (declare (dynamic-extent #'map-over-genera-shift-keysyms))
     (invoke-on-genera-shift-keysyms
       ,genera-shift-mask #'map-over-genera-shift-keysyms)))

;;; This is a horrible kludge for CLIM purposes.
(scl:advise tv:mouse-set-blinker-cursorpos-internal :after compute-buttons-released nil
  (kludge-buttons-released-when-mouse-over-no-window (first scl:arglist)))

;;; This is the guts of the horrible kludge for CLIM purposes.
(defun kludge-buttons-released-when-mouse-over-no-window (mouse)
  (multiple-value-bind (x y)
      (if (tv:mouse-warp-internal mouse)
	  (values (tv:mouse-last-x mouse)
		  (tv:mouse-last-y mouse))
	  (values (tv:mouse-x mouse)
		  (tv:mouse-y mouse)))
    (without-interrupts
      (let* ((old-buttons *mouse-buttons*)
	     (new-buttons tv:mouse-last-buttons)
	     (buttons-up (buttons-up old-buttons new-buttons))
	     ;; merge them all, for now.  This might drop a second up
	     ;; transition before the first one is noticed.  Even if we
	     ;; handled that here (by delaying updating *mouse-buttons*
	     ;; (actually, LOGIORing old and new) and
	     ;; *mouse-button-released*, it could require a mouse motion
	     ;; to actually notice the button up, which is gross!
	     (merged-up (logior buttons-up (or *mouse-button-released* 0)))
	     (new-released (and (not (zerop merged-up)) merged-up)))
	(setq *old-mouse-x* *mouse-x* 
	      *old-mouse-y* *mouse-y*)
	(setq *mouse-buttons* new-buttons
	      ;; doesn't yet handle multiple buttons released at once...
	      *mouse-button-released* new-released
	      *mouse-window* (let ((win
				     (tv:window-under-mouse-internal
				       mouse ':mouse-select ':active x y)))
			       (when (typep win 'genera-window) win))
	      *mouse-x* x
	      *mouse-y* y
	      *mouse-moved* ':pointer-motion)))))

(si:compile-advice 'tv:mouse-set-blinker-cursorpos-internal)

(scl:defmethod (:mouse-moves genera-window :after) (x y)
  (without-interrupts
    (let* ((old-buttons *mouse-buttons*)
	   (new-buttons tv:mouse-last-buttons)
	   (buttons-up (buttons-up old-buttons new-buttons))
	   ;; merge them all, for now.  This might drop a second up
	   ;; transition before the first one is noticed.  Even if we
	   ;; handled that here (by delaying updating *mouse-buttons*
	   ;; (actually, LOGIORing old and new) and
	   ;; *mouse-button-released*, it could require a mouse motion
	   ;; to actually notice the button up, which is gross!
	   (merged-up (logior buttons-up (or *mouse-button-released* 0)))
	   (new-released (and (not (zerop merged-up)) merged-up)))
      (setq *old-mouse-x* *mouse-x* 
	    *old-mouse-y* *mouse-y*)
      (setq *mouse-window* scl:self
	    *mouse-x* x
	    *mouse-y* y
	    *mouse-buttons* new-buttons
	    ;; doesn't yet handle multiple buttons released at once...
	    *mouse-button-released* new-released
	    *mouse-moved* ':pointer-motion))))

(scl:defmethod (:handle-mouse genera-window :before) ()
  (scl:send scl:self :select)			;---why??
  (without-interrupts
    (setq *mouse-window* scl:self
	  *mouse-moved* ':enter-notify
	  *mouse-x* -1
	  *mouse-y* -1)))			;--- Unsure this is right.

(scl:defmethod (:handle-mouse genera-window :after) ()
  (ecase :give-up-kbd-focus			;!!
    (:deselect-too-big-a-hammer (scl:send scl:self :deselect))
    (:give-up-kbd-focus
      (let* ((exposed (scl:send (tv:sheet-screen scl:self) :exposed-inferiors))
	     (window (find-if #'(lambda (window) (member window exposed))
			      tv:previously-selected-windows)))
	(when window (scl:send window :select)))))
  (without-interrupts
    (setq *mouse-window* scl:self		;--- Which window is this notification for?
	  *mouse-moved* ':leave-notify
	  *mouse-x* -1
	  *mouse-y* -1)))			;--- ???

#+++ignore
(scl:defmethod (:who-line-documentation-string genera-window) ()
  (port-pointer-documentation-string (port sheet)))

(scl:compile-flavor-methods genera-window temporary-genera-window)


(defmacro let-state ((&rest let-clauses) &body body)
  `(multiple-value-bind ,(mapcar #'first let-clauses)
       (without-scheduling
	 (values ,@(mapcar #'second let-clauses)))
     ,@body))

(defun-inline genera-button-number->standard-button-name (code)
  (aref `#(,+pointer-left-button+
	   ,+pointer-middle-button+
	   ,+pointer-right-button+) code))

;;; Take events out of the global queue and distribute them, and get key
;;; press "events" as characters out of the window's io-buffer.
;;;
;;; This method communicates with the dispatch-device-event method below through
;;; distribute-device-event.  It does this by passing in extra keywords, which
;;; are then passed back to dispatch.  I use the event-naming keywords used by
;;; the QUEUE-INPUT method.

;;; --- What should we do with non-:MOUSE-BUTTON blips?  What we
;;; --- currently do is just drop them on the floor...
(defmethod process-next-event ((port genera-port) 
			       &key (timeout nil) (wait-function nil)
				    ((:state whostate) "Genera Event"))
  ;; Motion events are queued by the mouse process (in the :mouse-moves method above)
  ;; but the character input side isn't so well defined.  Rely on the fact that
  ;; only the selected window can be receiving input.
  (let ((genera-window nil)
	(shifts-up nil)
	(shifts-down nil))
    (flet ((await-Genera-event ()
	     (or *mouse-moved*
		 ;; which mouse to pass off to MOUSE-CHORD-SHIFTS?
		 (let ((shifts (tv:mouse-chord-shifts))
		       (old *old-mouse-chord-shifts*))
		   (when (/= old shifts)
		     (multiple-value-setq (shifts-up shifts-down)
		       (buttons-up old shifts))
		     (setq *old-mouse-chord-shifts* shifts)
		     t))
		 (when (and (typep tv:selected-window 'genera-window)
			    (scl:send tv:selected-window :listen))
		   (setq genera-window tv:selected-window)
		   t)
		 (when wait-function
		   (funcall wait-function)))))
      (declare (dynamic-extent #'await-Genera-event))
      (sys:process-wait-with-timeout whostate timeout #'await-Genera-event))
    (cond (*mouse-moved*
	   (let-state ((mouse-moved     (shiftf *mouse-moved* nil))	;capture and reset
		       (mouse-window	*mouse-window*)
		       (mouse-x		*mouse-x*)
		       (mouse-y		*mouse-y*)
		       (old-mouse-x     *old-mouse-x*)
		       (old-mouse-y     *old-mouse-y*)
		       (mouse-buttons	*mouse-buttons*)
		       (mouse-button-released (shiftf *mouse-button-released* nil)))
	     (multiple-value-bind (left top)
		 (if *mouse-window*
		     (genera-window-margins *mouse-window*)
		     (values 0 0))
	       (let ((native-x (- mouse-x left))
		     (native-y (- mouse-y top)))
		 (when (and mouse-moved		;check again
			    (or (not (eq old-mouse-x mouse-x))
				(not (eq old-mouse-y mouse-y))))
		   (let ((sheet (and mouse-window (genera-window-sheet mouse-window))))
		     (when sheet
		       (distribute-event
			 (port sheet)
			 (make-instance 'pointer-motion-event
					:x mouse-x
					:y mouse-y
					:native-x native-x
					:native-y native-y
					:button (and (not (zerop mouse-buttons))
						     (genera-button-number->standard-button-name
						       (ash mouse-buttons -1)))
					:modifiers (current-modifier-state
						     (make-state-from-buttons mouse-buttons)
						     (tv:sheet-mouse mouse-window))
					:pointer (port-pointer port)
					:sheet sheet)))))
		 (when mouse-button-released
		   (let ((sheet (and mouse-window (genera-window-sheet mouse-window))))
		     ;; hmm?
		     (when sheet
		       (distribute-event
			 (port sheet)
			 (make-instance 'pointer-button-release-event
					:x mouse-x
					:y mouse-y
					:native-x native-x
					:native-y native-y
					:button (genera-button-number->standard-button-name
						  (ash mouse-button-released -1))
					:modifiers (current-modifier-state
						     (make-state-from-buttons mouse-buttons)
						     (tv:sheet-mouse mouse-window))
					:pointer (port-pointer port)
					:sheet sheet)))))))))
	  ;; Handle shift press and release events
	  ((or shifts-up shifts-down)
	   (when (typep tv:selected-window 'genera-window)
	     (let* ((genera-window tv:selected-window)
		    (sheet (genera-window-sheet genera-window)))
	       (when sheet
		 (let ((state (current-modifier-state 0 (tv:sheet-mouse genera-window))))
		   #+++ignore
		   (when shifts-up
		     (map-over-genera-shift-keysyms (shift-keysym shifts-up)
		       (distribute *mouse-x* *mouse-y*
				   :event-key ':key-release
				   :keysym shift-keysym
				   :char nil
				   :keyboard-p T
				   :state state)))
		   #+++ignore
		   (when shifts-down
		     (map-over-genera-shift-keysyms (shift-keysym shifts-down)
		       (distribute *mouse-x* *mouse-y*
				   :event-key ':key-press
				   :keysym shift-keysym
				   :char nil
				   :keyboard-p T
				   :state state))))))))
	  ;; Make sure we read from the same window that :LISTEN return T on, even
	  ;; if the selected window state has changed.
	  ((and genera-window			; must be a genera-window
		(scl:send genera-window :listen))
	   (let ((thing (let ((sys:kbd-intercepted-characters
				(remove #\Abort sys:kbd-intercepted-characters)))
			  (scl:send genera-window :any-tyi-no-hang)))
		 (sheet (genera-window-sheet genera-window)))
	     (typecase thing
	       (character
		 (when sheet
		   ;;--- Not snapshotting simultaneous X and Y, but I don't
		   ;; think it matters here.
		   ;; remember that state is always the state before
		   ;; this event was processed.  Since we're not yet distributing
		   ;; the key presses for the shifts, no big deal.
		   (let ((keysym (genera-character->keysym thing))
			 (char thing))
		     (when keysym
		       (distribute-event
			 (port sheet)
			 (make-instance 'key-press-event
					:key-name keysym
					:character char
					:modifiers (current-modifier-state 
						     0 (tv:sheet-mouse genera-window))
					:sheet sheet))
		       (distribute-event
			 (port sheet)
			 (make-instance 'key-release-event
					:key-name keysym
					:character char
					:modifiers (current-modifier-state 
						     0 (tv:sheet-mouse genera-window))
					:sheet sheet))))))
	       ;; See if it is a button-click blip
	       (list
		 (when (eq (first thing) ':mouse-button)
		   ;; (:mouse-button #\mouse-left window x y)
		   (let* ((mouse-x (fourth thing))
			  (mouse-y (fifth thing))
			  (code (tv:char-mouse-button (second thing)))
			  (window (third thing))
			  (button (genera-button-number->standard-button-name code)))
		     (when sheet
		       (multiple-value-bind (left top)
			   (if *mouse-window*
			       (genera-window-margins *mouse-window*)
			       (values 0 0))
			 (let ((native-x (- mouse-x left))
			       (native-y (- mouse-y top)))
			   (distribute-event
			     (port sheet)
			     (make-instance 'pointer-button-press-event
					    :x mouse-x
					    :y mouse-y
					    :native-x native-x
					    :native-y native-y
					    :button button
					    :modifiers (current-modifier-state
							 0 (tv:sheet-mouse window))
					    :pointer (port-pointer port)
					    :sheet sheet))))))))))))))

;; See general comments about sheet-native-native-region* and
;; sheet-native-region* about functions vs macros, etc.
(defun genera-window-margins (mirror)
  (declare (values left top inside-width inside-height))
  (multiple-value-bind (left top)
      (scl:send mirror :margins)
    (multiple-value-bind (width height)
	(scl:send mirror :inside-size)
      (values left top width height))))

(defun make-state-from-buttons (buttons)
  (ash 1 buttons))

(defun current-modifier-state (&optional (state 0) (mouse tv:main-mouse))
  ;; Take only the upper bits of state, compute the lower bits from the current
  ;; shifts.
  (let ((shifts (tv:mouse-chord-shifts mouse))
	(state (logand state #xFF00)))
    (macrolet ((do-shift (shift)
		 `(when (si:bit-test (si:name-bit ,shift) shifts)
		    (let ((bit (clim-internals::modifier-key-index ,shift)))
		      (setf state (dpb 1 (byte 1 bit) state))))))
      ;; why is SHIFT different from sys:%kbd-shifts-shift
      (when (ldb-test (byte 1 4) shifts)
	(let ((bit (clim-internals::modifier-key-index :shift)))
	  (setf state (dpb 1 (byte 1 bit) state))))
      (do-shift :control)
      (do-shift :meta)
      (do-shift :super)
      (do-shift :hyper))
    state))
