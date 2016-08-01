;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 International Lisp Associates."


;;--- This isn't really right, figure out what to do
(defmethod sheet-shell (sheet) sheet)


(defvar *clim-window-count* 0)

(defmethod realize-mirror ((port genera-port) sheet)
  (with-slots (screen height-pixels) port
    (multiple-value-bind (left top right bottom)
	(sheet-native-region* sheet)
      (fix-coordinates left top right bottom)
      (let* ((genera-parent (sheet-mirror sheet))
	     (name (format nil "CLIM window ~D" (incf *clim-window-count*)))
	     (frame (pane-frame sheet))
	     (save-under (and frame (getf (frame-properties frame) :save-under)))
	     (mirror
	       (tv:make-window
		 (if save-under 'temporary-genera-window 'genera-window)
		 ;; probably the screen
		 :superior genera-parent
		 :name name
		 :sheet sheet
		 :save-bits :delayed
		 :deexposed-typeout-action :permit
		 ;;--- :blinker-p t	;we should use Genera blinkers
		 :label nil		;labels are handled by toolkit
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

(defmethod bury-mirror ((port genera-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (scl:send window :bury)))

(defmethod mirror-visible-p ((port genera-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (scl:send mirror :exposed-p)))

(defmethod realize-graft ((port genera-port) graft)
  (with-slots (silica::pixels-per-point silica::pixel-width silica::pixel-height
	       silica::mm-width silica::mm-height silica::units) graft
    (let ((screen (slot-value port 'screen)))
      (setf silica::pixel-width  (scl:send screen :inside-width)
	    silica::pixel-height (scl:send screen :inside-height)
	    ;;--- Bogus numbers
	    silica::mm-width     360.0
	    silica::mm-height	 280.0
	    ;; (/ sage::*microns-per-point* sage::*microns-per-screen-pixel*)
	    silica::pixels-per-point 1.38)
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
	(values (coordinate xoff) (coordinate yoff)
		(coordinate (+ xoff width)) (coordinate (+ yoff height)))))))

;; Returns left,top,right,bottom
(defmethod mirror-region* ((port genera-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (genera-mirror-native-edges* port sheet mirror))))

;; Returns x,y,width,height
(defmethod mirror-inside-region* ((port genera-port) sheet)
  (multiple-value-bind (x y width height)
      (scl:send (sheet-mirror sheet) :inside-edges)
    (values (coordinate x) (coordinate y)
	    (coordinate width) (coordinate height))))

;;--- Shouldn't this be the same as MIRROR-REGION*?
(defmethod mirror-native-edges* ((port genera-port) sheet)
  (let ((mirror (sheet-direct-mirror sheet)))
    (genera-mirror-native-edges* port sheet mirror)))

;; Returns 0,0,width,height
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

#+++ignore
(scl:defmethod (:init genera-window :after) (args)
  (declare (ignore args))
  (let ((blinker (first (scl:send scl:self :blinker-list))))
    (when blinker 
      (scl:send blinker :set-visibility nil))))

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
		(let ((frame (pane-frame sheet)))
		  (when frame
		    ;; This is a frame whose input focus has not yet been directed to
		    ;; any pane.  Choose the interactor pane by default.
		    (let ((stream (frame-standard-input frame)))
		      (when (and stream (windowp stream))
			(scl:lexpr-send (medium-drawable (sheet-medium stream))
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
      (scl:send scl:self :set-char-aluf foreground)
      (scl:send scl:self :set-erase-aluf background))))

(scl:defmethod (:refresh genera-window :after) (&optional (type ':complete-redisplay))
  (unless (eq type ':use-old-bits)
    (unless *port-trigger*
      (let ((*port-trigger* t))
	(repaint-sheet sheet (sheet-region sheet))))))

(scl:defmethod (tv:refresh-rectangle genera-window) (left top right bottom)
  (when (typep sheet 'clim-stream-sheet)
    (setf (clim-internals::stream-highlighted-presentation sheet) nil))
  ;;--- This is surely the wrong rectangle
  (repaint-sheet sheet (make-bounding-rectangle left top right bottom)))

;;; Called on position changes as well as size?
(scl:defmethod (:change-of-size-or-margins genera-window :after) (&rest options)
  (declare (ignore options))
  (let ((*port-trigger* t)
	(port (port sheet)))
    (dispatch-event
      sheet
      (let ((region (mirror-region port sheet)))
	(allocate-event 'window-configuration-event
	  :native-region region
	  :region (untransform-region (sheet-native-transformation sheet) region)
	  :sheet sheet)))))

;;--- Problem: we disable the viewport that corresponds to the Genera
;;--- window, but that leaves the window stream enabled.  Should en/disable
;;--- on the viewport forward to the drawing-surface?
(scl:defmethod (:deexpose genera-window :after) (&rest args)
  (declare (ignore args))
  (unless *port-trigger*
    (let ((*port-trigger* t))
      ;;--- Want to call SHRINK-SHEET, but that doesn't appear to be supported
      #+++ignore 
      (disable-sheet sheet))))

(scl:defmethod (:ensure-blinker-for-cursor genera-window) (cursor)
  (unless blinker-table (setq blinker-table (make-hash-table)))
  (or (gethash cursor blinker-table)
      (let ((blinker (tv:make-blinker scl:self 'tv:rectangular-blinker :follow-p t)))
	(setf (gethash cursor blinker-table) blinker)
	(scl:send blinker :set-visibility nil)
	blinker)))

;; This suffices to make c-m-Abort and c-m-Suspend work on CLIM windows.
(scl:defmethod (:process genera-window) ()
  (let ((frame (pane-frame sheet)))
    (and frame
	 (slot-value frame 'clim-internals::top-level-process))))

;; This makes the wholine, Function A, etc., the proper CLIM process, rather
;; than the CLIM event process.  This is because these guys look at
;; (TV:WHO-LINE-SCREEN-LAST-PROCESS (TV:CONSOLE-WHO-LINE-SCREEN SYS:*CONSOLE*)),
;; which is normally updated by TV:CONSOLE-IO-BUFFER-GET, called by :TYI and
;; related methods.  
(defmacro with-clim-io-buffer-process ((console buffer process) &body body)
  (let ((console-var '#:console)
	(buffer-var  '#:buffer)
	(process-var '#:process))
    `(let ((,console-var ,console)
	   (,buffer-var  ,buffer)
	   (,process-var ,process))
       (without-scheduling
	 (multiple-value-prog1
	   (progn ,@body)
	   (when ,process-var
	     (let ((old-process (tv:io-buffer-last-output-process ,buffer-var)))
	       (let ((update-state-p (not (eq ,process-var old-process))))
		 ;; If new process reading, better update wholine run state
		 (when (and update-state-p
			    (eq ,buffer-var (sys:console-selected-io-buffer ,console-var)))
		   (tv:who-line-run-state-update))
		 (setf (tv:io-buffer-last-output-process ,buffer-var) ,process-var)))))))))


;; Shadow the method we would get from TV:STREAM-MIXIN, and disable all
;; intercepted characters as well.  We don't need to shadow :TYI, :TYI-NO-HANG,
;; :ANY-TYI, etc., because the port event processor only uses :ANY-TYI-NO-HANG.
(scl:defmethod (:any-tyi-no-hang genera-window) ()
  (let ((console (tv:sheet-console scl:self))
	(sys:kbd-intercepted-characters nil))
    (with-clim-io-buffer-process (console tv:io-buffer (scl:send scl:self :process))
      (tv:console-io-buffer-get console tv:io-buffer t))))


;;; Save this state (rather than using (mouse-x mouse)) 'cause the coords
;;; are in window coordinates already.
;;;--- All of these should really be per-console
(defvar *mouse-moved* nil)
(defvar *mouse-x* nil)
(defvar *mouse-y* nil)
(defvar *mouse-buttons* 0)
(defvar *mouse-window* nil)
(defvar *mouse-button-released* nil)

;; So we don't generate spurious motion events.  These get updated when
;; we read *MOUSE-X* and *MOUSE-Y*, not when we write them.
(defvar *old-mouse-x* nil)
(defvar *old-mouse-y* nil)
(defvar *old-mouse-chord-shifts* 0)

(defun-inline buttons-up (old-buttons new-buttons)
  (declare (values buttons-up buttons-down))
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
  `(flet ((map-over-genera-shift-keysyms (,keysym-var)
	    ,@body))
     (declare (dynamic-extent #'map-over-genera-shift-keysyms))
     (invoke-on-genera-shift-keysyms
       ,genera-shift-mask #'map-over-genera-shift-keysyms)))

;; This is the guts of a horrible kludge for CLIM purposes...
(defun kludge-buttons-released-when-mouse-over-no-window (mouse)
  (multiple-value-bind (x y)
      (if #+IMach (sys:system-case 
		    (:macivory t)		;MacIvories are so wierd...
		    (otherwise (tv:mouse-warp-internal mouse)))
	  #-IMach (tv:mouse-warp-internal mouse)
	  (values (tv:mouse-last-x mouse)
		  (tv:mouse-last-y mouse))
	  (values (tv:mouse-x mouse)
		  (tv:mouse-y mouse)))
    (without-scheduling
      (let* ((old-buttons *mouse-buttons*)
	     (new-buttons tv:mouse-last-buttons)
	     (buttons-up (buttons-up old-buttons new-buttons))
	     ;; Merge them all, for now.  This might drop a second up
	     ;; transition before the first one is noticed.  Even if we
	     ;; handled that here (by delaying updating *MOUSE-BUTTONS*,
	     ;; by, LOGIORing old and new) and *MOUSE-BUTTON-RELEASED*,
	     ;; it could require a mouse motion to actually notice the
	     ;; button up, which is gross!
	     (merged-up (logior buttons-up (or *mouse-button-released* 0)))
	     (new-released (and (not (zerop merged-up)) merged-up)))
	(setq *mouse-buttons* new-buttons
	      ;; doesn't yet handle multiple buttons released at once...
	      *mouse-button-released* new-released
	      *mouse-window* (let ((window (tv:window-under-mouse-internal
					     mouse ':mouse-select ':active x y)))
			       (when (typep window 'genera-window) window))
	      *mouse-x* x
	      *mouse-y* y
	      *mouse-moved* ':pointer-motion)))))

;; ...and this is the horrible kludge for CLIM purposes
(scl:advise tv:mouse-set-blinker-cursorpos-internal :after compute-buttons-released nil
  (kludge-buttons-released-when-mouse-over-no-window (first scl:arglist)))
(si:compile-advice 'tv:mouse-set-blinker-cursorpos-internal)

(scl:defmethod (:mouse-moves genera-window :after) (x y)
  (without-scheduling
    (let* ((old-buttons *mouse-buttons*)
	   (new-buttons tv:mouse-last-buttons)
	   (buttons-up (buttons-up old-buttons new-buttons))
	   ;; Merge them all, for now.  This might drop a second up
	   ;; transition before the first one is noticed.  Even if we
	   ;; handled that here (by delaying updating *MOUSE-BUTTONS*,
	   ;; by, LOGIORing old and new) and *MOUSE-BUTTON-RELEASED*,
	   ;; it could require a mouse motion to actually notice the
	   ;; button up, which is gross!
	   (merged-up (logior buttons-up (or *mouse-button-released* 0)))
	   (new-released (and (not (zerop merged-up)) merged-up)))
      (setq *mouse-window* scl:self
	    *mouse-x* x
	    *mouse-y* y
	    *mouse-buttons* new-buttons
	    ;; doesn't yet handle multiple buttons released at once...
	    *mouse-button-released* new-released
	    *mouse-moved* ':pointer-motion))))

(scl:defmethod (:handle-mouse genera-window :before) ()
  (scl:send scl:self :select)			;---why??
  (without-scheduling
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
  (without-scheduling
    (setq *mouse-window* scl:self		;--- Which window is this notification for?
	  *mouse-moved* ':leave-notify
	  *mouse-x* -1
	  *mouse-y* -1)))			;--- ???

(scl:compile-flavor-methods genera-window temporary-genera-window)


(defmacro let-state ((&rest let-clauses) &body body)
  `(multiple-value-bind ,(mapcar #'first let-clauses)
       (without-scheduling
	 (values ,@(mapcar #'second let-clauses)))
     ,@body))

(defun-inline genera-button-number->event-button (code)
  (case code
    (0 +pointer-left-button+)
    (1 +pointer-middle-button+)
    (2 +pointer-right-button+)))

(defun-inline make-state-from-buttons (buttons)
  (ash 1 buttons))

(defun-inline current-modifier-state (&optional (state 0) (mouse tv:main-mouse))
  ;; Take only the upper bits of state, compute the lower bits from the
  ;; current shifts.
  (convert-genera-shift-state state (tv:mouse-chord-shifts mouse)))

;;; Take events out of the global queue and distribute them, and get key
;;; press "events" as characters out of the window's io-buffer.
;;;
;;; This method communicates with the dispatch-device-event method below through
;;; distribute-device-event.  It does this by passing in extra keywords, which
;;; are then passed back to dispatch.  I use the event-naming keywords used by
;;; the QUEUE-INPUT method.

;;;--- What should we do with non-mouse button blips?  What we
;;;--- currently do is just drop them on the floor...
(defmethod process-next-event ((port genera-port) 
			       &key (timeout nil) (wait-function nil)
				    ((:state whostate) "Genera Event"))
  ;; Motion events are queued by the mouse process (in the :mouse-moves method above)
  ;; but the character input side isn't so well defined.  Rely on the fact that
  ;; only the selected window can be receiving input.
  (let ((genera-window nil)
	(shifts-up nil)
	(shifts-down nil))
    (flet ((await-genera-event ()
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
      (declare (dynamic-extent #'await-genera-event))
      (sys:process-wait-with-timeout whostate timeout #'await-genera-event))
    (cond (*mouse-moved*
	   (let-state ((mouse-moved     (shiftf *mouse-moved* nil))	;capture and reset
		       (mouse-window	*mouse-window*)
		       (mouse-x		*mouse-x*)
		       (mouse-y		*mouse-y*)
		       (old-mouse-x     (shiftf *old-mouse-x* *mouse-x*))
		       (old-mouse-y     (shiftf *old-mouse-y* *mouse-y*))
		       (mouse-buttons	*mouse-buttons*)
		       (mouse-button-released (shiftf *mouse-button-released* nil)))
	     (multiple-value-bind (left top)
		 (if mouse-window
		     (genera-window-margins mouse-window)
		     (values 0 0))
	       (let ((native-x (- mouse-x left))
		     (native-y (- mouse-y top)))
		 (when (and mouse-moved		;check again to close timing window
			    (or (not (eq old-mouse-x mouse-x))
				(not (eq old-mouse-y mouse-y))))
		   (let ((sheet (and mouse-window (genera-window-sheet mouse-window)))
			 (pointer (port-pointer port)))
		     ;; There shouldn't really be any need to do this, since
		     ;; pointer button up/down should have already done so.
		     ;; Unfortunately, Genera is prone to button-up events,
		     ;; so it's better to be safe than sorry.
		     (if (zerop mouse-buttons)
			 (setf (pointer-button-state pointer) 0)
			 (setf (pointer-button-state pointer)
			       (genera-button-number->event-button
				 (ash mouse-buttons -1))))
		     (when sheet
		       (distribute-event
			 port			;(EQ PORT (PORT-SHEET)) ==> T
			 (allocate-event 'pointer-motion-event
			   :x mouse-x
			   :y mouse-y
			   :native-x native-x
			   :native-y native-y
			   :modifier-state 
			     (current-modifier-state
			       (make-state-from-buttons mouse-buttons)
			       (tv:sheet-mouse mouse-window))
			   :pointer pointer
			   :sheet sheet)))))
		 (when mouse-button-released
		   (let ((sheet (and mouse-window (genera-window-sheet mouse-window)))
			 (pointer (port-pointer port)))
		     ;; Genera doesn't keep track of more than one button at
		     ;; a time, so set the state to zero
		     (setf (pointer-button-state pointer) 0)
		     (when sheet
		       (distribute-event
			 port
			 (allocate-event 'pointer-button-release-event
			   :x mouse-x
			   :y mouse-y
			   :native-x native-x
			   :native-y native-y
			   :button 
			     (genera-button-number->event-button
			       (ash mouse-button-released -1))
			   :modifier-state
			     (current-modifier-state
			       (make-state-from-buttons mouse-buttons)
			       (tv:sheet-mouse mouse-window))
			   :pointer pointer
			   :sheet sheet)))))))))
	  ;; Handle shift press and release events
	  ((or shifts-up shifts-down)
	   (when (typep tv:selected-window 'genera-window)
	     (let* ((genera-window tv:selected-window)
		    (sheet (genera-window-sheet genera-window)))
	       (when sheet
		 (let ((state (current-modifier-state 0 (tv:sheet-mouse genera-window))))
		   (when shifts-up
		     (map-over-genera-shift-keysyms (shift-keysym shifts-up)
		       (distribute-event
			 port
			 (allocate-event 'key-release-event
			   :key-name shift-keysym
			   :character nil
			   :modifier-state state
			   :sheet sheet))))
		   (when shifts-down
		     (map-over-genera-shift-keysyms (shift-keysym shifts-down)
		       (distribute-event
			 port
			 (allocate-event 'key-press-event
			   :key-name shift-keysym
			   :character nil
			   :modifier-state state
			   :sheet sheet)))))))))
	  ;; Make sure we read from the same window that :LISTEN return T on,
	  ;; even if the selected window state has changed.
	  ((and genera-window			;must be a genera-window
		(scl:send genera-window :listen))
	   (let ((thing (let ((sys:kbd-intercepted-characters
				(remove #\Abort sys:kbd-intercepted-characters)))
			  (scl:send genera-window :any-tyi-no-hang)))
		 (sheet (genera-window-sheet genera-window)))
	     (typecase thing
	       (character
		 (when sheet
		   ;; Remember that state is always the state before
		   ;; this event was processed.  Since we're not yet distributing
		   ;; the key presses for the shifts, no big deal.
		   (let ((keysym (genera-character->keysym thing))
			 (char thing))
		     (when keysym
		       (let ((state (current-modifier-state
				      0 (tv:sheet-mouse genera-window))))
			 (distribute-event
			   port
			   (allocate-event 'key-press-event
			     :key-name keysym
			     :character char
			     :modifier-state state
			     :sheet sheet))
			 (distribute-event
			   port
			   (allocate-event 'key-release-event
			     :key-name keysym
			     :character char
			     :modifier-state state
			     :sheet sheet)))))))
	       ;; See if it's a button-click blip
	       (list
		 (when (eq (first thing) ':mouse-button)
		   ;; (:mouse-button #\mouse-left window x y)
		   (let* ((window (third thing))
			  (mouse-x (fourth thing))
			  (mouse-y (fifth thing))
			  (code (tv:char-mouse-button (second thing)))
			  (button (genera-button-number->event-button code))
			  (modifiers
			    (convert-genera-shift-state
			      (make-state-from-buttons (ash 1 code))
			      (tv:char-mouse-bits (second thing))))
			  (pointer (port-pointer port)))
		     (declare (ignore window))
		     (when sheet
		       (multiple-value-bind (left top)
			   (if *mouse-window*
			       (genera-window-margins *mouse-window*)
			       (values 0 0))
			 (let ((native-x (- mouse-x left))
			       (native-y (- mouse-y top)))
			   (distribute-event
			     port
			     (allocate-event 'pointer-button-press-event
			       :x mouse-x
			       :y mouse-y
			       :native-x native-x
			       :native-y native-y
			       :button button
			       :modifier-state modifiers
			       :pointer pointer
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

(defun convert-genera-shift-state (button-state shift-state)
  (let ((state (logand button-state #xFF00)))
    (macrolet ((do-shift (shift)
		 `(when (si:bit-test (si:name-bit ,shift) shift-state)
		    (let ((bit (modifier-key-index ,shift)))
		      (setf state (dpb 1 (byte 1 bit) state))))))
      ;; Why is SHIFT different from sys:%kbd-shifts-shift?
      (when (ldb-test (byte 1 4) shift-state)
	(let ((bit (modifier-key-index :shift)))
	  (setf state (dpb 1 (byte 1 bit) state))))
      (do-shift :control)
      (do-shift :meta)
      (do-shift :super)
      (do-shift :hyper))
    state))
