;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: old-genera-implementation.lisp,v 1.1 92/02/24 13:28:09 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Will these SAGE variables be present in all Genera worlds?
;;; I don't want to have to figure out how many microns/pixel there are
;;; for each screen type.
(defmethod implementation-pixels-per-point ((stream sheet-implementation-mixin))
  (/ sage::*microns-per-point* sage:*microns-per-screen-pixel*))

(defmethod window-erase-viewport ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (when (window-drawing-possible stream)
      (scl:send window :clear-window))))

(defmethod window-clear-area ((stream sheet-implementation-mixin) left top right bottom)
  ;; In sheet window coordinates, basically
  (with-slots (window) stream
    (when (window-drawing-possible stream)
      (scl:send window :draw-rectangle (- right left) (- bottom top) left top
		(scl:send window :erase-aluf)))))

(defmethod window-visibility ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (scl:send window :exposed-p)))

(defmethod (setf window-visibility) (visibility (stream sheet-implementation-mixin))
  (with-slots (window) stream
    ;; visibility is a boolean
    (if visibility
	;; this isn't quite right, because activating it means it's now buried??
	(scl:send window :expose)
	(scl:send window :deactivate))
    visibility))

(defmethod wait-for-window-exposed ((window sheet-implementation-mixin))
  (unless (window-visibility window)
    (scl:process-wait "Await exposure" #'window-visibility window)))

(defmethod close ((stream sheet-implementation-mixin) &key abort)
  (declare (ignore abort))
  (with-slots (window) stream
    (if (eq window (scl:send window :screen)) ; if this is a screen.
	(mapc #'close (window-children stream))
	(scl:send window :kill))))

(defmethod bounding-rectangle-set-edges :after
	   ((stream sheet-implementation-mixin) left top right bottom)
  (with-slots (window) stream
    (let ((superior (tv:sheet-superior window)))
      (when t ; (null (tv:sheet-superior superior))	;root-p
	(multiple-value-bind (wl wt wr wb)
	    (scl:send superior :inside-edges)
	  (declare (ignore wr wb))
	  (translate-positions wl wt left top right bottom))))
    (scl:send window :set-edges left top right bottom)))

(defmethod bounding-rectangle-set-position* :after
	   ((stream sheet-implementation-mixin) new-left new-top)
  (with-slots (window) stream
    (let ((superior (tv:sheet-superior window)))
      (when t ; (null (tv:sheet-superior superior))	;root-p
	(multiple-value-bind (wl wt)
	    (scl:send superior :inside-edges)
	  (translate-positions wl wt new-left new-top))))
    (scl:send window :set-position new-left new-top)))

(defmethod bounding-rectangle-set-size :after
	   ((stream sheet-implementation-mixin) width height)
  (with-slots (window) stream
    (multiple-value-bind (wl wt wr wb)
	(scl:send window :inside-edges)
      (setq wr (+ wl width)
	    wb (+ wt height))
      (scl:send window :set-edges wl wt wr wb))))

(defmethod stream-clear-output ((stream sheet-implementation-mixin))
  )

(defmethod window-refresh ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (scl:send window :refresh)))

(defun clim-label->genera-label (label-spec stream)
  (when label-spec
    (let (label label-text-style character-style)
      (setq label (cond ((stringp label-spec) label-spec)
			(t (first label-spec))))

      (setq label-text-style
	    (cond ((stringp label-spec)
		   (stream-merged-text-style stream))
		  ((getf (rest label-spec) :text-style))
		  (t (stream-merged-text-style stream))))

      (setq character-style
	    (si:intern-character-style :device-font
				       (tv:font-name 
					 (text-style-mapping
					   (slot-value stream 'display-device-type)
					   *standard-character-set*
					   label-text-style
					   (slot-value stream 'window)))
				       :normal))
      `(:string ,label :character-style ,character-style))))

;;; A label is either a string or a list of the form (string :text-style style).
(defmethod (setf window-label) :after (new-label (stream sheet-implementation-mixin))
  (with-slots (window) stream
    (multiple-value-bind (old-width old-height)
	(scl:send window :inside-size)
      (let ((genera-label (clim-label->genera-label new-label stream)))
	(scl:send window :set-label genera-label))
      (ignore-errors
	(scl:send window :set-inside-size old-width old-height)))))

(defmethod window-label-size ((stream sheet-implementation-mixin)
			      &optional (label (window-label stream)))
  (cond ((null label)
	 (values 0 0))
	(t (dw::margin-label-size (clim-label->genera-label label stream)
				  (slot-value stream 'window)))))

(defmethod window-margins ((stream sheet-implementation-mixin))
  stream
  (values 0 0 0 0))

(defmethod host-window-margins ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (scl:send window :margins)))


(defmethod notify-user-1 ((stream sheet-implementation-mixin)
			  frame format-string &rest format-args)
  (declare (ignore frame) (dynamic-extent format-args))
  (apply #'tv:notify (slot-value stream 'window) format-string format-args))


;;; Interface to Genera scroll bars.
(scl:defmethod (:y-scroll-position clim-sheet) ()
  (declare (values top-displayed height-displayed minimum-y maximum-y))
  (let* ((stream (clim-window-for-host-window scl:self :error-if-no-match nil))
	 (history (and stream (stream-output-history stream)))
	 (viewport (and stream (window-viewport stream))))
    (cond ((and viewport history)
	   (with-bounding-rectangle* (vleft vtop vright vbottom) viewport
	     (declare (ignore vleft vright))
	     (with-bounding-rectangle* (hleft htop hright hbottom) history
	       (declare (ignore hleft hright))
	       (values vtop (- vbottom vtop) htop hbottom))))
	  (t (values 0 0 0 0)))))

(scl:defmethod (:x-scroll-position clim-sheet) ()
  (let* ((stream (clim-window-for-host-window scl:self :error-if-no-match nil))
	 (history (and stream (stream-output-history stream)))
	 (viewport (and stream (window-viewport stream))))
    (cond ((and viewport history)
	   (with-bounding-rectangle* (vleft vtop vright vbottom) viewport
	     (declare (ignore vtop vbottom))
	     (with-bounding-rectangle* (hleft htop hright hbottom) history
	       (declare (ignore htop hbottom))
	       (values vleft (- vright vleft) hleft hright))))
	  (t (values 0 0 0 0)))))

(defun calculate-new-viewport-position (stream x-or-y type position
					&optional (context-nlines 1))
  (declare (ignore context-nlines))
  (declare (values viewport-left viewport-top))
  (let ((viewport (window-viewport stream)))
    (ecase x-or-y
      (:y
	(ecase type
	  (:relative-jump
	    (values (bounding-rectangle-left viewport)
		    (+ (bounding-rectangle-top viewport)
		       (* position (stream-line-height stream))))
	    #+Ignore
	    (unless (zerop position)
	      (let ((index viewport-displayed-strings-start-index)
		    (max-index (fill-pointer displayed-strings))
		    (y (box-top cursor-viewport))
		    (inc (signum position)))
		(labels ((index-ok-p (index)
			   (and ( index 0)
				(< index max-index))))
		  (loop repeat (abs position)
			do (loop doing
			     (let ((new-index (+ index inc)))
			       (unless (index-ok-p new-index)
				 (loop-finish))
			       (setq index new-index))
			     (let ((new-top (box-top (presentation-displayed-box
						       (aref displayed-strings index)))))
			       (when ( new-top y)
				 (setq y new-top)
				 (loop-finish))))
			while (index-ok-p index))
		  (when (index-ok-p index)
		    (let ((new-top (box-top (presentation-displayed-box
					      (aref displayed-strings index)))))
		      (values (box-left cursor-viewport) new-top)))))))
	  (:relative 
	    (values (bounding-rectangle-left viewport)
		    (+ position (bounding-rectangle-top viewport))))
	  (:absolute
	    (values (bounding-rectangle-left viewport)
		    position))
	  (:screenful
	    (with-bounding-rectangle* (vleft vtop vright vbottom) viewport
	      (declare (ignore vright))
	      (let ((new-top (+ vtop (* position (- vbottom vtop)))))
		(setq new-top (min new-top (- vbottom vtop)))
		(values vleft new-top))))
	  ))
      (:x
	(ecase type
	  (:relative-jump
	    (calculate-new-viewport-position
	      stream :x :relative
	      (* position (stream-character-width stream #\Space))))
	  (:relative
	    (values (+ (bounding-rectangle-left viewport) position)
		    (bounding-rectangle-top viewport)))
	  (:absolute
	    (values position
		    (bounding-rectangle-top viewport))))))))

(scl:defmethod (:y-scroll-to clim-sheet) (position type)
  (let* ((stream (clim-window-for-host-window scl:self))
	 (history (stream-output-history stream))
	 (left (bounding-rectangle-left (window-viewport stream))))
    (multiple-value-bind (ignore top)
	(calculate-new-viewport-position stream :y type position)
      (declare (ignore ignore))
      (when history
	(with-bounding-rectangle* (hleft htop hright hbottom) history
	  (declare (ignore hleft hright))
	  (setq top (min (max htop top) hbottom))))
      (window-set-viewport-position* stream left top))))

(scl:defmethod (:x-scroll-to clim-sheet) (position type)
  (let* ((stream (clim-window-for-host-window scl:self))
	 (history (stream-output-history stream))
	 (top (bounding-rectangle-top (window-viewport stream))))
    (let ((left (calculate-new-viewport-position stream :x type position)))
      (when history
	(with-bounding-rectangle* (hleft htop hright hbottom) history
	  (declare (ignore htop hbottom))
	  (setq left (min (max left hleft) hright))))
      (window-set-viewport-position* stream left top))))

(scl:defmethod (tv:refresh-rectangle clim-sheet) (left top right bottom)
  (let ((window (clim-window-for-host-window scl:self)))
    (setf (slot-value window 'highlighted-presentation) nil)
    (multiple-value-bind (viewport-x viewport-y)
	(bounding-rectangle-position* (window-viewport window))
      (frame-replay *application-frame*
		    window (make-bounding-rectangle (+ left viewport-x)
						    (+ top viewport-y)
						    (+ right viewport-x)
						    (+ bottom viewport-y))))
    (tv:refresh-margins-rectangle scl:self left top right bottom)))

;;; This is a hideous kludge that attempts to address the problems with moving windows
;;; within their parents.
(scl:defwhopper (:set-position clim-sheet) (new-x new-y &optional verify)
  (ecase verify
    ((:verify)
     (scl:continue-whopper new-x new-y verify))
    ((nil)
     (let ((old-x tv:x-offset) (old-y tv:y-offset))
       (multiple-value-prog1 (scl:continue-whopper new-x new-y verify)
	 (unless (or *synchronous-window-operation-being-processed*
		     (and (= old-x tv:x-offset) (= old-y tv:y-offset)))
	   (scl:send scl:self :force-kbd-input
		     (make-window-size-or-position-change-event
		       (clim-window-for-host-window scl:self)
		       nil nil nil nil))))))))

(scl:defmethod (:convert-mouse-coords clim-sheet) (x y in-or-out)
  (case in-or-out
    (:in (values (- x tv:left-margin-size)
		 (- y tv:top-margin-size)))
    (:out (values (+ x tv:left-margin-size)
		  (+ y tv:top-margin-size)))))

(scl:defmethod (with-clim-sheet-clipping-region clim-sheet)
	       (left top right bottom continuation stream)
  (tv:with-sheet-clipping-region ((+ left tv:left-margin-size) (+ top tv:top-margin-size)
				  (+ right tv:left-margin-size) (+ bottom tv:top-margin-size))
    (declare (sys:downward-function))		;yecch
    (funcall continuation stream)))

(scl:compile-flavor-methods clim-sheet temp-clim-sheet)


;;; The margin stuff seems to have changed.
(defmethod initialize-instance :around ((stream sheet-window-stream) &key label)
  (call-next-method)
  (when (slot-value stream 'parent)
    (scl:send (slot-value stream 'window) :set-label " ")
    (scl:send (slot-value stream 'window)
	      :set-label (clim-label->genera-label label stream))))

(defun clim-sheet-margin-components (label-p scroll-bar borders-p)
  `(,@(and borders-p
	   '((dw::margin-borders )))
    ,@(and (member scroll-bar '(:vertical :both))
	   '((dw::margin-scroll-bar :history-noun "history")))
    ,@(and (member scroll-bar '(:horizontal :both))
	   '((dw::margin-scroll-bar :margin :bottom :history-noun "history")))
    ,@(and borders-p
	   '((dw::margin-white-borders :thickness 2)))
    ,@(and label-p
	   '((dw::margin-label )))))

;;; Since sheets have no concept of a "region", we'll just use a Y rectangle
;;; to represent one.

;;--- This should clip to real regions, not just to bounding-rectangles
(defmethod invoke-with-clipping-region
	   ((stream sheet-implementation-mixin) continuation
	    (region standard-bounding-rectangle))
  (let ((window (slot-value stream 'window)))
    (multiple-value-bind (vx vy) (window-viewport-position* stream)
      (declare (type coordinate vx vy))
      (multiple-value-bind (ml mt) (window-margins stream)
	(declare (type coordinate ml mt))
	(with-bounding-rectangle* (left top right bottom) region
	  ;;--- where should this FIXing be done?
	  (fix-points left top right bottom vx vy ml mt)
	  (with-clim-sheet-clipping-region window
					   (+ (- left vx) ml) (+ (- top vy) mt)
					   (+ (- right vx) ml) (+ (- bottom vy) mt)
					   continuation stream))))))


;;; Input side

(defmethod stream-clear-input :after ((stream sheet-window-stream))
  (with-slots (window) stream
    (scl:send window :clear-input)))

(defmethod window-modifier-state ((stream sheet-implementation-mixin))
  (let* ((window (slot-value stream 'window))
	 (mouse (tv:sheet-mouse window)))
    (let ((shifts (tv:mouse-chord-shifts mouse)))
      (mouse-char-bits->modifier-state shifts))))

(defmethod set-pointer-window-and-location ((stream sheet-window-stream) pointer)
  (let ((mouse (tv:sheet-mouse (slot-value stream 'window))))
    (setf (pointer-window pointer)
	  (clim-window-for-host-window (tv:window-under-mouse-internal mouse)
				       :error-if-no-match nil))
    (pointer-set-position* pointer (tv:mouse-x mouse) (tv:mouse-y mouse))))

(defmethod stream-event-handler ((stream sheet-window-stream)
				 &key (timeout nil) (input-wait-test nil))
  ;;--- Have to establish correspondence between pointing devices and pointer objects
  (with-slots (window) stream
    (let* ((sys:rubout-handler nil)
	   (mouse (tv:sheet-mouse window))
	   (pointer (stream-primary-pointer stream))
	   (old-buttons (tv:mouse-last-buttons mouse))
	   (old-x (pointer-x-position pointer))
	   (old-y (pointer-y-position pointer))
	   ;;--- Why doesn't the pointer object remember these?
	   (old-shifts (tv:mouse-chord-shifts mouse))
	   (old-window (pointer-window pointer))
	   (what-happened (if timeout :timeout :mouse-motion))
	   ;; If we're inside an encapsulating stream, such as the input editor, and a window
	   ;; resizing event is received, don't get confused while handling the event
	   (*original-stream* nil))
      (labels ((mouse-motion-pending ()
		 (or (/= (tv:mouse-chord-shifts mouse) old-shifts)
		     (/= old-buttons (tv:mouse-last-buttons mouse))
		     (/= old-x (mouse-x))
		     (/= old-y (mouse-y))
		     (not (eql old-window (clim-window-for-host-window
					    (tv:window-under-mouse-internal mouse)
					    :error-if-no-match nil)))))
	       (something-pending (sheet)
		 (cond ((not (tv:sheet-console window)) nil)	;wait for console
		       ((not (tv:mouse-sheet mouse)) nil)	;..
		       ((and input-wait-test (funcall input-wait-test stream))
			(setq what-happened :input-wait-test))
		       ((mouse-motion-pending)
			(setq what-happened :mouse-motion)
			T)
		       ((scl:send-if-handles sheet :listen)
			(setq what-happened :listen)
			T)))
	       (mouse-x () (tv:mouse-x mouse))
	       (mouse-y () (tv:mouse-y mouse)))
	(declare (dynamic-extent #'mouse-motion-pending #'something-pending
				 #'mouse-x #'mouse-y))
	(cond ((eql timeout 0)			;avoid consing bignums
	       (something-pending window))
	      (timeout
	       (scl:process-wait-with-timeout si:*whostate-awaiting-user-input* (* timeout 60)
		 #'something-pending window))
	      (t
	       (scl:process-wait si:*whostate-awaiting-user-input*
		 #'something-pending window)))
	(ecase what-happened
	  (:listen
	    (let ((character (scl:send window :any-tyi)))
	      (cond ((typep character 'window-size-or-position-change-event)
		     (let* ((window (event-window character))
			    (window-win (slot-value window 'window))
			    (parent (window-parent window))
			    (parent-win (slot-value parent 'window)))
		       (multiple-value-bind (left top right bottom)
			   (scl:send window-win :edges)
			 (when (null (tv:sheet-superior parent-win))
			   (multiple-value-bind (wl wt)
			       (scl:send parent-win :inside-edges)
			     (translate-positions (- wl) (- wt) left top right bottom)))
			 (window-note-size-or-position-change
			   window left top right bottom))))
		    ((listp character)
		     (case (first character)
		       (:mouse-button
			 (scl:destructuring-bind (blip-type char mouse-window x y) character
			   (declare (ignore blip-type))
			   (setf (pointer-button-state pointer)
				 (tv:mouse-last-buttons mouse))
			   ;; we don't need to translate x and y by the root-offsets
			   ;; because they're already in window coordinates...
			   (let ((pw (clim-window-for-host-window mouse-window)))
			     (setf (pointer-window pointer) pw)
			     (multiple-value-bind (xm ym) (scl:send mouse-window :margins)
			       ;; X and Y are presumed to be fixnums here
			       (stream-note-pointer-button-press
				 pw pointer (ash 1 (si:mouse-char-button char))
				 (mouse-char-bits->modifier-state (si:mouse-char-bits char))
				 (- x xm) (- y ym))))))))
		    (t (queue-put (stream-input-buffer stream) character)))))
	  (:mouse-motion
	    ;; ---
	    (pointer-set-position* pointer (mouse-x) (mouse-y))
	    (let ((window (clim-window-for-host-window (tv:window-under-mouse-internal mouse)
						       :error-if-no-match nil)))
	      (setf (pointer-window pointer) window)
	      (when window
		(setf (pointer-motion-pending window pointer) T))))
	  (:input-wait-test  :input-wait-test)
	  (:timeout  :timeout))
	what-happened))))

(defmethod set-stream-pointer-in-screen-coordinates ((stream sheet-window-stream) pointer x y)
  (declare (ignore pointer))			;Sigh.
  (let ((mouse (tv:sheet-mouse (slot-value stream 'window))))
    (tv:mouse-warp x y mouse)))

(defmethod mouse-documentation-window ((window sheet-implementation-mixin))
  (let ((console (tv:sheet-console (slot-value window 'window))))
    (if (eq console sys:*main-console*)
	tv:who-line-documentation-window
	(let ((who-screen (tv:console-who-line-screen console)))
	  (and who-screen
	       (tv:get-who-line-field :mouse-documentation who-screen))))))


;;; Text Cursors for sheets

(defclass sheet-text-cursor
	  (cursor)
    ((x :initarg :x :initform 0)		;needed if cursor is scrolled off viewport
     (y :initarg :y :initform 0)
     (stream :initarg :stream)
     (active :initform nil)			;false if visibility is :inactive
     (blinker :initform nil)))			;Genera window system blinker to be used

(defmethod cursor-active-p ((cursor sheet-text-cursor))
  (not (null (slot-value cursor 'active))))

(defmethod (setf cursor-stream) (new-value (cursor sheet-text-cursor))
  (setf (slot-value cursor 'stream) new-value)
  (let ((blinker (slot-value cursor 'blinker)))
    (when blinker
      (tv:blinker-set-sheet blinker (slot-value new-value 'window))))
  new-value)

#-Silica
(defmethod cursor-position* ((cursor sheet-text-cursor))
  (values (slot-value cursor 'x) (slot-value cursor 'y)))

#-Silica
(defmethod cursor-set-position* ((cursor sheet-text-cursor) x y)
  (setf (slot-value cursor 'x) x
	(slot-value cursor 'y) y)
  (let ((blinker (slot-value cursor 'blinker)))
    (when blinker
      (multiple-value-bind (x y)
	  (drawing-surface-to-viewport-coordinates (slot-value cursor 'stream) x y)
	(tv:blinker-set-cursorpos blinker x y)))))

(defmethod cursor-visibility ((cursor sheet-text-cursor))
  (let ((active (slot-value cursor 'active))
	(blinker (slot-value cursor 'blinker)))
    (cond ((not active) :inactive)
	  ((not blinker) :off)
	  ((member (tv:blinker-visibility blinker) '(:blink :on t)) :on)
	  (t :off))))

(defmethod (setf cursor-visibility) (new-visibility (cursor sheet-text-cursor))
  (setf (slot-value cursor 'active) (not (eq new-visibility :inactive)))
  (let ((stream (slot-value cursor 'stream))
	(blinker (slot-value cursor 'blinker)))
    (when (and (null blinker) (eq new-visibility :on))
      ;; It isn't safe to create the blinker any earlier than this, because of the
      ;; order of initializations while creating a sheet-window-stream
      (setq blinker 
	    (setf (slot-value cursor 'blinker)
		  (flavor:make-instance 'tv:rectangular-blinker
					:sheet (slot-value stream 'window)
					:visibility nil
					:deselected-visibility nil
					:follow-p nil)))
      (multiple-value-bind (x y)
	  (drawing-surface-to-viewport-coordinates stream (slot-value cursor 'x)
							  (slot-value cursor 'y))
	(tv:blinker-set-cursorpos blinker x y)))
    (when blinker
      (tv:blinker-set-visibility blinker (if (eq new-visibility :on)
					     (if (tv:sheet-selected-p
						   (slot-value stream 'window))
						 :blink
						 :on)
					     nil))
      (scl:send blinker :set-deselected-visibility (if (eq new-visibility :on) :on nil))))
  new-visibility)

;; The window system draws it, so we don't have to
(defmethod draw-cursor ((cursor sheet-text-cursor) on-p)
  on-p)
