;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: accept-values.lisp,v 1.48 92/12/03 10:25:53 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; For historical reasons "AVV" means "accepting-values"...

(defclass accept-values-stream
	  (standard-encapsulating-stream)
    ((avv-record :initform nil)
     (avv-frame :initform nil)
     (align-prompts :initform nil :initarg :align-prompts)))

;; Don't allow old AVV queries to be sensitive
(defvar *accept-values-tick* 0)
(defvar *current-accept-values-tick* 0)

(defclass accept-values-output-record
	  (standard-sequence-output-record)
    ((query-table :initarg :query-table)
     (tick :initform *current-accept-values-tick*)
     (resynchronize :initform nil))
  (:default-initargs :query-table (make-hash-table :test #'equal)))

(defmethod find-query ((record accept-values-output-record) query-identifier)
  (declare (values query found-p))
  (gethash query-identifier (slot-value record 'query-table)))

;; We are in control of the AVV code, and we know that there is only 1 AVV
;; under it's parent UPDATING-OUTPUT record.  Therefore we make it match.
;; (There's a separate issue, having nothing to do with incremental-redisplay
;; of making nested, hierarchical AVV's work.)
(defmethod match-output-records ((record accept-values-output-record) &rest initargs)
  (declare (ignore initargs))
  t)

(defclass accept-values-query ()
     ((query-identifier :initarg :query-identifier)
      (presentation-type :initarg :presentation-type 
			 :reader accept-values-query-type)
      (value :initarg :value
	     :accessor accept-values-query-value)
      (presentation :initarg :presentation
		    :accessor accept-values-query-presentation)
      (changed-p :initform nil
		 :accessor accept-values-query-changed-p)
      (active-p :initform t
		:accessor accept-values-query-active-p)
      (error-p :initform nil
	       :accessor accept-values-query-error-p))
  (:default-initargs :presentation nil))

(defresource accept-values-stream (stream &key align-prompts)
  :constructor (make-instance 'accept-values-stream :stream stream)
  :matcher 't					;any will suffice
  :initializer
    (progn
      (setf (slot-value accept-values-stream 'stream) stream)
      (setf (slot-value accept-values-stream 'align-prompts) align-prompts)))

(defun find-accept-values-record (output-record)
  (do ((record output-record (output-record-parent record)))
      ((null record) nil)
    (when (typep record 'accept-values-output-record)
      (return record))))

(defmethod prompt-for-accept :around ((stream accept-values-stream) type (view view)
				      &rest accept-args
				      &key query-identifier (prompt t) (display-default nil)
					   (active-p t)
				      &allow-other-keys)
  (declare (dynamic-extent accept-args))
  ;;--- When ACTIVE-P is NIL, this should do some sort of "graying out"
  (declare (ignore active-p))
  ;; ACCEPT within ACCEPTING-VALUES has to have a query-identifier, so default
  ;; it here and return it.  It's better to cons this list than to call format
  ;; to intern something.
  (unless query-identifier
    (setq query-identifier (list ':query-identifier prompt type)))
  (when (or prompt display-default)
    (updating-output (stream :unique-id (list :prompt query-identifier)
			     :id-test #'equal
			     :cache-value prompt)
      (with-keywords-removed (prompt-args accept-args '(:display-default :query-identifier))
	;; Explicitly pass DISPLAY-DEFAULT so that if it is not supplied by the
	;; user, we forcibly default it to NIL, which gives better-looking AVVs.
	(apply #'call-next-method stream type view
	       			  :display-default display-default
				  :query-identifier query-identifier
				  prompt-args))))
  query-identifier)

(defmethod prompt-for-accept ((stream accept-values-stream) type view
			      &rest accept-args 
			      &key query-identifier &allow-other-keys)
  (declare (ignore accept-args))
  ;; This does nothing, the gadget ACCEPT methods should provide a label
  (unless (gadget-includes-prompt-p type stream view)
    (call-next-method))
  query-identifier)

(defmethod stream-accept ((stream accept-values-stream) type
			  &rest accept-args
			  &key (prompt t) (default nil default-supplied-p)
			       (view (stream-default-view stream)) (active-p t)
			  &allow-other-keys)
  (declare (dynamic-extent accept-args))
  ;;--- When ACTIVE-P is NIL, this should do some sort of "graying out"
  (let ((align-prompts (slot-value stream 'align-prompts))
	query-identifier query)
    (cond (align-prompts
	   ;; The user has asked to line up the labels, so oblige him
	   (formatting-row (stream)
	     (formatting-cell (stream :align-x align-prompts)
	       (setq query-identifier 
		     (apply #'prompt-for-accept
			    (encapsulated-stream stream) type view accept-args)))
	     (formatting-cell (stream :align-x :left)
	       (setq query (find-or-add-query stream query-identifier type prompt
					      default default-supplied-p view active-p)))))
	  (t
	   (setq query-identifier
		 (apply #'prompt-for-accept
			(encapsulated-stream stream) type view accept-args))
	   (setq query (find-or-add-query stream query-identifier type prompt
					  default default-supplied-p view active-p))))
    (values (accept-values-query-value query)
	    (accept-values-query-type query)
	    ;; Set this to NIL so that it appears that nothing has
	    ;; changed at the start of the next pass.
	    (prog1 (accept-values-query-changed-p query)
		   (setf (accept-values-query-changed-p query) nil)))))

;; Probably should be keyword arguments...
(defmethod find-or-add-query ((stream accept-values-stream) query-identifier ptype prompt
			      default default-supplied-p view active-p)
  (let ((avv-record (slot-value stream 'avv-record)))
    (with-slots (query-table) avv-record
      (multiple-value-bind (query found-p)
	  (gethash query-identifier query-table)
	(unless query
	  (setq query (make-instance 'accept-values-query 
			:presentation-type ptype
			:value default
			:query-identifier query-identifier)))
	(setf (accept-values-query-active-p query) active-p)
	;;--- Really wants to reuse existing presentation if found and
	;;--- make sure that that presentation gets re-parented if necessary
	;;--- (suppose inside FORMATTING TABLE...)
	(setf (accept-values-query-presentation query)
	      (updating-output
		  (stream :unique-id query-identifier
			  :id-test #'equal
			  :cache-value (cons active-p
					     (if (accept-values-query-changed-p query)
						 (accept-values-query-value query)
						 default))
			  :cache-test #'(lambda (x y)
					  (unless (accept-values-query-changed-p query)
					    (equal x y))))
		;;--- Calling ACCEPT-1 directly bypasses default preprocessing,
		;;--- is that OK?
		(cond ((or default-supplied-p (accept-values-query-changed-p query))
		       (accept-1 stream ptype
				 :view view :prompt prompt
				 ;; If this field changed, use the value it changed to,
				 ;; making sure that the query object gets changed, too.
				 ;; We need to do this because the body of the dialog
				 ;; can change values without changing the query itself.
				 :default (if (accept-values-query-changed-p query)
					      (accept-values-query-value query)
					      (setf (accept-values-query-value query) default))
				 :history ptype
				 :present-p `(accept-values-choice ,query)
				 :query-identifier query
				 :active-p active-p))
		      ((and found-p
			    (presentation-typep (accept-values-query-value query) ptype))
		       ;; The programmer supplied no default, but a previous edit
		       ;; has put something in the query that we now must use as
		       ;; the default.
		       (accept-1 stream ptype
				 :view view :prompt prompt
				 :default (accept-values-query-value query)
				 :history ptype
				 :present-p `(accept-values-choice ,query)
				 :query-identifier query
				 :active-p active-p))
		      (t
		       ;; No default supplied, field has never been edited.
		       (accept-1 stream ptype
				 :view view :prompt prompt
				 :history ptype :provide-default nil
				 :present-p `(accept-values-choice ,query)
				 :query-identifier query
				 :active-p active-p)))))
	;; really wants to move the cursor position to some reasonable
	;; place in case we're just reusing an existing presentation
	(unless found-p
	  (setf (gethash query-identifier query-table) query))
	query))))

;;--- It seems a little bizarre having a prompt argument, but we can't
;;--- think of a better way.
(defmethod stream-present ((stream accept-values-stream) object presentation-type 
			   &rest present-args
			   &key view (prompt nil prompt-p) (query-identifier nil)
			   &allow-other-keys)
  (declare (dynamic-extent present-args))
  (declare (ignore prompt))
  (unless prompt-p
    ;; This is the normal, non-gadget case of doing PRESENT
    (return-from stream-present (call-next-method)))
  (flet ((do-present (stream)
	   (updating-output (stream :unique-id query-identifier
				    :id-test #'equal
				    :cache-value object
				    :cache-test #'equal)
	     stream
	     ;;--- We did this because (SEQUENCE INTEGER) reinvoked
	     ;;--- PRESENT with the prompt for each of components of
	     ;;--- the stream, which ended up back here creating badly
	     ;;--- identified updating-output records.
	     (apply #'stream-present (encapsulating-stream-stream stream)
		    object presentation-type present-args))))
    (declare (dynamic-extent #'do-present))
    (let ((align-prompts (slot-value stream 'align-prompts)))
      (cond (align-prompts
	     ;; The user has asked to line up the labels, so oblige him
	     (formatting-row (stream)
	       (formatting-cell (stream :align-x align-prompts)
		 (setq query-identifier 
		       (apply #'prompt-for-accept (encapsulated-stream stream)
			      presentation-type view present-args)))
	       (formatting-cell (stream :align-x :left)
		 (do-present stream))))
	    (t
	     (setq query-identifier
		   (apply #'prompt-for-accept (encapsulated-stream stream)
			  presentation-type view present-args))
	     (do-present stream)))))
  (values object))


;;--- These frames should be resourced
(define-application-frame accept-values ()
    ((stream :initarg :stream)
     (exit-button-stream :initarg :exit-button-stream)
     (continuation :initarg :continuation)
     (exit-boxes :initform nil :initarg :exit-boxes)
     (selected-item :initform nil)
     (initially-select-query-identifier 
       :initform nil :initarg :initially-select-query-identifier)
     (resynchronize-every-pass :initform nil :initarg :resynchronize-every-pass)
     (check-overlapping :initform t :initarg :check-overlapping)
     (own-window :initform nil :initarg :own-window)
     (own-window-properties :initform nil :initarg :own-window-properties))
  (:top-level (accept-values-top-level))
  (:command-definer t))

;;--- These frames should be resourced
(define-application-frame accept-values-own-window (accept-values)
    ((help-window :initform nil)
     (scroll-bars :initform nil :initarg :scroll-bars))
  (:pane 
    (with-slots (stream own-window exit-button-stream scroll-bars) *application-frame*
      (vertically ()
	(outlining ()
	  (progn
	    (setq stream (make-instance 'accept-values-stream
			   :stream (setq own-window 
					 (make-pane 'clim-stream-pane
					   :initial-cursor-visibility :off
					   :end-of-page-action :allow))))
	    (if scroll-bars
		(scrolling (:scroll-bars scroll-bars) own-window)
		own-window)))
	(outlining ()
	  (setf exit-button-stream
		(make-pane 'clim-stream-pane
		  :initial-cursor-visibility nil))))))
  (:menu-bar nil)
  (:top-level (accept-values-top-level))
  (:command-table accept-values)
  (:command-definer nil))

(defmethod frame-manager-accepting-values-frame-class ((framem standard-frame-manager))
  'accept-values-own-window)

(defmethod frame-manager-default-exit-boxes ((framem standard-frame-manager))
  '((:abort) (:exit)))

(defmethod frame-manager-exit-box-labels 
	   ((framem standard-frame-manager) frame view)
  (declare (ignore frame view))
  '((:exit   "Exit")
    (:abort  "Cancel")))

;; So the continuation can run with the proper value of *APPLICATION-FRAME*
(defvar *avv-calling-frame*)

(defun invoke-accepting-values (stream continuation
				 &key frame-class command-table own-window 
				      (exit-boxes 
					(frame-manager-default-exit-boxes
					  (frame-manager stream)))
 				      (resize-frame nil) (scroll-bars nil)
				      (initially-select-query-identifier nil)
				      (modify-initial-query nil) align-prompts
				      (resynchronize-every-pass nil) (check-overlapping t)
				      label x-position y-position width height)
   (incf *accept-values-tick*)
   (setq align-prompts (ecase align-prompts
			 ((t :right) :right)
			 ((:left) :left)
			 ((nil) nil)))
   (let ((frame-manager (frame-manager stream))
	 (*current-accept-values-tick* *accept-values-tick*)
	 (the-own-window nil)
	 (right-margin 10)
	 (bottom-margin 10))
     ;; Create the AVV, run it, and return its values
     (if own-window
	 (let ((frame (make-application-frame 
			(or frame-class 
			    (frame-manager-accepting-values-frame-class
			      frame-manager))
			:calling-frame *application-frame*
			:parent frame-manager
			:pretty-name label
			:continuation continuation
			:exit-boxes exit-boxes
			:own-window the-own-window
			:own-window-properties (list x-position y-position
						     width height
						     right-margin bottom-margin)
			:initially-select-query-identifier 
			  (and initially-select-query-identifier
			       (cons initially-select-query-identifier modify-initial-query))
			:resynchronize-every-pass resynchronize-every-pass
			:check-overlapping check-overlapping
			:resize-frame resize-frame
			:scroll-bars scroll-bars)))
	   (when command-table
	     (setf (frame-command-table frame) command-table))
	   (unwind-protect
	       (let ((*avv-calling-frame* *application-frame*))
		 (run-frame-top-level frame))
	     ;; Flush it so the GC can get rid of it
	     (disown-frame (frame-manager frame) frame)))
	 (using-resource (avv-stream accept-values-stream (or the-own-window stream)
				     :align-prompts align-prompts)
	   (let ((frame (make-application-frame (or frame-class 'accept-values)
			  :calling-frame *application-frame*
			  :stream avv-stream
			  :continuation continuation
			  :exit-boxes exit-boxes
			  :initially-select-query-identifier 
			    (and initially-select-query-identifier
				 (cons initially-select-query-identifier modify-initial-query))
			  :resynchronize-every-pass resynchronize-every-pass
			  :check-overlapping check-overlapping
			  ;; This frame won't necessarily be adopted, so make
			  ;; sure that we share the sheet with the parent frame
			  ;; in the case of "inlined" dialogs
			  :top-level-sheet (frame-top-level-sheet *application-frame*))))
	     (when command-table
	       (setf (frame-command-table frame) command-table))
	     (unwind-protect
		 (let ((*avv-calling-frame* *application-frame*))
		   (run-frame-top-level frame))
	       ;; Since this frame isn't really adopted, we don't really want
	       ;; to disown it since that will end up disowning the calling
	       ;; frame.  Flush it from the frame manager manually.  Barf.
	       (let ((framem (frame-manager frame)))
		 (setf (frame-manager-frames framem)
		         (delete frame (frame-manager-frames framem))
		       (slot-value frame 'frame-manager) nil))))))))

(defmethod accept-values-top-level ((frame accept-values) &rest args)
  (declare (ignore args))
  (with-slots (stream continuation resynchronize-every-pass check-overlapping
	       selected-item initially-select-query-identifier
	       own-window own-window-properties exit-button-stream) frame
    (let* ((original-view (stream-default-view stream))
	   (return-values nil)
	   (initial-query nil)
	   exit-button-record
	   avv avv-record
	   (align-prompts (slot-value stream 'align-prompts))
	   (properties own-window-properties)
	   (own-window-x-position (pop properties))
	   (own-window-y-position (pop properties))
	   (own-window-width  (pop properties))
	   (own-window-height (pop properties))
	   (own-window-right-margin  (pop properties))
	   (own-window-bottom-margin (pop properties)))
      (letf-globally (((stream-default-view stream) 
		       (frame-manager-dialog-view (frame-manager frame)))
		      ((cursor-state (stream-text-cursor stream)) nil))
	(labels ((run-continuation (stream avv-record)
		   (setf (slot-value stream 'avv-record) avv-record)
		   (setf (slot-value stream 'avv-frame) frame)
		   (with-output-recording-options (stream :draw nil :record t)
		     (let ((*application-frame* *avv-calling-frame*))
		       (if align-prompts
			   ;; Use of FORMATTING-TABLE here implies that
			   ;; no output should be done by the user code
			   ;; outside of calls to FORMATTING-ROW and
			   ;; FORMATTING-CELL.  Too bad.
			   (formatting-table (stream)
			     (setq return-values
				   (multiple-value-list
				     (funcall continuation stream))))
			   (setq return-values
				 (multiple-value-list
				   (funcall continuation stream)))))
		     (unless own-window
		       (display-exit-boxes frame stream
					   (stream-default-view stream)))))
		 (run-avv ()
		   (when (and initially-select-query-identifier
			      (setq initial-query
				    (find-query avv-record 
						(car initially-select-query-identifier))))
		     (if (cdr initially-select-query-identifier)
			 (com-modify-avv-choice initial-query)
			 (com-edit-avv-choice initial-query))
		     (redisplay avv stream :check-overlapping check-overlapping))
		   (loop
		     (let ((command
			     (let ((command-stream (encapsulating-stream-stream stream)))
			       ;; While we're reading commands, restore the view
			       ;; to what it was before we started.
			       (letf-globally (((stream-default-view command-stream)
						original-view))
				 (read-frame-command frame :stream command-stream)))))
		       (if (and command (not (keyboard-event-p command)))
			   (execute-frame-command frame command)
			   (beep stream)))
		     (with-deferred-gadget-updates
		       (when (or resynchronize-every-pass
				 (slot-value avv-record 'resynchronize))
			 ;; When the user has asked to resynchronize every pass, that
			 ;; means we should run the continuation an extra time to see
			 ;; that all visible stuff is up to date.  That's all!
			 (with-output-recording-options (stream :draw nil)
			   (redisplay avv stream :check-overlapping check-overlapping)))
		       (setf (slot-value avv-record 'resynchronize) nil)
		       (when exit-button-record
			 (redisplay exit-button-record exit-button-stream))
		       (redisplay avv stream :check-overlapping check-overlapping)
		       (when (ecase (frame-resizable frame)
			       ((nil) nil)
			       ((t :grow)
				(and own-window
				     (multiple-value-bind (width height)
					 (bounding-rectangle-size
					   (stream-output-history own-window))
				       (multiple-value-bind (vwidth vheight)
					   (bounding-rectangle-size 
					     (window-viewport own-window))
					 (if (eq (frame-resizable frame) :grow)
					     (or (> width vwidth) (> height vheight))
					     (or (/= width vwidth) (/= height vheight))))))))
			 (size-panes-appropriately)))))
		 (size-panes-appropriately ()
		   (changing-space-requirements ()
		     ;; We really want to specify the min/max sizes of
		     ;; the exit-button pane also
		     (size-frame-from-contents exit-button-stream
		       :size-setter
		         #'(lambda (pane w h)
			     (change-space-requirements pane 
			       :width w :min-width w :max-width w
			       :height h :min-height h :max-height h)))
		     (when own-window
		       (size-frame-from-contents own-window
			 :width own-window-width
			 :height own-window-height
			 :right-margin own-window-right-margin
			 :bottom-margin own-window-bottom-margin)))))
	  (declare (dynamic-extent #'run-continuation #'run-avv
				   #'size-panes-appropriately))
	  (handler-bind ((frame-exit
			   #'(lambda (condition)
			       (let ((exit-frame (frame-exit-frame condition)))
				 (when (eq frame exit-frame)
				   (return-from accept-values-top-level
				     (values-list return-values)))))))
	    (setq avv
		  (updating-output (stream)
		    (setq avv-record
			  (with-end-of-line-action (stream :allow)
			    (with-end-of-page-action (stream :allow)
			      (with-new-output-record
				  (stream 'accept-values-output-record avv-record)
				(run-continuation stream avv-record)))))))
	    ;; In own window dialogs the buttons are displayed separately
	    (when (and own-window exit-button-stream)
	      (setq exit-button-record
		    (updating-output (exit-button-stream)
		      (with-end-of-line-action (exit-button-stream :allow)
			(with-end-of-page-action (exit-button-stream :allow)
			  (display-exit-boxes frame exit-button-stream
					      (stream-default-view stream)))))))
	    (unwind-protect
		(cond (own-window
		       (size-panes-appropriately)
		       (multiple-value-bind (x y)
			   #+++ignore (pointer-position (port-pointer (port own-window)))
			   #---ignore (values 100 100)
			 (when (and own-window-x-position own-window-y-position)
			   (setq x own-window-x-position
				 y own-window-y-position))
			 (position-sheet-carefully
			   (frame-top-level-sheet (pane-frame own-window)) x y))
		       (setf (window-visibility own-window) t)
		       (with-input-focus (own-window)
			 (when exit-button-record
			   (replay exit-button-record exit-button-stream))
			 (replay avv stream)
			 (run-avv)))
		      (t
		       ;; Ensure that bottom of the AVV is visible.  I think that
		       ;; this is OK even if the AVV is bigger than the viewport.
		       (move-cursor-beyond-output-record
			 (encapsulating-stream-stream stream) avv)
		       (stream-ensure-cursor-visible stream)
		       (replay avv stream)
		       (run-avv)))
	      (unless own-window
		(deactivate-all-gadgets avv-record)
		(move-cursor-beyond-output-record 
		  (encapsulating-stream-stream stream) avv)))))))))

(defmethod invoke-with-aligned-prompts ((stream accept-values-stream) continuation &key (align-prompts t))
  (setq align-prompts (ecase align-prompts
			((t :right) :right)
			((:left) :left)
			((nil) nil)))
  (letf-globally (((slot-value stream 'align-prompts) align-prompts)) 
    (formatting-table (stream)
	(funcall continuation stream))))

(defmethod invoke-with-aligned-prompts ((stream t) continuation &key align-prompts)
  (declare (ignore align-prompts))
  (funcall continuation stream))

(defmethod frame-manager-display-input-editor-error
	   ((framem standard-frame-manager) (frame accept-values) stream error)
  ;;--- Resignal the error so the user can handle it
  ;;--- (in lieu of HANDLER-BIND-DEFAULT)
  (notify-user frame (princ-to-string error)
	       :title "Input error"
	       :style :error :exit-boxes '(:exit))
  (remove-activation-gesture stream)
  ;; Now wait until the user forces a rescan by typing
  ;; an input editing command
  (loop (read-gesture :stream stream)))

(defmethod frame-manager-display-help 
	   ((framem standard-frame-manager) (frame accept-values-own-window) stream continuation)
  (declare (dynamic-extent continuation))
  (when (null (slot-value frame 'help-window))
    (setf (slot-value frame 'help-window)
	  (allocate-resource 'menu stream (window-root stream))))
  (let ((help-window (slot-value frame 'help-window))
	(own-window (slot-value frame 'own-window)))
    (setf (window-visibility help-window) nil)
    ;; You might think that we should bind *INPUT-CONTEXT* to NIL, but
    ;; by not doing the translators from the ACCEPT-VALUES dialog apply
    ;; to any items presented in the help window.  Cool, huh?
    (let ((*original-stream* nil))
      (window-clear help-window)
      (with-output-recording-options (help-window :draw t :record t)
	(with-end-of-line-action (help-window :allow)
	  (with-end-of-page-action (help-window :allow)
	    (funcall continuation help-window)
	    (fresh-line help-window)
	    (with-text-face (help-window :italic)
	      (write-line "Press any key to remove this window" help-window)))))
      (size-frame-from-contents help-window)
      (multiple-value-bind (x y) (bounding-rectangle-position own-window)
	(position-sheet-carefully 
	  (frame-top-level-sheet (pane-frame help-window)) x y))
      (setf (window-visibility help-window) t)
      (clear-input help-window)
      (unwind-protect
	  (with-input-focus (help-window)
	    (read-gesture :stream help-window))
	(setf (window-visibility help-window) nil)))))

(defmethod accept-values-top-level :around ((frame accept-values-own-window) &rest args)
  (declare (ignore args))
  (unwind-protect
      (call-next-method)
    (with-slots (help-window) frame
      (when help-window
	(deallocate-resource 'menu help-window)
	(setq help-window nil)))))

(defmethod read-frame-command ((frame accept-values) &key (stream *standard-input*))
  (read-command (frame-command-table frame)
		:stream stream
		:command-parser 'menu-command-parser
		:use-keystrokes t))

(defmethod accept-values-resynchronize ((stream accept-values-stream))
  (setf (slot-value (slot-value stream 'avv-record) 'resynchronize) t))

(defmethod accept-values-resize-window ((stream accept-values-stream))
  (with-slots (own-window own-window-properties)
	      (slot-value stream 'avv-frame)
    (when own-window
    (let* ((own-window-x-position (pop own-window-properties))
	   (own-window-y-position (pop own-window-properties))
	   (own-window-width  (pop own-window-properties))
	   (own-window-height (pop own-window-properties))
	   (own-window-right-margin  (pop own-window-properties))
	   (own-window-bottom-margin (pop own-window-properties)))
      (declare (ignore own-window-x-position own-window-y-position 
		       own-window-width own-window-height))
      (multiple-value-bind (new-width new-height)
	  (bounding-rectangle-size (slot-value stream 'avv-record))
	(multiple-value-bind (width height)
	    (window-inside-size own-window)
	  (when (or (> new-width width)
		    (> new-height height))
	    (size-frame-from-contents own-window
				      :right-margin own-window-right-margin
				      :bottom-margin own-window-bottom-margin))))))))


(define-presentation-type accept-values-exit-box ())

(defmacro with-exit-box-decoded ((value label text-style) exit-box labels &body body)
  `(let* ((,value (if (consp ,exit-box) (first ,exit-box) ,exit-box))
	  (,label (or (and (consp ,exit-box)
			   (second ,exit-box))
		      (second (assoc ,value ,labels))))
	  (,text-style (or (and (consp ,exit-box)
				(getf (cddr ,exit-box) :text-style))
			   (getf (cddr (assoc ,value ,labels)) :text-style))))
     ,@body))

;;; Applications can create their own AVV class and specialize this method in
;;; order to get different exit boxes.
(defmethod display-exit-boxes ((frame accept-values) stream (view view))
  ;; Do the fresh-line *outside* of the updating-output so that it
  ;; doesn't get repositioned relatively in the X direction if the
  ;; previous line gets longer.  Maybe there should be some better
  ;; way of ensuring this.
  (fresh-line stream)
  (let ((labels (frame-manager-exit-box-labels (frame-manager frame) frame view)))
    (updating-output (stream :unique-id stream :cache-value 'exit-boxes)
      (with-slots (exit-boxes) frame
	(dolist (exit-box exit-boxes)
	  (with-exit-box-decoded (value label text-style) exit-box labels
	    (when label
	      (with-text-style (stream text-style)
		(with-output-as-presentation (stream value 'accept-values-exit-box)
		  #-CCL-2
		  (write-string label stream)
		  #+CCL-2
		  (if (eq value ':abort)
		      ;; Kludge to print the cloverleaf char in MCL.
		      ;; Needs an accompanying kludge in STREAM-WRITE-CHAR so that
		      ;; #\CommandMark doesn't get lozenged.
		      (progn
			(with-text-style (stream '(:mac-menu :roman :normal))
			  (write-char #\CommandMark stream))
			(write-string "-. aborts" stream))
		      (write-string label stream)))
		(write-string " " stream)))))))))

;;--- Get this right
(defmethod frame-pointer-documentation-output ((frame accept-values-own-window))
  nil)


(defmethod deactivate-all-gadgets (record)
  (declare (ignore record))
  nil)

(defmethod deactivate-all-gadgets ((record output-record-mixin))
  (map-over-output-records #'deactivate-all-gadgets record))

(defmethod deactivate-all-gadgets :after ((record gadget-output-record))
  (map-over-sheets
    #'(lambda (sheet)
	(when (typep sheet 'gadget)
	  (deactivate-gadget sheet)))
    (output-record-gadget record)))


;;--- It would have been nice to have defined these in terms of the
;;--- other gestures
(define-gesture-name :edit-field   :pointer-button (:left))
(define-gesture-name :modify-field :pointer-button (:middle))
(define-gesture-name :delete-field :pointer-button (:middle :shift))

(define-presentation-type accept-values-choice ())

(defun-inline accept-values-query-valid-p (query query-record)
  (and (or (null query) (accept-values-query-active-p query))
       (let ((avv-record (find-accept-values-record query-record)))
	 (and avv-record
	      (= (slot-value avv-record 'tick) *current-accept-values-tick*)))))

(define-accept-values-command com-edit-avv-choice
    ((choice 'accept-values-choice))
  (with-slots (stream selected-item) *application-frame*
    (setq selected-item choice)
    (accept-values-query-edit-value choice stream)))

(define-presentation-to-command-translator edit-avv-choice
    (accept-values-choice com-edit-avv-choice accept-values
     :documentation "Edit this field"
     :pointer-documentation "Edit this field"
     :tester ((object presentation)
	      (accept-values-query-valid-p object presentation))
     :gesture :edit-field)
    (object)
  (list object))

(define-accept-values-command com-modify-avv-choice
    ((choice 'accept-values-choice))
  (with-slots (stream selected-item) *application-frame*
    (setq selected-item choice)
    (accept-values-query-edit-value choice stream :modify t))) 

(define-presentation-to-command-translator modify-avv-choice
    (accept-values-choice com-modify-avv-choice accept-values
     :documentation "Modify this field"
     :pointer-documentation "Modify this field"
     :tester ((object presentation)
	      (accept-values-query-valid-p object presentation))
     :gesture :modify-field)
    (object)
  (list object))

(defmethod accept-values-query-edit-value ((query accept-values-query) stream &key modify)
  (with-slots (presentation-type value changed-p prompt presentation) query
    (let ((stream (encapsulating-stream-stream stream)))
      (with-stream-cursor-position-saved (stream)
	(multiple-value-bind (xoff yoff)
	    (convert-from-relative-to-absolute-coordinates
	      stream (output-record-parent presentation))
	  (multiple-value-bind (x y) (output-record-position presentation)
	    (stream-set-cursor-position stream (+ x xoff) (+ y yoff))))
	(erase-output-record presentation stream)
	(catch-abort-gestures ("Abort editing the current field")
	  (let ((new-value nil)
		(record nil))
	    (setq record
		  (with-new-output-record (stream)
		    (setq new-value
			  ;; The text cursor should be visible while this ACCEPT is
			  ;; waiting for input to be typed into this field
			  (with-cursor-state (stream t)
			    (accept presentation-type
				    :stream stream :prompt nil :default value
				    :insert-default modify)))))
	    ;; This so that the input editor's typing gets erased properly.
	    (erase-output-record record stream)
	    ;;--- Kludge until Bill can explain the whole "leave the delimiter" vs
	    ;;--- "process the delimiter" scheme to me
	    (when (read-gesture :stream stream :peek-p t :timeout 0)
	      (process-delimiter stream))
	    (setf value new-value
		  changed-p t)))))))

(defun map-over-accept-values-queries (avv-record continuation)
  (declare (dynamic-extent continuation))
  (labels ((map-queries (record)
	     (let ((unique-id (and (updating-output-record-p record)
				   (slot-value record 'unique-id))))
	       (when (and (listp unique-id)
			  (eq (first unique-id) :query-identifier))
		 (funcall continuation record unique-id)))
	     (map-over-output-records #'map-queries record)))
    (declare (dynamic-extent #'map-queries))
    (map-queries avv-record)))

#+Genera
(define-accept-values-command (com-next-avv-choice :keystroke (:n :control)) ()
  (with-slots (stream selected-item) *application-frame*
    (let ((avv-record (slot-value stream 'avv-record)))
      (cond ((null selected-item)
	     (map-over-accept-values-queries avv-record
	       #'(lambda (record unique-id)
		   (declare (ignore record))
		   (return-from com-next-avv-choice
		     (setq selected-item (find-query avv-record unique-id))))))
	    (t
	     (let* ((item (find-query avv-record (slot-value selected-item 'query-identifier)))
		    (item-record (slot-value item 'presentation))
		    (found-item nil))
	       (map-over-accept-values-queries avv-record
		 #'(lambda (record unique-id)
		     (if found-item
			 (return-from com-next-avv-choice
			   (setq selected-item (find-query avv-record unique-id)))
		       (when (eq record item-record)
			 (setq found-item t)))))))))))

#+Genera
(define-accept-values-command (com-previous-avv-choice :keystroke (:p :control)) ()
  (with-slots (stream selected-item) *application-frame*
    (let ((avv-record (slot-value stream 'avv-record)))
      (cond ((null selected-item)
	     ;;--- Do this
	     )
	    (t
	     ;;--- Do this
	     )))))

#+Genera
(add-keystroke-to-command-table 'accept-values '(:e :control) :function
  #'(lambda (gesture numeric-argument)
      (declare (ignore gesture numeric-argument))
      (with-slots (selected-item) *application-frame*
	(if (null selected-item)
	    (beep)
	    `(com-edit-avv-choice ,selected-item))))
  :errorp nil)

(define-accept-values-command com-delete-avv-choice
    ((choice 'accept-values-choice))
  (accept-values-query-delete-value choice))

(define-presentation-to-command-translator delete-avv-choice
    (accept-values-choice com-delete-avv-choice accept-values
     :documentation "Remove this field"
     :pointer-documentation "Remove this field"
     :tester ((object presentation)
	      (accept-values-query-valid-p object presentation))
     :gesture :delete-field)
    (object)
  (list object))

(defmethod accept-values-query-delete-value ((query accept-values-query))
  (with-slots (presentation-type value changed-p) query
    (when (presentation-typep nil presentation-type)
      (setf value nil
	    changed-p t))))

(define-gesture-name :exit-dialog  :keyboard (:end))
(define-gesture-name :abort-dialog :keyboard (:abort))

(define-accept-values-command (com-exit-avv :keystroke :exit-dialog) ()
  (with-slots (stream) *application-frame*
    (let ((avv-record (slot-value stream 'avv-record))
	  (query-ids nil))
      (map-over-accept-values-queries avv-record
	#'(lambda (record unique-id)
	    (declare (ignore record))
	    (when (accept-values-query-error-p (find-query avv-record unique-id))
	      (push unique-id query-ids))))
      (if query-ids
	  (notify-user *application-frame*
		       (format nil "The following fields are not valid:~{ ~A~}"
			 (mapcar #'cadr (nreverse query-ids)))
		       :title "Invalid Fields"
		       :style :error :exit-boxes '(:exit))
	  (frame-exit *application-frame*)))))

(define-accept-values-command (com-abort-avv :keystroke :abort-dialog) ()
  (abort))

(define-presentation-translator abort-or-exit-avv
    (accept-values-exit-box (command :command-table accept-values) accept-values
     :tester-definitive t		;just like a to-command translator
     :documentation ((object stream)
		     (case object
		       (:exit (write-string "Exit" stream))
		       (:abort (write-string "Abort" stream))))
     :gesture :select)
    (object)
  (case object
    (:exit '(com-exit-avv))
    (:abort '(com-abort-avv))))


;;; ACCEPTING-VALUES command buttons

(defclass accept-values-command-button ()
    ((continuation :initarg :continuation)
     (documentation :initarg :documentation)
     (resynchronize :initarg :resynchronize)))

(define-presentation-type accept-values-command-button ())

(defmacro accept-values-command-button ((&optional stream &rest options) prompt 
					&body body &environment env)
  #+Genera (declare (zwei:indentation 1 3 2 1))
  #-(or Genera Minima) (declare (ignore env))
  (declare (arglist ((&optional stream 
		      &key documentation query-identifier
			   (cache-value t) (cache-test #'eql)
			   view resynchronize)
		     prompt &body body)))
  (default-input-stream stream accept-values-command-button)
  (let ((constant-prompt-p
	  (and (constantp prompt #+(or Genera Minima) env)
	       (stringp (eval prompt #+(or Genera Minima-Developer) env)))))
    `(flet ((avv-command-button-body () ,@body)
	    ,@(unless constant-prompt-p
		`((avv-command-button-prompt (,stream) ,prompt))))
       ,@(unless constant-prompt-p
	   `((declare (dynamic-extent #'avv-command-button-prompt))))
       (invoke-accept-values-command-button
	 ,stream
	 #'avv-command-button-body ,(getf options :view `(stream-default-view ,stream))
	 ,(if constant-prompt-p
	      (eval prompt #+(or Genera Minima-Developer) env)
	      '#'avv-command-button-prompt)
	 ,@options))))

(defmethod invoke-accept-values-command-button
	   (stream continuation (view t) prompt
	    &key (documentation (if (stringp prompt)
				    prompt
				    (with-output-to-string (stream)
				      (funcall prompt stream))))
		 (query-identifier (list ':button documentation))
		 (cache-value t) (cache-test #'eql)
		 resynchronize)
  (declare (dynamic-extent prompt))
  (updating-output (stream :unique-id query-identifier :id-test #'equal
			   :cache-value cache-value :cache-test cache-test)
    (with-output-as-presentation (stream
				  (make-instance 'accept-values-command-button
				    :continuation continuation
				    :documentation documentation
				    :resynchronize resynchronize)
				  'accept-values-command-button)
      (if (stringp prompt)
	  (write-string prompt stream)
	  (funcall prompt stream)))))

(define-accept-values-command com-avv-command-button
    ((button 'accept-values-command-button)
     (button-presentation 't))
  (funcall (slot-value button 'continuation))
  (when (slot-value button 'resynchronize)
    (let ((avv-record (find-accept-values-record button-presentation)))
      (setf (slot-value avv-record 'resynchronize) t))))

(define-presentation-to-command-translator avv-command-button
    (accept-values-command-button com-avv-command-button accept-values
     :documentation document-command-button
     :pointer-documentation document-command-button
     :tester ((presentation)
	      (accept-values-query-valid-p nil presentation))
     :gesture :select)
    (object presentation)
  (list object presentation))

(defun document-command-button
       (button presentation context-type frame event window x y stream)
  (declare (ignore presentation context-type frame event window x y))
  (let ((documentation (slot-value button 'documentation)))
    (cond ((stringp documentation)
	   (write-string documentation stream))
	  (documentation
	   (funcall documentation stream))
	  (t
	   (write-string "Execute this command button" stream)))))


;;; One-of and Some-of choices display

(defstruct accept-values-multiple-choices
  query-identifier
  select-action)

;; "One-of"
(defstruct accept-values-multiple-choice
  choices
  value)

(define-presentation-type accept-values-one-of ())
(define-presentation-type accept-values-some-of ())

(define-accept-values-command com-avv-choose-one-of
    ((choice 'accept-values-one-of))
  (avv-choose-one-of-1 choice))

(defun avv-choose-one-of-1 (choice)
  (let* ((choices (accept-values-multiple-choice-choices choice))
	 (query (accept-values-multiple-choices-query-identifier choices)))
    (setf (accept-values-query-value query)
	  (funcall (accept-values-multiple-choices-select-action choices)
		   (accept-values-multiple-choice-value choice)
		   (accept-values-query-value query)))
    (setf (accept-values-query-changed-p query) t)))

(define-presentation-to-command-translator avv-choose-one-of
    (accept-values-one-of com-avv-choose-one-of accept-values
     :documentation "Select this value"
     :pointer-documentation "Select this value"
     :tester ((object presentation)
	      (accept-values-query-valid-p
		(accept-values-multiple-choices-query-identifier
		  (accept-values-multiple-choice-choices object))
		presentation))
     :gesture :select)
    (object)
  (list object))

(defun accept-values-choose-from-sequence (stream sequence key selected-value tester
					   type query-identifier
					   select-action highlighting-function)
  (declare (dynamic-extent select-action highlighting-function))
  (flet ((presenter (thing stream)
	   (present thing type :stream stream)))
    (declare (dynamic-extent #'presenter))
    (accept-values-choose-from-sequence-1
      stream sequence key selected-value tester
      type query-identifier
      select-action highlighting-function
      'accept-values-one-of #'presenter)))

(defun accept-values-choose-from-sequence-1 (stream sequence key selected-value tester
					     type query-identifier
					     select-action highlighting-function
					     choice-type choice-presenter)
  (declare (dynamic-extent select-action highlighting-function tester choice-presenter))
  (declare (ignore type))
  ;;--- Unfortunately, this makes a new object every time we go around...
  (let ((choices (make-accept-values-multiple-choices
		   :query-identifier query-identifier
		   :select-action select-action)))
    (flet ((print-choice (object stream)
	     (let* ((value (funcall key object))
		    (selected-p (funcall tester value selected-value)))
	       (updating-output (stream :unique-id object
					:cache-value selected-p)
		 (with-output-as-presentation 
		     (stream
		      (make-accept-values-multiple-choice :choices choices :value value)
		      choice-type)
		   (formatting-cell (stream)
		     (if selected-p
			 (funcall highlighting-function choice-presenter value stream)
			 (funcall choice-presenter value stream))))))))
      (declare (dynamic-extent #'print-choice))
      ;; :MAX-WIDTH makes it use as few rows as will fit in the available width.
      ;; :X-SPACING makes it not spread the choices to fill the whole width.
      ;; :INITIAL-SPACING NIL makes it not add whitespace at the start of the field.
      (formatting-item-list (stream :max-width (- (stream-text-margin stream)
						  (stream-cursor-position stream))
				    :x-spacing '(2 :character)
				    :initial-spacing nil)
	(doseq (object sequence)
	  (print-choice object stream))))))

(define-accept-values-command com-avv-choose-some-of
    ((choice 'accept-values-some-of))
  (avv-choose-some-of-1 choice))

(defun avv-choose-some-of-1 (choice)
  (let* ((choices (accept-values-multiple-choice-choices choice))
	 (query (accept-values-multiple-choices-query-identifier choices)))
    (let ((new-value (funcall (accept-values-multiple-choices-select-action choices)
			      (accept-values-multiple-choice-value choice)
			      (accept-values-query-value query))))
      (if (member new-value (accept-values-query-value query))
	  ;; Use REMOVE instead of DELETE so the the new value is not EQ
	  ;; to the old one.  If it were to stay EQ, the field would not
	  ;; be properly redisplayed.
	  (setf (accept-values-query-value query)
		(remove new-value (accept-values-query-value query)))
	  (push new-value (accept-values-query-value query))))
    (setf (accept-values-query-changed-p query) t)))

(define-presentation-to-command-translator avv-choose-some-of
    (accept-values-some-of com-avv-choose-some-of accept-values
     :documentation "De/Select this value"
     :pointer-documentation "De/Select this value"
     :tester ((object presentation)
	      (accept-values-query-valid-p
		(accept-values-multiple-choices-query-identifier
		  (accept-values-multiple-choice-choices object))
		presentation))
     :gesture :select)
    (object)
  (list object))

(defun accept-values-choose-from-subset (stream sequence key selected-value tester
					 type query-identifier
					 select-action highlighting-function)
  (declare (dynamic-extent select-action highlighting-function))
  (flet ((presenter (thing stream)
	   (present (list thing) type :stream stream)))
    (declare (dynamic-extent #'presenter))
    (accept-values-choose-from-sequence-1
      stream sequence key selected-value 
      #'(lambda (object sequence)
	  (member object sequence :test tester))
      type query-identifier
      select-action highlighting-function
      'accept-values-some-of #'presenter)))


;;; Support for AVVs as application panes

(define-command-table accept-values-pane)

;;--- There must be a better way to do this
(defvar *pane-to-avv-stream-table* (make-hash-table))

;; No need for AVV panes to deal with *CURRENT-ACCEPT-VALUES-TICK*, since it is
;; guaranteed that those queries will be valid at all times.
(defun accept-values-pane-displayer (frame pane
				     &key displayer
					  resynchronize-every-pass
					  (check-overlapping t) 
					  align-prompts
					  max-height max-width)
  (declare (ignore max-height max-width))
  (setq align-prompts (ecase align-prompts
			((t :right) :right)
			((:left) :left)
			((nil) nil)))
  (let* ((stream-and-record (and (not *frame-layout-changing-p*)
				 (gethash pane *pane-to-avv-stream-table*)))
	 (avv-stream (car stream-and-record))
	 (avv-record (cdr stream-and-record)))
    (cond (avv-stream
	   (with-deferred-gadget-updates
	     (letf-globally (((stream-default-view avv-stream) 
			      (frame-manager-dialog-view (frame-manager frame))))
	       (redisplay avv-record avv-stream :check-overlapping check-overlapping)
	       (when resynchronize-every-pass
		 (redisplay avv-record avv-stream :check-overlapping check-overlapping)))))
	  (t
	   (accept-values-pane-displayer-1 frame pane displayer align-prompts)))))

(defun accept-values-pane-displayer-1 (frame pane displayer align-prompts)
  (let ((avv-stream (make-instance 'accept-values-stream 
		      :stream pane :align-prompts align-prompts))
	(avv-record nil))
    (setf (slot-value avv-stream 'avv-frame) frame)
    (letf-globally (((stream-default-view pane) 
		     (frame-manager-dialog-view (frame-manager frame))))
      (setq avv-record
	    (updating-output (avv-stream)
	      (with-new-output-record 
		  (avv-stream 'accept-values-output-record avv-record)
		(setf (slot-value avv-stream 'avv-record) avv-record)
		(if align-prompts
		    (formatting-table (avv-stream)
		      (funcall displayer frame avv-stream))
		    (funcall displayer frame avv-stream))))))
    (unless *sizing-application-frame*
      (setf (gethash pane *pane-to-avv-stream-table*)
	    (cons avv-stream avv-record)))))

(define-command (com-edit-avv-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice)
     (pane 't))
  (accept-values-query-edit-value choice (car (gethash pane *pane-to-avv-stream-table*))))

(define-presentation-to-command-translator edit-avv-pane-choice
    (accept-values-choice com-edit-avv-pane-choice accept-values-pane
     :documentation "Edit this field"
     :pointer-documentation "Edit this field"
     :gesture :edit-field
     :priority 1	;prefer this to IDENTITY when in COMMAND-OR-FORM context
     ;; Echoing this is annoying, as is putting it into the command history
     :echo nil :maintain-history nil)
    (object window)
  (list object window))

(define-command (com-modify-avv-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice)
     (pane 't))
  (accept-values-query-edit-value choice (car (gethash pane *pane-to-avv-stream-table*))
			:modify t))

(define-presentation-to-command-translator modify-avv-pane-choice
    (accept-values-choice com-modify-avv-pane-choice accept-values-pane
     :documentation "Modify this field"
     :pointer-documentation "Modify this field"
     :gesture :modify-field
     :priority 1	;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object window)
  (list object window))

(define-command (com-delete-avv-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice))
  (accept-values-query-delete-value choice))

(define-presentation-to-command-translator delete-avv-pane-choice
    (accept-values-choice com-delete-avv-pane-choice accept-values-pane
     :documentation "Remove this field"
     :pointer-documentation "Remove this field"
     :gesture :delete-field
     :priority 1	;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object)
  (list object))

(define-command (com-avv-pane-command-button :command-table accept-values-pane)
    ((button 'accept-values-command-button)
     (pane 't))
  (funcall (slot-value button 'continuation))
  (when (slot-value button 'resynchronize)
    (let* ((stream-and-record (and (not *frame-layout-changing-p*)
				   (gethash pane *pane-to-avv-stream-table*)))
	   (avv-stream (car stream-and-record))
	   (avv-record (cdr stream-and-record)))
      (when avv-stream
	(letf-globally (((stream-default-view avv-stream) +textual-dialog-view+))
	  (redisplay avv-record avv-stream))))))

(define-presentation-to-command-translator avv-pane-command-button
    (accept-values-command-button com-avv-pane-command-button accept-values-pane
     :documentation document-command-button
     :pointer-documentation document-command-button
     :gesture :select
     :priority 1	;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object window)
  (list object window))

(define-command (com-avv-pane-choose-one-of :command-table accept-values-pane)
    ((choice 'accept-values-one-of))
  (avv-choose-one-of-1 choice))

(define-presentation-to-command-translator avv-pane-choose-one-of
    (accept-values-one-of com-avv-pane-choose-one-of accept-values-pane
     :documentation "Select this value"
     :pointer-documentation "Select this value"
     :gesture :select
     :priority 1	;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object)
  (list object))

(define-command (com-avv-pane-choose-some-of :command-table accept-values-pane)
    ((choice 'accept-values-some-of))
  (avv-choose-some-of-1 choice))

(define-presentation-to-command-translator avv-pane-choose-some-of
    (accept-values-some-of com-avv-pane-choose-some-of accept-values-pane
     :documentation "De/Select this value"
     :pointer-documentation "De/Select this value"
     :gesture :select
     :priority 1	;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object)
  (list object))


;;; Fancy gadget-based dialogs...

(defmethod display-exit-boxes ((frame accept-values) stream (view gadget-dialog-view))
  (fresh-line stream)
  (let ((labels (frame-manager-exit-box-labels (frame-manager frame) frame view)))
    (with-slots (exit-boxes) frame
      (updating-output (stream :unique-id stream
			       :cache-value 'exit-boxes)
	(formatting-table (stream :equalize-column-widths nil)
	  (dolist (exit-box exit-boxes)
	    (with-exit-box-decoded (value label text-style) exit-box labels
	      (when label
		(when text-style
		  (setq text-style `(:text-style ,text-style)))
		(formatting-column (stream)
		  (formatting-cell (stream)
		    (with-output-as-gadget (stream)
		      (apply #'make-pane 'push-button
			:label label
			:client frame :id value
			:activate-callback #'handle-exit-box-callback
			text-style))))))))))))

(defun handle-exit-box-callback (gadget)
  (let ((id (gadget-id gadget)))
    (case id
      (:exit (com-exit-avv))
      (:abort (com-abort-avv))
      (t (funcall id)))))

;; It's OK that this is only in the ACCEPT-VALUES command table because
;; we're going to execute it manually in the callbacks below
(define-command (com-change-query :command-table accept-values)
    ((query t)
     (new-value t))
  (with-slots (value changed-p) query
    (setf value new-value
	  changed-p t)))

(defmethod accept-values-value-changed-callback ((gadget value-gadget) new-value stream query)
  (when (accept-values-query-valid-p query (accept-values-query-presentation query))
    ;; Only call the callback if the query is still valid
    (do-avv-command new-value stream query)))

(defun do-avv-command (new-value client query)
  (let ((sheet (slot-value client 'stream)))
    (process-command-event
      sheet
      (allocate-event 'presentation-event
	:sheet sheet
	:presentation-type 'command
	:value `(com-change-query ,query ,new-value)
	:frame (slot-value client 'avv-frame)))))

(defmethod accept-values-value-changed-callback ((gadget radio-box) new-value stream query-id)
  (call-next-method gadget (gadget-id new-value) stream query-id))

(defmethod accept-values-value-changed-callback ((gadget check-box) new-value stream query-id)
  (call-next-method gadget (mapcar #'gadget-id new-value) stream query-id))

(defun make-accept-values-value-changed-callback (stream query)
  ;; We attach a callback function directly now
  `(,#'accept-values-value-changed-callback ,stream ,query))

;; This is how we associate an output-record with the button
(defmethod invoke-accept-values-command-button
    	   (stream continuation (view gadget-dialog-view) prompt
	    &key (documentation (if (stringp prompt)
				    prompt
				    (with-output-to-string (stream)
				      (funcall prompt stream))))
		 (query-identifier (list ':button documentation))
		 (cache-value t) (cache-test #'eql)
		 resynchronize)
  (declare (dynamic-extent prompt))
  (updating-output (stream :unique-id query-identifier :id-test #'equal
			   :cache-value cache-value :cache-test cache-test)
   (with-output-as-gadget (stream)
     (let ((record (stream-current-output-record (encapsulating-stream-stream stream)))
	   (client (make-instance 'accept-values-command-button
		     :continuation continuation
		     :documentation documentation
		     :resynchronize resynchronize)))
       (make-pane 'push-button
	 :label (if (stringp prompt)
		    prompt
		    ;;--- Perhaps we should create a pixmap
		    (with-output-to-string (stream)
		      (funcall prompt stream)))
	 :id record :client client
	 :activate-callback
	   #'(lambda (button)
	       (when (accept-values-query-valid-p nil record)	;---can't be right
		 (throw-highlighted-presentation
		   (make-instance 'standard-presentation
		     :object `(com-avv-command-button ,client ,record)
		     :type 'command)
		   *input-context*
		   ;;--- It would be nice if we had the real event...
		   (allocate-event 'pointer-button-press-event
		     :sheet (sheet-parent button)
		     :x 0 :y 0
		     :modifier-state 0
		     :button +pointer-left-button+)))))))))
