;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: accept-values.lisp,v 1.16 92/04/21 16:12:56 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; For historical reasons "AVV" means "accepting-values"...

(defclass accept-values-stream
	  (standard-encapsulating-stream)
    ((avv-record :initform nil)
     (avv-frame :initform nil)))

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
(defmethod match-output-records ((record accept-values-output-record) &rest init-args)
  (declare (ignore init-args))
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
		 :accessor accept-values-query-changed-p))
  (:default-initargs :presentation nil))

(defresource accept-values-stream (encapsulee)
  :constructor (make-instance 'accept-values-stream :stream encapsulee)
  :matcher 't					;any will suffice
  :initializer (setf (slot-value accept-values-stream 'stream) encapsulee))

(defun find-accept-values-record (output-record)
  (do ((record output-record (output-record-parent record)))
      ((null record) nil)
    (when (typep record 'accept-values-output-record)
      (return record))))

(defmethod prompt-for-accept :around ((stream accept-values-stream) type (view view)
				      &rest accept-args
				      &key query-identifier (prompt t) (display-default nil)
				      &allow-other-keys)
  (declare (dynamic-extent accept-args))
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

(defmethod stream-accept ((stream accept-values-stream) ptype
			  &key query-identifier (prompt t) (default nil default-supplied-p)
			  &allow-other-keys)
  (let ((query (find-or-add-query stream query-identifier ptype prompt
				  default default-supplied-p)))
    (values (accept-values-query-value query)
	    (accept-values-query-type query)
	    ;; Set this to NIL so that it appears that nothing has
	    ;; changed at the start of the next pass.
	    (prog1 (accept-values-query-changed-p query)
		   (setf (accept-values-query-changed-p query) nil)))))

;; Probably should be keyword arguments...
(defmethod find-or-add-query ((stream accept-values-stream) query-identifier ptype prompt
			      default default-supplied-p)
  (let ((avv-record
	  #-ignore (slot-value stream 'avv-record)
	  #+ignore (find-accept-values-record
		     (or (stream-current-output-record stream)
			 (stream-output-history stream)))))
    (with-slots (query-table) avv-record
      (multiple-value-bind (query found-p)
	  (gethash query-identifier query-table)
	(unless query
	  (setq query (make-instance 'accept-values-query 
				     :presentation-type ptype
				     :value default
				     :query-identifier query-identifier)))
	;;--- Really wants to reuse existing presentation if found and
	;;--- make sure that that presentation gets re-parented if necessary
	;;--- (suppose inside FORMATTING TABLE...)
	(setf (accept-values-query-presentation query)
	      (updating-output
		  (stream :unique-id query-identifier
			  :id-test #'equal
			  :cache-value (if (accept-values-query-changed-p query)
					   (accept-values-query-value query)
					   default)
			  :cache-test #'equal)
		;;--- Calling ACCEPT-1 directly bypasses default preprocessing,
		;;--- is that OK?
		(cond ((or default-supplied-p (accept-values-query-changed-p query))
		       (accept-1 stream ptype
				 :prompt prompt
				 ;; If this field changed, use the value it changed to,
				 ;; making sure that the query object gets changed, too.
				 ;; We need to do this because the body of the dialog
				 ;; can change values without changing the query itself.
				 :default (if (accept-values-query-changed-p query)
					      (accept-values-query-value query)
					      (setf (accept-values-query-value query) default))
				 :history ptype
				 :present-p `(accept-values-choice ,query)
				 :query-identifier query))
		      ((presentation-typep (accept-values-query-value query) ptype)
		       ;; The programmer supplied no default, but a previous edit
		       ;; has put something in the query that we now must use as
		       ;; the default.
		       (accept-1 stream ptype
				 :prompt prompt
				 :default (accept-values-query-value query)
				 :history ptype
				 :present-p `(accept-values-choice ,query)
				 :query-identifier query))
		      (t
		       ;; No default supplied, field has never been edited.
		       (accept-1 stream ptype
				 :prompt prompt
				 :history ptype :provide-default nil
				 :present-p `(accept-values-choice ,query)
				 :query-identifier query)))))
	;; really wants to move the cursor position to some reasonable
	;; place in case we're just reusing an existing presentation
	(unless found-p
	  (setf (gethash query-identifier query-table) query))
	query))))

(define-application-frame accept-values ()
    ((stream :initarg :stream)
     (exit-button-stream :initarg :exit-button-stream)
     (continuation :initarg :continuation)
     (own-window :initform nil :initarg :own-window)
     (own-window-x-position :initform nil :initarg :x-position)
     (own-window-y-position :initform nil :initarg :y-position)
     (own-window-width :initform nil :initarg :width)
     (own-window-height :initform nil :initarg :height)
     (own-window-right-margin :initform nil :initarg :right-margin)
     (own-window-bottom-margin :initform nil :initarg :bottom-margin)
     (exit-boxes :initform nil :initarg :exit-boxes)
     (selected-item :initform nil)
     ;;--- Do this through RUN-FRAME-TOP-LEVEL?
     (initially-select-query-identifier :initform nil
					:initarg :initially-select-query-identifier)
     (resynchronize-every-pass :initform nil :initarg :resynchronize-every-pass)
     (check-overlapping :initform t :initarg :check-overlapping))
  (:top-level (accept-values-top-level))
  (:command-definer t))

(define-application-frame accept-values-own-window (accept-values)
    ()
  (:pane 
    (with-slots (stream own-window  exit-button-stream) *application-frame*
      (vertically ()
	  (outlining ()
	(scrolling  (:scroll-bars :dynamic)
	  (progn
	    (setq stream
		  (make-instance 'accept-values-stream
				 :stream (setf own-window
					       (make-pane 'clim-stream-pane
							  :initial-cursor-visibility nil))))
	    own-window)))
	  (outlining ()
	(setf exit-button-stream
	      (make-pane 'clim-stream-pane
			 :initial-cursor-visibility nil))))))
  (:menu-bar nil)
  (:top-level (accept-values-top-level))
  (:command-table accept-values)
  (:command-definer nil))

;; So the continuation can run with the proper value of *APPLICATION-FRAME*
(defvar *avv-calling-frame*)

#-Silica
(defun invoke-accepting-values (stream continuation
				&key frame-class own-window exit-boxes
				     (initially-select-query-identifier nil)
				     (resynchronize-every-pass nil) (check-overlapping t)
				     label x-position y-position width height)
  (incf *accept-values-tick*)
  (let ((*current-accept-values-tick* *accept-values-tick*)
	(the-own-window nil)
	(right-margin 10)
	(bottom-margin 10))
    (unwind-protect
	(progn
	  (when own-window
	    (setq the-own-window
		  (allocate-resource 'menu stream (window-root stream)))
	    ;;--- make the window big.  We shouldn't have to do this.
	    (multiple-value-bind (width height)
		(window-inside-size (slot-value the-own-window 'parent))
	      (bounding-rectangle-set-size the-own-window width height))
	    (setf (window-label the-own-window) label)
	    (when (listp own-window)
	      (setf right-margin
		    (process-spacing-arg the-own-window (getf own-window :right-margin 10)
					 'accepting-values :right-margin))
	      (setf bottom-margin
		    (process-spacing-arg the-own-window (getf own-window :bottom-margin 10)
					 'accepting-values :bottom-margin))))
	  
	  (using-resource (avv-stream accept-values-stream (or the-own-window stream))
	    ;;--- This should resource the AVV application frame, too
	    (let ((frame 
		    (make-application-frame 'accept-values
					    :parent (slot-value avv-stream 'stream)
					    :frame-class frame-class
					    :stream avv-stream
					    :exit-boxes exit-boxes
					    :continuation continuation
					    :own-window the-own-window
					    :x-position x-position
					    :y-position y-position
					    :width width :height height
					    :right-margin right-margin
					    :bottom-margin bottom-margin
					    :initially-select-query-identifier
					      initially-select-query-identifier
					    :resynchronize-every-pass
					      resynchronize-every-pass
					    :check-overlapping check-overlapping)))
	      (when (frame-top-level-sheet frame)
		(window-expose (frame-top-level-sheet frame)))
	      ;; Run the AVV and return its values
	      (let ((*avv-calling-frame* *application-frame*))
		(run-frame-top-level frame)))))
      (when (and the-own-window (windowp the-own-window))
	;; Deallocate menu resources deexposes them for us
	(deallocate-resource 'menu the-own-window)))))

#+Silica
(defun invoke-accepting-values (stream continuation
				 &key frame-class own-window exit-boxes
				      (initially-select-query-identifier nil)
				      (resynchronize-every-pass nil) (check-overlapping t)
				      label x-position y-position width height)
   (incf *accept-values-tick*)
   (let ((*current-accept-values-tick* *accept-values-tick*)
	 (the-own-window nil)
	 (right-margin 10)
	 (bottom-margin 10))
     (if own-window
	 (let ((frame (make-application-frame (or frame-class 'accept-values-own-window)
					      :calling-frame *application-frame*
					      :parent
					      *application-frame*
					      :pretty-name label
					      :continuation continuation
					      :exit-boxes exit-boxes
					      :own-window the-own-window
					      :x-position x-position
					      :y-position y-position
					      :right-margin right-margin
					      :bottom-margin bottom-margin
					      :initially-select-query-identifier
					        initially-select-query-identifier
					      :resynchronize-every-pass
					        resynchronize-every-pass
					      :check-overlapping
					      check-overlapping)))
	   ;; What do we do about sizing?????
	   ;; What do we do about positioning????
	   (let ((*avv-calling-frame* *application-frame*))
	     (run-frame-top-level frame)))
       (using-resource (avv-stream accept-values-stream (or the-own-window stream))
	 ;;--- This should resource the AVV application frame, too
	 (let ((frame (make-application-frame (or frame-class 'accept-values)
					      :calling-frame *application-frame*
					      :stream avv-stream
					      :continuation continuation
					      :exit-boxes exit-boxes
					      :own-window the-own-window
					      :x-position x-position
					      :y-position y-position
					      :right-margin right-margin
					      :bottom-margin bottom-margin
					      :initially-select-query-identifier
						initially-select-query-identifier
					      :resynchronize-every-pass
						resynchronize-every-pass
					      :check-overlapping
						check-overlapping)))
	   ;; Run the AVV and return its values
	   (let ((*avv-calling-frame* *application-frame*))
	     (run-frame-top-level frame)))))))

(defmethod accept-values-top-level ((frame accept-values) &rest args)
  (declare (ignore args))
  ;; this might want to use table-formatting or equivalent
  ;; to make sure that the rows line up properly.
  ;; This requires formatting-table and friends to return their bodies' values properly.
  (with-slots (stream continuation resynchronize-every-pass check-overlapping
	       selected-item initially-select-query-identifier
	       own-window own-window-x-position own-window-y-position
	       own-window-width own-window-height
	       own-window-right-margin own-window-bottom-margin
	       exit-button-stream) frame
    (let ((command-table (frame-command-table frame))
	  (original-view (stream-default-view stream))
	  (return-values nil)
	  (initial-query nil)
	  exit-button-record
	  avv avv-record)
      (letf-globally (((stream-default-view stream) 
		       (port-dialog-view (port stream))))
	(flet ((run-continuation (stream avv-record)
		 (setf (slot-value stream 'avv-record) avv-record)
		 (setf (slot-value stream 'avv-frame) frame)
		 (with-output-recording-options (stream :draw nil :record t)
		   (setq return-values (multiple-value-list
					 (let ((*application-frame* *avv-calling-frame*))
					   (funcall continuation stream))))
		   (unless own-window
		     (display-exit-boxes frame stream
					 (stream-default-view stream)))))
	       (run-avv ()
		 (when (and initially-select-query-identifier
			    (setq initial-query
				  (find-query avv-record initially-select-query-identifier)))
		   (com-edit-avv-choice initial-query)
		   (redisplay avv stream :check-overlapping check-overlapping))
		 (loop
		   (let ((command
			   (let ((command-stream (slot-value stream 'stream)))
			     ;; While we're reading commands, restore the view
			     ;; to what it was before we started.
			     (letf-globally (((stream-default-view command-stream)
					      original-view))
			       (read-command command-table
					     :stream command-stream
					     :command-parser 'menu-command-parser
					     :use-keystrokes t)))))
		     (if (and command (not (keyboard-event-p command)))
			 (execute-frame-command frame command)
		       (beep stream)))
		   (with-defered-gadget-updates
		       (when (or resynchronize-every-pass (slot-value avv-record 'resynchronize))
			 ;; When the user has asked to resynchronize every pass, that
			 ;; means we should run the continuation an extra time to see
			 ;; that all visible stuff is up to date.  That's all!
			 (with-output-recording-options (stream :draw nil)
			   (redisplay avv stream :check-overlapping check-overlapping)))
		     (setf (slot-value avv-record 'resynchronize) nil)
		     (when exit-button-record
		       (redisplay exit-button-record exit-button-stream))
		     (redisplay avv stream :check-overlapping check-overlapping)))))
	  (declare (dynamic-extent #'run-continuation #'run-avv))
	  (with-simple-restart (frame-exit "Exit from the ACCEPT-VALUES dialog")
	    (setq avv
		  (updating-output (stream)
		    (setq avv-record
			  (with-end-of-line-action (stream :allow)
			    (with-end-of-page-action (stream :allow)
			      (with-new-output-record
				  (stream 'accept-values-output-record avv-record)
				(run-continuation stream avv-record)))))))
	    ;; In own window dialogs the buttons are displayed separately
	    (when own-window
	      (setq exit-button-record
		    (updating-output (exit-button-stream)
		      (with-end-of-line-action (exit-button-stream :allow)
			(with-end-of-page-action (exit-button-stream :allow)
			  (display-exit-boxes frame exit-button-stream
					      (stream-default-view stream)))))))
	    (unwind-protect
		(cond (own-window
		       (size-menu-appropriately exit-button-stream)
		       (size-menu-appropriately own-window
						:width own-window-width
						:height own-window-height
						:right-margin own-window-right-margin
						:bottom-margin own-window-bottom-margin)
		       (multiple-value-bind (x y)
			   #-Silica
			   (stream-pointer-position-in-window-coordinates
			    (window-parent own-window))
			   #+Silica
			   (values 500 500)
			 (when (and own-window-x-position own-window-y-position)
			   (setq x own-window-x-position
				 y own-window-y-position))
			 (position-window-near-carefully own-window x y))
		       (window-expose own-window)
		       (with-input-focus (own-window)
			 (replay exit-button-record exit-button-stream)
			 (replay avv stream)
			 (run-avv)))
		      (t
		       ;; Ensure that bottom of the AVV is visible.  I think that
		       ;; this is OK even if the AVV is bigger than the viewport.
		       (move-cursor-beyond-output-record (slot-value stream 'stream) avv)
		       (stream-ensure-cursor-visible stream)
		       (replay avv stream)
		       (run-avv)))
	      (unless own-window
		(move-cursor-beyond-output-record (slot-value stream 'stream) avv))))
	  (values-list return-values))))))

(defmethod accept-values-resynchronize ((stream accept-values-stream))
  (setf (slot-value (slot-value stream 'avv-record) 'resynchronize) t))

(defmethod accept-values-resize-window ((stream accept-values-stream))
  (with-slots (own-window own-window-right-margin own-window-bottom-margin)
	      (slot-value stream 'avv-frame)
    (when own-window
      (multiple-value-bind (new-width new-height)
	  (bounding-rectangle-size (slot-value stream 'avv-record))
	(multiple-value-bind (width height)
	    (window-inside-size own-window)
	  (when (or (> new-width width)
		    (> new-height height))
	    (size-menu-appropriately own-window
				     :right-margin own-window-right-margin
				     :bottom-margin own-window-bottom-margin)))))))

(define-presentation-type accept-values-exit-box ())

;;; Applications can create their own AVV class and specialize this method in
;;; order to get different exit boxes.
(defmethod display-exit-boxes ((frame accept-values) stream (view view))
  ;; Do the fresh-line *outside* of the updating-output so that it
  ;; doesn't get repositioned relatively in the X direction if the
  ;; previous line gets longer.  Maybe there should be some better
  ;; way of ensuring this.
  (fresh-line stream)
  (with-slots (exit-boxes) frame
    (let ((exit  (or (second (assoc :exit  exit-boxes)) "<End> uses these values"))
	  (abort (or (second (assoc :abort exit-boxes)) "<Abort> aborts")))
      (updating-output (stream :unique-id stream
			       :cache-value 'exit-boxes)
	(with-output-as-presentation (stream ':abort 'accept-values-exit-box)
	  #-CCL-2 (write-string abort stream)
	  ;; Kludge to print the cloverleaf char in MCL.
	  ;; Needs an accompanying kludge in STREAM-WRITE-CHAR so that
	  ;; #\CommandMark doesn't get lozenged.
	  #+CCL-2 (progn
		    (with-text-style (stream '(:mac-menu :roman :normal))
		      (write-char #\CommandMark stream))
		    (write-string "-. aborts" stream)))
	(write-string ", " stream)
	(with-output-as-presentation (stream ':done 'accept-values-exit-box)
	  (write-string exit stream))))))

;;--- Get this right
(defmethod frame-pointer-documentation-output ((frame accept-values))
  nil)


(define-presentation-type accept-values-choice ())

(defun-inline accept-values-query-valid-p (query-record)
  (let ((avv-record (find-accept-values-record query-record)))
    (and avv-record
	 (= (slot-value avv-record 'tick) *current-accept-values-tick*))))

(define-accept-values-command com-edit-avv-choice
    ((choice 'accept-values-choice))
  (with-slots (stream selected-item) *application-frame*
    (setq selected-item choice)
    (accept-values-query-edit-value choice stream)))

(define-presentation-to-command-translator edit-avv-choice
    (accept-values-choice com-edit-avv-choice accept-values
     :documentation "Edit this field"
     :pointer-documentation "Edit this field"
     :tester ((presentation)
	      (accept-values-query-valid-p presentation))
     :gesture :select)
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
     :tester ((presentation)
	      (accept-values-query-valid-p presentation))
     :gesture :describe)
    (object)
  (list object))

(defmethod accept-values-query-edit-value ((query accept-values-query) stream &key modify)
  (let ((stream (slot-value stream 'stream)))
    (with-slots (presentation-type value changed-p prompt presentation) query
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
			(letf-globally (((cursor-visibility (stream-text-cursor stream)) :off))
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
		changed-p t))))))

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
(define-accept-values-command (com-next-avv-choice :keystroke (:n :control))
    ()
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
(define-accept-values-command (com-previous-avv-choice :keystroke (:p :control))
    ()
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
     :tester ((presentation)
	      (accept-values-query-valid-p presentation))
     :gesture :delete)
    (object)
  (list object))

(defmethod accept-values-query-delete-value ((query accept-values-query))
  (with-slots (presentation-type value changed-p) query
    (when (presentation-typep nil presentation-type)
      (setf value nil
	    changed-p t))))

(define-accept-values-command (com-exit-avv :keystroke :end)
    ()
  (invoke-restart 'frame-exit))

(define-accept-values-command (com-abort-avv :keystroke :abort)
    ()
  (abort))

(define-presentation-translator abort-or-exit-avv
    (accept-values-exit-box (command :command-table accept-values) accept-values
     :tester-definitive t		;just like a to-command translator
     :documentation ((object stream)
		     (ecase object
		       (:done (write-string "Exit" stream))
		       (:abort (write-string "Abort" stream))))
     :gesture :select)
    (object)
  (ecase object
    (:done '(com-exit-avv))
    (:abort '(com-abort-avv))))


;;; ACCEPTING-VALUES command buttons

(defclass accept-values-command-button ()
    ((continuation :initarg :continuation)
     (documentation :initarg :documentation)
     (resynchronize :initarg :resynchronize)))

(define-presentation-type accept-values-command-button ())

(defmacro accept-values-command-button ((&optional stream &rest options) prompt &body body)
  #+Genera (declare (zwei:indentation 1 3 2 1))
  (declare (arglist ((&optional stream 
		      &key documentation query-identifier
			   (cache-value t) (cache-test #'eql)
			   view resynchronize)
		     prompt &body body)))
  (default-query-stream stream accept-values-command-button)
  `(flet ((avv-command-button-body () ,@body)
	  ,@(unless (stringp prompt)
	      `((avv-command-button-prompt (,stream) ,prompt))))
     ,@(unless (stringp prompt)
	 `((declare (dynamic-extent #'avv-command-button-prompt))))
     (invoke-accept-values-command-button
       ,stream
       #'avv-command-button-body ,(getf options :view `(stream-default-view ,stream))
       ,(if (stringp prompt) prompt '#'avv-command-button-prompt)
       ,@options)))

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
	      (accept-values-query-valid-p presentation))
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
     :tester ((presentation)
	      (accept-values-query-valid-p presentation))
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
     :tester ((presentation)
	      (accept-values-query-valid-p presentation))
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
					  resynchronize-every-pass (check-overlapping t))
  (let* ((stream-and-record (and #-Silica (not *frame-layout-changing-p*)
				 (gethash pane *pane-to-avv-stream-table*)))
	 (avv-stream (car stream-and-record))
	 (avv-record (cdr stream-and-record)))
    (cond (avv-stream
	   (letf-globally (((stream-default-view avv-stream) +textual-dialog-view+))
	     (redisplay avv-record avv-stream :check-overlapping check-overlapping)
	     (when resynchronize-every-pass
	       (redisplay avv-record avv-stream :check-overlapping check-overlapping))))
	  (t
	   (accept-values-pane-displayer-1 frame pane displayer)))))

(defun accept-values-pane-displayer-1 (frame pane displayer)
  (let ((avv-stream (make-instance 'accept-values-stream :stream pane))
	(avv-record nil))
    (letf-globally (((stream-default-view avv-stream) +textual-dialog-view+))
      (setq avv-record
	    (updating-output (avv-stream)
	      (with-new-output-record 
		  (avv-stream 'accept-values-output-record avv-record)
		(setf (slot-value avv-stream 'avv-record) avv-record)
		(funcall displayer frame avv-stream)))))
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
     :gesture :select
     :priority 1	;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil)
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
     :gesture :describe
     :priority 1	;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil)
    (object window)
  (list object window))

(define-command (com-delete-avv-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice))
  (accept-values-query-delete-value choice))

(define-presentation-to-command-translator delete-avv-pane-choice
    (accept-values-choice com-delete-avv-pane-choice accept-values-pane
     :documentation "Remove this field"
     :pointer-documentation "Remove this field"
     :gesture :delete
     :priority 1	;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil)
    (object)
  (list object))

(define-command (com-avv-pane-command-button :command-table accept-values-pane)
    ((button 'accept-values-command-button)
     (pane 't))
  (funcall (slot-value button 'continuation))
  (when (slot-value button 'resynchronize)
    (let* ((stream-and-record (and #-Silica (not *frame-layout-changing-p*)
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
     :echo nil)
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
     :echo nil)
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
     :echo nil)
    (object)
  (list object))

(defmethod pane-type-options ((type (eql :accept-values)))
  `(:display-after-commands :no-clear
    :default-size :compute
    :scroll-bars nil
    :initial-cursor-visibility :off))

#-silica
(defmethod size-frame-pane ((window window-mixin)
			    (frame standard-application-frame)
			    (type (eql :accept-values))
			    pane-description
			    cluster-type
			    available-width available-height)
  available-width available-height
  (let* ((options (pane-descriptor-options pane-description))
	 (display-function (getf options :display-function)))
    (if display-function
	(let ((window-contents
		(with-output-to-output-record (window)
		  (call-display-function display-function frame window))))
	  (multiple-value-bind (width height)
	      (bounding-rectangle-size window-contents)
	    (case cluster-type
	      (:row width)
	      (:column height))))
      ':rest)))


;;; Fancy dialogs...

(defmethod prompt-for-accept ((stream accept-values-stream) type (view gadget-dialog-view)
			      &rest accept-args 
			      &key query-identifier &allow-other-keys)
  ;; This does nothing, the gadget ACCEPT methods should provide a
  ;; label
  (unless (gadget-includes-prompt-p type stream view)
    (call-next-method))
  query-identifier)

(defmethod display-exit-boxes ((frame accept-values) stream (view gadget-dialog-view))
  (fresh-line stream)
  (with-slots (exit-boxes) frame
    (let ((exit  (or (second (assoc :exit  exit-boxes)) "<End> uses these values"))
	  (abort (or (second (assoc :abort exit-boxes)) "<Abort> aborts")))
      (updating-output (stream :unique-id stream
			       :cache-value 'exit-boxes)
	(formatting-item-list (stream :n-columns 2 :initial-spacing nil)
	 (formatting-cell (stream)
	  (with-output-as-gadget (stream)
	    (make-pane 'push-button
		       :label abort
		       :client frame :id :abort
		       :activate-callback
		         #'(lambda (gadget)
			     (declare (ignore gadget))
			     (com-abort-avv)))))
	 (formatting-cell (stream)
	  (with-output-as-gadget (stream)
	    (make-pane 'push-button
		       :label exit
		       :client frame :id :exit
		       :activate-callback
		         #'(lambda (gadget)
			    (declare (ignore gadget))
			    (com-exit-avv))))))))))


;; It's OK that this is only in the ACCEPT-VALUES command table because
;; we're going to execute it manually in the callbacks below
(define-command (com-change-query :command-table accept-values)
    ((id t)
     (new-value t))
  (with-slots (value changed-p) id
    (setf value new-value
	  changed-p t)))

(defun accept-values-value-changed-callback (gadget new-value stream query-id)
  (declare (ignore gadget))
  (when (accept-values-query-valid-p (accept-values-query-presentation query-id))
    ;; Only call the callback if the query is still valid
    (do-avv-command new-value stream query-id)))

(defun make-accept-values-value-changed-callback (stream query-id)
  ;; We attach a callback function directly now
  `(,#'accept-values-value-changed-callback ,stream ,query-id))

(defun do-avv-command (new-value client id)
  (throw-highlighted-presentation
   (make-instance 'standard-presentation
		  :object `(com-change-query ,id ,new-value)
		  :type 'command)
   *input-context*
   ;;--- It would be nice if we had the real event...
   (make-instance 'pointer-button-press-event
		  :sheet (slot-value client 'stream)
		  :x 0
		  :y 0
		  :modifiers 0
		  :button 256)))

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
  (updating-output 
   (stream :unique-id query-identifier :id-test #'equal
	   :cache-value cache-value :cache-test cache-test)
   (with-output-as-gadget (stream)
     (let ((id (stream-current-output-record (slot-value stream 'stream)))
	   (client (make-instance 'accept-values-command-button
				  :continuation continuation
				  :documentation documentation
				  :resynchronize resynchronize)))
       (make-pane 'push-button
		  :label prompt
		  :id id :client client
		  :activate-callback
		    #'(lambda (button)
		        (when (accept-values-query-valid-p id)
			      (throw-highlighted-presentation
				(make-instance 'standard-presentation
					       :object `(com-avv-command-button ,client ,id)
					       :type 'command)
				*input-context*
				;;--- It would be nice if we had the real event...
				(make-instance 'pointer-button-press-event
					       :sheet (sheet-parent button)
					       :x 0
					       :y 0
					       :modifiers 0
					       :button +pointer-left-button+)))))))))
