;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: CLIM; Lowercase: Yes -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;

;; $fiHeader: accept-values.lisp,v 1.2 92/01/02 15:11:54 cer Exp Locker: cer $

(in-package :clim)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
Copyright (c) 1991, Franz Inc. All rights reserved
 Portions copyright (c) 1989, 1990 International Lisp Associates."

(defclass avv-stream
	  (encapsulating-stream-mixin)
    ((avv-record :initform nil)))

;; Don't allow old AVV queries to be sensitive
(defvar *accept-values-tick* 0)

(defclass avv-output-record
	  (standard-sequence-output-record)
    ((query-table :initarg :query-table)
     (tick :initform *accept-values-tick*)
     (resynchronize :initform nil))
  (:default-initargs :query-table (make-hash-table :test #'equal)))

(defmethod find-query ((record avv-output-record) query-identifier)
  (declare (values query found-p))
  (gethash query-identifier (slot-value record 'query-table)))

;; We are in control of the AVV code, and we know that there is only 1 AVV
;; under it's superior updating-output record.  Therefore we make it match.
;; (There's a separate issue, having nothing to do with incremental-redisplay
;; of making nested, hierarchical, AVV's work.)
(defmethod match-output-records ((record avv-output-record) &rest init-args)
  (declare (ignore init-args))
  t)

(defclass avv-query ()
     ((query-identifier :initarg :query-identifier)
      (presentation-type :initarg :presentation-type :reader avv-query-presentation-type)
      (value :initarg :value :accessor avv-query-value)
      (presentation :initarg :presentation :accessor avv-query-presentation)
      (changed-p :initform nil :accessor avv-query-changed-p))
  (:default-initargs :presentation nil))

(clim-utils::defresource avv-stream (encapsulee)
  :constructor (make-instance 'avv-stream :stream encapsulee)
  :matcher 't					;any will suffice
  :initializer (setf (slot-value avv-stream 'stream) encapsulee))

(defun find-avv-record (output-record)
  (do ((record output-record (output-record-parent record)))
      ((null record) nil)
    (when (typep record 'avv-output-record)
      (return record))))

(defmethod prompt-for-accept :around ((stream avv-stream) presentation-type
				      &rest args
				      &key query-identifier (prompt t) (display-default nil)
				      &allow-other-keys)
  (declare (dynamic-extent args))
  ;; AVV has to have a query-identifier, so default it here and return it.
  ;; It's better to cons this list than to call format to intern something.
  (unless query-identifier
    (setq query-identifier (list ':query-identifier prompt presentation-type)))
  (when (or prompt display-default)
    (updating-output (stream :unique-id (list :prompt query-identifier)
			     :id-test #'equal
			     :cache-value prompt)
      (with-rem-keywords (prompt-args args '(:display-default))
	;; Explicitly pass DISPLAY-DEFAULT so that if it is not supplied by the
	;; user, we forcibly default it to NIL, which gives better-looking AVVs.
	(apply #'prompt-for-accept-internal stream presentation-type
					    :display-default display-default prompt-args))))
  query-identifier)

(defmethod accept-1 ((stream avv-stream) ptype
		     &key query-identifier (prompt t) (default nil default-supplied-p)
		     &allow-other-keys)
  (let ((query (find-or-add-query stream query-identifier ptype prompt
				  default default-supplied-p)))
    (values (avv-query-value query)
	    (avv-query-presentation-type query)
	    ;; Set this to NIL so that it appears that nothing has
	    ;; changed at the start of the next pass.
	    (prog1 (avv-query-changed-p query)
		   (setf (avv-query-changed-p query) nil)))))

;; Probably should be keyword arguments...
(defmethod find-or-add-query ((avv-stream avv-stream) query-identifier ptype prompt
			      default default-supplied-p)
  (let ((avv-record
	  #-ignore (slot-value avv-stream 'avv-record)
	  #+ignore (find-avv-record
		     (or (output-recording-stream-current-output-record-stack avv-stream)
			 (output-recording-stream-output-record avv-stream)))))
    (with-slots (query-table) avv-record
      (multiple-value-bind (query found-p)
	  (gethash query-identifier query-table)
	(unless query
	  (setq query (make-instance 'avv-query 
				     :presentation-type ptype
				     :value default
				     :query-identifier query-identifier)))
	;;--- Really wants to reuse existing presentation if found and
	;;--- make sure that that presentation gets re-superiored if necessary
	;;--- (suppose inside FORMATTING TABLE...)
	(setf (avv-query-presentation query)
	      (updating-output
		  (avv-stream :unique-id query-identifier
			      :id-test #'equal
			      :cache-value (if (avv-query-changed-p query)
					       (avv-query-value query)
					       default)
			      :cache-test #'equal)
		;;--- Calling ACCEPT-2 directly bypasses default preprocessing,
		;;--- is that OK?
		(cond ((or default-supplied-p (avv-query-changed-p query))
		       (accept-2 avv-stream ptype
				 :prompt prompt
				 ;; If this field changed, use the value it changed to,
				 ;; making sure that the query object gets changed, too.
				 ;; We need to do this because the body of the dialog
				 ;; can change values without changing the query itself.
				 :default (if (avv-query-changed-p query)
					      (avv-query-value query)
					      (setf (avv-query-value query) default))
				 :history ptype
				 :present-p `(accept-values-choice ,query)
				 :query-identifier query))
		      ((presentation-typep (avv-query-value query) ptype)
		       ;; The programmer supplied no default, but a previous edit
		       ;; has put something in the query that we now must use as
		       ;; the default.
		       (accept-2 avv-stream ptype
				 :prompt prompt
				 :default (avv-query-value query)
				 :history ptype
				 :present-p `(accept-values-choice ,query)
				 :query-identifier query))
		      (t
		       ;; No default supplied, field has never been edited.
		       (accept-2 avv-stream ptype
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
     (continuation :initarg :continuation)
     (own-window :initform nil :initarg :own-window)
     (own-window-x-position :initform nil :initarg :x-position)
     (own-window-y-position :initform nil :initarg :y-position)
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
   (with-slots (stream own-window) *application-frame*
     (silica::scrolling ()
			(progn
			  (setq stream
			    (make-instance 'avv-stream
					   :stream
					   (setf own-window
					     (silica::realize-pane 'extended-stream-pane
								   :initial-cursor-visibility nil
								   ))))
			  own-window))))
  (:command-table accept-values)
  (:command-definer nil))

;; So the continuation can run with the proper value of *APPLICATION-FRAME*
(defvar *avv-calling-frame*)

(defun accept-values-1 (stream continuation
			&key frame-class own-window exit-boxes
			     (initially-select-query-identifier nil)
			     (resynchronize-every-pass nil) (check-overlapping t)
			     label x-position y-position)
  (incf *accept-values-tick*)
  (let ((the-own-window nil)
	(right-margin 10)
	(bottom-margin 10))
    (if own-window
	(let ((frame (make-application-frame (or frame-class 'accept-values-own-window)
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
      (clim-utils::using-resource (avv-stream avv-stream (or the-own-window stream))
				  ;;--- This should resource the AVV application frame, too
				  (let ((frame 
					 (make-application-frame (or frame-class 'accept-values)
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
  ;; This requires formatting-table and friends to return their
  ;; bodies' values properly.
  (with-accessors ((command-table frame-command-table)) frame
    (with-slots (stream continuation 
			own-window own-window-x-position own-window-y-position
			own-window-right-margin own-window-bottom-margin
			selected-item initially-select-query-identifier
			resynchronize-every-pass check-overlapping) frame
      (let ((original-view (stream-default-view stream))
	    (return-values nil)
	    (initial-query nil)
	    avv avv-record)
	(letf-globally (((stream-default-view stream) 
			 (port-dialog-view (port stream))))
		       (flet ((run-continuation (stream avv-record)
				(setf (slot-value stream 'avv-record) avv-record)
				(with-output-recording-options (stream :draw-p nil :record-p t)
				  (setq return-values (multiple-value-list
							  (let ((*application-frame* *avv-calling-frame*))
							    (funcall continuation stream))))
				  (display-exit-boxes frame stream)))
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
									:command-parser 'menu-only-command-parser
									:use-keystrokes t)))))
				    (if (and command (not (characterp command)))
					(execute-frame-command frame command)
				      (beep stream)))
				  (when (or resynchronize-every-pass (slot-value avv-record 'resynchronize))
				    ;; When the user has asked to resynchronize every pass, that
				    ;; means we should run the continuation an extra time to see
				    ;; that all visible stuff is up to date.  That's all!
				    (with-output-recording-options (stream :draw-p nil)
				      (redisplay avv stream :check-overlapping check-overlapping)))
				  (setf (slot-value avv-record 'resynchronize) nil)
				  (redisplay avv stream :check-overlapping check-overlapping))))
			 (declare (dynamic-extent #'run-continuation #'run-avv))
			 (with-simple-restart (frame-exit "Exit from the ACCEPT-VALUES dialog")
			   (setq avv
			     (updating-output (stream)
					      (setq avv-record
						(with-end-of-line-action (:allow stream)
						  (with-end-of-page-action (:allow stream)
						    (with-new-output-record (stream 'avv-output-record avv-record)
						      (run-continuation stream avv-record)))))))
			   (unwind-protect
			       (cond (own-window
				      (size-menu-appropriately own-window
							       :right-margin own-window-right-margin
							       :bottom-margin own-window-bottom-margin)
				      (multiple-value-bind (x y)
					  (stream-pointer-position* stream)
					(when (and own-window-x-position own-window-y-position)
					  (setq x own-window-x-position
						y own-window-y-position))
					(position-window-near-carefully own-window x y))
				      (window-expose own-window)
				      (with-input-focus (own-window)
					(replay avv stream)
					(run-avv)))
				     (t
				      ;; Ensure that bottom of the AVV is visible.  I think that
				      ;; this is OK even if the AVV is bigger than the viewport.
				      (move-cursor-beyond-output-record (slot-value stream 'stream) avv)
				      (stream-ensure-cursor-visible
				       stream)
				      (replay avv stream)
				      (run-avv)))
			     (unless own-window
			       (move-cursor-beyond-output-record (slot-value stream 'stream) avv))))
			 (values-list return-values)))))))

(define-presentation-type accept-values-exit-box ())

;;; Applications can create their own AVV class and specialize this method in
;;; order to get different exit boxes.

(defmethod display-exit-boxes ((frame accept-values) stream)
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
	(with-output-as-presentation (:stream stream
				      :type 'accept-values-exit-box
				      :object ':abort)
	  (write-string abort stream))
	(write-string ", " stream)
	(with-output-as-presentation (:stream stream
				      :type 'accept-values-exit-box
				      :object ':done)
	  (write-string exit stream))))))

(defmethod frame-pointer-documentation-output ((frame accept-values))
  (with-slots (own-window stream) frame
    (cond (own-window
	   #+Genera (typep own-window 'sheet-window-stream)
	   #-Genera nil)
	  (t
	   ;;--- This should ask the "frame manager" what the parent frame is
	   ;;--- and use the pointer documentation window for that.
	   (let ((parent (slot-value stream 'stream)))
	     #+Genera (typep parent 'sheet-window-stream)
	     #-Genera nil)))))


(define-presentation-type accept-values-choice ())

(define-accept-values-command com-edit-avv-choice
    ((choice 'accept-values-choice))
  (with-slots (stream selected-item) *application-frame*
    (setq selected-item choice)
    (avv-query-edit-value choice stream)))

(defun-inline accept-values-query-valid-p (query-record)
  (let ((avv-record (find-avv-record query-record)))
    (and avv-record
	 (= (slot-value avv-record 'tick) *accept-values-tick*))))

(define-presentation-to-command-translator edit-avv-choice
    (accept-values-choice com-edit-avv-choice accept-values
     :documentation "Edit this field"
     :pointer-documentation "Edit this field"
     :tester ((presentation)
	      (accept-values-query-valid-p presentation))
     :gesture :select)
    (object)
  (list object))

(defmethod avv-query-edit-value ((query avv-query) stream)
  (let ((stream (slot-value stream 'stream)))
    (with-slots (presentation-type value changed-p prompt presentation) query
      (multiple-value-bind (x y) (output-record-position* presentation)
	  (stream-set-cursor-position* stream x y))
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
				  :stream stream :prompt nil :default value)))))
	  ;; This so that the input editor's typing gets erased properly.
	  (erase-output-record record stream)
	  ;;--- Kludge until Bill can explain the whole "leave the delimiter" vs
	  ;;--- "process the delimiter" scheme to me
	  (when (read-gesture :stream stream :peek-p t :timeout 0)
	    (process-delimiter stream))
	  (setf value new-value
		changed-p t))))))

(defun map-over-avv-queries (avv-record continuation)
  (declare (dynamic-extent continuation))
  (labels ((map-queries (record)
	     (let ((unique-id (and (typep record 'updating-output-record)
				   (slot-value record 'unique-id))))
	       (when (and (listp unique-id)
			  (eq (first unique-id) :query-identifier))
		 (funcall continuation record unique-id)))
	     (map-over-output-record-elements record #'map-queries)))
    (declare (dynamic-extent #'map-queries))
    (map-queries avv-record)))

#+Genera
(define-accept-values-command (com-next-avv-choice :keystroke #\c-N)
    ()
  (with-slots (stream selected-item) *application-frame*
    (let ((avv-record (slot-value stream 'avv-record)))
      (cond ((null selected-item)
	     (map-over-avv-queries avv-record
	       #'(lambda (record unique-id)
		   (declare (ignore record))
		   (return-from com-next-avv-choice
		     (setq selected-item (find-query avv-record unique-id))))))
	    (t
	     (let* ((item (find-query avv-record (slot-value selected-item 'query-identifier)))
		    (item-record (slot-value item 'presentation))
		    (found-item nil))
	       (map-over-avv-queries avv-record
		 #'(lambda (record unique-id)
		     (if found-item
			 (return-from com-next-avv-choice
			   (setq selected-item (find-query avv-record unique-id)))
		       (when (eq record item-record)
			 (setq found-item t)))))))))))

#+Genera
(define-accept-values-command (com-previous-avv-choice :keystroke #\c-P)
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
(add-keystroke-to-command-table 'accept-values #\c-E :function
  #'(lambda (gesture numeric-argument)
      (declare (ignore gesture numeric-argument))
      (with-slots (selected-item) *application-frame*
	(if (null selected-item)
	    (beep)
	  `(com-edit-avv-choice ,selected-item))))
  :errorp nil)

(define-accept-values-command com-delete-avv-choice
    ((choice 'accept-values-choice))
  (avv-query-delete-value choice))

(define-presentation-to-command-translator delete-avv-choice
    (accept-values-choice com-delete-avv-choice accept-values
     :documentation "Remove this field"
     :pointer-documentation "Remove this field"
     :tester ((presentation)
	      (accept-values-query-valid-p presentation))
     :gesture :delete)
    (object)
  (list object))

(defmethod avv-query-delete-value ((query avv-query))
  (with-slots (presentation-type value changed-p) query
    (when (presentation-typep nil presentation-type)
      (setf value nil
	    changed-p t))))

(define-accept-values-command (com-exit-avv :keystroke #+Genera #\End #-Genera nil)
    ()
  (invoke-restart 'frame-exit))

(define-accept-values-command (com-abort-avv :keystroke #+Genera #\Abort #-Genera nil)
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


;;; AVV command buttons

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
			   resynchronize)
		     prompt &body body)))
  (default-query-stream stream accept-values-command-button)
  `(flet ((avv-command-button-body () ,@body)
	  ,@(unless (stringp prompt)
	      `((avv-command-button-prompt (,stream) ,prompt))))
     ,@(unless (stringp prompt)
	 `((declare (dynamic-extent #'avv-command-button-prompt))))
     (accept-values-command-button-1
       ,stream
       ,(if (stringp prompt) prompt '#'avv-command-button-prompt)
       #'avv-command-button-body
       ,@options)))

(defun accept-values-command-button-1
       (stream prompt continuation
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
    (with-output-as-presentation (:stream stream
				  :object (make-instance 'accept-values-command-button
							 :continuation continuation
							 :documentation documentation
							 :resynchronize resynchronize)
				  :type 'accept-values-command-button)
      (if (stringp prompt)
	  (write-string prompt stream)
	  (funcall prompt stream)))))

(define-accept-values-command com-avv-command-button
    ((button 'accept-values-command-button)
     (button-presentation 't))
  (funcall (slot-value button 'continuation))
  (when (slot-value button 'resynchronize)
    (let ((avv-record (find-avv-record button-presentation)))
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
    (setf (avv-query-value query)
	  (funcall (accept-values-multiple-choices-select-action choices)
		   (accept-values-multiple-choice-value choice)
		   (avv-query-value query)))
    (setf (avv-query-changed-p query) t)))

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
  ;;--- Unfortunatey, this makes a new object every time we go around...
  (let ((choices (make-accept-values-multiple-choices
		   :query-identifier query-identifier
		   :select-action select-action)))
    (flet ((print-choice (object stream)
	     (let* ((value (funcall key object))
		    (selected-p (funcall tester value selected-value)))
	       (updating-output (stream :unique-id object
					:cache-value selected-p)
		 (with-output-as-presentation (:type choice-type
					       :stream stream
					       :object (make-accept-values-multiple-choice
							 :choices choices
							 :value value))
		   (formatting-cell (stream)
		     (if selected-p
			 (funcall highlighting-function choice-presenter value stream)
		         (funcall choice-presenter value stream))))))))
      (declare (dynamic-extent #'print-choice))
      ;; :MAX-WIDTH makes it use as few rows as will fit in the available width.
      ;; :INTER-COLUMN-SPACING makes it not spread the choices to fill the whole width.
      ;; :NO-INITIAL-SPACING makes it not add whitespace at the start of the field.
      (formatting-item-list (stream :max-width (- (stream-text-margin stream)
						  (stream-cursor-position* stream))
				    :inter-column-spacing '(2 :character)
				    :no-initial-spacing t)
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
			      (avv-query-value query))))
      (if (member new-value (avv-query-value query))
	  (setf (avv-query-value query) (delete new-value (avv-query-value query)))
	  (push new-value (avv-query-value query))))
    (setf (avv-query-changed-p query) t)))

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

;; No need for AVV panes to deal with *ACCEPT-VALUES-TICK*, since it is
;; guaranteed that those queries will be valid at all times.
(defun accept-values-pane-displayer (frame pane
				     &key displayer
					  resynchronize-every-pass (check-overlapping t))
  (let* ((stream-and-record (and (not *frame-layout-changing-p*)
				 (gethash pane *pane-to-avv-stream-table*)))
	 (avv-stream (car stream-and-record))
	 (avv-record (cdr stream-and-record)))
    (cond (avv-stream
	   (letf-globally (((stream-default-view avv-stream) +dialog-view+))
	     (redisplay avv-record avv-stream :check-overlapping check-overlapping)
	     (when resynchronize-every-pass
	       (redisplay avv-record avv-stream :check-overlapping check-overlapping))))
	  (t
	   (accept-values-pane-displayer-1 frame pane displayer)))))

(defun accept-values-pane-displayer-1 (frame pane displayer)
  (let ((avv-stream (make-instance 'avv-stream :stream pane))
	(avv-record nil))
    (letf-globally (((stream-default-view avv-stream) +dialog-view+))
      (setq avv-record
	    (updating-output (avv-stream)
	      (with-new-output-record (avv-stream 'avv-output-record avv-record)
		(setf (slot-value avv-stream 'avv-record) avv-record)
		(funcall displayer frame avv-stream)))))
    (unless *sizing-application-frame*
      (setf (gethash pane *pane-to-avv-stream-table*)
	    (cons avv-stream avv-record)))))

(define-command (com-edit-avv-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice)
     (pane 't))
  (avv-query-edit-value choice (car (gethash pane *pane-to-avv-stream-table*))))

(define-presentation-to-command-translator edit-avv-pane-choice
    (accept-values-choice com-edit-avv-pane-choice accept-values-pane
     :documentation "Edit this field"
     :pointer-documentation "Edit this field"
     :gesture :select
     :echo nil)
    (object window)
  (list object window))

(define-command (com-delete-avv-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice))
  (avv-query-delete-value choice))

(define-presentation-to-command-translator delete-avv-pane-choice
    (accept-values-choice com-delete-avv-pane-choice accept-values-pane
     :documentation "Remove this field"
     :pointer-documentation "Remove this field"
     :gesture :delete
     :echo nil)
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
	(letf-globally (((stream-default-view avv-stream) +dialog-view+))
	  (redisplay avv-record avv-stream))))))

(define-presentation-to-command-translator avv-pane-command-button
    (accept-values-command-button com-avv-pane-command-button accept-values-pane
     :documentation document-command-button
     :pointer-documentation document-command-button
     :gesture :select
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
     :echo nil)
    (object)
  (list object))

(defmethod pane-type-options ((type (eql :accept-values)))
  `(:display-after-commands :no-clear
    :default-size :compute
    :scroll-bars nil
    :initial-cursor-visibility :off))


