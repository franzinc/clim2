;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: input-protocol.lisp,v 1.10 92/03/30 17:52:30 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; This file implements our extended input protocol on top of the
;;; proposed "standard" protocol defined in CL-STREAMS.LISP.


;;; This is the class that you mix in to any extended input stream
;;; implementation that you define.  It exists only to provide the
;;; extended-input-stream-p method and to hang
;;; implementation-independent code (see the STREAM-READ-GESTURE :AROUND
;;; method below).
(defclass basic-extended-input-protocol
	  (fundamental-character-input-stream)
     ())

(define-protocol-p-method extended-input-stream-p basic-extended-input-protocol)

(defmethod stream-read-gesture :around
	   ((stream basic-extended-input-protocol)
	    &key timeout peek-p
		 (input-wait-test *input-wait-test*)
		 (input-wait-handler *input-wait-handler*)
		 (pointer-button-press-handler *pointer-button-press-handler*))
  ;; All output should be visible before you do any input.
  ;;--- Should this call FINISH-OUTPUT instead?
  (when (output-stream-p stream)
    (force-output stream))
  (loop
    (multiple-value-bind (gesture flag)
	(call-next-method 
	  stream
	  :timeout timeout :peek-p peek-p
	  :input-wait-test input-wait-test
	  :input-wait-handler input-wait-handler
	  :pointer-button-press-handler pointer-button-press-handler)
      (if (and gesture
	       pointer-button-press-handler
	       (typep gesture 'pointer-button-press-event))
	  ;; If we call a normal translator, we'll throw to a tag outside of
	  ;; this function that was established by WITH-INPUT-CONTEXT.  If we
	  ;; call an action, it will throw to NO-TRANSLATION, and return here
	  ;; In that case, we want to loop through this again.
	  (funcall pointer-button-press-handler stream gesture)
	;; A "normal" gesture
	(return-from stream-read-gesture
	  (values gesture flag)))
      ;; If we're looping when PEEK-P is T, we have to eat the gesture.
      (call-next-method stream :timeout 0 :peek-p nil))))

;;; Our implementation of the extended-input protocol.
(defclass input-protocol-mixin
	 (basic-extended-input-protocol)
    (#+++ignore (input-buffer :accessor stream-input-buffer
			      :initarg :input-buffer)
     (pointer-motion-pending :initform nil)
     (text-cursor :accessor stream-text-cursor
		  :initarg :text-cursor))
  (:default-initargs
    :text-cursor (make-instance 'text-cursor)))

(defmethod stream-input-buffer ((x input-protocol-mixin))
  (sheet-event-queue x))

(defmethod stream-primary-pointer ((stream input-protocol-mixin))
  (let ((port (port stream)))
    (when port
      (or (port-pointer port)
	  (setf (port-pointer port) (make-instance 'standard-pointer
						   :root (window-root stream)))))))

(defmethod initialize-instance :after ((stream input-protocol-mixin)
				       &key 
				       #+Silica (initial-cursor-visibility t)
				       #-Silica (initial-cursor-visibility ':inactive))
  (with-slots (text-cursor) stream
    #+Silica
    (when text-cursor
      (setf (cursor-active text-cursor) initial-cursor-visibility)
      (setf (cursor-stream text-cursor) stream))
    #-Silica
    (when text-cursor
      (setf (cursor-stream text-cursor) stream)
      (setf (cursor-visibility text-cursor) initial-cursor-visibility))))

;;; --- Cross-protocol violation here because CURSOR-POSITION is a method on
;;; --- extended-OUTPUT-protocol right now.  Secondly, this should probably be a
;;; --- method on the abstract class, anyway.
#+Silica
(defmethod stream-set-cursor-position :after ((stream input-protocol-mixin) x y)
  (let ((cursor (stream-text-cursor stream)))
    (when cursor
      (cursor-set-position cursor x y))))

#+Silica
(defmethod stream-set-cursor-position-internal :after ((stream input-protocol-mixin) x y)
  (let ((cursor (stream-text-cursor stream)))
    (when cursor
      (cursor-set-position cursor x y))))


(defmethod initialize-menu :before (port (menu input-protocol-mixin) associated-window)
  (declare (ignore port associated-window))
  (let ((cursor (stream-text-cursor menu)))
    (when cursor
      (setf (cursor-active cursor) nil))))

#+Silica
(defmethod handle-repaint :after ((stream input-protocol-mixin) medium (region nowhere))
  (declare (ignore medium))
  ;; Repainting nowhere, don't repaint the cursor
  )

(defvar *cursor-repaint-rectangle* (make-bounding-rectangle 0 0 0 0))

(defmethod handle-repaint :after ((stream input-protocol-mixin) medium region)
  (declare (ignore medium))
  (let ((cursor (stream-text-cursor stream))
	(viewport (pane-viewport stream)))
    (when (and cursor viewport)
      (when (and (cursor-active cursor)
		 (cursor-state cursor)
		 (cursor-focus cursor))
	(multiple-value-bind (x y) (cursor-position cursor)
	  (multiple-value-bind (width height)
	      (cursor-width-and-height-pending-protocol cursor)
	    (let ((cursor-rect (make-bounding-rectangle x y (+ x width) (+ y height))))
	      (when (region-intersects-region-p region cursor-rect)
		(note-cursor-change cursor 'cursor-active t t)))))))))

(defmethod pointer-motion-pending ((stream input-protocol-mixin)
				   &optional
				   (pointer #+Ignore (stream-primary-pointer stream)))
  (declare (ignore pointer))					;---
  (with-slots (pointer-motion-pending) stream
    (prog1 pointer-motion-pending
	   (setf pointer-motion-pending nil))))

(defmethod (setf pointer-motion-pending) (new-value
					   (stream input-protocol-mixin)
					   &optional (pointer 
						       #+Ignore (stream-primary-pointer stream)))
  (declare (ignore pointer))					;---
  (with-slots (pointer-motion-pending) stream
    (setf pointer-motion-pending new-value)))

#+Silica
(progn 

;;; --- assumes that input-protocol-mixin will be mixed in with a sheet that
;;; has standard-input-contract
(defmethod queue-event ((stream input-protocol-mixin) (event key-press-event))
  (let ((char (keyboard-event-character event))
	(keysym (keyboard-event-key-name event)))
    ;;--- This probably wants to be STRING-CHAR-P...
    (cond ((and char (standard-char-p char))
	   (queue-put (stream-input-buffer stream) char))
	  ((and keysym (not (typep keysym 'modifier-keysym)))
	   (queue-put (stream-input-buffer stream) (copy-event event)))
	  (keysym
	   ;; must be a shift keysym
	   ;; must update the pointer shifts.
	   (let ((pointer (stream-primary-pointer stream)))
	     (when pointer
	       (setf (pointer-button-state pointer) 
		     (event-modifier-state event))))))))

(defmethod queue-event ((stream input-protocol-mixin) (event key-release-event))
  ;;--- key state table?  Not unless all sheets are helping maintain it.
  (let ((keysym (keyboard-event-key-name event)))
    (when (and keysym (typep keysym 'modifier-keysym))
      ;; update the pointer shifts.
      (let ((pointer (stream-primary-pointer stream)))
	(when pointer
	  (setf (pointer-button-state pointer) (event-modifier-state event))))))
  nil)

;;;--- need to modularize stream implementation into fundamental and extended
;;;--- layers so that we can tell when to queue up non-characters into the
;;;--- stream.  These don't need to set pointer-motion-pending because they
;;;--- synchronize through the io buffer.
(defmethod queue-event ((stream input-protocol-mixin) (event pointer-button-press-event))
  (queue-put (stream-input-buffer stream) (copy-event event)))

(defmethod queue-event ((stream input-protocol-mixin) (event pointer-button-release-event))
  ;; --- What to do such that tracking-pointer can really
  ;; --- see these.
  (queue-put (stream-input-buffer stream) (copy-event event)))

;;; --- Handle "clicks" differently than press and release?
;;; so that we can tell when to queue up non-characters into the stream.
(defmethod queue-event ((stream input-protocol-mixin) (event pointer-click-event))
  (queue-put (stream-input-buffer stream) (copy-event event)))

(defmethod queue-event ((stream input-protocol-mixin) (event pointer-motion-event))
  (let ((pointer (stream-primary-pointer stream)))
    (pointer-set-position pointer (pointer-event-x event) (pointer-event-y event))
    (pointer-set-native-position
      pointer 
      (pointer-event-native-x event) (pointer-event-native-y event))
    (setf (pointer-button-state pointer) 
	  (event-modifier-state event))
    (setf (pointer-window pointer) stream)
    (setf (pointer-motion-pending stream pointer) t)))

(defmethod queue-event :after ((stream input-protocol-mixin) (event pointer-enter-event))
  (let ((text-cursor (stream-text-cursor stream)))
    (when text-cursor (setf (cursor-focus text-cursor) t))))

(defmethod queue-event :before ((stream input-protocol-mixin) (event pointer-exit-event))
  ;; what about unhighlighting highlighted presentations?
  (let ((text-cursor (stream-text-cursor stream)))
    (when text-cursor (setf (cursor-focus text-cursor) nil))))

(defmethod sheet-transformation-changed :after ((stream input-protocol-mixin) &key)
  (let ((pointer (stream-primary-pointer stream)))
    (when pointer (pointer-decache pointer))))

(defmethod queue-event ((stream input-protocol-mixin) (event window-repaint-event))
  (queue-put (stream-input-buffer stream) (copy-event event)))

)	;#+Silica

(defmethod (setf window-visibility) :after (visibility (stream input-protocol-mixin))
  (declare (ignore visibility))
  (ensure-pointer-window stream))

(defmethod window-stack-on-top :after ((stream input-protocol-mixin))
  (ensure-pointer-window stream))

(defmethod window-stack-on-bottom :after ((stream input-protocol-mixin))
  (ensure-pointer-window stream))

(defun ensure-pointer-window (window)
  #-Silica
  (dolist (pointer (stream-pointers window))
    (set-pointer-window-and-location window pointer)))

#-Silica
(defmethod window-set-viewport-position :around ((stream input-protocol-mixin) new-x new-y)
  (declare (ignore new-x new-y))
  (let ((cursor (stream-text-cursor stream)))
    (if cursor
	;; Shifting the viewport may need to redraw the cursor
	(multiple-value-bind (x y)
	    (cursor-position cursor)
	  (call-next-method)
	  (cursor-set-position cursor x y))
	(call-next-method))))


;;--- Inline this?  It's called for every call to STREAM-READ-GESTURE
(defmacro with-cursor-state ((state &optional stream) &body body)
  (default-input-stream stream)
  `(flet ((with-cursor-state-body () ,@body))
     (declare (dynamic-extent #'with-cursor-state-body))
     (invoke-with-cursor-state ,stream #'with-cursor-state-body
			       ,state (stream-text-cursor ,stream))))

;;--- This might want to be a method and extract the cursor.
(defun invoke-with-cursor-state (stream continuation state text-cursor)
  (declare (dynamic-extent continuation)
	   ;; why does this take stream as an arg?  I guess
	   ;; because it might want to be a method one day.
	   (ignore stream))
  (let ((old-state (and text-cursor (cursor-state text-cursor)))
	(abort-p t))
    (unwind-protect
	(progn (when text-cursor
		 (cond ((eq old-state state))
		       #-Silica ((eq visibility ':inactive))
		       (t
			(setf (cursor-state text-cursor) state)
			(setf abort-p nil))))
	       (funcall continuation))
      (when text-cursor
	(unless abort-p
	  (setf (cursor-state text-cursor) old-state))))))

(defmethod stream-read-gesture ((stream input-protocol-mixin)
				&key timeout peek-p
				     (input-wait-test *input-wait-test*)
				     (input-wait-handler *input-wait-handler*)
				     pointer-button-press-handler)
  (declare (ignore pointer-button-press-handler))
  (with-cursor-state (:on stream)
    (loop
      (multiple-value-bind (input-happened flag)
	  (stream-input-wait
	    stream
	    :timeout timeout :input-wait-test input-wait-test)
	(case flag
	  (:timeout
	    (return-from stream-read-gesture (values nil :timeout)))
	  (:input-wait-test
	    ;; only call the input-wait-handler if we didn't get a first-rate
	    ;; gesture back from stream-input-wait.
	    (when input-wait-handler
	      (funcall input-wait-handler stream)))
	  (otherwise 
	    (when input-happened
	      #+Silica
	      (let ((gesture (queue-get (stream-input-buffer stream))))
		(when gesture
		  ;;--- What we *should* do is to reinstate a separate input
		  ;;--- buffer for the stream, then this should loop doing
		  ;;--- HANDLE-EVENT until something gets inserted into the
		  ;;--- stream's input buffer.  Then this should read and
		  ;;--- process the gesture in the input buffer.
		  (let* ((sheet (and (typep gesture 'device-event)
				     (event-sheet gesture)))
			 (new-gesture 
			   (receive-gesture
			     (if (or (null sheet) (eq sheet stream))
				 (or *original-stream* stream)
				 sheet)
			     gesture)))
		    (when new-gesture
		      (when peek-p (queue-unget (stream-input-buffer stream) gesture))
		      (return-from stream-read-gesture new-gesture)))))
	      #-Silica
	      (with-slots (input-buffer) stream
		(let ((gesture (queue-get input-buffer)))
		  (when gesture
		    (when peek-p
		      (queue-unget input-buffer gesture))
		    ;; --- Foo.  I can't find a general way to communicate from the outside
		    ;; (i.e. the global event process) that something has happened that the
		    ;; application should care about.
		    ;; Add this kludge until BSG's gesture preprocessor (or something) gets
		    ;; installed.  This could throw when there is no catch tag, but I don't
		    ;; know how to tell if we are within run-frame-top-level.
		    (cond ((and (characterp gesture)
				(member gesture *accelerator-gestures*))
			   (signal 'accelerator-gesture
				   :event gesture
				   :numeric-argument (or *accelerator-numeric-argument* 1)))
			  ((and (characterp gesture)
				(member gesture *abort-gestures*))
			   (let ((cursor (slot-value stream 'text-cursor)))
			     (when (and cursor
					(cursor-active-p cursor))
			       (write-string "[Abort]" stream)
			       (force-output stream)))
			   (error 'abort-gesture :event gesture))
			  (t (return-from stream-read-gesture gesture)))))))))))))

(defmethod receive-gesture
	   ((stream standard-encapsulating-stream) gesture)
  (receive-gesture (slot-value stream 'stream) gesture))

;; Presentation translators have probably already run...
(defmethod receive-gesture
	   ((stream input-protocol-mixin) (gesture pointer-button-press-event))
  (if *pointer-button-press-handler*
      ;; This may throw or something, but otherwise we will return the gesture
      (progn (funcall *pointer-button-press-handler* stream gesture)
	     nil)
      gesture))

(defmethod receive-gesture
	   ((stream input-protocol-mixin) (gesture (eql ':resynchronize)))
  (throw 'resynchronize t))

(defmethod receive-gesture
	   ((stream input-protocol-mixin) (gesture list))
  (let ((*original-stream* nil))
    (receive-list-gesture stream (first gesture) (rest gesture)))
  ;; Don't return this gesture to the higher level
  nil)

(defmethod receive-list-gesture
	   ((stream input-protocol-mixin) (type (eql 'redisplay-pane)) args)
  (redisplay-frame-pane (first args)))

;;--- This needs to be looked at carefully!
(defmethod receive-list-gesture
	   ((stream input-protocol-mixin) (type (eql 'execute-frame-command)) command)
  ;; Instead of executing here, we throw to the command catch tag if there is one
  (let ((command-function (command-name (second command))))
    (dolist (this-context *input-context*)
      (let* ((context (first this-context))
	     (tag (second this-context)))
	(when (presentation-subtypep 'command context)
	  (throw tag (values (second command) context)))))
    ;; If no command context applies, then we can do no better than execute here and
    ;; resynchronize
    (apply #'execute-frame-command command)
    ;; probably wants to resynchronize, right?
    ;; should signal, though
    (throw 'command-executed t)))

(defmethod receive-list-gesture
	   ((stream input-protocol-mixin) (type (eql 'stop-frame)) args)
  (apply #'stop-frame args))

(defmethod receive-gesture
	   ((stream input-protocol-mixin) (gesture window-repaint-event))
  ;; Handle synchronous repaint request
  (handle-repaint (event-sheet gesture) nil (window-event-region gesture))
  ;; don't return.
  nil)

(defmethod receive-gesture 
	   ((stream input-protocol-mixin) (gesture pointer-enter-event))
  nil)

(defmethod receive-gesture
	   ((stream input-protocol-mixin) (gesture pointer-exit-event))
  nil)

;;; default method
(defmethod receive-gesture (stream (gesture event))
  (process-event-locally stream gesture)
  nil)

;;--- We need this to deal with XM-SILICA::PRESENTATION-EVENTs.   Perhaps that
;;--- method class just needs a RECEIVE-GESTURE method instead.
(defmethod receive-gesture ((stream input-protocol-mixin) (gesture event))
  (process-event-locally stream gesture)
  nil)
  
(defmethod receive-gesture
	   ((stream input-protocol-mixin) gesture)
  ;; don't translate it
  gesture)

(defmethod receive-gesture
	   ((stream input-protocol-mixin) (gesture character))
  (process-abort-or-accelerator-gesture stream gesture)
  gesture)

(defmethod receive-gesture
	   ((stream input-protocol-mixin) (gesture key-press-event))
  (process-abort-or-accelerator-gesture stream gesture)
  gesture)

(defun process-abort-or-accelerator-gesture (stream gesture)
  (cond ((member gesture *accelerator-gestures*
		 :test #'keyboard-event-matches-gesture-name-p)
	 (signal 'accelerator-gesture
		 :event gesture
		 :numeric-argument (or *accelerator-numeric-argument* 1)))
	((member gesture *abort-gestures*
		 :test #'keyboard-event-matches-gesture-name-p)
	 (let ((cursor (slot-value stream 'text-cursor)))
	   (when (and cursor
		      (cursor-active cursor))
	     (write-string "[Abort]" stream)
	     (force-output stream)))
	 (error 'abort-gesture :event gesture))))

;;; This function is just a convenience for the programmer, defaulting the
;;; keyword :STREAM argument to *standard-input*.  The application can call
;;; stream-read-gesture directly.
(defun read-gesture (&rest args &key (stream *standard-input*) &allow-other-keys)
  (declare (arglist &key (stream *standard-input*)
			 timeout peek-p input-wait-test input-wait-handler
			 pointer-button-press-handler))
  (declare (dynamic-extent args))
  (with-keywords-removed (keywords args '(:stream))
    (apply #'stream-read-gesture stream keywords)))

(defmethod stream-unread-gesture ((stream input-protocol-mixin) gesture)
  (queue-unget (stream-input-buffer stream) gesture))

(defun unread-gesture (gesture &key (stream *standard-output*))
  (stream-unread-gesture stream gesture))

;;; Our extended input protocol replaces this method and others below
;;; (from FUNDAMENTAL-INPUT-STREAM) so that the input-wait and other
;;; behavior works even when the application calls standard CL stream
;;; operations.
(defmethod stream-read-char ((stream input-protocol-mixin))
  (let ((gesture nil))
    (loop
      ;; Don't pass off a pointer-button-press-handler that ignores clicks,
      ;; we want this to be runnable inside a WITH-INPUT-CONTEXT.
      (setq gesture (stream-read-gesture (or *original-stream* stream)))
      (when (characterp gesture)
	(return-from stream-read-char gesture))
      (beep stream))))				;??

(defmethod stream-unread-char ((stream input-protocol-mixin) character)
  (stream-unread-gesture (or *original-stream* stream) character))

;;; Again, we only need this function (as opposed to using :timeout)
;;; because the X3J13 proposal includes it explicitly.
(defmethod stream-read-char-no-hang ((stream input-protocol-mixin))
  (let ((gesture nil))
    (loop
      ;; Don't pass off a pointer-button-press-handler that ignores clicks,
      ;; we want this to be runnable inside a WITH-INPUT-CONTEXT.
      (setq gesture (stream-read-gesture (or *original-stream* stream) :timeout 0))
      (when (or (null gesture) (characterp gesture))
	(return-from stream-read-char-no-hang gesture)))))

(defmethod stream-peek-char ((stream input-protocol-mixin))
  ;; stream-listen used to return the char, but Hornig says it has to return T
  (or nil ;(stream-listen stream)
      (let ((char (stream-read-char (or *original-stream* stream))))
	(prog1 char (stream-unread-gesture (or *original-stream* stream) char)))))

;;; We think that the "standard" demands that this only see characters.
;;; However, it does not want to flush any pending action elements that
;;; might precede the character, 'cause LISTEN should have no side effects.
(defmethod stream-listen ((stream input-protocol-mixin))
  #-Silica (stream-event-handler stream :timeout 0)	;Process pending keyboard input events
  (let ((input-buffer (stream-input-buffer stream)))
    (when (queue-empty-p input-buffer)
      (return-from stream-listen nil))
    ;; map over the input buffer looking for characters.  
    ;; If we find one, return true
    (flet ((find-char (gesture)
	     (when (characterp gesture)
	       (return-from stream-listen T))))
      (declare (dynamic-extent #'find-char))
      (map-over-queue #'find-char input-buffer))
    nil))

(defmethod stream-read-line ((stream input-protocol-mixin))
  (with-temporary-string (result :length 100 :adjustable t)
    ;; Read the first char separately, since we are supposed to react differently
    ;; to EOF on the first char than we do when in the middle of a line.
    (let ((ch (stream-read-char stream)))
      ;;--- Reconcile various error cases.  When CH is NIL then that probably means
      ;; that the caller supplied error-p nil, and we may have to return the eof-val.
      ;; Of course, we aren't dealing with EOF on our window streams at all.
      (unless (eq ch :eof)
	(loop
	  ;; Process the character
	  (cond ((or (eql ch #\Newline)
		     (eq ch :eof))
		 (return-from stream-read-line
		   (evacuate-temporary-string result)))
		(t
		 ;; Be robust against weird characters
		 (if (ordinary-char-p ch)
		     (vector-push-extend ch result)
		     (beep stream))))
	  (setq ch (stream-read-char stream)))))))

(defmethod stream-clear-input ((stream input-protocol-mixin))
  (queue-flush (stream-input-buffer stream)))

#+Genera
;; The eof argument is as for the :tyi message
(defmethod stream-compatible-read-char ((stream input-protocol-mixin) &optional eof)
  (let ((char (stream-read-char stream)))
    (cond ((not (eq char ':eof)) char)
	  (eof (error 'sys:end-of-file :stream stream :format-string eof))
	  (t nil))))

#+Genera
;; The eof argument is as for the :tyi-no-hang message
(defmethod stream-compatible-read-char-no-hang ((stream input-protocol-mixin) &optional eof)
  (let ((char (stream-read-char-no-hang stream)))
    (cond ((not (eq char ':eof)) char)
	  (eof (error 'sys:end-of-file :stream stream :format-string eof))
	  (t nil))))

#+Genera
;; The eof argument is as for the :tyipeek message
(defmethod stream-compatible-peek-char ((stream input-protocol-mixin) &optional eof)
  (let ((char (stream-peek-char stream)))
    (cond ((not (eq char ':eof)) char)
	  (eof (error 'sys:end-of-file :stream stream :format-string eof))
	  (t nil))))

;;; Extended Input
(defmethod stream-input-wait ((stream input-protocol-mixin)
			      &key timeout input-wait-test)
  #-Silica
  (with-slots (input-buffer) stream
    (unless (queue-empty-p input-buffer)
      (return-from stream-input-wait t))
    ;; Non-silica version is always one-process
    (loop
      (let ((flag (stream-event-handler stream :timeout timeout
					       :input-wait-test input-wait-test)))
	(cond ((or (eq flag ':timeout) (eq flag ':input-wait-test))
	       (return-from stream-input-wait (values nil flag)))
	      ((not (queue-empty-p input-buffer))
	       (return-from stream-input-wait (values t :input-buffer)))
	      ((and input-wait-test (funcall input-wait-test stream))
	       (return-from stream-input-wait (values nil :input-wait-test)))))))
  #+Silica
  (with-accessors ((input-buffer stream-input-buffer)) stream
    ;; The assumption is that the input-wait-test function can only change
    ;; its value when an event is received and processed.  The only
    ;; commonly-desired exception is a timeout, which we provide directly.
    (cond ((not (queue-empty-p input-buffer))
	   (return-from stream-input-wait t))
	  ((and input-wait-test (funcall input-wait-test stream))
	   (return-from stream-input-wait (values nil :input-wait-test))))
    ;; Will go blocked if there are no pending events, which unfortunately puts
    ;; the input-wait-test function out of commission.  Need to get the "process-wait"
    ;; story straight.
    ;; Non-silica version is always one-process

    (let* ((flag nil)
	   (start-time (get-internal-real-time))
	   (end-time (and timeout
			  (+ start-time
			     (* timeout internal-time-units-per-second)))))
      (flet ((waiter ()
	       (when (not (queue-empty-p input-buffer))
		 (setq flag :input-buffer))
	       (when (and input-wait-test (funcall input-wait-test stream))
		 (setq flag :input-wait-test))
	       (when (and end-time
			  (> (get-internal-real-time) end-time))
		 (setq flag :timeout))
	       flag))
	(declare (dynamic-extent #'waiter))
	(port-event-wait (port stream) #'waiter :timeout timeout)
	(when flag
	  (return-from stream-input-wait
	    (values (when (eq flag ':input-buffer) t)
		    flag)))))))


#+Genera
(defmethod stream-compatible-any-tyi
	   ((stream input-protocol-mixin)
	    &optional eof)
  (stream-compatible-any-tyi-1 stream nil eof))

#+Genera
(defmethod stream-compatible-any-tyi-no-hang
	   ((stream input-protocol-mixin)
	    &optional eof)
  (stream-compatible-any-tyi-1 stream 0 eof))

#+Genera
(defun stream-compatible-any-tyi-1 (stream timeout eof)
  (let ((character (stream-read-gesture (or *original-stream* stream) :timeout timeout)))
    (cond ((null character) nil)
	  ((eq character ':eof) (and eof (error "~a" eof)))
	  ((and (characterp character)
		(let ((activation (si:input-editor-option :activation)))
		  (and activation
		       (apply (car activation) character (cdr activation)))))
	   ;; When called from WITH-CLIM-COMPATIBLE-INPUT-EDITING, turn activation
	   ;; characters into the appropriate blips to be compatible with the
	   ;; Genera input editor.
	   (si:ie-make-blip :activation character nil))
	  ((and (characterp character)
		(let ((blip-gesture (si:input-editor-option :blip-character)))
		  (and blip-gesture
		       (apply (car blip-gesture) character (cdr blip-gesture)))))
	   ;; When called from WITH-CLIM-COMPATIBLE-INPUT-EDITING, turn blip
	   ;; characters into the appropriate blips to be compatible with the
	   ;; Genera input editor.
	   (si:ie-make-blip :blip-character character nil))
	  (t character))))

#+Genera
;;; Return T so READ-CHAR will echo.
(defmethod stream-compatible-interactive ((stream input-protocol-mixin))
  t)

#+Genera
;;; Needed for Y-OR-N-P
(defmethod stream-compatible-input-wait ((stream input-protocol-mixin)
					 whostate function &rest arguments)
  (declare (dynamic-extent arguments)
	   (ignore whostate))
  (stream-input-wait stream :input-wait-test #'(lambda (stream)
						 (declare (sys:downward-function))
						 (declare (ignore stream))
						 (apply function arguments))))

#+Genera
;;; Needed for Y-OR-N-P
(defgeneric stream-compatible-notification-cell (stream)
  (:selector :notification-cell))

#+Genera
(defmethod stream-compatible-notification-cell ((stream input-protocol-mixin))
  nil)


;;; STREAM-POINTER-POSITION method returns X,Y in history coordinates.
;;; Around methods take care of this (see protocol-intermediaries.lisp)
(defmethod stream-pointer-position ((stream input-protocol-mixin) &key (timeout 0) pointer)
  (stream-pointer-position-in-window-coordinates stream :timeout timeout :pointer pointer))

(defmethod stream-set-pointer-position ((stream input-protocol-mixin) x y &key pointer)
  (set-stream-pointer-position-in-window-coordinates stream x y :pointer pointer))

#+CLIM-1-compatibility
(define-compatibility-function (stream-pointer-position* 
				stream-pointer-position)
			       (stream)
  (stream-pointer-position stream))

#+CLIM-1-compatibility
(define-compatibility-function (stream-set-pointer-position*
				stream-set-pointer-position)
			       (stream x y)
  (stream-set-pointer-position stream x y))

;;; The primitive we are interested in.  The pointer is in inside-host-window coords.
#+Silica
(defun stream-pointer-position-in-window-coordinates (stream &key (timeout 0) pointer)
  (declare (ignore timeout))
  (let ((pointer (or pointer (stream-primary-pointer stream))))
    (pointer-position pointer)))

#-Silica
(defun stream-pointer-position-in-window-coordinates (stream &key (timeout 0) pointer)
  ;; Process any pending pointer motion events.
  (stream-event-handler stream :timeout timeout)
  (let ((pointer (or pointer (stream-primary-pointer stream))))
    (multiple-value-bind (left top) (window-offset stream)
      (declare (type coordinate left top))
      (if pointer
	  (multiple-value-bind (x y) (pointer-position pointer)
	    (declare (type coordinate x y))
	    (values (- x left) (- y top)))
	  (values (coordinate 0) (coordinate 0))))))

(defun set-stream-pointer-position-in-window-coordinates (stream x y &key pointer)
  (declare (type coordinate x y))
  (unless pointer (setf pointer (stream-primary-pointer stream)))
  (setf (pointer-position-changed pointer) t)
  #+Silica
  (pointer-set-position pointer x y)
  #-Silica
  (multiple-value-bind (left top) (window-offset stream)
    (declare (type coordinate left top))
    (set-stream-pointer-in-screen-coordinates
      stream pointer (+ x left) (+ y top))))

#-Silica
(defmethod stream-note-pointer-button-press ((stream input-protocol-mixin)
					     pointer button modifier-state x y)
  (declare (ignore pointer))
  (with-slots (input-buffer) stream
    (queue-put input-buffer
	       ;; X and Y had better be fixnums
	       (make-button-press-event stream 
					(coordinate x) (coordinate y)
					button modifier-state))))

#-Silica
(defmethod stream-note-pointer-button-release ((stream input-protocol-mixin)
					       pointer button modifier-state x y)
  (declare (ignore pointer))
  (with-slots (input-buffer) stream
    (queue-put input-buffer
	       (make-button-release-event stream
					  (coordinate x) (coordinate y)
					  button modifier-state))))

#+Silica
(defmethod stream-set-input-focus ((stream input-protocol-mixin))
  (setf (port-keyboard-input-focus (port stream)) stream))

#+Silica
(defmethod stream-restore-input-focus ((stream input-protocol-mixin) old-focus)
  (setf (port-keyboard-input-focus (port stream)) old-focus))
