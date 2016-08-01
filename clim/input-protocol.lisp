;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 International Lisp Associates."

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

(defmethod interactive-stream-p ((stream basic-extended-input-protocol))
  nil)


;;; Our implementation of the extended-input protocol.
(defclass input-protocol-mixin
         (basic-extended-input-protocol)
    (#+++ignore (input-buffer :accessor stream-input-buffer
                              :initarg :input-buffer)
     (pointer-motion-pending :initform nil)
     (text-cursor :accessor stream-text-cursor
                  :initarg :text-cursor)
     (read-gesture-cursor-state :initarg :read-gesture-cursor-state
                                :accessor stream-read-gesture-cursor-state))
  (:default-initargs
      :text-cursor (make-instance 'standard-text-cursor)
    :read-gesture-cursor-state t))

;; Use the sheet's event queue as the input buffer.
;;--- This may not be right.  See comment in STREAM-READ-GESTURE.
(defmethod stream-input-buffer ((stream input-protocol-mixin))
  (sheet-event-queue stream))

(defmethod stream-primary-pointer ((stream input-protocol-mixin))
  (let ((port (port stream)))
    (and port (port-pointer port))))

(defmethod initialize-instance :after ((stream input-protocol-mixin)
                                       &key (initial-cursor-visibility :off))
  (let ((cursor (slot-value stream 'text-cursor)))
    (when cursor
      (setf (cursor-visibility cursor) initial-cursor-visibility)
      (setf (cursor-stream cursor) stream))))

#+(or aclpc acl86win32)
(defmethod stream-set-cursor-position (stream x y)
  (declare (ignore stream x y))
  nil)

#+(or aclpc acl86win32)
(defmethod stream-set-cursor-position-internal (stream x y)
  (declare (ignore stream x y))
  nil)

;;--- Cross-protocol violation here because CURSOR-POSITION is a method on
;;--- extended-OUTPUT-protocol right now.  Secondly, this should probably be a
;;--- method on the abstract class, anyway.
(defmethod stream-set-cursor-position :after ((stream input-protocol-mixin) x y)
  (let ((cursor (stream-text-cursor stream)))
    (when cursor
      (cursor-set-position cursor x y))))

(defmethod stream-set-cursor-position-internal :after ((stream input-protocol-mixin) x y)
  (let ((cursor (stream-text-cursor stream)))
    (when cursor
      ;; Assumes that the cursors are all off for the stream
      (cursor-set-position cursor x y t))))

#+(or aclpc acl86win32)
(defmethod initialize-menu (port menu &key label)
  (declare (ignore port menu label))
  nil)

(defmethod initialize-menu :before (port (menu input-protocol-mixin) &key label)
  (declare (ignore port label))
  (let ((cursor (stream-text-cursor menu)))
    (when cursor
      (setf (cursor-active cursor) nil))))

(defmethod repaint-sheet :after ((stream input-protocol-mixin) (region nowhere))
  ;; Repainting nowhere, don't repaint the cursor
  )

(defmethod repaint-sheet :after ((stream input-protocol-mixin) region)
  (let ((cursor (stream-text-cursor stream))
        #+ignore (viewport (pane-viewport stream)))
    ;; I don't know why the requirement was being made for the stream
    ;; to have a viewport. This was causing problems for cursor redraw
    ;; in panes with no scroll-bars.
    (when (and cursor #+ignore viewport)
      (when (and (cursor-active cursor)
                 (cursor-state cursor))
        (multiple-value-bind (x y) (cursor-position cursor)
          (multiple-value-bind (width height)
              (cursor-width-and-height-pending-protocol cursor)
            (with-bounding-rectangle* (left top right bottom) region
              (when (ltrb-overlaps-ltrb-p left top right bottom
                                          x y (+ x width) (+ y height))
                (with-sheet-medium (medium stream)
                  (with-medium-clipping-region (medium region)
                    (declare (ignore medium))
                    ;; this forces the cursor to be redrawn
                    (note-cursor-change cursor 'cursor-state nil t)))))))))))

(defmethod pointer-motion-pending ((stream input-protocol-mixin)
                                   &optional
                                   (pointer #+++ignore (stream-primary-pointer stream)))
  #-aclpc (declare (ignore pointer))                                        ;---
  (with-slots (pointer-motion-pending) stream
    (prog1 pointer-motion-pending
           (setf pointer-motion-pending nil))))

(defmethod (setf pointer-motion-pending) (new-value (stream input-protocol-mixin)
                                          &optional (pointer #+++ignore (stream-primary-pointer stream)))
  #-aclpc (declare (ignore pointer))                                        ;---
  (with-slots (pointer-motion-pending) stream
    (setf pointer-motion-pending new-value)))

;; All the QUEUE-EVENT on INPUT-PROTOCOL-MIXIN either discard the event or
;; insert a *copy* of the event into the stream's input buffer
(defmethod queue-event :after ((stream input-protocol-mixin) (event keyboard-event))
  (deallocate-event event))

(defmethod queue-event :after ((stream input-protocol-mixin) (event pointer-event))
  (deallocate-event event))

(defmethod queue-event ((stream input-protocol-mixin) (event key-press-event))
  (let ((char (keyboard-event-character event))
        (keysym (keyboard-event-key-name event))
        #+(or aclpc acl86win32) (modstate (slot-value event 'silica::modifier-state))
        temp)
    (cond ((and (member event *asynchronous-abort-gestures*
                        :test #'keyboard-event-matches-gesture-name-p)
                (setq temp (pane-frame stream))
                (setq temp (frame-top-level-process temp)))
           (process-interrupt temp #'(lambda () (process-abort-gesture stream event))))
          ((and (characterp char)
                #+(or aclpc acl86win32) (not (> modstate +shift-key+))
                (or (ordinary-char-p char)
                    (diacritic-char-p char)))
           (queue-put (stream-input-buffer stream) char))
          ((and keysym (not (typep keysym 'modifier-keysym)))
           (queue-put (stream-input-buffer stream) (copy-event event)))
          (keysym
           ;; Must be a shift keysym, the port event loop should have already
           ;; updated the port's modifier state
           nil))))

(defmethod queue-event ((stream input-protocol-mixin) (event key-release-event))
  ;; If this is a shift keysym, the port event loop should have already
  ;; updated the port's modifier state.  In any case, just ignore the event.
  nil)

(defmethod queue-event ((stream input-protocol-mixin) (event pointer-button-press-event))
  (queue-put (stream-input-buffer stream) (copy-event event)))

(defmethod queue-event ((stream input-protocol-mixin) (event pointer-button-release-event))
  (queue-put (stream-input-buffer stream) (copy-event event)))

;;;--- Handle "clicks" differently than press and release?
(defmethod queue-event ((stream input-protocol-mixin) (event pointer-click-event))
  (queue-put (stream-input-buffer stream) (copy-event event)))

(defmethod queue-event ((stream input-protocol-mixin) (event pointer-motion-event))
  (let ((pointer (stream-primary-pointer stream)))
    (setf (pointer-motion-pending stream pointer) t)))

(defmethod queue-event ((stream input-protocol-mixin) (event pointer-exit-event))
  (queue-put (stream-input-buffer stream) (copy-event event)))

#-(or aclpc acl86win32)               ;;mm: this was commented out in PC alpha
(defmethod note-sheet-transformation-changed :after
           ((stream input-protocol-mixin) &key port-did-it)
  (declare (ignore port-did-it))
  (let ((pointer (stream-primary-pointer stream)))
    (when pointer (pointer-decache pointer))))

(defmethod queue-event ((stream input-protocol-mixin) (event window-repaint-event))
  (queue-put (stream-input-buffer stream) (copy-event event)))


;(defmethod (setf window-visibility) :after (visibility (stream input-protocol-mixin))
;  (declare (ignore visibility))
;  (ensure-pointer-window stream))
;
;(defun ensure-pointer-window (window)
;  #-silica
;  (dolist (pointer (stream-pointers window))
;    (set-pointer-sheet-and-location window pointer)))

#-silica
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


(defmethod stream-read-gesture ((stream input-protocol-mixin)
                                &key timeout peek-p
                                (input-wait-test *input-wait-test*)
                                (input-wait-handler *input-wait-handler*)
                                pointer-button-press-handler)
  (declare (ignore pointer-button-press-handler))
  (let ((read-gesture-cursor-state (stream-read-gesture-cursor-state stream)))
    (with-cursor-state (stream read-gesture-cursor-state)
      (with-input-focus (stream read-gesture-cursor-state)
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
                 (let ((gesture (queue-get (stream-input-buffer stream))))
                   (when gesture
                     ;;--- What we *should* do is to reinstate a separate input
                     ;;--- buffer for the stream, then this should loop doing
                     ;;--- HANDLE-EVENT until something gets inserted into the
                     ;;--- stream's input buffer.  Then this should read and
                     ;;--- process the gesture in the input buffer.
                     ;(when (typep gesture 'window-change-event)
                     ;  (setq *wce* gesture)
                     ;  (break "look at *wce* = ~a" *wce*))
                     (let* ((sheet (and (typep gesture 'device-event)
                                        (event-sheet gesture)))
                            (new-gesture 
                             (receive-gesture
                              (if (or (null sheet) (eq sheet stream))
                                  (encapsulating-stream stream)
                                sheet)
                              gesture)))
                       (when new-gesture
                         (when peek-p
                           (queue-unget (stream-input-buffer stream) gesture))
                         (return-from stream-read-gesture new-gesture))))))))))))))

(defmethod stream-read-gesture :around
           ((stream basic-extended-input-protocol)
            &key timeout peek-p
                 (input-wait-test *input-wait-test*)
                 (input-wait-handler *input-wait-handler*)
                 (pointer-button-press-handler *pointer-button-press-handler*))
  ;; All output should be visible before you do any input.
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
          ;; call an action, it will throw to NO-TRANSLATION, and return here.
          ;; In that case, we want to loop through this again.
          (funcall pointer-button-press-handler stream gesture)
          ;; A "normal" gesture
          (return-from stream-read-gesture
            (values gesture flag)))
      ;; If we're looping when PEEK-P is T, we have to eat the gesture.
      ;;--- What if PEEK-P is T and another gesture has arrived
      ;;--- between the last call-next-method and this one? (cim)
      (call-next-method stream :timeout 0 :peek-p nil))))

;; Presentation translators have probably already run...
(defmethod receive-gesture
           ((stream input-protocol-mixin) (gesture pointer-button-press-event))
  (if *pointer-button-press-handler*
      ;; This may throw or something, but otherwise we will return NIL
      ;; which will cause the gesture to be eaten
      (progn
        (when (and *click-outside-menu-handler*
                   (output-recording-stream-p stream)
                   (not (region-contains-position-p
                          (stream-output-history stream)
                          (pointer-event-x gesture) (pointer-event-y gesture))))
          (funcall *click-outside-menu-handler*))
        (funcall *pointer-button-press-handler* stream gesture)
        nil)
      ;; No button press handler, just return the gesture
      gesture))

;; TRACKING-POINTER binds this to NIL
(defvar *discard-pointer-release-events* t)

(defmethod receive-gesture
           ((stream input-protocol-mixin) (gesture pointer-button-release-event))
  ;; In the usual case, button release events don't get queued.  However,
  ;; TRACKING-POINTER arranges to get them, so just pass them through.
  (unless *discard-pointer-release-events*
    gesture))

;(defmethod receive-gesture
;           ((stream input-protocol-mixin) (gesture (eql ':resynchronize)))
;  (throw 'resynchronize t))
;
;(defmethod receive-gesture
;           ((stream input-protocol-mixin) (gesture list))
;  (let ((*original-stream* nil))
;    (receive-list-gesture stream (first gesture) (rest gesture)))
;  ;; Don't return this gesture to the higher level
;  nil)
;
;(defmethod receive-list-gesture
;           ((stream input-protocol-mixin) (type (eql 'redisplay-pane)) args)
;  (redisplay-frame-pane (first args) (second args)))
;
;;;--- This needs to be looked at carefully!
;(defmethod receive-list-gesture
;           ((stream input-protocol-mixin) (type (eql 'execute-frame-command)) command)
;  ;; Instead of executing here, we throw to the command catch tag if there is one
;  (let ((command-function (command-name (second command))))
;    (dolist (this-context *input-context*)
;      (let* ((context (first this-context))
;             (tag (second this-context)))
;        (when (presentation-subtypep-1 'command context)
;          (throw tag (values (second command) context)))))
;    ;; If no command context applies, then we can do no better than execute here and
;    ;; resynchronize
;    (apply #'execute-frame-command command)
;    ;; probably wants to resynchronize, right?
;    ;; should signal, though
;    (throw 'command-executed t)))
;
;(defmethod receive-list-gesture
;           ((stream input-protocol-mixin) (type (eql 'stop-frame)) args)
;  (apply #'stop-frame args))

(defmethod receive-gesture
           ((stream input-protocol-mixin) (gesture window-repaint-event))
  ;; Handle synchronous repaint request
  (repaint-sheet (event-sheet gesture) (window-event-region gesture))
  ;; Don't return.
  nil)

(defmethod receive-gesture
           ((stream input-protocol-mixin) (gesture pointer-enter-event))
  nil)

(defmethod receive-gesture
           ((stream input-protocol-mixin) (gesture pointer-exit-event))
  (when (port stream)
    (unhighlight-highlighted-presentation stream))
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
         (process-abort-gesture stream gesture))))

;;;#+(or aclpc acl86win32)
;;;(eval-when (compile load eval)
;;;   ;;mm: 11Jan95 - this is defined later in  ???
;;;   (unless (ignore-errors (find-class 'interactor-pane))
;;;      (defclass interactor-pane () ()))
;;;   (unless (ignore-errors (find-class 'application-pane))
;;;      (defclass application-pane () ()))
;;;   )

(defun process-abort-gesture (stream gesture)
  (let* ((frame (pane-frame stream))
		 ;; When dealing with an application frame, prefer an
		 ;; interactor pane to any other stream
		 (stream (cond ((typep stream 'interactor-pane)
						stream)
					   ((typep *standard-input* 'interactor-pane)
						*standard-input*)
					   (t
						 (or (and frame
								  (find-frame-pane-of-type frame 'interactor-pane))
							 (and (typep stream 'application-pane)
								  stream)))))
		 (cursor (and stream (stream-text-cursor stream))))
	(when (and cursor
			   (cursor-active cursor))
	  ;; OK, this is wierd.  Try to find an input editing stream
	  ;; to do the output on, but only if that input editing stream
	  ;; encapsulates the stream we chose above
	  (let ((istream (encapsulating-stream stream)))
		(if (and (input-editing-stream-p istream)
				 (eq (encapsulating-stream-stream istream) stream))
			(progn
			  (input-editor-format istream "[Abort]")
			  (force-output istream))
			(progn
			  (write-string "[Abort]" stream)
			  (force-output stream))))))
  (error 'abort-gesture          
		 #+(or aclpc acl86win32) :format-control 
		 #+(or aclpc acl86win32) "Abort gesture seen"
		 :event gesture))

;;; This function is just a convenience for the programmer, defaulting the
;;; keyword :STREAM argument to *standard-input*.  The application can call
;;; stream-read-gesture directly.
(defun read-gesture (&rest args &key (stream *standard-input*) &allow-other-keys)
  (declare (arglist &rest args
                    &key (stream *standard-input*)
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
      (setq gesture (stream-read-gesture (encapsulating-stream stream)))
      (when (and (characterp gesture)
                 (or (ordinary-char-p gesture)
                     (diacritic-char-p gesture)))
        (return-from stream-read-char gesture))
      ;;--- Probably wrong.  It prevents the input editor from ever seeing
      ;;--- mouse clicks, for example.
      #+ignore (beep stream))))

(defmethod stream-unread-char ((stream input-protocol-mixin) character)
  (stream-unread-gesture (encapsulating-stream stream) character))

;;; Again, we only need this function (as opposed to using :timeout)
;;; because the X3J13 proposal includes it explicitly.
(defmethod stream-read-char-no-hang ((stream input-protocol-mixin))
  (let ((gesture nil))
    (loop
      ;; Don't pass off a pointer-button-press-handler that ignores clicks,
      ;; we want this to be runnable inside a WITH-INPUT-CONTEXT.
      (setq gesture (stream-read-gesture (encapsulating-stream stream) :timeout 0))
      (when (or (null gesture)
                (and (characterp gesture)
                     (or (ordinary-char-p gesture)
                         (diacritic-char-p gesture)) ))
        (return-from stream-read-char-no-hang gesture)))))

(defmethod stream-peek-char ((stream input-protocol-mixin))
  ;; stream-listen used to return the char, but Hornig says it has to return T
  (or nil ;(stream-listen stream)
      (let ((char (stream-read-char (encapsulating-stream stream))))
        (prog1 char (stream-unread-gesture (encapsulating-stream stream) char)))))

;;; We think that the "standard" demands that this only see characters.
;;; However, it does not want to flush any pending action elements that
;;; might precede the character, 'cause LISTEN should have no side effects.
(defmethod stream-listen ((stream input-protocol-mixin))
  #-silica (stream-event-handler stream :timeout 0)        ;Process pending keyboard input events
  (let ((input-buffer (stream-input-buffer stream)))
    (when (queue-empty-p input-buffer)
      (return-from stream-listen nil))
    ;; map over the input buffer looking for characters.
    ;; If we find one, return true
    (flet ((find-char (gesture)
             (when (and (characterp gesture)
                        (or (ordinary-char-p gesture)
                            (diacritic-char-p gesture)))
               (return-from stream-listen t))))
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
      (unless (eq ch *end-of-file-marker*)
        (loop
          ;; Process the character
          (cond ((or (eql ch #\Newline)
                     (eq ch *end-of-file-marker*))
                 (return-from stream-read-line
                   (evacuate-temporary-string result)))
                (t
                 ;; Be robust against weird characters
                 (if (or (ordinary-char-p ch)
                         (diacritic-char-p ch))
                     (vector-push-extend ch result)
                     (beep stream))))
          (setq ch (stream-read-char stream)))))))

(defmethod stream-clear-input ((stream input-protocol-mixin))
  (queue-flush (stream-input-buffer stream)))

#+Genera
;; The eof argument is as for the :tyi message
(defmethod stream-compatible-read-char ((stream input-protocol-mixin) &optional eof)
  (let ((char (stream-read-char stream)))
    (cond ((not (eq char *end-of-file-marker*)) char)
          (eof (error 'sys:end-of-file :stream stream :format-string eof))
          (t nil))))

#+Genera
;; The eof argument is as for the :tyi-no-hang message
(defmethod stream-compatible-read-char-no-hang ((stream input-protocol-mixin) &optional eof)
  (let ((char (stream-read-char-no-hang stream)))
    (cond ((not (eq char *end-of-file-marker*)) char)
          (eof (error 'sys:end-of-file :stream stream :format-string eof))
          (t nil))))

#+Genera
;; The eof argument is as for the :tyipeek message
(defmethod stream-compatible-peek-char ((stream input-protocol-mixin) &optional eof)
  (let ((char (stream-peek-char stream)))
    (cond ((not (eq char *end-of-file-marker*)) char)
          (eof (error 'sys:end-of-file :stream stream :format-string eof))
          (t nil))))

;;; Extended Input
(defmethod stream-input-wait ((stream input-protocol-mixin)
                              &key timeout input-wait-test)
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
    ;; story straight.  PORT-EVENT-WAIT deals with the case where the port
    ;; does not support multi-processing.
    (let* ((flag nil)
           (start-time (get-internal-real-time))
           (end-time (and timeout
                          (+ start-time
                             (* timeout internal-time-units-per-second)))))
      (flet ((waiter ()
               (unless (port stream) (return-from waiter (setq flag :eof)))
               (when (not (queue-empty-p input-buffer))
                 (setq flag :input-buffer))
               (when (and input-wait-test (funcall input-wait-test stream))
                 (setq flag :input-wait-test))
               (when (and end-time
                          (> (get-internal-real-time) end-time))
                 (setq flag :timeout))
               flag))
	#+os-threads (declare (dynamic-extent #'waiter))
        (port-event-wait (port stream) #'waiter 
          :wait-reason "Clim Input"
          :timeout timeout)
        (return-from stream-input-wait
          (values (when (eq flag ':input-buffer) t)
                  (or flag ':timeout)))))))


#+Genera
(defmethod stream-compatible-any-tyi
           ((stream input-protocol-mixin) &optional eof)
  (stream-compatible-any-tyi-1 stream nil eof))

#+Genera
(defmethod stream-compatible-any-tyi-no-hang
           ((stream input-protocol-mixin) &optional eof)
  (stream-compatible-any-tyi-1 stream 0 eof))

#+Genera
(defun stream-compatible-any-tyi-1 (stream timeout eof)
  (let ((character (stream-read-gesture (encapsulating-stream stream) :timeout timeout)))
    (cond ((null character) nil)
          ((eq character *end-of-file-marker*)
           (and eof (error "~a" eof)))
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
(defmethod stream-pointer-position ((stream input-protocol-mixin) &key (timeout 0) pointer)
  (declare (ignore timeout))
  (let ((pointer (or pointer (stream-primary-pointer stream))))
    (sheet-pointer-position stream pointer)))

(defmethod stream-set-pointer-position ((stream input-protocol-mixin) x y &key pointer)
  (declare (type real x y))
  (unless pointer
    (setf pointer (stream-primary-pointer stream)))
  ;; Make sure the pointer is on the right sheet
  (set-sheet-pointer-position stream pointer x y))

(defgeneric* (setf stream-pointer-position) (x y stream))
(defmethod* (setf stream-pointer-position) (x y (stream t))
  (stream-set-pointer-position stream x y))

(defmethod stream-set-input-focus ((stream input-protocol-mixin))
  (let ((old-focus (port-keyboard-input-focus (port stream))))
    (setf (port-keyboard-input-focus (port stream)) stream)
    old-focus))

(defmethod stream-restore-input-focus ((stream input-protocol-mixin) old-focus)
  (setf (port-keyboard-input-focus (port (graft stream))) old-focus))

(defmethod note-sheet-gain-focus ((sheet input-protocol-mixin))
  (let ((text-cursor (stream-text-cursor sheet)))
    (when text-cursor
      (setf (cursor-focus text-cursor) t))))

(defmethod note-sheet-lose-focus ((sheet input-protocol-mixin))
  (let ((text-cursor (stream-text-cursor sheet)))
    (when text-cursor
      (setf (cursor-focus text-cursor) nil))))


