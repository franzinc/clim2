;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
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

(in-package :clim-internals)

"Copyright (c) 1989, 1990 International Lisp Associates.  All rights reserved."

;;; This file implements our extended input protocol on top of the
;;; proposed "standard" protocol defined in cl-streams.lisp.


;;; This is the class that you mix in to any extended input stream
;;; implementation that you define.  It exists only to provide the
;;; extended-input-stream-p method and to hang
;;; implementation-independent code (see the STREAM-READ-GESTURE :AROUND
;;; method below).
(defclass basic-extended-input-protocol
	  (fundamental-character-input-stream)
     ()
  )

(define-protocol-p-method extended-input-stream-p basic-extended-input-protocol)


(defmethod stream-read-gesture :around ((stream basic-extended-input-protocol)
					&key timeout peek-p
					(input-wait-test *input-wait-test*)
					(input-wait-handler *input-wait-handler*)
					(pointer-button-press-handler *pointer-button-press-handler*))
					
  ;; Policy decision: we think that all output should be visible before
  ;; you do any input.  Should this call finish-output instead?
  (when (output-stream-p stream) (stream-force-output stream))
  (call-next-method stream 
		    :timeout timeout :peek-p peek-p
		    :input-wait-test input-wait-test
		    :input-wait-handler input-wait-handler
		    :pointer-button-press-handler pointer-button-press-handler)
  )


;;; Our implementation of the extended-input protocol.
(defclass input-protocol-mixin
    (basic-extended-input-protocol)
    (
     #+ignore
     (input-buffer :accessor stream-input-buffer
		   :initarg :input-buffer)
     (pointer-motion-pending :initform nil) ;--- "primary" pointer
     (text-cursor :accessor stream-text-cursor
		  :initarg :text-cursor))
  (:default-initargs :input-buffer  (make-queue)
		     :text-cursor (make-instance 'text-cursor))
  )

(defmethod stream-input-buffer ((x input-protocol-mixin))
  (silica::sheet-event-queue x))

(defmethod stream-primary-pointer ((stream input-protocol-mixin))
  (let ((port (port stream)))
    (when port
      (or (port-pointer port)
	  (setf (port-pointer port) (make-instance 'pointer))))))

(defmethod initialize-instance :after ((stream input-protocol-mixin)
				       &rest args
				       &key (initial-cursor-visibility t))
  (declare (ignore args)
	   (dynamic-extent args))
  (with-slots (text-cursor) stream
    (when text-cursor
      (setf (cursor-active text-cursor) initial-cursor-visibility)
      (setf (cursor-stream text-cursor) stream))))

(defmethod (setf stream-pointers) :after (new-pointers (stream input-protocol-mixin))
  (with-slots (primary-pointer) stream
    (unless (member primary-pointer new-pointers)
      (setf primary-pointer (first new-pointers)))))

;;; --- Cross-protocol violation here because the cursor-position is a method on
;;; --- extended-OUTPUT-protocol right now.  Secondly, this should probably be a method
;;; --- on the abstract class, anyway.

(defmethod stream-set-cursor-position* :after ((stream input-protocol-mixin)
					       x y)
  (when (minusp x) (warn "negative cursor x"))
  (let ((cursor (stream-text-cursor stream)))
    (when cursor
      (entity-set-position cursor x y))))


(defmethod initialize-menu :before (port (menu input-protocol-mixin) associated-window)
  (declare (ignore port associated-window))
  (let ((cursor (stream-text-cursor menu)))
    (when cursor
      (setf (cursor-active cursor) nil))))

(defmethod handle-repaint :after 
	   ((stream input-protocol-mixin) 
	    ignore 
	    (region clim-utils::nowhere))
  ;; Repainting nowhere, don't repaint the cursor
  )

(defvar *cursor-repaint-rectangle* (make-rectangle* 0 0 0 0))

(defmethod handle-repaint :after
	   ((stream input-protocol-mixin) medium region)
  (let ((cursor (stream-text-cursor stream))
	(viewport (pane-viewport stream)))
    (when (and cursor viewport)
      (when (and (cursor-active cursor)
		 (cursor-state cursor)
		 (cursor-focus cursor))
	(multiple-value-bind (x y)
	    (bounding-rectangle* cursor)
	  (multiple-value-bind (width height)
	      (cursor-width-and-height-pending-protocol cursor)
	    (let ((cursor-rect 
		    (make-rectangle* x y (+ x width) (+ y height))))
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


(progn 

;;; --- assumes that input-protocol-mixin will be mixed in with a sheet that
;;; has standard-input-contract
(defmethod queue-event ((stream input-protocol-mixin)
			(event key-press-event))
  (let ((char (keyboard-event-character event))
	(keysym (keyboard-event-key-name event)))
    ;; this probably wants to be STRING-CHAR-P, but that will still require some thought.
    (cond ((and char (string-char-p char)) (queue-put (stream-input-buffer stream) char))
	  ((and keysym
		(not (typep keysym 'modifier-keysym))
	   (queue-put (stream-input-buffer stream) event)))
	  (keysym
	   ;; must be a shift keysym
	   ;; must update the pointer shifts.
	   (let ((pointer (stream-primary-pointer stream)))
	     (when pointer
	       (setf (pointer-button-state pointer) 
		 (event-modifier-key-state event)))
	     )))))


(defmethod queue-event ((stream input-protocol-mixin) 
			(event key-release-event))
  ;;--- key state table?  Not unless all sheets are helping maintain it.
  (let ((keysym (keyboard-event-key-name  event)))
    (when (and keysym (typep keysym 'modifier-keysym))
      ;; update the pointer shifts.
      (let ((pointer (stream-primary-pointer stream)))
	(when pointer
	  (setf (pointer-button-state pointer) (event-input-state event)))
	)))
  nil)

;;;--- this should be in CLOS, or maybe is already under some other name?
;;; --- Just replace it with copy-event...
#+PCL
(defun copy-instance (instance)
  (cond ((string-equal pcl::*pcl-system-date* "5/22/89  Victoria Day PCL")
	 (unless (pcl::iwmc-class-p instance)
	   (error "Can't copy the instance ~S" instance))
	 (let* ((class (class-of instance))
		(copy (pcl::allocate-instance class))
		(wrapper (pcl::class-wrapper class))
		(layout (pcl::wrapper-instance-slots-layout wrapper))
		(old-slots (pcl::iwmc-class-static-slots instance))
		(new-slots (pcl::iwmc-class-static-slots copy)))
	   (dotimes (i (length layout))
	     (setf (svref new-slots i) (svref old-slots i)))
	   copy))
	;; assume "pcl for the 90's"
	(t
	 (unless (pcl::standard-class-p (class-of instance))
	   (error "Can't copy the instance ~S" instance))
	 (let* ((class (class-of instance))
		(copy (pcl::allocate-instance class))
		(wrapper (pcl::class-wrapper class))
		(layout (pcl::wrapper-instance-slots-layout wrapper))
		(old-slots (pcl::std-instance-slots instance))
		(new-slots (pcl::std-instance-slots copy)))
	   (dotimes (i (length layout))
	     (setf (svref new-slots i) (svref old-slots i)))
	   copy))))

#-(or PCL ccl-2)
(defun copy-instance (instance)
  (let* ((class (class-of instance))
	 (copy (allocate-instance class)))
    (dolist (slot (clos:class-slots class))
      (let ((name (clos:slot-definition-name slot)))
	(setf (slot-value copy name) (slot-value instance name))))
    copy))

#+ccl-2
(defun copy-instance (instance)
  (let* ((class (class-of instance))
	 (copy (allocate-instance class)))
    (dolist (slot (ccl:class-slots class))
      (let ((name (ccl:slot-definition-name slot)))
	(setf (slot-value copy name) (slot-value instance name))))
    copy))

;;; --- need to modularize stream implementation into fundamental and extended
;;; layers
;;; so that we can tell when to queue up non-characters into the stream.
;;; These don't need to set pointer-motion-pending because they synchronize through the io buffer.
(defmethod queue-event ((stream input-protocol-mixin) 
			(event pointer-press-event))
  (queue-put (stream-input-buffer stream) (copy-instance event)))

(defmethod queue-event ((stream input-protocol-mixin) 
			(event pointer-release-event))
  ;; --- What to do such that tracking-pointer can really
  ;; --- see these.
  )

;;; --- Handle "clicks" differently than press and release?
;;; so that we can tell when to queue up non-characters into the stream.
(defmethod queue-event ((stream input-protocol-mixin) 
			(event click-event))
  (queue-put (stream-input-buffer stream) (copy-instance event)))

(defmethod queue-event ((stream input-protocol-mixin) 
			(event pointer-motion-event))
  (let ((pointer (stream-primary-pointer stream)))
    (pointer-set-position pointer (pointer-event-x event) (pointer-event-y event))
    (pointer-set-native-position pointer 
				 (pointer-event-native-x event)
				 (pointer-event-native-y event))
    (setf (pointer-button-state pointer) 
      (event-modifier-key-state event))
    (setf (pointer-window pointer) stream)
    (setf (pointer-motion-pending stream pointer) t)))

(defmethod queue-event :after ((stream input-protocol-mixin)
			       (event pointer-enter-event))
  (let ((text-cursor (stream-text-cursor stream)))
    (when text-cursor (setf (cursor-focus text-cursor) t))))

(defmethod queue-event :before ((stream input-protocol-mixin)
				(event pointer-exit-event))
  ;; what about unhighlighting highlighted presentations?
  (let ((text-cursor (stream-text-cursor stream)))
    (when text-cursor (setf (cursor-focus text-cursor) nil))))

(defmethod sheet-transformation-changed :after ((stream input-protocol-mixin) &key)
  (let ((pointer (stream-primary-pointer stream)))
    (when pointer (pointer-decache pointer))))

(defmethod queue-event ((stream input-protocol-mixin) (event window-repaint-event))
  (queue-put (stream-input-buffer stream) (copy-instance event)))

) ; End of #+Silica progn


(defparameter *abort-gestures* '(:abort))
(defparameter *end-gestures* '(:end))

;;; Internal, no user will ever use this.
(defmacro with-cursor-state ((state stream) &body body)
  (let ((text-cursor (gensymbol 'text-cursor))
	(old-state (gensymbol 'old-state))
	(abort-p (gensymbol 'abort-p))
	(new-state (gensymbol 'new-state)))
    `(let* ((,text-cursor (stream-text-cursor ,stream))
	    (,old-state (cursor-state ,text-cursor))
	    (,abort-p t)
	    (,new-state ,state))
       (unwind-protect
	   (progn (when ,text-cursor
		    (cond ((eql ,old-state ,new-state))
			  (t (setf (cursor-state ,text-cursor) ,new-state)
			     (setf ,abort-p nil))))
		  ,@body)
	 (when ,text-cursor
	   (unless ,abort-p
	     (setf (cursor-state ,text-cursor) ,old-state)))))))

;;; This is what you use as a user/programmer to "turn the cursor off"
(defmacro with-cursor-visibility ((active stream) &body body)
  (let ((text-cursor (gensymbol 'text-cursor))
	(old-active (gensymbol 'old-active))
	(abort-p (gensymbol 'abort-p))
	(new-active (gensymbol 'new-active)))
    `(let* ((,text-cursor (stream-text-cursor ,stream))
	    (,old-active (cursor-active ,text-cursor))
	    (,abort-p t)
	    (,new-active ,active))
       (unwind-protect
	   (progn (when ,text-cursor
		    (cond ((eql ,old-active ,new-active))
			  (t (setf (cursor-active ,text-cursor) ,new-active)
			     (setf ,abort-p nil))))
		  ,@body)
	 (when ,text-cursor
	   (unless ,abort-p
	     (setf (cursor-active ,text-cursor) ,old-active)))))))

(defmethod stream-read-gesture ((stream input-protocol-mixin)
				&key timeout peek-p
				(input-wait-test *input-wait-test*)
				(input-wait-handler *input-wait-handler*)
				&allow-other-keys)
				
  (with-cursor-state (t stream)
    (loop
      (multiple-value-bind (input-happened flag)
	  (stream-input-wait
	   stream
	   :timeout timeout :input-wait-test input-wait-test)
	(case flag
	  (:timeout (return-from stream-read-gesture (values nil :timeout)))
	  (:input-wait-test
	   ;; only call the input-wait-handler if we didn't get a first-rate
	   ;; gesture back from stream-input-wait.
	   (when input-wait-handler
	     (funcall input-wait-handler stream)))
	  (otherwise 
	   (when input-happened
	     (with-accessors ((input-buffer stream-input-buffer)) stream
	       (let ((gesture (queue-get input-buffer)))
		 (when gesture
		   (let ((new-gesture (receive-gesture (or *outer-self* stream) gesture)))
		     (when new-gesture
		       (when peek-p (queue-unget input-buffer gesture))
		       (return-from stream-read-gesture new-gesture)))))))))))))

;;; --- Think clearly about what the class hierarchy should be here.
;;; --- Why didn't it work to specialize on basic-extended-input-protocol-mixin??

(defmethod receive-gesture ((stream input-protocol-mixin)
			    ;; any button events that make it into the stream...
			    (gesture pointer-button-event))
  ;; --- the around method may have to bind the specials back to the args
  ;; --- to be on the safe side...
  (when *pointer-button-press-handler*
    ;; This may throw or something, but otherwise we will return the gesture
    (funcall *pointer-button-press-handler* stream gesture))
  gesture)

(defmethod receive-gesture ((stream input-protocol-mixin) (gesture (eql ':resynchronize)))
  ;; signal resynchronize, naturally
  (throw 'resynchronize t))

(defmethod receive-gesture ((stream input-protocol-mixin) (gesture list))
  ;; --- We do this here rather than by throwing, because
  ;; --- we don't want you to lose the accumulated "Show Directory ..."
  ;; --- you've already typed just because a damage event came in.
  (let ((*original-stream* nil)
	(*outer-self* nil))
    (receive-list-gesture stream (first gesture) (rest gesture)))
  ;; don't return this gesture to the higher level
  nil)

(defmethod receive-list-gesture ((stream input-protocol-mixin) (type (eql 'redisplay-pane)) 
				 args)
  (redisplay-frame-pane (first args)))

(defmethod receive-list-gesture ((stream input-protocol-mixin)
				 (type (eql 'execute-frame-command)) 
				 command)
  ;; instead of executing here, we throw to the command catch tag if there is one
  (let ((command-function (first (second command))))
    ;; --- Other side of horrible kludge 
    (cond (#+ignore
	   (eql command-function 'ws::xx-do-menu-for)
	   (apply #'execute-frame-command command))
	  (t
	   (dolist (this-context *input-context*)
	     (let* ((context (first this-context))
		    (tag (second this-context)))
	       (when (presentation-subtypep 'command context)
		 (throw tag (values (second command) context)))))
	   ;; if no command context applies, then we can do no better than execute here and
	   ;; resynchronize
	   (apply #'execute-frame-command command)
	   ;; probably wants to resynchronize, right?
	   ;; should signal, though
	   (throw 'command-executed t)))))

(defmethod receive-list-gesture ((stream input-protocol-mixin) (type (eql 'stop-frame)) args)
  (apply #'stop-frame args))

(defmethod receive-gesture ((stream input-protocol-mixin) (gesture window-repaint-event))
  ;; Handle synchronous repaint request
  (silica::handle-repaint
   (silica::event-sheet gesture)
   nil
   (silica::window-event-region gesture))
  ;; don't return.
  nil)


(defmethod receive-gesture ((stream input-protocol-mixin) (gesture pointer-enter-event))
  nil)

(defmethod receive-gesture ((stream input-protocol-mixin) (gesture pointer-exit-event))
  nil)

;;; default method

(defmethod receive-gesture ((stream input-protocol-mixin) (gesture silica::event))
  (silica::process-event-locally stream gesture)
  nil)
  
(defmethod receive-gesture ((stream input-protocol-mixin) gesture)
  ;; don't translate it
  gesture
  )

;;; Ok, there's still an issue.  Just how do we want to handle this, given
;;; that it's non-trivial to turn a character back into a key-press event.
;;; This issue is still pending.

(warn "bogus")
(defparameter *abort-characters* '(#\control-z #\^z))

(defmethod receive-gesture :around ((stream input-protocol-mixin) (gesture character))
  (when (member gesture *abort-characters*)
    (abort))
  (call-next-method))

(defmethod receive-gesture :around ((stream input-protocol-mixin) (gesture key-press-event))
  #+ignore
  (when (keysym-and-shift-mask-member
	 (keyboard-event-key-name gesture)
	 (event-modifier-key-state gesture)
	 *abort-gestures*
	 (port stream))
    ;; only echo "[Abort]" when the cursor is visible.
    ;; Maybe this predicate should be done by the above
    (let ((cursor (slot-value stream 'text-cursor)))
      (when (and (cursor-active cursor)
		 (cursor-state cursor)
		 (cursor-focus cursor))
	(write-string "[Abort]" stream)
	(force-output stream)))
    (abort))
  (call-next-method))

;;; This allows us to write RECEIVE-GESTURE methods for our own new gesture types.
;;; Perhaps we should punt the list form of gesture, in favor of defining new classes
;;; and specializing that way...

;;; Now, how does the input editor work in this scheme?
;;; Can BUTTON-PRESS-EVENTS be handled using this mechanism, as well, eliminating
;;; the need for the :AROUND method on STREAM-READ-GESTURE?



;;; This function is just a convenience for the programmer, defaulting the
;;; keyword :STREAM argument to *standard-input*.  The application can call
;;; stream-read-gesture directly.

(defun read-gesture (&rest args &key (stream *standard-input*) &allow-other-keys)
  #+Genera (declare (scl:arglist &key (stream *standard-input*)
				 timeout peek-p input-wait-test input-wait-handler
				 pointer-button-press-handler))
  (declare (dynamic-extent args))
  (with-rem-keywords (keywords args '(:stream))
    (apply #'stream-read-gesture stream keywords)))

(defmethod stream-unread-gesture ((stream input-protocol-mixin) gesture)
  (with-accessors ((input-buffer stream-input-buffer)) stream
    (queue-unget input-buffer gesture)))

(defun unread-gesture (gesture &key (stream *standard-output*))
  (stream-unread-gesture stream gesture))

;;; Our extended input protocol replaces this method and others below
;;; (from FUNDAMENTAL-INPUT-STREAM) so that the input-wait and other
;;; behavior works even when the application calls standard CL stream
;;; operations.
(defmethod stream-read-char ((stream input-protocol-mixin))
  (let ((gesture nil))
    (loop
      ;; don't pass off a pointer-button-press-handler
      ;; that ignores the clicks, we want this to
      ;; be runnable inside a with-input-context.
      (setq gesture (stream-read-gesture (or *original-stream* stream)))
      (when (characterp gesture)
	(return-from stream-read-char gesture))
      (beep stream)				;??
      )))

(defmethod stream-unread-char ((stream input-protocol-mixin) character)
  (stream-unread-gesture (or *original-stream* stream) character))

;;; Again, we only need this function (as opposed to using :timeout)
;;; because the X3J13 proposal includes it explicitly.
(defmethod stream-read-char-no-hang ((stream input-protocol-mixin))
  (let ((gesture nil))
    (loop
      ;; don't pass off a pointer-button-press-handler
      ;; that ignores the clicks, we want this to
      ;; be runnable inside a with-input-context.
      (setq gesture (stream-read-gesture (or *original-stream* stream) :timeout 0))
      (when (or (null gesture) (characterp gesture))
	(return-from stream-read-char-no-hang gesture))
      )))

;;; Gray proposal says this doesn't take the optional peek-type.
(defmethod stream-peek-char ((stream input-protocol-mixin))
  (let* ( ;; --- does this want to be *outer-self*?  I dunno.
	  ;; --- me neither -- rsl
	 (the-stream (or *original-stream* stream))
	 (char (stream-read-char the-stream)))
    (prog1 char (stream-unread-gesture the-stream char))))

;;; We think that the "standard" demands that this only see characters.
;;; However, it does not want to flush any pending action elements that
;;; might precede the character, 'cause LISTEN should have no side effects.
(defmethod stream-listen ((stream input-protocol-mixin))
  (with-slots (input-buffer) stream
    (when (queue-empty-p input-buffer)
      (return-from stream-listen nil))
    ;; map over the input buffer looking for characters.  
    ;; If we find one, return it
    (let ((the-buffer input-buffer)) ; again, avoid stupid Genera CLOS compiler warnings
      (with-slots (buffer output-pointer input-pointer size) the-buffer
	(let ((i output-pointer))
	  (loop
	    (when (= i input-pointer)
	      (return))
	    (let ((gesture (aref buffer i)))
	      (when (characterp gesture)
		(return-from stream-listen t)))
	    (setq i (pointer-increment i 1 size))))))))

(defmethod stream-read-line ((stream input-protocol-mixin))
  ;; need a stack array, or maybe a permanent buffer
  (let ((result (make-array 20 
			    :element-type +string-array-element-type+
                            :fill-pointer 0
                            :adjustable t)))
    ;; Read the first char separately, since we are supposed to react differently
    ;; to EOF on the first char than we do when in the middle of a line.
    (let ((ch (stream-read-char stream)))
      ;; ---Reconcile various error cases.  When CH is NIL then that probably means
      ;; that the caller supplied error-p nil, and we may have to return the eof-val.
      ;; Of course, we aren't dealing with EOF on our window streams at all.
      (unless (eq ch :eof)
	(loop
	  ;; Process the first character
	  (cond ((eq ch :eof) (return-from stream-read-line result))
		((eql ch #\newline) (return-from stream-read-line result))
		(t (vector-push-extend ch result)))
	  (setq ch (stream-read-char stream)))))))

(defmethod stream-clear-input ((stream input-protocol-mixin))
  (with-slots (input-buffer) stream
    (queue-flush input-buffer)))

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
	    (values (when (eql flag ':input-buffer) t)
		    flag)))))))

;;; STREAM-POINTER-POSITION* method returns x,y in history coordinates.
;;; Around methods take care of this (see protocol-intermediaries.lisp)
(defmethod stream-pointer-position* ((stream input-protocol-mixin) &key (timeout 0) pointer)
  (stream-pointer-position-in-window-coordinates stream :timeout timeout :pointer pointer))

(defmethod stream-set-pointer-position* ((stream input-protocol-mixin) x y &key pointer)
  (set-stream-pointer-position-in-window-coordinates stream x y :pointer pointer))

;;; The primitive we are interested in.  The pointer is in inside-host-window coords.

(defun stream-pointer-position-in-window-coordinates (stream &key (timeout 0) pointer)
  (declare (ignore timeout))
  (let ((pointer (or pointer (stream-primary-pointer stream))))
    (pointer-position pointer)))



(defun set-stream-pointer-position-in-window-coordinates (stream x y &key pointer)
  (unless pointer (setf pointer (stream-primary-pointer stream)))
  (setf (pointer-position-changed pointer) t)
  (pointer-set-position pointer x y))



(defmethod parse-error-generic ((stream input-protocol-mixin)
				format-string &rest format-args)
  (declare (ignore format-string format-args)
	   (dynamic-extent format-args))
  (beep stream))

;;; required methods:
;;;   STREAM-EVENT-HANDLER
;;;   STREAM-RESTORE-INPUT-FOCUS
;;;   STREAM-SET-INPUT-FOCUS
