;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; This file implements our input editing stream protocol on top of the
;;; proposed "standard" protocol defined in CL-STREAMS.LISP.

(define-protocol-class input-editing-stream ())

(defgeneric stream-scan-pointer (stream)
  #+Genera (:selector :read-location))

(defgeneric stream-rescanning-p (stream)
  #+Genera (:selector :rescanning-p))

;;; Fake methods to keep things like COMPLETE-INPUT from blowing up on
;;; non-input-editing streams like string streams.
(defmethod stream-scan-pointer ((stream t))
  (file-position stream))

(defmethod (setf stream-scan-pointer) (position (stream t))
  (file-position stream position))

(defmethod stream-rescanning-p ((object t)) nil)

(defmethod replace-input
           ((stream t) new-input &key (start 0) end rescan buffer-start)
  (declare (ignore new-input start end buffer-start rescan))
  nil)

(defmethod presentation-replace-input
           ((stream t) object type view
            &key rescan buffer-start query-identifier for-context-type)
  (declare (ignore object type view rescan buffer-start query-identifier for-context-type))
  nil)

;;; Specific implementation of input editing protocol mixin
(defclass input-editing-stream-mixin
          (standard-encapsulating-stream input-editing-stream)
      ;; The fill-pointer of INPUT-BUFFER is the "high water" mark,
      ;; that is, it points past the last thing in the buffer.
     ((input-buffer :initform (make-array 100 :fill-pointer 0 :adjustable t)
                    :accessor input-editor-buffer)
      ;; STREAM-SCAN-POINTER  is the point at which parsing takes place.
      (scan-pointer :initform 0 :accessor stream-scan-pointer)
      ;; STREAM-INSERTION-POINTER is where the input-editing cursor is,
      ;; that is, input editing commands and insertions take place
      ;; at the insertion pointer.
      (insertion-pointer :initform 0 :accessor stream-insertion-pointer)
      ;; STREAM-RESCANNING-P is now part of the input editing stream protocol.
      ;; If T, it means that the input editor is "rescanning", that is, re-processing
      ;; input that was already typed in response to some editing command.
      (rescanning-p :accessor stream-rescanning-p :initform nil)
      (rescan-queued :initform nil)
      ;; A buffer for an activation gesture to process.  Conceptually,
      ;; the activation gesture lives at the end of the input buffer.
      (activation-gesture :initform nil)
      ;; top level command state
      (command-mode :initform nil)
      ;; State for prefixed commands, holds a command aarray
      (command-state :initform nil)
      ;; Numeric argument for input editing commands
      (numeric-argument :initform nil)
      ;; State for passing information from c-Y/c-m-Y to m-Y
      (previous-history :initform nil)
      (previous-insertion-pointer :initform nil)
      ;; For deciding whether to do kill-ring merging, etc.
      (last-command-type :initform nil)
      ;; A mark that we can set
      (mark :initform nil)))

;; The structure of *INPUT-EDITOR-COMMAND-AARRAY* is an alist that
;; associates a gesture with either an input editor command, in the case
;; of a "prefix", another alist.  Perhaps the input-editor command table
;; [or alist] should be a conceptual "slot" in the so that specific
;; implementations can add commands.  More thought may be needed.
(defvar *input-editor-command-aarray*
        (make-array 80 :fill-pointer 0 :adjustable t))

(defvar *kana-input-editor-command-aarray*
    (make-array 80 :fill-pointer 0 :adjustable t))

(defmethod initialize-input-editing-stream ((istream input-editing-stream-mixin))
  (with-slots (input-buffer scan-pointer insertion-pointer
               activation-gesture rescanning-p rescan-queued command-mode
               command-state numeric-argument mark last-command-type
               previous-history previous-insertion-pointer)
              istream
    (setf (fill-pointer input-buffer) 0
          scan-pointer 0
          insertion-pointer 0
          rescanning-p nil
          rescan-queued nil
          activation-gesture nil
          command-mode *input-editor-command-aarray*
          command-state *input-editor-command-aarray*
          numeric-argument nil
          mark nil
          last-command-type nil
          previous-history nil
          previous-insertion-pointer nil)))

#+Genera
;; This is needed because CLOS setf methods take the new value as the first argument
(defgeneric compatible-set-input-position (stream position)
  (:selector :set-location))

#+Genera
(defmethod compatible-set-input-position ((stream input-editing-stream-mixin) position)
  (setf (stream-scan-pointer stream) position))

(defmacro do-input-buffer-pieces ((input-buffer &key (start 0) end)
                                  (start-index-var end-index-var noise-string-var)
                                  &key normal noise-string)
  #+Genera (declare (zwei:indentation 2 1))
  (let ((end-var '#:end)
        (next-var '#:next))
    `(let ((,start-index-var ,start)
           (,end-var (or ,end (length ,input-buffer)))
           (,end-index-var 0)
           (,next-var nil))
       (loop
         (setq ,next-var (position-if #'noise-string-p ,input-buffer
                                      :start ,start-index-var :end ,end-var))
         (setf ,end-index-var (or ,next-var ,end-var))
         (when (/= ,start-index-var ,end-index-var)
           ;; Only do the normal code when there is something to do it to.
           ,normal)
         (cond ((null ,next-var)
                (return (values)))
               (t
                (let ((,noise-string-var (elt ,input-buffer ,next-var)))
                  ,noise-string)
                (setf ,start-index-var (1+ ,end-index-var))))))))

;; This assumes that the cursor has been set correctly???
;;--- Right now, it can't make too many assumptions about where the cursor
;;--- actually is.  Too bad.
(defmethod do-input-buffer-screen-real-estate ((istream input-editing-stream-mixin)
                                               continuation
                                               &optional start-position end-position)
  (declare (dynamic-extent continuation))
  ;; Continuation is a function which takes L T R B Baseline
  (multiple-value-bind (cursor-x cursor-y baseline height style max-x)
      (decode-stream-for-writing istream)
    ;; Cache some slot variables since we will not be writing them.
    (let ((start-x cursor-x)
          (start-y cursor-y)
          (input-buffer (slot-value istream 'input-buffer))
          (insertion-pointer (slot-value istream 'insertion-pointer))
          (stream (encapsulating-stream-stream istream)))
      (unless start-position (setf start-position insertion-pointer))
      (unless end-position (setf end-position (fill-pointer input-buffer)))
      ;; cursor-X, cursor-Y are right if the start position is the insertion position.
      (cond ((= start-position 0)
             (multiple-value-setq (cursor-x cursor-y) (start-cursor-position istream)))
            ((= start-position insertion-pointer))
            (t
             (multiple-value-setq (cursor-x cursor-y)
               (input-buffer-input-position->cursor-position istream start-position))))
      ;; OK, now we know our cursor-(X,Y) and the line height.  Now scan things from here.
      (do-input-buffer-pieces (input-buffer :start start-position :end end-position)
                              (from to noise-string)
        :normal
          (multiple-value-setq (cursor-x cursor-y height baseline)
            (do-text-screen-real-estate
              stream continuation input-buffer from to
              cursor-x cursor-y height baseline style max-x))
        :noise-string
          (let ((style (merge-text-styles (noise-string-text-style noise-string) style)))
            (multiple-value-setq (cursor-x cursor-y height baseline)
              (do-text-screen-real-estate
                stream continuation (noise-string-display-string noise-string) 0 nil
                cursor-x cursor-y height baseline style max-x))))
      (values cursor-x cursor-y start-x start-y height baseline))))

(defmethod prompt-for-accept ((istream input-editing-stream-mixin) type (view view)
                              &key (prompt t) (prompt-mode ':normal) (display-default prompt)
                                   (default nil default-supplied-p)
                                   (default-type type)
                                   query-identifier
                              &allow-other-keys)
  (unless default-supplied-p
    (setq display-default nil))
  (when (or prompt display-default)
    (with-slots (input-buffer scan-pointer insertion-pointer) istream
      (let ((next-char (and (< scan-pointer insertion-pointer)
                            (aref input-buffer scan-pointer)))
            (noise-string nil))
        (cond ((and (typep next-char 'noise-string)
                    (eql (noise-string-unique-id next-char) query-identifier))
               (setq noise-string next-char)        ;---so what do we do with NOISE-STRING?
               (incf scan-pointer)
               (noise-string-unique-id next-char))
              (t (when (< scan-pointer insertion-pointer)
                   #+++ignore (error "Trying to make a noise string while rescanning")
                   (return-from prompt-for-accept (values)))
                 (setq noise-string
                       (make-noise-string
                         :display-string
                           (concatenate 'string
                             (if (eq prompt-mode ':normal) "(" "")
                             (cond ((eq prompt t)
                                    (describe-presentation-type type nil nil))
                                   ((not (eq prompt nil))
                                    prompt))
                             (if (and display-default prompt) " [" "")
                             (if display-default "default " "")
                             (if display-default (present-to-string default default-type) "")
                             (if (and display-default prompt) "]" "")
                             (if (eq prompt-mode ':normal) ") " ""))
                         :unique-id query-identifier))
                 (vector-push-extend noise-string input-buffer)
                 (incf scan-pointer)
                 (incf insertion-pointer)
                 (with-text-style (istream (noise-string-text-style noise-string))
                   (write-string (noise-string-display-string noise-string) istream))
                 (noise-string-unique-id noise-string)))))))

(defmethod stream-accept ((istream input-editing-stream-mixin) type &rest accept-args
                          &key view &allow-other-keys)
  (declare (dynamic-extent accept-args))
  (with-slots (input-buffer scan-pointer insertion-pointer
               previous-history previous-insertion-pointer) istream
    (let ((query-identifier
            (apply #'prompt-for-accept
                   (encapsulating-stream istream) type view accept-args)))
      (unless (stream-rescanning-p istream)
        ;; If we're not rescanning, reset these so that m-Y will not work
        ;; until the user types c-Y or c-m-Y.
        (setq previous-history nil
              previous-insertion-pointer nil))
      (let ((next-char (and (< scan-pointer insertion-pointer)
                            (aref input-buffer scan-pointer))))
        (cond ((and (typep next-char 'accept-result)
                    (eql (noise-string-unique-id next-char) query-identifier))
               (incf scan-pointer)
               (values (accept-result-presentation-object next-char)
                       (accept-result-presentation-type next-char)))
              (t (apply #'accept-1 (encapsulating-stream istream) type
                                   :query-identifier query-identifier
                                   accept-args)))))))

(defmethod input-buffer-input-position->cursor-position ((istream input-editing-stream-mixin)
                                                         &optional position)
    (multiple-value-bind (cursor-x cursor-y)
        (flet ((ignore (&rest args)
                 (declare (ignore args))))
          (declare (dynamic-extent #'ignore))
          (do-input-buffer-screen-real-estate istream #'ignore 0 position))
      ;; Return only the first two values.
      (values cursor-x cursor-y)))


(defmethod ie-set-cursor-position ((istream input-editing-stream-mixin)
                                   x-pos y-pos)
  (with-slots (input-buffer stream insertion-pointer) istream
    (let ((cursor (stream-text-cursor stream)))
      ;; I am not sure this is right.  The insertion pointer may be
      ;; looking at something which is not a character (a noise string
      ;; for instance).  In that case we need to grab a width from
      ;; *somewhere*, so we chose #\Space because it's there.
      ;; -- tfb@cley.com (Tim Bradshaw) Mon Mar  6 17:30:05 2000
      (when cursor
        (setf (slot-value cursor 'width)
          (stream-character-width stream
                                  (if (and (< insertion-pointer
					      (fill-pointer input-buffer))
					   (characterp (aref input-buffer
                                            insertion-pointer)))
				      (aref input-buffer
                                            insertion-pointer)
				      #\space)))
        (stream-set-cursor-position stream x-pos y-pos)))))

(defmethod redraw-input-buffer ((istream input-editing-stream-mixin)
                                &optional (start-position 0))
  (with-slots (input-buffer stream insertion-pointer) istream
    (multiple-value-bind (x-pos y-pos)
        (input-buffer-input-position->cursor-position istream start-position)
      (ie-set-cursor-position istream x-pos y-pos))
    (macrolet ((do-part (from &optional to)
                 `(do-input-buffer-pieces (input-buffer :start ,from :end ,to)
                                          (start-index end-index noise-string)
                   :normal
                     (with-temporary-substring (buf input-buffer start-index end-index)
                       (replace buf input-buffer :start2 start-index :end2 end-index)
                       (write-string buf stream))
                   :noise-string
                     (with-text-style (stream (noise-string-text-style noise-string))
                       (write-string (noise-string-display-string noise-string)
                                     stream)))))
      ;;--- If I were smart, I'd probably be able to figure out how to
      ;;--- do this more effectively.  I.e., the insertion pointer would
      ;;--- already be at the right place.  However, that will require a
      ;;--- more detailed analysis of all callers.  The MIN below is to catch
      ;;--- the case where the insertion pointer is still at 5 but a character
      ;;--- has been rubbed out.  That's what RUBOUT-USING-PREDICATE does: this
      ;;--- function can be called (at present) while the input buffer is in an
      ;;--- inconsistent state.
      (let ((ip (min insertion-pointer (fill-pointer input-buffer))))
        (do-part start-position ip)
        ;; Remember where the cursor goes (at the insertion pointer!)
        (multiple-value-bind (x-pos y-pos) (stream-cursor-position istream)
          (do-part ip)
          ;; And put it there.
          (ie-set-cursor-position istream x-pos y-pos))))
    (force-output stream)))

;;--- Too bad this never gets called...
(defmethod window-refresh :after ((istream input-editing-stream-mixin))
  (redraw-input-buffer istream))

#||
(defmethod push-input-buffer ((istream input-editing-stream-mixin))
  (with-slots (input-buffer scan-pointer insertion-pointer) istream
    (multiple-value-prog1 (values input-buffer scan-pointer insertion-pointer)
                          (setf input-buffer (make-array 100 :fill-pointer 0 :adjustable t)
                                scan-pointer 0
                                insertion-pointer 0))))

(defmethod pop-input-buffer ((istream input-editing-stream-mixin) old-ib old-scan old-ip)
  (with-slots (input-buffer scan-pointer insertion-pointer) istream
    (setf input-buffer old-ib
          scan-pointer old-scan
          insertion-pointer old-ip)))

(defmacro saving-input-buffer ((&optional stream) &body body)
  (default-output-stream stream)
  `(flet ((saving-input-buffer-body (,stream) ,@body))
     (declare (dynamic-extent #'saving-input-buffer))
     (invoke-saving-input-buffer ,stream #'saving-input-buffer-body)))

(defmethod invoke-saving-input-buffer ((istream input-editing-stream-mixin) continuation)
  (let (ib sp ip)
    (unwind-protect
        (progn
          (multiple-value-setq (ib sp ip) (push-input-buffer istream))
          (funcall continuation istream))
      (when ib
        (pop-input-buffer istream ib sp ip)))))
||#

(defmethod reset-scan-pointer ((istream input-editing-stream-mixin) &optional (sp 0))
  (with-slots (scan-pointer rescanning-p) istream
    (setf rescanning-p t)
    (setf scan-pointer sp))
  (values))

(defun immediate-rescan (istream)
  (declare (ignore istream))
  (throw 'rescan (values)))

(defmethod queue-rescan ((istream input-editing-stream-mixin) &optional (rescan-type t))
  (with-slots (rescan-queued) istream
    (setf rescan-queued rescan-type)))

;; Do a rescan if one is pending.
;; If it's an "activation" rescan, that means that the user typed an activation
;; character at some point, so this rescan should terminate all input.  That's
;; why we set the insertion pointer to the end of the buffer...
;; In the cases where the program is maintaining another input buffer besides
;; the input editor's buffer, that buffer can get out of synch (for instance,
;; it could appear to be empty) because the user yanked something in and then
;; immediately hit <End>.  In that case, the yank commands have left a rescan
;; pending which we can take care of now.  In general, any piece of code that
;; is maintaining an input buffer separate from the input editor's buffer should
;; call RESCAN-IF-NECESSARY before parsing any input.
(defmethod rescan-if-necessary ((istream input-editing-stream-mixin)
                                &optional inhibit-activation)
  (with-slots (rescan-queued input-buffer insertion-pointer) istream
    (when rescan-queued
      (when (and (not inhibit-activation)
                 (eql rescan-queued ':activation))
        (setq insertion-pointer (fill-pointer input-buffer)))
      (setf rescan-queued nil)
      (throw 'rescan (values)))))

(defun shift-buffer-portion (buffer from-index to-index)
  (declare (type fixnum from-index to-index))
  (let (#+(or Genera Minima) (buffer buffer)
        (length (fill-pointer buffer)))
    (declare (type vector buffer) (type fixnum length))
    (cond ((< from-index to-index)
           ;; Extending the buffer to the right
           (let* ((n-places (the fixnum (- to-index from-index)))
                  (old-size (the fixnum (array-dimension buffer 0)))
                  (new-size (the fixnum (+ length n-places))))
             (when (> new-size old-size)
               (setq buffer (adjust-array buffer new-size)))
             (incf (fill-pointer buffer) n-places)
             (let* ((n-words (the fixnum (- length from-index)))
                    (to (the fixnum (+ to-index n-words)))
                    (from (the fixnum (+ from-index n-words))))
               (repeat n-words
                 (decf to)
                 (decf from)
                 (setf (aref buffer to) (aref buffer from))))))
          ((> from-index to-index)
           ;; Shrinking the buffer to the left
           (let ((n-places (the fixnum (- from-index to-index))))
             (decf (fill-pointer buffer) n-places)
             (let ((n-words (the fixnum (- length from-index))))
               (repeat n-words
                 (setf (aref buffer to-index) (aref buffer from-index))
                 (incf to-index)
                 (incf from-index))))))
    buffer))

(defmethod stream-unread-gesture ((istream input-editing-stream-mixin) gesture)
  (with-slots (input-buffer scan-pointer activation-gesture stream) istream
    (when (characterp gesture)                ;no general keyboard events here...
      ;; If it's an activation gesture, store it in the input editor's
      ;; slot rather than sending it back to the underlying stream.
      (when (and (activation-gesture-p gesture)
                 (not (delimiter-gesture-p gesture)))
        (when activation-gesture
          (cerror "Proceed anyway"
                  "Unexpected activation gesture ~S found in the input editor"
                  activation-gesture))
        (setq activation-gesture gesture)
        (return-from stream-unread-gesture nil))
      (let ((tsp scan-pointer))
        (loop
          (when (zerop tsp) (return))
          (decf tsp)
          (let ((prev-gesture (aref input-buffer tsp)))
            (cond ((characterp prev-gesture)
                   (cond ((char= prev-gesture gesture)
                          (setf scan-pointer tsp)
                          (return-from stream-unread-gesture nil))
                         (t (return))))
                  (t (return)))))))
    ;; If it didn't come from us, it must have come directly
    ;; from the underlying stream, which can complain if it wants to.
    (stream-unread-gesture stream gesture)))

(defmethod stream-read-gesture ((istream input-editing-stream-mixin)
                                &key timeout peek-p
                                     (input-wait-test *input-wait-test*)
                                     (input-wait-handler *input-wait-handler*)
                                     (pointer-button-press-handler
                                       *pointer-button-press-handler*))
  (rescan-if-necessary istream t)
  (with-slots (stream input-buffer scan-pointer insertion-pointer
               activation-gesture rescanning-p
               numeric-argument previous-history) istream
    (declare (type fixnum scan-pointer insertion-pointer))
    (loop        ;until a real gesture is read or we throw out
      ;; First look in the input-buffer of the input-editor and see
      ;; if there's something there.
      (loop
        (cond ((= scan-pointer insertion-pointer)
               (return))
              ((< scan-pointer insertion-pointer)
               (let ((gesture (aref input-buffer scan-pointer)))
                 (cond ((characterp gesture)
                        (unless peek-p (incf scan-pointer))
                        (return-from stream-read-gesture (values gesture)))
                       (t (incf scan-pointer)))))
              (t (return)
                 ;; If the scan pointer is greater than the insertion pointer
                 ;; then we'll definitely have to rescan if a character is typed
                 ;; at this point.
                 )))

      ;; If we're about to go to the stream but there's an activation
      ;; character buffered, return it instead.
      (when activation-gesture
        (return-from stream-read-gesture
          (prog1 activation-gesture
                 (unless peek-p
                   (setf activation-gesture nil)))))

      ;;--- This is presumably much slower than necessary.
      ;;--- Perhaps there is a better way to keep track of where the cursor should be.
      (multiple-value-bind (x-pos y-pos)
          (input-buffer-input-position->cursor-position istream insertion-pointer)
        (declare (type coordinate x-pos y-pos))
        (multiple-value-bind (cx cy)
            (stream-cursor-position stream)
          (declare (type coordinate cx cy))
          ;; Don't set the cursor position if it's already right.
          ;; This prevents the input editor from scrolling the window after
          ;; the user has scrolled it back until the cursor position actually changes.
          (unless (and (= cx x-pos) (= cy y-pos))
            (ie-set-cursor-position istream x-pos y-pos))))

      (setf rescanning-p nil)
      (multiple-value-bind (thing type)
          (let* ((*input-buffer-empty* (zerop (fill-pointer input-buffer)))
                 (*accelerator-numeric-argument* (or numeric-argument 1))
                 (*accelerator-gestures*
                   ;; If there's anything in the input buffer, disallow accelerators
                   (and *input-buffer-empty* *accelerator-gestures*)))
            (stream-read-gesture stream
                                 :timeout timeout :peek-p peek-p
                                 :input-wait-test input-wait-test
                                 :input-wait-handler input-wait-handler
                                 :pointer-button-press-handler
                                 pointer-button-press-handler))
        (cond ((eq type ':timeout)
               #+(or aclpc acl86win32) nil
               #-(or aclpc acl86win32)
               (return-from stream-read-gesture
                 (values thing type)))
              (peek-p
               (return-from stream-read-gesture
                 (values thing type)))
              (t
               (multiple-value-bind (new-thing new-type)
                   ;; This can throw out in order to do rescans.
                   ;; NEW-THING is a character, a presentation "blip", or NIL
                   (stream-process-gesture istream thing type)
                 (when (and (characterp new-thing)
                            ;; Don't put things in the buffer that we can't echo later
                            (or (ordinary-char-p new-thing)
                                (diacritic-char-p new-thing))
                            (not (activation-gesture-p new-thing)))
                   ;; If we are inserting multiple copies of this character
                   ;; we'll need to do a rescan in order to keep user-level
                   ;; buffers up-to-date
                   (let* ((count (or numeric-argument 1))
                          (immediate-rescan (> count 1)))
                     (dotimes (i count)
                       (cond ((< insertion-pointer (fill-pointer input-buffer))
                              (when (= i (1- count))        ;optimization
                                (erase-input-buffer istream insertion-pointer))
                              (setq input-buffer (shift-buffer-portion
                                                   input-buffer
                                                   insertion-pointer (1+ insertion-pointer)))
                              (setf (aref input-buffer insertion-pointer) new-thing)
                              (when (= i (1- count))        ;optimization
                                (redraw-input-buffer istream insertion-pointer))
                              (let ((rescan (> scan-pointer insertion-pointer)))
                                (incf insertion-pointer)
                                (if rescan
                                    (setq immediate-rescan t)
                                    (setf scan-pointer insertion-pointer))))
                             (t (vector-push-extend new-thing input-buffer)
                                (incf scan-pointer)
                                (incf insertion-pointer)
                                (write-char new-thing istream))))
                     (setq numeric-argument nil
                           previous-history nil)
                     (if immediate-rescan
                         (immediate-rescan istream)
                         (rescan-if-necessary istream t))
                     (return-from stream-read-gesture
                       (values new-thing new-type))))
                 (when new-thing
                   (setq numeric-argument nil
                         previous-history nil)
                   (cond ((activation-gesture-p new-thing)
                          ;; If we got an activation gesture, we must first finish
                          ;; scanning the input line, moving the insertion pointer
                          ;; to the end and finishing rescanning.  Only then can we
                          ;; return the activation gesture.
                          (cond ((= insertion-pointer (fill-pointer input-buffer))
                                 (return-from stream-read-gesture
                                   (values new-thing new-type)))
                                (t (setf insertion-pointer (fill-pointer input-buffer))
                                   (setf activation-gesture new-thing))))
                         ((or (not (characterp new-thing))
                              (ordinary-char-p new-thing)
                              (diacritic-char-p new-thing))
                          ;; There might be some queued up rescans from destructive
                          ;; input editing commands, so take care of them now
                          (rescan-if-necessary istream t)
                          (return-from stream-read-gesture
                            (values new-thing new-type)))
                         (t
                           ;; Some input editing doesn't throw, and should not
                           ;; cause us to return just yet, since IE commands don't
                           ;; count as real gestures.
                           (beep istream)))))))))))

;; Move the cursor forward or backward in an input buffer until PREDICATE
;; returns true.  PREDICATE has to be prepared to interact with ACCEPT-RESULTs
;; because to the user they behave as big characters.  NOISE-STRINGs, on the
;; other hand, are invisible.
;; The second value returned from the predicate controls whether the last
;; character seen (that is the character for which the predicate returns T)
;; is included in the region.  For instance, rubout-s-expression will use a
;; predicate which succeeds for whitespace if no parens have been seen and
;; the result does not want to include the space, but which succeeds on a
;; balancing open-paren and the result does want to include the paren.
(defun forward-or-backward (input-buffer start-position reverse-p predicate)
  (declare (values new-position))
  (let ((position start-position)
        (adjustment (if reverse-p -1 +1))
        (limit (if reverse-p 0 (fill-pointer input-buffer))))
    (unless (= position limit)                        ;return nil if nowhere to go
      (when reverse-p (incf position adjustment))
      (loop
        (let ((thing (aref input-buffer position)))
          (when (or (characterp thing)
                    (noise-string-p thing))
            (multiple-value-bind (ok dont-include-me) (funcall predicate thing)
              (when ok
                (return (if (eq dont-include-me reverse-p)
                            (1+ position)
                            position))))))
        (when (= position limit)
          ;; Necessary for the case where the forward part of the token
          ;; token search bumps into the fill pointer.
          ;;--- This makes me nervous because it compensates for the pointer
          ;;--- adjusting the callers do.
          (return (if reverse-p position (1+ position))))
        (incf position adjustment)))))

;; KILL-RING should be NIL, T, or :MERGE
(defun ie-kill (stream input-buffer kill-ring start end &optional reverse no-kill)
  (when (< end start) (rotatef start end))
  (when kill-ring
    (let* ((top (and (eq kill-ring ':merge)
                     (eq *kill-ring-application* *application-frame*)
                     (history-top-element *kill-ring*)))
           (length (if top (length top) 0)))
      (setq *kill-ring-application* *application-frame*)
      (do-input-buffer-pieces (input-buffer :start start :end end)
                              (start end noise-string)
        :normal (incf length (- end start))
        :noise-string (when (typep noise-string 'accept-result)
                        (incf length (length (noise-string-display-string noise-string)))))
      (let ((new-top (make-string length))
            (index 0))
        (when top
          (cond (reverse
                 ;; Even when we're deleting backwards, we want to merge the
                 ;; kills so they come out in the original order
                 (replace new-top top
                          :start1 (- length (length top)))
                 (setq index 0))
                (t
                 (replace new-top top)
                 (setq index (length top)))))
        (do-input-buffer-pieces (input-buffer :start start :end end)
                                (start end noise-string)
          :normal (replace new-top input-buffer
                           :start1 index :end1 (incf index (- end start))
                           :start2 start :end2 end)
          :noise-string (when (typep noise-string 'accept-result)
                          (let ((string (noise-string-display-string noise-string)))
                            (replace new-top string
                                     :start1 index :end1 (incf index (length string))))))
        (cond (top
               (setf (history-top-element *kill-ring*) new-top)
               #+Genera (genera-kill-ring-save new-top t))
              (t
               (push-history-element *kill-ring* new-top)
               #+Genera (genera-kill-ring-save new-top nil))))))
  (unless no-kill                                ;skip if kill ring only
    ;; Erase what used to be there, side effect the input buffer, then redraw it
    (erase-input-buffer stream start)
    (if end
        (shift-buffer-portion input-buffer end start)
        (setf (fill-pointer input-buffer) start))
    ;; The insertion pointer started out at either start or end, they're the same now
    (setf (stream-insertion-pointer stream) start)
    ;; Make sure the scan pointer doesn't point past the insertion pointer
    (minf (stream-scan-pointer stream) (stream-insertion-pointer stream))
    (redraw-input-buffer stream)
    ;; This can be called in a loop, so reflect the kill operation now
    (setf (slot-value stream 'last-command-type) 'kill)
    ;; If the buffer is now empty, rescan immediately so that the state
    ;; of the input editor gets reinitialized
    (when (zerop (fill-pointer input-buffer))
      (immediate-rescan stream))))

(defmethod remove-activation-gesture ((istream input-editing-stream-mixin))
  (with-slots (stream input-buffer insertion-pointer activation-gesture) istream
    (when activation-gesture
      (setf activation-gesture nil)
      (return-from remove-activation-gesture))
    (let ((pointer (fill-pointer input-buffer)))
      (when (plusp pointer)
        (decf pointer)
        (let ((character (aref input-buffer pointer)))
          (when (activation-gesture-p character)
            (setf (fill-pointer input-buffer) pointer)
            (minf insertion-pointer (fill-pointer input-buffer))
            (return-from remove-activation-gesture)))))
    (let ((char (stream-read-gesture stream :timeout 0 :peek-p t)))
      (when (and char (activation-gesture-p char))
        ;; throw it away
        (stream-read-gesture stream)))))

;; Replace from buffer-start below scan-pointer with new-input[start..end]
;; Returns the new insertion pointer as its value.
(defmethod replace-input ((istream input-editing-stream-mixin) new-input
                          &key (start 0) end rescan
                               (buffer-start (stream-scan-pointer istream)))
  (declare (type fixnum start buffer-start))
  (let ((rescan-p nil))
    (with-slots (input-buffer scan-pointer insertion-pointer) istream
      (declare (type fixnum scan-pointer insertion-pointer))
      (let* ((the-end (the fixnum (or end (length new-input))))
             (nchars (the fixnum (- the-end start)))
             (buffer-total-length (the fixnum (array-dimension input-buffer 0)))
             (old-fp (the fixnum (fill-pointer input-buffer)))
             (n-pending-chars (the fixnum (- old-fp scan-pointer))))
        (assert (>= nchars 0))
        (let ((new-length (the fixnum (+ buffer-start nchars n-pending-chars))))
          (when (> new-length buffer-total-length)
            (setf input-buffer (adjust-array input-buffer (floor (* new-length 3) 2))))
          (let ((m (mismatch new-input input-buffer
                             :start2 buffer-start :end2 old-fp
                             :start1 start :end1 the-end)))
            ;; We only want to do any of this work if the new stuff is different
            ;; from the old stuff.
            ;; We really only want to erase the input buffer starting at the point
            ;; of difference, which is M.
            (when m
              (decf m start)
              (erase-input-buffer istream (the fixnum (+ buffer-start m)))
              (unless (zerop n-pending-chars)
                (unless (= new-length old-fp)
                  ;; Increase fill-pointer to defeat bounds checking in replace
                  (setf (fill-pointer input-buffer) (max new-length old-fp)))
                (replace input-buffer input-buffer
                         :start1 (the fixnum (- new-length n-pending-chars))
                         :start2 scan-pointer
                         :end2 old-fp))
              (unless (= new-length old-fp)
                (setf (fill-pointer input-buffer) new-length))
              (replace input-buffer new-input :start1 buffer-start
                                              :start2 start :end2 the-end)
              ;; Keep the insertion pointer at the same relative place in the text
              (when (>= insertion-pointer buffer-start)
                (if (> insertion-pointer scan-pointer)
                    (incf insertion-pointer
                          (the fixnum (- nchars (- scan-pointer buffer-start))))
                    (setf insertion-pointer
                          (the fixnum (+ buffer-start nchars)))))
              ;; Advance over stuff we insert, but don't back up to it
              (setq scan-pointer (the fixnum (+ buffer-start nchars)))
              (redraw-input-buffer istream (the fixnum (+ buffer-start m)))
              (setq rescan-p t)))))
      ;; This is so that we'll leave the context of ACCEPT *this* time, rather
      ;; than next time, preserving our activation-p-ness.
      (when (and rescan-p rescan)
        (queue-rescan istream))
      insertion-pointer)))

;;--- What does it mean when VIEW is not a textual view?
;; Returns the new insertion pointer as its value.
(defmethod presentation-replace-input ((istream input-editing-stream-mixin) object type view
                                       &key (buffer-start (stream-scan-pointer istream))
                                            rescan query-identifier for-context-type)
  (setq for-context-type (or for-context-type type))
  (let ((input-string
          (handler-case
              (let ((*print-readably* t))
                (present-to-string object type
                                   :acceptably t :view view
                                   :for-context-type for-context-type))
            (error ()
              ;; Can't present it the normal way, so make a "blip" to hold it
              (let ((blip (make-accept-result :unique-id query-identifier
                                              :presentation-type type
                                              :presentation-object object)))
                ;; Set the printed representation of that blip as best we can
                (setf (noise-string-display-string blip)
                      (handler-case
                        (let ((*print-readably* nil))
                          (present-to-string object type
                                             :acceptably nil :view view
                                             :for-context-type for-context-type))
                        (error ()
                          (princ-to-string object))))
                (vector blip)
                #+ignore-since-this-seems-to-be-wrong
                (with-slots (input-buffer scan-pointer insertion-pointer) istream
                  (vector-push-extend blip input-buffer)
                  (incf insertion-pointer)
                  (incf scan-pointer)
                  (with-text-style (istream (noise-string-text-style blip))
                    (write-string (noise-string-display-string blip) istream))
                  (return-from presentation-replace-input insertion-pointer)))))))
    (replace-input istream input-string
                   :buffer-start buffer-start :rescan rescan)))

;;; How do we implement an input editor?
;;; I think it wants to be a new stream which encapsulates the old stream.
;;; However, we've managed to survive so far without having to address these issues.
;;; The basic idea is that (READ *STANDARD-INPUT*) should support a rubout-handler
;;; interaction style, or at least the following should:
;;; (WITH-INPUT-EDITING (*STANDARD-INPUT*) (READ *STANDARD-INPUT*))

;;; If we use an encapsulating stream, we will have to more formally identify
;;; the stream protocol.  In particular, we need to decide whether each stream
;;; must support stream-output-record and (setf stream-output-record) methods, etc.
;;; (This so that we can do normal formatted output to the encapsulated stream.)
;;; If not, we must require that each stream support ancillary methods that take
;;; an xstream and pass the requests along.
(defclass standard-input-editing-stream
          (input-editing-stream-mixin)
     ((start-x-position)
      (start-y-position)
      ;; all I want is :writer, but PCL barfs
      (original-stream-recording-p :accessor original-stream-recording-p)))

;;; Required methods:
(defmethod start-cursor-position ((istream standard-input-editing-stream))
  (with-slots (start-x-position start-y-position) istream
    (values start-x-position start-y-position)))

(defmethod initialize-position ((istream standard-input-editing-stream))
  (with-slots (stream start-x-position start-y-position) istream
    (multiple-value-bind (x y)
        ;; in this specific implementation it's ok for
        ;; us to know that our encapsulated stream will
        ;; support the extended output protocol.
        (stream-cursor-position stream)
      (setf start-x-position x
            start-y-position y))))

(defmethod reset-cursor-position ((istream standard-input-editing-stream))
  (with-slots (stream start-x-position start-y-position) istream
    (stream-set-cursor-position stream start-x-position start-y-position)))

#+(or aclpc acl86win32)
(defvar *wd40italic* nil)

(defmethod erase-input-buffer ((istream standard-input-editing-stream)
                               &optional (start-position 0))

  (let ((stream (encapsulating-stream-stream istream))
        (oleft nil)
        #+(or aclpc acl86win32) (*wd40italic* nil)
        otop oright obottom)
    ;; Assumptions: 1. Erasure happens left-to-right, top-to-bottom (just
    ;; like text output).  2. Nothing interesting appears on the screen below
    ;; and to the right of text from the input editor.  We merge erasures so
    ;; as to erase as few rectangles as possible.
    (labels ((erase-merged-stuff ()
               #+(or aclpc acl86win32)
               (if *wd40italic*
                 (draw-polygon-internal stream (coordinate *wd40italic*)
                                        oleft otop oright obottom
                                        +background-ink+) 
                 (draw-rectangle-internal stream (coordinate 0) (coordinate 0)
                                          oleft otop oright obottom
                                          +background-ink+ nil))
               #-(or aclpc acl86win32)
               (draw-rectangle-internal stream (coordinate 0) (coordinate 0)
                                        oleft otop oright obottom
                                        +background-ink+ nil)
               )
             (erase-screen-piece (left top right bottom extra)
               (declare (ignore extra))
               (cond ((null oleft)                ;First rectangle
                      (setf oleft left
                            otop  top
                            oright  right
                            obottom bottom))
                     ((= otop top)                ;Same line
                      (maxf oright right)
                      (maxf obottom bottom)
                      (minf oleft left))
                     ((<= oleft left)                ;Further down, same or larger indent
                      (maxf oright right)
                      (maxf obottom bottom))
                     (t                                ;next line is further left than previous
                      (erase-merged-stuff)
                      (setf oleft left
                            otop  top
                            oright  right
                            obottom bottom)))))
      (declare (dynamic-extent #'erase-merged-stuff #'erase-screen-piece))
      (do-input-buffer-screen-real-estate istream #'erase-screen-piece start-position)
      (when oleft (erase-merged-stuff)))))

;;--- This mechanism is only partially implemented.  In order to work better,
;;--- it requires that the IE maintain its own concept of the prompt.
(defmethod invoke-with-input-editor-typeout ((istream standard-input-editing-stream)
                                             continuation &key erase)
  ;; I don't know why someone made this reset the cursor position to the original
  ;; start position, but that's wrong.  The output should come out *above* the input,
  ;; and the input's start position has to move down.
  (with-slots (original-stream-recording-p) istream
    (erase-input-buffer istream)
    (reset-cursor-position istream)
    (fresh-line istream)
    (unwind-protect
        (if (and erase original-stream-recording-p)
            (let ((record (with-output-recording-options (istream :draw nil :record t)
                            (with-new-output-record (istream)
                              (funcall continuation istream)))))
              (with-bounding-rectangle* (left top right bottom) record
                (multiple-value-bind (xoff yoff)
                    (convert-from-relative-to-absolute-coordinates
                     istream (output-record-parent record))
                  (draw-rectangle-internal
                   istream xoff yoff
                   left top right (+ bottom (stream-line-height istream))
                   +background-ink+ nil)))
              (replay record istream))
          (with-output-recording-options (istream :record original-stream-recording-p)
            (funcall continuation istream)))
      (fresh-line istream)
      (terpri istream)
      (initialize-position istream)
      (redraw-input-buffer istream))))

(defmethod invoke-with-input-editor-typeout ((stream t) continuation &key erase)
  (declare (ignore erase))
  (fresh-line stream)
  (unwind-protect
      (funcall continuation stream)
    (fresh-line stream)
    (terpri stream)))

(defmethod input-editor-format ((istream standard-input-editing-stream)
                                format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (with-slots (input-buffer scan-pointer insertion-pointer) istream
    (unless (stream-rescanning-p istream)
      (let* ((string (apply #'format nil format-string format-args))
             (noise-string
               (make-noise-string
                 :display-string string
                 :unique-id string
                 :text-style (medium-text-style istream))))
        (vector-push-extend noise-string input-buffer)
        (incf scan-pointer)
        (incf insertion-pointer)
        (with-text-style (istream (noise-string-text-style noise-string))
          (write-string (noise-string-display-string noise-string) istream))))))


(defresource input-editing-stream (class stream)
  :constructor (make-instance class :stream stream)
  :initializer (initialize-input-editing-stream input-editing-stream))

(defun invoke-with-input-editing (stream continuation class input-sensitizer
                                  &optional initial-contents)
  (cond ((not (stream-supports-input-editing stream))
         (funcall continuation stream))
        ((input-editing-stream-p stream)
         (with-stack-list (context 'input-editor :stream stream)
           (with-input-context (context) ()
                (funcall continuation stream)
              (t (beep stream)))))
        (t
         (let ((original-stream stream))
           (letf-using-resource (stream input-editing-stream class stream)
             (setf (original-stream-recording-p stream) (stream-recording-p stream))
             (unwind-protect
                 (letf-globally (((stream-input-editor-stream original-stream) stream))
                   (with-output-recording-options (stream :record nil)
                     (initialize-position stream)
                     (cond ((stringp initial-contents)
                            (replace-input stream initial-contents))
                           ((consp initial-contents)
                            (presentation-replace-input
                             stream (first initial-contents) (second initial-contents)
                             +textual-view+)))
                     (with-stack-list (context 'input-editor :stream stream)
                       (loop
                         (catch 'rescan
                           (reset-scan-pointer stream)
                           (handler-bind 
                               ((parse-error
                                 #'(lambda (anerror)
                                     (display-input-editor-error stream anerror))))
                             (return
                               (let #+Genera ((sys:rubout-handler :read)) #-Genera ()
                                    (with-input-context (context) ()
                                                        (funcall continuation stream)
                                                        (t (beep stream)))))))))))
               ;; Need to put the input buffer into the history, if it would
               ;; have gone in anyway.
               ;; What about making presentations out of the stuff in the input buffer?
               ;; The only guy that has *that* level of information is ACCEPT.
               ;; Don't bother to redraw if the buffer is empty.
               (unless (zerop (fill-pointer (input-editor-buffer stream)))
                 (with-output-recording-options (stream :draw nil)
                   (if input-sensitizer
                       (funcall input-sensitizer #'redraw-input-buffer stream)
                     (redraw-input-buffer stream))))))))))

(defmethod stream-supports-input-editing ((stream fundamental-stream)) t)

;; It really sucks that we have to write T when we mean STRING-INPUT-STREAM
#-allegro
(defmethod stream-supports-input-editing ((stream t)) nil)
#+allegro
(progn
  (defmethod stream-supports-input-editing 
      ((stream excl::string-input-stream))
    nil)
  (defmethod stream-supports-input-editing 
      ((stream excl:simple-stream))
    ;; this is obviously wrong, but as far as CLIM is concerned, they
    ;; don't, or not yet.
    nil)
  )

#+Genera
(defmethod si:stream-compatible-input-editing
           ((stream input-protocol-mixin)
            continuation activation-gesture-p delimiter-gesture-p)
  ;; If there's a prompt, for now just print it once before entering the input editor
  (si:display-prompt-option stream (si:input-editor-option :prompt) nil :prompt)
  ;; Enter the CLIM input editor in a way compatible with the Genera input editor
  (with-input-editing (stream)
    (with-activation-gestures (activation-gesture-p)
      (with-delimiter-gestures (delimiter-gesture-p)
        (multiple-value-prog1
          (handler-bind ((sys:parse-error
                           #'(lambda (anerror)
                               (display-input-editor-error stream anerror))))
            (let ((sys:rubout-handler :read))
              (funcall continuation stream)))
          ;; On the way out, swallow the unread delimiter, since the Genera input
          ;; editor discards activation gestures on the way out.
          ;; This breaks some cases of read-preserving-whitespace, which could be
          ;; fixed by only discarding activation gestures, but there's no way to
          ;; tell here if a character was an activation gesture, so trying to fix
          ;; read-preserving-whitespace would only break more important things
          (stream-read-char-no-hang stream))))))

#+Genera
(defmethod si:stream-compatible-input-editing
           ((stream input-editing-stream-mixin)
            continuation activation-gesture-p delimiter-gesture-p)
  ;; We are already in the CLIM input editor, but still need to establish compatibility
  ;; with the Genera input editor
  (with-activation-gestures (activation-gesture-p)
    (with-delimiter-gestures (delimiter-gesture-p)
      (handler-bind ((sys:parse-error
                       #'(lambda (anerror)
                           (display-input-editor-error stream anerror))))
        (let ((sys:rubout-handler :read))
          (funcall continuation stream))))))

#+Genera
(defgeneric stream-compatible-replace-input-since
            (stream location string &optional begin end rescan-mode)
  (:selector :replace-input-since))

#+Genera
(defmethod stream-compatible-replace-input-since
           ((stream input-editing-stream-mixin) location string
            &optional (begin 0) (end nil) (rescan-mode :ignore))
  (when (stream-rescanning-p stream)
    (case rescan-mode
      ((:ignore) (return-from stream-compatible-replace-input-since nil))
      ((:error) (error "Replace-input while rescanning"))))
  (replace-input stream (string string) :start begin :end end
                 :buffer-start location :rescan nil))


;;;#+(or aclpc acl86win32)
;;;(eval-when (compile load eval)
;;;   ;;mm: 11Jan95 - this is defined later in  ???
;;;   (unless (ignore-errors (find-class 'accept-values-pane))
;;;      (defclass accept-values-pane () ()))
;;;   (unless (ignore-errors (find-class 'accept-values-stream))
;;;      (defclass accept-values-stream () ()))
;;;   (unless (ignore-errors (find-class 'accept-values))
;;;      (defclass accept-values () ()))
;;;   )

(defmethod frame-manager-display-input-editor-error 
           ((framem standard-frame-manager) frame (stream standard-input-editing-stream) anerror)
  ;;--- Resignal the error so the user can handle it
  ;;--- (in lieu of HANDLER-BIND-DEFAULT)
  (if (typep (encapsulating-stream-stream stream) 'accept-values-pane)
      (notify-user frame (princ-to-string anerror)
                   :title "Input error"
                   :style :error :exit-boxes '(:exit))
    (progn
      (beep stream)
      (with-input-editor-typeout (stream :erase t)
        (format stream "~A~%Please edit your input." anerror))))
  (remove-activation-gesture stream)
  ;; Wait until the user forces a rescan by typing an input editing command
  (loop (read-gesture :stream stream)))

(defmethod frame-manager-display-help
    (framem frame (stream standard-input-editing-stream) continuation)
  (declare (dynamic-extent continuation))
  (declare (ignore framem frame))
  ;;-- Yuck but think of a better way
  (let ((old-help *accept-help*))
    (if (or (typep (encapsulating-stream-stream stream) '(or accept-values-pane accept-values-stream))
            (typep *application-frame* 'accept-values))
        (accepting-values (stream :exit-boxes '(:exit)
                                  :label "Input editor help"
                                  :own-window t)
          (let ((*accept-help* old-help))
            (funcall continuation stream)))
      (with-input-editor-typeout (stream :erase t) ;don't scribble over previous output
        (funcall continuation stream)))))


(defun stream-yay-or-nay-p (stream ptype format-string args &rest accept-args)
  (values (apply #'accept ptype
                 :stream stream
                 :prompt-mode :raw
                 :prompt (if format-string
                             (apply #'format nil format-string args)
                           "")
                 accept-args)))

(defmethod stream-yes-or-no-p  
  ((stream input-protocol-mixin) &optional format-string &rest args)
  (stream-yay-or-nay-p stream
                       '(member-alist (("Yes" :value t) ("No" :value nil)))
                       format-string args))


(defmethod stream-y-or-n-p
  ((stream input-protocol-mixin) &optional format-string &rest args)
  (stream-yay-or-nay-p stream
                       '(member-alist (("Y" :value t) ("N" :value nil)))
                       format-string args))


(defmethod stream-y-or-n-or-newline-p 
  ((stream input-protocol-mixin) &optional format-string &rest args)
  (stream-yay-or-nay-p stream
                       '(member-alist (("Y" :value t) ("N" :value nil)))
                       format-string args
                       :display-default nil 
                       :default t))


#+(or aclpc acl86win32)
(defvar *arr-for-draw-polygon-internal* (make-array '(8))) 

#+(or aclpc acl86win32)
(defun draw-polygon-internal
       (stream xoff left top right bottom ink)
  (let ((medium (sheet-medium stream))
        (arr *arr-for-draw-polygon-internal*))
    (letf-globally (((medium-transformation medium) +identity-transformation+)
                    ((medium-ink medium) ink))
      (setf (svref arr 0) (- left 1))
      (setf (svref arr 1) bottom)
      (setf (svref arr 2) (+ left xoff 1))
      (setf (svref arr 3) top)
      (setf (svref arr 4) (+ right xoff 3))
      (setf (svref arr 5) top)
      (setf (svref arr 6) right)
      (setf (svref arr 7) bottom)
      (medium-draw-polygon* medium arr t t))))

