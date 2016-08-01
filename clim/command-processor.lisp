;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 International Lisp Associates."

(defparameter *command-dispatchers* '(#\:))

;;; What separates command name from command args
(defparameter *command-name-delimiters* '(#\Space))

;;; What separates command args.
(defparameter *command-argument-delimiters* '(#\Space))

(define-gesture-name :preview-command :keyboard (:complete :meta))
(defparameter *command-previewers* '(:preview-command))

;; This is around only to provide a input context "wall" during parsing
(define-presentation-type command-arguments ())

(defun invoke-command-name-parser-and-collect-1
       (command-name arg-parser delimiter-parser stream)
  (declare (dynamic-extent arg-parser delimiter-parser))
  (let ((parser-function (get command-name 'command-parser))
        (arguments nil))
    (catch 'stop-reading-command-arguments
      (flet ((parse-and-collect (stream presentation-type &rest args)
               (declare (dynamic-extent args))
               (multiple-value-bind (object type)
                   (apply arg-parser stream presentation-type args)
                 (push object arguments)
                 (values object type))))
        (declare (dynamic-extent #'parse-and-collect))
        (funcall parser-function #'parse-and-collect delimiter-parser stream)))
    (let ((final-delimiter (funcall delimiter-parser stream ':end)))
      (values (cons command-name (nreverse arguments))
              final-delimiter))))

;;; Various ways to invoke the command parser.
(defun invoke-command-parser-and-collect (command-table arg-parser delimiter-parser stream)
  (declare (dynamic-extent arg-parser delimiter-parser))
  ;; At this point, a user can click on both commands and command names.
  (let ((command-name
          (with-stack-list (name-type 'command-name ':command-table command-table)
            (with-stack-list (cmd-type 'command ':command-table command-table)
              (handler-case
                  (funcall arg-parser stream name-type
                           :history cmd-type
                           ;; Prevent it from making a noise string of the
                           ;; command prompt.  Arg parsers better always be
                           ;; prepared to receive a :PROMPT argument
                           :prompt nil)
                (empty-completion-error ()
                 ;; No command name, just bag it and return NIL
                 (return-from invoke-command-parser-and-collect nil)))))))
    (funcall delimiter-parser stream ':args)
    ;; Establish a "wall" so that commands are no longer sensitive.
    (with-input-context ('command-arguments :override t)
                        ()
         (flet ((command-help (stream action string-so-far)
                  (declare (ignore action string-so-far))
                  (write-string "No additional arguments are allowed for this command."
                                stream)))
           (declare (dynamic-extent #'command-help))
           (with-accept-help ((:top-level-help #'command-help))
             (multiple-value-bind (command final-delimiter)
                 (invoke-command-name-parser-and-collect-1 
                   command-name arg-parser delimiter-parser stream)
               (if (and final-delimiter
                        (member final-delimiter *command-previewers*
                                :test #'keyboard-event-matches-gesture-name-p))
                   (with-input-editor-typeout (stream)
                     (accept-values-command-parser
                       command-name command-table 
                       ;; Really want to use stream here but not all the protocols
                       ;; are properly encapsulated, I guess
                       (if (encapsulating-stream-p stream) 
                           (encapsulating-stream-stream stream)
                           stream)
                       command))
                   command))))
       (t (error "You can't get here from there")))))

(defun parse-normal-arg (stream arg-type &rest options)
  (declare (dynamic-extent options))
  (with-delimiter-gestures (*command-argument-delimiters*)
    (flet ((command-arg-help (stream action string-so-far)
             (declare (ignore action string-so-far))
             (write-string "You are being asked to enter " stream)
             (describe-presentation-type arg-type stream)
             (write-char #\. stream)))
      (declare (dynamic-extent #'command-arg-help))
      (with-accept-help
          (((:top-level-help :override)
            ;; Override the help for the higher level COMMAND context so
            ;; that we get information about this argument instead of
            ;; "You are being asked to enter a command or form."
            #'command-arg-help))
        (multiple-value-prog1
          (apply #'accept arg-type :stream stream options)
          ;; Check this out.  If we're not rescanning and there is
          ;; nothing else in the buffer, that means the call to ACCEPT
          ;; must have been satisfied by clicking on something with
          ;; the mouse.  So insert an argument delimiter so that the
          ;; user can get to the next field without typing anything.
          (unless (or (stream-rescanning-p stream)
                      (read-gesture :stream stream :timeout 0 :peek-p t))
            (replace-input stream (string (first *command-argument-delimiters*))
                           :rescan t)))))))

(defun process-delimiter (stream &key activation-p echo-space)
  (let ((delimiter (read-gesture :stream stream)))
    (when (and activation-p (activation-gesture-p delimiter))
      (unread-gesture delimiter :stream stream)
      (when echo-space
        ;; If we're not finished yet, leave the activation gesture for later,
        ;; insert a space in front of it, and read past the space.  Something
        ;; like the echo of a default will come after the space.
        ;; Must REPLACE-INPUT after UNREAD-GESTURE so the delimiter is unread
        ;; into the input editor's buffer, not the underlying stream's buffer.
        (when (input-editing-stream-p stream)
          (unless (stream-rescanning-p stream)
            (replace-input stream (string (first *command-argument-delimiters*)))))))
    delimiter))

(define-presentation-type keyword-argument-name
                          (keywords &key keyword-documentation keyword-defaults))

(define-presentation-method present (object (type keyword-argument-name) stream
                                     (view textual-view) &key)
  (format stream ":~A" (string-capitalize object)))

;;--- I wish this did not cons so much
(define-presentation-method accept ((type keyword-argument-name) stream
                                    (view textual-view) &key)
  (flet ((print-keyword (possibility type stream)
           (with-output-as-presentation (stream (second possibility) type
                                         :single-box t)
             (write-string (first possibility) stream)
             (let ((documentation (assoc (second possibility) keyword-documentation))
                   (default (assoc (second possibility) keyword-defaults)))
               (when (or documentation default)
                 (multiple-value-bind (x y) (stream-cursor-position stream)
                   (stream-set-cursor-position stream (max 200 (+ x 20)) y)
                   (write-string (or (second documentation) "No documentation") stream)
                   (when default
                     (write-string " (default is " stream)
                     (present (second default) (third default) :stream stream)
                     (write-string ")"  stream))))))))
    (declare (dynamic-extent #'print-keyword))
    (values (completing-from-suggestions (stream :possibility-printer #'print-keyword)
              (dolist (keyword keywords)
                (suggest (format nil ":~A" (string-capitalize keyword)) keyword))))))

(define-presentation-method presentation-typep (object (type keyword-argument-name))
  (not (not (member object keywords))))

(define-presentation-method presentation-subtypep ((sub keyword-argument-name) super)
  (equal sub super))

(defun process-keyword-args (stream keywords continuation delimiter-parser arg-parser
                             &optional keyword-documentation keyword-defaults)
  (funcall delimiter-parser stream :keywords)
  (loop
    (unless keywords (return))
    (let ((keyword (parse-keyword arg-parser delimiter-parser stream keywords
                                  keyword-documentation keyword-defaults)))
      (unless keyword (return))                        ;arg unparser can return NIL
      (funcall continuation keyword)
      (setq keywords (remove keyword keywords)))
    (funcall delimiter-parser stream :optional)))

(defun parse-keyword (arg-parser delimiter-parser stream arguments
                      keyword-documentation keyword-defaults)
  (handler-case
      (prog1 (funcall arg-parser stream
                      `(keyword-argument-name ,arguments
                                              :keyword-documentation ,keyword-documentation
                                              :keyword-defaults ,keyword-defaults)
                      :prompt nil)
             ;; There must be a value following, no?
             (funcall delimiter-parser stream :args))
    (empty-completion-error ()
     ;; If there is trailing whitespace that causes the completer to try
     ;; to complete a null string, just give up and activate the command
     (return-from parse-keyword nil))))

#+++ignore        ;this is slow and useless, don't bother
(defun build-command (command-name &rest partial-command-args)
  (declare (dynamic-extent partial-command-args))
  (flet ((arg-parser (&rest args)
           (declare (ignore args))
           (cond (partial-command-args (pop partial-command-args))
                 (t *unsupplied-argument-marker*)))
         (delimiter-parser (&rest args)
           (declare (ignore args))))
    (declare (dynamic-extent #'arg-parser #'delimiter-parser))
    (invoke-command-name-parser-and-collect-1 
      command-name #'arg-parser #'delimiter-parser 'ignore)))


;;; Command-line interaction style

(defun command-line-command-parser (command-table stream)
  (flet ((delimiter-parser (stream args-to-go)
           ;; All the checks for EOF are in support of ACCEPT-FROM-STRING...
           (cond ((eq args-to-go :args)
                  ;; Reached after a command name or keyword argument name, before
                  ;; the argument(s) that must follow
                  (process-delimiter stream :activation-p t :echo-space t))
                 ((eq args-to-go :end)
                  ;; Reached after an entire command has been read, to ignore
                  ;; trailing whitespace and eat the Return
                  (loop
                    (let ((char (process-delimiter stream)))
                      (when (or (activation-gesture-p char)
                                (eq char *end-of-file-marker*))
                        (return char))
                      (unless (whitespace-char-p char)
                        (simple-parse-error "Extraneous character seen at end of line")))))
                 ((eq args-to-go :keywords)
                  ;; Reached before reading the first keyword argument
                  ;; If this is a command with no positional arguments, the preceding call
                  ;; should have had args-to-go = (&key ...), but actually had :ARGS.
                  ;; Compensate for that by checking for an activation gesture now
                  (let ((char (read-gesture :stream stream :timeout 0)))
                    (when char
                      (unread-gesture char :stream stream)
                      (when (or (activation-gesture-p char)
                                (eq char *end-of-file-marker*))
                        (throw 'stop-reading-command-arguments nil))))
                  (prompt-for-accept stream 'keyword +textual-view+
                                     :prompt "keywords" :query-identifier '#:keywords))
                 ((or (eq args-to-go :optional)
                      (eq (first args-to-go) '&key))
                  ;; Reached if we are about to read a keyword argument name
                  ;; :optional happens after the value for a keyword argument
                  ;; Otherwise args-to-go is a list of the remaining argument specifiers
                  ;; after reading the value of a positional argument
                  (let ((char (process-delimiter stream :activation-p t)))
                    (when (or (activation-gesture-p char)
                              (eq char *end-of-file-marker*))
                      (throw 'stop-reading-command-arguments nil))
                    char))
                 (t
                  ;; Reached if we are about to read a positional argument
                  ;; other than the first positional argument
                  (process-delimiter stream :activation-p t :echo-space t)))))
    (declare (dynamic-extent #'delimiter-parser))
    (with-activation-gestures (*command-previewers*)
      (invoke-command-parser-and-collect
        command-table #'parse-normal-arg #'delimiter-parser stream))))

(defun command-line-command-unparser (command-table stream args-to-go
                                      &rest keys
                                      &key for-context-type (acceptably t) &allow-other-keys)
  (declare (ignore keys))
  (let ((command-name-unread t))
    (flet ((reverse-parser (stream presentation-type &rest args)
             (declare (ignore args))
             (cond (command-name-unread
                    (let ((command-name (pop args-to-go)))
                      (setq command-name-unread nil)
                      (present command-name presentation-type :stream stream
                               :acceptably acceptably :for-context-type for-context-type)
                      command-name))
                   ((null args-to-go) nil)
                   (t (let ((thing (pop args-to-go)))
                        (if (unsupplied-argument-p thing)
                            (with-text-face (stream :italic)
                              (write-string "<Unsupplied>" stream))
                            (present thing presentation-type :stream stream
                                     :acceptably acceptably
                                     :for-context-type for-context-type))
                        thing))))
           (delimiter-parser (stream delimiter-args-to-go)
             (if args-to-go                        ;only if there are still arguments remaining
                 (unless (member delimiter-args-to-go '(:end :keywords))
                   (write-string " " stream))
                 ;; If there are no more args to go, we're done.  Stopping this
                 ;; way allows us to gracefully stop unparsing when there are 
                 ;; keyword arguments in the command template that are unsupplied
                 ;; in the command object.
                 (return-from command-line-command-unparser nil))))
      (declare (dynamic-extent #'reverse-parser #'delimiter-parser))
      (invoke-command-parser-and-collect
        command-table #'reverse-parser #'delimiter-parser stream))))

;; Finds the least specific context type that is a supertype of TYPE,
;; stopping as soon as there is a context type that is not a supertype.
;; Used to find something useful for :FOR-CONTEXT-TYPE.
(defun least-specific-matching-context-type (type)
  (let ((match nil))
    (dolist (context *input-context*)
      (let ((context-type (input-context-type context)))
        (if (presentation-subtypep-1 type context-type)
            (setq match context-type)
            (return))))
    match))

(defun command-line-read-remaining-arguments-for-partial-command
       (partial-command command-table stream start-location &key for-accelerator)
  (let* ((last-supplied (position *unsupplied-argument-marker* partial-command
                                  :from-end t :test-not #'eql))
         (unsupplied-before-that (and last-supplied
                                      (position *unsupplied-argument-marker* partial-command
                                                :end last-supplied :from-end t)))
         (command-type `(command :command-table ,command-table))
         (for-context-type (least-specific-matching-context-type command-type)))
    (cond ((or unsupplied-before-that for-accelerator
               (null (command-line-name-for-command 
                       (command-name partial-command) command-table :errorp nil)))
           ;; If the unsupplied argument is not the last argument in the
           ;; command line, we go through the ACCEPTING-VALUES command parser.
           ;; Ditto, if there is no command-line name for the command.
           (let ((command
                   (with-input-editor-typeout (stream)
                     (accept-values-command-parser
                       (first partial-command) command-table
                       (if (encapsulating-stream-p stream) 
                           (encapsulating-stream-stream stream)
                           stream)
                       partial-command))))
             command))
          (t
           (block doit
             (flet ((reverse-parser (stream presentation-type &rest args)
                      (declare (ignore args))
                      (let ((arg-p partial-command)
                            (arg (pop partial-command)))
                        (cond ((and arg-p (not (unsupplied-argument-p arg)))
                               (setq start-location
                                     (presentation-replace-input
                                       stream arg presentation-type +textual-view+
                                       :buffer-start start-location :rescan nil
                                       :for-context-type (or for-context-type
                                                             presentation-type)))
                               arg)
                              (t (return-from doit)))))
                    (delimiter-parser (stream delimiter-args-to-go)
                      (declare (ignore delimiter-args-to-go))
                      (when partial-command
                        (setq start-location
                              (replace-input stream " "
                                             :buffer-start start-location :rescan nil)))
                      #\space))
               (declare (dynamic-extent #'reverse-parser #'delimiter-parser))
               (invoke-command-parser-and-collect
                 command-table #'reverse-parser #'delimiter-parser stream)))
           (immediate-rescan stream)))))

(defun accept-values-command-parser (command-name command-table stream partial-command
                                     &key own-window)
  (let ((*original-stream* nil)
        copy-partial-command
        (result nil)
        result-index)
    (labels ((arg-parser (stream presentation-type &rest args)
               (declare (dynamic-extent args))
               ;; This code is to handle the case where a partial command has been
               ;; passed in.  PARSE-NORMAL-ARG needs to be called with a :DEFAULT of
               ;; the appropriate element of the partial command structure.  
               (let* ((default (if copy-partial-command
                                   (pop copy-partial-command)
                                   *unsupplied-argument-marker*)))
                 ;; Make a place to store the impending result.  We need to
                 ;; resort to this so that we don't clobber changed fields
                 ;; during each pass through the dialog.
                 (incf result-index)
                 (setq result (nconc result (list nil)))
                 (with-presentation-type-decoded (type-name parameters) presentation-type
                   (when (eq type-name 'command-name)
                     ;; Side effect the result (same deal below)
                     (setf (nth result-index result) command-name)
                     (return-from arg-parser (values command-name presentation-type)))
                   (cond ((not (unsupplied-argument-p default))
                          (cond ((eq type-name 'keyword-argument-name) 
                                 (setf (nth result-index result) default)
                                 default)
                                (t (multiple-value-bind (arg type)
                                       (apply #'parse-normal-arg
                                              stream presentation-type
                                              :default default args)
                                     (setf (nth result-index result) arg)
                                     (values arg type)))))
                         ((eq type-name 'keyword-argument-name)
                          (let ((keyword
                                  (intern (symbol-name (caar parameters)) *keyword-package*)))
                            (setf (nth result-index result) keyword)
                            keyword))
                         (t 
                          (multiple-value-bind (arg type)
                              (apply #'parse-normal-arg
                                     stream presentation-type
                                     :provide-default nil args)
                            (setf (nth result-index result) arg)
                            (values arg type)))))))
             (parse-normal-arg (stream arg-type &rest options)
               (declare (dynamic-extent options))
               (with-delimiter-gestures (*command-argument-delimiters*)
                 (apply #'accept arg-type :stream stream options)))
             (separate-args (stream args-to-go)
               (when (and args-to-go (not (member args-to-go '(:end :keywords))))
                 (fresh-line stream))))
      (declare (dynamic-extent #'arg-parser #'parse-normal-arg #'separate-args))
      (let ((command
              (accepting-values (stream :own-window own-window)
                (fresh-line stream)
                (with-output-recording-options (stream :record t)
                  (updating-output (stream :unique-id '#:header :cache-value t)
                    (with-text-face (stream :italic)
                      (format stream "Specify arguments for ")
                      (present command-name `(command-name :command-table ,command-table)
                               :stream stream)
                      (write-char #\: stream))
                    (fresh-line stream)))
                ;; This copy is done because the accepting-values may/will run this
                ;; body several times.
                (setq copy-partial-command (or result
                                               (copy-list partial-command)))
                (setq result nil
                      result-index -1)
                (invoke-command-parser-and-collect
                  command-table #'arg-parser #'separate-args stream)
                result)))
        ;; If the person clicked on the <Abort> exit box, the ABORT restart
        ;; will be invoked and we'll never get here.
        command))))


;;; Menu-only interaction style

(defun menu-command-parser (command-table stream &key timeout)
  (with-stack-list (command-type 'command ':command-table command-table)
    (with-command-table-keystrokes (keystrokes command-table)
      (with-input-context (command-type :override t)
                          (command type)
           (let ((first-arg t))
             (flet
               ((menu-parser (stream presentation-type &rest args)
                  (declare (ignore args))
                  (multiple-value-prog1
                    ;; For subsequent command args it makes no sense to still be "within"
                    ;; the inherited context because you can't "back up" to edit things.
                    ;; The only interesting context is the current argument.
                    (with-input-context (presentation-type :override (not first-arg))
                                        (object type)
                         (if keystrokes
                             (loop
                               (multiple-value-bind (keystroke numeric-arg)
                                   (block keystroke
                                     (handler-bind
                                       ((accelerator-gesture
                                          #'(lambda (c)
                                              (return-from keystroke
                                                (values
                                                  (accelerator-gesture-event c)
                                                  (accelerator-gesture-numeric-argument c))))))
                                       (let ((*accelerator-gestures* keystrokes))
                                         (read-gesture :stream stream :timeout timeout))))
                                 (when (eq numeric-arg :timeout)
                                   (return-from menu-command-parser nil))
                                 (when (keyboard-event-p keystroke)
                                   (let ((command 
                                           (lookup-keystroke-command-item
                                             keystroke command-table
                                             :numeric-argument numeric-arg)))
                                     (when (presentation-typep command command-type)
                                       (return-from menu-command-parser
                                         (values command
                                                 command-type)))))
                                 (beep stream)
                                 (discard-keystroke-gesture stream)))
                           (loop 
                             (multiple-value-bind (keystroke numeric-arg)
                                 (read-gesture :stream stream :timeout timeout)
                               (declare (ignore keystroke))
                               (when (eq numeric-arg :timeout)
                                 (return-from menu-command-parser nil))
                               (beep stream)
                               (discard-keystroke-gesture stream))))
                       (t (values object type)))
                    (setq first-arg nil)))
                (menu-delimiter (stream args-to-go)
                  (declare (ignore stream))
                  (when (only-keyword-args-remain args-to-go)
                    (throw 'stop-reading-command-arguments nil))))
               (declare (dynamic-extent #'menu-parser #'menu-delimiter))
               (invoke-command-parser-and-collect 
                 command-table #'menu-parser #'menu-delimiter stream)))
         (t (values command type))))))


(defun discard-keystroke-gesture (stream)
  ;;--- Nasty hack
  (when (typep stream 'input-editing-stream-mixin)
    (let ((input-buffer
           (input-editor-buffer stream)))
      (ie-rub-del stream input-buffer -1))))

(defun menu-read-remaining-arguments-for-partial-command
       (partial-command command-table stream start-location &key for-accelerator)
  (declare (ignore start-location for-accelerator))
  (flet ((reverse-parser (stream presentation-type &rest args)
           (declare (ignore args))
           (let ((arg-p partial-command)
                 (arg (pop partial-command)))
             (cond ((and arg-p (not (unsupplied-argument-p arg)))
                    (return-from reverse-parser (values arg presentation-type)))
                   (t 
                    ;; Override the command context so that only objects
                    ;; have this exact presentation type are sensitive
                    (let ((*input-context* nil))
                      ;;--- We really don't want to allow keyboard input...
                      (accept presentation-type :stream stream 
                                                :prompt nil :replace-input nil))))))
         (menu-delimiter (stream args-to-go)
           (declare (ignore stream))
           (when (only-keyword-args-remain args-to-go)
             (throw 'stop-reading-command-arguments nil))))
    (declare (dynamic-extent #'reverse-parser #'menu-delimiter))
    (invoke-command-parser-and-collect
      command-table #'reverse-parser #'menu-delimiter stream)))

(defun only-keyword-args-remain (argument-specs)
  (declare (ignore argument-specs))
  ;; For future expansion...
  nil)


;;; Command presentation types

(define-presentation-type command-name
                          (&key (command-table (frame-command-table *application-frame*))))

(define-presentation-method accept ((type command-name) stream (view textual-view) &key)
  (setq command-table (find-command-table command-table))
  (values
   (complete-input stream
                   #'(lambda (string action)
                       (command-table-complete-input command-table string action
                                                     :frame *application-frame*))
                   :partial-completers *command-name-delimiters*)))

(define-presentation-method present (symbol (type command-name) stream (view textual-view)
                                     &key for-context-type)
  (setq command-table (find-command-table command-table))
  (when (eq (presentation-type-name for-context-type) 'command-or-form)
    (write-char (first *command-dispatchers*) stream))
  (let ((name (command-line-name-for-command 
                symbol command-table 
                ;; If someone is presenting the command textually, then we
                ;; have to come up with a command-line name no matter what.
                ;; This can happen for the Mouse-Right menu, for example.
                :errorp :create)))
    (write-string name stream)))

(define-presentation-method presentation-typep (symbol (type command-name))
  (command-accessible-in-command-table-p symbol (find-command-table command-table)))

;; Should we really be using the names everywhere because they're inviolate?
(define-presentation-method presentation-subtypep ((sub command-name) super)
  (let ((ct1 (find-command-table (with-presentation-type-parameters (command-name sub)
                                   command-table)))
        (ct2 (find-command-table (with-presentation-type-parameters (command-name super)
                                   command-table))))
    (command-table-presentation-subtypep ct1 ct2)))

;; Returns T iff COMMAND-TABLE-1 inherits from COMMAND-TABLE-2
(defun command-table-presentation-subtypep (command-table-1 command-table-2)
  (values
    (or (eq command-table-1 command-table-2)        ;cheap optimization
        (do-command-table-inheritance (comtab command-table-1)
          (when (eq comtab command-table-2)
            (return-from command-table-presentation-subtypep (values t t)))))
    t))

(define-presentation-method describe-presentation-type :after
                            ((type command-name) stream plural-count)
  (declare (ignore plural-count))        ;primary method gets it
  (format stream " in ~A" (command-table-name (find-command-table command-table)))) 

;;; This is the interface between commands and completion
(defun command-table-complete-input (command-table string action &key frame)
  (with-slots (completion-alist completion-alist-tick) command-table
    (when (or (null completion-alist)
              (> *completion-cache-tick* completion-alist-tick))
      (setq completion-alist nil)
      (do-command-table-inheritance (comtab command-table)
        (when (slot-value comtab 'command-line-names)
          (dovector (entry (slot-value comtab 'command-line-names))
            (pushnew entry completion-alist :test #'string-equal :key #'first))))
      (setq completion-alist (sort completion-alist #'string-lessp :key #'first)
            completion-alist-tick *completion-cache-tick*))
    (flet ((enabled-p (command)
             (or (null frame)
                 (command-enabled command frame))))
      (declare (dynamic-extent #'enabled-p))
      (complete-from-possibilities string completion-alist '(#\space)
                                   :action action :predicate #'enabled-p))))

(define-presentation-type command
                          (&key (command-table (frame-command-table *application-frame*))))

(define-presentation-method presentation-type-history ((type command))
  (presentation-type-history-for-frame type *application-frame*))

(define-presentation-method accept ((type command) stream (view textual-view) &key)
  (setq command-table (find-command-table command-table))
  (let ((start-position (and (input-editing-stream-p stream)
                             (stream-scan-pointer stream)))
        ;; this also requires some thought, but I suspect that
        ;; we can just kludge it this way in this presentation type,
        ;; because we can't think of any other presentation types
        ;; that would establish a shadowing context within the one
        ;; established by ACCEPT-1.
        (replace-input-p nil))
    (multiple-value-bind (object type)
        ;; We establish a new input context so that clicks throw to us
        ;; rather than to the input context established in ACCEPT-1.
        ;; This will let's us handle "partial commands" below.
        ;; The "partial" notion could be extended to apply to all
        ;; presentation-types, but there are so few which need this
        ;; treatment, that it does not seem worthwhile.
        (with-input-context (type :override nil)
                            (object presentation-type nil options)
             (funcall *command-parser* command-table stream)
           (t (when (getf options :echo t)
                (setq replace-input-p t))
              (values object type)))
      (cond ((partial-command-p object)
             (values (funcall *partial-command-parser*
                              object command-table stream start-position)
                     type))
            (t (when replace-input-p
                 (presentation-replace-input stream object type view
                                             :buffer-start start-position
                                             ;;--- We really need to pass the
                                             ;;--- query-identifier to the parser.
                                             ; :query-identifier query-identifier
                                             ))
               (values object type))))))

(define-presentation-method present (command (type command) stream (view textual-view)
                                    &key for-context-type acceptably)
  (setq command-table (find-command-table command-table))
  (funcall *command-unparser* command-table stream command
           :for-context-type (or for-context-type type) :acceptably acceptably))

(define-presentation-method presentation-typep (object (type command))
  (and (listp object)
       (command-accessible-in-command-table-p
         (command-name object) (find-command-table command-table))))

;; Should we really be using the names everywhere because they're inviolate?
(define-presentation-method presentation-subtypep ((sub command) super)
  (let ((ct1 (find-command-table (with-presentation-type-parameters (command sub)
                                   command-table)))
        (ct2 (find-command-table (with-presentation-type-parameters (command super)
                                   command-table))))
    (command-table-presentation-subtypep ct1 ct2)))

(define-presentation-method map-over-presentation-type-supertypes ((type command) function)
  (map-over-presentation-type-supertypes-augmented type function
    ;; Include COMMAND-OR-FORM in the supertypes
    (with-presentation-type-parameters (command type)
      (with-stack-list (new-type 'command-or-form :command-table command-table)
        (funcall function 'command-or-form new-type)))))


;;; COMMAND-OR-FORM, for Lisp Listener style applications

(define-presentation-type command-or-form
                          (&key (command-table (frame-command-table *application-frame*)))
  :options ((auto-activate nil boolean)))

(define-presentation-method presentation-type-history ((type command-or-form))
  (presentation-type-history-for-frame type *application-frame*))

(define-presentation-method accept ((type command-or-form) stream (view textual-view)
                                    &rest args)
  (declare (dynamic-extent args))
  (let ((command-type `(command :command-table ,command-table))
        (form-type `((form) :auto-activate ,auto-activate))
        (start-position (and (input-editing-stream-p stream)
                             (stream-scan-pointer stream)))
        (replace-input-p nil))
    (multiple-value-bind (object type)
        (with-input-context (command-type) (command command-presentation-type nil options)
             (with-input-context (form-type) (form form-presentation-type nil options)
                  (let ((gesture (read-gesture :stream stream :peek-p t)))
                    (cond ((and (characterp gesture)
                                (find gesture *command-dispatchers* :test #'char-equal))
                           (read-gesture :stream stream)        ;get out the colon
                           (apply #'accept command-type
                                  :stream stream :prompt nil :view view
                                  :history type args))
                          (t (apply #'accept form-type
                                    :stream stream :prompt nil :view view
                                    :history type args))))
                (t (when (getf options :echo t)
                     (setq replace-input-p t))
                   (values form form-presentation-type)))
           (t (when (getf options :echo t)
                (setq replace-input-p t))
              (when (partial-command-p command)
                (setq command (funcall *partial-command-parser*
                                       command command-table stream start-position)))
              (when replace-input-p
                (unless (stream-rescanning-p stream)
                  (replace-input stream (string (first *command-dispatchers*))
                                 :buffer-start start-position)
                  (incf start-position)))
              (values command command-presentation-type)))
      (when replace-input-p
        (presentation-replace-input stream object type view
                                    :buffer-start start-position))
      (values object type))))

(define-presentation-method present (thing (type command-or-form) stream view &rest args)
  (declare (dynamic-extent args))
  (setq command-table (find-command-table command-table))
  (apply #'present thing (if (object-is-command-p thing command-table)
                             `(command :command-table ,command-table)
                             `form)
         :stream stream :view view args))

(define-presentation-method presentation-typep (object (type command-or-form))
  (or (object-is-command-p object command-table)
      ;; Everything that's not a command is a form
      t))

(define-presentation-method presentation-subtypep ((sub command-or-form) super)
  (let ((ct1 (find-command-table (with-presentation-type-parameters (command sub)
                                   command-table)))
        (ct2 (find-command-table (with-presentation-type-parameters (command super)
                                   command-table))))
    (command-table-presentation-subtypep ct1 ct2)))

(defun object-is-command-p (object command-table)
  (and (listp object)
       (command-line-name-for-command
         (command-name object) command-table
         ;; Don't cons up a command-line name, since we know that forms are OK here
         :errorp nil)))


;; Read a command.
;; If USE-KEYSTROKES is T, allow the command to be input via keystroke accelerators.
(defun read-command (command-table
                     &key (stream *standard-input*)
                          (command-parser *command-parser*)
                          (command-unparser *command-unparser*)
                          (partial-command-parser *partial-command-parser*)
                          (use-keystrokes nil))
  (if use-keystrokes
      (with-command-table-keystrokes (keystrokes command-table)
        (read-command-using-keystrokes command-table keystrokes
                                       :stream stream
                                       :command-parser command-parser
                                       :command-unparser command-unparser
                                       :partial-command-parser partial-command-parser))
      (let ((*command-parser* command-parser)
            (*command-unparser* command-unparser)
            (*partial-command-parser* partial-command-parser))
	;; spr16572: Due to the identity translator, it is possible to
	;; click on anything that has been presented as a command, including
	;; disabled commands, commands from other command tables, and lists
	;; that aren't even funcallable.  This problem may be rooted in the 
	;; design of the identity translator and all the cures I have tried
	;; create much bigger problems.
	;; JPM 5/27/98.
        (values (accept `(command :command-table ,command-table)
                        :stream stream :prompt nil)))))

;; Read a command, allowing keystroke accelerators.  If we get a keystroke
;; with no corresponding command, just return the keystroke itself.
(defun read-command-using-keystrokes (command-table keystrokes
                                      &key (stream *standard-input*)
                                           (command-parser *command-parser*)
                                           (command-unparser *command-unparser*)
                                           (partial-command-parser *partial-command-parser*))
  (let ((*command-parser* command-parser)
        (*command-unparser* command-unparser)
        (*partial-command-parser* partial-command-parser))
    ;; NUMERIC-ARG only applies when we read a keystroke accelerator
    (multiple-value-bind (command numeric-arg)
        (block keystroke
          (handler-bind ((accelerator-gesture
                           #'(lambda (c)
                               (return-from keystroke
                                (values
                                  (accelerator-gesture-event c)
                                  (accelerator-gesture-numeric-argument c))))))
            (let ((*accelerator-gestures* keystrokes))
              (accept `(command :command-table ,command-table)
                      :stream stream :prompt nil))))
      (if (keyboard-event-p command)
          (let ((command (lookup-keystroke-command-item command command-table
                                                        :numeric-argument numeric-arg)))
            (if (partial-command-p command)
                (funcall *partial-command-parser*
                         command command-table stream nil :for-accelerator t)
                command))
          command))))


;;; Presentation to command translators

(defclass presentation-to-command-translator (presentation-translator)
     ((command-name :initarg :command-name
                    :reader presentation-translator-command-name)))

(defmacro define-presentation-to-command-translator
          (name
           (from-type command-name command-table
            &key (gesture ':select) tester
                 documentation pointer-documentation
                 (menu t) priority 
                 (echo t) (maintain-history t))
           arglist
           &body body &environment env)
  #+Genera (declare (zwei:indentation 1 3 3 1))
  (with-warnings-for-definition name define-presentation-translator
    (unless (and (symbolp command-name)
                 (or (get command-name 'command-parser)
                     (compile-time-property command-name 'command-name)))
      (warn "This translator produces a ~S command but this command-name has not been defined."
            command-name))
    (let ((to-type `(command :command-table ,command-table))
          (tester-args (first tester))
          (tester-body (rest tester)))
      (multiple-value-bind (doc-string declarations body)
          (extract-declarations tester-body env)
        (when body
          (setq body `((progn ,@body))))
        (pushnew 'frame tester-args :test #'string=)
        (setq tester `(,tester-args
                       ,@declarations
                       ,doc-string
                       (and (command-enabled ',command-name
                                             ,(find 'frame tester-args :test #'string=))
                            ,@body))))
      (multiple-value-bind (doc-string declarations body)
          (extract-declarations body env)
        (when body
          (setq body `((progn ,@body))))
        (setq body `(,@declarations
                     ,doc-string
                     ;; The body supplied by the user returns a list of the
                     ;; command's arguments
                     (values (cons ',command-name ,@body)
                             nil
                             '(:echo ,echo :maintain-history ,maintain-history))))
        `(define-presentation-translator-1 ,name
             (,from-type ,to-type ,command-table
              :gesture ,gesture
              :tester ,tester
              :documentation ,documentation
              ;; Old behavior
              ;; The pointer documentation defaults to the name of the command,
              ;; not the documentation.  This is a speed bum.
              ;; New Behavior is to to the right thing. If only
              ;; documentation is specified then rely on the
              ;; appropriate defaulting down the line to ensure that
              ;; we dont get duplicate functions.
              ,@(and (or pointer-documentation (null documentation))
                     `(:pointer-documentation ,(or pointer-documentation
                                                   (command-name-from-symbol command-name))))
              :menu ,menu
              :priority ,priority
              :tester-definitive t
              :translator-class presentation-to-command-translator
              :command-name ',command-name)
             ,arglist
           ,@body)))))

#+Genera
(scl:defprop define-presentation-to-command-translator define-presentation-translator
             zwei:definition-function-spec-type)

(defun document-presentation-to-command-translator
       (translator presentation context-type frame event window x y stream)
  ;; If we're translating to a command, it's a pretty sure bet that
  ;; we can run the body.  The command name will surely provide enough
  ;; information, and it's faster than unparsing the whole command.
  ;; If the user wants more, he can do it himself.
  (catch 'no-translation
    (let ((command-name
            (or (presentation-translator-command-name translator)
                (command-name
                  (call-presentation-translator translator presentation context-type
                                                frame event window x y)))))
      (with-presentation-type-parameters (command context-type)
        (with-stack-list (type 'command-name ':command-table command-table)
          ;; Use a lower level function for speed
          (return-from document-presentation-to-command-translator
            (funcall-presentation-generic-function present
              command-name type stream +textual-view+))))))
  (format stream "Command translator ~S" (presentation-translator-name translator))
  (values))


;;; Command menu translators

(define-presentation-translator command-menu-element-to-command
    (command-menu-element command global-command-table
     :tester
       ((object event)
        (let* ((menu-item (third object))
               (type (command-menu-item-type menu-item)))
          (and (or (eq type ':command)
                   (eq type ':function))
               (command-enabled
                 (command-name (extract-command-menu-item-value menu-item event))
                 *application-frame*))))
     :tester-definitive t
     ;; The pointer-documentation uses this, too
     :documentation
       ((object presentation context-type frame event window x y stream)
        (let ((documentation (getf (command-menu-item-options (third object)) :documentation)))
          (if documentation
              (write-string documentation stream)
              (document-presentation-to-command-translator
                (find-presentation-translator 'command-menu-element-to-command
                                              'global-command-table)
                presentation context-type frame event
                window x y stream)))))
    (object event)
  (extract-command-menu-item-value (third object) event))

(define-presentation-translator command-menu-element-to-sub-menu
    (command-menu-element command global-command-table
     :tester
       ((object)
        (let ((menu-item (third object)))
          (and (eq (command-menu-item-type menu-item) ':menu)
               (let ((comtab (find-command-table
                               (command-menu-item-value menu-item)
                               :errorp nil)))
                 (and comtab
                      (slot-value comtab 'menu)
                      ;; You might think that including the following in the
                      ;; tester is reasonable (that is, make the translator
                      ;; applicable only if the context's command table inherits
                      ;; from the command table that this menu item points to.)
                      ;; Unfortunately, that doesn't work because command table
                      ;; inheritance isn't the only way to get something into a
                      ;; command table.  The only alternative is to recursively
                      ;; walk over the sub-menu(s) to see if at least one command
                      ;; is applicable.
                      #+++ignore
                      (with-presentation-type-parameters (command context-type)
                        (command-table-presentation-subtypep command-table comtab)))))))
     :tester-definitive t
     ;; The pointer-documentation uses this, too
     :documentation
       ((object stream)
        (write-string (first object) stream)
        (write-char #\Space stream)
        (write-string "Menu" stream)))
    (object window)
  (values
    (menu-execute-command-from-command-table
      (command-menu-item-value (third object))
      :associated-window window :cache t)))
