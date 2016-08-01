;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;; VECTOR must be adjustable...
(defun extend-vector (vector token)
  (let* ((vector-max-length (array-dimension vector 0))
         (vector-end (length vector))
         (desired-length (+ vector-end (length token))))
    (when (> desired-length vector-max-length)
      (adjust-array vector desired-length))
    (when (array-has-fill-pointer-p vector)
      (setf (fill-pointer vector) desired-length))
    (replace vector token :start1 vector-end))
  vector)

;; This is just to prevent extraneous consing in COMPLETE-INPUT
(defvar *magic-completion-gestures*
        (append *completion-gestures* *help-gestures*
                *possibilities-gestures* *apropos-possibilities-gestures*))

(define-presentation-type completer (&key stream function possibility-printer
                                          prefix location))

(define-presentation-method presentation-typep (object (type completer))
  (declare (ignore object))
  nil)

(define-presentation-method presentation-subtypep ((subtype completer) supertype)
  (declare (ignore supertype))
  t)

(define-condition simple-completion-error (simple-parse-error) ())
(define-condition empty-completion-error (simple-completion-error) ())

;; Note that the usual completion character, Tab, may get filtered out
;; by xm-silica::discard-accelerator-event-p and thus never be seen by the completer.
(defun complete-input (stream function
                       &key partial-completers allow-any-input possibility-printer
                            (help-displays-possibilities t))
  (declare (dynamic-extent function))
  (declare (values answer-object success string))
  (with-temporary-string (stuff-so-far :length 100 :adjustable t)
   (with-delimiter-gestures (partial-completers)
    (with-activation-gestures (*magic-completion-gestures*)
     (flet ((completion-help (stream action string-so-far)
              (declare (ignore string-so-far))
              (display-completion-possibilities
                stream function stuff-so-far
                :possibility-printer possibility-printer
                :possibility-type
                  (if (eq action :help) 
                      (and help-displays-possibilities :possibilities)
                      action))))
      (declare (dynamic-extent #'completion-help))
      (with-accept-help ((:subhelp #'completion-help))
       ;; Keep the input editor from handling help and possibilities gestures.
       ;; They will get treated as activation gestures, thus ensuring that 
       ;; STUFF-SO-FAR will be accurate when we display the possibilities.
       (let ((*ie-help-enabled* nil)
             (location (stream-scan-pointer stream))
             token ch
             unread return extend
             completion-mode completion-type
             answer-object)
        (flet ((ends-in-char-p (string char)
                 (let ((sl (length string)))
                   (and (plusp sl)
                        (char-equal (aref string (1- sl)) char)))))
         (declare (dynamic-extent #'ends-in-char-p))
         (loop
           (setq unread nil return nil extend nil)
           (block get-input
             (loop
               (with-input-context (`(completer :stream ,stream
                                                :function ,function
                                                :possibility-printer
                                                ,possibility-printer 
                                                :prefix ,stuff-so-far
                                                :location ,location)) ()
                   (return-from get-input
                     (progn 
                       (setq token (read-token stream))
                       (setq ch (read-gesture :stream stream))))
                   (t nil))))
           (extend-vector stuff-so-far token)
           (cond ((null ch)
                  (error "Null character?"))
                 ((keyboard-event-p ch)
                  (cond ((member ch *help-gestures* 
                                 :test #'keyboard-event-matches-gesture-name-p)
                         (setq completion-mode ':help))
                        ((member ch *possibilities-gestures* 
                                 :test #'keyboard-event-matches-gesture-name-p)
                         (setq completion-mode ':possibilities))
                        ((member ch *apropos-possibilities-gestures* 
                                 :test #'keyboard-event-matches-gesture-name-p)
                         (setq completion-mode ':apropos-possibilities))
                        ((member ch *completion-gestures*
                                 :test #'keyboard-event-matches-gesture-name-p)
                         (setq completion-mode ':complete-maximal
                               ;; If the completion fails, unread this char
                               ;; so that a higher level gets the chance to
                               ;; try the completion again.  For example, when
                               ;; several completion types are OR'ed together.
                               unread 'unless-completed))
                        ((member ch partial-completers 
                                 :test #'keyboard-event-matches-gesture-name-p)
                         (setq completion-mode ':complete-limited
                               unread t extend t return 'if-completed))
                        ;; What about "overloaded" partial completers??
                        ((delimiter-gesture-p ch)
                         (setq completion-mode (if allow-any-input nil ':complete)
                               unread t extend t return t))
                        ((activation-gesture-p ch)
                         (setq completion-mode (if allow-any-input nil ':complete) 
                               unread t return t))))
                 ((eq ch *end-of-file-marker*)
                  (setq completion-mode (if allow-any-input nil ':complete) 
                        return t))
                 (t                                ;mouse click?
                  (beep stream)))

           ;; OK, this is a SPECIAL case.  We check to see if the null string
           ;; was read, and if so, we signal a parse-error (because ACCEPT
           ;; handles this specially) so that the default value will be filled
           ;; in by ACCEPT.
           ;; There is a tension here between wanting to fill in the default and
           ;; use the maximal left substring when the user types #\End or a field
           ;; terminator that also does completion.  Putting this check before the
           ;; completion code means that the default always wins.
           (when (and return (zerop (fill-pointer stuff-so-far)))
             (when (eq unread t)
               (unread-gesture ch :stream stream))
             (when (input-editing-stream-p stream)
               (rescan-if-necessary stream))
             (signal 'empty-completion-error
                     :format-string "Attempting to complete the null string")
             (when (eq ch *end-of-file-marker*)
               (error 'empty-completion-error
                       :format-string "Attempting to complete the null string")))

           (cond ((member completion-mode '(:help :possibilities :apropos-possibilities))
                  ;; Since we've asked the input editor not to do this,
                  ;; we must do it here ourselves
                  (display-accept-help stream completion-mode "")
                  (setq completion-type nil))
                 (completion-mode
                  (multiple-value-bind (string success object nmatches)
                      (funcall function stuff-so-far completion-mode)
                    (setq answer-object object)
                    (cond ((= nmatches 0)
                           ;; No valid completion, so no replace input
                           (setq completion-type 'invalid)
                           (when extend
                             (vector-push-extend ch stuff-so-far)))
                          ((= nmatches 1)
                           (setq completion-type (if success 'unique 'ambiguous))
                           ;; Replace contents of stuff-so-far with completion
                           (setf (fill-pointer stuff-so-far) 0)
                           (extend-vector stuff-so-far string)
                           )
                          ((> nmatches 1)
                           (setq completion-type 'ambiguous)
                           ;; Replace contents of stuff-so-far with completion
                           (setf (fill-pointer stuff-so-far) 0)
                           (extend-vector stuff-so-far string)
                           ;; Need-to-add-delimiter test??
                           (when (and extend
                                      (not (ends-in-char-p string ch)))
                             (vector-push-extend ch stuff-so-far))))))
                 (t (setq answer-object nil)))

           ;; Check for errors unconditionally, remembering that we may not have
           ;; called the completer at all (completion-type = NIL)
           (ecase completion-type
             ((nil unique left-substring))        ;no possible errors to report
             (invalid
               (unless allow-any-input
                 (when unread
                   (unread-gesture ch :stream stream))
                 (signal 'simple-completion-error
                         :format-string "Invalid completion: ~A"
                         :format-arguments (list (evacuate-temporary-string stuff-so-far)))))
             (ambiguous
               ;; Only beep on ambiguous full completions, in either ALLOW-ANY-INPUT mode
               (when (eq completion-mode :complete)
                 (beep stream))))

           (when (eq return 'if-completed)
             (unless (eq completion-type 'unique)
               (setq return nil)))

           ;; Decide whether or not to return, remembering that
           ;; we might have called the completer.
           (when return
              (when (or (member completion-type '(nil unique left-substring))
                       allow-any-input)
               ;; Leave the last delimiter for our caller
               (when (eq unread t)
                 (unread-gesture ch :stream stream))
               ;; Must replace-input after unread-gesture so the delimiter is unread
               ;; into the input editor's buffer, not the underlying stream's buffer
               (unless (stream-rescanning-p stream)
                 (replace-input stream stuff-so-far :buffer-start location))
               (return-from complete-input
                 (values answer-object t (evacuate-temporary-string stuff-so-far))))
             (when (eq ch *end-of-file-marker*)
               (error "Eof when encountered when trying to complete")))

           ;; Not returning yet, but update the input editor's buffer anyway
           (unless (stream-rescanning-p stream)
             (replace-input stream stuff-so-far :buffer-start location)))))))))))

;; DISPLAY-POSSIBILITIES
(defun display-completion-possibilities (stream function stuff-so-far
                                         &key possibility-printer 
                                              (possibility-type :possibilities))
  (when possibility-type
    (fresh-line stream)
    (multiple-value-bind (string success object nmatches possibilities)
        (funcall function stuff-so-far possibility-type)
      (declare (ignore string object success))
      (if (or (= nmatches 0) (null possibilities))
          (write-string "There are no possible completions" stream)
        ;;--- Just using the type from the innermost context is far too simplistic
        ;; Be sure to un-stack-cons the context type
        (let ((type (evacuate-list
                      (input-context-type (first *input-context*)))))
          (flet ((print-possibility (possibility stream)
                   (cond (possibility-printer
                          (funcall possibility-printer possibility type stream))
                         (type
                          (present (second possibility) type :stream stream))
                         (t
                          (format stream "~A" (first possibility))))))
            (declare (dynamic-extent #'print-possibility))
            (cond ((= nmatches 1)
                   (write-string "The only possible completion is:" stream)
                   (fresh-line stream)
                   (print-possibility (first possibilities) stream))
                  (t
                   (write-string "The possible completions are:" stream)
                   (fresh-line stream)
                   (formatting-table (stream :multiple-columns t)
                     (dolist (possibility possibilities)
                       (formatting-row (stream)
                         (formatting-cell (stream)
                           (print-possibility possibility stream)))))))))))))

(defvar *null-object* '#:null)

;; Complete STRING chunk-wise against the completion possibilities in the
;; COMPLETIONS, using DELIMITERS to break the strings into chunks.  ACTION
;; should be :COMPLETE, :COMPLETE-LIMITED, :COMPLETE-MAXIMAL, or
;; :POSSIBILITIES (see below).  NAME-KEY and VALUE-KEY are used to extract
;; the completion string and object from the entries in COMPLETIONS, and
;; PREDICATE (if supplied) is applied to filter out unwanted objects.
;; Returns five values, the completed string, whether or not the completion
;; successfully matched, the object associated with the completion, the
;; number of things that matches, and (if ACTION is :POSSIBILITIES) a list
;; of possible completions.
;;
;; When ACTION is :COMPLETE, this completes the input as much as possible,
;; except that if the user's input exactly matches one of the possibilities,
;; even if it is a left substring of another possibility, the shorter
;; possibility is returned as the result.
;; When ACTION is :COMPLETE-LIMITED, this completes the input up to the next
;; partial delimiter.
;; When ACTION is :COMPLETE-MAXIMAL, this completes the input as much as possible.
;; When ACTION is :POSSIBILITIES or :APROPOS-POSSIBILITIES, this returns a list
;; of the possible completions.
(defun complete-from-possibilities (string completions delimiters
                                    &key (action :complete) predicate
                                         (name-key #'first) (value-key #'second))
  (declare (values string success object nmatches possibilities))
  (when (and (not (eq action :possibilities))
             (not (eq action :apropos-possibilities))
             (zerop (length string)))
    (return-from complete-from-possibilities 
      (values nil nil nil 0 nil)))
  (let* ((best-completion nil)
         (best-length nil)
         (best-object *null-object*)
         (nmatches 0)
         (possibilities nil))
    (flet ((complete-1 (possibility)
             (let ((completion (funcall name-key possibility))
                   (object (funcall value-key possibility)))
               (when (or (null predicate)
                         (funcall predicate object))
                 ;; If we are doing simple completion and the user-supplied string is
                 ;; exactly equal to this completion, then claim success (even if there
                 ;; are other completions that have this one as a left substring!).
                 (when (and (eq action :complete)
                            (string-equal string completion))
                   (return-from complete-from-possibilities
                     (values completion t object 1)))
                 (multiple-value-setq
                     (best-completion best-length best-object nmatches possibilities)
                   (chunkwise-complete-string string completion object action delimiters
                                              best-completion best-length best-object
                                              nmatches possibilities))))))
      (declare (dynamic-extent #'complete-1))
      (map nil #'complete-1 completions))
    (values (if best-completion (subseq best-completion 0 best-length) string)
            (not (eq best-object *null-object*))
            (if (eq best-object *null-object*) nil best-object)
            nmatches
            (nreverse possibilities))))

;; Just like COMPLETE-FROM-POSSIBILITIES, except that the possibilities are
;; gotten by funcalling a generator rather than from a completion alist.
(defun complete-from-generator (string generator delimiters
                                &key (action :complete) predicate)
  (declare (values string success object nmatches possibilities))
  (declare (dynamic-extent generator))
  (when (and (not (eq action :possibilities))
             (not (eq action :apropos-possibilities))
             (zerop (length string)))
    (return-from complete-from-generator 
      (values nil nil nil 0 nil)))
  (let* ((best-completion nil)
         (best-length nil)
         (best-object *null-object*)
         (possibilities nil)
         (nmatches 0))
    (flet ((suggest-handler (completion object &optional presentation-type)
             (declare (ignore presentation-type))        ;for now
             (when (or (null predicate)
                       (funcall predicate object))
               (when (and (eq action :complete)
                          (string-equal string completion))
                 (return-from complete-from-generator
                   (values completion t object 1)))
               (multiple-value-setq
                   (best-completion best-length best-object nmatches possibilities)
                 (chunkwise-complete-string string completion object action delimiters
                                            best-completion best-length best-object
                                            nmatches possibilities)))))
      (declare (dynamic-extent #'suggest-handler))
      (funcall generator string #'suggest-handler))
    (values (if best-completion (subseq best-completion 0 best-length) string)
            (not (eq best-object *null-object*))
            (if (eq best-object *null-object*) nil best-object)
            nmatches
            (nreverse possibilities))))

;; The common subroutine used to do chunkwise completion.
;;--- Extending this to support completion aarrays is pretty straightforward
(defun chunkwise-complete-string (string completion object action delimiters
                                  best-completion best-length best-object
                                  nmatches possibilities)
  (declare (values best-completion best-length best-object nmatches possibilities))
  (let* ((length (length string))
         (matches (if (eq action :apropos-possibilities)
                      (if (search string completion :test #'char-equal) length 0)
                      (chunkwise-string-compare string completion delimiters))))
    (when (= matches length)
      (incf nmatches)
      (case action
        ((:possibilities :apropos-possibilities)
         (push (list completion object) possibilities))
        ((:complete :complete-maximal)
         nil)
        (:complete-limited
         ;; Match up only as many chunks as the user has typed
         (flet ((delimiter-p (char)
                  (member char delimiters)))
           (declare (dynamic-extent #'delimiter-p))
           (let* ((nchunks (1+ (count-if #'delimiter-p string)))
                  (cutoff (let ((start 0)
                                (cutoff nil))
                            (dotimes (i nchunks cutoff)
                              #-(or Genera Minima allegro aclpc) (declare (ignore i))
                              (let ((new (position-if #'delimiter-p completion :start start)))
                                (unless new (return nil))
                                (setq cutoff new
                                      start (1+ new)))))))
             (when cutoff
               (setq completion (subseq completion 0 (1+ cutoff)))
               ;; Increment this once more to make the higher level think
               ;; that the completion is ambiguous
               (incf nmatches))))))
      (cond (best-completion
             (let ((new-length (chunkwise-string-compare best-completion completion delimiters
                                                         t best-length)))
               (cond ((or (null best-length)
                          (> new-length best-length))
                      (setq best-length new-length
                            best-object object))
                     (t
                      (setq best-length new-length
                            best-object *null-object*)))))
            (t
             (setq best-completion (copy-seq completion)
                   best-length (length best-completion)
                   best-object object)))))
  (values best-completion best-length best-object nmatches possibilities))

;; Compare STRING1 against STRING2 in "chunks", using DELIMITERS to break
;; the strings into chunks.  Returns two values, the index of the first place
;; where the strings mismatches and the index of the last character that was
;; unambiguous.  When MERGE-P, STRING1 gets side-effected.
(defun chunkwise-string-compare (string1 string2 delimiters &optional merge-p end1)
  (declare (values matched ambiguous))
  (let ((len1 (or end1 (length string1)))
        (len2 (length string2))
        (matched 0)
        ambiguous
        (i1 0) (i2 0)
        char1 char2)
    (loop
      (unless (and (< i1 len1) (< i2 len2))
        (return))
      (setq char1 (aref string1 i1)
            char2 (aref string2 i2))
      (cond ((or (eql char1 char2)
                 (char-equal char1 char2))
             (when merge-p
               (setf (aref string1 matched) char1))
             (incf matched) (incf i1) (incf i2))
            (t
             (unless ambiguous
               (setq ambiguous matched))
             (cond ((member char1 delimiters)
                    (when (or (and (not merge-p)
                                   (> i1 matched))
                              (member char2 delimiters))
                      (return nil)))
                   ((member char2 delimiters)
                    (when (and (not merge-p)
                               (> i2 matched))
                      (return nil)))
                   (t (unless merge-p
                        (return nil))))
             (loop
               (when (or (member (aref string1 i1) delimiters)
                         (>= (incf i1) len1))
                 (return)))
             (loop
               (when (or (member (aref string2 i2) delimiters)
                         (>= (incf i2) len2))
                 (return))))))
    (values matched (or ambiguous matched))))
