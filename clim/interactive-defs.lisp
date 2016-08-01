;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;; For communication through parsers to lower levels.
(defvar *input-wait-test* nil)
(defvar *input-wait-handler* nil)
(defvar *pointer-button-press-handler* nil)

(defparameter *abort-gestures* '(:abort))

(defparameter *asynchronous-abort-gestures* '(:asynchronous-abort))

(defvar *accelerator-gestures* nil)
(defvar *accelerator-numeric-argument* nil)


;; Noise strings and ACCEPT results

(eval-when (compile load eval)
(defstruct (noise-string (:constructor make-noise-string-1))
  display-string
  text-style
  unique-id)

(defstruct (accept-result (:constructor make-accept-result-1)
                          (:include noise-string))
  presentation-type
  presentation-object)
)        ;eval-when

(defvar *noise-string-style* (make-text-style nil nil :smaller))
(defun-inline make-noise-string (&key display-string unique-id text-style)
  (make-noise-string-1 :display-string display-string
                       :text-style (or text-style *noise-string-style*)
                       :unique-id unique-id))

(defvar *accept-result-style* (make-text-style nil :italic nil))
(defun-inline make-accept-result (&key display-string unique-id
                                       presentation-type presentation-object)
  (make-accept-result-1 :display-string display-string
                        :text-style *accept-result-style*
                        :unique-id unique-id
                        :presentation-type presentation-type
                        :presentation-object presentation-object))

;;--- Kludge for processing asynchronous presentation events...
(defvar *input-buffer-empty* t)

;; Activation gestures terminate the entire input line.  They are usually
;; non-printing characters such as #\Newline or #\End.
(defvar *activation-gestures* nil)

(defvar *standard-activation-gestures* '(:newline :return :end))

(defmacro with-activation-gestures ((additional-gestures &key override)
                                    &body body &environment env)
  #-(or Genera Minima) (declare (ignore env))
  (when (characterp additional-gestures)        ;yes, we mean CHARACTERP
    (setq additional-gestures `'(,additional-gestures)))
  `(with-stack-list*
       (*activation-gestures*
         ,additional-gestures
         ,(cond ((constantp override #+(or Genera Minima) env)
                 (if (null (eval override #+(or Genera Minima-Developer) env))
                     '*activation-gestures*
                     nil))
                (t
                 `(unless ,override *activation-gestures*))))
     ,@body))

(defun activation-gesture-p (gesture)
  (and (not (typep gesture '(or pointer-event noise-string end-of-file-marker)))
       (dolist (set *activation-gestures*)
         (when (if (listp set)
                   (member gesture set
                           :test #'keyboard-event-matches-gesture-name-p)
                   (funcall set gesture))
           (return-from activation-gesture-p t)))))


;; Delimiter gestures terminate a field in an input line.  They are usually
;; printing characters such as #\Space or #\Tab
(defvar *delimiter-gestures* nil)

(defmacro with-delimiter-gestures ((additional-gestures &key override)
                                   &body body &environment env)
  #-(or Genera Minima) (declare (ignore env))
  (when (characterp additional-gestures)        ;yes, we mean CHARACTERP
    (setq additional-gestures `'(,additional-gestures)))
  `(with-stack-list*
       (*delimiter-gestures*
         ,additional-gestures
         ,(cond ((constantp override #+(or Genera Minima) env)
                 (if (null (eval override #+(or Genera Minima-Developer) env))
                     '*delimiter-gestures*
                     nil))
                (t
                 `(unless ,override *delimiter-gestures*))))
     ,@body))

(defun delimiter-gesture-p (gesture)
  (and (not (typep gesture '(or pointer-event noise-string end-of-file-marker)))
       (dolist (set *delimiter-gestures*)
         (when (if (listp set)
                   (member gesture set
                           :test #'keyboard-event-matches-gesture-name-p)
                   (funcall set gesture))
           (return-from delimiter-gesture-p t)))))


;;; Reading and writing of tokens

(defparameter *quotation-character* #\")

;; READ-TOKEN reads characters until it encounters an activation gesture,
;; a delimiter character, or something else (like a mouse click).
(defun read-token (stream &key input-wait-handler pointer-button-press-handler
                               click-only timeout)
  (with-temporary-string (string :length 50 :adjustable t)
    (let* ((gesture nil)
           (gesture-type nil)
           (quote-seen nil)
           (old-delimiters *delimiter-gestures*)
           (*delimiter-gestures* *delimiter-gestures*))
      (flet ((return-token (&optional unread)
               (when unread
                 (unread-gesture unread :stream stream))
               (when (and (activation-gesture-p unread)
                          (input-editing-stream-p stream))
                 (rescan-if-necessary stream))
               (return-from read-token
                 (values (evacuate-temporary-string string)))))
        (loop
          (multiple-value-setq (gesture gesture-type)
            (stream-read-gesture stream
				 :input-wait-handler
				 (or input-wait-handler
				     *input-wait-handler*)
				 :pointer-button-press-handler
				 (or pointer-button-press-handler
				     *pointer-button-press-handler*)
				 :timeout (and click-only timeout)))
          (cond ((eq gesture-type :timeout)
                 (return-from read-token :timeout))
                ((and click-only (eq gesture :eof))
                 (error "Got an EOF when waiting for mouse input"))
                ((eq gesture :eof)
                 (return-token gesture))
                ((and click-only
                      (not (typep gesture 'pointer-button-event)))
                 (beep stream))
                ((typep gesture 'pointer-button-event)
                 ;; No need to do anything, since this should have been handled
                 ;; in the presentation type system already
                 )
                ((characterp gesture)
                 (cond ((and (zerop (fill-pointer string))
                             (eql gesture *quotation-character*))
                        (setq quote-seen t)
                        (setq *delimiter-gestures* nil))
                       ((and quote-seen
                             (eql gesture *quotation-character*))
                        (setq quote-seen nil)
                        (setq *delimiter-gestures* old-delimiters))
                       ((activation-gesture-p gesture)
                        (return-token gesture))
                       ((delimiter-gesture-p gesture)
                        ;; ditto?
                        (return-token gesture))
                       ((or (ordinary-char-p gesture)
                            (diacritic-char-p gesture))
                        (vector-push-extend gesture string)
                        ;;--- haven't updated WRITE-CHAR yet
                        #+++ignore (write-char gesture stream))
                       (t (beep stream))))
                ((activation-gesture-p gesture)
                 (return-token gesture))
                ((delimiter-gesture-p gesture)
                 (return-token gesture))
                (t (beep stream))))))))

(defun write-token (token stream &key acceptably)
  (cond ((and acceptably (some #'delimiter-gesture-p token))
         (write-char *quotation-character* stream)
         (write-string token stream)
         (write-char *quotation-character* stream))
        (t
         (write-string token stream))))


;;; Input editor macros

;; The collected numeric argument, not fully implemented
(defvar *numeric-argument* nil)

;; The kill ring
(defvar *kill-ring*)
(defvar *kill-ring-application* nil)

;; Used for passing information from ACCEPT through the input editor to here
(defvar *presentation-type-for-yanking* nil)

;; WITH-INPUT-EDITING simply encapsulates the stream and sets up an editing
;; context that allows rescanning, etc.

(defvar *default-input-editing-stream-class*
#+allegro
    (excl:ics-target-case
     (:+ics #+mswindows 'standard-input-editing-stream
	    #-mswindows 'japanese-input-editing-stream)
     (:-ics 'standard-input-editing-stream))
#-allegro
	'standard-input-editing-stream)

(defmacro with-input-editing ((&optional stream
                               &key input-sensitizer initial-contents
                                    (class `*default-input-editing-stream-class*))
                              &body body)
  (default-input-stream stream with-input-editing)
  `(flet ((with-input-editing-body (,stream) ,@body))
     (declare (dynamic-extent #'with-input-editing-body))
     (invoke-with-input-editing ,stream #'with-input-editing-body
                                ,class ,input-sensitizer ,initial-contents)))

(defmacro with-input-editor-typeout ((&optional stream &key erase) &body body)
  (default-input-stream stream with-input-editor-typeout)
  `(flet ((with-ie-typeout-body (,stream) ,@body))
     (declare (dynamic-extent #'with-ie-typeout-body))
     (invoke-with-input-editor-typeout
       ,stream #'with-ie-typeout-body :erase ,erase)))


;;; Support for the errors and the Help key while inside ACCEPT

(defun display-input-editor-error (stream anerror)
  ;; Use *APPLICATION-FRAME* instead of (PANE-FRAME STREAM) because
  ;; when in non-own-window AVVs, (PANE-FRAME STREAM) points to the
  ;; calling frame rather than the AVV frame.
  (frame-manager-display-input-editor-error
    (frame-manager *application-frame*) 
    *application-frame* stream anerror))

(defvar *accept-help* nil)

(defmacro with-input-editor-help (stream &body body)
  `(flet ((with-input-editor-help-body (,stream) ,@body))
     (declare (dynamic-extent #'with-input-editor-help-body))
     (let* ((frame (pane-frame stream))
            (framem (and frame (frame-manager frame))))
       (frame-manager-display-help
         framem frame stream #'with-input-editor-help-body))))

;; ACTION is either :HELP, :POSSIBILITIES, or :APROPOS-POSSIBILITIES
(defun display-accept-help (stream action string-so-far)
  (with-input-editor-help stream
    (flet ((find-help-clauses-named (help-name)
             (let ((clauses nil))
               (dolist (clause *accept-help* clauses)
                 (when (eq (caar clause) help-name)
                   (push clause clauses)))))
           (display-help-clauses (help-clauses)
             (dolist (clause help-clauses)
               (let ((type (first clause))
                     (args (rest clause)))
                 (declare (ignore type))
                 (fresh-line stream)
                 (typecase (first args)
                   (string (format stream (first args)))
                   (function
                     (apply (first args) stream action string-so-far (rest args))))))))
      (declare (dynamic-extent #'find-help-clauses-named #'display-help-clauses))
      (let ((top-level-help-clauses
              (find-help-clauses-named :top-level-help))
            (subhelp-clauses
              (find-help-clauses-named :subhelp)))
        (cond ((null top-level-help-clauses)
               (fresh-line stream)
               (write-string "No top-level help specified.  Check the code." stream))
              (t (display-help-clauses top-level-help-clauses)))
        (when subhelp-clauses
          (display-help-clauses subhelp-clauses))))))

;; OPTIONS is a list of a help type followed by a help string (or a function
;; of two arguments, a stream and the help string so far) A "help type" is
;; either a single keyword (either :TOP-LEVEL-HELP or :SUBHELP), or a list
;; consisting of the type and a suboption (:OVERRIDE, :APPEND, or
;; :ESTABLISH-UNLESS-OVERRIDDEN).
;; Specifying :SUBHELP means "Append to previous subhelp, unless an outer
;; context has established an :OVERRIDE".
;; Specifying (:SUBHELP :APPEND) means append no matter what.
;; Specifying (:SUBHELP :OVERRIDE) means "This is the subhelp, subject to
;; lower-level explicit :APPENDs, unless someone above has already :OVERRIDden us.
;; Specifying (<type> :ESTABLISH-UNLESS-OVERRIDDEN) means "Establish <type>
;; at this level, unless someone above has already established <type>."  It does
;; not imply :APPENDING.
(defmacro with-accept-help (options &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (check-type options list)
  (assert (every #'listp options))
  (dolist (option options)
    (let* ((option-name-spec (if (symbolp (first option))
                                `(,(first option) :normal)
                                (first option)))
           (option-name (first option-name-spec))
           (option-type (second option-name-spec))
           (option-args (rest option)))
      (check-type option-name (member :top-level-help :subhelp))
      (check-type option-type (member :normal :append :override :establish-unless-overridden))
      (setq body
            `((with-stack-list* (*accept-help*
                                  (list ',option-name-spec ,@option-args) *accept-help*)
                ,@(cond ((eq option-type :override)
                         `((if (assoc (caar *accept-help*) (rest *accept-help*)
                                      :test #'(lambda (a b)
                                                (and (eq (first a) (first b))
                                                     (member :override (rest b)))))
                               (pop *accept-help*)
                               (setq *accept-help*
                                     (cons (first *accept-help*)
                                           (delete ,option-name (rest *accept-help*)
                                                   :test #'(lambda (a b)
                                                             (eq (caar b) a))))))))
                        ((eq option-type :append)
                         )
                        ((eq option-type :establish-unless-overridden)
                         `((when (assoc (caaar *accept-help*) (rest *accept-help*)
                                        :key #'first)
                             (pop *accept-help*))))
                        (t
                         `((when (assoc (caar *accept-help*) (rest *accept-help*)
                                        :test #'(lambda (a b)
                                                  (and (eq (first a) (first b))
                                                       (member :override (rest b)))))
                             (pop *accept-help*)))))
                ,@body)))))
  `(progn ,@body))
