;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: input-editor-commands.lisp,v 2.4 2003/12/15 18:35:12 layer Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Define some useful input editor commands.  For now, all these are defined
;;; on INPUT-EDITING-STREAM-MIXIN rather than on our specific implementation of
;;; an input editor.  This may prove to be a foolish decision down the pike.

(eval-when (compile load eval)
(defvar *ie-command-arglist* '(stream input-buffer gesture numeric-argument))
)

;; GESTURES is either a gesture name, or a list of gesture names.  FUNCTION
;; is the function that implements an input editor command.  This associates
;; the gesture(s) with the command.

;;-- It seems that an input editor gesture can only be one that
;;-- corresponds to a shift/control/meta/hyper character or one is a
;;-- member if this list. This seems silly but what else can we do?
;;--- What a kludge!  What should this really be?
;;-- Either we dont try to enfore the restriction or we have a better
;;-- way of telling. Perhaps we should try just to list the standard
;;-- printing characters.

(defvar *printable-keysyms*
    '(:space :\! :\" :\# :\$ :\% :\& :\' :\( :\) :\* :\+ :\, :\- :\. :\/ :\0 :\1
      :\2 :\3 :\4 :\5 :\6 :\7 :\8 :\9 :\: :\; :\< :\= :\> :\? :\@ :a :a :b
      :b :c :c :d :d :e :e :f :f :g
      :g :h :h :i :i :j :j :k :k :l
      :l :m :m :n :n :o :o :p :p :q
      :q :r :r :s :s :t :t :u :u :v
      :v :w :w :x :x :y :y :z :z :\[
      :\\ :\] :\^ :\_ :\` :\{ :\| :\}
      :\~))

(defun add-input-editor-command (gestures function
                                 &optional (command-array
                                            *input-editor-command-aarray*))
  (flet ((add-aarray-entry (gesture thing aarray)
           (let ((old (find gesture aarray :key #'first)))
             (if old
                 (setf (second old) thing)
               (vector-push-extend (list gesture thing) aarray))))
         ;; Does the gesture correspond to some character with any bucky
         ;; bits on, or some other non-standard non-printing character?
         (bucky-char-p (gesture)
           (multiple-value-bind (keysym modifier-state)
               (gesture-name-keysym-and-modifiers gesture)
             (when keysym
               (if (zerop modifier-state)
                   (not (member keysym *printable-keysyms*))
                 (logtest modifier-state
                          (make-modifier-state :control :meta :super :hyper)))))))
    (declare (dynamic-extent #'add-aarray-entry #'bucky-char-p))
    (cond ((atom gestures)
           (unless (bucky-char-p gestures)
             (warn "~S does not correspond to non-printing gesture. ~
This may confused the input editor" gestures))
           (add-aarray-entry gestures function command-array))
          (t
           (assert (> (length gestures) 1))
           (assert (bucky-char-p (first gestures)) (gestures)
             "~S does not correspond to non-printing gesture" gestures)
           ;; We've got a command that wil be bound to a sequence of gestures,
           ;; so set up the prefix tables.
           (let ((aarray command-array))
             (dorest (rest gestures)
                     (let* ((prefix (first rest))
                            (rest (rest rest)))
                       (if (null rest)
                           (add-aarray-entry prefix function aarray)
                         (let ((subaarray (second (find prefix aarray :key #'first))))
                           (when (null subaarray)
                             (setq subaarray (make-array 30 :fill-pointer 0 :adjustable t))
                             (add-aarray-entry prefix subaarray aarray))
                           (setq aarray subaarray))))))))))


      ;; When T, the input editor should handle help and completion.  Otherwise,
      ;; something like COMPLETE-INPUT will do it for us.
(defvar *ie-help-enabled* t)

;; These need to be on a per-implementation basis, naturally
;;--- If you change these, change *MAGIC-COMPLETION-GESTURES* too
(defvar *completion-gestures* '(:complete))
(defvar *help-gestures* '(:help))
(defvar *possibilities-gestures* '(:possibilities))
(defvar *apropos-possibilities-gestures* '(:apropos-possibilities))

(defun lookup-input-editor-command (gesture aarray)
  ;; Need to handle the help and possibilities commands specially so
  ;; that they work correctly inside of COMPLETE-INPUT
  (cond ((and *ie-help-enabled*
              (member gesture *help-gestures*
                      :test #'keyboard-event-matches-gesture-name-p))
         'com-ie-help)
        ((and *ie-help-enabled*
              (member gesture *possibilities-gestures*
                      :test #'keyboard-event-matches-gesture-name-p))
         'com-ie-possibilities)
        ((and *ie-help-enabled*
              (member gesture *apropos-possibilities-gestures*
                      :test #'keyboard-event-matches-gesture-name-p))
         'com-ie-apropos-possibilities)
        ((and *ie-help-enabled*
              (member gesture *completion-gestures*
                      :test #'keyboard-event-matches-gesture-name-p))
         'com-ie-complete)
        (t
         (let* ((keysym (keyboard-event-key-name gesture))
                (modifier-state (event-modifier-state gesture))
                (bucky-p
                 (logtest modifier-state
                          (make-modifier-state :control :meta :super :hyper))))
           (cond ((and (eq aarray *input-editor-command-aarray*)
                       bucky-p
                       ;; If it's a numeric argument, return the digit
                       (position keysym '#(:|0| :|1| :|2| :|3| :|4|
                                           :|5| :|6| :|7| :|8| :|9|))))
                 ((and (eq aarray *input-editor-command-aarray*)
                       bucky-p
                       (eq keysym ':|-|))
                  -1)
                 (t
                  (second (find gesture aarray
                                :key #'first
                                :test #'keyboard-event-matches-gesture-name-p))))))))

(defmacro define-input-editor-command ((name &key (rescan t) (type 'motion) history)
                                       arglist &body body)
  (multiple-value-bind (arglist ignores)
      (canonicalize-and-match-lambda-lists *ie-command-arglist* arglist)
    (let ((stream (first arglist)))
      `(define-group ,name define-input-editor-command
         (defun ,name ,arglist
           ,@(and ignores `((declare (ignore ,@ignores))))
           ,@body
           (setf (slot-value ,stream 'last-command-type) ',type)
           ,@(unless history `((setf (slot-value ,stream 'previous-history) nil)))
           ,@(ecase rescan
               ((t) `((queue-rescan ,stream)))
               (:immediate `((immediate-rescan ,stream)))
               ((nil) nil))
           (values))))))

#+Genera
(scl:defprop define-input-editor-command "CLIM Input Editor Command" si:definition-type-name)
#+Genera
(scl:defprop define-input-editor-command zwei:defselect-function-spec-finder
  zwei:definition-function-spec-finder)

(defmethod stream-process-gesture ((istream input-editing-stream-mixin)
                                   gesture type)
  (values gesture type))

;; No input editing commands are standard characters, unless we have just
;; read a prefix character
(defmethod stream-process-gesture ((istream input-editing-stream-mixin)
                                   (gesture character) type)
  (with-slots (numeric-argument last-command-type command-state
                                command-mode) istream
    (cond (#+allegro
	   (excl:ics-target-case
            (:+ics (eq command-mode *kana-input-editor-command-aarray*)))
           #-allegro nil
           (setq last-command-type 'character)
	   (excl:ics-target-case
            (:+ics (kana-process-gesture istream gesture type))))
          ((eq command-state command-mode)
           (setq last-command-type 'character)
           (values gesture type))
          (t
           (let* (#+aclpc (portgest (port-canonicalize-gesture-spec
                                      (port istream) gesture 0))
                  #+aclpc (keysym (car portgest))
                  #+aclpc (modifier-state (cdr portgest))
                  #+aclpc (gesture (allocate-event
                                     'key-press-event
                                     :sheet (encapsulating-stream-stream
                                              istream)
                                     :character gesture
                                     :key-name keysym
                                     :modifier-state modifier-state)) 
                  #-aclpc
                  (gesture (destructuring-bind (keysym . modifier-state)
                               (port-canonicalize-gesture-spec (port istream) gesture 0)
                             (allocate-event 'key-press-event
                                             :sheet (encapsulating-stream-stream istream)
                                             :character gesture
                                             :key-name keysym
                                             :modifier-state modifier-state)))
                  (command (lookup-input-editor-command gesture command-state)))
             (cond ((arrayp command)
                    ;; Another prefix, update the state
                    (setq command-state command))
                   (command
                    (let ((argument (or numeric-argument 1)))
                      (setq numeric-argument nil
                            command-state command-mode)
                      (funcall command
                               istream (slot-value istream 'input-buffer) gesture argument)))
                   (t
                    (beep istream)
                    (setq numeric-argument nil
                          command-state command-mode)))
             (deallocate-event gesture)
             nil)))))

(defmethod stream-process-gesture ((istream input-editing-stream-mixin)
                                   (gesture key-press-event) type)
  (with-slots (numeric-argument last-command-type command-state
                                command-mode) istream
    ;; The COMMAND-STATE slot holds the current IE command aarray, and
    ;; gets updated when we see a prefix (such as ESC or c-X)
    (let ((command (unless (activation-gesture-p gesture)
                     (lookup-input-editor-command gesture command-state))))
      (cond ((numberp command)
             (cond ((null numeric-argument)
                    (setq numeric-argument command))
                   ((= command -1)
                    (setq numeric-argument (- numeric-argument)))
                   (t
                    (setq numeric-argument (+ (* numeric-argument 10) command))))
             ;; Numeric arguments don't affect LAST-COMMAND-TYPE
             (return-from stream-process-gesture nil))
            ((arrayp command)
             ;; A prefix, update the state and return
             (setq command-state command)
             (return-from stream-process-gesture nil))
            (command
             (let ((argument (or numeric-argument 1)))
               (setq numeric-argument nil
                     command-state command-mode)
               (funcall command
                        istream (slot-value istream 'input-buffer) gesture argument))
             (return-from stream-process-gesture nil))
            ((eq command-state command-mode)
             (setq last-command-type 'character
                   command-state command-mode)
             (return-from stream-process-gesture
               (values gesture type)))
            (t
             (beep istream)
             (setq numeric-argument nil
                   command-state command-mode)
             (return-from stream-process-gesture nil))))))

;;--- This method never gets run because STREAM-READ-CHAR just gives up
;;--- Maybe it would be better to use a BLANK-AREA translator while in
;;--- the INPUT-EDITOR context?
(defmethod stream-process-gesture ((istream input-editing-stream-mixin)
                                   (gesture pointer-button-press-event) type)
  (when (eq (pointer-event-button gesture) +pointer-left-button+)
    (let ((position (compute-input-buffer-input-position
                      istream (pointer-event-x gesture) (pointer-event-y gesture))))
      (when position
        (setf (stream-insertion-pointer istream) position)
        (return-from stream-process-gesture (values nil nil)))))
  (values gesture type))

;;--- A work in progress...
(defmethod compute-input-buffer-input-position ((istream input-editing-stream-mixin) x y)
  (declare (ignore y))
  (multiple-value-bind (cursor-x cursor-y baseline height style max-x)
      (decode-stream-for-writing istream)
    ;; Cache some slot variables since we will not be writing them.
    (let* ((input-buffer (slot-value istream 'input-buffer))
           (stream (encapsulating-stream-stream istream))
           (medium (sheet-medium stream))
           (start 0)
           (end (fill-pointer input-buffer)))
      (setq max-x x)
      (setf start 0)
      (setf end (fill-pointer input-buffer))
      (multiple-value-setq (cursor-x cursor-y)
        (start-cursor-position istream))
      (let ((index
              (block index
                (do-input-buffer-pieces (input-buffer :start start :end end)
                                        (from to noise-string)
                  :normal
                    (multiple-value-bind (char index new-cursor-x new-baseline new-height)
                        (stream-scan-string-for-writing
                          stream medium input-buffer from to style cursor-x max-x)
                      (declare (ignore char new-baseline new-height))
                      (when (> cursor-x max-x)
                        (return-from index nil))
                      (when (< index to)
                        (return-from index index))
                      (setq cursor-x new-cursor-x))
                  :noise-string
                    (let ((style (merge-text-styles
                                   (noise-string-text-style noise-string) style)))
                      (multiple-value-setq (cursor-x cursor-y height baseline)
                        (do-text-screen-real-estate
                          stream #'true (noise-string-display-string noise-string) 0 nil
                          cursor-x cursor-y height baseline style max-x))))
                nil)))
        index))))


;;; Help commands, handled by special magic

(define-input-editor-command (com-ie-help :rescan nil)
                             (stream)
  "Display completion help"
  (display-accept-help stream :help ""))

(define-input-editor-command (com-ie-possibilities :rescan nil)
                             (stream)
  "Display completion possibilities"
  (display-accept-help stream :possibilities ""))

(define-input-editor-command (com-ie-apropos-possibilities :rescan nil)
                             (stream)
  "Display completion apropos possibilities"
  (display-accept-help stream :apropos-possibilities ""))

(define-input-editor-command (com-ie-complete :rescan nil)
                             (stream input-buffer)
  "Complete the current symbol"
  (multiple-value-bind (string ambiguous word-start)
      (complete-symbol-name stream input-buffer)
    (when string
      (replace-input stream string :buffer-start word-start))
    (when (or ambiguous (null string))
      (beep stream))
    (when string
      (queue-rescan stream ':activation))))

(defun complete-symbol-name (stream input-buffer &aux (colon-string ":"))
  (declare (values string ambiguous word-start))
  (multiple-value-bind (word-start word-end colon)
      (word-start-and-end input-buffer '(#\space #\( #\) #\")
                          (stream-insertion-pointer stream))
    (when word-end
      (with-temporary-substring
          (package-name input-buffer word-start (or colon word-start))
        (when (and colon
                   (< colon word-end)
                   (char-equal (aref input-buffer (1+ colon)) #\:))
          (incf colon)
          (setq colon-string "::"))
        (with-temporary-substring
            (symbol-name input-buffer (if colon (1+ colon) word-start) word-end)
          (multiple-value-bind (new-symbol-name success object nmatches)
              (complete-symbol-name-1 symbol-name)
            (declare (ignore success object))
            (when (and new-symbol-name (not (zerop nmatches)))
              (return-from complete-symbol-name
                (values
                  (if (and colon (> colon word-start))
                      (format nil "~A~A~A" package-name colon-string new-symbol-name)
                      new-symbol-name)
                  (/= nmatches 1)
                  word-start)))))))))

#+Genera
(defun complete-symbol-name-1 (string)
  (complete-from-possibilities
    string (scl:g-l-p zwei:*zmacs-completion-aarray*) '(#\-)
    :action :complete-maximal))

#-Genera
(defun complete-symbol-name-1 (string)
  string                        ;-- fix me
  nil)


(define-input-editor-command (com-ie-input-editor-help :rescan nil)
                             (stream)
  "Display input editor help"
  (with-input-editor-typeout (stream)
    (formatting-table (stream :x-spacing "  "
                              :multiple-columns t
                              :multiple-columns-x-spacing "    ")
      (dovector (entry (slot-value stream 'command-mode))
        (let ((gesture-name (first entry))
              (command (second entry)))
          (when (symbolp command)
            (dolist (gesture (gesture-specs-from-gesture-name gesture-name))
              (formatting-row (stream)
                (formatting-cell (stream)
                  (describe-gesture-spec gesture :stream stream :brief t))
                (formatting-cell (stream)
                  (format stream "~A" (or (documentation command 'function)
                                          command)))))))))))


(define-input-editor-command (com-ie-refresh :rescan nil)
                             (stream)
  "Refresh the interactor window"
  (window-refresh stream)
  ;;-- This is being done twice now
  (redraw-input-buffer stream))

;;--- It would be nice to have save/restore scroll position, and scroll searching
(define-input-editor-command (com-ie-scroll-forward :rescan nil)
                             (stream numeric-argument)
  "Scroll the display window forward"
  (ie-scroll-window numeric-argument :up stream))

(define-input-editor-command (com-ie-scroll-backward :rescan nil)
                             (stream numeric-argument)
  "Scroll the display window backward"
  (ie-scroll-window numeric-argument :down stream))

(define-input-editor-command (com-ie-scroll-left :rescan nil)
                             (stream numeric-argument)
  "Scroll the display window left"
  (ie-scroll-window numeric-argument :left stream))

(define-input-editor-command (com-ie-scroll-right :rescan nil)
                             (stream numeric-argument)
  "Scroll the display window right"
  (ie-scroll-window numeric-argument :right stream))

;; Scroll the frame's standard output stream in some direction by some amount,
;; one screenful being the default.
(defun ie-scroll-window (distance direction &optional istream)
  (let* ((window (frame-standard-output *application-frame*))
         (history (and window
                       (output-recording-stream-p window)
                       (stream-output-history window)))
         (x-direction (case direction
                        (:left +1)
                        (:right -1)
                        (otherwise 0)))
         (y-direction (case direction
                        (:up +1)
                        (:down -1)
                        (otherwise 0))))
    (when (and window
               (pane-viewport window))
      (multiple-value-bind (x y) (window-viewport-position window)
        (multiple-value-bind (width height)
            (bounding-rectangle-size (window-viewport window))
          (incf x (* (if (= distance 1)
                         width
                         (* distance (stream-character-width window #\0)))
                     x-direction))
          (incf y (* (if (= distance 1)
                         height
                         (* distance (stream-line-height window)))
                     y-direction))
          (with-bounding-rectangle* (hleft htop hright hbottom) history
            (setq x (min (max hleft x) hright))
            (setq y (min (max htop y) hbottom)))
          (window-set-viewport-position window x y)
          (when (and istream
                     (eq window (encapsulating-stream-stream istream))
                     (multiple-value-bind (x-pos y-pos)
                         (input-buffer-input-position->cursor-position istream)
                       (and (<= x x-pos (+ x width))
                            (<= y y-pos (+ y height)))))
            ;; When we are scrolling the same window that we are doing input
            ;; editing on, redraw the input buffer.  Don't bother redrawing
            ;; if the input line is not on the screen.  This will redraw
            ;; more than it needs to, but who cares -- it's fast enough.
            (with-end-of-page-action (istream :allow)
              (redraw-input-buffer istream))))))))


;;; Some macrology for talking about the input-buffer

(defun-inline ie-line-start (buffer pointer)
  (1+ (or (position #\Newline buffer :end pointer :from-end t)
          -1)))

(defun-inline ie-line-end (buffer pointer)
  (or (position #\Newline buffer :start pointer)
      (fill-pointer buffer)))

;; Things which move over words must move over whitespace until they see
;; alphanumerics, then alphanumerics until they see whitespace.
(defun move-over-word (buffer start-position reverse-p)
  (flet ((word-break-character-p (thing)
           (when (and (characterp thing)
                      (not (alphanumericp thing)))
             (values t t)))
         (word-character-p (thing)
           (when (and (characterp thing)
                      (alphanumericp thing))
             (values t t))))
    (declare (dynamic-extent #'word-break-character-p #'word-character-p))
    (setq start-position
          (forward-or-backward buffer start-position reverse-p #'word-character-p))
    (when start-position
      (let ((position
              (forward-or-backward buffer start-position reverse-p
                                   #'word-break-character-p)))
        (and position
             (min position (fill-pointer buffer)))))))

(defun move-over-sexp (buffer start-position reverse-p)
  (setq start-position (move-over-whitespace buffer start-position reverse-p))
  (when start-position
    (let ((start-char (if reverse-p
                          (aref buffer (max 0 (1- start-position)))
                          (aref buffer start-position))))
      (unless reverse-p
        (case start-char
          (#\#
           (setq start-position (forward-or-backward buffer start-position nil #'true))
           (when start-position
             (setq start-position (forward-or-backward buffer start-position nil #'true)))
           (when start-position
             (setq start-char (aref buffer start-position))))
          ((#\' #\,)
           (setq start-position (forward-or-backward buffer start-position nil #'true))
           (when start-position
             (setq start-char (aref buffer start-position))))))
      (cond ((or (eql start-char #\")
                 (eql start-char #\|))
             (move-over-matching-thing buffer start-position reverse-p start-char))
            ((or (and reverse-p (eql start-char #\) ))
                 (and (not reverse-p) (eql start-char #\( )))
             (move-over-cons buffer start-position reverse-p start-char))
            (t (move-over-atom buffer start-position reverse-p))))))

(defun move-over-matching-thing (buffer start-position reverse-p start-char)
  (flet ((matches-thing (thing)
           (when (eql thing start-char)
             (values t nil))))
    (declare (dynamic-extent #'matches-thing))
    (setq start-position
          (forward-or-backward buffer start-position reverse-p #'true))
    (when start-position
      (let ((position
              (forward-or-backward buffer start-position reverse-p #'matches-thing)))
        (and position
             (min position (fill-pointer buffer)))))))

(defun move-over-cons (buffer start-position reverse-p start-char)
  (let ((end-char (if (eql start-char #\( ) #\) #\( ))
        (count 0))
    (flet ((matches-thing (thing)
             (cond ((eql thing start-char)
                    (incf count))
                   ((eql thing end-char)
                    (decf count)))
             (when (zerop count)
               (values t nil))))
      (declare (dynamic-extent #'matches-thing))
      (let ((position
              (forward-or-backward buffer start-position reverse-p #'matches-thing)))
        (and position
             (min position (fill-pointer buffer)))))))

(defun move-over-atom (buffer start-position reverse-p)
  (labels ((atom-character-p (thing)
             (when (and (characterp thing)
                        (or (alphanumericp thing)
                            (member thing '(#\- #\* #\+ #\:))))
               (values t t)))
           (atom-break-character-p (thing)
             (unless (atom-character-p thing)
               (values t t))))
    (declare (dynamic-extent #'atom-break-character-p #'atom-character-p))
    (setq start-position
          (forward-or-backward buffer start-position reverse-p #'atom-character-p))
    (when start-position
      (let ((position
              (forward-or-backward buffer start-position reverse-p
                                   #'atom-break-character-p)))
        (and position
             (min position (fill-pointer buffer)))))))

(defun move-over-whitespace (buffer start-position reverse-p)
  (flet ((non-whitespace (thing)
           (unless (whitespace-char-p thing)
             (values t t))))
    (forward-or-backward buffer start-position reverse-p #'non-whitespace)))


;;; The basic input editing commands...

;; Don't do anything
(define-input-editor-command (com-ie-ctrl-g :rescan nil)
                             (stream)
  "Abort the command"
  (with-slots (numeric-argument command-state command-mode) stream
    (setq numeric-argument nil
          command-state command-mode))
  (beep stream))


(define-input-editor-command (com-ie-universal-argument :rescan nil :type nil)
                             (stream numeric-argument)
  "Universal argument (* 4)"
  (setf (slot-value stream 'numeric-argument)
        (* (or numeric-argument 1) 4)))


(define-input-editor-command (com-ie-forward-character :rescan nil)
                             (stream input-buffer numeric-argument)
  "Move forward character"
  (repeat (abs numeric-argument)
    (let ((p (forward-or-backward input-buffer (stream-insertion-pointer stream)
                                  (minusp numeric-argument) #'true)))
      (if p
          (setf (stream-insertion-pointer stream) p)
          (return (beep stream))))))

(define-input-editor-command (com-ie-forward-word :rescan nil)
                             (stream input-buffer numeric-argument)
  "Move forward word"
  (repeat (abs numeric-argument)
    (let ((p (move-over-word input-buffer (stream-insertion-pointer stream)
                             (minusp numeric-argument))))
      (if p
          (setf (stream-insertion-pointer stream) p)
          (return (beep stream))))))

;;--- Moving up and down lists (c-m-U and c-m-D) would be nice
(define-input-editor-command (com-ie-forward-sexp :rescan nil)
                             (stream input-buffer numeric-argument)
  "Move forward sexp"
  (repeat (abs numeric-argument)
    (let ((p (move-over-sexp input-buffer (stream-insertion-pointer stream)
                             (minusp numeric-argument))))
      (if p
          (setf (stream-insertion-pointer stream) p)
          (return (beep stream))))))


(define-input-editor-command (com-ie-backward-character :rescan nil)
                             (stream input-buffer numeric-argument)
  "Move backward character"
  (repeat (abs numeric-argument)
    (let ((p (forward-or-backward input-buffer (stream-insertion-pointer stream)
                                  (plusp numeric-argument) #'true)))
      (if p
          (setf (stream-insertion-pointer stream) p)
          (return (beep stream))))))

(define-input-editor-command (com-ie-backward-word :rescan nil)
                             (stream input-buffer numeric-argument)
  "Move backward word"
  (repeat (abs numeric-argument)
    (let ((p (move-over-word input-buffer (stream-insertion-pointer stream)
                             (plusp numeric-argument))))
      (if p
          (setf (stream-insertion-pointer stream) p)
          (return (beep stream))))))

(define-input-editor-command (com-ie-backward-sexp :rescan nil)
                             (stream input-buffer numeric-argument)
  "Move backward sexp"
  (repeat (abs numeric-argument)
    (let ((p (move-over-sexp input-buffer (stream-insertion-pointer stream)
                             (plusp numeric-argument))))
      (if p
          (setf (stream-insertion-pointer stream) p)
          (return (beep stream))))))


(define-input-editor-command (com-ie-beginning-of-buffer :rescan nil :type nil)
                             (stream)
  "Beginning of buffer"
  (setf (stream-insertion-pointer stream) 0))

(define-input-editor-command (com-ie-end-of-buffer :rescan nil)
                             (stream input-buffer)
  "End of buffer"
  (setf (stream-insertion-pointer stream) (fill-pointer input-buffer)))


(define-input-editor-command (com-ie-beginning-of-line :rescan nil)
                             (stream input-buffer)
  "Beginning of line"
  (setf (stream-insertion-pointer stream)
        (ie-line-start input-buffer (stream-insertion-pointer stream))))

(define-input-editor-command (com-ie-end-of-line :rescan nil)
                             (stream input-buffer)
  "End of line"
  (setf (stream-insertion-pointer stream)
        (ie-line-end input-buffer (stream-insertion-pointer stream))))


;; Positions to the nearest column in the next line
(define-input-editor-command (com-ie-next-line :rescan nil)
                             (stream input-buffer numeric-argument)
  "Move to next line"
  (unless (= (stream-insertion-pointer stream) (fill-pointer input-buffer))
    (ie-next-previous stream input-buffer numeric-argument)))

(define-input-editor-command (com-ie-previous-line :rescan nil)
                             (stream input-buffer numeric-argument)
  "Move to previous line"
  (unless (zerop (stream-insertion-pointer stream))
    (ie-next-previous stream input-buffer (- numeric-argument))))

;; positive = next, negative = previous
(defun ie-next-previous (stream input-buffer numeric-argument)
  (unless (zerop numeric-argument)
    (let* ((pointer (stream-insertion-pointer stream))
           (this-line (ie-line-start input-buffer pointer))
           (target-line this-line))
      (if (plusp numeric-argument)
          (let (next-line-1)
            (repeat numeric-argument
              (setq next-line-1 (position #\Newline input-buffer :start target-line))
              (unless next-line-1 (return))
              (setq target-line (1+ next-line-1)))
            (setf (stream-insertion-pointer stream)
                  (let ((position-in-line (- pointer this-line)))
                    (min (+ target-line position-in-line)
                         (ie-line-end input-buffer target-line)))))
          (let (prev-line-end)
            (repeat (- numeric-argument)
              (setq prev-line-end (position #\Newline input-buffer
                                            :end target-line :from-end t))
              (unless prev-line-end (return))
              (setq target-line prev-line-end))
            (setf (stream-insertion-pointer stream)
                  (let ((position-in-line (- pointer this-line)))
                    (min (+ (ie-line-start input-buffer target-line)
                            position-in-line)
                         (ie-line-end input-buffer target-line)))))))))


;;; spr25914
;;; A kill can change the input context 
;;; (for example, by rubbing out a command-arg,
;;; or part of a [previously completed] command-name).
;;; So, to be safe always rescan immediately following a kill.
(define-input-editor-command (com-ie-rubout :type delete 
					    :rescan :immediate) 
                             (stream input-buffer numeric-argument)
  "Rubout character"
  (ie-rub-del stream input-buffer (- numeric-argument)))

(define-input-editor-command (com-ie-delete-character :type delete 
						      ;; spr29514 (see above)
						      :rescan :immediate)
                             (stream input-buffer numeric-argument)
  "Delete character"
  (ie-rub-del stream input-buffer numeric-argument))

;; positive = delete, negative = rubout
(defun ie-rub-del (stream input-buffer numeric-argument)
  (let* ((p1 (stream-insertion-pointer stream))
         (p2 p1)
         (reverse-p (minusp numeric-argument)))
    (repeat (abs numeric-argument)
      (let ((p3 (forward-or-backward input-buffer p2 reverse-p #'true)))
        (if p3 (setq p2 p3) (return))))
    (when (noise-string-p (aref input-buffer p2))
      ;; If we are pointing right at a noise string, delete it too
      (if reverse-p
          (setq p2 (max 0 (1- p2)))
          (setq p2 (min (fill-pointer input-buffer) (1+ p2)))))
    (if (/= p1 p2)
        (ie-kill stream input-buffer
                 (cond ((eq (slot-value stream 'last-command-type) 'kill) :merge)
                       ((> (abs numeric-argument) 1) t)
                       (t nil))
                 p2 p1 reverse-p)
        (beep stream))))


(define-input-editor-command (com-ie-rubout-word :type kill  
						 ;; spr29514 (see above)
						 :rescan :immediate)
                             (stream input-buffer numeric-argument)
  "Rubout word"
  (ie-rub-del-word stream input-buffer (- numeric-argument)))

(define-input-editor-command (com-ie-delete-word :type kill  
						 ;; spr29514 (see above)
						 :rescan :immediate)
                             (stream input-buffer numeric-argument)
  "Delete word"
  (ie-rub-del-word stream input-buffer numeric-argument))

;; positive = next, negative = previous
(defun ie-rub-del-word (stream input-buffer numeric-argument)
  (let* ((p1 (stream-insertion-pointer stream))
         (p2 p1)
         (reverse-p (minusp numeric-argument)))
    (repeat (abs numeric-argument)
      (let ((p3 (move-over-word input-buffer p2 reverse-p)))
        (if p3 (setq p2 p3) (return))))
    (if (/= p1 p2)
        (ie-kill stream input-buffer
                 (if (eq (slot-value stream 'last-command-type) 'kill) :merge t)
                 p2 p1 reverse-p)
        (beep stream))))


(define-input-editor-command (com-ie-rubout-sexp :type kill 
						 ;; spr29514 (see above)
						 :rescan :immediate)
                             (stream input-buffer numeric-argument)
  "Rubout sexp"
  (ie-rub-del-sexp stream input-buffer (- numeric-argument)))

(define-input-editor-command (com-ie-delete-sexp :type kill 
						 ;; spr29514 (see above)
						 :rescan :immediate)
                             (stream input-buffer numeric-argument)
  "Delete sexp"
  (ie-rub-del-sexp stream input-buffer numeric-argument))

;; positive = next, negative = previous
(defun ie-rub-del-sexp (stream input-buffer numeric-argument)
  (let* ((p1 (stream-insertion-pointer stream))
         (p2 p1)
         (reverse-p (minusp numeric-argument)))
    (repeat (abs numeric-argument)
      (let ((p3 (move-over-sexp input-buffer p2 reverse-p)))
        (if p3 (setq p2 p3) (return))))
    (if (/= p1 p2)
        (ie-kill stream input-buffer
                 (if (eq (slot-value stream 'last-command-type) 'kill) :merge t)
                 p2 p1 reverse-p)
        (beep stream))))


(define-input-editor-command (com-ie-clear-input :type kill)
                             (stream input-buffer)
  "Clear input buffer"
  ;; Just push a copy of the input buffer onto the kill ring, no merging
  (ie-kill stream input-buffer t 0 (fill-pointer input-buffer)))

;;--- Kill backwards up list (c-sh-K) would be nice
(define-input-editor-command (com-ie-kill-line :type kill)
                             (stream input-buffer numeric-argument)
  "Kill to end of line"
  (let* ((reverse-p (minusp numeric-argument))
         (point (stream-insertion-pointer stream))
         (other-point (if reverse-p
                          (ie-line-start input-buffer point)
                          (ie-line-end input-buffer point))))
    (ie-kill stream input-buffer
             (if (eq (slot-value stream 'last-command-type) 'kill) :merge t)
             point
             other-point
             reverse-p)))


(define-input-editor-command (com-ie-delete-whitespace :type kill)
                             (stream input-buffer)
  "Delete surrounding whitespace"
  (unless (delete-surrounding-whitespace stream input-buffer)
    (beep stream)))

(define-input-editor-command (com-ie-just-one-space :type kill)
                             (stream input-buffer)
  "Leave a single space"
  (unless (delete-surrounding-whitespace stream input-buffer t)
    (beep stream)))

(defun delete-surrounding-whitespace (stream input-buffer &optional leave-one)
  (let* ((start (let ((ip (stream-insertion-pointer stream)))
                  (if (whitespace-char-p (aref input-buffer ip))
                      ip
                      (max (1- ip) 0))))
         (next (and (whitespace-char-p (aref input-buffer start))
                    (move-over-whitespace input-buffer start nil)))
         (prev (and next (move-over-whitespace input-buffer next t))))
    (when (and prev next)
      (when leave-one
        (when (eql (aref input-buffer prev) #\Tab)
          (setf (aref input-buffer prev) #\Space))
        (incf prev))
      (ie-kill stream input-buffer
               (if (eq (slot-value stream 'last-command-type) 'kill) :merge t)
               prev next nil)
      t)))


(define-input-editor-command (com-ie-make-room)
                             (stream input-buffer)
  "Insert a new line"
  (let ((point (stream-insertion-pointer stream))
        (end (fill-pointer input-buffer)))
    (cond ((= point end)
           (incf (fill-pointer input-buffer)))
          (t
           (erase-input-buffer stream point)
           (shift-buffer-portion input-buffer point (1+ point))))
    (setf (aref input-buffer point) #\Newline)
    (redraw-input-buffer stream point)))

(define-input-editor-command (com-ie-make-|()|)
                             (stream input-buffer)
  "Insert a \(\) pair"
  (let ((point (stream-insertion-pointer stream))
        (end (fill-pointer input-buffer)))
    (cond ((= point end)
           (incf (fill-pointer input-buffer) 2))
          (t
           (erase-input-buffer stream point)
           (shift-buffer-portion input-buffer point (+ point 2))))
    (setf (aref input-buffer (+ point 0)) #\( )
    (setf (aref input-buffer (+ point 1)) #\) )
    (incf (stream-insertion-pointer stream))
    (redraw-input-buffer stream point)))


(define-input-editor-command (com-ie-transpose-characters)
                             (stream input-buffer)
  "Transpose characters"
  (let* ((start (min (1+ (stream-insertion-pointer stream))
                     (fill-pointer input-buffer)))
         (this (forward-or-backward input-buffer start t #'true))
         (prev (and this (forward-or-backward input-buffer this t #'true))))
    (cond ((and this prev (/= this prev))
           (let ((this-char (aref input-buffer this))
                 (prev-char (aref input-buffer prev)))
             (erase-input-buffer stream prev)
             (setf (aref input-buffer prev) this-char)
             (setf (aref input-buffer this) prev-char)
             (setf (stream-insertion-pointer stream) start)
             (redraw-input-buffer stream prev)))
          (t (beep stream)))))

(define-input-editor-command (com-ie-transpose-words)
                             (stream input-buffer)
  "Transpose words"
  (let* ((start (stream-insertion-pointer stream))
         (next (move-over-word input-buffer start nil))
         (this (and next (move-over-word input-buffer next t)))
         (prev (and this (move-over-word input-buffer this t))))
    (cond ((and next prev (/= next prev))
           (with-temporary-substring (s1 input-buffer prev (- this 1))
             (with-temporary-substring (s2 input-buffer (- this 1) this)
               (with-temporary-substring (s3 input-buffer this next)
                 (replace input-buffer s3 :start1 prev)
                 (replace input-buffer s2 :start1 (+ prev (length s3)))
                 (replace input-buffer s1 :start1 (+ prev (length s3) 1)))))
           (erase-input-buffer stream prev)
           (setf (stream-insertion-pointer stream) next)
           (redraw-input-buffer stream prev))
          (t (beep stream)))))

(define-input-editor-command (com-ie-transpose-sexps)
                             (stream input-buffer)
  "Transpose sexps"
  (let* ((start (stream-insertion-pointer stream))
         (next (and start (move-over-sexp input-buffer start nil)))
         (this (and next (move-over-sexp input-buffer next t)))
         (prev (and this (move-over-sexp input-buffer this t))))
    (cond ((and next prev (/= next prev))
           (with-temporary-substring (s1 input-buffer prev (- this 1))
             (with-temporary-substring (s2 input-buffer (- this 1) this)
               (with-temporary-substring (s3 input-buffer this next)
                 (replace input-buffer s3 :start1 prev)
                 (replace input-buffer s2 :start1 (+ prev (length s3)))
                 (replace input-buffer s1 :start1 (+ prev (length s3) 1)))))
           (erase-input-buffer stream prev)
           (setf (stream-insertion-pointer stream) next)
           (redraw-input-buffer stream prev))
          (t (beep stream)))))

(defun change-word-case (stream input-buffer numeric-argument function)
  (declare (dynamic-extent function))
  (let* ((start (stream-insertion-pointer stream))
         (end (let ((p start))
                (repeat (abs numeric-argument)
                  (when p
                    (setq p (move-over-word input-buffer p (minusp numeric-argument)))))
                p)))
    (cond (end
           (when (< end start) (rotatef start end))
           (do ((i start (1+ i)))
               ((>= i end)
                (progn
                  (erase-input-buffer stream start)
                  (setf (stream-insertion-pointer stream) end)
                  (redraw-input-buffer stream start)))
             (let ((char (aref input-buffer i)))
               (when (characterp char)
                 (setf (aref input-buffer i) (funcall function char))))))
          (t (beep stream)))))

(define-input-editor-command (com-ie-upcase-word)
                             (stream input-buffer numeric-argument)
  "Upcase word"
  (change-word-case stream input-buffer numeric-argument #'char-upcase))

(define-input-editor-command (com-ie-downcase-word)
                             (stream input-buffer numeric-argument)
  "Downcase word"
  (change-word-case stream input-buffer numeric-argument #'char-downcase))

(define-input-editor-command (com-ie-capitalize-word)
                             (stream input-buffer numeric-argument)
  "Capitalize word"
  (repeat (abs numeric-argument)
    (let* ((start (stream-insertion-pointer stream))
           (end (move-over-word input-buffer start nil))
           (state nil))
      (if end
          (do ((i start (1+ i)))
              ((>= i end)
               (progn
                 (erase-input-buffer stream start)
                 (setf (stream-insertion-pointer stream) end)
                 (redraw-input-buffer stream start)))
            (let ((char (aref input-buffer i)))
              (when (characterp char)
                (if (null state)
                    (when (alphanumericp char)
                      (setf (aref input-buffer i) (char-upcase char))
                      (setq state t))
                    (if (alphanumericp char)
                        (setf (aref input-buffer i) (char-downcase char))
                        (setq state nil))))))
          (beep stream)))))


;;; Mark setting, etc.

(define-input-editor-command (com-ie-set-mark :rescan nil)
                             (stream numeric-argument)
  "Set the mark"
  (setf (slot-value stream 'mark)
        (if (eq numeric-argument 4)                ;control-U
            nil
            (stream-insertion-pointer stream))))

(define-input-editor-command (com-ie-mark-beginning :rescan nil)
                             (stream)
  "Mark beginning of buffer"
  (setf (slot-value stream 'mark) 0))

(define-input-editor-command (com-ie-mark-end :rescan nil)
                             (stream input-buffer)
  "Mark end of buffer"
  (setf (slot-value stream 'mark) (fill-pointer input-buffer)))

(define-input-editor-command (com-ie-swap-point-and-mark :rescan nil)
                             (stream)
  "Swap point and mark"
  (if (slot-value stream 'mark)
      (rotatef (slot-value stream 'mark) (stream-insertion-pointer stream))
      (beep stream)))

(define-input-editor-command (com-ie-kill-region :type kill)
                             (stream input-buffer)
  "Kill marked region"
  (if (slot-value stream 'mark)
      (ie-kill stream input-buffer
               (if (eq (slot-value stream 'last-command-type) 'kill) :merge t)
               (stream-insertion-pointer stream) (slot-value stream 'mark))
      (beep stream)))

(define-input-editor-command (com-ie-save-region :rescan nil)
                             (stream input-buffer)
  "Save marked region"
  (if (slot-value stream 'mark)
      (ie-kill stream input-buffer
               (if (eq (slot-value stream 'last-command-type) 'kill) :merge t)
               (stream-insertion-pointer stream) (slot-value stream 'mark)
               nil t)                                ;don't kill anything
      (beep stream)))


;;; Lispy input editing commands

(defun function-arglist (function)
  (declare (values arglist found-p))
  #+Genera (values (sys:arglist function) T)
  #+Cloe-Runtime (sys::arglist function)
  #+allegro (values (excl::arglist function) t)
  #+Lucid (values (lucid-common-lisp:arglist function) t))

(defun word-start-and-end (string start-chars &optional (start 0))
  (declare (values word-start word-end colon))
  (flet ((open-paren-p (thing)
           (or (not (characterp thing))                ;noise strings and blips are delimiters
               (member thing start-chars)))
         (atom-break-char-p (thing)
           (or (not (characterp thing))                ;ditto
               (not (graphic-char-p thing))
               (multiple-value-bind (mac nt)
                   (get-macro-character thing)
                 (and mac (not nt)))
               (member thing '(#\space #\( #\) #\")))))
    (declare (dynamic-extent #'open-paren-p #'atom-break-char-p))
    (let* ((word-start
             (forward-or-backward string start t #'open-paren-p))
           (word-end
             (and word-start
                  (or (forward-or-backward string (1+ word-start) nil
                                           #'atom-break-char-p)
                      (let ((fp (fill-pointer string)))
                        (and (> fp (1+ word-start)) fp)))))
           (colon
             (and word-start word-end
                  (position #\: string
                            :start (1+ word-start) :end (1- word-end)))))
      (values (and word-start
                   (if (atom-break-char-p (aref string word-start))
                       (1+ word-start)
                       word-start))
              (and word-end (1- word-end))
              colon))))

(defun symbol-at-position (stream input-buffer delimiters)
  (declare (values symbol package start end))
  (multiple-value-bind (word-start word-end)
      (word-start-and-end input-buffer delimiters
                          (stream-insertion-pointer stream))

    (when word-end
      (let ((symbol
             (read-from-string (coerce input-buffer 'string) nil nil
                               :start word-start
                               :end word-end)))
        (values
         symbol
         (symbol-package symbol)
         word-start
         word-end)))))

(define-input-editor-command (com-ie-show-arglist :rescan nil)
                             (stream input-buffer)
  "Show function arglist"
  (let* ((symbol (symbol-at-position stream input-buffer '(#\( )))
         (function (and symbol (fboundp symbol) (symbol-function symbol))))
    (if function
        (multiple-value-bind (arglist found-p)
            (function-arglist function)
          (when found-p
            (with-input-editor-typeout (stream)
              #-Cloe-Runtime
              (format stream "~S: (~{~A~^ ~})"
                symbol arglist)
              #+Cloe-Runtime
              (format stream "~S (~A): (~{~:A~^ ~})"
                symbol found-p arglist))))
        (beep stream))))

(define-input-editor-command (com-ie-show-value :rescan nil)
                             (stream input-buffer)
  "Show symbol value"
  (let* ((symbol (symbol-at-position stream input-buffer '(#\space #\( #\) #\")))
         (value (and symbol (boundp symbol) (symbol-value symbol))))
    (if value
        (with-input-editor-typeout (stream)
          (format stream "~S: ~S" symbol value))
        (beep stream))))

(define-input-editor-command (com-ie-show-documentation :rescan nil)
                             (stream input-buffer)
  "Show symbol documentation"
  (multiple-value-bind (symbol package start end)
      (symbol-at-position stream input-buffer '(#\space #\( #\) #\"))
    (declare (ignorable package end))
    (when (null symbol)
      ;; If we can't find a variable, try for a function
      (multiple-value-setq (symbol package start end)
        (symbol-at-position stream input-buffer '(#\( ))))
    (if symbol
        (let* ((type (if (char-equal (aref input-buffer (max (1- start) 0)) #\( )
                         'function
                         'variable))
               (documentation (documentation symbol type)))
          (when documentation
            (with-input-editor-typeout (stream)
              (format stream "~S: ~A" symbol documentation))))
        (beep stream))))


;;; Yanking commands

(defun ie-yank-from-history (history function istream numeric-argument
                             &key test replace-previous)
  (setf (slot-value istream 'previous-history) history)
  (cond ((zerop numeric-argument)
         (display-history-contents history istream))
        ((= numeric-argument 1)
         (let ((element (funcall function history :test test)))
           (cond (element
                  (history-replace-input history istream element
                                         :replace-previous replace-previous)
                  ;; The yanking commands don't do an immediate rescan
                  ;; because that can cause premature activation.  What
                  ;; we do is queue a rescan for later, and when the user
                  ;; hits <End> the rescan is done if necessary.
                  (queue-rescan istream ':activation))
                 (t (beep istream)))))
        (t
         (let ((element (funcall function history :index numeric-argument :test test)))
           (cond (element
                  (history-replace-input history istream element
                                         :replace-previous replace-previous)
                  (queue-rescan istream ':activation))
                 (t (beep istream)))))))

;;--- "Yank matching" (c-sh-Y) would be nice
(define-input-editor-command (com-ie-kill-ring-yank :history t :type yank :rescan nil)
                             (stream numeric-argument)
  "Yank from kill ring"
  (ie-yank-from-history *kill-ring* #'yank-from-history stream numeric-argument))

;;--- "History yank matching" (c-m-sh-Y) would be nice
(define-input-editor-command (com-ie-history-yank :history t :type yank :rescan nil)
                             (stream numeric-argument)
  "Yank from history"
  (let ((history
          (and *presentation-type-for-yanking*
               (presentation-type-history *presentation-type-for-yanking*))))
    (if history
        (ie-yank-from-history history #'yank-from-history stream numeric-argument)
        (beep stream))))

;;--- "Yank next matching" (m-sh-Y) would be nice
(define-input-editor-command (com-ie-yank-next :history t :type yank :rescan nil)
                             (stream numeric-argument)
  "Yank next item"
  (let ((history (slot-value stream 'previous-history)))
    (if history
        (ie-yank-from-history history #'yank-next-from-history stream numeric-argument
                              :replace-previous t)
        (beep stream))))


;;; Key bindings

(defmacro define-input-editor-gestures (&body gestures)
  `(progn
     ,@(mapcar #'(lambda (gesture)
                   (let ((name (first gesture))
                         (gesture-spec (rest gesture)))
                     `(add-gesture-name ,name :keyboard ',gesture-spec :unique nil)))
               gestures)))

(define-input-editor-gestures
  (:ie-abort                :g   :control)
  (:ie-universal-argument   :u   :control)
  (:ie-forward-character    :f   :control)
  (:ie-forward-word            :f   :meta)
  (:ie-forward-sexp            :f   :control :meta)
  (:ie-backward-character   :b   :control)
  (:ie-backward-word            :b   :meta)
  (:ie-backward-sexp            :b   :control :meta)
  (:ie-beginning-of-buffer  :\<  :meta)
  (:ie-beginning-of-buffer  :home :control)
  (:ie-end-of-buffer            :\>  :meta)
  (:ie-end-of-buffer            :end :control)
  (:ie-beginning-of-buffer  :\<  :meta :shift)
  (:ie-end-of-buffer            :\>  :meta :shift)
  (:ie-beginning-of-line    :a   :control)
  (:ie-beginning-of-line    :home)
  (:ie-end-of-line            :e   :control)
  (:ie-end-of-line            :end)
  (:ie-next-line            :n   :control)
  (:ie-previous-line            :p   :control)
  (:ie-delete-character            :d   :control)
  (:ie-delete-word            :d   :meta)
  (:ie-delete-sexp            :k   :control :meta)
  (:ie-rubout-character     :rubout)
  (:ie-rubout-word            :rubout :meta)
  (:ie-rubout-sexp            :rubout :control :meta)
  (:ie-kill-line            :k   :control)
  (:ie-clear-input            :clear-input)
  (:ie-delete-whitespace    :\\  :meta)
  (:ie-just-one-space            :\|  :meta)
  (:ie-make-room            :o   :control)
  (:ie-make-|()|            :\(  :meta)
  (:ie-make-|()|            :\(  :meta :shift)
  (:ie-transpose-characters :t   :control)
  (:ie-transpose-words            :t   :meta)
  (:ie-transpose-sexps            :t   :control :meta)
  (:ie-upcase-word            :u   :meta)
  (:ie-downcase-word            :l   :meta)
  (:ie-capitalize-word      :c   :meta)
  (:ie-kill-ring-yank            :y   :control)
  (:ie-history-yank            :y   :control :meta)
  (:ie-yank-next            :y   :meta)
  (:ie-set-mark                    :space :control)
  (:ie-mark-beginning            :< :control)
  (:ie-mark-end                    :> :control)
  (:ie-mark-beginning            :< :control :shift)
  (:ie-mark-end                    :> :control :shift)
  (:ie-swap-point-and-mark  :space :control :meta)
  (:ie-kill-region            :w :control)
  (:ie-save-region            :w :meta)
  (:ie-refresh                    :l   :control)
  (:ie-refresh                    :refresh)
  (:ie-scroll-forward            :v   :control)
  (:ie-scroll-backward            :v   :meta)
  (:ie-scroll-left            :v   :super)
  (:ie-scroll-right            :v   :super :meta)
  (:ie-scroll-forward            :scroll)
  (:ie-scroll-backward            :scroll :meta)
  (:ie-scroll-backward            :scroll-up)
  (:ie-scroll-left            :scroll :super)
  (:ie-scroll-right            :scroll :super :meta)
  (:ie-scroll-right            :scroll-up :super)
  (:ie-input-editor-help    :help :control))

#-(or (and (not acl86win32) allegro) Lucid)
(define-input-editor-gestures
  (:ie-show-arglist            :a   :control :shift)
  (:ie-show-value            :v   :control :shift)
  (:ie-show-documentation   :d   :control :shift))

#+(or (and (not acl86win32) allegro) Lucid)
(define-input-editor-gestures
  (:ie-show-arglist            :a   :meta :shift)
  (:ie-show-value            :v   :meta :shift)
  (:ie-show-documentation   :d   :meta :shift))

#+(or allegro aclpc)
(define-input-editor-gestures
  (:ie-rubout-character     :backspace)
  (:ie-rubout-word	    :backspace :meta)
  (:ie-rubout-sexp	    :backspace :control :meta))

#+Cloe-Runtime
(define-input-editor-gestures
  (:ie-rubout-character     :backspace)
  (:ie-rubout-word	    :backspace :meta)
  (:ie-rubout-sexp	    :backspace :control :meta)
  (:ie-clear-input	    :clear)
  (:ie-scroll-forward	    :page-down)
  (:ie-scroll-backward	    :page-up)
  (:ie-scroll-left	    :page-up :super)
  (:ie-scroll-right	    :page-down :super))

#+(or aclpc acl86win32)
(define-input-editor-gestures
  (:ie-delete-character            :delete)
  (:ie-delete-word            :delete :meta)
  (:ie-clear-input            :clear)
  (:ie-scroll-left            :page-up :control)
  (:ie-scroll-right            :page-down :control))

(defmacro assign-input-editor-key-bindings (&body functions-and-gestures)
  (let ((forms nil))
    (loop
      (when (null functions-and-gestures) (return))
      (let* ((function (pop functions-and-gestures))
             (gesture (pop functions-and-gestures)))
        (when gesture
          (push `(add-input-editor-command ',gesture ',function)
                forms))))
    `(progn ,@(nreverse forms))))

(assign-input-editor-key-bindings
  com-ie-ctrl-g                       :ie-abort
  com-ie-universal-argument    :ie-universal-argument
  com-ie-forward-character     :ie-forward-character
  com-ie-forward-word               :ie-forward-word
  com-ie-forward-sexp               :ie-forward-sexp
  com-ie-backward-character    :ie-backward-character
  com-ie-backward-word               :ie-backward-word
  com-ie-backward-sexp               :ie-backward-sexp
  com-ie-beginning-of-buffer   :ie-beginning-of-buffer
  com-ie-end-of-buffer               :ie-end-of-buffer
  com-ie-beginning-of-line     :ie-beginning-of-line
  com-ie-end-of-line               :ie-end-of-line
  com-ie-next-line               :ie-next-line
  com-ie-previous-line               :ie-previous-line
  com-ie-delete-character      :ie-delete-character
  com-ie-delete-word               :ie-delete-word
  com-ie-delete-sexp               :ie-delete-sexp
  com-ie-rubout                       :ie-rubout-character
  com-ie-rubout-word               :ie-rubout-word
  com-ie-rubout-sexp               :ie-rubout-sexp
  com-ie-kill-line               :ie-kill-line
  com-ie-clear-input               :ie-clear-input
  com-ie-delete-whitespace     :ie-delete-whitespace
  com-ie-just-one-space               :ie-just-one-space
  com-ie-make-room               :ie-make-room
  com-ie-make-|()|               :ie-make-|()|
  com-ie-transpose-characters  :ie-transpose-characters
  com-ie-transpose-words       :ie-transpose-words
  com-ie-transpose-sexps       :ie-transpose-sexps
  com-ie-upcase-word                :ie-upcase-word
  com-ie-downcase-word               :ie-downcase-word
  com-ie-capitalize-word       :ie-capitalize-word
  com-ie-show-arglist               :ie-show-arglist
  com-ie-show-value               :ie-show-value
  com-ie-show-documentation    :ie-show-documentation
  com-ie-kill-ring-yank               :ie-kill-ring-yank
  com-ie-history-yank               :ie-history-yank
  com-ie-yank-next               :ie-yank-next
  com-ie-set-mark               :ie-set-mark
  com-ie-mark-beginning               :ie-mark-beginning
  com-ie-mark-end               :ie-mark-end
  com-ie-swap-point-and-mark   :ie-swap-point-and-mark
  com-ie-kill-region               :ie-kill-region
  com-ie-save-region               :ie-save-region
  com-ie-refresh               :ie-refresh
  com-ie-scroll-forward               :ie-scroll-forward
  com-ie-scroll-backward       :ie-scroll-backward
  com-ie-scroll-left               :ie-scroll-left
  com-ie-scroll-right          :ie-scroll-right
  com-ie-input-editor-help     :ie-input-editor-help)

(define-input-editor-gestures
  (:ie-prefix-1 :escape)
  (:ie-prefix-2 :x :control))

(assign-input-editor-key-bindings
  (com-ie-swap-point-and-mark  (:ie-prefix-2 (:x :control))))

#+(or (and allegro (not acl86win32)) Lucid)
(assign-input-editor-key-bindings
  com-ie-forward-word               (:ie-prefix-1 :f)
  com-ie-backward-word               (:ie-prefix-1 :b)
  com-ie-beginning-of-buffer   (:ie-prefix-1 :\<)
  com-ie-end-of-buffer               (:ie-prefix-1 :\>)
  com-ie-delete-word               (:ie-prefix-1 :d)
  com-ie-rubout-word               (:ie-prefix-1 :rubout)
  com-ie-history-yank               (:ie-prefix-1 (:y :control))
  com-ie-yank-next               (:ie-prefix-1 :y)
  com-ie-scroll-backward       (:ie-prefix-1 :v)
  com-ie-show-arglist               (:ie-prefix-2 (:a :control))
  com-ie-show-value               (:ie-prefix-2 (:v :control)))


;;; Some Genera-specific stuff

#+Genera (progn

(defmacro with-debug-io-selected ((stream) &body body)
  (let ((window '#:window))
    `(let ((,window (sheet-mirror (frame-top-level-sheet (pane-frame ,stream)))))
       (when ,window
         (scl:send *debug-io* :select))
       (unwind-protect
           (progn ,@body)
         (when ,window
           (scl:send ,window :select))))))

(define-input-editor-command (com-ie-break :rescan nil)
                             (stream)
  "Enter a BREAK loop"
  (with-debug-io-selected (stream)
    (zl:break (format nil "Break for ~A" (frame-pretty-name (pane-frame stream))))))

(define-input-editor-command (com-ie-debugger-break :rescan nil)
                             (stream)
  "Enter the debugger"
  (with-debug-io-selected (stream)
    (cl:break "Debugger break for ~A" (frame-pretty-name (pane-frame stream)))))

(define-input-editor-command (com-ie-show-context :rescan nil)
                             (stream)
  "Show the current input context"
  (with-input-editor-typeout (stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-cell (stream)
          (with-text-face (stream :italic)
            (write-string "Delimiters" stream)))
        (formatting-cell (stream)
          (print *delimiter-gestures* stream)))
      (formatting-row (stream)
        (formatting-cell (stream)
          (with-text-face (stream :italic)
            (write-string "Activators" stream)))
        (formatting-cell (stream)
          (print *activation-gestures* stream)))
      (formatting-row (stream)
        (formatting-cell (stream)
          (with-text-face (stream :italic)
            (write-string "Context" stream)))
        (formatting-cell (stream)
          (dolist (context *input-context*)
            (print (input-context-type context) stream)))))))

(define-input-editor-gestures
  (:ie-break              :suspend)
  (:ie-debugger-break :suspend  :meta)
  (:ie-show-context   :help        :meta))

(assign-input-editor-key-bindings
  com-ie-break                :ie-break
  com-ie-debugger-break :ie-debugger-break
  com-ie-show-context   :ie-show-context)

)        ;#+Genera

#+allegro
(progn
  (define-input-editor-gestures
      (:left-arrow :left-arrow)
      (:right-arrow :right-arrow)
      (:up-arrow :up-arrow)
      (:down-arrow :down-arrow))

  (assign-input-editor-key-bindings
      com-ie-backward-character  :left-arrow
    com-ie-forward-character :right-arrow
    com-ie-scroll-backward    :up-arrow
    com-ie-scroll-forward  :down-arrow))
