;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: clim-defs.lisp,v 1.29.42.1 2000/08/23 21:58:11 cley Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;; POSITIONS is a list of pairs of numbers.
;; No assumptions are made about the types of the numbers.
(defmacro translate-positions (x-delta y-delta &body positions)
  (once-only (x-delta y-delta)
    `(progn
       ,@(let ((forms nil))
           (dorest (pts positions cddr)
             (push `(incf ,(first pts)  ,x-delta) forms)
             (push `(incf ,(second pts) ,y-delta) forms))
           (nreverse forms)))))

(defun translate-position-sequence (x-delta y-delta positions)
  (let ((new-positions (make-list (length positions))))
    (if (listp positions)
        (do ((positions positions (cddr positions))
             (new-positions new-positions (cddr new-positions)))
            ((null new-positions))
          (let ((x (first positions))
                (y (second positions)))
            (translate-positions x-delta y-delta x y)
            (setf (first new-positions) x)
            (setf (second new-positions) y)))
        (do ((i 0 (+ i 2))
             (new-positions new-positions (cddr new-positions)))
            ((null new-positions))
          (let ((x (aref positions i))
                (y (aref positions (1+ i))))
            (translate-positions x-delta y-delta x y)
            (setf (first new-positions) x)
            (setf (second new-positions) y))))
    new-positions))

;; Destructive version of the above
(defun ntranslate-position-sequence (x-delta y-delta positions)
  (if (listp positions)
      (do ((positions positions (cddr positions)))
          ((null positions))
        (translate-positions x-delta y-delta (first positions) (second positions)))
      (let ((length (length positions)))
        (do ((i 0 (+ i 2)))
            ((>= i length))
          (translate-positions x-delta y-delta (aref positions i) (aref positions (1+ i))))))
  positions)


(defclass io-buffer ()
    ((size :accessor io-buffer-size :initarg :size)
     (buffer :accessor io-buffer-buffer)
     (input-pointer :accessor io-buffer-input-pointer)
     (output-pointer :accessor io-buffer-output-pointer)))


(defmacro with-stream-cursor-position-saved ((stream) &body body)
  (let ((x '#:x)
        (y '#:y))
    `(multiple-value-bind (,x ,y) (stream-cursor-position ,stream)
       (unwind-protect
           (progn ,@body)
         (stream-set-cursor-position ,stream ,x ,y)))))

(defmacro with-viewport-position-saved ((stream) &body body)
  (let ((x '#:x)
        (y '#:y))
    `(multiple-value-bind (,x ,y) (window-viewport-position ,stream)
       (unwind-protect
           (progn ,@body)
         (window-set-viewport-position ,stream ,x ,y)))))

(defmacro with-output-recording-options 
          ((stream &key (draw nil draw-supplied)
                        (record nil record-supplied))
           &body body)
  (let ((new-stream (gensymbol 'stream)))
    `(let ((,new-stream ,stream))
       (flet ((with-output-recording-options-body () ,@body))
         (declare (dynamic-extent #'with-output-recording-options-body))
         (invoke-with-output-recording-options
           ,new-stream #'with-output-recording-options-body
           ,(if record-supplied record `(stream-recording-p ,new-stream))
           ,(if draw-supplied draw `(stream-drawing-p ,new-stream)))))))

(eval-when (compile eval load)
(defvar *output-record-constructor-cache* (make-hash-table))

(defmacro construct-output-record (type &rest initargs &environment env)
  #-(or Genera Minima) (declare (ignore env))
  (let ((constructor nil))
    (cond ((and (constantp type #+(or Genera Minima) env)
                (setq constructor (gethash (eval type #+(or Genera Minima-Developer) env)
                                           *output-record-constructor-cache*)))
           `(,constructor ,@initargs))
          (t `(construct-output-record-1 ,type ,@initargs)))))

(defmacro define-output-record-constructor (record-type arglist &body initargs)
  (let ((constructor-name (fintern "~A-~A" record-type 'constructor)))
    `(progn
       (define-constructor ,constructor-name ,record-type ,arglist ,@initargs)
       (setf (gethash ',record-type *output-record-constructor-cache*)
             ',constructor-name))))
)        ;eval-when

(defmacro with-new-output-record ((stream &optional record-type record &rest initargs)
                                  &body body &environment env)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  #-(or Genera Minima) (declare (ignore env))
  (unless record-type
    (setq record-type `'standard-sequence-output-record))
  (let ((constructor nil)
        (ignore-record nil))
    (when (and (constantp record-type #+(or Genera Minima) env)
               (setq constructor (gethash (eval record-type #+(or Genera Minima-Developer) env)
                                          *output-record-constructor-cache*))))
    (unless record
      (setq record '#:record
            ignore-record t))
    `(flet ((with-new-output-record-body (,record)
              ,@(when ignore-record `((declare (ignore ,record))))
              ,@body))
       (declare (dynamic-extent #'with-new-output-record-body))
       (invoke-with-new-output-record
         ,stream #'with-new-output-record-body
         ,record-type ',constructor ,@initargs))))

(defmacro with-output-to-output-record ((stream &optional record-type record &rest initargs)
                                        &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream)
  ;; INVOKE-WITH-OUTPUT-RECORDING-OPTIONS only implemented for output
  ;; recording streams, which will serve to validate whether we are
  ;; doing this on the right kind of stream
  `(with-output-recording-options (,stream :draw nil :record t)
     (letf-globally (((stream-current-output-record ,stream) nil)
                     ((stream-output-history ,stream) nil)
                     ((stream-text-output-record ,stream) nil))
       (with-new-output-record (,stream ,record-type ,record ,@initargs)
         (with-stream-cursor-position-saved (,stream)
           ,@body)))))


;;; Presentation type stuff

(defvar *allow-sensitive-inferiors* t)
(defmacro with-output-as-presentation ((stream object type 
                                        &rest options
                                        &key modifier single-box
                                             (allow-sensitive-inferiors t asi-p)
                                             parent
                                             (record-type `'standard-presentation)
                                        &allow-other-keys)
                                       &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream)
  (setq options (remove-keywords options '(:modifier :single-box :parent
                                           :allow-sensitive-inferiors :record-type)))
  ;; Maybe with-new-output-record should turn record-p on?
  (unless asi-p
    (setq allow-sensitive-inferiors '*allow-sensitive-inferiors*))
  (let ((nobject '#:object)                        ;(once-only (object type) ...)
        (ntype '#:type))
    `(with-output-recording-options (,stream :record t)
       (let ((,nobject ,object)
             (,ntype ,type))
         (with-new-output-record (,stream (if *allow-sensitive-inferiors*
                                              ,record-type
                                              'standard-nonsensitive-presentation) nil
                                  :object ,nobject
                                  :type (if ,ntype
                                            (expand-presentation-type-abbreviation ,ntype)
                                            (presentation-type-of ,nobject))
                                  :single-box ,single-box
                                  ,@(when modifier `(:modifier ,modifier))
                                  ,@(when parent `(:parent ,parent))
                                  ,@options)
           (let ((*allow-sensitive-inferiors* ,allow-sensitive-inferiors))
             ,@body))))))


;; The current input context consists of a list of context entries.  Each entry
;; is a list of the form (CONTEXT-TYPE CATCH-TAG)
(defvar *input-context* nil)

(defun-inline input-context-type (context-entry)
  (first context-entry))

(defun-inline input-context-tag (context-entry)
  (second context-entry))

;; This is the presentation you get if you click while not over anything...
(defvar *null-presentation*)

(defun make-input-context-clauses (pt-var clauses)
  (let ((new-clauses nil))
    (dolist (clause clauses (nreverse new-clauses))
      (let ((type (first clause))
            (body (rest clause)))
        (push `((presentation-subtypep-1 ,pt-var ',type)
                ,@body)
              new-clauses)))))        ;eval-when

#+allegro
(excl:defun-proto invoke-with-input-context (type override body-continuation 
				  context-continuation)
  (declare (dynamic-extent body-continuation context-continuation)))


(defmacro with-input-context ((type &key override) 
                              (&optional object-var type-var event-var options-var)
                              form
                              &body clauses)
  #+Genera (declare (zwei:indentation 0 2 2 4 3 2))
  (let ((ignores nil))
    (when (null object-var)
      (setq object-var '#:object)
      (push object-var ignores))
    (when (null type-var)
      (setq type-var '#:presentation-type)
      (unless clauses (push type-var ignores)))
    (when (null event-var)
      (setq event-var '#:event)
      (push event-var ignores))
    (when (null options-var)
      (setq options-var '#:options)
      (push options-var ignores))
    `(flet ((body-continuation () ,form)
            (context-continuation (,object-var ,type-var ,event-var ,options-var)
              ,@(and ignores `((declare (ignore ,@ignores))))
              (cond ,@(make-input-context-clauses type-var clauses))))
       (declare (dynamic-extent #'body-continuation #'context-continuation))
       (invoke-with-input-context
         (expand-presentation-type-abbreviation ,type) ,override
         #'body-continuation #'context-continuation))))

;;; A problem can arise if the a command is launched asynchronous
;;; (e.g. the command is launched by the program, not as the 
;;; result of user-input).
;;; Specifically, the input-focus can be snatched away from the
;;; current user frame.
;;;
;;; The current rules are:
;;; 1] On entry, note the original input-focus.
;;;    Only set the input-focus to a new value if the frame
;;;    of the new-input-focus is the same as the frame of 
;;;    the original- input-focus.
;;;    (I.e. don't snatch the input focus away if we are, for
;;;    example, running an asynchronously launched command.)
;;; 
;;; 2] On exit, note the then current input-focus.
;;;    Only re-set the input-focus to the original-input-focus
;;;    if the frame of the now-current-input-focus is the
;;;    same as the "new" value set above.
;;;    (I.e. worry whether the input focus has been swapped 
;;;    out from under us while we were waiting.)
;;; 
(defmacro with-input-focus ((stream &optional (doit t)) &body body)
  (let ((old-input-focus '#:old-input-focus)
	(orig-input-focus '#:orig-input-focus)
	(orig-input-focus-frame '#:orig-input-focus-frame)
	(stream-frame '#:stream-frame)
	)
    `(let* ((,old-input-focus nil)
	    (,stream-frame (and ,stream
				(pane-frame ,stream)))
	    (,orig-input-focus (and ,stream
				    (port-keyboard-input-focus (port ,stream))))
	    (,orig-input-focus-frame (and ,orig-input-focus
					  (pane-frame ,orig-input-focus))))
       (unwind-protect
           (progn
             (when (and ,doit
			(eq ,stream-frame 
			    ,orig-input-focus-frame))
               (setq ,old-input-focus (stream-set-input-focus ,stream)))
             ,@body)
         (when (and ,old-input-focus
		    (port-keyboard-input-focus (port ,stream))
		    (eql (pane-frame (port-keyboard-input-focus (port ,stream)))
			 ,stream-frame))
           (stream-restore-input-focus ,stream ,old-input-focus))))))


(defmacro completing-from-suggestions 
          ((stream &rest options
            &key partial-completers allow-any-input
                 possibility-printer (help-displays-possibilities t)) 
           &body body)
  (declare (values object success string nmatches)
           (ignore allow-any-input possibility-printer help-displays-possibilities))
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (let ((string '#:string)
        (action '#:action))
    `(flet ((completing-from-suggestions-body (,string ,action)
              (suggestion-completer (,string :action ,action
                                             :partial-completers ,partial-completers)
                ,@body)))
       (declare (dynamic-extent #'completing-from-suggestions-body))
       (complete-input ,stream #'completing-from-suggestions-body ,@options))))

;; The second argument to the generator function is a function to be
;; called on a string (and object and presentation type) to suggest
;; that string.
(defmacro suggestion-completer ((string &key action partial-completers) &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (let ((function '#:function))
    `(flet ((suggestion-completer-body (,string ,function)
              (declare (ignore ,string))
              (flet ((suggest (&rest args)
                       (declare (dynamic-extent args))
                       (apply ,function args)))
                nil                ;workaround broken compilers
                ,@body)))
       (declare (dynamic-extent #'suggestion-completer-body))
       (complete-from-generator ,string #'suggestion-completer-body
                                ,partial-completers :action ,action))))

;; Very few lisp compilers seem to be able to handle the case where this top-level
;; macro is shadowed by the FLET in the continuation above.
#+Genera
(defmacro suggest (name &rest objects)
  (declare (ignore name objects))
  (error "You cannot use ~S outside of ~S" 'suggest 'completing-from-suggestions))


;;; From MENUS.LISP
;;; For now, MENU-CHOOSE requires that you pass in a parent.
(defmacro with-menu ((menu &optional (associated-window nil aw-p)
                      &rest options &key label scroll-bars) &body body)
  (declare (ignore label scroll-bars))
  (let ((window '#:associated-window))
    `(let ((,window ,(if aw-p
                         associated-window
                         `(frame-top-level-sheet *application-frame*))))        ;once-only
       (using-resource (,menu menu (window-top-level-window ,window) (window-root ,window)
                        ,@options)
         (letf-globally (((stream-default-view ,menu) +textual-menu-view+))
           ,@body)))))

;;; From ACCEPTING-VALUES.LISP
(defmacro accepting-values ((&optional stream &rest args) &body body)
  (declare (arglist (&optional stream
                     &key frame-class command-table own-window
                             background foreground text-style
                          exit-boxes resize-frame align-prompts
                          initially-select-query-identifier modify-initial-query
                          resynchronize-every-pass (check-overlapping t)
                          label x-position y-position width height (scroll-bars nil))
                    &body body))
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-input-stream stream accepting-values)
  `(flet ((accepting-values-body (,stream) ,@body))
     (declare (dynamic-extent #'accepting-values-body))
     (invoke-accepting-values ,stream #'accepting-values-body ,@args)))

(defmacro with-aligned-prompts ((stream &rest args) &body body)
  (default-input-stream stream accepting-values)
  `(flet ((with-aligned-prompts-body (,stream) ,@body))
     (declare (dynamic-extent #'with-aligned-prompts-body))
     (invoke-with-aligned-prompts ,stream #'with-aligned-prompts-body ,@args)))


;; Establish a first quadrant coordinate system, execute the body, and then
;; place the output in such a way that the upper left corner of it is where
;; the cursor was.  Finally, leave the cursor at the end of the output.
;; HEIGHT is useful when you are doing this inside of incremental redisplay,
;; and the graphics are going to change size from pass to pass.
(defmacro with-room-for-graphics ((&optional stream 
                                   &key height (first-quadrant t fq-p) (move-cursor t)
                                        record-type)
                                  &body body)
  (default-output-stream stream with-room-for-graphics)
  (unless record-type
    (setq record-type `'standard-sequence-output-record))
  `(flet ((with-room-for-graphics-body (,stream) ,@body))
     (declare (dynamic-extent #'with-room-for-graphics-body))
     (invoke-with-room-for-graphics ,stream #'with-room-for-graphics-body
                                    ,record-type ,move-cursor
                                    ,@(and height `(:height ,height))
                                    ,@(and fq-p `(:first-quadrant ,first-quadrant)))))


;;; Application frame variables
(defvar *application-frame*)
(defvar *pointer-documentation-output* nil)
(defvar *assume-all-commands-enabled* nil)

(defvar *click-outside-menu-handler* nil)

;; Bound to T when the frame is being layed out
(defvar *sizing-application-frame* nil)

(defmacro with-application-frame ((frame) &body body)
  `(let ((,frame *application-frame*))
     ,@body))


;;; Activities
(defvar *activity* nil)


;;; Command processor variables
(defvar *command-parser* 'command-line-command-parser)
(defvar *command-unparser* 'command-line-command-unparser)
(defvar *partial-command-parser* 'command-line-read-remaining-arguments-for-partial-command)


;;; Conditions

(define-condition abort-gesture (error)
  ((event :initform nil :initarg :event :reader abort-gesture-event))
  (:report
    (lambda (condition stream)
      (format stream "Abort event ~S seen" (abort-gesture-event condition)))))

(defun handle-abort-gesture (condition)
  (declare (ignore condition))
  (abort))

(defmacro catch-abort-gestures ((format-string &rest format-args) &body body)
  `(with-simple-restart (abort ,format-string ,@format-args)
     (handler-bind ((abort-gesture #'handle-abort-gesture))
       ,@body)))

(define-condition accelerator-gesture (error)
  ((event :initform nil :initarg :event
          :reader accelerator-gesture-event)
   (numeric-argument :initform 1 :initarg :numeric-argument
                     :reader accelerator-gesture-numeric-argument))
  (:report
    (lambda (condition stream)
      (format stream "Accelerator event ~S seen" (accelerator-gesture-event condition)))))


;;; Useful functions for a few things

(defun true (&rest args)
  (declare (ignore args))
  t)

(defun false (&rest args)
  (declare (ignore args))
  nil)


#+(and PCL Genera)
(progn

(walker:define-walker-template scl:letf walker::walk-let)
(walker:define-walker-template scl:letf* walker::walk-let*)
)
