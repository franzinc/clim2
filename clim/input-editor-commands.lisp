;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-
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

;; $fiHeader: input-editor-commands.lisp,v 1.1 91/09/09 12:43:33 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

;;; Define some useful input editor commands.  For now, all these are defined
;;; on INTERACTIVE-STREAM-MIXIN rather than on our specific implementation of
;;; an input editor.  This may prove to be a foolish decision down the pike.

(eval-when (compile load eval)
(defvar *ie-command-arglist* '(stream input-buffer character numeric-argument))
)

;; CHARS is either a single character, or a list of characters.  FUNCTION is
;; the function that implements an input editor command.  This associates
;; the character(s) with the command.
(defun add-input-editor-command (characters function)
  (flet ((add-aarray-entry (char thing aarray)
	   (let ((old (find char aarray :key #'first)))
	     (if old
		 (setf (second old) thing)
	         (vector-push-extend (list char thing) aarray)))))
    (declare (dynamic-extent #'add-aarray-entry))
    (cond ((characterp characters)
	   (assert (not (ordinary-char-p characters)))
	   (add-aarray-entry characters function *input-editor-command-aarray*))
	  (t
	   (assert (> (length characters) 1))
	   (assert (not (ordinary-char-p (first characters))))
	   ;; We've got a command that wil be bound to a sequence of characters,
	   ;; so set up the prefix tables.
	   (let ((aarray *input-editor-command-aarray*))
	     (dorest (rest characters)
	       (let* ((prefix (first rest))
		      (rest (rest rest)))
		 (if (null rest)
		     (add-aarray-entry prefix function aarray)
		     (let ((subaarray (second (find prefix aarray :key #'first))))
		       (when (null subaarray)
			 (setq subaarray (make-array 20 :fill-pointer 0))
			 (add-aarray-entry prefix subaarray aarray))
		       (setq aarray subaarray))))))))))

(defmacro assign-input-editor-key-bindings (&body functions-and-keystrokes)
  (let ((forms nil))
    (loop
      (when (null functions-and-keystrokes) (return))
      (let* ((function (pop functions-and-keystrokes))
	     (keystrokes (pop functions-and-keystrokes)))
	(when keystrokes
	  (push `(add-input-editor-command ',keystrokes ',function)
		forms))))
    `(progn ,@(nreverse forms))))


#+Cloe-Runtime
(sys::define-character-name "Help" 128)

;; When T, the input editor should handle help and completion.  Otherwise,
;; something like COMPLETE-INPUT will do it for us.
(defvar *ie-help-enabled* t)

;; These need to be on a per-implementation basis, naturally
;;--- If you change these, change *MAGIC-COMPLETION-CHARACTERS* too
(defvar *complete-characters* #+Genera '(#\Complete #\Tab)
			      #-Genera '(#\Tab))
(defvar *help-characters* '(#+Symbolics #\Help))
(defvar *possibilities-characters* #+Genera '(#\c-?)
				   #+Lucid  '(#\control-\?)
				   #+excl   '(#\c-?)
				   #+ccl-2  (list (extended-char #\? :control :shift)))

(defun lookup-input-editor-command (character aarray)
  ;; Need to handle the help and possibilities commands specially so
  ;; that they work correctly inside of COMPLETE-INPUT
  (cond ((and *ie-help-enabled* (member character *help-characters*))
	 'com-ie-help)
	((and *ie-help-enabled* (member character *possibilities-characters*))
	 'com-ie-possibilities)
	((and *ie-help-enabled* (member character *complete-characters*))
	 'com-ie-complete)
	(t
	 (let ((code (char-code character))
	       (bits (char-bits character)))
	   (cond ((and (eql aarray *input-editor-command-aarray*)
		       (not (zerop bits))
		       (<= (char-code #\0) code (char-code #\9)))
		  ;; A numeric argument...
		  (- code (char-code #\0)))
		 ((and (eql aarray *input-editor-command-aarray*)
		       (not (zerop bits))
		       (= code (char-code #\-)))
		  -1)
		 (t
		  (second (find character aarray :key #'first))))))))

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
	   ,@(when rescan `((immediate-rescan ,stream)))
	   (values))))))

#+Genera
(scl:defprop define-input-editor-command "CLIM Input Editor Command" si:definition-type-name)
#+Genera
(scl:defprop define-input-editor-command zwei:defselect-function-spec-finder
	     zwei:definition-function-spec-finder)

(defmethod interactive-stream-process-gesture ((istream interactive-stream-mixin) gesture type)
  (with-slots (numeric-argument last-command-type command-state) istream
    (cond ((characterp gesture)
	   ;; The COMMAND-STATE slot holds the current IE command aarray,
	   ;; and gets updated when we see a prefix characters (e.g., ESC)
	   (let ((command (lookup-input-editor-command gesture command-state)))
	     (cond ((numberp command)
		    (cond ((null numeric-argument)
			   (setq numeric-argument command))
			  ((= command -1)
			   (setq numeric-argument (- numeric-argument)))
			  (t
			   (setq numeric-argument (+ (* numeric-argument 10) command))))
		    ;; Numeric arguments don't affect LAST-COMMAND-TYPE
		    (return-from interactive-stream-process-gesture nil))
		   ((arrayp command)
		    ;; A prefix character, update the state and return
		    (setq command-state command)
		    (return-from interactive-stream-process-gesture nil))
		   (command
		    (let ((argument (or numeric-argument 1)))
		      (setq numeric-argument nil
			    command-state *input-editor-command-aarray*)
		      (funcall command
			       istream (slot-value istream 'input-buffer) gesture argument))
		    (return-from interactive-stream-process-gesture nil))
		   ((not (eq command-state *input-editor-command-aarray*))
		    (beep istream)
		    (setq numeric-argument nil
			  command-state *input-editor-command-aarray*)
		    (return-from interactive-stream-process-gesture nil))
		   (t
		    (setq last-command-type 'character
			  command-state *input-editor-command-aarray*)))))
	  (t
	   (setq numeric-argument nil
		 last-command-type 'gesture
		 command-state *input-editor-command-aarray*))))
  (values gesture type))


;;; Help commands, handled by special magic

(define-input-editor-command (com-ie-help :rescan nil)
			     (stream)
  (display-accept-help stream :help ""))

(define-input-editor-command (com-ie-possibilities :rescan nil)
			     (stream)
  (display-accept-help stream :possibilities ""))

(define-input-editor-command (com-ie-complete :rescan nil)
			     (stream input-buffer)
  (multiple-value-bind (string ambiguous word-start)
      (complete-symbol-name stream input-buffer)
    (when string
      (replace-input stream string :buffer-start word-start))
    (when (or ambiguous (null string))
      (beep stream))))

(defun complete-symbol-name (stream input-buffer &aux (colon-string ":"))
  (declare (values string ambiguous word-start))
  (multiple-value-bind (word-beginning word-end colon)
      (word-start-and-end input-buffer '(#\space #\( #\) #\") (insertion-pointer stream))
    (when word-end
      (with-temp-substring (package-name input-buffer (min colon (1+ word-beginning)) colon)
	(when (and (< colon (1- word-end))
		   (char-equal (aref input-buffer (1+ colon)) #\:))
	  (incf colon)
	  (setq colon-string "::"))
	(with-temp-substring (symbol-name input-buffer (1+ colon) (1- word-end))
	  (multiple-value-bind (new-symbol-name success object nmatches)
	      (complete-symbol-name-1 symbol-name)
	    (declare (ignore success object))
	    (when (and new-symbol-name (not (zerop nmatches)))
	      (return-from complete-symbol-name
		(values
		  (if (and colon (> colon word-beginning))
		      (format nil "~A~A~A" package-name colon-string new-symbol-name)
		      new-symbol-name)
		  (/= nmatches 1)
		  (1+ word-beginning))))))))))

#+Genera
(defun complete-symbol-name-1 (string)
  (complete-from-possibilities string (scl:g-l-p zwei:*zmacs-completion-aarray*) '(#\-)
			       :action :complete-maximal))

#-Genera
(defun complete-symbol-name-1 (string)
  nil)

(define-input-editor-command (com-ie-scroll-forward :rescan nil)
			     (stream numeric-argument)
  (ie-scroll-window numeric-argument +1))

(define-input-editor-command (com-ie-scroll-backward :rescan nil)
			     (stream numeric-argument)
  (ie-scroll-window numeric-argument -1))

;; Scroll the frame's standard output stream vertically by some amount, 
;; one screenful being the default.
(defun ie-scroll-window (distance direction)
  (let* ((window (frame-standard-output *application-frame*))
	 (history (and window
		       (output-recording-stream-p window)
		       (output-recording-stream-output-record window))))
    (when window
      (multiple-value-bind (x y) (window-viewport-position* window)
	(incf y (* (if (= distance 1)
		       (bounding-rectangle-height (window-viewport window))
		       (* distance (stream-line-height window)))
		   direction))
	(with-bounding-rectangle* (hleft htop hright hbottom) history
	  (declare (ignore hleft hright))
	  (setq y (min (max htop y) hbottom)))
	(window-set-viewport-position* window x y)))))


;;; Some macrology for talking about the input-buffer

(defmacro ie-line-start (buffer pointer)
  `(1+ (or (position #\Newline ,buffer :end ,pointer :from-end t)
	   -1)))

(defmacro ie-line-end (buffer pointer)
  `(or (position #\Newline ,buffer :start ,pointer)
       (fill-pointer ,buffer)))

;; Things which move over words must move over whitespace until they see
;; alphanumerics, then alphanumerics until they see whitespace.
(defun word-break-character-p (thing)
  (when (and (characterp thing)
	     (not (alphanumericp thing)))
    (values t t)))

(defun word-character-p (thing)
  (when (and (characterp thing)
	     (alphanumericp thing))
    (values t t)))

(defun move-over-word (buffer start-position reverse-p)
  (setq start-position
	(forward-or-backward buffer start-position reverse-p #'word-character-p))
  (when start-position
    (forward-or-backward buffer start-position reverse-p #'word-break-character-p)))


;;; The input editing commands...

;; Don't do anything
(define-input-editor-command (com-ie-ctrl-g :rescan nil)
			     (stream)
  (with-slots (numeric-argument command-state) stream
    (setq numeric-argument nil
	  command-state *input-editor-command-aarray*))
  (beep stream))

(define-input-editor-command (com-ie-universal-argument :rescan nil :type nil)
			     (stream numeric-argument)
  (setf (slot-value stream 'numeric-argument)
	(* (or numeric-argument 1) 4)))

(define-input-editor-command (com-ie-forward-character :rescan nil)
			     (stream input-buffer numeric-argument)
  (dotimes (i numeric-argument) 
    #-excl (declare (ignore i))
    (let ((p (forward-or-backward input-buffer (insertion-pointer stream) nil #'true)))
      (if p
	  (setf (insertion-pointer stream) p)
	  (return (beep stream))))))

(define-input-editor-command (com-ie-forward-word :rescan nil)
			     (stream input-buffer numeric-argument)
  (dotimes (i numeric-argument)
    #-excl (declare (ignore i))
    (let ((p (move-over-word input-buffer (insertion-pointer stream) nil)))
      (if p
	  (setf (insertion-pointer stream) p)
	  (return (beep stream))))))

(define-input-editor-command (com-ie-backward-character :rescan nil)
			     (stream input-buffer numeric-argument)
  (dotimes (i numeric-argument)
    #-excl (declare (ignore i))
    (let ((p (forward-or-backward input-buffer (insertion-pointer stream) t #'true)))
      (if p
	  (setf (insertion-pointer stream) p)
	  (return (beep stream))))))

(define-input-editor-command (com-ie-backward-word :rescan nil)
			     (stream input-buffer numeric-argument)
  (dotimes (i numeric-argument)
    #-excl (declare (ignore i))
    (let ((p (move-over-word input-buffer (insertion-pointer stream) t)))
      (if p
	  (setf (insertion-pointer stream) p)
	  (return (beep stream))))))

(define-input-editor-command (com-ie-beginning-of-buffer :rescan nil :type nil)
			     (stream)
  (setf (insertion-pointer stream) 0))

(define-input-editor-command (com-ie-end-of-buffer :rescan nil)
			     (stream input-buffer)
  (setf (insertion-pointer stream) (fill-pointer input-buffer)))

(define-input-editor-command (com-ie-beginning-of-line :rescan nil)
			     (stream input-buffer)
  (setf (insertion-pointer stream)
	(ie-line-start input-buffer (insertion-pointer stream))))

(define-input-editor-command (com-ie-end-of-line :rescan nil)
			     (stream input-buffer)
  (setf (insertion-pointer stream)
	(ie-line-end input-buffer (insertion-pointer stream))))

;; Positions to the nearest column in the next line
(define-input-editor-command (com-ie-next-line :rescan nil)
			     (stream input-buffer numeric-argument)
  (unless (= (insertion-pointer stream) (fill-pointer input-buffer))
    (ie-next-previous stream input-buffer numeric-argument)))

(define-input-editor-command (com-ie-previous-line :rescan nil)
			     (stream input-buffer numeric-argument)
  (unless (zerop (insertion-pointer stream))
    (ie-next-previous stream input-buffer (- numeric-argument))))

;;; +ve = next, -ve = previous
(defun ie-next-previous (stream input-buffer numeric-argument)
  (unless (zerop numeric-argument)
    (let* ((pointer (insertion-pointer stream))
	   (this-line (ie-line-start input-buffer pointer))
	   (target-line this-line))
      (if (plusp numeric-argument)
	  (let (next-line-1)
	    (dotimes (i numeric-argument)
	      #-excl (declare (ignore i))
	      (setq next-line-1 (position #\Newline input-buffer :start target-line))
	      (unless next-line-1 (return))
	      (setq target-line (1+ next-line-1)))
	    (setf (insertion-pointer stream)
		  (let ((position-in-line (- pointer this-line)))
		    (min (+ target-line position-in-line)
			 (ie-line-end input-buffer target-line)))))
	  (let (prev-line-end)
	    (dotimes (i (- numeric-argument))
	      #-excl (declare (ignore i))
	      (setq prev-line-end (position #\Newline input-buffer
					    :end target-line :from-end t))
	      (unless prev-line-end (return))
	      (setq target-line prev-line-end))
	    (setf (insertion-pointer stream)
		  (let ((position-in-line (- pointer this-line)))
		    (min (+ (ie-line-start input-buffer target-line)
			    position-in-line)
			 (ie-line-end input-buffer target-line)))))))))

(define-input-editor-command (com-ie-rubout :type delete)
			     (stream input-buffer numeric-argument)
  (ie-rub-del stream input-buffer (- numeric-argument)))

(define-input-editor-command (com-ie-delete-character :type delete)
			     (stream input-buffer numeric-argument)
  (ie-rub-del stream input-buffer numeric-argument))

;;; +ve = delete, -ve = rubout
(defun ie-rub-del (stream input-buffer numeric-argument)
  (let* ((p1 (insertion-pointer stream))
	 (p2 p1)
	 (reverse-p (minusp numeric-argument))) 
    (dotimes (i (abs numeric-argument))
      #-excl (declare (ignore i))
      (let ((p3 (forward-or-backward input-buffer p2 reverse-p #'true)))
	(if p3 (setq p2 p3) (return))))
    (if (/= p1 p2)
	(ie-kill stream input-buffer
		 (cond ((eql (slot-value stream 'last-command-type) 'kill) :merge)
		       ((> (abs numeric-argument) 1) t)
		       (t nil))
		 p2 p1 reverse-p)
        (beep stream))))

(define-input-editor-command (com-ie-rubout-word :type kill)
			     (stream input-buffer numeric-argument)
  (ie-rub-del-word stream input-buffer (- numeric-argument)))

(define-input-editor-command (com-ie-delete-word :type kill)
			     (stream input-buffer numeric-argument)
  (ie-rub-del-word stream input-buffer numeric-argument))

;;; +ve = delete, -ve = rubout
(defun ie-rub-del-word (stream input-buffer numeric-argument)
  (let* ((p1 (insertion-pointer stream))
	 (p2 p1)
	 (reverse-p (minusp numeric-argument))) 
    (dotimes (i (abs numeric-argument))
      #-excl (declare (ignore i))
      (let ((p3 (move-over-word input-buffer p2 reverse-p)))
	(if p3 (setq p2 p3) (return))))
    (if (/= p1 p2)
	(ie-kill stream input-buffer
		 (if (eql (slot-value stream 'last-command-type) 'kill) :merge t)
		 p2 p1 reverse-p)
        (beep stream))))

(define-input-editor-command (com-ie-clear-input :type kill)
			     (stream input-buffer)
  ;; Just push a copy of the input buffer onto the kill ring, no merging
  (ie-kill stream input-buffer t 0 (fill-pointer input-buffer)))

(define-input-editor-command (com-ie-kill-line :type kill)
			     (stream input-buffer numeric-argument)
  (let* ((reverse-p (minusp numeric-argument))
	 (point (insertion-pointer stream))
	 (other-point (if reverse-p
			  (ie-line-start input-buffer point)
			  (ie-line-end input-buffer point))))
    (ie-kill stream input-buffer
	     (if (eql (slot-value stream 'last-command-type) 'kill) :merge t)
	     point
	     other-point
	     reverse-p)))

(define-input-editor-command (com-ie-make-room)
			     (stream input-buffer)
  (let ((point (insertion-pointer stream))
	(end (fill-pointer input-buffer)))
    (cond ((= point end)
	   (incf (fill-pointer input-buffer)))
	  (t
	   (erase-input-buffer stream point)
	   (shift-buffer-portion input-buffer point (1+ point))))
    (setf (aref input-buffer point) #\Newline)
    (redraw-input-buffer stream point)))

(define-input-editor-command (com-ie-transpose-characters)
			     (stream input-buffer)
  (let* ((start-position (min (1+ (insertion-pointer stream))
			      (fill-pointer input-buffer)))
	 (this-position (forward-or-backward input-buffer start-position t #'true))
	 (prev-position (forward-or-backward input-buffer this-position t #'true)))
    (cond ((and this-position prev-position (/= this-position prev-position))
	   (let ((this-char (aref input-buffer this-position))
		 (prev-char (aref input-buffer prev-position)))
	     (erase-input-buffer stream prev-position)
	     (setf (aref input-buffer prev-position) this-char)
	     (setf (aref input-buffer this-position) prev-char)
	     (redraw-input-buffer stream prev-position)))
	  (t (beep stream)))))

(defun function-arglist (function)
  (declare (values arglist found-p))
  #+Genera (values (sys:arglist function) T)
  #+excl (values (excl::arglist function) t)
  #+Lucid (values (lucid-common-lisp:arglist function) t))

(defun word-start-and-end (string start-chars &optional (start 0))
  (declare (values word-beginning word-end colon))
  (flet ((open-paren-p (thing)
	   (and (characterp thing)
		(member thing start-chars)))
	 (atom-break-char-p (thing)
	   (and (characterp thing)
		(or (not (graphic-char-p thing))
		    (multiple-value-bind (mac nt)
			(get-macro-character thing)
		      (and mac (not nt)))
		    (member thing '(#\space #\( #\) #\"))))))
    (declare (dynamic-extent #'open-paren-p #'atom-break-char-p))
    (let* ((word-beginning
	     (forward-or-backward string start t #'open-paren-p))
	   (word-end
	     (and word-beginning
		  (or (forward-or-backward string (1+ word-beginning) nil
					   #'atom-break-char-p)
		      (fill-pointer string))))
	   (colon
	     (and word-beginning word-beginning
		  (or (position #\: string
				:start word-beginning :end (1- word-end))
		      word-beginning))))
      (values word-beginning word-end colon))))

(define-input-editor-command (com-ie-show-arglist :rescan nil)
			     (stream input-buffer)
  (multiple-value-bind (word-beginning word-end colon)
      (word-start-and-end input-buffer '(#\( ) (insertion-pointer stream))
    (block doit
      (when word-end
	(with-temp-substring (package-name input-buffer
					   (min colon (1+ word-beginning)) colon)
	  (when (and (< colon (1- word-end))
		     (char-equal (aref input-buffer (1+ colon)) #\:))
	    (incf colon))
	  (with-temp-substring (symbol-name input-buffer (1+ colon) (1- word-end))
	    (let* ((symbol (find-symbol (string-upcase symbol-name)
					(if (= colon word-beginning)
					    *package*
					    (find-package package-name))))
		   (function (and symbol (fboundp symbol) (symbol-function symbol))))
	      (when function
		(multiple-value-bind (arglist found-p)
		    (function-arglist function)
		  (when found-p
		    (return-from doit
		      (with-input-editor-typeout (stream)
			(format stream "~S: ~A" symbol arglist))))))))))
      (beep stream))))

(define-input-editor-command (com-ie-show-value :rescan nil)
			     (stream input-buffer)
  (multiple-value-bind (word-beginning word-end colon)
      (word-start-and-end input-buffer '(#\space #\( #\) #\") (insertion-pointer stream))
    (block doit
      (when word-end
	(with-temp-substring (package-name input-buffer (min colon (1+ word-beginning)) colon)
	  (when (and (< colon (1- word-end))
		     (char-equal (aref input-buffer (1+ colon)) #\:))
	    (incf colon))
	  (with-temp-substring (symbol-name input-buffer (1+ colon) (1- word-end))
	    (let* ((symbol (find-symbol (string-upcase symbol-name)
					(if (= colon word-beginning)
					    *package*
					    (find-package package-name))))
		   (value (and symbol (boundp symbol) (symbol-value symbol))))
	      (when value
		(return-from doit
		  (with-input-editor-typeout (stream)
		    (format stream "~S: ~S" symbol value))))))))
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
		  ;;--- I don't like this, see comment on RESCAN-FOR-ACTIVATION
		  (queue-rescan istream ':activation))
		 (t (beep)))))
	(t
	 (let ((element (funcall function history :index numeric-argument :test test)))
	   (cond (element
		  (history-replace-input history istream element
					 :replace-previous replace-previous)
		  (queue-rescan istream ':activation))
		 (t (beep)))))))

(define-input-editor-command (com-ie-kill-ring-yank :history t :type yank :rescan nil)
			     (stream numeric-argument)
  (ie-yank-from-history *kill-ring* #'yank-from-history stream numeric-argument))

(define-input-editor-command (com-ie-history-yank :history t :type yank :rescan nil)
			     (stream numeric-argument)
  (let ((history
	  (and *presentation-type-for-yanking*
	       (presentation-type-history *presentation-type-for-yanking*))))
    (if history
	(ie-yank-from-history history #'yank-from-history stream numeric-argument)
        (beep))))

(define-input-editor-command (com-ie-yank-next :history t :type yank :rescan nil)
			     (stream numeric-argument)
  (let ((history (slot-value stream 'previous-history)))
    (if history
	(ie-yank-from-history history #'yank-next-from-history stream numeric-argument
			      :replace-previous t)
        (beep))))


;;; Per-platform key bindings

#+Genera
(assign-input-editor-key-bindings
  com-ie-ctrl-g		       #\c-G
  com-ie-universal-argument    #\c-U
  com-ie-forward-character     #\c-F
  com-ie-forward-word	       #\m-F
  com-ie-backward-character    #\c-B
  com-ie-backward-word	       #\m-B
  com-ie-beginning-of-buffer   #\m-<
  com-ie-end-of-buffer	       #\m->
  com-ie-beginning-of-line     #\c-A
  com-ie-end-of-line	       #\c-E
  com-ie-next-line	       #\c-N
  com-ie-previous-line	       #\c-P
  com-ie-rubout		       #\Rubout
  com-ie-delete-character      #\c-D
  com-ie-rubout-word	       #\m-Rubout
  com-ie-delete-word	       #\m-D
  com-ie-clear-input	       #\Clear-Input
  com-ie-kill-line	       #\c-K
  com-ie-make-room	       #\c-O
  com-ie-transpose-characters  #\c-T
  com-ie-show-arglist	       #\c-sh-A
  com-ie-show-value	       #\c-sh-V
  com-ie-kill-ring-yank	       #\c-Y
  com-ie-history-yank	       #\c-m-Y
  com-ie-yank-next	       #\m-Y
  com-ie-scroll-forward	       #\Scroll
  com-ie-scroll-backward       #\meta-Scroll
  com-ie-scroll-forward	       #\c-V
  com-ie-scroll-backward       #\meta-V)

#+Cloe-Runtime
(assign-input-editor-key-bindings
  com-ie-ctrl-g		       #\BEL
  com-ie-universal-argument    nil    
  com-ie-beginning-of-buffer   #\m-<
  com-ie-end-of-buffer	       #\m->
  com-ie-forward-character     #\ACK
  com-ie-forward-word	       #\m-F
  com-ie-backward-character    #\STX
  com-ie-backward-word	       #\m-B
  com-ie-beginning-of-line     #\SOH
  com-ie-end-of-line	       #\ENQ
  com-ie-next-line	       #\SO
  com-ie-previous-line	       #\DLE
  com-ie-rubout		       #\Rubout
  com-ie-delete-character      #\Eot
  com-ie-rubout-word	       #\m-Rubout
  com-ie-delete-word	       #\m-D
  com-ie-clear-input	       #\Del
  com-ie-kill-line	       #\Vt
  com-ie-make-room	       nil
  com-ie-transpose-characters  #\DC4
  com-ie-show-arglist	       #\c-sh-A
  com-ie-show-value	       #\c-sh-V
  com-ie-kill-ring-yank	       #\EM
  com-ie-history-yank	       #\c-m-Y
  com-ie-yank-next	       #\m-Y
  com-ie-scroll-forward        #\Syn
  com-ie-scroll-backward       #\m-V)

#+Lucid
(assign-input-editor-key-bindings
  com-ie-ctrl-g		       #\Control-\g
  com-ie-universal-argument    #\Control-\u
  com-ie-forward-character     #\Control-\f
  com-ie-forward-word	       #\Meta-\f
  com-ie-backward-character    #\Control-\b
  com-ie-backward-word	       #\Meta-\b
  com-ie-beginning-of-buffer   #\Meta-<
  com-ie-end-of-buffer	       #\Meta->
  com-ie-beginning-of-line     #\Control-\a
  com-ie-end-of-line	       #\Control-\e
  com-ie-next-line	       #\Control-\n
  com-ie-previous-line	       #\Control-\p
  com-ie-rubout		       #\Rubout
  com-ie-delete-character      #\Control-\d
  com-ie-rubout-word	       #\Meta-Rubout
  com-ie-delete-word	       #\Meta-\d
  com-ie-clear-input	       #\Control-Meta-Rubout
  com-ie-kill-line	       #\Control-\k
  com-ie-make-room	       #\Control-\o
  com-ie-transpose-characters  #\Control-\t
  com-ie-show-arglist	       #\Control-\A
  com-ie-show-value	       #\Control-\V
  com-ie-kill-ring-yank	       #\Control-\y
  com-ie-history-yank	       #\Control-Meta-\y
  com-ie-yank-next	       #\Meta-\y
  com-ie-scroll-forward	       #\Control-\v
  com-ie-scroll-backward       #\Meta-\v)

#+excl
;; Like about but lowercase characters
(assign-input-editor-key-bindings
 com-ie-ctrl-g		       #\c-\g
 com-ie-universal-argument    nil
 com-ie-forward-character     #\c-\f
 com-ie-forward-word	       (#\escape #\f)
 com-ie-forward-word	       #\meta-\f
 com-ie-backward-character    #\c-\b
 com-ie-backward-word	       (#\escape #\b)
 com-ie-backward-word	       #\meta-\b
 com-ie-beginning-of-buffer   (#\escape #\<)
 com-ie-beginning-of-buffer   #\meta-<
 com-ie-end-of-buffer	       (#\escape #\>)
 com-ie-end-of-buffer	       #\meta->
 com-ie-beginning-of-line     #\c-\a
 com-ie-end-of-line	       #\c-\e
 com-ie-next-line	       #\c-\n
 com-ie-previous-line	       #\c-\p
 com-ie-rubout		       #\rubout
 com-ie-delete-character      #\c-\d
 com-ie-rubout-word	       (#\escape #\rubout)
 com-ie-rubout-word	       #\meta-rubout
 com-ie-delete-word	       (#\escape #\d)
 com-ie-delete-word	       #\meta-\d
 com-ie-clear-input	       #\c-\u
 com-ie-kill-line	       #\c-\k
 com-ie-make-room	       #\c-\o
 com-ie-transpose-characters  #\c-\t
 com-ie-show-arglist	       (#\c-\x #\c-\a)
 com-ie-show-value	       (#\c-\x #\c-\v)
 com-ie-kill-ring-yank	       #\c-\y
 com-ie-history-yank	       (#\escape #\c-\y)
 com-ie-history-yank	       #\meta-c-\y
 com-ie-yank-next	       (#\escape #\y)
 com-ie-yank-next	       #\meta-\y
 com-ie-scroll-forward	       #\c-\v
 com-ie-scroll-backward       (#\escape #\v)
 com-ie-scroll-backward       #\meta-\v)

#+ccl-2
(defmacro assign-input-editor-key-bindings-ccl (&body functions-and-keystrokes)
  (let ((forms nil))
    (loop
      (when (null functions-and-keystrokes) (return))
      (let* ((function (pop functions-and-keystrokes))
             (keystroke (pop functions-and-keystrokes)))
        (when keystroke
          (push `(add-input-editor-command ,keystroke ',function)
                forms))))
    `(progn ,@(nreverse forms))))

#+ccl-2
(assign-input-editor-key-bindings-ccl
  com-ie-ctrl-g		       (extended-char #\g :control)
  com-ie-universal-argument    (extended-char #\u :control)
  com-ie-forward-character     (extended-char #\f :control)
  com-ie-forward-word	       (extended-char #\f :meta)
  com-ie-backward-character    (extended-char #\b :control)
  com-ie-backward-word	       (extended-char #\b :meta)
  com-ie-beginning-of-buffer   (extended-char #\< :meta)
  com-ie-end-of-buffer	       (extended-char #\> :meta)
  com-ie-beginning-of-line     (extended-char #\a :control)
  com-ie-end-of-line	       (extended-char #\e :control)
  com-ie-next-line	       (extended-char #\n :control)
  com-ie-previous-line	       (extended-char #\p :control)
  com-ie-rubout		       #\Rubout
  com-ie-delete-character      (extended-char #\d :control)
  com-ie-rubout-word	       (extended-char #\Rubout :meta)
  com-ie-delete-word	       (extended-char #\d :meta)
  com-ie-clear-input	       (extended-char #\Rubout :control :meta)
  com-ie-kill-line	       (extended-char #\k :control)
  com-ie-make-room	       (extended-char #\o :control)
  com-ie-transpose-characters  (extended-char #\t :control)
  com-ie-show-arglist	       (extended-char #\A :control :shift)
  com-ie-show-value	       (extended-char #\V :control :shift)
  com-ie-kill-ring-yank	       (extended-char #\y :control)
  com-ie-history-yank	       (extended-char #\y :control :meta)
  com-ie-yank-next	       (extended-char #\y :meta)
  com-ie-scroll-forward	       (extended-char #\v :control)
  com-ie-scroll-backward       (extended-char #\v :meta))

;#+KCL
;(assign-input-editor-key-bindings
;  com-ie-ctrl-g nil
;  com-ie-frob-numeric-arg nil
;  com-ie-beginning-of-buffer nil
;  com-ie-end-of-buffer nil
;  com-ie-forward-character #\^F
;  com-ie-forward-word nil
;  com-ie-backward-character #\^B 
;  com-ie-backward-word nil
;  com-ie-beginning-of-line #\^A
;  com-ie-end-of-line #\^E
;  com-ie-next-line nil
;  com-ie-previous-line nil
;  com-ie-rubout #\Rubout
;  com-ie-delete-character #\^D
;  com-ie-rubout-word nil
;  com-ie-delete-word nil
;  com-ie-clear-input nil
;  com-ie-kill-line #\^K
;  com-ie-make-room nil
;  com-ie-transpose-characters #\^T
;  com-ie-show-arglist nil
;  com-ie-show-value nil
;  com-ie-kill-ring-yank nil 
;  com-ie-history-yank nil
;  com-ie-yank-next nil
;  )
