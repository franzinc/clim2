;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: input-editor-commands.lisp,v 1.10 92/07/01 15:46:34 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Define some useful input editor commands.  For now, all these are defined
;;; on INPUT-EDITING-STREAM-MIXIN rather than on our specific implementation of
;;; an input editor.  This may prove to be a foolish decision down the pike.

(eval-when (compile load eval)
(defvar *ie-command-arglist* '(stream input-buffer gesture numeric-argument))
)

;; GESTURES is either a gesture name, or a list of gesture names.  FUNCTION
;; is the function that implements an input editor command.  This associates
;; the gesture(s) with the command.
(defun add-input-editor-command (gestures function)
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
	     (if (zerop modifier-state)
		 ;;--- What should this really be?
		 (member keysym '(:rubout :clear-input :scroll :refresh))
		 (logtest modifier-state
			  #.(logior +control-key+ +meta-key+ +super-key+ +hyper-key+))))))
    (declare (dynamic-extent #'add-aarray-entry #'bucky-char-p))
    (cond ((atom gestures)
	   (assert (bucky-char-p gestures) (gestures)
		   "~S does not correspond to a bucky character" gestures)
	   (add-aarray-entry gestures function *input-editor-command-aarray*))
	  (t
	   (assert (> (length gestures) 1))
	   (assert (bucky-char-p (first gestures)) (gestures)
		   "~S does not correspond to a bucky character" gestures)
	   ;; We've got a command that wil be bound to a sequence of gestures,
	   ;; so set up the prefix tables.
	   (let ((aarray *input-editor-command-aarray*))
	     (dorest (rest gestures)
	       (let* ((prefix (first rest))
		      (rest (rest rest)))
		 (if (null rest)
		     (add-aarray-entry prefix function aarray)
		     (let ((subaarray (second (find prefix aarray :key #'first))))
		       (when (null subaarray)
			 (setq subaarray (make-array 40 :fill-pointer 0))
			 (add-aarray-entry prefix subaarray aarray))
		       (setq aarray subaarray))))))))))


;; When T, the input editor should handle help and completion.  Otherwise,
;; something like COMPLETE-INPUT will do it for us.
(defvar *ie-help-enabled* t)

(define-gesture-name :complete :keyboard (:tab))
(define-gesture-name :complete :keyboard (:complete))
(define-gesture-name :help     :keyboard (:help))
;;--- Both of these because of a bug in KEYBOARD-EVENT-MATCHES-GESTURE-NAME-P
;;--- that causes control-? not to match sometimes
(define-gesture-name :possibilities :keyboard (:? :control))
(define-gesture-name :possibilities :keyboard (:? :control :shift))

;; These need to be on a per-implementation basis, naturally
;;--- If you change these, change *MAGIC-COMPLETION-GESTURES* too
(defvar *completion-gestures* '(:complete))
(defvar *help-gestures* '(:help))
(defvar *possibilities-gestures* '(:possibilities))

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
	      (member gesture *completion-gestures*
		      :test #'keyboard-event-matches-gesture-name-p))
	 'com-ie-complete)
	(t
	 (let* ((keysym (keyboard-event-key-name gesture))
		(modifier-state (event-modifier-state gesture))
		(bucky-p
		  (logtest modifier-state
			   #.(logior +control-key+ +meta-key+ +super-key+ +hyper-key+))))
	   (cond ((and (eq aarray *input-editor-command-aarray*)
		       bucky-p
		       ;; If a numeric argument, return the digit
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

;; No input editing commands are standard characters...
(defmethod stream-process-gesture ((istream input-editing-stream-mixin)
				   (gesture character) type)
  (with-slots (last-command-type command-state) istream
    (setq last-command-type 'character
	  command-state *input-editor-command-aarray*))
  (values gesture type))

(defmethod stream-process-gesture ((istream input-editing-stream-mixin) 
				   (gesture key-press-event) type)
  (with-slots (numeric-argument last-command-type command-state) istream
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
		     command-state *input-editor-command-aarray*)
	       (funcall command
			istream (slot-value istream 'input-buffer) gesture argument))
	     (return-from stream-process-gesture nil))
	    ((not (eq command-state *input-editor-command-aarray*))
	     (beep istream)
	     (setq numeric-argument nil
		   command-state *input-editor-command-aarray*)
	     (return-from stream-process-gesture nil))
	    (t
	     (setq last-command-type 'character
		   command-state *input-editor-command-aarray*)))))
  (values gesture type))

;;--- This method never gets run because STREAM-READ-CHAR just gives up
;;--- Maybe it would be better to use a BLANK-AREA translator while in
;;--- the INPUT-EDITOR context?
(defmethod stream-process-gesture ((istream input-editing-stream-mixin)
				   (gesture pointer-button-press-event) type)
  (when (eq (pointer-event-button gesture) +pointer-left-button+)
    (let ((position (compute-input-buffer-input-position
		      istream (pointer-event-x gesture) (pointer-event-y gesture))))
      (when position
	(setf (insertion-pointer istream) position)
	(return-from stream-process-gesture (values nil nil)))))
  (values gesture type))
      
;;--- A work in progress...
(defmethod compute-input-buffer-input-position ((istream input-editing-stream-mixin) x y)
  (multiple-value-bind (cursor-x cursor-y baseline height style max-x)
      (decode-stream-for-writing istream)
    ;; Cache some slot variables since we will not be writing them.
    (let* ((input-buffer (slot-value istream 'input-buffer))
	   (stream (slot-value istream 'stream))
	   (medium (sheet-medium stream))
	   (noisy-style (merge-text-styles *noise-string-style* style))
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
		    (multiple-value-setq (cursor-x cursor-y height baseline)
		      (do-text-screen-real-estate
			stream #'true (noise-string-display-string noise-string) 0 nil
			cursor-x cursor-y height baseline noisy-style max-x)))
		nil)))
	index))))


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
      (beep stream))
    (when string
      (queue-rescan stream ':activation))))

(defun complete-symbol-name (stream input-buffer &aux (colon-string ":"))
  (declare (values string ambiguous word-start))
  (multiple-value-bind (word-start word-end colon)
      (word-start-and-end input-buffer '(#\space #\( #\) #\") (insertion-pointer stream))
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
  (complete-from-possibilities string (scl:g-l-p zwei:*zmacs-completion-aarray*) '(#\-)
			       :action :complete-maximal))

#-Genera
(defun complete-symbol-name-1 (string)
  nil)

(define-input-editor-command (com-ie-refresh :rescan nil)
			     (stream)
  (stream-replay stream)
  (redraw-input-buffer stream))

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
		       (stream-output-history window))))
    (when (and window
	       (pane-viewport window))
      (multiple-value-bind (x y) (window-viewport-position window)
	(incf y (* (if (= distance 1)
		       (bounding-rectangle-height (window-viewport window))
		       (* distance (stream-line-height window)))
		   direction))
	(with-bounding-rectangle* (hleft htop hright hbottom) history
	  (declare (ignore hleft hright))
	  (setq y (min (max htop y) hbottom)))
	(window-set-viewport-position window x y)))))


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
      (forward-or-backward buffer start-position reverse-p #'word-break-character-p))))


;;; The basic input editing commands...

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
  (repeat (abs numeric-argument)
    (let ((p (forward-or-backward input-buffer (insertion-pointer stream)
				  (minusp numeric-argument) #'true)))
      (if p
	  (setf (insertion-pointer stream) p)
	  (return (beep stream))))))

(define-input-editor-command (com-ie-forward-word :rescan nil)
			     (stream input-buffer numeric-argument)
  (repeat (abs numeric-argument)
    (let ((p (move-over-word input-buffer (insertion-pointer stream) 
			     (minusp numeric-argument))))
      (if p
	  (setf (insertion-pointer stream) p)
	  (return (beep stream))))))

(define-input-editor-command (com-ie-backward-character :rescan nil)
			     (stream input-buffer numeric-argument)
  (repeat (abs numeric-argument)
    (let ((p (forward-or-backward input-buffer (insertion-pointer stream)
				  (plusp numeric-argument) #'true)))
      (if p
	  (setf (insertion-pointer stream) p)
	  (return (beep stream))))))

(define-input-editor-command (com-ie-backward-word :rescan nil)
			     (stream input-buffer numeric-argument)
  (repeat (abs numeric-argument)
    (let ((p (move-over-word input-buffer (insertion-pointer stream)
			     (plusp numeric-argument))))
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
	    (repeat numeric-argument
	      (setq next-line-1 (position #\Newline input-buffer :start target-line))
	      (unless next-line-1 (return))
	      (setq target-line (1+ next-line-1)))
	    (setf (insertion-pointer stream)
		  (let ((position-in-line (- pointer this-line)))
		    (min (+ target-line position-in-line)
			 (ie-line-end input-buffer target-line)))))
	  (let (prev-line-end)
	    (repeat (- numeric-argument)
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
    (repeat (abs numeric-argument)
      (let ((p3 (move-over-word input-buffer p2 reverse-p)))
	(if p3 (setq p2 p3) (return))))
    (if (/= p1 p2)
	(ie-kill stream input-buffer
		 (if (eq (slot-value stream 'last-command-type) 'kill) :merge t)
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
	     (if (eq (slot-value stream 'last-command-type) 'kill) :merge t)
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


;;; Lispy input editing commands

(defun function-arglist (function)
  (declare (values arglist found-p))
  #+Genera (values (sys:arglist function) T)
  #+Cloe-Runtime (values (sys::arglist function) t)
  #+Allegro (values (excl::arglist function) t)
  #+Lucid (values (lucid-common-lisp:arglist function) t))

#+Cloe-Runtime
(defun sys::arglist (symbol)
  (let ((fsanda (si::sys%get symbol 'arglist))
	(argl nil)
	(fun nil))
    (unless fsanda (setq fsanda (get symbol 'arglist)))
    (if fsanda
	(progn
	  (setq fun (car fsanda) argl (cadr fsanda))
	  (return-from sys::arglist (values argl fun)))
	(return-from  sys::arglist (values nil nil)))))

(defun word-start-and-end (string start-chars &optional (start 0))
  (declare (values word-start word-end colon))
  (flet ((open-paren-p (thing)
	   (or (not (characterp thing))		;noise strings and blips are delimiters
	       (member thing start-chars)))
	 (atom-break-char-p (thing)
	   (or (not (characterp thing))		;ditto
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

(define-input-editor-command (com-ie-show-arglist :rescan nil)
			     (stream input-buffer)
  (multiple-value-bind (word-start word-end colon)
      (word-start-and-end input-buffer '(#\( ) (insertion-pointer stream))
    (block doit
      (when word-end
	(with-temporary-substring
	    (package-name input-buffer word-start (or colon word-start))
	  (when (and colon
		     (< colon word-end)
		     (char-equal (aref input-buffer (1+ colon)) #\:))
	    (incf colon))
	  (with-temporary-substring
	      (symbol-name input-buffer (if colon (1+ colon) word-start) word-end)
	    (let* ((symbol (find-symbol (string-upcase symbol-name)
					(if colon (find-package package-name) *package*)))
		   (function (and symbol (fboundp symbol) (symbol-function symbol))))
	      (when function
		(multiple-value-bind (arglist found-p)
		    (function-arglist function)
		  (when found-p
		    (return-from doit
		      (with-input-editor-typeout (stream)
			#-Cloe-Runtime
			(format stream "~S: (~{~A~^ ~})"
			  symbol arglist)
			#+Cloe-Runtime
			(format stream "~S (~A): (~{~:A~^ ~})"
			  symbol found-p arglist))))))))))
      (beep stream))))

(define-input-editor-command (com-ie-show-value :rescan nil)
			     (stream input-buffer)
  (multiple-value-bind (word-start word-end colon)
      (word-start-and-end input-buffer '(#\space #\( #\) #\") (insertion-pointer stream))
    (block doit
      (when word-end
	(with-temporary-substring
	    (package-name input-buffer word-start (or colon word-start))
	  (when (and colon
		     (< colon word-end)
		     (char-equal (aref input-buffer (1+ colon)) #\:))
	    (incf colon))
	  (with-temporary-substring
	      (symbol-name input-buffer (if colon (1+ colon) word-start) word-end)
	    (let* ((symbol (find-symbol (string-upcase symbol-name)
					(if colon (find-package package-name) *package*)))
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
		 (t (beep istream)))))
	(t
	 (let ((element (funcall function history :index numeric-argument :test test)))
	   (cond (element
		  (history-replace-input history istream element
					 :replace-previous replace-previous)
		  (queue-rescan istream ':activation))
		 (t (beep istream)))))))

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
        (beep stream))))

(define-input-editor-command (com-ie-yank-next :history t :type yank :rescan nil)
			     (stream numeric-argument)
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
  (:ie-abort                :\G  :control)
  (:ie-universal-argument   :\U  :control)
  (:ie-forward-character    :\F  :control)
  (:ie-forward-word	    :\F  :meta)
  (:ie-backward-character   :\B  :control)
  (:ie-backward-word	    :\B  :meta)
  (:ie-beginning-of-buffer  :\<  :meta)
  (:ie-end-of-buffer	    :\>  :meta)
  (:ie-beginning-of-line    :\A  :control)
  (:ie-end-of-line	    :\E  :control)
  (:ie-next-line	    :\N  :control)
  (:ie-previous-line	    :\P  :control)
  (:ie-delete-character	    :\D  :control)
  (:ie-delete-word	    :\D  :meta)
  (:ie-rubout-character     :rubout)
  (:ie-rubout-word	    :rubout :meta)
  (:ie-kill-line	    :\K  :control)
  (:ie-clear-input	    :clear-input)
  (:ie-make-room	    :\O  :control)
  (:ie-transpose-characters :\T  :control)
  (:ie-show-arglist	    :\A  :control :shift)
  (:ie-show-value	    :\V  :control :shift)
  (:ie-kill-ring-yank	    :\Y  :control)
  (:ie-history-yank	    :\Y  :control :meta)
  (:ie-yank-next	    :\Y  :meta)
  (:ie-refresh		    :\L  :control)
  (:ie-refresh		    :refresh)
  (:ie-scroll-forward	    :\V  :control)
  (:ie-scroll-backward	    :\V  :meta)
  (:ie-scroll-forward	    :scroll)
  (:ie-scroll-backward	    :scroll :meta))

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
  com-ie-ctrl-g		       :ie-abort
  com-ie-universal-argument    :ie-universal-argument
  com-ie-forward-character     :ie-forward-character
  com-ie-forward-word	       :ie-forward-word
  com-ie-backward-character    :ie-backward-character
  com-ie-backward-word	       :ie-backward-word
  com-ie-beginning-of-buffer   :ie-beginning-of-buffer
  com-ie-end-of-buffer	       :ie-end-of-buffer
  com-ie-beginning-of-line     :ie-beginning-of-line
  com-ie-end-of-line	       :ie-end-of-line
  com-ie-next-line	       :ie-next-line
  com-ie-previous-line	       :ie-previous-line
  com-ie-delete-character      :ie-delete-character
  com-ie-delete-word	       :ie-delete-word
  com-ie-rubout		       :ie-rubout-character
  com-ie-rubout-word	       :ie-rubout-word
  com-ie-clear-input	       :ie-clear-input
  com-ie-kill-line	       :ie-kill-line
  com-ie-make-room	       :ie-make-room
  com-ie-transpose-characters  :ie-transpose-characters
  com-ie-show-arglist	       :ie-show-arglist
  com-ie-show-value	       :ie-show-value
  com-ie-kill-ring-yank	       :ie-kill-ring-yank
  com-ie-history-yank	       :ie-history-yank
  com-ie-yank-next	       :ie-yank-next
  com-ie-refresh	       :ie-refresh
  com-ie-scroll-forward	       :ie-scroll-forward
  com-ie-scroll-backward       :ie-scroll-backward)
