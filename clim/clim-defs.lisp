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

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

(defmacro default-output-stream (stream &optional must-be-variable-macro-name)
  `(cond ((member ,stream '(t nil))
	  (setq ,stream '*standard-output*))
	 ,@(when must-be-variable-macro-name
	     `(((not (and (symbolp ,stream)
			  (not (keywordp ,stream))))
		(warn "The stream argument to ~S, ~S, is invalid.~@
		       This argument must be a variable that can be bound to a new stream."
		      ',must-be-variable-macro-name ,stream)
		(setq ,stream '*standard-output*))))))

(defmacro default-query-stream (stream &optional must-be-variable-macro-name)
  `(cond ((member ,stream '(t nil))
	  (setq ,stream '*query-io*))
	 ,@(when must-be-variable-macro-name
	     `(((not (and (symbolp ,stream)
			  (not (keywordp ,stream))))
		(warn "The stream argument to ~S, ~S, is invalid.~@
		       This argument must be a variable that can be bound to a new stream."
		      ',must-be-variable-macro-name ,stream)
		(setq ,stream '*query-io*))))))



(defun-inline position-translate (point dx dy)
  (values (+ (point-x point) dx)
	  (+ (point-y point) dy)))

(defun-inline position-translate* (x y dx dy)
  (values (+ x dx)
	  (+ y dy)))

(defmacro translate-positions (x-delta y-delta &rest points)
  `(progn
     ,@(let ((forms nil))
	 (dorest (pts points cddr)
	   (push `(incf ,(first pts) ,x-delta) forms)
	   (push `(incf ,(second pts) ,y-delta) forms))
	 (nreverse forms))))

(defun translate-point-sequence (x-delta y-delta points)
  (let ((new-points (make-list (length points)))
	(i 0))
    (dorest (stuff points cddr)
      (let ((x (first stuff))
	    (y (second stuff)))
	(translate-positions x-delta y-delta x y)
	(setf (elt new-points i) x)
	(setf (elt new-points (1+ i)) y))
      (incf i 2))
    new-points))

(defclass io-buffer
	  ()
     ((size :accessor io-buffer-size :initarg :size)
      (buffer :accessor io-buffer-buffer)
      (input-pointer :accessor io-buffer-input-pointer)
      (output-pointer :accessor io-buffer-output-pointer)))

(defvar +string-array-element-type+ 'cltl1::string-char)

(defmacro define-accessors (flavor &body instance-variables)
  (let ((forms nil))
    (dolist (iv instance-variables)
      (let ((name (fintern "~A-~A" flavor iv)))
	(push `(defmethod ,name ((,flavor ,flavor))
		 (slot-value ,flavor ',iv))
	      forms)
	(push `(defmethod (setf ,name) (new-val (,flavor ,flavor))
		 (setf (slot-value ,flavor ',iv) new-val))
	      forms)))
    `(define-group ,flavor define-accessors 
       ,@(nreverse forms))))

;;; Implementation defining and managing tools

(defvar *implementations* nil)

(defun define-implementation (name creation-function)
  (pushnew name *implementations*)
  (setf (get name 'creation-function) creation-function))

(defun window-type-creation-function (window-stream-type)
  (let ((cf (get window-stream-type 'creation-function)))
    (unless cf
      (error "No window creation function is defined for type ~S" window-stream-type))
    cf))

;;; Utilities



;;; Action definitions

;;; Kludge for now, support multiple pointers per console and window-stream later
;;; Removed 23 January 1988 by rsl -- window-streams have pointers of their own now.
;;; (defvar *the-pointer* (make-instance 'pointer))

(defstruct (action (:copier nil))	 ; a "blip" -- COPIER NIL so Genera doesn't barf.
  )

(defstruct (pointer-button-press-action (:include action))
  button
  x
  y
  pointer
  window
  shift-mask)

(defstruct (window-action (:include action))
  window)

(defstruct (window-size-or-position-change-action (:include window-action))
  left
  top
  right
  bottom)

;;;  There really should be a file full of these macro definitions someplace.
(defmacro with-output-recording-options ((stream &key
						 (draw-p nil draw-p-supplied)
						 (record-p nil record-p-supplied))
					 &body body)
  (let ((new-stream (gensymbol "STREAM")))
    `(let ((,new-stream ,stream))
       (flet ((with-output-recording-options () ,@body))
	 (declare (dynamic-extent #'with-output-recording-options))
	 (with-output-recording-options-internal
	   ,new-stream
	   ,(if draw-p-supplied draw-p `(stream-draw-p ,new-stream))
	   ,(if record-p-supplied record-p `(stream-record-p ,new-stream))
	   #'with-output-recording-options)))))

(eval-when (compile eval load)
(defvar *output-record-constructor-cache* (make-hash-table))

(defmacro construct-output-record (type &rest init-args #+Genera &environment #+Genera env)
  (let ((constructor nil))
    (cond ((and (constantp type #+Genera env)
		(setq constructor (gethash (eval type) *output-record-constructor-cache*)))
	   `(,constructor ,@init-args))
	  (t `(construct-output-record-1 ,type ,@init-args)))))

(defmacro define-output-record-constructor (record-type arglist &rest initialization-arguments)
  (let ((constructor-name (fintern "~S-CONSTRUCTOR" record-type)))
    `(progn
       (define-constructor ,constructor-name ,record-type ,arglist ,@initialization-arguments)
       (setf (gethash ',record-type *output-record-constructor-cache*)
	     ',constructor-name))))
)

(defmacro with-new-output-record ((stream &optional record-type record &rest init-args)
				  &body body #+Genera &environment #+Genera env)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (unless record-type
    (setq record-type `'standard-sequence-output-record))
  (let ((constructor nil)
	(ignore-record nil))
    (when (and (constantp record-type #+Genera env)
	       (setq constructor
		     (gethash (eval record-type) *output-record-constructor-cache*))))
    (unless record
      (setq record '#:record
	    ignore-record t))
    `(flet ((with-new-output-record-body (,record)
	      ,@(when ignore-record `((declare (ignore ,record))))
	      ,@body))
       (declare (dynamic-extent #'with-new-output-record-body))
       (with-new-output-record-internal
	 #'with-new-output-record-body
	 ,stream ,record-type ',constructor ,@init-args))))


(defmacro with-output-to-output-record ((stream &optional record-type record &rest init-args)
					&body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream)
  ;; --- validate protocol here.
  `(with-output-recording-options (,stream :draw-p nil :record-p t)
     (letf-globally (((output-recording-stream-output-record ,stream) nil)
		     ((output-recording-stream-text-output-record ,stream) nil))
       (with-new-output-record (,stream ,record-type ,record ,@init-args)
	 (with-stream-cursor-position-saved (,stream)
	   ,@body)))))


;;; Ditto.  From window-stream
(defmacro with-input-focus ((stream) &body body)
  (let ((old-input-focus (gensymbol 'old-input-focus)))
    `(let ((,old-input-focus nil))
       (unwind-protect
	   (progn (setq ,old-input-focus (stream-set-input-focus ,stream))
		  ,@body)
	 (when ,old-input-focus
	   (stream-restore-input-focus ,stream ,old-input-focus))))))

;;; Ditto.  From accepting-values 
(defmacro accepting-values ((&optional stream &rest args) &body body)
  #+Genera (declare (arglist (&optional stream &key application-class own-window
					initially-select-query-identifier) &body body))
  (default-output-stream stream)
  `(flet ((accepting-values (,stream) ,@body))
     (declare (dynamic-extent #'accepting-values))
     (accept-values-1 ,stream #'accepting-values
		      ,@args)))

(defmacro with-clipping-region ((stream region) &body body)
  (default-output-stream stream)
  `(with-clipping-region-1 ,stream ,region (named-continuation with-clipping-region
							       (,stream) ,@body)))


;;; Trivial Ink support



;;; Presentation type variables
(defvar *input-context* nil)

(defun-inline input-context-type (context-entry)
  (first context-entry))

(defun-inline input-context-tag (context-entry)
	      (second context-entry))

;;; This is the presentation you get if you click while not over anything...
(defvar *null-presentation*)

;;; Application variables.
(defvar *application*)
(defvar *default-application*)

(defvar *whitespace* (coerce '(#\Space #\Tab) 'string))

(defun whitespace-character-p (character)
  (find character *whitespace* :test #'char-equal))

(defun word-break-character-p (thing)
  (and (characterp thing)
       (not (alpha-char-p thing))))

(defun newline-p (thing)
  (and (characterp thing)
       (char-equal thing #\Newline)))

(defun writable-character-p (character)
  ;; do we need this extra level of safety?
  (and (characterp character)
       (or (graphic-char-p character)
	   (not (null (find character '#(#\Newline #\Return #\Tab) :test #'char-equal))))))


#+(and pcl genera)
(progn

(walker:define-walker-template scl:letf walker::walk-let)
(walker:define-walker-template scl:letf* walker::walk-let*)
)

(defmacro with-output-as-presentation ((&key object type stream modifier
					     single-box (allow-sensitive-inferiors t)
					     (record-type `'standard-presentation)
					     parent)
				       &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream)
  ;;--- keep a separate coordinate-sorted set of just presentations (for
  ;;faster sensitivity searching)?
  ;; Maybe with-new-output-record should turn record-p on?
  `(with-output-recording-options (,stream :record-p t)
     (with-new-output-record (,stream ,record-type nil
			      :object ,object
			      :type (expand-presentation-type-abbreviation ,type)
			      :single-box ,single-box
			      :allow-sensitive-inferiors ,allow-sensitive-inferiors
			      ,@(when modifier `(:modifier ,modifier))
			      ,@(when parent `(:parent ,parent)))
       ,@body)))



(defun true (&rest ignore) (declare (ignore ignore)) t)


(defmacro with-room-for-graphics ((&optional stream &key record-type height (move-cursor t))
				  &body body)
  (default-output-stream stream with-room-for-graphics)
  (unless record-type
    (setq record-type `'standard-sequence-output-record))
  `(flet ((with-room-for-graphics-body (,stream) ,@body))
     (declare (dynamic-extent #'with-room-for-graphics-body))
     (with-room-for-graphics-1 ,stream ,record-type ,move-cursor
			       #'with-room-for-graphics-body
			       ,@(and height `(:height ,height)))))


(defmacro with-first-quadrant-coordinates ((&optional stream) &body body)
  (default-output-stream stream with-first-quadrant-coordinates)
  (let (( x '#:x)  ( y '#:y)
	(tx '#:tx) (ty '#:ty))
    `(multiple-value-bind (,x ,y)
	 (stream-cursor-position* ,stream)
       (multiple-value-bind (,tx ,ty)
	   (transform-point* (medium-transformation ,stream) 0 0)
	 (with-drawing-options
	     ;; Don't flip the stream over if we already have
	     (,stream :transformation (if (medium-+y-upward-p ,stream)
					  +identity-transformation+
					  (make-transformation 1 0 0 -1
							       (- ,x ,tx) (- ,y ,ty))))
	   (letf-globally (((medium-+y-upward-p ,stream) t))
			  ,@body))))))


(defmacro catch-abort-gestures ((format-string &rest format-args) &body body)
  `(with-simple-restart (abort ,format-string ,@format-args)
     (handler-bind ((abort-gesture #'handle-abort-gesture))
       ,@body)))

(defun handle-abort-gesture (&rest x)
  (print (list :handle-abort-gesture x)))

(defvar  *command-unparser* nil)

(defvar *command-parser* nil)
(defvar *partial-command-parser* nil)

(define-condition accelerator-gesture (error)
  ((event :initform nil :initarg :event :reader accelerator-gesture-event))
  (:report
    (lambda (condition stream)
      (format stream "Accelerator event ~S seen" (accelerator-gesture-event condition)))))

(define-condition abort-gesture (error)
  ((event :initform nil :initarg :event :reader abort-gesture-event))
  (:report
    (lambda (condition stream)
      (format stream "Abort event ~S seen" (abort-gesture-event condition)))))


(defmacro with-stream-cursor-position-saved ((stream) &body body)
  (let ((x '#:x)
	(y '#:y))
    `(multiple-value-bind (,x ,y) (stream-cursor-position* ,stream)
       (unwind-protect
	   (progn ,@body)
	 (stream-set-cursor-position* ,stream ,x ,y)))))

(defmacro translate-fixnum-positions (x-delta y-delta &body points)
  (once-only (x-delta y-delta)
    `(progn
       ,@(let ((forms nil))
	   (dorest (pts points cddr)
	     (push `(setf ,(first pts)  (the fixnum (+ ,(first pts)  ,x-delta))) forms)
	     (push `(setf ,(second pts) (the fixnum (+ ,(second pts) ,y-delta))) forms))
	   (nreverse forms)))))
