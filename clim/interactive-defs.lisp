;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-INTERNALS; Base: 10 -*-
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

;; --- Genera name alert! ---
(defvar *activation-characters* nil)
(defvar *blip-characters* nil)

;;; Until we have a real per-implementation key table, we don't
;;; know whether the implementation puts #\Newline or #\Return on the
;;; key marked "Return".
(defvar *standard-activation-characters* '(#+Genera #\End #\Newline #\Return))

;;; For communication through parsers to lower levels.
;;; Later, clever use of macrolet can replace this.
(defvar *input-wait-test* nil)
(defvar *input-wait-handler* nil)
(defvar *pointer-button-press-handler* nil)

(defmacro with-activation-characters ((additional-characters &key override) &body body)
  (when (characterp additional-characters)
    (setq additional-characters `'(,additional-characters)))
  `(let ((*activation-characters*
	   (cons ,additional-characters
		 ,(if override
		      `(and (not ,override) *activation-characters*)
		      '*activation-characters*))))
     ,@body))

(defmacro with-blip-characters ((additional-characters &key override) &body body)
  (when (characterp additional-characters)
    (setq additional-characters `'(,additional-characters)))
  `(let ((*blip-characters*
	   (cons ,additional-characters
		 ,(if override
		      `(and (not ,override) *blip-characters*)
		      '*blip-characters*))))
     ,@body))

(defun activation-character-p (character)
  (dolist (set *activation-characters*)
    (when (member character set)
      (return-from activation-character-p t))))

(defun blip-character-p (character)
  (dolist (set *blip-characters*)
    (when (member character set)
      (return-from blip-character-p t))))

;;; read-token reads characters until it encounters
;;; an activation character, a blip character, or something else
;;; (like a mouse click).
(defun read-token (stream &key input-wait-handler pointer-button-press-handler click-only)
  (let ((string (make-array 50
			    :element-type +string-array-element-type+
                            :fill-pointer 0
                            :adjustable t))
	(gesture nil))
    (flet ((return-token (&optional unread)
	     (when unread
	       (unread-gesture unread :stream stream))
	     (return-from read-token (values string))))
      (loop
	(setq gesture
	      (read-gesture :stream stream
			    :input-wait-handler (or input-wait-handler
						    *input-wait-handler*)
			    :pointer-button-press-handler (or pointer-button-press-handler
							      *pointer-button-press-handler*)
			    ))
	(cond ((and click-only
		    (not (typep gesture 'silica::pointer-press-event)))
	       (beep stream))
	      ((typep gesture
		       'silica::pointer-press-event)
	       ;; no need to funcall the pointer-button-press-handler
	       ;; as it will already have been called
	       ;; from the whopper on read-gesture, I think.
	       )
	      ((characterp gesture)
	       (cond ((activation-character-p gesture)
		      ;; who's responsible for turning #\End into
		      ;; (:activation #\End)??
		      (return-token gesture))
		     ((blip-character-p gesture)
		      ;; ditto?
		      (return-token gesture))
		     ((writable-character-p gesture)
		      (vector-push-extend gesture string)
		      ;; --- haven't updated write-char yet
		      #+Ignore
		      (write-char gesture stream))
		     (t (beep stream))))
	      (t (return-token gesture)))))))

(defun dummy-accept-parser (type stream &rest args)
  (declare (ignore type args)
	   (dynamic-extent args))
  ;; so mouse input still works
  (with-blip-characters (() :override t)
    (read-token stream :click-only t)))

(defun click-only-accept-parser (type stream &rest args)
  (declare (ignore type args)
	   (dynamic-extent args))
  (loop
    (read-token stream :click-only t)))

(defvar *kill-ring* nil)
(defvar *kill-ring-application* nil)
(defvar *presentation-type-for-yanking* nil)
(defvar *application-frame* nil)
(defvar *accelerator-characters* nil)
(defvar *accept-help* nil)
(defvar *pointer-documentation-output* nil)

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
		,@(cond ((eql option-type :override)
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
			((eql option-type :append)
			 )
			((eql option-type :establish-unless-overridden)
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

;; WITH-INPUT-EDITING simply encapsulates the stream and sets up an editing
;; context that allows rescanning, etc.
(defmacro with-input-editing ((&optional stream
			       &key (class `'input-editing-stream) input-sensitizer)
			      &body body)
  (default-query-stream stream with-input-editing)
  `(flet ((with-input-editing-body (,stream) ,@body))
     (declare (dynamic-extent #'with-input-editing-body))
     (with-input-editing-internal ,stream ,class
				  #'with-input-editing-body
				  ,input-sensitizer)))

(defmacro with-input-editor-typeout ((&optional stream) &body body)
  (default-query-stream stream with-input-editor-typeout)
  `(flet ((with-ie-typeout-body (,stream) ,@body))
     (declare (dynamic-extent #'with-ie-typeout-body))
     (with-ie-typeout-internal ,stream #'with-ie-typeout-body)))


(defun write-token (token stream &key acceptably)
  (cond ((and acceptably (some #'blip-character-p token))
	 (write-char *quotation-character* stream)
	 (write-string token stream)
	 (write-char *quotation-character* stream))
	(t
	 (write-string token stream))))
