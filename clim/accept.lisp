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

;; $fiHeader: accept.lisp,v 1.7 91/08/05 13:25:19 cer Exp $

(in-package :clim)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

(defun accept (type &rest accept-args
	       &key (stream *query-io*)
		    (view (stream-default-view stream))
		    (default nil default-supplied-p)
		    (default-type type)
		    (history type)
		    (provide-default nil)
		    (prompt t)
		    (prompt-mode ':normal)
		    (display-default prompt)
		    (query-identifier nil)
		    (activation-characters nil)
		    (additional-activation-characters nil)
		    (blip-characters nil)
		    (additional-blip-characters nil)
		    (present-p nil))
  (declare (dynamic-extent accept-args))
  (declare (values object type))
  (declare (ignore view prompt prompt-mode display-default query-identifier
		   activation-characters additional-activation-characters
		   blip-characters additional-blip-characters present-p))

  ;; Allow the arguments to be presentation type abbreviations
  (multiple-value-bind (expansion expanded)
      (expand-presentation-type-abbreviation type)
    (when expanded
      (when (eq default-type type)
	(setq default-type expansion))
      (when (eql history type)
	(setq history expansion))
      (setq type expansion)))
  (unless (eq default-type type)
    (multiple-value-bind (expansion expanded)
	(expand-presentation-type-abbreviation default-type)
      (when expanded
	(setq default-type expansion)
	(setq accept-args `(:default-type ,default-type ,@accept-args)))))
  (unless (eq history type)
    (multiple-value-bind (expansion expanded)
	(expand-presentation-type-abbreviation history)
      (when expanded
	(setq history expansion)
	(setq accept-args `(:history ,history ,@accept-args)))))

  (let ((insert-default nil))
    (when (and provide-default (null default-supplied-p))
      ;; If the user wants a default, but provided none, go get it from the history
      (let ((history (if (typep history 'basic-history)
			 history
		         (presentation-type-history history))))
	(when history
	  (let ((element (yank-from-history history)))
	    (when element
	      (setq default (presentation-history-element-object element)
		    default-supplied-p t
		    insert-default t))))))
    (when default-supplied-p
      ;; Massage the default
      (multiple-value-bind (new-default new-type)
	  (presentation-default-preprocessor default type :default-type default-type)
	(when (or (not (eq default new-default))
		  (not (eq default-type new-type)))
	  (setq default new-default
		default-type (or new-type default-type)
		insert-default t))))
    (when insert-default
      (setq accept-args `(:default ,default :default-type ,default-type ,@accept-args))))

  ;; Call methods to do the work
  (with-rem-keywords (accept-args accept-args '(:stream))
    (let ((query-identifier
	    (apply #'prompt-for-accept (or *original-stream* stream) type accept-args)))
      (apply #'accept-1 (or *original-stream* stream) type
			:query-identifier query-identifier accept-args))))

(defmethod accept-1 ((stream input-protocol-mixin) type &rest args)
  (declare (dynamic-extent args))
  (apply #'accept-2 (or *original-stream* stream) type args))

(defmethod prompt-for-accept ((stream input-protocol-mixin) type &rest args
			      &key query-identifier &allow-other-keys)
  (declare (dynamic-extent args))
  (when (output-stream-p stream)
    (apply #'prompt-for-accept-internal stream type args))
  query-identifier)

(defun prompt-for-accept-internal (stream type
				   &key (default nil default-supplied-p) (default-type type)
					(prompt t) (prompt-mode ':normal)
					(display-default prompt)
				   &allow-other-keys)
  (cond ((eq prompt t)
	 (write-string "Enter " stream)
	 (describe-presentation-type type stream 1))
	((not (eq prompt nil))
	 (write-string prompt stream)))
  (when (and display-default default-supplied-p)
    (when prompt (write-string " [" stream))
    (write-string "default " stream)
    (present default default-type :stream stream)
    (when prompt (write-string "]" stream)))
  (when (eq prompt-mode ':normal)
    (when (or prompt (and display-default default-supplied-p))
      (write-string ": " stream))))

(defun accept-2 (stream type
		 &key (view (stream-default-view stream))
		      (default nil default-supplied-p)
		      (default-type type)
		      ((:history history-type) type)
		      (present-p nil)
		      (query-identifier nil)
		      (activation-characters nil)
		      (additional-activation-characters nil)
		      (blip-characters nil)
		      (additional-blip-characters nil)
		 &allow-other-keys)

  ;; Set up the input editing environment
  (let ((the-object nil)
	(the-type nil)
	(activated t)
	(history nil))

    (cond ((typep history-type 'basic-history)
	   (setq history history-type
		 history-type type))
	  (history-type
	   (setq history (presentation-type-history history-type))))

    ;; In AVVs, ACCEPT can turn into PRESENT
    (when present-p
      (return-from accept-2
	(accept-present-default type stream view default default-supplied-p
				present-p query-identifier)))

    (block input-editing
      (flet ((input-sensitizer (continuation stream)
	       (declare (dynamic-extent continuation))
	       (if (stream-record-p stream)
		   (with-output-as-presentation (:type (or the-type type)
						 :stream stream
						 :object the-object)
		     (funcall continuation stream))
		   (funcall continuation stream))))
	(declare (dynamic-extent #'input-sensitizer))
	(with-input-editing (stream :input-sensitizer #'input-sensitizer)
	  (let ((start-position (input-position stream)))
	    (with-input-context (type)
				(object presentation-type nil options)
	      (with-activation-characters ((or activation-characters
					       additional-activation-characters
					       *standard-activation-characters*)
					   :override (not (null activation-characters)))
		(with-blip-characters ((or blip-characters
					   additional-blip-characters)
				       :override (not (null blip-characters)))
		  (handler-bind ((parse-error
				   #'(lambda (error)
				       (declare (ignore error))
				       (when (and default-supplied-p
						  (check-for-default stream start-position
								     default default-type
								     view))
					 (setq the-object default
					       the-type default-type)
					 (return-from input-editing))
				       ;; Decline to handle the parse error
				       nil)))
		    (flet ((accept-help (stream action string-so-far)
			     (declare (ignore action string-so-far))
			     (write-string "You are being asked to enter " stream)
			     (describe-presentation-type type stream)
			     (write-char #\. stream)))
		      (declare (dynamic-extent #'accept-help))
		      (with-accept-help
			  (((:top-level-help :establish-unless-overridden)
			    ;; :ESTABLISH-... here because we want (SEQUENCE PATHNAME)'s
			    ;; help, not both (SEQUENCE PATHNAME) and PATHNAME.
			    #'accept-help))
			;; Call the presentation type's ACCEPT method
			(multiple-value-setq (the-object the-type)
			  (let ((*presentation-type-for-yanking* (and history history-type)))
			    (if default-supplied-p
				(if history
				    (let ((default-element
					    (make-presentation-history-element
					      :object default :type default-type)))
				      (with-default-bound-in-history history default-element
					(call-presentation-generic-function accept
					  type stream view
					  :default default :default-type default-type)))
				    (call-presentation-generic-function accept
				      type stream view
				      :default default :default-type default-type))
			        (call-presentation-generic-function accept
				  type stream view)))))))))

	       ;; A presentation translator was invoked
	       (t 
		 (setq the-object object
		       the-type presentation-type
		       activated nil)
		 (when (getf options :echo t)
		   (presentation-replace-input stream object presentation-type view
					       :buffer-start start-position
					       :query-identifier query-identifier))))))))

    ;; The input has been parsed, moused, or defaulted.
    ;; If we are still inside a WITH-INPUT-EDITING at an outer level, leave the
    ;; delimiter in the stream.  But if this was the top level of input, eat
    ;; the activation character instead of leaving it in the stream.
    ;; Don't eat the activation character on streams that can't ever support
    ;; input editing, such as string streams.
    ;;--- This is really lousy.  We need a coherent theory here.
    (when activated
      (when (and (not (interactive-stream-p stream))
		 (stream-supports-input-editing stream))
	(let ((gesture (read-gesture :stream stream :timeout 0)))
	  ;;--- For now, just ignore button release events
	  (when (typep gesture 'silica::pointer-release-event)
	    (read-gesture :stream stream :timeout 0)))))
    (when (and history (frame-maintain-presentation-histories *application-frame*))
      ;;--- Should this only record stuff that was input via the keyboard?
      (push-history-element history (make-presentation-history-element
				      :object the-object :type (or the-type type))))
    #+compulsive-type-checking
    (when (and the-type (not (eq the-type type)))
      (unless (presentation-subtypep the-type type)
	;; Catch a common bug by verifying that the returned type is a subtype
	;; of the requested type
	(cerror "Return a second value of ~*~*~*~S"
		"The ~S method for the type ~S returned a second value of ~S, ~
		 which is not a subtype of ~S"
		'accept type the-type type)
	(setq the-type type)))
    ;; Ensure that there are no stale highlighting boxes lying around if
    ;; we are exiting via keyboard input
    (when (output-recording-stream-p stream)
      (unhighlight-highlighted-presentation stream t))
    (values the-object (or the-type type))))

;;; If the input from stream, starting at location, is a request to use the default
;;; then insert the default and return true
(defun check-for-default (stream location default default-type view)
  (setf (input-position stream) location)
  (loop
    (let ((char (read-gesture :stream stream)))
      (when (or (not (characterp char))
		(blip-character-p char)
		(not (whitespace-character-p char)))
	;; If we got some kind of mouse gesture or a blip character or
	;; a printing character, then put it back and check to see if
	;; we should insert the default.
	(unread-gesture char :stream stream)
	(setf (input-position stream) location)
	(when (or (null char)
		  (activation-character-p char)
		  (blip-character-p char))
	  ;; Insert the default if we got a character that will terminate
	  ;; the current call to ACCEPT
	  (unless (rescanning-p stream)
	    (presentation-replace-input stream default default-type view
					:buffer-start location))
	  (return-from check-for-default t))
	(return-from check-for-default nil)))))

;;; As in DW, this does not do defaulting, but does accept a :default argument
;;; in case the accept method needs it (e.g. for pathnames)
(defun accept-from-string (type string &key (view +textual-view+)
					    (default nil default-supplied-p)
					    (default-type type)
					    (start 0)
					    (end nil))
  (declare (values object type index))

  ;; Allow the arguments to be presentation type abbreviations
  (multiple-value-bind (expansion expanded)
      (expand-presentation-type-abbreviation type)
    (when expanded
      (when (eq default-type type)
	(setq default-type expansion))
      (setq type expansion)))
  (unless (eq default-type type)
    (multiple-value-bind (expansion expanded)
	(expand-presentation-type-abbreviation default-type)
      (when expanded
	(setq default-type expansion))))

  ;; Call the presentation type's accept method
  (let ((index start))
    (multiple-value-bind (the-object the-type)
	(with-input-from-string (stream string :start start :end end :index index)
	  (handler-bind ((simple-parse-error
			   #'(lambda (error)
			       ;; This private version of CHECK-FOR-DEFAULT is
			       ;; enough for string and string streams to do a
			       ;; reasonable job, but it's not perfect.  Some
			       ;; hairy presentation types may still not work.
			       (flet ((check-for-default (stream)
				        (loop
					  (let ((char (read-char stream nil :eof)))
					    (when (or (not (characterp char))
						      (blip-character-p char)
						      (not (whitespace-character-p char)))
					      (unread-char char stream)
					      (when (or (eql char :eof)
							(activation-character-p char)
							(blip-character-p char))
						(return-from check-for-default t))
					      (return-from check-for-default nil))))))
				 (declare (dynamic-extent #'check-for-default))
				 (when (check-for-default stream)
				   (return-from accept-from-string
				     (values default default-type index)))))))
	    (if default-supplied-p
		(call-presentation-generic-function accept
		  type stream view
		  :default default :default-type default-type)
	        (call-presentation-generic-function accept type stream view))))
      (values the-object (or the-type type) index))))

;; Make ACCEPT work inside WITH-INPUT-FROM-STRING
;; by defining methods that partially implement the extended stream input
;; protocol on input-from-string streams 
(defmethod stream-read-gesture ((stream t)
				&key timeout peek-p
				     input-wait-test input-wait-handler
				     pointer-button-press-handler)
  (declare (ignore input-wait-test input-wait-handler pointer-button-press-handler))
  (let ((char (if (eql timeout 0)
		  (stream-read-char-no-hang stream)
		  (stream-read-char stream))))
    (when (and char peek-p)
      (stream-unread-char stream char))
    char))

(defmethod stream-unread-gesture ((stream t) gesture)
  (unless (eq gesture ':eof)
    (check-type gesture character)
    (stream-unread-char stream gesture)))

(defmethod accept-1 ((stream t) type &rest args)
  (declare (dynamic-extent args))
  (apply #'accept-2 (or *original-stream* stream) type args))

(defmethod prompt-for-accept ((stream t) type &key query-identifier &allow-other-keys)
  (declare (ignore type))
  query-identifier)
