;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: accept.lisp,v 1.9 92/07/24 10:54:17 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

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
		    (activation-gestures nil)
		    (additional-activation-gestures nil)
		    (delimiter-gestures nil)
		    (additional-delimiter-gestures nil)
		    #+CLIM-1-compatibility (activation-characters nil)
		    #+CLIM-1-compatibility (additional-activation-characters nil)
		    #+CLIM-1-compatibility (blip-characters nil)
		    #+CLIM-1-compatibility (additional-blip-characters nil)
		    (insert-default nil) (replace-input t)
		    (present-p nil) (active-p t))
  (declare (dynamic-extent accept-args))
  (declare (values object type))
  (declare (ignore prompt-mode display-default query-identifier
		   activation-gestures additional-activation-gestures
		   delimiter-gestures additional-delimiter-gestures 
		   #+CLIM-1-compatibility activation-characters
		   #+CLIM-1-compatibility additional-activation-characters
		   #+CLIM-1-compatibility blip-characters
		   #+CLIM-1-compatibility additional-blip-characters
		   insert-default replace-input present-p active-p))

  ;; Allow the arguments to be presentation type abbreviations
  (multiple-value-bind (expansion expanded)
      (expand-presentation-type-abbreviation type)
    (when expanded
      (when (eq default-type type)
	(setq default-type expansion))
      (when (eq history type)
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

  (typecase view
    (null)
    (symbol (setq view (make-instance view)))
    (cons   (setq view (apply #'make-instance view))))

  (setq view (decode-indirect-view view (frame-manager stream) type))
  
  ;; Call methods to do the work
  (with-keywords-removed (accept-args accept-args '(:stream :view))
    (let ((query-identifier
	    (apply #'prompt-for-accept
		   (or *original-stream* stream) type view accept-args)))
      (apply #'stream-accept (or *original-stream* stream) type
			     :view view :query-identifier query-identifier
			     accept-args))))

(defun decode-indirect-view (view framem type)
  (funcall-presentation-generic-function
   decode-indirect-view view framem type))

(define-presentation-method decode-indirect-view
    ((view view) (framem standard-frame-manager) (type t))
  view)
 


(defmethod stream-accept ((stream input-protocol-mixin) type &rest accept-args)
  (declare (dynamic-extent accept-args))
  (apply #'accept-1 (or *original-stream* stream) type accept-args))

(defmethod prompt-for-accept ((stream input-protocol-mixin) type (view view)
			      &rest accept-args
			      &key query-identifier &allow-other-keys)
  (declare (dynamic-extent accept-args))
  (when (output-stream-p stream)
    (apply #'prompt-for-accept-1 stream type accept-args))
  query-identifier)

(defun prompt-for-accept-1 (stream type
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

(defun accept-1 (stream type
		 &key (view (stream-default-view stream))
		      (default nil default-supplied-p)
		      (default-type type)
		      ((:history history-type) type)
		      (insert-default nil) (replace-input t replace-supplied-p)
		      (prompt t)
		      (present-p nil)
		      (query-identifier nil)
		      (activation-gestures nil)
		      (additional-activation-gestures nil)
		      (delimiter-gestures nil)
		      (additional-delimiter-gestures nil)
		      #+CLIM-1-compatibility (activation-characters nil)
		      #+CLIM-1-compatibility (additional-activation-characters nil)
		      #+CLIM-1-compatibility (blip-characters nil)
		      #+CLIM-1-compatibility (additional-blip-characters nil)
		 &allow-other-keys)

  #+CLIM-1-compatibility
  (when (or activation-characters additional-activation-characters
	    blip-characters additional-blip-characters)
    (setq activation-gestures activation-characters
	  additional-activation-gestures additional-activation-characters
	  delimiter-gestures blip-characters
	  additional-delimiter-gestures additional-blip-characters))

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

    ;; Inside ACCEPTING-VALUES, ACCEPT can turn into PRESENT
    (when present-p
      (return-from accept-1
	(accept-present-default type stream view default default-supplied-p
				present-p query-identifier :prompt prompt)))

    (block input-editing
      (flet ((input-sensitizer (continuation stream)
	       (declare (dynamic-extent continuation))
	       (if (stream-recording-p stream)
		   (with-output-as-presentation (stream the-object (or the-type type))
		     (funcall continuation stream))
		   (funcall continuation stream))))
	(declare (dynamic-extent #'input-sensitizer))
	(with-input-editing (stream :input-sensitizer #'input-sensitizer
				    :initial-contents (and insert-default
							   default-supplied-p
							   (list default default-type)))
	  (let ((start-position (stream-scan-pointer stream)))
	    (with-input-context (type)
				(object presentation-type nil options)
	      (with-activation-gestures ((or activation-gestures
					     additional-activation-gestures
					     *standard-activation-gestures*)
					 :override (not (null activation-gestures)))
		(with-delimiter-gestures ((or delimiter-gestures
					      additional-delimiter-gestures)
					  :override (not (null delimiter-gestures)))
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
					(funcall-presentation-generic-function accept
					  type stream view
					  :default default :default-type default-type)))
				    (funcall-presentation-generic-function accept
				      type stream view
				      :default default :default-type default-type))
			        (funcall-presentation-generic-function accept
				  type stream view)))))))))

	       ;; A presentation translator was invoked
	       (t 
		 (setq the-object object
		       the-type presentation-type
		       activated nil)
		 (when (if replace-supplied-p
			   replace-input
			   (getf options :echo t))
		   (presentation-replace-input stream object presentation-type view
					       :buffer-start start-position
					       :query-identifier query-identifier))))))))

    ;; The input has been parsed, moused, or defaulted.
    ;; If we are still inside a WITH-INPUT-EDITING at an outer level, leave the
    ;; delimiter in the stream.  But if this was the top level of input, eat
    ;; the activation gesture instead of leaving it in the stream. Don't eat
    ;; the activation gesture on streams that can't ever support input editing,
    ;; such as string streams.
    ;;--- This is really lousy.  We need a coherent theory here.
    (when activated
      (when (and (not (input-editing-stream-p stream))
		 (stream-supports-input-editing stream))
	(let ((gesture (read-gesture :stream stream :timeout 0)))
	  ;;--- For now, just ignore button release events
	  (when (typep gesture 'pointer-button-release-event)
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
  (setf (stream-scan-pointer stream) location)
  (loop
    (let ((char (read-gesture :stream stream)))
      (when (or (not (characterp char))
		(delimiter-gesture-p char)
		(not (whitespace-char-p char)))
	;; If we got some kind of mouse gesture or a delimiter or a
	;; printing character, then put it back and check to see if
	;; we should insert the default.
	(unread-gesture char :stream stream)
	(setf (stream-scan-pointer stream) location)
	(when (or (null char)
		  (activation-gesture-p char)
		  (delimiter-gesture-p char))
	  ;; Insert the default if we got a character that will terminate
	  ;; the current call to ACCEPT
	  (unless (stream-rescanning-p stream)
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
	  (handler-bind ((parse-error
			   #'(lambda (error)
			       ;; This private version of CHECK-FOR-DEFAULT is
			       ;; enough for string and string streams to do a
			       ;; reasonable job, but it's not perfect.  Some
			       ;; hairy presentation types may still not work.
			       (flet ((check-for-default (stream)
				        (loop
					  (let ((char (read-char stream nil :eof)))
					    (when (or (not (characterp char))
						      (delimiter-gesture-p char)
						      (not (whitespace-char-p char)))
					      (unless (eql char :eof)
						(unread-char char stream))
					      (when (or (eq char :eof)
							(activation-gesture-p char)
							(delimiter-gesture-p char))
						(return-from check-for-default t))
					      (return-from check-for-default nil))))))
				 (declare (dynamic-extent #'check-for-default))
				 (when (check-for-default stream)
				   (return-from accept-from-string
				     (values default default-type index)))))))
	    (if default-supplied-p
		(funcall-presentation-generic-function accept
		  type stream view
		  :default default :default-type default-type)
	        (funcall-presentation-generic-function accept
		  type stream view))))
      (values the-object (or the-type type) index))))

;; Make ACCEPT work inside WITH-INPUT-FROM-STRING
;; by defining methods that partially implement the extended stream input
;; protocol on input-from-string streams 
(defmethod stream-read-gesture ((stream t)
				&key timeout peek-p
				     input-wait-test input-wait-handler
				     pointer-button-press-handler)
  (declare (ignore input-wait-test input-wait-handler pointer-button-press-handler))
  (let ((char (if (eq timeout 0)
		  (stream-read-char-no-hang stream)
		  (stream-read-char stream))))
    (when (and char peek-p)
      (stream-unread-char stream char))
    char))

(defmethod stream-unread-gesture ((stream t) gesture)
  (unless (eq gesture ':eof)
    (check-type gesture character)
    (stream-unread-char stream gesture)))

(defmethod stream-accept ((stream t) type &rest accept-args)
  (declare (dynamic-extent accept-args))
  (apply #'accept-1 (or *original-stream* stream) type accept-args))

(defmethod prompt-for-accept ((stream t) type (view view) 
			      &key query-identifier &allow-other-keys)
  (declare (ignore type))
  query-identifier)
