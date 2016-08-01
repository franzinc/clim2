;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.	 All rights reserved."

(defun accept (type &rest accept-args
	       &key (stream *standard-input*)
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
               (insert-default nil) (replace-input t)
               (present-p nil) (active-p t))
  (declare (dynamic-extent accept-args))
  (declare (values object type))
  (declare (ignore prompt-mode display-default
		   activation-gestures additional-activation-gestures
		   delimiter-gestures additional-delimiter-gestures 
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
	  (let ((element (yank-from-history
                          history
                          :test #'(lambda (element)
                                    (presentation-subtypep
                                     (presentation-history-element-type element)
                                     type)))))
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
  (setq view (decode-indirect-view type view (frame-manager stream)
				   :query-identifier query-identifier))

  ;; Call STREAM-ACCEPT to do the work.	 It would be nice if we could
  ;; call PROMPT-FOR-ACCEPT to generate the real query-id here, but we
  ;; can't because we want to be able to decide exactly how it is called
  ;; on a case-by-case basis.  For example, within ACCEPTING-VALUES...
  (with-keywords-removed (accept-args accept-args '(:stream :view))
    (apply #'stream-accept (encapsulating-stream stream) type
           :view view :query-identifier query-identifier
           accept-args)))

(defmethod stream-accept ((stream input-protocol-mixin) type &rest accept-args
			  &key view &allow-other-keys)
  (declare (dynamic-extent accept-args))
  (let* ((stream (encapsulating-stream stream))
	 (query-identifier (apply #'prompt-for-accept
				  stream type view accept-args)))
    (apply #'accept-1 stream type
	   :query-identifier query-identifier
	   accept-args)))

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
		      (activation-gestures nil activation-gestures-p)
		      (additional-activation-gestures nil)
		      (delimiter-gestures nil delimiter-gestures-p)
		      (additional-delimiter-gestures nil)
		      (active-p t)
		 &allow-other-keys)

  ;; Set up the input editing environment
  (let ((the-object nil)
	(the-type nil)
	(the-options nil)
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
				present-p query-identifier
				:prompt prompt :active-p active-p)))

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
	      (with-activation-gestures ((if activation-gestures-p
					     activation-gestures
					   (or additional-activation-gestures
					       *standard-activation-gestures*))
					 :override activation-gestures-p)
		(with-delimiter-gestures ((if delimiter-gestures-p
					      delimiter-gestures
					    additional-delimiter-gestures)
					  :override delimiter-gestures-p)
		  (handler-bind
		      ((parse-error
			 #'(lambda (anerror)
			     (declare (ignore anerror))
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
		       the-options options
		       activated nil)
		 (when (if replace-supplied-p
			   replace-input
			   (getf the-options :echo t))
		   (presentation-replace-input stream the-object the-type view
					       :buffer-start start-position
					       :query-identifier query-identifier)
		   		  ;; spr25912 --PnC
		  ;; Windows seems to handle this slightly differently.
		  ;; As a result, when processing a command, if the user
		  ;; does a command-completion (from a menu), the focus
		  ;; doesn't come back to the window.  Furthermore, 
		  ;; trying to read the gesture goes into an infinite loop,
		  ;; because there's nothing there to read.		
		  #+mswindows
		  (when (eql (if (listp the-type)
				 (first the-type)
			       the-type) 
			     'clim:command-name)
		    (clim:stream-set-input-focus (encapsulating-stream-stream stream))
		    (stream-unread-gesture stream #\space))
		  )))))))

    ;; The input has been parsed, moused, or defaulted.
    ;; If we are still inside a WITH-INPUT-EDITING at an outer level, leave the
    ;; delimiter in the stream.	 But if this was the top level of input, eat
    ;; the activation gesture instead of leaving it in the stream. Don't eat
    ;; the activation gesture on streams that can't ever support input editing,
    ;; such as string streams.
    ;;--- This is really lousy.	 We need a coherent theory here.
    (when activated
      (when (and (not (input-editing-stream-p stream))
		 (stream-supports-input-editing stream))
	(let ((gesture (read-gesture :stream stream :timeout 0)))
	  ;;--- For now, just ignore button release events
	  (when (typep gesture 'pointer-button-release-event)
	    (read-gesture :stream stream :timeout 0)))))
    (when (and history
	       (frame-maintain-presentation-histories *application-frame*)
	       (getf the-options :maintain-history t))
      ;;--- Should this only record stuff that was input via the keyboard?
      (push-history-element history (make-presentation-history-element
				      :object the-object :type (or the-type type))))
    #+compulsive-type-checking
    (when (and the-type (not (eq the-type type)))
      (unless (presentation-subtypep-1 the-type type)
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
(defun accept-from-string (type string 
			   &key (view +textual-view+)
				(default nil default-supplied-p)
				(default-type type)
				(activation-gestures nil activation-gestures-p)
				(additional-activation-gestures nil)
				(delimiter-gestures nil delimiter-gestures-p)
				(additional-delimiter-gestures nil)
				(start 0) (end nil))
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
	  (with-activation-gestures ((if activation-gestures-p
					 activation-gestures
				       (or additional-activation-gestures
					   *standard-activation-gestures*))
				     :override activation-gestures-p)
	    (with-delimiter-gestures ((if delimiter-gestures-p
					  delimiter-gestures
					additional-delimiter-gestures)
				      :override delimiter-gestures-p)
	      (handler-bind 
		  ((parse-error
		     #'(lambda (anerror)
			 (declare (ignore anerror))
			 ;; This private version of CHECK-FOR-DEFAULT is
			 ;; enough for string and string streams to do a
			 ;; reasonable job, but it's not perfect.  Some
			 ;; hairy presentation types may still not work.
			 (flet ((check-for-default (stream)
				  (loop
				    (let ((char (read-char stream nil *end-of-file-marker*)))
				      (when (or (not (characterp char))
						(delimiter-gesture-p char)
						(not (whitespace-char-p char)))
					(unless (eq char *end-of-file-marker*)
					  (unread-char char stream))
					(when (and default-supplied-p
						   (or (eq char *end-of-file-marker*)
						       (activation-gesture-p char)
						       (delimiter-gesture-p char)))
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
							   type stream view))))))
      #+aclpc ;; for some reason paul want to do this twice (error? :)
	(with-input-from-string (stream string :start start :end end :index index)
	  (with-activation-gestures ((if activation-gestures-p
					 activation-gestures
				       (or additional-activation-gestures
					   *standard-activation-gestures*))
				     :override activation-gestures-p)
	    (with-delimiter-gestures ((if delimiter-gestures-p
					  delimiter-gestures
					additional-delimiter-gestures)
				      :override delimiter-gestures-p)
	      (handler-bind 
		  ((parse-error
		     #'(lambda (anerror)
			 (declare (ignore anerror))
			 ;; This private version of CHECK-FOR-DEFAULT is
			 ;; enough for string and string streams to do a
			 ;; reasonable job, but it's not perfect.  Some
			 ;; hairy presentation types may still not work.
			 (flet ((check-for-default (stream)
				  (loop
				    (let ((char (read-char stream nil *end-of-file-marker*)))
				      (when (or (not (characterp char))
						(delimiter-gesture-p char)
						(not (whitespace-char-p char)))
					(unless (eq char *end-of-file-marker*)
					  (unread-char char stream))
					(when (and default-supplied-p
						   (or (eq char *end-of-file-marker*)
						       (activation-gesture-p char)
						       (delimiter-gesture-p char)))
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
		      type stream view))))))
      (values the-object (or the-type type) index))))

;; Make ACCEPT work inside WITH-INPUT-FROM-STRING
;; by defining methods that partially implement the extended stream input
;; protocol on input-from-string streams 
(defmethod stream-read-gesture ((stream t)
				&key timeout peek-p
				     input-wait-test input-wait-handler
				     pointer-button-press-handler)
  (declare (ignore input-wait-test input-wait-handler pointer-button-press-handler))
  ;; avoid using STREAM-x functions to reduce Gray stream dependence,
  ;; tfb 13-jun-2000
  (let ((char (if (eq timeout 0)
		  (read-char-no-hang stream nil *end-of-file-marker*)
		(read-char stream nil *end-of-file-marker*))))
    (when (and char peek-p 
	       ;; spr26071 -pnc
	       ;; As noted above, this used to call stream-unread-char,
	       ;; which could handle the case where char was :eof.
	       ;; unread-char breaks, so mimic what it used to do.
	       (not (eq char *end-of-file-marker*)))
      (unread-char char stream))
    char))

(defmethod stream-unread-gesture ((stream t) gesture)
  (unless (eq gesture *end-of-file-marker*)
    (check-type gesture character)
    ;; avoid using STREAM-x functions to reduce Gray stream dependence,
    ;; tfb 13-jun-2000
    (unread-char gesture stream)))

(defmethod stream-accept ((stream t) type &rest accept-args
			  &key view &allow-other-keys)
  (declare (dynamic-extent accept-args))
  (let ((query-identifier
	  (apply #'prompt-for-accept
		 (encapsulating-stream stream) type view accept-args)))
    (apply #'accept-1 (encapsulating-stream stream) type
		      :query-identifier query-identifier
		      accept-args)))

(defmethod prompt-for-accept ((stream t) type (view view) 
			      &key query-identifier &allow-other-keys)
  (declare (ignore type))
  query-identifier)
