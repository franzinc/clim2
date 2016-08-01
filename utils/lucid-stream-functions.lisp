;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-


(in-package :clim-utils)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; All of this is taken from the STREAM-DEFINITION-BY-USER proposal to
;;; the X3J13 committee, made by David Gray of TI on 22 March 1989.  No
;;; Lisp implementation yet supports this proposal, so we implement it
;;; here in this separate package.  This way we will be ready when some
;;; Lisp implementation adopts it (or something like it).


;;; just for development
#+ignore
(eval-when (compile load eval)
  (defparameter sym-list '("PEEK-CHAR" "READ-BYTE" "READ-CHAR" "UNREAD-CHAR"
			   "READ-CHAR-NO-HANG" "LISTEN" "READ-LINE" 
			   "CLEAR-INPUT" "WRITE-BYTE" "WRITE-CHAR"
			   "WRITE-STRING" "TERPRI" "FRESH-LINE" "FORCE-OUTPUT"
			   "FINISH-OUTPUT" "CLEAR-OUTPUT" ))
  (dolist (sym sym-list)
    (unintern (find-symbol sym :clim-lisp) :clim-lisp)
    (unintern (find-symbol sym :clim) :clim)))

;;; Output functions

(defmacro write-forwarding-lucid-output-stream-function (name args)
  (let* ((cl-name (find-symbol (symbol-name name) (find-package 'lisp)))
	 (method-name (intern (lisp:format nil "~A-~A" 'stream (symbol-name name))))
	 (optional-args (or (member '&optional args) (member '&key args)))
	 (required-args (ldiff args optional-args))
	 (optional-parameters (mapcan #'(lambda (arg)
					  (cond ((member arg lambda-list-keywords) nil)
						((atom arg) (list arg))
						(t (list (car arg)))))
				      optional-args))
	 (pass-args (append required-args optional-parameters))
	 ;; optional-args are &optional in the method,
	 ;; even if &key in the Common Lisp function
	 (method-args (if (eq (first optional-args) '&key)
			  (append required-args '(&optional) (cdr optional-args))
			  args))
	 (pass-keys (if (eq (first optional-args) '&key)
			(mapcan #'(lambda (arg)
				    (unless (atom arg)
				      (setq arg (car arg)))
				    (list (intern (string arg) :keyword) arg))
				(cdr optional-args))
			optional-parameters))
	 )
    (when (eq (first optional-args) '&optional)
      (pop optional-args))
    `(let ((orig-lucid-closure (or (getf (symbol-plist ',name) :original-lucid-closure)
				(setf (getf (symbol-plist ',name) :original-lucid-closure)
				      (symbol-function ',name)))))
       
       ;;(proclaim '(inline ,name))
       (defun ,name (,@required-args &optional stream ,@optional-args)
	 (cond ((null stream) (setq stream *standard-output*))
	       ((eq stream t) (setq stream *terminal-io*)))
	 (if (and (system:standard-object-p stream)
		  (typep stream 'fundamental-stream))
	     (,method-name stream ,@pass-args)
	     (funcall orig-lucid-closure ,@required-args stream ,@pass-keys)))

       ;; Define a default method for the generic function that calls back to the
       ;; system stream implementation.  Call back via a message if there is one,
       ;; otherwise via the Common Lisp function.
       ;; Uses T as a parameter specializer name as a standin for cl:stream,
       ;; which Genera doesn't support as a builtin class
       (defmethod ,method-name ((stream t) ,@method-args)
	 (,cl-name ,@required-args stream ,@pass-keys))
       ;;(import ',name :clim-lisp)
       ;;(export ',name :clim-lisp)
       )))

(write-forwarding-lucid-output-stream-function lisp:write-byte (integer))

(write-forwarding-lucid-output-stream-function lisp:write-char (character))

(write-forwarding-lucid-output-stream-function lisp:write-string (string &key (start 0) end))

(write-forwarding-lucid-output-stream-function lisp:terpri ())

(write-forwarding-lucid-output-stream-function lisp:fresh-line ())

(write-forwarding-lucid-output-stream-function lisp:force-output ())

(write-forwarding-lucid-output-stream-function lisp:finish-output ())

(write-forwarding-lucid-output-stream-function lisp:clear-output ())


;;; Input functions

(defmacro write-forwarding-lucid-input-stream-function (name lambda-list
							     &key eof 
							     additional-arguments)
  (let* ((cl-name (find-symbol (symbol-name name) (find-package 'lisp)))
	 (method-name (intern (lisp:format nil "~A-~A" 'stream (symbol-name name))))
	 (method-lambda-list (set-difference lambda-list '(stream peek-type)))
	 (args (mapcar #'(lambda (var) (if (atom var) var (first var)))
		       (remove-if #'(lambda (x) (member x lambda-list-keywords))
				  lambda-list)))
	 (method-calling-args (set-difference args '(stream peek-type)))
	 (cleanup `(cond ((null stream) (setq stream *standard-input*))
			 ((eq stream t) (setq stream *terminal-io*))))
	 (call-method `(,method-name stream ,@method-calling-args))
	 (calling-lambda-list (remove '&optional lambda-list)))
    (when (member (first (last method-lambda-list)) lambda-list-keywords)
      (setf method-lambda-list (butlast method-lambda-list)))
    `(let ((orig-lucid-closure 
	     (or (getf (symbol-plist ',name) :original-lucid-closure)
		 (setf (getf (symbol-plist ',name) :original-lucid-closure)
		       (symbol-function ',name)))))

	   ;;(proclaim '(inline ,name))
	   ,(if eof
		(let ((args `(eof-error-p eof-value ,@(and (not (eq eof :no-recursive))
							   '(recursive-p)))))
		  `(defun ,name (,@lambda-list ,@args)
		     ,cleanup
		     (if (and (system:standard-object-p stream)
			      (typep stream 'fundamental-stream))
			 (let ((result ,call-method))
			   (cond ((not (eq result *end-of-file-marker*))
				  result)
				 (eof-error-p
				  (signal-stream-eof stream ,@(and (not (eq eof :no-recursive))
								   '(recursive-p))))
				 (t
				  eof-value)))
			 (funcall orig-lucid-closure ,@calling-lambda-list ,@args))))
		`(defun ,name ,lambda-list
		   ,cleanup
		   (if (and (system:standard-object-p stream)
			    (typep stream 'fundamental-stream))
		       ,call-method
		       (funcall orig-lucid-closure ,@calling-lambda-list))))
	   ;; Define a default method for the generic function that calls back to the
	   ;; system stream implementation.  Call back via a message if there is one,
	   ;; otherwise via the Common Lisp function.
	   (defmethod ,method-name ((stream t) ,@method-lambda-list)
	     (,cl-name ,@additional-arguments ,@(remove 'peek-type args)
		       ,@(when eof `(nil *end-of-file-marker*))))
	   ;;(import ',name :clim-lisp)
	   ;;(export ',name :clim-lisp)
	   )))

(write-forwarding-lucid-input-stream-function lisp:peek-char (&optional peek-type stream)
					      :eof t 
					      :additional-arguments (nil))

(write-forwarding-lucid-input-stream-function lisp:read-byte (&optional stream)
					      :eof :no-recursive)

(write-forwarding-lucid-input-stream-function lisp:read-char (&optional stream) :eof t)

(write-forwarding-lucid-input-stream-function lisp:unread-char (character
								 &optional stream))

(write-forwarding-lucid-input-stream-function lisp:read-char-no-hang (&optional stream)
					      :eof t)

(write-forwarding-lucid-input-stream-function lisp:listen (&optional stream))

(write-forwarding-lucid-input-stream-function lisp:read-line (&optional stream) :eof t)

(write-forwarding-lucid-input-stream-function lisp:clear-input (&optional stream))


(defun signal-stream-eof (stream &optional recursive-p)
  (declare (ignore recursive-p))
  (error 'end-of-file :stream stream))


;;; Make CLIM-LISP:FORMAT do something useful on CLIM windows.

#||
(defun format (stream format-control &rest format-args)
  (when (null stream)
    (return-from format
      (apply #'lisp:format nil format-control format-args)))
  (when (eq stream 't)
    (setq stream *standard-output*))
  (cond ((streamp stream)
	 ;; this isn't going to quite work for ~&,
	 ;; but it's better than nothing.
	 (write-string (apply #'lisp:format nil format-control format-args) stream)
	 nil)
	(t
	 (apply #'lisp:format stream format-control format-args))))
||#



;;; Higher level lisp printing functions.


(eval-when (load)
  (let ((original-lucid-closure
	  (or (getf (symbol-plist 'lisp:format) :original-lucid-closure) 
	      (setf (getf (symbol-plist 'lisp:format) :original-lucid-closure)
		    (symbol-function 'lisp:format)))))
    (defun format (stream format-control &rest format-args)
      (when (eq stream 't)
	(setq stream *standard-output*))
      (cond ((null stream)
	     (apply original-lucid-closure nil format-control format-args))
	    ;; clim stream
	    ((and (system:standard-object-p stream)
		  (typep stream 'fundamental-stream))
	     (write-string (apply original-lucid-closure nil format-control format-args)
			   stream))
	    ;; Lucid stream
	    (t
	     (apply original-lucid-closure stream format-control format-args))))))

;;; Support for the IO functions with more varied argument templates and no
;;; Grey stream equivalent.  Assumes there is an argument called "STREAM".

(defmacro redefine-lucid-io-function (name lambda-list &body clim-body)
  (let ((args (mapcar #'(lambda (var) (if (atom var) var (first var)))
		      (remove-if #'(lambda (x) (member x lambda-list-keywords))
				 lambda-list))))
    `(let ((orig-lucid-closure 
	     (or (getf (symbol-plist ',name) :original-lucid-closure)
		 (setf (getf (symbol-plist ',name) :original-lucid-closure)
		       (symbol-function ',name)))))
	   (defun ,name ,lambda-list
	     (if (and (system:standard-object-p stream)
		      (typep stream 'fundamental-stream))
		 ,@clim-body
		 (funcall orig-lucid-closure ,@args))))))


(defmacro %string-stream (stream &body body)
  `(let (result
	 (new-stream (cond ((encapsulating-stream-p ,stream)
			    (encapsulating-stream-stream ,stream))
			   ((typep ,stream 'fundamental-stream)
			    ,stream)
			   (t
			    (let ((*standard-output* *terminal-io*))
			      (error "Unknown stream type, ~S" ,stream))))))
     (write-string
       ;; execute the body using the STREAM locally rebound
       ;; to an output stream object for I/O purposes:
       (let ((,stream (make-string-output-stream)))
	 ;; stream I/O stuff goes here
	 (setq result ,@body				)
	 ;; return the accumulated output string:
	 (get-output-stream-string ,stream))
       ;; use original output stream here ....
       new-stream)
     result))

  
(redefine-lucid-io-function lisp:streamp (stream) t)

(redefine-lucid-io-function lcl:underlying-stream (stream &optional direction
							  (recurse t)
							  exact-same)
			    (if (encapsulating-stream-p stream)
				(encapsulating-stream-stream stream)
				stream))

(redefine-lucid-io-function lisp:prin1 (object &optional (stream *standard-output*))
			      (%string-stream stream (lisp:prin1 object stream)))

(redefine-lucid-io-function lisp:print (object &optional (stream *standard-output*))
			      (%string-stream stream (lisp:print object stream)))

(redefine-lucid-io-function lisp:princ (object &optional (stream *standard-output*))
			      (%string-stream stream (lisp:princ object stream)))

(redefine-lucid-io-function lisp:pprint (object &optional (stream *standard-output*))
			      (%string-stream stream (lisp:pprint object stream)))

(redefine-lucid-io-function lisp:write-line (string &optional (stream *standard-output*)
						      &key (start 0) end)
			      (%string-stream stream (lisp:write-line string stream :start start :end end)))


;;; Easier to write this one out.
;;;
(let ((orig-lucid-closure (symbol-function 'lisp:write)))
  (defun lisp:write (object
		     &key
		     ((:stream stream) *standard-output*)
		     ((:escape escapep) *print-escape*)
		     ((:radix *print-radix*) *print-radix*)
		     ((:base new-print-base) *print-base* print-base-p)
		     ((:circle *print-circle*) *print-circle*)
		     ((:pretty *print-pretty*) *print-pretty*)
		     ((:level *print-level*) *print-level*)
		     ((:length *print-length*) *print-length*)
		     ((:case new-print-case) *print-case* print-case-p)
		     ((:array *print-array*) *print-array*)
		     ((:gensym *print-gensym*) *print-gensym*)
		     ((:structure lcl:*print-structure*) lcl:*print-structure*))
    (if (and (system:standard-object-p stream)
	     (typep stream 'fundamental-stream))
	(%string-stream stream (lisp:write object :stream stream 
					   :escape escapep
					   :radix *print-radix*
					   :base new-print-base :circle *print-circle*
					   :pretty *print-pretty*
					   :level *print-level* :length *print-length*
					   :case new-print-case
					   :array *print-array* :gensym *print-gensym*
					   :structure lcl:*print-structure*))
	(funcall orig-lucid-closure object :stream stream :escape escapep
		 :radix *print-radix*
		 :base new-print-base :circle *print-circle* :pretty *print-pretty*
		 :level *print-level* :length *print-length* :case new-print-case
		 :array *print-array* :gensym *print-gensym*
		 :structure lcl:*print-structure*))))


			    

;;; Higher level lisp reading functions.

;; this hack is necessary in order to allow (ACCEPT 'T ...) and
;; (ACCEPT 'EXPRESSION ...) to function (sort of) correctly ....

(defmethod make-instance ((t-class (eql (find-class t))) &rest args)
  (declare (ignore args) (dynamic-extent args))
  t)


(redefine-lucid-io-function lisp:read (&optional (stream *standard-input*)
						 (eof-error-p t)
						 (eof-value nil)
						 (recursive-p nil))
  ;; ACCEPT is only a rough equivalent of READ
  (clim:accept 'clim:expression :stream stream))


;;; Don't forget about this guys even if we don't implement them.

;; READ-PRESERVING-WHITESPACE (&OPTIONAL (STREAM *STANDARD-INPUT*)
;;					 (EOF-ERROR-P T)
;;					 (EOF-VALUE NIL)
;;					 (RECURSIVE-P NIL))

;; READ-DELIMITED-LIST (CHAR &OPTIONAL (STREAM *STANDARD-INPUT*) (RECURSIVE-P NIL))


;;; User Query Functions (interacts with the *QUERY-IO* stream):

;;; Y-OR-N-P (&OPTIONAL FORMAT-STRING &REST FORMAT-ARGS)

;;; NOTE: the built-in presentation type CLIM:BOOLEAN requires YES or NO -- not
;;; Y or P -- as would normally be expected from Y-OR-N-P.

(lcl:defadvice (lisp:y-or-n-p stream-wrapper) (&optional format-string &rest args)
  (declare (dynamic-extent args))
  (if (and (system:standard-object-p *query-io*)
	   (typep *query-io* 'fundamental-stream))
      (clim:accept 'clim:boolean :prompt (apply #'lisp::format nil format-string
					   args))
      (lcl:apply-advice-continue format-string args)))


;;; YES-OR-NO-P (&OPTIONAL FORMAT-STRING &REST FORMAT-ARGS)
;;;
(lcl:defadvice (lisp:yes-or-no-p stream-wrapper) (&optional format-string &rest args)
  (declare (dynamic-extent args))
  (if (and (system:standard-object-p *query-io*)
	   (typep *query-io* 'fundamental-stream))
      (clim:accept 'clim:boolean :prompt (apply #'lisp:format nil format-string
					   args))
      (lcl:apply-advice-continue format-string args)))

#+nope
(lcl:defadvice (lisp:format stream-wrapper) (stream control-string &rest args)
  (let ((stream (if (eq stream t) *standard-output* stream)))
    (if (and (system:standard-object-p stream)
	     (typep stream 'fundamental-stream))
	(apply #'clim:format stream control-string args)
	(lcl:apply-advice-continue stream control-string args))))

