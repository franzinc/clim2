;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; All of this is taken from the STREAM-DEFINITION-BY-USER proposal to
;;; the X3J13 committee, made by David Gray of TI on 22 March 1989.  No
;;; Lisp implementation yet supports this proposal, so we implement it
;;; here in this separate package.  This way we will be ready when some
;;; Lisp implementation adopts it (or something like it).

;;; This file defines functions in the Gray proposal.  This file is
;;; entirely #-CLIM-uses-Lisp-stream-functions, which means that any
;;; class and function names are shadowed in this package (not
;;; inherited from the CL package).


;;; Output functions

(defmacro write-forwarding-cl-output-stream-function (name args)
  (let* ((cl-name (find-symbol (symbol-name name) (find-package :lisp)))
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
			optional-parameters)))
    (when (eq (first optional-args) '&optional)
      (pop optional-args))
    `(define-group ,name write-forwarding-cl-output-stream-function
       (eval-when (compile load eval) (proclaim '(inline ,name)))
       (defun ,name (,@required-args &optional stream ,@optional-args)
	 (cond ((null stream) (setq stream *standard-output*))
	       ((eq stream t) (setq stream *terminal-io*)))
	 (,method-name stream ,@pass-args))

       ;; Define a default method for the generic function that calls back to the
       ;; system stream implementation.  Call back via a message if there is one,
       ;; otherwise via the Common Lisp function.
       ;; Uses T as a parameter specializer name as a standin for cl:stream,
       ;; which Genera doesn't support as a builtin class
       (defmethod ,method-name ((stream t) ,@method-args)
	 (,cl-name ,@required-args stream ,@pass-keys)))))

(write-forwarding-cl-output-stream-function write-byte (integer))

(write-forwarding-cl-output-stream-function write-char (character))

(write-forwarding-cl-output-stream-function write-string (string &key (start 0) end))

(write-forwarding-cl-output-stream-function terpri ())

(write-forwarding-cl-output-stream-function fresh-line ())

(write-forwarding-cl-output-stream-function force-output ())

(write-forwarding-cl-output-stream-function finish-output ())

(write-forwarding-cl-output-stream-function clear-output ())


;;; Input functions

(defmacro write-forwarding-cl-input-stream-function (name lambda-list
						     &key eof
							  additional-arguments)
  (let* ((cl-name (find-symbol (symbol-name name) (find-package :lisp)))
	 (method-name (intern (lisp:format nil "~A-~A" 'stream (symbol-name name))))
	 (method-lambda-list (remove 'stream lambda-list))
	 (args (mapcar #'(lambda (var) (if (atom var) var (first var)))
		       (remove-if #'(lambda (x) (member x lambda-list-keywords))
				  lambda-list)))
	 (stream-args (remove 'stream args))
	 (cleanup `(cond ((null stream) (setq stream *standard-input*))
			 ((eq stream t) (setq stream *terminal-io*))))
	 (call-method `(,method-name stream ,@stream-args)))
    (when (member (first (last method-lambda-list)) lambda-list-keywords)
      (setf method-lambda-list (butlast method-lambda-list)))
    `(define-group ,name write-forwarding-cl-input-stream-function
       (eval-when (compile load eval) (proclaim '(inline ,name)))
       ,(if eof
	    (let ((args `(eof-error-p eof-value ,@(and (not (eq eof :no-recursive))
						       '(recursive-p)))))
	      `(defun ,name (,@lambda-list ,@args)
		 ,cleanup
		 (let ((result ,call-method))
		   (cond ((not (eq result *end-of-file-marker*))
			  result)
			 (eof-error-p
			  (signal-stream-eof stream ,@(and (not (eq eof :no-recursive))
							   '(recursive-p))))
			 (t
			  eof-value)))))
	    `(defun ,name ,lambda-list
	       ,cleanup
	       ,call-method))

       ;; Define a default method for the generic function that calls back to the
       ;; system stream implementation.  Call back via a message if there is one,
       ;; otherwise via the Common Lisp function.
       (defmethod ,method-name ((stream t) ,@method-lambda-list)
	 (,cl-name ,@additional-arguments ,@args ,@(and eof `(nil *end-of-file-marker*)))))))

(write-forwarding-cl-input-stream-function read-byte (&optional stream) :eof :no-recursive)

(write-forwarding-cl-input-stream-function read-char (&optional stream) :eof t)

(write-forwarding-cl-input-stream-function unread-char (character &optional stream))

(write-forwarding-cl-input-stream-function read-char-no-hang (&optional stream) :eof t)

;;(write-forwarding-cl-input-stream-function peek-char (&optional stream)
;;					     :eof t :additional-arguments (nil))

;;; Rewritten for true Gray stream proposal.
;;; STREAM-PEEK-CHAR doesn't get a peek-type; that's all handled at the PEEK-CHAR level.
(defun peek-char (&optional peek-type input-stream (eof-error-p t)
			    eof-value recursive-p)
  #-aclpc (declare (ignore recursive-p))
  #+(or aclpc acl86win32); handle the case of non-clim stream
  (when (typep input-stream 'acl:simple-stream)
    (return-from peek-char
		 (cl:peek-char peek-type
			       input-stream
			       eof-error-p
			       eof-value
			       recursive-p)))
  (case input-stream
    ((nil) (setf input-stream *standard-input*))
    ((t)   (setf input-stream *standard-output*)))
  (assert (or (null peek-type) (eq peek-type t) (characterp peek-type))
      (peek-type)
    "Illegal peek type ~S" peek-type)
  (loop
    (let ((ch (stream-peek-char input-stream)))
      (cond ((eq ch *end-of-file-marker*)
	     (if eof-error-p
		 (signal-stream-eof input-stream recursive-p)
		 (return-from peek-char eof-value)))
	    ((or (null peek-type)
		 (and (eq peek-type t)
		      (not (whitespace-char-p ch)))
		 (eq peek-type ch))
	     (return-from peek-char ch))))))

(write-forwarding-cl-input-stream-function listen (&optional stream))

(write-forwarding-cl-input-stream-function read-line (&optional stream) :eof t)

(write-forwarding-cl-input-stream-function clear-input (&optional stream))

(defun signal-stream-eof (stream &optional recursive-p)
  (declare (ignore recursive-p))
  (error 'end-of-file :stream stream))


;;; PATHNAME and TRUENAME

(defgeneric pathname (stream))

(defmethod pathname (stream)
  (lisp:pathname stream))

(deftype pathname () 'lisp:pathname)


(defgeneric truename (stream))

(defmethod truename (stream)
  (lisp:truename stream))


;;; Make CLIM-LISP:FORMAT do something useful on CLIM windows.

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
