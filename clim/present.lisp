;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: present.lisp,v 1.7 92/09/30 18:03:54 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defun present (object &optional (presentation-type (presentation-type-of object))
		&key (stream *standard-output*) (view (stream-default-view stream))
		     (modifier nil) (acceptably nil)
		     (for-context-type presentation-type)
		     (single-box nil) (allow-sensitive-inferiors *allow-sensitive-inferiors*)
		     (sensitive *allow-sensitive-inferiors*)
		     (record-type 'standard-presentation))

  ;; The arguments are allowed to be presentation type abbreviations
  (multiple-value-bind (expansion expanded)
      (expand-presentation-type-abbreviation presentation-type)
    (when expanded
      (when (eq for-context-type presentation-type)
	(setq for-context-type expansion))
      (setq presentation-type expansion)))
  (unless (eq for-context-type presentation-type)
    (multiple-value-bind (expansion expanded)
	(expand-presentation-type-abbreviation for-context-type)
      (when expanded
	(setq for-context-type expansion))))

  #+compulsive-type-checking
  (unless (presentation-typep object presentation-type)
    (cerror "Use the type ~*~*~S instead"
	    "The object ~S is not of type ~S"
	    object presentation-type (class-name (class-of object))))

  ;; Make a presentation if desired, and call the type's present method to fill it in
  (if (and sensitive
	   ;; The right way to fix this is probably to make all the
	   ;; WITH-xxx macros turn into noops on non-window-streams, but
	   ;; this is easier and less expensive.
	   (extended-output-stream-p stream))
      (with-output-as-presentation (stream object presentation-type
				    :modifier modifier
				    :single-box single-box
				    :allow-sensitive-inferiors allow-sensitive-inferiors
				    :record-type record-type)
	(funcall-presentation-generic-function present
	  object presentation-type stream view
	  :acceptably acceptably :for-context-type for-context-type))
      (let ((*allow-sensitive-inferiors* allow-sensitive-inferiors))
	(funcall-presentation-generic-function present
	  object presentation-type stream view
	  :acceptably acceptably :for-context-type for-context-type))))

(defun present-to-string (object
			  &optional (presentation-type (presentation-type-of object))
			  &key (view +textual-view+) (acceptably nil)
			       (for-context-type presentation-type)
			       string index)
  (cond (string
	 (when index
	   (setf (fill-pointer string) index))
	 (with-output-to-string (stream string)
	   (present object presentation-type :stream stream :view view
		    :acceptably acceptably :for-context-type for-context-type))
	 (values string (fill-pointer string)))
	(t
	 (with-output-to-string (stream)
	   (present object presentation-type :stream stream :view view
		    :acceptably acceptably :for-context-type for-context-type)))))

(defmethod stream-present ((stream basic-extended-output-protocol) 
			   object type 
			   &key (view (stream-default-view stream))
			   (modifier nil) (acceptably nil)
			   (for-context-type type) (single-box nil)
			   ;;--- should these next two default to
			   ;;*allow-sensitive-inferiors*? 
			   (allow-sensitive-inferiors t) (sensitive t)
			   (record-type 'standard-presentation))
  (present object type :stream stream :view view :modifier modifier
	   :acceptably acceptably :for-context-type for-context-type
	   :single-box single-box 
	   :allow-sensitive-inferiors allow-sensitive-inferiors 
	   :sensitive sensitive :record-type record-type))

;; Like WITH-OUTPUT-TO-STRING, but stream is a full output-recording
;; protocol stream so all CLIM operations "just work", rather than having
;; to special-case them all for string streams.  Result is a string
;; representation of the output.  STREAM must be an actual window; the
;; formatting of the string will match the dimensions of the window.
(defmacro with-presentations-to-string ((stream &optional string
					 &rest options
					 &key (element-type ''character)
					      (end-of-line-action ':allow)
					      (end-of-page-action ':allow))
					&body body)
  (declare (ignore element-type end-of-line-action end-of-page-action))
  (default-output-stream stream with-presentations-to-string)
  `(flet ((with-presentations-to-string-body (,stream) ,@body))
     (declare (dynamic-extent #'with-presentations-to-string-body))
     (invoke-with-presentations-to-string 
       ,stream #'with-presentations-to-string-body ,string ,@options)))

(defun invoke-with-presentations-to-string (stream continuation string
					   &key (element-type 'character) 
						(end-of-line-action ':allow)
						(end-of-page-action ':allow))
  (let ((record
	  (with-end-of-line-action (stream end-of-line-action)
	    (with-end-of-page-action (stream end-of-page-action)
	      (with-output-to-output-record (stream)
		(funcall continuation stream))))))
    (when (null string)
      (setq string (make-array 40 :fill-pointer 0 :element-type element-type)))
    (with-output-to-string (s string)
      (copy-textual-output-history stream s nil record)
      string)))
