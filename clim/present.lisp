;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: present.lisp,v 1.2 92/01/31 14:58:31 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defun present (object &optional (presentation-type (presentation-type-of object))
		&key (stream *standard-output*) (view (stream-default-view stream))
		     (modifier nil) (acceptably nil)
		     (for-context-type presentation-type)
		     (single-box nil) (allow-sensitive-inferiors t)
		     (sensitive t) (record-type 'standard-presentation))

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
	(call-presentation-generic-function present
	  object presentation-type stream view
	  :acceptably acceptably :for-context-type for-context-type))
      (call-presentation-generic-function present
	object presentation-type stream view
	:acceptably acceptably :for-context-type for-context-type)))

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
