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

;; $fiHeader: present.lisp,v 1.7 91/08/05 14:35:12 cer Exp $

(in-package :clim)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

(defun default-presentation-type-of (object)
  (dolist (class (class-precedence-list (class-of object)) 'expression)
    (let ((name (class-proper-name class)))
      (unless (eq name 't)	;prefer expression over t
	(when (or (acceptable-presentation-type-class class)
		  (and (symbolp name)
		       (find-presentation-type-class name nil)
		       (presentation-type-specifier-p name)))
	  (return name))))))

(defun present (object &optional (presentation-type (default-presentation-type-of object))
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
	   ;; the right way to fix this is probably
	   ;; to make all the with-xxx macros turn into noops on non-window-streams, but
	   ;; this is easier and less expensive.
	   (extended-output-stream-p stream))
      (with-output-as-presentation (:stream stream
				    :object object
				    :type presentation-type
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
			  &optional (presentation-type (default-presentation-type-of object))
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
