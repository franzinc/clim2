;;; -*- Syntax: Common-Lisp; Package: CLIM-UTILS; Base: 10; Mode: LISP; Lowercase: Yes -*-
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
;; $fiHeader$

(in-package :clim-utils)

"Copyright (c) 1990 International Lisp Associates.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

;;; This is a compile-time tool to make it easier for us to install lisp-system
;;; dependent code.

;;; Tool to temporarily add a macro character.
(defmacro with-macro-character ((char function) &body body)
  (let ((existing (make-symbol "EXISTING")))
    `(let ((,existing (get-macro-character ,char)))
       (unwind-protect
	   (progn
	     (set-macro-character ,char ,function)
	     ,@body)
	 (set-macro-character ,char ,existing)))))

(defun |READ-#-{| (stream subchar arg)
  (declare (ignore subchar arg))
  (labels ((feature-p (feature)
	     (cond ((atom feature)
		    (or (member feature *features*)
			;; Also check to see if there's a keyword
			;; feature of the same name.
			(and (symbolp feature)
			     (member (intern (symbol-name feature)
					     (find-package 'keyword))
				     *features*))))
		   ((eq (first feature) 'not)
		    (not (feature-p (cadr feature))))
		   ((eq (first feature) 'and)
		    (every #'feature-p (rest feature)))
		   ((eq (first feature) 'or)
		    (some #'feature-p (rest feature)))
		   (t (error "Unknown feature spec: ~S" feature)))))
    (declare (dynamic-extent #'feature-p))
    (let ((form nil)
	  (features-so-far nil)
	  (found-it nil)
	  (found-otherwise nil))
      (catch 'stop
	;; here's where we install #\} as the terminating character
	;; for #{
	(with-macro-character (#\} '|READ-}|)
	  (loop
	    (let ((feature (read stream t nil t)))
	      (when found-otherwise
		(error "It is illegal to specify #{ ... } clauses after OTHERWISE: ~S"
		       feature))
	      (when (eq feature 'otherwise)
		(setq found-otherwise t))
	      (push feature features-so-far)
	      (cond (found-it
		     (let ((*read-suppress* t))
		       (read stream t nil t)))
		    ((or found-otherwise (feature-p feature))
		     (setq form (read stream t nil t)
			   found-it t))
		    (t (let ((*read-suppress* t))
			 (read stream t nil t))))))))
      (cond (found-it form)
	    (t 
	     `(macrolet
	       ((compile-time-warn ()
		      (warn "No #{ ... } clause for this implementation was specified.  Only clauses for ~S"
			    ',(setq features-so-far (nreverse features-so-far)))))
	       (compile-time-warn)
	       (cerror 
		 "Return NIL"
		 "No #{ ... } clause for this implementation was specified.  Only clauses for ~S]"
		 ',features-so-far)
	       nil))))))

(defun |READ-}| (stream arg)
  (declare (ignore stream arg))
  (throw 'stop t))

;;; Install the #{ reader.
(set-dispatch-macro-character #\# #\{ #'|READ-#-{|)

