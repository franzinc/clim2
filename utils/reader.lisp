;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1990 International Lisp Associates.  All rights reserved."

;;; This is a compile-time tool to make it easier for us to install lisp-system
;;; dependent code.

;;; Install the #{ reader, only when compiling.
(eval-when (:compile-toplevel :execute)
;;; Tool to temporarily add a macro character.
(defmacro with-macro-character ((char function) &body body)
  (let ((existing (make-symbol (symbol-name 'existing))))
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
									  (member (intern (symbol-name feature) :keyword)
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
			 (with-macro-character (#\} #'|READ-}|)
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

  (set-dispatch-macro-character #\# #\{ #'|READ-#-{|))

