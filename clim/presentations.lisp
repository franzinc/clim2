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

;; $fiHeader: presentations.lisp,v 1.2 91/12/13 10:36:54 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
Copyright (c) 1991, Franz Inc. All rights reserved
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(defclass presentation () ())

(defun-inline presentationp (object) (typep object 'presentation))

(defclass standard-presentation
	  (standard-sequence-output-record presentation)
     ((object :accessor presentation-object :initarg :object)
      (type :accessor presentation-type :initarg :type)
      (single-box :accessor presentation-single-box :initarg :single-box))
  (:default-initargs :size 5))

(define-output-record-constructor standard-presentation
				  (&key object type modifier
					single-box allow-sensitive-inferiors (size 5))
  :object object :type type :modifier modifier
  :single-box single-box :allow-sensitive-inferiors allow-sensitive-inferiors
  :size size)

(defmethod print-object ((object standard-presentation) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S /x ~A:~A y ~A:~A/"
	    (safe-slot-value object 'type)
	    (safe-slot-value object 'object)
	    (safe-slot-value object 'left)
	    (safe-slot-value object 'right)
	    (safe-slot-value object 'top)
	    (safe-slot-value object 'bottom))))

(defmethod shared-initialize :after ((presentation standard-presentation) ignore
				     &key modifier allow-sensitive-inferiors single-box)
  ;;--- Support MODIFIER and ALLOW-SENSITIVE-INFERIORS sometime...
  (declare (ignore modifier allow-sensitive-inferiors))
  (check-type single-box (member t nil :highlighting :position))
  (with-slots (type) presentation
    (setq type (evacuate-list type)))
  nil)

;;; A presentation can fit anywhere in the output-record hierarchy
;;; This means that you are allowed to have a hierarchy like:
;;;
;;; PRESENTATION
;;;   TABLE
;;;     ROW 1
;;;       CELL 1.1
;;;       CELL 1.2
;;;     PRESENTATION
;;;       ROW 2
;;;         CELL 2.1
;;;         PRESENTATION
;;;           CELL 2.2
;;;     ROW 3
;;;       PRESENTATION
;;;         CELL 3.1
;;;         CELL 3.2

(defmethod match-output-records ((record standard-presentation)
				 &rest init-args &key object type &allow-other-keys)
  (declare (ignore init-args))
  (and (eql (presentation-object record) object)
       (eql (presentation-type record) type)))

(defmethod highlight-output-record-1 ((record output-record) stream state)
  ;; State is :HIGHLIGHT or :UNHIGHLIGHT
  (declare (ignore state))
  (with-bounding-rectangle* (left top right bottom) record
			    (draw-rectangle* stream left top right bottom
					     :filled nil :ink +flipping-ink+)))

(defmethod highlight-output-record-1 :around ((record standard-presentation) stream state)
  (let ((single-box (presentation-single-box record)))
    (if (or (eql single-box t) (eql single-box :highlighting))
	(call-next-method)
      ;; Handles :SINGLE-BOX :POSITION
      (labels ((highlight (inferior)
		 (when (displayed-output-record-element-p inferior)
		   (if (eq record inferior)
		       (call-next-method)	;avoid infinite recursion
		       (highlight-output-record-1 inferior stream state)))
		 (map-over-output-record-children 
		    #'highlight inferior +everywhere+)))
	(declare (dynamic-extent #'highlight))
	(highlight record)))))

(defun highlight-output-record (stream record state &optional presentation-type)
  (unless presentation-type
    (when (presentationp record)
      (setq presentation-type (presentation-type record))))
  (highlight-presentation record presentation-type stream state))

(define-presentation-type blank-area ())

(define-presentation-method presentation-typep (object (type blank-area))
  (eq object *null-presentation*))

;;; This is the presentation you get if you click while not over anything...
(setq *null-presentation*
      (let ((null-presentation (make-instance 'standard-presentation
					      :object nil :type 'blank-area)))
	(setf (presentation-object null-presentation) null-presentation)
	null-presentation))

(defun window-cohorts-p (window-one window-two)
  ;;; --- The best test for whether or not
  ;;; two windows care about each other's mouse motion
  ;;; (for now) is whether they share an io-buffer.
  ;;; This is a function rather than a multi-method (which probably would be faster)
  ;;; because we don't think our trampoline technology could forward this
  ;;; from an encapsulating stream properly.
  (eq (stream-input-buffer window-one)
      (stream-input-buffer window-two)))

(defun highlight-presentation-of-context-type (stream)
  (highlight-applicable-presentation
    #-Silica *application-frame*
    #+Silica (pane-frame stream)
    stream *input-context*))

(defun input-context-button-press-handler (stream button)
  (frame-input-context-button-press-handler 
    #-Silica *application-frame*
    #+Silica (pane-frame stream)
    stream button))

(defun with-input-context-1 (type override body-continuation context-continuation)
  (declare (dynamic-extent body-continuation context-continuation))
  (with-presentation-type-decoded (type-name type-parameters) type
    (with-stack-list* (type type-name type-parameters)
      (with-stack-list (this-tag 'catch-tag type)	;unique
	(with-stack-list (this-context type this-tag)
	  (let ((pwindow nil)
		(px 0)
		(py 0)
		(old-shifts 0))
	    (flet ((pointer-motion-pending (stream)
		     (let ((moved-p nil))
		       (multiple-value-setq (moved-p pwindow px py)
			 (pointer-state-changed (stream-primary-pointer stream)
						pwindow px py))
		       (or 
			 (and moved-p
			      pwindow
			      (window-cohorts-p stream pwindow))
			 ;; This is more like the pointer moving than anything else.
			 (/= old-shifts
			     (setf old-shifts (window-shift-mask stream)))))))
	      (declare (dynamic-extent #'pointer-motion-pending))
	      (let ((*input-wait-handler* #'highlight-presentation-of-context-type)
		    (*input-wait-test* #'pointer-motion-pending)
		    (*pointer-button-press-handler* #'input-context-button-press-handler))
		;; Push the new context.  Most specific context is at the beginning
		;; of *INPUT-CONTEXT*
		(with-stack-list* (*input-context*
				    this-context (unless override *input-context*))
		  (block with-input-context
		    (multiple-value-bind (object ptype event options)
			(catch this-tag
			  (return-from with-input-context-1
			    (funcall body-continuation)))
		      (funcall context-continuation object ptype event options))))))))))))

(eval-when (eval compile load)
(defun make-input-context-clauses (pt-var clauses)
  (let ((new-clauses nil))
    (dolist (clause clauses (nreverse new-clauses))
      (let ((type (first clause))
	    (body (rest clause)))
	(push `((presentation-subtypep ,pt-var ',type)
		,@body)
	      new-clauses)))))
)	;eval-when

(defmacro with-input-context ((type &key override) 
			      (&optional object-var type-var event-var options-var)
			      form
			      &body clauses)
  #+Genera (declare (zwei:indentation 0 2 2 4 3 2))
  (let ((ignores nil))
    (when (null object-var)
      (setq object-var '#:object)
      (push object-var ignores))
    (when (null type-var)
      (setq type-var '#:presentation-type)
      (unless clauses (push type-var ignores)))
    (when (null event-var)
      (setq event-var '#:event)
      (push event-var ignores))
    (when (null options-var)
      (setq options-var '#:options)
      (push options-var ignores))
    `(flet ((body-continuation () ,form)
	    (context-continuation (,object-var ,type-var ,event-var ,options-var)
	      ,@(and ignores `((declare (ignore ,@ignores))))
	      (cond ,@(make-input-context-clauses type-var clauses))))
       (declare (dynamic-extent #'body-continuation #'context-continuation))
       (with-input-context-1
	 (expand-presentation-type-abbreviation ,type) ,override
	 #'body-continuation #'context-continuation))))

;;; This is what happens when you click.  It does a type walk looking
;;; for the innermost catch tag that satisfies this presentation.
;;; Eventually, it will take handlers into account
(defun throw-highlighted-presentation (presentation input-context button-press-event)
  (catch 'no-translation
    (let ((x (pointer-event-x button-press-event))
	  (y (pointer-event-y button-press-event))
	  (window (event-sheet button-press-event))
	  (frame *application-frame*))
      (dolist (this-context input-context)
	(let* ((context-type (input-context-type this-context))
	       (tag (input-context-tag this-context)))
	  (let ((translator (presentation-matches-context-type
			      presentation context-type
			      frame window x y
			      :event button-press-event)))
	    (when translator
	      (multiple-value-bind (translated-object translated-type options)
		  ;; Suppose this is a presentation action that
		  ;; pops up a window to do input...
		  (let ((*original-stream* nil))
		    ;; Eat the gesture that invoked the translator now, since it
		    ;; might be an action that invokes a menu.  (In that case, the
		    ;; the user can type <Abort>, which will cause the gesture
		    ;; not to be eaten at all.)
		    ;;--- Yuck, this can't possibly be right.
		    (read-gesture :stream window :timeout 0)
		    (call-presentation-translator translator presentation context-type
						  frame button-press-event window x y))
		(throw tag (values translated-object
				   (or translated-type
				       ;; Sure to be stack-consed...
				       (evacuate-list context-type))
				   button-press-event
				   options))))))))
    (unless (eq presentation *null-presentation*)
      (beep))))

(defun find-innermost-applicable-presentation
       (input-context window x y &optional (shift-mask (window-shift-mask window))
					   (frame *application-frame*))
  (declare (fixnum x y))
  ;; Depth first search for a presentation that is both under the pointer and
  ;; matches the input context.
  ;; This relies on map-over-output-record-elements-containing-point* traversing
  ;; the most recently drawn of overlapping output records first.
  (labels ((mapper (record presentations)
	     ;; RECORD is an output record whose bounding rectangle contains (X,Y).
	     ;; PRESENTATIONS is a list of non-:single-box presentations superior to RECORD.
	     ;; X-OFFSET and Y-OFFSET are the position on the drawing plane of the
	     ;; origin of RECORD's coordinate system, i.e. RECORD's parent's start-position.
	     (multiple-value-bind (sensitive superior-sensitive inferior-presentation)
		 ;; SENSITIVE is true if RECORD is a presentation to test against the context
		 ;; SUPERIOR-SENSITIVE is true if PRESENTATIONS should be tested also
		 ;; INFERIOR-PRESENTATION is a presentation to pass down to our inferiors
		 (if (presentationp record)
		     (if (output-record-refined-sensitivity-test
			   record
			   (the fixnum x) (the fixnum y))
			 ;; Passed user-defined sensitivity test for presentations.
			 ;; It might be both a presentation and a displayed output record.
			 ;; It might be sensitive now [:single-box t] or the decision might
			 ;; depend on finding a displayed output record [:single-box nil].
			 (let ((displayed (displayed-output-record-element-p record))
			       (single-box (presentation-single-box record)))
			   (if (or (eq single-box t) (eq single-box :position))
			       ;; This presentation is sensitive
			       (values t displayed nil)
			       ;; This presentation is not presented as a single box,
			       ;; so it contains the point (X,Y) if and only if a
			       ;; visibly displayed inferior contains that point.
			       (values nil displayed record)))
			 (values nil nil nil))
		     ;; RECORD is not a presentation, but a superior presentation's
		     ;; sensitivity might depend on whether record contains (X,Y)
		     (values nil
			     (and presentations
				  (dolist (presentation presentations nil)
				    (unless (null presentation) (return t)))
				  (displayed-output-record-element-p record)
				  ;; Refined sensitivity test for displayed graphics
				  (output-record-refined-sensitivity-test
				    record
				    (the fixnum x) (the fixnum y)))
			     nil))

	       ;; Add INFERIOR-PRESENTATION to PRESENTATIONS
	       (with-stack-list* (more-presentations inferior-presentation presentations)
		 (when inferior-presentation
		   (setq presentations more-presentations))
  
		 ;; Depth-first recursion
		 (map-over-output-records-containing-point*
		  record #'(lambda (rec) (mapper rec presentations)) x y)
  
		 ;; If we get here, didn't find anything in the inferiors of record so test
		 ;; any presentations that are now known to be sensitive, depth-first
		 (when sensitive
		   (test record))
		 (when superior-sensitive
		   (do* ((presentations presentations (cdr presentations))
			 (presentation (car presentations) (car presentations)))
			((null presentations))
		     (when presentation
		       (test presentation)
		       ;; A given presentation only has to be tested once
		       (setf (car presentations) nil)))))))
	   (test (presentation)
	     ;; This presentation contains the point (X,Y).  See if there is
	     ;; a translator from it to the input context.
	     (dolist (context input-context)
	       (let ((context-type (input-context-type context)))
		 (when (presentation-matches-context-type presentation context-type
							  frame window x y
							  :shift-mask shift-mask)
		   (return-from find-innermost-applicable-presentation
		     presentation))))))
    (declare (dynamic-extent #'mapper #'test))
    (mapper (output-recording-stream-output-record window) nil)))

(defvar *last-pointer-documentation-shift-mask* 0)

(defun highlight-applicable-presentation (frame stream input-context
					  &optional (prefer-pointer-window t))
  (let ((history-window (if prefer-pointer-window
			    (find-appropriate-window stream)
			    stream)))
    ;; If non-NIL, guaranteed to have output-recording-mixin.
    (when history-window
      (with-slots (highlighted-presentation) history-window
	(multiple-value-bind (px py)
	    (stream-pointer-position* history-window)
	  (let ((presentation (frame-find-innermost-applicable-presentation
				frame input-context history-window px py)))
	    ;; Does this cover all the cases?
	    (cond ((and (null presentation)
			(null highlighted-presentation))
		   ;; Nothing highlighted, over nothing
		   )
		  ((null presentation)
		   ;; Over nothing now, was over something before.
		   ;; Unhighlight.
		   (highlight-output-record 
		     history-window highlighted-presentation :unhighlight)
		   (setf highlighted-presentation nil)
		   (when *pointer-documentation-output*
		     (frame-document-highlighted-presentation
		       frame highlighted-presentation
		       input-context history-window px py
		       *pointer-documentation-output*)))
		  ((eq presentation highlighted-presentation)
		   ;; Over the same presentation as before
		   (when (and *pointer-documentation-output*
			      (/= (window-shift-mask history-window)
				  *last-pointer-documentation-shift-mask*))
		     (frame-document-highlighted-presentation
		       frame presentation
		       input-context history-window px py
		       *pointer-documentation-output*))
		   nil)
		  (t
		   ;; Over something now, might have been over something before
		   (when highlighted-presentation
		     (highlight-output-record 
		       history-window highlighted-presentation :unhighlight))
		   (highlight-output-record history-window presentation :highlight)
		   (setf highlighted-presentation presentation)
		   (when *pointer-documentation-output*
		     (frame-document-highlighted-presentation
		       frame presentation
		       input-context history-window px py
		       *pointer-documentation-output*))))))))))

(defun highlighted-presentation (stream &optional (prefer-pointer-window t))
  (let ((history-window (if prefer-pointer-window (find-appropriate-window stream) stream)))
    (when (and history-window
	       (slot-exists-p history-window 'highlighted-presentation))
      (slot-value history-window 'highlighted-presentation))))

(defun set-highlighted-presentation (stream presentation &optional (prefer-pointer-window t))
  (let ((history-window (if prefer-pointer-window (find-appropriate-window stream) stream)))
    (when history-window
      (with-slots (highlighted-presentation) history-window
	(cond ((eq highlighted-presentation presentation)
	       )
	      (t
	       (when highlighted-presentation
		 ;; unhighlight old presentation
		 (highlight-output-record 
		   history-window highlighted-presentation :unhighlight))
	       (setf highlighted-presentation presentation)
	       (when presentation
		 (highlight-output-record history-window presentation :highlight))))))))

(defun unhighlight-highlighted-presentation (stream &optional (prefer-pointer-window t))
  (let ((history-window (if prefer-pointer-window (find-appropriate-window stream) stream)))
    (when history-window
      (with-slots (highlighted-presentation) history-window
	(when highlighted-presentation
	  (highlight-output-record history-window highlighted-presentation :unhighlight)
	  (setf highlighted-presentation nil))))))

(warn "Checking for port")

(defun find-appropriate-window (stream)
  ;;--- how do we hack multiple pointers?
  (let* ((pointer (stream-primary-pointer stream))
	 (window (pointer-window pointer)))
    ;; It ain't no good if it doesn't have a history.
    (when (and window
	       (silica::port window)
	       (output-recording-stream-p window))
      window)))
