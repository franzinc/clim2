;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: tracking-pointer.lisp,v 1.5 92/03/04 16:22:24 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(eval-when (compile load eval)

;;; For compile-time checking.
(defvar *tracking-pointer-clauses* nil)		;each clause looks like (name arglist)

(defmacro register-tracking-pointer-clause (name arglist)
  `(push-unique '(,name ,arglist) *tracking-pointer-clauses* :key #'car))

(defun validate-tracking-pointer-clause (name arglist)
  (declare (values new-arglist ignores))
  (let ((item (find name *tracking-pointer-clauses* :key #'car)))
    (unless item
      (error "~S is not a known TRACKING-POINTER clause." name))
    ;; If we had an error system we'd use it here to generate a better
    ;; error message
    (canonicalize-and-match-lambda-lists (second item) arglist)))

;; The clauses...
(register-tracking-pointer-clause :pointer-motion (window x y))
(register-tracking-pointer-clause :pointer-button-press (event x y))
(register-tracking-pointer-clause :pointer-button-release (event x y))
(register-tracking-pointer-clause :keyboard (character))
(register-tracking-pointer-clause :presentation (presentation window x y))
(register-tracking-pointer-clause :presentation-button-press (presentation event x y))
(register-tracking-pointer-clause :presentation-button-release (presentation event x y))

(defvar *tracking-pointer-presentation-clauses*
	'(:presentation :presentation-button-press :presentation-button-release))

)	;eval-when

(defmacro tracking-pointer ((&optional stream
			     &key pointer multiple-window transformp 
				  (context-type t) (highlight nil highlight-p))
			    &body clauses)
  (default-output-stream stream tracking-pointer)
  (let ((do-highlighting highlight)
	(case-clauses nil)
	(function-defs nil)
	(names nil)
	(function-type '#:function-type))
    (dolist (clause clauses)
      (let* ((type (pop clause))
	     (arglist (pop clause))
	     (body clause)
	     (function-name (gensymbol type))
	     (ignores nil))
	(multiple-value-setq (arglist ignores)
	  (validate-tracking-pointer-clause type arglist))
	(unless highlight-p
	  ;; If there are any presentation clauses, the default is to do highlighting
	  (when (member type *tracking-pointer-presentation-clauses*)
	    (setq do-highlighting t)))
	(push `(,function-name ,arglist
		;; how to merge this with implementation-dependent
		;; declare-downward-function declaration?
		(declare ,@(when ignores `((ignore ,@ignores))))
		,@body)
	      function-defs)
	(push `(,type #',function-name) case-clauses)
	(push `#',function-name names)))
    ;; We do this hack of FLET and a generator function so that
    ;; TRACKING-POINTER-1 need not take a hundred arguments.
    ;; Thus, if we extend what TRACKING-POINTER does, it won't be
    ;; necessary to recompile every single caller.
    `(block nil
       (flet ,function-defs
	 (declare (dynamic-extent ,@names))
	 (flet ((tracking-pointer-clause-generator (,function-type)
		 (case ,function-type
		   ,@case-clauses)))
	   (declare (dynamic-extent #'tracking-pointer-clause-generator))
	   (tracking-pointer-1 ,stream ,pointer
				      #'tracking-pointer-clause-generator
				      :multiple-window ,multiple-window
				      :transformp ,transformp
				      :context-type ,context-type
				      :highlight ,do-highlighting))))))

(defun tracking-pointer-1 (stream pointer generator
				  &key multiple-window transformp 
				       context-type highlight)
  (unless pointer
    (setq pointer (stream-primary-pointer stream)))
  (let* ((moved-p nil)
	 (current-window stream)
	 (last-window nil)
	 (last-x nil) (last-y nil)
	 (input-happened nil)
	 ;; Grab these up front.  The closures will share the same environment.
	 (motion-function (funcall generator :pointer-motion))
	 (button-press-function (funcall generator :pointer-button-press))
	 (button-release-function (funcall generator :pointer-button-release))
	 (presentation-motion-function (funcall generator :presentation))
	 (presentation-press-function (funcall generator :presentation-button-press))
	 (presentation-release-function (funcall generator :presentation-button-release))
	 (keyboard-function (funcall generator :keyboard))
	 ;; Ask the substrate to generate release events for us, or on
	 ;; Genera's sheet window system, do it right here.
	 (*generate-button-release-events*
	   (or button-release-function presentation-release-function))
	 ;;--- We shouldn't need to kludge this any more!
	 #+Genera (generate-release-events
		    (and *generate-button-release-events*
			 (eql (port-type (port stream)) ':genera)))
	 #+Genera (genera-mouse 
		    (and generate-release-events
			 (tv:sheet-mouse 
			   (scl:send (slot-value (sheet-medium stream) 'genera-clim::window)
				     :screen))))
	 #+Genera (genera-mouse-buttons 
		    (and generate-release-events
			 (tv:mouse-last-buttons genera-mouse)))
	 (highlighted-presentation nil)
	 (highlighted-presentation-type nil)
	 ;; Bind the input context so that presentation searching works
	 (*input-context*
	   (with-presentation-type-decoded (name parameters) context-type
	     (list (list (list* name parameters) nil))))
	 ;; Bind these to NIL in case someone is doing TRACKING-POINTER
	 ;; inside of a presentation action
	 (*input-wait-test* nil)
	 (*input-wait-handler* nil)
	 (*pointer-button-press-handler* nil))
    (macrolet ((highlight (presentation)
		 `(progn
		    (setq highlighted-presentation ,presentation
			  highlighted-presentation-type (presentation-type ,presentation))
		    (highlight-presentation 
		      highlighted-presentation highlighted-presentation-type
		      stream :highlight)))
	       (unhighlight ()
		 `(when highlighted-presentation
		    (highlight-presentation
		      highlighted-presentation highlighted-presentation-type
		      stream :unhighlight)
		    (setq highlighted-presentation nil))))
      (flet (#+Genera
	     ;; Genera's ancient window system cannot be easily cajoled into
	     ;; doing this, so we'll do it by hand right here.
	     (make-release-event (window pointer buttons)
	       (multiple-value-bind (px py) (pointer-position* pointer)
		 (multiple-value-bind (ox oy)
		     #+Silica (values 0 0)
		     #-Silica (window-offset window)
		   (let ((wx (- px ox))
			 (wy (- py oy))
			 (mask (tv:mouse-chord-shifts genera-mouse)))
		     #-Silica
		     (multiple-value-setq (wx wy)
		       (viewport-to-drawing-surface-coordinates current-window wx wy))
		     (let ((event (make-instance 'pointer-button-release-event
				    :sheet window
				    :button buttons
				    :modifiers mask
				    :x wx :y wy)))
		       (when transformp
			 (multiple-value-bind (tx ty)
			     (transform-point* (medium-transformation window)
					       wx wy)
			   (setq wx (floor tx)
				 wy (floor ty))))
		       (values event wx wy)))))))
	#+Genera (declare (dynamic-extent #'make-release-event))
	#+Genera (when (and button-release-function
			    generate-release-events
			    (zerop genera-mouse-buttons))
		   ;; Kludge initial release event
		   (multiple-value-bind (event wx wy)
		       (make-release-event stream pointer 0)
		     (funcall button-release-function event wx wy)))
	(unwind-protect
	    (loop
	      ;; Handle pointer motion
	      (block handle-simple-motion
		(when (or motion-function presentation-motion-function highlight)
		  (when multiple-window
		    (setq current-window (or (pointer-window pointer) (pointer-root pointer))))
		  (multiple-value-bind (x y) (pointer-position* pointer)
		    (when moved-p
		      (setq moved-p nil)
		      (setq last-x x last-y y
			    last-window current-window)
		      ;; Pointer position is in root coordinates
		      ;;--- What to do about window offset and drawing-to-surface-coordinates?
		      (multiple-value-bind (ox oy) 
			  #+Silica (values 0 0)
			  #-Silica (window-offset current-window)
			(declare (type coordinate ox oy))
			(let ((wx (- x ox))
			      (wy (- y oy)))
			  (declare (type coordinate wx wy))
			  #-Silica
			  (multiple-value-setq (wx wy)
			    (viewport-to-drawing-surface-coordinates current-window wx wy))
			  (when (or presentation-motion-function highlight)
			    (let ((presentation
				    (frame-find-innermost-applicable-presentation
				      *application-frame* *input-context*
				      current-window wx wy)))
			      (when presentation
				(when highlight
				  (unless (eq presentation highlighted-presentation)
				    (unhighlight)
				    (highlight presentation)))
				(when presentation-motion-function
				  (funcall presentation-motion-function
					   presentation current-window wx wy))
				(return-from handle-simple-motion))))
			  (unhighlight)
			  (when motion-function
			    (when transformp
			      (multiple-value-bind (tx ty)
				  (transform-point* (medium-transformation current-window)
						    wx wy)
				(setq wx (floor tx)
				      wy (floor ty))))
			    (funcall motion-function
				     current-window wx wy))))))))
	      (block input-wait
		(loop
		  ;; Handle any clicks or characters, otherwise wait for something
		  ;; interesting to happen.
		  (when multiple-window
		    (setq current-window (or (pointer-window pointer)
					     (pointer-root pointer))))
		  (flet ((pointer-motion-pending (window)
			   (when (and (or multiple-window
					  (eq current-window (pointer-window pointer)))
				      (eq (stream-primary-pointer window) pointer))
			     (multiple-value-setq (moved-p last-window last-x last-y)
			       (pointer-state-changed pointer last-window last-x last-y))
			     (or (and moved-p last-window)
				 #+Genera (and generate-release-events
					       (not (zerop genera-mouse-buttons))
					       (zerop (tv:mouse-last-buttons genera-mouse)))))))
		    (declare (dynamic-extent #'pointer-motion-pending))
		    (setq input-happened
			  (stream-input-wait current-window
					     :input-wait-test #'pointer-motion-pending)))
		  ;; Don't call READ-GESTURE if there is no gesture to read
		  ;;--- This has the possibly undesirable side-effect that the
		  ;;--- "input focus" will follow the mouse, having particularly 
		  ;;--- weird behavior regarding keystrokes.
		  (let ((gesture
			  (cond #+Genera
				((and generate-release-events
				      (not (zerop genera-mouse-buttons))
				      (zerop (tv:mouse-last-buttons genera-mouse)))
				 (prog1 
				   (when button-release-function
				     (make-release-event
				       current-window pointer genera-mouse-buttons))
				   (setq genera-mouse-buttons 0)))
				(input-happened
				 (read-gesture :stream current-window :timeout 0)))))
		    (block process-gesture
		      (cond ((null gesture)
			     (return-from input-wait))
			    ((keyboard-event-p gesture)
			     (when keyboard-function
			       (funcall keyboard-function gesture)
			       (return-from process-gesture)))
			    ((or (typep gesture 'pointer-button-press-event)
				 (typep gesture 'pointer-button-release-event))
			     (let* ((px (pointer-event-x gesture))
				    (py (pointer-event-y gesture))
				    (wx px)
				    (wy py))
			       (when transformp
				 (multiple-value-bind (tx ty)
				     (transform-point* (medium-transformation current-window)
						       px py)
				   (setq wx (floor tx)
					 wy (floor ty))))
			       (typecase gesture
				 (pointer-button-press-event
				   #+Genera (when generate-release-events
					      (setq genera-mouse-buttons
						    (pointer-event-button gesture)))
				   (when presentation-press-function
				     (let* ((window (event-sheet gesture))
					    (presentation
					      (frame-find-innermost-applicable-presentation
						*application-frame* *input-context*
						window px py :event gesture)))
				       (when presentation
					 (funcall presentation-press-function
						  presentation gesture wx wy)
					 (return-from process-gesture))))
				   (when button-press-function
				     (funcall button-press-function gesture wx wy))
				   (return-from process-gesture))
				 (t		;pointer-button-release-event
				   (when presentation-release-function
				     (let* ((window (event-sheet gesture))
					    (presentation
					      (frame-find-innermost-applicable-presentation
						*application-frame* *input-context*
						window px py :event gesture)))
				       (when presentation
					 (funcall presentation-release-function
						  presentation gesture wx wy)
					 (return-from process-gesture))))
				   (when button-release-function
				     (funcall button-release-function gesture wx wy))
				   (return-from process-gesture))))))
		      (beep))))))
	  (unhighlight)
	  (when last-window
	    (unhighlight-highlighted-presentation last-window)))))))


;;; Miscellaneous utilities

(defun pointer-place-rubber-band-line* (stream
					&optional (pointer (stream-primary-pointer stream))
					&key multiple-window)
  (let (start-x start-y end-x end-y)
    (tracking-pointer (stream :pointer pointer :multiple-window multiple-window)
      (:pointer-motion (window x y)
       (when (and start-x (eq window stream))
	 (with-output-recording-options (window :draw t :record nil)
	   (when end-x
	     (draw-line* window start-x start-y end-x end-y
			 :ink +flipping-ink+))
	   (setq end-x x end-y y)
	   (draw-line* window start-x start-y end-x end-y
		       :ink +flipping-ink+))))
      (:pointer-button-press (event)
       (let ((x (pointer-event-x event))
	     (y (pointer-event-y event))
	     (window (event-sheet event)))
	 (when (eq window stream)
	   (cond (start-x
		  (with-output-recording-options (window :draw t :record nil)
		    (draw-line* window start-x start-y end-x end-y
				:ink +flipping-ink+))
		  (draw-line* window start-x start-y end-x end-y)
		  (return-from pointer-place-rubber-band-line*
		    (values start-x start-y end-x end-y)))
		 (t (setq start-x x start-y y)))))))))

(defun pointer-input-rectangle* (&key left top right bottom
				      (stream *standard-input*)
				      (pointer (stream-primary-pointer stream)))
  (stream-pointer-input-rectangle* 
    stream pointer
    :left left :top top
    :right right :bottom bottom))

(defun pointer-input-rectangle (&key rectangle
				     (stream *standard-input*)
				     (pointer (stream-primary-pointer stream)))
  (let (left top right bottom)
    (when rectangle
      (multiple-value-setq (left top right bottom)
	(bounding-rectangle* rectangle)))
    (multiple-value-call
      #'make-bounding-rectangle
      (pointer-input-rectangle* :left left :top top
				:right right :bottom bottom
				:stream stream
				:pointer pointer))))

(defun portable-pointer-input-rectangle* (stream
					  &optional (pointer (stream-primary-pointer stream))
					  &key multiple-window)
  (let (left top right bottom
	(rectangle-drawn nil))
    (with-output-recording-options (stream :draw t :record nil)
      (tracking-pointer (stream :pointer pointer :multiple-window multiple-window)
	(:pointer-motion (window x y)
	 (when rectangle-drawn
	   (draw-rectangle* stream left top right bottom
			    :filled nil :ink +flipping-ink+)
	   (setq rectangle-drawn nil))
	 (when left
	   (when (eq window stream)
	     (setq right x bottom y)
	     (draw-rectangle* stream left top right bottom
			      :filled nil :ink +flipping-ink+)
	     (setq rectangle-drawn t))))
	(:pointer-button-press (event)
	 (if (eq (event-sheet event) stream)
	     (cond ((null left)
		    (setq left (pointer-event-x event))
		    (setq top (pointer-event-y event)))
		   (t
		    (when rectangle-drawn
		      (draw-rectangle* stream left top right bottom
				       :filled nil :ink +flipping-ink+)
		      (setq rectangle-drawn nil))
		      (return-from portable-pointer-input-rectangle*
			(values left top
				(pointer-event-x event) (pointer-event-y event)))))
	   (beep stream)))))))

