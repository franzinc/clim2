;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: drag-and-drop.lisp,v 1.6 92/11/06 18:59:26 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1991, 1992 Symbolics, Inc.  All rights reserved."


;;; Direct manipulation ("click and drag", "drag and drop") translators

(defclass drag-and-drop-translator (presentation-translator)
    ((destination-type :initarg :destination-type
		       :reader presentation-translator-destination-type)
     (feedback :initform nil :initarg :feedback
	       :reader presentation-translator-feedback)
     (highlighting :initform nil :initarg :highlighting
		   :reader presentation-translator-highlighting)
     ;; The body, tester, and documentation supplied by the programmer.
     ;; DEFINE-DRAG-AND-DROP-TRANSLATOR uses the other body, tester,
     ;; and documentaiton slots for its own purposes
     (real-body :initarg :real-body
		:reader presentation-translator-real-body)
     (real-tester :initform nil :initarg :real-tester
		  :reader presentation-translator-real-tester)
     (real-documentation :initform nil :initarg :real-documentation
			 :reader presentation-translator-real-documentation)))

(defmethod print-object ((object drag-and-drop-translator) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
	(with-slots (name from-type destination-type to-type) object
	  (format stream "~S (~S x ~S -> ~S)"
	    name from-type destination-type to-type)))
      (print-unreadable-object (object stream :type t :identity t)
	(with-slots (name from-type to-type) object
	  (format stream "~S" name)))))

(defmacro define-drag-and-drop-translator
	  (name
	   (from-type to-type destination-type command-table
	    &key (gesture ':select) tester
		 documentation (menu t) priority
		 feedback highlighting)
	   arglist
	   &body body)
  #+Genera (declare (zwei:indentation 1 3 3 1))
  (with-warnings-for-definition name define-presentation-translator
    (multiple-value-bind (body-function body-name)
	(write-translator-function (cons arglist body) name 'real-body
				   '(destination-object destination-presentation))
      (multiple-value-bind (tester-function tester-name)
	  (when tester
	    (write-translator-function tester name 'real-tester
				       '(destination-object destination-presentation)))
	(multiple-value-bind (doc-function doc-name)
	    (if (stringp documentation)
		(values nil documentation)
	        (when documentation
		  (write-translator-function 
		    documentation name 'real-doc
		    '(destination-object destination-presentation stream))))
	  (multiple-value-bind (feedback-function feedback-name)
	      (if (symbolp feedback)
		  (values nil (or feedback 'frame-drag-and-drop-feedback))
		  (write-drag-and-drop-function 
		    feedback name 'feedback
		    '(frame presentation stream initial-x initial-y new-x new-y state)))
	    (multiple-value-bind (highlighting-function highlighting-name)
		(if (symbolp highlighting)
		    (values nil (or highlighting 'frame-drag-and-drop-highlighting))
		    (write-drag-and-drop-function
		      highlighting name 'highlighting
		      '(frame presentation stream state)))
	      `(progn
		 (define-presentation-translator-1 ,name
		     (,from-type ,to-type ,command-table
		      :gesture ,gesture
		      :tester nil
		      :tester-definitive t
		      :documentation "Drag presentation"
		      :pointer-documentation "Drag presentation"
		      :menu ,menu
		      :priority ,priority
		      :translator-class drag-and-drop-translator
		      :destination-type ',destination-type
		      :real-body ',body-name
		      :feedback ',feedback-name
		      :highlighting ',highlighting-name
		      ,@(when tester `(:real-tester ',tester-name))
		      ,@(when documentation `(:real-documentation ',doc-name)))
		     ,*translator-function-arglist*
		   (invoke-drag-and-drop-translator ,@*translator-function-arglist*))
		 ,body-function
		 ,tester-function
		 ,doc-function
		 ,feedback-function
		 ,highlighting-function))))))))

(defun write-drag-and-drop-function (function translator-name clause-name args)
  (let ((function-name (gensymbol translator-name clause-name))
	(arglist (first function))
	(body (rest function)))
    (multiple-value-bind (arglist ignores)
	(canonicalize-and-match-lambda-lists args arglist)
      (values `(defun ,function-name ,arglist
		 ,@(and ignores `((declare (ignore ,@ignores))))
		 ,@body)
	      function-name))))

(defparameter *drag-and-drop-finish-on-release* nil)

;; All the drag-and-drop translators that satisfy the current from-type,
;; to-type, and gesture will get here.  However, the translator now being
;; run is not necessarily the right one.  What we do here is to track the 
;; pointer until the destination presentation has been reached, choose the
;; correct translator and run it.
(defun invoke-drag-and-drop-translator (object presentation context-type
					frame event window x y
					&optional (finish-on-release
						    *drag-and-drop-finish-on-release*))
  (let* ((command-table (frame-command-table frame))
	 (translators
	   (let ((all-translators
		   (find-presentation-translators
		     (presentation-type presentation) context-type command-table)))
	     ;; This find all translators that could conceivably apply
	     (mapcan #'(lambda (translator)
			 (when (and (typep translator 'drag-and-drop-translator)
				    (test-presentation-translator
				      translator presentation context-type
				      frame window x y :event event))
			   (list translator)))
		     all-translators)))
	 (destination-types
	   ;; So we can let TRACKING-POINTER do the work of finding applicable
	   ;; destination presentations...
	   (mapcar #'presentation-translator-destination-type translators))
	 ;; Get the feedback and highlighting functions
	 ;;--- This should really check to make sure that the feedback and
	 ;;--- highlighting functions are the same for the set of translators
	 (feedback
	   (and translators (presentation-translator-feedback (first translators))))
	 (highlighting
	   (and translators (presentation-translator-highlighting (first translators))))
	 (initial-x x)
	 (initial-y y)
	 (last-x nil)
	 (last-y nil)
	 (last-window nil)
	 (from-presentation presentation)
	 ;; The presentation we finally landed on, and the event used
	 (destination nil)
	 (destination-event nil)
	 ;; Defeat the normal gesture processing stuff
	 (*input-wait-handler* nil)
	 (*input-wait-test* nil)
	 (*pointer-button-press-handler* nil))
    (read-gesture :stream window :timeout 0)	;eat the initial mouse click
    (unhighlight-highlighted-presentation window)
    (when (null translators)
      (return-from invoke-drag-and-drop-translator nil))
    (flet ((find-translator (destination-presentation window x y)
	     (let* ((translator
		      (find (presentation-type destination-presentation) translators
			    :key #'presentation-translator-destination-type
			    :test #'presentation-subtypep))
		    (tester
		      (and translator (presentation-translator-real-tester translator))))
	       (when (or (null tester)
			 (funcall tester
				  object presentation context-type
				  frame event window x y
				  (presentation-object destination-presentation)
				  destination-presentation))
		 translator))))
      (declare (dynamic-extent #'find-translator))
      (macrolet ((feedback (window x y state)
		   `(funcall feedback frame from-presentation ,window 
				      initial-x initial-y ,x ,y ,state))
		 (highlight (presentation window state)
		   `(when ,presentation
		      (funcall highlighting frame ,presentation ,window ,state))))
	(block drag-presentation
	  (tracking-pointer (window :context-type `((or ,@destination-types))
				    :multiple-window t :highlight nil)
	    (:presentation-button-press (presentation event)
	     (unless finish-on-release
	       (highlight destination last-window :unhighlight)
	       (setq destination presentation
		     destination-event event
		     last-window (event-sheet event)
		     last-x (pointer-event-x event)
		     last-y (pointer-event-y event))
	       (return-from drag-presentation)))
	    (:pointer-button-press (event)
	     (unless finish-on-release
	       (highlight destination last-window :unhighlight)
	       (setq destination nil
		     destination-event event
		     last-window (event-sheet event)
		     last-x (pointer-event-x event)
		     last-y (pointer-event-y event))
	       (return-from drag-presentation)))
	    (:presentation-button-release (presentation event)
	     (when finish-on-release
	       (highlight destination last-window :unhighlight)
	       (setq destination presentation
		     destination-event event
		     last-window (event-sheet event)
		     last-x (pointer-event-x event)
		     last-y (pointer-event-y event))
	       (return-from drag-presentation)))
	    (:pointer-button-release (event)
	     (when finish-on-release
	       (highlight destination last-window :unhighlight)
	       (setq destination nil
		     destination-event event
		     last-window (event-sheet event)
		     last-x (pointer-event-x event)
		     last-y (pointer-event-y event))
	       (return-from drag-presentation)))
	    (:presentation (presentation window x y)
	     (when last-x
	       (feedback last-window last-x last-y :unhighlight))
	     (setq last-x x
		   last-y y
		   last-window window)
	     (feedback window x y :highlight)
	     (when (not (eq presentation destination))
	       (highlight destination last-window :unhighlight)
	       (setq destination presentation)
	       (highlight presentation window :highlight)
	       #+++ignore				;--- documentation
	       (let ((translator (find-translator destination window x y)))
		 (when translator
		   (let ((documentation
			   (presentation-translator-real-documentation translator)))
		     (if (stringp documentation)
			 (write-string documentation doc-stream)
		         (funcall documentation 
				  (presentation-object presentation) presentation context-type
				  frame event window x y
				  (presentation-object destination) destination
				  doc-stream)))))))
	    (:pointer-motion (window x y)
			     (when (typep window 'clim-stream-sheet)
			       (highlight destination last-window :unhighlight)
			       (setq destination nil)
			       (when last-x
				 (feedback last-window last-x last-y :unhighlight))
			       (setq last-x x
				     last-y y
				     last-window window)
			       (feedback window x y :highlight)))))
	(when last-x
	  (feedback last-window last-x last-y :unhighlight)))
      ;; The user has put down the presentation, figure out what to do
      ;;--- What if there is more than one translator?
      (unless destination (setq destination *null-presentation*))
      (let ((translator (find-translator destination last-window last-x last-y)))
	(when translator
	  (multiple-value-bind (result-object result-type options)
	      (funcall (presentation-translator-real-body translator)
		       object presentation context-type
		       frame event last-window last-x last-y
		       (presentation-object destination) destination)
	    (setq result-type (or result-type (evacuate-list context-type)))
	    ;; Find the tag to throw to, and do so
	    (dolist (this-context *input-context*)
	      (let* ((context-type (input-context-type this-context))
		     (tag (input-context-tag this-context)))
		(when (presentation-subtypep result-type context-type)
		  (throw tag
		    (values result-object
			    result-type
			    (or destination-event event)
			    options)))))))))))

;; NEW-X and NEW-Y are in stream coordinates.
(defmethod frame-drag-and-drop-feedback 
	   ((frame standard-application-frame) presentation stream
	    initial-x initial-y new-x new-y state)
  (declare (ignore initial-x initial-y state))	;we'll just use XOR
  (multiple-value-bind (width height) (bounding-rectangle-size presentation)
    (with-output-recording-options (stream :record nil)
      (with-identity-transformation (stream)
	(draw-rectangle* stream new-x new-y (+ new-x width) (+ new-y height)
			 :filled nil :ink +flipping-ink+ :line-dashes #(4 4))))))

(defmethod frame-drag-and-drop-highlighting 
	   ((frame standard-application-frame) presentation stream state)
  (ecase state
    (:highlight
      (set-highlighted-presentation stream presentation))
    (:unhighlight
      (unhighlight-highlighted-presentation stream))))

;; Used to extract all presentations of a certain type in a "surrounded region"
#+++ignore
(defun presentations-of-type-in-region (type stream region &optional (sort t))
  (let ((collection nil))
    (labels ((collect (record x-offset y-offset)
	       (if (presentationp record)
		   (when (presentation-typep (presentation-object record) type)
		     (pushnew record collection))
		 (multiple-value-bind (xoff yoff)
		     (output-record-start-cursor-position record)
		   (map-over-output-records-overlapping-region #'collect
		     record region (+ x-offset xoff) (+ y-offset yoff))))))
      (declare (dynamic-extent #'collect))
      (collect (stream-output-history stream) 0 0)
      (when (and collection sort)
	(setq collection (sort-presentations-by-position collection)))
      collection)))

#+++ignore
(defun sort-presentations-by-position (presentations)
  (flet ((position-lessp (p1 p2)
	   (unless (eq (output-record-parent p1) (output-record-parent p2))
	     ;;--- Convert to absolute coordinates if they don't share parents
	     )
	   (multiple-value-bind (x1 y1) (bounding-rectangle-position p1)
	     (multiple-value-bind (x2 y2) (bounding-rectangle-position p2)
	       (if (= y1 y2)
		   (< x1 x2)
		   (< y1 y2))))))
    (declare (dynamic-extent #'position-lessp))
    (sort presentations #'position-lessp)))

