;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1991, 1992 Symbolics, Inc.  All rights reserved."


;;; Direct manipulation ("click and drag", "drag and drop") translators

(defparameter *drag-and-drop-finish-on-release* t)

(defclass drag-and-drop-translator (presentation-translator)
    ((destination-type :initarg :destination-type
                       :reader presentation-translator-destination-type)
     (feedback :initform nil :initarg :feedback
               :reader presentation-translator-feedback)
     (highlighting :initform nil :initarg :highlighting
                   :reader presentation-translator-highlighting)
     (pointer-cursor :initform nil :initarg :pointer-cursor
		     :reader presentation-translator-pointer-cursor)
     (finish-on-release :initarg :finish-on-release
			:reader presentation-translator-finish-on-release)
     (destination-tester :initform nil :initarg :destination-tester
                  :reader presentation-translator-destination-tester)
     (drag-documentation :initform nil :initarg :drag-documentation
                         :reader presentation-translator-drag-documentation)
     ;; The body supplied by the programmer.
     ;; DEFINE-DRAG-AND-DROP-TRANSLATOR uses the other body slot for
     ;; its own purposes.
     (real-body :initarg :real-body
                :reader presentation-translator-real-body)))

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
	    &key (gesture ':select) tester destination-tester
		 documentation pointer-documentation
		 drag-documentation (menu t)
		 priority
		 (feedback 'frame-drag-and-drop-feedback)
		 (highlighting 'frame-drag-and-drop-highlighting)
		 pointer-cursor (finish-on-release *drag-and-drop-finish-on-release*))
	   arglist
	   &body body)
  #+Genera (declare (zwei:indentation 1 3 3 1))
  (with-warnings-for-definition name define-presentation-translator
    (flet ((translator-function-and-name (clause clause-name
                                          &optional args
                                                    (use-default-args t)
                                                    string-ok)
             (when clause
               (cond ((or (functionp clause)
                          (symbolp clause)
                          (and string-ok (stringp clause)))
                      (values nil clause))
                     (t (write-translator-function clause name clause-name args
                                                   use-default-args))))))
      (multiple-value-bind (body-function body-name)
	  (write-translator-function (cons arglist body) name 'real-body
				     '(destination-object destination-presentation))
	(multiple-value-bind (destination-tester-function destination-tester-name)
	    (translator-function-and-name
	     destination-tester 'destination-tester
	     '(destination-object destination-presentation))
	  (multiple-value-bind (drag-documentation-function drag-documentation-name)
	      (translator-function-and-name
	       drag-documentation 'drag-documenation
	       '(destination-object destination-presentation stream) t t)
	    (multiple-value-bind (feedback-function feedback-name)
		(translator-function-and-name
		 feedback 'feedback
		 '(frame presentation stream initial-x initial-y new-x new-y state) nil)
	      (multiple-value-bind (highlighting-function highlighting-name)
		(translator-function-and-name
		 highlighting 'highlighting
		 '(frame presentation stream state) nil)
		`(progn
		   (define-presentation-translator-1 ,name
		       (,from-type ,to-type ,command-table
				   :gesture ,gesture
				   :tester ,tester
				   :tester-definitive t
				   :documentation ,(or documentation
						       (format nil "~a" name))
				   :pointer-documentation ,pointer-documentation
				   :menu ,menu
				   :priority ,priority
				   :translator-class drag-and-drop-translator
				   :destination-type ',destination-type
				   :real-body ',body-name
				   :feedback ',feedback-name
				   :highlighting ',highlighting-name
				   :pointer-cursor ',pointer-cursor
				   :finish-on-release ',finish-on-release
				   :destination-tester ',destination-tester-name
				   :drag-documentation ',drag-documentation-name)
		     ,*translator-function-arglist*
		     (invoke-drag-and-drop-translator ,@*translator-function-arglist*))
		   ,body-function
		   ,destination-tester-function
		   ,drag-documentation-function
		   ,feedback-function
		   ,highlighting-function)))))))))

;; All the drag-and-drop translators that satisfy the current from-type,
;; to-type, and gesture will get here.  However, the translator now being
;; run is not necessarily the right one.  What we do here is to track the
;; pointer until the destination presentation has been reached, choose the
;; correct translator and run it.

(defun invoke-drag-and-drop-translator (object presentation context-type
                                        frame event window x y)

  (catch-abort-gestures ("Abort drag and drop")
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
	   ;;--- highlighting functions and the pointer cursor are the same for
	   ;;--- the related set of translators
	   (feedback
	    (and translators (presentation-translator-feedback (first translators))))
	   (highlighting
	    (and translators (presentation-translator-highlighting (first translators))))
	   (pointer-cursor
	    (or (and translators (presentation-translator-pointer-cursor (first translators)))
		:move))
	   (finish-on-release
	    (and translators (presentation-translator-finish-on-release (first translators))))
	   ;;--- spr15619: needed this when we were calling (setf pointer-cursor)
	   ;;--- to change the mouse pointer to :move during the drag;
	   ;;--- now we use sheet-pointer-cursor.
	   #+ignore (pointer (port-pointer (port window)))
	   #+ignore (old-pointer-cursor (pointer-cursor pointer))
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
      (read-gesture :stream window :timeout 0) ;eat the initial mouse click
      (when (output-recording-stream-p window)
        (unhighlight-highlighted-presentation window))
      (when (null translators)
        (return-from invoke-drag-and-drop-translator nil))
      (flet ((find-translator (destination-presentation window x y)
	       (let* ((translator
		       (find (presentation-type destination-presentation) translators
			     :key #'presentation-translator-destination-type
			     :test #'presentation-subtypep-1))
		      (tester
		       (and translator (presentation-translator-destination-tester translator))))
		 (when (or (null tester)
			   (funcall tester
				    object presentation context-type
				    frame event window x y
				    (presentation-object destination-presentation)
				    destination-presentation))
		   translator))))
	(declare (dynamic-extent #'find-translator))
	;;--- when drop initiated an accepting-values, (setf pointer-cursor)
	;;--- was losing, ie changing the cursor back to default on the wrong frame
	(letf-globally ((#+ignore (pointer-cursor-override pointer)
			 ;; why doesn't this work?
			 #+ignore (sheet-pointer-cursor window)
			 ;; calling sheet-pointer-cursor on the top-level
			 ;; sheet is not quite right, since we only want to
			 ;; change this for the appropriate pane, not the
			 ;; whole frame, but this at least ensures that the
			 ;; pointer cursor will always revert to its
			 ;; previous shape.
			 ;;       1-24-97 tjm (w/colin) for spr15619
			 #-ignore (sheet-pointer-cursor 
				   (frame-top-level-sheet frame))
			 pointer-cursor))
	  #+ignore (setf (pointer-cursor pointer) pointer-cursor)
	  (macrolet ((feedback (window x y state)
		       `(funcall feedback frame from-presentation ,window
				 initial-x initial-y ,x ,y ,state))
		     (highlight (presentation window state)
		       `(when (and ,presentation
				   (output-recording-stream-p ,window))
			  (funcall highlighting frame ,presentation ,window ,state))))
	    (block drag-presentation
	      ;; :multiple-window t below was changed by colin (to nil) in
	      ;; post-4.3 sources, since he had promised this to a customer
	      ;; as a fix for something (spr?) but this breaks
	      ;; functionality which is needed by other customers.
	      ;;  cf spr15818    7feb97 --tjm 
	      (tracking-pointer (window :context-type `((or ,@destination-types))
					:multiple-window t :highlight nil)
		(:presentation-button-press (presentation event)
		 (unless finish-on-release
		   (setq destination-event event
			 destination presentation)
		   (return-from drag-presentation)))

		(:pointer-button-press (event)
		 (unless finish-on-release
		   (setq destination-event event
			 destination nil)
		   (return-from drag-presentation)))

		(:presentation-button-release (presentation event)
		 (when finish-on-release
		   (setq destination-event event
			 destination presentation)
		   (return-from drag-presentation)))

		(:pointer-button-release (event)
		 (when finish-on-release
		   (setq destination-event event
			 destination nil)
		   (return-from drag-presentation)))

		(:presentation (presentation window x y)
		 (when last-x
		   (feedback last-window last-x last-y :unhighlight))
		 (feedback window x y :highlight)
		 (when (not (eq presentation destination))
		   (highlight destination last-window :unhighlight)
		   (let ((translator (find-translator presentation window x y)))
		     (setq destination (and translator presentation))
		     (highlight destination window :highlight)
		     (document-drag-and-drop-translator translator
							from-presentation destination
							context-type
							frame event
							window x y)))
		 (setq last-x x last-y y last-window window))

		(:pointer-motion (window x y)
		 (when (typep window 'clim-stream-sheet)
		   (when last-x
		     (feedback last-window last-x last-y :unhighlight))
		   (feedback window x y :highlight)
		   (highlight destination last-window :unhighlight)
		   (setq destination nil)
		   (let ((translator (find-translator presentation window x y)))
		     (document-drag-and-drop-translator translator
							from-presentation destination
							context-type
							frame event
							window x y))
		   (setq last-x x last-y y last-window window)))))

	    (highlight destination last-window :unhighlight)
	    (when last-x
	      (feedback last-window last-x last-y :unhighlight)))
	  ;; see above "now we use ..."
	  #+ignore (setf (pointer-cursor pointer) old-pointer-cursor))
	;; The user has put down the presentation, figure out what to do
	;;--- What if there is more than one translator?
	(unless destination
	  ;; No destination presentation?  Then use the null presentation
	  (setq destination *null-presentation*))
	(let* ((window (event-sheet destination-event))
	       (x (pointer-event-x destination-event))
	       (y (pointer-event-y destination-event))
	       (translator (find-translator destination window x y)))
	  (when translator
	    (multiple-value-bind (result-object result-type options)
		(funcall (presentation-translator-real-body translator)
			 object presentation context-type
			 frame event window x y
			 (presentation-object destination) destination)
	      (setq result-type (or result-type (evacuate-list context-type)))
	      ;; Find the tag to throw to, and do so
	      (dolist (this-context *input-context*)
		(let* ((context-type (input-context-type this-context))
		       (tag (input-context-tag this-context)))
		  (when (presentation-subtypep-1 result-type context-type)
		    (throw tag
		      (values result-object
			      result-type
			      (or destination-event event)
			      options)))))))))))
  (throw 'no-translation nil))

;; NEW-X and NEW-Y are in stream coordinates.
(defmethod frame-drag-and-drop-feedback
           ((frame standard-application-frame) presentation stream
            initial-x initial-y new-x new-y state)
  (declare (optimize (speed 3) (safety 0) #-aclpc (debug 0)))
  (declare (ignore state))        ;we'll just use XOR
  (multiple-value-bind (xoff yoff)
      (convert-from-relative-to-absolute-coordinates
        stream (output-record-parent presentation))
    (with-bounding-rectangle* (left top right bottom) presentation
      (let ((width (- right left))
            (height (- bottom top))
            (dx (- (+ left xoff) initial-x))
            (dy (- (+ top yoff) initial-y)))
        (with-output-recording-options (stream :record nil)
          (with-identity-transformation (stream)
            (let ((x (+ new-x dx))
                  (y (+ new-y dy)))
              (draw-rectangle* stream x y (+ x width) (+ y height)
                               :filled nil :ink +flipping-ink+ :line-dashes #(4 4)))))))))

(defmethod frame-drag-and-drop-highlighting
           ((frame standard-application-frame) presentation stream state)
  (ecase state
    (:highlight
      (set-highlighted-presentation stream presentation))
    (:unhighlight
      (unhighlight-highlighted-presentation stream))))

;; consider merging this with document-presentation-translator
;; specifying :documentation-type as :drag-documentation (cim 4/3/96)

(defun document-drag-and-drop-translator
    (translator from-presentation destination-presentation
     context-type frame event window x y
     &key (stream (frame-pointer-documentation-output frame)))
  (when stream
    (window-clear stream)
    (when translator
      (with-output-recording-options (stream :record nil)
        (with-end-of-line-action (stream :allow)
          (with-end-of-page-action (stream :allow)
            (let ((documentation
                   (presentation-translator-drag-documentation translator)))
              (when documentation
                (if (stringp documentation)
                    (write-string documentation stream)
                  (funcall documentation
                           (presentation-object from-presentation)
                           from-presentation context-type
                           frame event window x y
                           (presentation-object destination-presentation)
                           destination-presentation
                           stream))))))))
    (force-output stream)))

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

