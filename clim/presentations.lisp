;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(define-protocol-class presentation ())

(defclass standard-presentation
          (standard-sequence-output-record presentation)
     ((object :accessor presentation-object :initarg :object)
      (type :accessor presentation-type :initarg :type)
      (single-box :accessor presentation-single-box :initarg :single-box))
  (:default-initargs :size 5))

(define-output-record-constructor standard-presentation
                                  (&key object type modifier single-box
                                        x-position y-position (size 5))
  :object object :type type :modifier modifier :single-box single-box 
  :x-position x-position :y-position y-position :size size)

(defmethod print-object ((object standard-presentation) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S /x ~A:~A y ~A:~A/"
      (safe-slot-value object 'type)
      (if (eq object (safe-slot-value object 'object))
          '|circular presentation|
          (safe-slot-value object 'object))
      (safe-slot-value object 'left)
      (safe-slot-value object 'right)
      (safe-slot-value object 'top)
      (safe-slot-value object 'bottom))))

(defmethod shared-initialize :after ((presentation standard-presentation) slots
                                     &key modifier single-box)
  ;;--- Support :MODIFIER sometime...
  (declare (ignore slots modifier))
  (check-type single-box (member t nil :highlighting :position))
  (with-slots (type) presentation
    (setq type (evacuate-list type)))
  nil)

(defclass standard-nonsensitive-presentation
          (standard-sequence-output-record)
    ()
  (:default-initargs :size 5))

(define-output-record-constructor standard-nonsensitive-presentation
                                  (&key object type modifier single-box
                                        x-position y-position (size 5))
  :x-position x-position :y-position y-position :size size)

(defmethod shared-initialize :after ((presentation standard-nonsensitive-presentation) slots
                                     &key modifier single-box)
  ;;--- Support :MODIFIER sometime...
  (declare (ignore slots modifier single-box))
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
                                 &rest initargs &key object type &allow-other-keys)
  #-aclpc (declare (ignore initargs))
  (and (eql (presentation-object record) object)
       (eq (presentation-type record) type)))

(defmethod highlight-output-record ((record output-record-element-mixin) stream state)
  ;; State is :HIGHLIGHT or :UNHIGHLIGHT
  (declare (ignore state))
  (multiple-value-bind (xoff yoff)
      (convert-from-relative-to-absolute-coordinates stream (output-record-parent record))
    (declare (type coordinate xoff yoff))
    (with-bounding-rectangle* (left top right bottom) record
      (draw-rectangle-internal stream xoff yoff
                               ;; Offset the left/top, but not the right/bottom
                               ;; because of the rules CLIM uses for rasterizing
                               ;; unfilled rectangles.  Bounding rectangles for
                               ;; graphical objects will already be a bit big
                               ;; on the lower right anyway.
                               (1- left) (1- top) right bottom
                               +flipping-ink+ +highlighting-line-style+))))

(defmethod highlight-output-record :around ((record standard-presentation) stream state)
  (let ((single-box (presentation-single-box record)))
    (if (or (eq single-box t) 
            (eq single-box :highlighting))
        (call-next-method)
      ;; Handles :SINGLE-BOX :POSITION
      (labels ((highlight (child x-offset y-offset)
                 (declare (type coordinate x-offset y-offset))
                 (when (displayed-output-record-p child)
                   (if (eq record child)
                       (call-next-method)        ;avoid infinite recursion
                       (highlight-output-record child stream state)))
                 (multiple-value-bind (xoff yoff) (output-record-position child)
                   (declare (type coordinate xoff yoff))
                   (map-over-output-records
                     #'highlight child (coordinate 0) (coordinate 0)
                     (+ xoff x-offset) (+ yoff y-offset)))))
        (declare (dynamic-extent #'highlight))
        (highlight record (coordinate 0) (coordinate 0))))))


(define-presentation-type blank-area ())

(define-presentation-method presentation-typep (object (type blank-area))
  (eq object *null-presentation*))

;;; This is the presentation you get if you click while not over anything...
(setq *null-presentation*
      (let ((null-presentation 
              (make-instance 'standard-presentation 
                :object nil :type 'blank-area :single-box nil)))
        (setf (presentation-object null-presentation) null-presentation)
        null-presentation))


(defun window-cohorts-p (window-one window-two)
  ;; The best test for whether or not two windows care about each
  ;; other's mouse motion (for now) is whether they share an IO buffer.
  (eq (sheet-event-queue window-one)
      (sheet-event-queue window-two)))

(defun highlight-presentation-of-context-type (stream)
  (highlight-applicable-presentation
    ;; We really do mean to use *APPLICATION-FRAME*, because using
    ;; (PANE-FRAME STREAM) will return the wrong value when we have
    ;; an ACCEPT-VALUES dialog inside some pane in the frame.
    *application-frame* stream *input-context*))

(defun input-context-button-press-handler (stream button)
  (frame-input-context-button-press-handler 
    ;; We really do mean to use *APPLICATION-FRAME*, because using
    ;; (PANE-FRAME STREAM) will return the wrong value when we have
    ;; an ACCEPT-VALUES dialog inside some pane in the frame.
    *application-frame* stream button))

(defun invoke-with-input-context (type override body-continuation context-continuation)
  (declare (dynamic-extent body-continuation context-continuation)
	   (special *input-wait-handler*
		    *input-wait-test*
		    *pointer-button-press-handler*))
  ;; At one time, this used to canonicalize the presentation type by
  ;; calling WITH-PRESENTATION-TYPE-DECODED and then consing just the
  ;; type name and parameters.  That turns out not to be necessary any
  ;; more since everyplace else in CLIM is careful to decode the type.
  ;; Furthermore, it is necessary to include the type options in the
  ;; input context in case the options are needed to correctly present
  ;; an object using the context type as the presentation type.
  (with-stack-list (this-tag '#:context-tag type)        ;guaranteed to be unique
    (with-stack-list (this-context type this-tag)
      (let ((pwindow nil)
            (px 0)
            (py 0)
            (old-state 0))
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
                     (/= old-state
                         (setf old-state (window-modifier-state stream)))))))
          #-allegro (declare (dynamic-extent #'pointer-motion-pending))
          (let ((*input-wait-handler* #'highlight-presentation-of-context-type)
                (*input-wait-test* #'pointer-motion-pending)
                (*pointer-button-press-handler* #'input-context-button-press-handler))
            ;; Push the new context.  The most specific context is at the
            ;; beginning of *INPUT-CONTEXT, the least specific is at the end.
            (with-stack-list* (*input-context* this-context (unless override *input-context*))
              (block with-input-context
                (multiple-value-bind (object ptype event options)
                    (catch this-tag
                      (return-from invoke-with-input-context
                        (funcall body-continuation)))
                  (funcall context-continuation object ptype event options))))))))))

;; Given a presentation, return the next ancestor presentation that
;; has exactly the same bounding box.
(defun parent-presentation-with-shared-box (presentation stream)
  (with-bounding-rectangle* (left top right bottom) presentation
    (let ((parent (output-record-parent presentation)))
      (when parent                                ;we're done if it's null
        (multiple-value-bind (xoff yoff)
            (convert-from-relative-to-absolute-coordinates stream parent)
          (translate-coordinates xoff yoff left top right bottom)))
      (loop
        (cond ((or (null parent)
                   (with-bounding-rectangle* (pleft ptop pright pbottom) parent
                     (multiple-value-bind (xoff yoff)
                         (convert-from-relative-to-absolute-coordinates 
                           stream (output-record-parent parent))
                       (translate-coordinates xoff yoff pleft ptop pright pbottom))
                     (not (ltrb-equals-ltrb-p left top right bottom
                                              pleft ptop pright pbottom))))
               (return nil))
              ((presentationp parent)
               (return parent)))
        (setq parent (output-record-parent parent))))))

;; This is what happens when you click.  It does a type walk looking
;; for the innermost presentation that has an applicable translator,
;; and then executes that translator.  Note that we do a loop over
;; presentations that have the exact same bounding rectangle to be
;; sure the translator with the highest priority is chosen.
(defun throw-highlighted-presentation (presentation input-context button-press-event)
  (catch 'no-translation
    (let ((x (pointer-event-x button-press-event))
          (y (pointer-event-y button-press-event))
          (window (event-sheet button-press-event))
          (frame *application-frame*)
          (preferred-translator nil)
          (preferred-presentation nil)
          (preferred-context-type nil)
          (preferred-tag nil))
      (do ((presentation presentation
                         (parent-presentation-with-shared-box presentation window)))
          ((null presentation))
        (dolist (this-context input-context)
          (let* ((context-type (input-context-type this-context))
                 (tag (input-context-tag this-context)))
            (let ((translator (presentation-matches-context-type
                                presentation context-type
                                frame window x y 
                                :event button-press-event)))
              (when translator
                (when (or (null preferred-translator)
                          (> (presentation-translator-priority translator)
                             (presentation-translator-priority preferred-translator)))
                  (setq preferred-translator translator
                        preferred-presentation presentation
                        ;; Context type is sure to be stack-consed...
                        preferred-context-type (evacuate-list context-type)
                        preferred-tag tag)))))))
      (when preferred-translator
        (multiple-value-bind (translated-object translated-type options)
            ;; Suppose this is a presentation action that
            ;; pops up a window to do input...
            (let ((*original-stream* nil))
              ;; Eat the gesture that invoked the translator now, since it
              ;; might be an action that invokes a menu.  (In that case, the
              ;; the user can type <Abort>, which will cause the gesture
              ;; not to be eaten at all.)  Don't let the button press handler
              ;; run recursively, either.
              ;;--- Yuck, this just really turns my stomach.
              (read-gesture :stream window 
                            :pointer-button-press-handler nil
                            :timeout 0)
              (call-presentation-translator 
                preferred-translator preferred-presentation preferred-context-type
                frame button-press-event window x y))
	  ;; NLC/P&C- If there is no tag, just complain and exit.
	  ;; This can occur in certain unusual circumstances,
	  ;; for example, when clicking on a command-button or 
	  ;; menu-item while inside a drag which is itself the
	  ;; result of executing another command.
	  (cond (preferred-tag
		 (throw (or preferred-tag 'no-translation)
		   (values translated-object
			   (or translated-type preferred-context-type)
			   button-press-event
			   options)))
		(t 
		 (beep)
		 (throw 'no-translation nil))))))
    (unless (eq presentation *null-presentation*)
      (beep))))

(defun find-innermost-applicable-presentation
       (input-context stream x y
        &key (frame *application-frame*) 
             (modifier-state (window-modifier-state stream)) event)
  (let ((x (coordinate x))
        (y (coordinate y)))
    (declare (type coordinate x y))
    ;; Depth first search for a presentation that is both under the pointer and
    ;; matches the input context.
    ;; This relies on MAP-OVER-OUTPUT-RECORDS-CONTAINING-POSITION traversing
    ;; the most recently drawn of overlapping output records first.
    (labels 
      ((mapper (record presentations x-offset y-offset)
         (declare (type coordinate x-offset y-offset))
         ;; RECORD is an output record whose bounding rectangle contains (X,Y).
         ;; PRESENTATIONS is a list of non-:SINGLE-BOX presentations that are
         ;; ancestors of RECORD.
         ;; X-OFFSET and Y-OFFSET are the position on the drawing plane of the
         ;; origin of RECORD's coordinate system, i.e. RECORD's parent's start-position.
         (multiple-value-bind (sensitive superior-sensitive inferior-presentation)
             ;; SENSITIVE is true if RECORD is a presentation to test against the context.
             ;; SUPERIOR-SENSITIVE is true if PRESENTATIONS should be tested also.
             ;; INFERIOR-PRESENTATION is a presentation to pass down to our children.
             (if (presentationp record)
                 ;;--- This should call PRESENTATION-REFINED-POSITION-TEST
                 (if (output-record-refined-position-test 
                       record (- x x-offset) (- y y-offset))
                     ;; Passed user-defined sensitivity test for presentations.
                     ;; It might be both a presentation and a displayed output record.
                     ;; It might be sensitive now [:single-box t] or the decision might
                     ;; depend on finding a displayed output record [:single-box nil].
                     (let ((displayed (displayed-output-record-p record))
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
                              (displayed-output-record-p record)
                              ;; Call the refined position test for displayed
                              ;; output records (e.g., ellipses, text, etc.)
                              (output-record-refined-position-test
                                record (- x x-offset) (- y y-offset)))
                         nil))
  
           ;; Add INFERIOR-PRESENTATION to PRESENTATIONS
           (with-stack-list* (more-presentations inferior-presentation presentations)
             (when inferior-presentation
               (setq presentations more-presentations))
    
             ;; Depth-first recursion
             (multiple-value-bind (dx dy) (output-record-position record)
               (map-over-output-records-containing-position 
                 #'mapper record x y
                 (- x-offset) (- y-offset)
                 presentations (+ x-offset dx) (+ y-offset dy)))
    
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
             (multiple-value-bind (translator any-match-p)
                 (presentation-matches-context-type presentation context-type
                                                    frame stream x y
                                                    :event event
                                                    :modifier-state modifier-state)
               (declare (ignore translator))
               (when any-match-p
                 (return-from find-innermost-applicable-presentation
                   presentation)))))))
      (declare (dynamic-extent #'mapper #'test))
      (mapper (stream-output-history stream) nil (coordinate 0) (coordinate 0)))))

(defvar *last-pointer-documentation-modifier-state* 0)
(defparameter *document-blank-area-translators* t)

(defun highlight-applicable-presentation (frame stream input-context
                                          &optional (prefer-pointer-window t))
  (let ((history-window (if prefer-pointer-window
                            (find-appropriate-window stream)
                            stream)))
    ;; If non-NIL, guaranteed to have OUTPUT-RECORDING-MIXIN.
    (when history-window
      (with-slots (highlighted-presentation) history-window
        (multiple-value-bind (px py)
            (stream-pointer-position history-window)
          (let ((presentation (frame-find-innermost-applicable-presentation
                                frame input-context history-window px py)))
            (when (and (null presentation) 
                       *document-blank-area-translators*)
              ;; Generate documentation for blank area, too
              (setq presentation *null-presentation*))
            ;; Does this cover all the cases?
            (cond ((and (null presentation)
                        (null highlighted-presentation))
                   ;; Nothing highlighted, over nothing
                   )
                  ((null presentation)
                   ;; Over nothing now, was over something before.
                   ;; Unhighlight.
                   (highlight-presentation 
                     highlighted-presentation (presentation-type highlighted-presentation)
                     history-window :unhighlight)
                   (setf highlighted-presentation nil)
                   (frame-document-highlighted-presentation
                     frame highlighted-presentation
                     input-context history-window px py
                     *pointer-documentation-output*))
                  ((eq presentation highlighted-presentation)
                   ;; Over the same presentation as before
                   (when (/= (window-modifier-state history-window)
                             *last-pointer-documentation-modifier-state*)
                     (frame-document-highlighted-presentation
                       frame presentation
                       input-context history-window px py
                       *pointer-documentation-output*))
                   nil)
                  (t
                   ;; Over something now, might have been over something before
                   (when highlighted-presentation
                     (highlight-presentation 
                       highlighted-presentation (presentation-type highlighted-presentation)
                       history-window :unhighlight))
                   (highlight-presentation 
                     presentation (presentation-type presentation)
                     history-window :highlight)
                   (setf highlighted-presentation presentation)
                   (frame-document-highlighted-presentation
                     frame presentation
                     input-context history-window px py
                     *pointer-documentation-output*)))))))))

(defun highlighted-presentation (stream &optional (prefer-pointer-window t))
  (let ((history-window (if prefer-pointer-window (find-appropriate-window stream) stream)))
    (when history-window
      (slot-value history-window 'highlighted-presentation))))

(defun set-highlighted-presentation (stream presentation &optional (prefer-pointer-window t))
  (let ((history-window (if prefer-pointer-window (find-appropriate-window stream) stream)))
    (when history-window
      (with-slots (highlighted-presentation) history-window
        (cond ((eq highlighted-presentation presentation)
               ;; It's already highlighted, do nothing
               )
              (t
               (when highlighted-presentation
                 ;; Unhighlight old presentation
                 (highlight-presentation 
                   highlighted-presentation (presentation-type highlighted-presentation)
                   history-window :unhighlight))
               (setf highlighted-presentation presentation)
               (when presentation
                 (highlight-presentation 
                   presentation (presentation-type presentation)
                   history-window :highlight))))))))

(defun unhighlight-highlighted-presentation (stream &optional (prefer-pointer-window t))
  (let ((history-window (if prefer-pointer-window (find-appropriate-window stream) stream)))
    (when history-window
      (with-slots (highlighted-presentation) history-window
        (when highlighted-presentation
          (highlight-presentation 
            highlighted-presentation (presentation-type highlighted-presentation)
            history-window :unhighlight)
          (setf highlighted-presentation nil))))))

(defun find-appropriate-window (stream)
  ;;--- How do we hack multiple pointers?
  (when (extended-input-stream-p stream)
    (let* ((pointer (stream-primary-pointer stream))
           (window (pointer-sheet pointer)))
      ;; It ain't no good if it doesn't have a history.
      (when (and window
                 (port window)
                 (output-recording-stream-p window))
        window))))
