;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(eval-when (compile load eval)

;;; For compile-time checking.
(defvar *tracking-pointer-clauses* nil)                ;each clause looks like (name arglist)

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
(register-tracking-pointer-clause :timeout ())

(defvar *tracking-pointer-presentation-clauses*
        '(:presentation :presentation-button-press
          :presentation-button-release))

)        ;eval-when

(defmacro tracking-pointer ((&optional stream
                             &key pointer multiple-window transformp
                                  (context-type t) (highlight nil
                                                    highlight-p) timeout)
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
                                      :highlight ,do-highlighting
                                      :timeout ,timeout))))))

(defun tracking-pointer-1 (stream pointer generator
                           &key multiple-window transformp
                           context-type highlight timeout)
  (unless pointer
    (setq pointer (stream-primary-pointer stream)))
  (let* ((moved-p nil)
         (current-window stream)
         (last-window nil)
         (last-x nil) (last-y nil)
         (input-happened nil)
         (timed-out nil)
	 ;; Grab these up front.  The closures will share the same environment.
         (motion-function (funcall generator :pointer-motion))
         (button-press-function (funcall generator :pointer-button-press))
         (button-release-function (funcall generator :pointer-button-release))
         (presentation-motion-function (funcall generator :presentation))
         (presentation-press-function (funcall generator :presentation-button-press))
         (presentation-release-function (funcall generator :presentation-button-release))
         (keyboard-function (funcall generator :keyboard))
         (timeout-function (funcall generator :timeout))
	 ;; Tell the input streams not to discard pointer release events
         (*discard-pointer-release-events* nil)
         (highlighted-presentation nil)
         (highlighted-presentation-type nil)
         (highlighted-presentation-window nil)
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
                          highlighted-presentation-type (presentation-type ,presentation)
                          highlighted-presentation-window current-window)
                    (when (output-recording-stream-p current-window)
                      (highlight-presentation
                       highlighted-presentation highlighted-presentation-type
                       highlighted-presentation-window :highlight))))
               (unhighlight ()
                 `(when highlighted-presentation
                    (when (output-recording-stream-p current-window)
                      (highlight-presentation
                       highlighted-presentation highlighted-presentation-type
                       highlighted-presentation-window :unhighlight))
                    (setq highlighted-presentation nil)))
               (find-presentation (window x y &key event)
                 `(if (eq context-type t)
                      (find-innermost-presentation ,window ,x ,y)
                      (frame-find-innermost-applicable-presentation
                       *application-frame* *input-context*
                       ,window ,x ,y ,@(and event `(:event ,event))))))

      ;; 18-Mar-04  mm: move the flet out of the loop and out of the 
      ;;                unwind-protect
      (flet ((pointer-motion-pending (window &aux m-p)
	       (when (and (or multiple-window
			      (eq current-window (pointer-sheet pointer)))
			  (eq (stream-primary-pointer window) pointer))
		 (multiple-value-setq (m-p last-window last-x last-y)
                     (pointer-state-changed pointer last-window last-x last-y))
		 ;; bug14162:  the variable moved-p should only be set to non-nil
		 ;;  in the wait function - it is reset to nil only in the body
		 ;;  because the wait function may be called several times 
		 ;;  before the body notices
		 (when m-p (setf moved-p m-p))
		 (and moved-p last-window))))
	(unwind-protect
	    (progn			; for spr17056 by kreti
	      (when multiple-window
		(port-remove-all-pointer-grabs (port pointer)))
	      (loop
		;; Handle pointer motion
               (block handle-simple-motion
                 (when multiple-window
                   (setq current-window (or (pointer-sheet pointer) (graft pointer))))
                 (multiple-value-bind (x y) (sheet-pointer-position
                                             current-window pointer)
                   (when moved-p
                     ;; Position updates must happen regardless of
                     ;; presence of a :pointer-motion clause in the
                     ;; tracking-pointer form. (spr36006, afuchs/2009-08-11)
                     (setq moved-p nil)
                     (setq last-x x last-y y
                           last-window current-window)
                     (when (or motion-function presentation-motion-function highlight)
                       ;; Pointer position is in root (graft) coordinates
                       (when (or presentation-motion-function highlight)
                         (let ((presentation
                                (and (output-recording-stream-p current-window)
                                     (find-presentation current-window x y))))
                           (when presentation
                             (when highlight
                               (unless (eq presentation highlighted-presentation)
                                 (unhighlight)
                                 (highlight presentation)))
                             (when presentation-motion-function
                               (funcall presentation-motion-function
                                        presentation current-window x y))
                             (return-from handle-simple-motion))))
                       (unhighlight)
                       (when motion-function
                         (when transformp
                           (multiple-value-bind (tx ty)
                               (transform-position
                                (medium-transformation current-window) x y)
                             (setq x (coordinate tx)
                                   y (coordinate ty))))
                         (funcall motion-function current-window x y))))))
               (block input-wait
                 (loop
                   ;; Handle any clicks or characters, otherwise wait for something
                   ;; interesting to happen.
                  (when multiple-window
                    (setq current-window (or (pointer-sheet pointer) (graft pointer))))
                  (flet ()
                    #-allegro (declare (dynamic-extent #'pointer-motion-pending))
                    ;; It's OK to do the input wait on STREAM instead of on
                    ;; CURRENT-WINDOW, because multiple-window mode only works
                    ;; when all the windows share the same I/O buffer.  Same
                    ;; deal with the READ-GESTURE below, too.
                    ;;--- It's a bug that multiple-window has that restriction...
                    (multiple-value-setq (input-happened timed-out)
                        (stream-input-wait stream
                                           :timeout timeout
                                           :input-wait-test #'pointer-motion-pending)))
                  ;; Don't call READ-GESTURE if there is no gesture to read
                  ;;--- If we called READ-GESTURE on CURRENT-WINDOW, we would
                  ;;--- get the (possibly undesirable) side-effect that the
                  ;;--- "input focus" will follow the mouse, having particularly
                  ;;--- weird behavior regarding keystrokes.
                  (let ((gesture
                         (and (or input-happened (eq timed-out :input-wait-test))
                              (read-gesture :stream stream :timeout 0))))
                    (block process-gesture
                      (cond ((null gesture)
                             (if (and (eq timed-out :timeout)
                                      timeout-function)
                                 (funcall timeout-function))
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
                                     (transform-position
                                      (medium-transformation current-window) px py)
                                   (setq wx (coordinate tx)
                                         wy (coordinate ty))))
                               (typecase gesture
                                 (pointer-button-press-event
                                  (when multiple-window
                                    (port-remove-all-pointer-grabs (port pointer)))
                                  (when presentation-press-function
                                    (let* ((window (event-sheet gesture))
                                           (presentation
                                            (and (output-recording-stream-p window)
                                                 (find-presentation
                                                  window px py :event gesture))))
                                      (when presentation
                                        (funcall presentation-press-function
                                                 presentation gesture wx wy)
                                        (return-from process-gesture))))
                                  (when button-press-function
                                    (funcall button-press-function gesture wx wy))
                                  (return-from process-gesture))
                                 (t	;pointer-button-release-event
                                  (when presentation-release-function
                                    (let* ((window (event-sheet gesture))
                                           (presentation
                                            (and (output-recording-stream-p window)
                                                 (find-presentation
                                                  window px py :event gesture))))
                                      (when presentation
                                        (funcall presentation-release-function
                                                 presentation gesture wx wy)
                                        (return-from process-gesture))))
                                  (when button-release-function
                                    (funcall button-release-function gesture wx wy))
                                  (return-from process-gesture))))))
                      (beep stream)))))))
	  (unhighlight)
	  (when last-window
	    (unhighlight-highlighted-presentation last-window)))))))


;; This is like FIND-INNERMOST-APPLICABLE-PRESENTATION, except that it
;; finds the innermost presentation irrespective of the input context
;; and any translators that might be around.
(defun find-innermost-presentation (stream x y)
  (let ((x (coordinate x))
        (y (coordinate y)))
    (declare (type coordinate x y))
    (labels
      ((mapper (record presentations x-offset y-offset)
         (declare (type coordinate x-offset y-offset))
         (multiple-value-bind (sensitive superior-sensitive inferior-presentation)
             (if (presentationp record)
                 ;;--- This should call PRESENTATION-REFINED-POSITION-TEST
                 (if (output-record-refined-position-test
                       record (- x x-offset) (- y y-offset))
                     (let ((displayed (displayed-output-record-p record))
                           (single-box (presentation-single-box record)))
                       (if (or (eq single-box t) (eq single-box :position))
                           (values t displayed nil)
                           (values nil displayed record)))
                     (values nil nil nil))
                 (values nil
                         (and presentations
                              (dolist (presentation presentations nil)
                                (unless (null presentation) (return t)))
                              (displayed-output-record-p record)
                              (output-record-refined-position-test
                                record (- x x-offset) (- y y-offset)))
                         nil))
           (with-stack-list* (more-presentations inferior-presentation presentations)
             (when inferior-presentation
               (setq presentations more-presentations))
             (multiple-value-bind (dx dy) (output-record-position record)
               (map-over-output-records-containing-position
                 #'mapper record x y
                 (- x-offset) (- y-offset)
                 presentations (+ x-offset dx) (+ y-offset dy)))
             (when sensitive
               (return-from find-innermost-presentation record))
             (when superior-sensitive
               (do* ((presentations presentations (cdr presentations))
                     (presentation (car presentations) (car presentations)))
                    ((null presentations))
                 (when presentation
                   (return-from find-innermost-presentation presentation))))))))
      (declare (dynamic-extent #'mapper))
      (mapper (stream-output-history stream) nil (coordinate 0) (coordinate 0)))))


;;; Miscellaneous utilities

(defun pointer-place-rubber-band-line* (&key start-x start-y
                                             (stream *standard-input*)
                                             (pointer (stream-primary-pointer stream))
                                             multiple-window)
  (let (end-x end-y)
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
                                      (pointer (stream-primary-pointer stream))
                                      multiple-window
                                      finish-on-release)
  (let ((rectangle-drawn nil))
    (flet ((finish (event)
             (when rectangle-drawn
               (draw-rectangle* stream left top right bottom
                                :filled nil :ink +flipping-ink+)
               (setq rectangle-drawn nil))
             (return-from pointer-input-rectangle*
               (values left top
                       (pointer-event-x event) (pointer-event-y event)))))
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
                (if (null left)
                    (setq left (pointer-event-x event)
                          top (pointer-event-y event))
                  (when (not finish-on-release)
                    (finish event)))
              (beep stream)))
          (:pointer-button-release (event)
            (if (eq (event-sheet event) stream)
                (when (and left
                           finish-on-release)
                  (finish event))
              (beep stream))))))))

(defun pointer-input-rectangle (&rest keys &key rectangle &allow-other-keys)
  (declare (dynamic-extent keys))
  (let (left top right bottom)
    (when rectangle
      (multiple-value-setq (left top right bottom)
        (bounding-rectangle* rectangle)))
    (multiple-value-call #'make-bounding-rectangle
      (with-keywords-removed (keys keys '(:rectangle))
        (apply #'pointer-input-rectangle*
               :left left :top top
               :right right :bottom bottom
               keys)))))

