;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/clim/accept-values.lisp,v 1.81 1997/05/31 01:00:29 tomj Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; For historical reasons "AVV" means "accepting-values"...

(defclass accept-values-stream
          (standard-encapsulating-stream)
    ((avv-record :initform nil)
     (avv-frame :initform nil)
     (align-prompts :initform nil :initarg :align-prompts)))

;; Don't allow old AVV queries to be sensitive
(defvar *accept-values-tick* 0)
(defvar *current-accept-values-tick* 0)

(defclass accept-values-output-record
          (standard-sequence-output-record)
    ((query-table :initarg :query-table)
     (tick :initform *current-accept-values-tick*)
     (resynchronize :initform nil))
  (:default-initargs :query-table (make-hash-table :test #'equal)))

(defmethod find-query ((record accept-values-output-record) query-identifier)
  (declare (values query found-p))
  (gethash query-identifier (slot-value record 'query-table)))

;; We are in control of the AVV code, and we know that there is only 1 AVV
;; under it's parent UPDATING-OUTPUT record.  Therefore we make it match.
;; (There's a separate issue, having nothing to do with incremental-redisplay
;; of making nested, hierarchical AVV's work.)
(defmethod match-output-records ((record accept-values-output-record) &rest initargs)
  (declare (ignore initargs))
  t)

(defclass accept-values-query ()
     ((query-identifier :initarg :query-identifier)
      (presentation-type :initarg :presentation-type
                         :reader accept-values-query-type)
      (value :initarg :value
             :accessor accept-values-query-value)
      (presentation :initarg :presentation
                    :accessor accept-values-query-presentation)
      (changed-p :initform nil
                 :accessor accept-values-query-changed-p)
      (active-p :initform t
                :accessor accept-values-query-active-p)
      (error-p :initform nil
               :accessor accept-values-query-error-p))
  (:default-initargs :presentation nil))

(defresource accept-values-stream (stream &key align-prompts)
  :constructor (make-instance 'accept-values-stream :stream stream)
  :matcher 't                                   ;any will suffice
  :initializer
    (progn
      (setf (slot-value accept-values-stream 'stream) stream)
      (setf (slot-value accept-values-stream 'align-prompts) align-prompts)))

(defun find-accept-values-record (output-record)
  (do ((record output-record (output-record-parent record)))
      ((null record) nil)
    (when (typep record 'accept-values-output-record)
      (return record))))

(defmethod prompt-for-accept :around ((stream accept-values-stream) type (view view)
                                      &rest accept-args
                                      &key query-identifier (prompt t) (display-default nil)
                                           (active-p t)
                                      &allow-other-keys)
  (declare (dynamic-extent accept-args))
  ;;--- When ACTIVE-P is NIL, this should do some sort of "graying out"
  #-aclpc (declare (ignore active-p))
  ;; ACCEPT within ACCEPTING-VALUES has to have a query-identifier, so default
  ;; it here and return it.  It's better to cons this list than to call format
  ;; to intern something.
  (unless query-identifier
    (setq query-identifier (list ':query-identifier prompt type)))
  (when (or prompt display-default)
    (updating-output (stream :unique-id (list :prompt query-identifier)
                             :id-test #'equal
                             :cache-value prompt)
      (with-keywords-removed (prompt-args accept-args '(:display-default :query-identifier))
        ;; Explicitly pass DISPLAY-DEFAULT so that if it is not supplied by the
        ;; user, we forcibly default it to NIL, which gives better-looking AVVs.
        (apply #'call-next-method stream type view
                                  :display-default display-default
                                  :query-identifier query-identifier
                                  prompt-args))))
  query-identifier)

(defmethod prompt-for-accept ((stream accept-values-stream) type view
                              &rest accept-args
                              &key query-identifier &allow-other-keys)
  ;; This does nothing, the gadget ACCEPT methods should provide a label
  (unless (gadget-includes-prompt-p type stream view)
    (call-next-method))
  query-identifier)

(defmethod stream-accept ((stream accept-values-stream) type
                          &rest accept-args
                          &key (prompt t) query-identifier (default nil default-supplied-p)
                               (view (stream-default-view stream)) (active-p t)
                          &allow-other-keys)
  (declare (dynamic-extent accept-args))
  ;;--- When ACTIVE-P is NIL, this should do some sort of "graying out"
  (let ((align-prompts (slot-value stream 'align-prompts)) query)
    (cond (align-prompts
           ;; The user has asked to line up the labels, so oblige him
           (updating-output (stream :unique-id (cons '#:accept (or query-identifier prompt))
                                    :id-test #'equal)
             (formatting-row (stream)
               (formatting-cell (stream :align-x align-prompts)
                 (setq query-identifier
                   (apply #'prompt-for-accept
                          (encapsulating-stream stream) type view accept-args)))
               (letf-globally (((slot-value stream 'align-prompts) nil))
                 (formatting-cell (stream :align-x :left)
                   (setq query (find-or-add-query stream query-identifier type prompt
                                                  default default-supplied-p view active-p)))))))
          (t
           (setq query-identifier
                 (apply #'prompt-for-accept
                        (encapsulating-stream stream) type view accept-args))
           (setq query (find-or-add-query stream query-identifier type prompt
                                          default default-supplied-p view active-p))))
    (values (accept-values-query-value query)
            (accept-values-query-type query)
            ;; Set this to NIL so that it appears that nothing has
            ;; changed at the start of the next pass.
            (prog1 (accept-values-query-changed-p query)
                   (setf (accept-values-query-changed-p query) nil)))))

;; Probably should be keyword arguments...


(defmethod find-or-add-query ((stream accept-values-stream) query-identifier ptype prompt
                              default default-supplied-p view active-p)
  (let ((avv-record (slot-value stream 'avv-record)))
    (with-slots (query-table) avv-record
      (multiple-value-bind (query found-p)
          (gethash query-identifier query-table)
        (unless query
          (setq query (make-instance 'accept-values-query
                        :presentation-type ptype
                        :value default
                        :query-identifier query-identifier)))
        (setf (accept-values-query-active-p query) active-p)
        ;;--- Really wants to reuse existing presentation if found and
        ;;--- make sure that that presentation gets re-parented if necessary
        ;;--- (suppose inside FORMATTING TABLE...)
        (setf (accept-values-query-presentation query)
              (updating-output
                  (stream :unique-id query-identifier
                          ;;-- If the old-style text field value is
                          ;;-- unchanged then we need this hack
                          :all-new (and (not (typep view 'actual-gadget-view))
                                         (accept-values-query-changed-p query))
                          :id-test #'equal
                          :cache-value (list (if (accept-values-query-changed-p query)
                                                 (accept-values-query-value query)
                                               default)
                                             ptype
                                             active-p)
                          :cache-test #'(lambda (x y)
                                          (unless (accept-values-query-changed-p query)
                                            (equal x y))))
                ;;--- Calling ACCEPT-1 directly bypasses default preprocessing,
                ;;--- is that OK?
                (cond ((or default-supplied-p (accept-values-query-changed-p query))
                       (accept-1 stream ptype
                                 :view view :prompt prompt
                                 ;; If this field changed, use the value it changed to,
                                 ;; making sure that the query object gets changed, too.
                                 ;; We need to do this because the body of the dialog
                                 ;; can change values without changing the query itself.
                                 :default (if (accept-values-query-changed-p query)
                                              (accept-values-query-value query)
                                              (setf (accept-values-query-error-p query) nil
                                                  (accept-values-query-value query) default))
                                 :history ptype
                                 :present-p `(accept-values-choice ,query)
                                 :query-identifier query
                                 :active-p active-p))
                      ((and found-p
                            (presentation-typep (accept-values-query-value query) ptype))
                       ;; The programmer supplied no default, but a previous edit
                       ;; has put something in the query that we now must use as
                       ;; the default.
                       (accept-1 stream ptype
                                 :view view :prompt prompt
                                 :default (accept-values-query-value query)
                                 :history ptype
                                 :present-p `(accept-values-choice ,query)
                                 :query-identifier query
                                 :active-p active-p))
                      (t
                       ;; No default supplied, field has never been edited.
                       (accept-1 stream ptype
                                 :view view :prompt prompt
                                 :history ptype :provide-default nil
                                 :present-p `(accept-values-choice ,query)
                                 :query-identifier query
                                 :active-p active-p)))))
        ;; really wants to move the cursor position to some reasonable
        ;; place in case we're just reusing an existing presentation
        (unless found-p
          (setf (gethash query-identifier query-table) query))
        query))))

;;--- It seems a little bizarre having a prompt argument, but we can't
;;--- think of a better way.
(defmethod stream-present ((stream accept-values-stream) object presentation-type
                           &rest present-args
                           &key view (prompt nil prompt-p) (query-identifier nil)
                           &allow-other-keys)
  (declare (dynamic-extent present-args))
  #-aclpc (declare (ignore prompt))
  (unless prompt-p
    ;; This is the normal, non-gadget case of doing PRESENT
    (return-from stream-present (call-next-method)))
  (flet ((do-present (stream)
           (updating-output (stream :unique-id query-identifier
                                    :id-test #'equal
                                    :cache-value object
                                    :cache-test #'equal)
             stream
             ;;--- We did this because (SEQUENCE INTEGER) reinvoked
             ;;--- PRESENT with the prompt for each of components of
             ;;--- the stream, which ended up back here creating badly
             ;;--- identified updating-output records.
             (apply #'stream-present (encapsulating-stream-stream stream)
                    object presentation-type present-args))))
    (declare (dynamic-extent #'do-present))
    (let ((align-prompts (slot-value stream 'align-prompts)))
      (cond (align-prompts
             ;; The user has asked to line up the labels, so oblige him
             (formatting-row (stream)
               (formatting-cell (stream :align-x align-prompts)
                 (setq query-identifier
                       (apply #'prompt-for-accept (encapsulating-stream stream)
                              presentation-type view present-args)))
               (formatting-cell (stream :align-x :left)
                 (do-present stream))))
            (t
             (setq query-identifier
                   (apply #'prompt-for-accept (encapsulating-stream stream)
                          presentation-type view present-args))
             (do-present stream)))))
  (values object))


;;--- These frames should be resourced
(define-application-frame accept-values ()
    ((stream :initarg :stream)
     (exit-button-stream :initarg :exit-button-stream)
     (continuation :initarg :continuation)
     (exit-boxes :initform nil :initarg :exit-boxes)
     (selected-item :initform nil)
     (initially-select-query-identifier
       :initform nil :initarg :initially-select-query-identifier)
     (resynchronize-every-pass :initform nil :initarg :resynchronize-every-pass)
     (check-overlapping :initform t :initarg :check-overlapping)
     (own-window :initform nil :initarg :own-window)
     (own-window-properties :initform nil :initarg :own-window-properties)
     (view :initarg :view))

  (:top-level (accept-values-top-level))
  (:command-definer t))

;;--- These frames should be resourced
(define-application-frame accept-values-own-window (accept-values)
    ((help-window :initform nil)
     (scroll-bars :initform nil :initarg :scroll-bars)
     (align-prompts :initform nil :initarg :align-prompts))
    (:pane
     (let ((frame *application-frame*)
           panes)
       (with-slots (stream own-window exit-button-stream scroll-bars align-prompts)  frame
         (multiple-value-setq (panes own-window exit-button-stream)
                              (frame-manager-construct-avv-panes
                               frame (frame-manager frame)))
         (setq stream (make-instance 'accept-values-stream
                                     :align-prompts align-prompts
                                     :stream own-window))
         panes)))
  (:menu-bar nil)
  (:top-level (accept-values-top-level))
  (:command-table accept-values)
  (:command-definer nil))

(defmethod frame-manager-construct-avv-panes ((frame accept-values-own-window)
                                              (framem standard-frame-manager))
  (let (exit-button-stream own-window)
    (values
     (with-look-and-feel-realization (framem frame)
       (with-slots (scroll-bars) frame
         (vertically ()
           (outlining ()
             (progn
               (setq own-window
                 (make-pane 'clim-stream-pane
                            :initial-cursor-visibility :off
                            :end-of-page-action :allow))
               (if scroll-bars
                   (scrolling (:scroll-bars scroll-bars) own-window)
                 own-window)))
           (outlining ()
             (setf exit-button-stream
               (make-pane 'clim-stream-pane
                          :initial-cursor-visibility nil))))))
     own-window
     exit-button-stream)))


(defmethod frame-manager-accepting-values-frame-class ((framem standard-frame-manager))
  'accept-values-own-window)

(defmethod frame-manager-default-exit-boxes ((framem standard-frame-manager))
  '((:abort) (:exit)))

(defmethod frame-manager-exit-box-labels
           ((framem standard-frame-manager) frame view)
  (declare (ignore frame view))
  '((:exit   "Exit")
    (:abort  "Cancel")))

;; So the continuation can run with the proper value of *APPLICATION-FRAME*
(defvar *avv-calling-frame*)


(defun invoke-accepting-values (stream continuation
                                 &key frame-class command-table
                                      own-window background foreground text-style
                                      (exit-boxes
                                        (frame-manager-default-exit-boxes
                                          (frame-manager stream)))
                                      (resize-frame nil) (scroll-bars nil)
                                      (initially-select-query-identifier nil)
                                      (modify-initial-query nil) align-prompts
                                      (resynchronize-every-pass nil) (check-overlapping t)
                                      label x-position y-position
                                      width height (right-margin 10) (bottom-margin 10)
                                      view)
   (incf *accept-values-tick*)
   (setq align-prompts (ecase align-prompts
                         ((t :right) :right)
                         ((:left) :left)
                         ((nil) nil)))

   (let ((frame-manager (frame-manager stream))
         (*current-accept-values-tick* *accept-values-tick*)
         (the-own-window nil))
     ;; Create the AVV, run it, and return its values
     (if own-window
         (let ((frame (make-application-frame
                        (or frame-class
                            (frame-manager-accepting-values-frame-class
                             frame-manager))
                        ;;--- What is the correct thing here?
                        :calling-frame (or (and (typep stream '(or basic-pane standard-encapsulating-stream))
                                                (pane-frame stream))
                                           *application-frame*)
                        :input-buffer (and (typep stream '(or input-protocol-mixin standard-encapsulating-stream))
                                           (stream-input-buffer stream))
                        :frame-manager frame-manager
                        :pretty-name label
                        :continuation continuation
                        :exit-boxes exit-boxes
                        :own-window the-own-window
                        :own-window-properties (list x-position y-position
                                                     width height
                                                     right-margin bottom-margin)
                        :left x-position :top y-position
                        :initially-select-query-identifier
                          (and initially-select-query-identifier
                               (cons initially-select-query-identifier modify-initial-query))
                        :resynchronize-every-pass resynchronize-every-pass
                        :check-overlapping check-overlapping
                        :resize-frame resize-frame
                        :scroll-bars scroll-bars
                        :background background
                        :foreground foreground
                        :text-style text-style
                        :align-prompts align-prompts
                        :view view)))
           (when command-table
             (setf (frame-command-table frame) command-table))
           (unwind-protect
               (let ((*avv-calling-frame* *application-frame*))
                 (run-frame-top-level frame))
             ;; Flush it so the GC can get rid of it
             (destroy-frame frame)))
         (using-resource (avv-stream accept-values-stream (or the-own-window stream)
                                     :align-prompts align-prompts)
           (let ((frame (make-application-frame (or frame-class 'accept-values)
                          :calling-frame *application-frame*
                          :stream avv-stream
                          :continuation continuation
                          :exit-boxes exit-boxes
                          :initially-select-query-identifier
                            (and initially-select-query-identifier
                                 (cons initially-select-query-identifier modify-initial-query))
                          :resynchronize-every-pass resynchronize-every-pass
                          :check-overlapping check-overlapping
                          ;; This frame won't necessarily be adopted, so make
                          ;; sure that we share the sheet with the parent frame
                          ;; in the case of "inlined" dialogs
                          :top-level-sheet (frame-top-level-sheet
                                            *application-frame*)
                          :view view)))
             (when command-table
               (setf (frame-command-table frame) command-table))
             (unwind-protect
                 (let ((*avv-calling-frame* *application-frame*))
                   (run-frame-top-level frame))
               ;; Since this frame isn't really adopted, we don't really want
               ;; to disown it since that will end up disowning the calling
               ;; frame.  Flush it from the frame manager manually.  Barf.
               (let ((framem (frame-manager frame)))
                 (setf (frame-manager-frames framem)
                         (delete frame (frame-manager-frames framem))
                       (slot-value frame 'frame-manager) nil))))))))

(defvar *editting-field-p* nil)

(defmethod accept-values-top-level ((frame accept-values) &rest args)
  (declare (ignore args))
  (with-slots (stream continuation resynchronize-every-pass check-overlapping
               selected-item initially-select-query-identifier
               own-window own-window-properties exit-button-stream
                      #-(or aclpc acl86win32) view) frame
    (let* ((original-view (stream-default-view stream))
           (return-values nil)
           (initial-query nil)
           exit-button-record
           avv avv-record
           (align-prompts (slot-value stream 'align-prompts))
           (properties own-window-properties)
           (own-window-x-position (pop properties))
           (own-window-y-position (pop properties))
           (own-window-width  (pop properties))
           (own-window-height (pop properties))
           (own-window-right-margin  (pop properties))
           (own-window-bottom-margin (pop properties))
           (view 
            (or #-(or aclpc acl86win32) view #+(or aclpc acl86win32) (slot-value frame 'view)
                (frame-manager-dialog-view (frame-manager frame))))
           (*editting-field-p* nil))
      (letf-globally (((stream-default-view stream) view)
                      ((stream-read-gesture-cursor-state stream) nil))
        (labels ((run-continuation (stream avv-record)
                   (setf (slot-value stream 'avv-record) avv-record)
                   (setf (slot-value stream 'avv-frame) frame)
                   (with-output-recording-options (stream :draw nil :record t)
                     (let ((*application-frame* *avv-calling-frame*))
                       (if align-prompts
                           ;; Use of FORMATTING-TABLE here implies that
                           ;; no output should be done by the user code
                           ;; outside of calls to FORMATTING-ROW and
                           ;; FORMATTING-CELL.  Too bad.
                           (formatting-table (stream)
                             (setq return-values
                                   (multiple-value-list
                                       (funcall continuation
                                                stream))))
                         (progn
                           (display-view-background stream view)
                           (setq return-values
                                 (multiple-value-list
                                   (funcall continuation stream))))))
                     (unless own-window
                       (display-exit-boxes frame stream
                                           (stream-default-view stream)))))
                 (run-avv ()
                   (and initially-select-query-identifier
                        (setq initial-query
                          (find-query avv-record
                                      (car initially-select-query-identifier)))
                        (move-focus-to-query stream
                                             initial-query
                                             (not (cdr initially-select-query-identifier)))
                        ;; only do the redisplay if move-focus-to-query
                        ;; returns t - ie for the non-gadget case
                        (redisplay avv stream :check-overlapping check-overlapping))
                   (loop
                     (let ((command
                             (let ((command-stream (encapsulating-stream-stream stream)))
                               ;; While we're reading commands, restore the view
                               ;; to what it was before we started.
                               (letf-globally (((stream-default-view command-stream)
                                                original-view))
                                 (read-frame-command frame :stream command-stream)))))
                       (if (and command (not (keyboard-event-p command)))
                           (execute-frame-command frame command)
                           (beep stream)))
                     (with-deferred-gadget-updates
                       (when (or resynchronize-every-pass
                                 (slot-value avv-record 'resynchronize))
                         ;; When the user has asked to resynchronize every pass, that
                         ;; means we should run the continuation an extra time to see
                         ;; that all visible stuff is up to date.  That's all!
                         (with-output-recording-options (stream :draw nil)
                           (redisplay avv stream :check-overlapping
                                      check-overlapping)))
                       (setf (slot-value avv-record 'resynchronize) nil)
                       (when exit-button-record
                         (redisplay exit-button-record exit-button-stream))
                       (redisplay avv stream :check-overlapping check-overlapping)
                       (when (ecase (frame-resizable frame)
                               ((nil) nil)
                               ((t :grow)
                                (and own-window
                                     (multiple-value-bind (width height)
                                         (bounding-rectangle-size
                                           (stream-output-history own-window))
                                       (multiple-value-bind (vwidth vheight)
                                           (bounding-rectangle-size
                                             (window-viewport own-window))
                                         (if (eq (frame-resizable frame) :grow)
                                             (or (> width vwidth) (> height vheight))
                                             (or (/= width vwidth) (/= height vheight))))))))
                         (size-panes-appropriately)))))
                 (size-panes-appropriately ()
                   (changing-space-requirements (:layout t)
                     ;; We really want to specify the min/max sizes of
                     ;; the exit-button pane also
                     (when exit-button-stream
                       (size-frame-from-contents exit-button-stream
                         :size-setter
                         #'(lambda (pane w h)
                             (setf w (max w 1) h (max h 1))
                             (change-space-requirements pane
                               :width w :min-width w :max-width w
                               :height h :min-height h :max-height h))
                         :right-margin 0
                         :bottom-margin 0))
                     (when own-window
                       (size-frame-from-contents own-window
                         :size-setter
			 #'(lambda (pane w h)
                             (change-space-requirements
			      pane
			      ;; only use the min-xxx if explicitly specified
			      ;; in call to accepting-values (cim 7/12/96)
			      :width w :min-width (and own-window-width w)
			      :height h	:min-height (and own-window-height h)
			      :resize-frame t))
			 :width own-window-width
                         :height own-window-height
                         :right-margin own-window-right-margin
                         :bottom-margin own-window-bottom-margin)))))
          (declare (dynamic-extent #'run-continuation #'run-avv
                                   #'size-panes-appropriately))
          (handler-bind ((frame-exit
                           #'(lambda (condition)
                               (let ((exit-frame (frame-exit-frame condition)))
                                 (when (eq frame exit-frame)
                                   (return-from accept-values-top-level
                                     (values-list return-values)))))))
            (setq avv
                  (updating-output (stream)
                    (setq avv-record
                          (with-end-of-line-action (stream :allow)
                            (with-end-of-page-action (stream :allow)
                              (with-new-output-record
                                  (stream 'accept-values-output-record avv-record)
                                (run-continuation stream avv-record)))))))
            ;; In own window dialogs the buttons are displayed separately
            (when (and own-window exit-button-stream)
              (setq exit-button-record
                    (updating-output (exit-button-stream)
                      (with-end-of-line-action (exit-button-stream :allow)
                        (with-end-of-page-action (exit-button-stream :allow)
                          (display-exit-boxes frame exit-button-stream
                                              (stream-default-view stream)))))))
            (unwind-protect
                (cond (own-window
                       (size-panes-appropriately)
                       (frame-manager-position-dialog
                        (frame-manager frame)
                        frame
                        own-window-x-position own-window-y-position)
                       (setf (window-visibility own-window) t)
                       (with-input-focus (own-window)
                         (when exit-button-record
                           (replay exit-button-record exit-button-stream))
                         (replay avv stream)
                         (run-avv)))
                      (t
                       ;; Ensure that bottom of the AVV is visible.  I think that
                       ;; this is OK even if the AVV is bigger than the viewport.
                       (move-cursor-beyond-output-record
                         (encapsulating-stream-stream stream) avv)
                       (stream-ensure-cursor-visible stream)
                       (replay avv stream)
                       (run-avv)))
              (unless own-window
                (deactivate-all-gadgets avv-record)
                (move-cursor-beyond-output-record
                 (encapsulating-stream-stream stream) avv)))))))))


(defun find-query-gadget (query)
  (let ((record (accept-values-query-presentation query)))
    (when record
      (labels ((find-gadget-output-record (record)
                 (typecase record
                   (gadget-output-record
                    (return-from find-query-gadget (output-record-gadget record)))
                   (output-record
                    (map-over-output-records #'find-gadget-output-record record)))))
        (declare (dynamic-extent #'find-gadget-output-record))
        (find-gadget-output-record record)))))

(defun move-focus-to-query (stream query &optional (editp t))
  (declare (ignore stream))
  ;;-- If there is a gadget then we should move the focus to that
  ;;-- otherwise edit the field
  (let ((gadget (find-query-gadget query)))
    (cond (gadget
           (move-focus-to-gadget gadget))
          ;;-- This stuff will still loose on old style CLIM
          ;;-- member fields
          (editp
           (com-edit-avv-choice query))
          (t
           (com-modify-avv-choice query)))
    ;; return nil for the gadget case so that we avoid redisplay which
    ;; would otherwise mess up the gadget focus (cim 2/2/95)
    (not gadget)))

(defmethod display-view-background (stream (view view))
  (declare (ignore stream))
  nil)

(defmethod invoke-with-aligned-prompts ((stream accept-values-stream) continuation 
                                                                      &key (align-prompts t))
  #+(or aclpc acl86win32) (declare (dynamic-extent continuation))
  (setq align-prompts (ecase align-prompts
                        ((t :right) :right)
                        ((:left) :left)
                        ((nil) nil)))
  (let ((return-values nil))
    (flet ((run-continuation (stream)
             (if align-prompts
                 (formatting-table (stream)
                   (setq return-values
                     (multiple-value-list
                         (funcall continuation stream))))
               (setq return-values
                 (multiple-value-list
                     (funcall continuation stream))))))
      (let ((old-align-prompts (slot-value stream 'align-prompts)))
        (unwind-protect
            (progn (setf (slot-value stream 'align-prompts) align-prompts)
                   (if old-align-prompts
                       (formatting-row (stream)
                         (formatting-cell (stream)
                           (run-continuation stream)))
                     (run-continuation stream)))
          (setf (slot-value stream 'align-prompts) old-align-prompts))))
    (values-list return-values)))

(defmethod invoke-with-aligned-prompts ((stream t) continuation &key align-prompts)
  (declare (ignore align-prompts))
  #+(or aclpc acl86win32) (declare (dynamic-extent continuation))
  (funcall continuation stream))

(defmethod frame-manager-display-input-editor-error
           ((framem standard-frame-manager) (frame accept-values) stream anerror)
  ;;--- Resignal the error so the user can handle it
  ;;--- (in lieu of HANDLER-BIND-DEFAULT)
  (notify-user frame (princ-to-string anerror)
               :title "Input error"
               :style :error :exit-boxes '(:exit))
  (remove-activation-gesture stream)
  ;; Now wait until the user forces a rescan by typing
  ;; an input editing command
  (loop (read-gesture :stream stream)))

(defmethod frame-manager-display-help
    (framem (frame accept-values-own-window) (stream standard-input-editing-stream) continuation)
  (declare (ignore framem))
  (declare (dynamic-extent continuation))
  ;;-- Yuck but think of a better way
  (let ((old-help *accept-help*))
    (accepting-values (stream :exit-boxes '(:exit)
                              :label "Input editor help"
                              :own-window t)
      (let ((*accept-help* old-help))
        (funcall continuation stream)))))


(defmethod accept-values-top-level :around ((frame accept-values-own-window) &rest args)
  #-aclpc (declare (ignore args))
  (unwind-protect
      (call-next-method)
    (with-slots (help-window) frame
      (when help-window
        (deallocate-resource 'menu help-window)
        (setq help-window nil)))))

(defmethod read-frame-command ((frame accept-values) &key (stream *standard-input*))
  (read-command (frame-command-table frame)
                :stream stream
                :command-parser 'menu-command-parser
                :use-keystrokes t))

(defmethod accept-values-resynchronize ((stream accept-values-stream))
  (setf (slot-value (slot-value stream 'avv-record) 'resynchronize) t))

(defmethod accept-values-resize-window ((stream accept-values-stream))
  (with-slots (own-window own-window-properties)
              (slot-value stream 'avv-frame)
    (when own-window
    (let* ((own-window-x-position (pop own-window-properties))
           (own-window-y-position (pop own-window-properties))
           (own-window-width  (pop own-window-properties))
           (own-window-height (pop own-window-properties))
           (own-window-right-margin  (pop own-window-properties))
           (own-window-bottom-margin (pop own-window-properties)))
      (declare (ignore own-window-x-position own-window-y-position
                       own-window-width own-window-height))
      (multiple-value-bind (new-width new-height)
          (bounding-rectangle-size (slot-value stream 'avv-record))
        (multiple-value-bind (width height)
            (window-inside-size own-window)
          (when (or (> new-width width)
                    (> new-height height))
            (size-frame-from-contents own-window
                                      :right-margin own-window-right-margin
                                      :bottom-margin own-window-bottom-margin))))))))


(define-presentation-type accept-values-exit-box ())

(defmacro with-exit-box-decoded ((value label text-style documentation show-as-default)
                                 exit-box labels &body body)
  `(let* ((,value (if (consp ,exit-box) (first ,exit-box) ,exit-box))
          (,label (or (and (consp ,exit-box)
                           (second ,exit-box))
                      (second (assoc ,value ,labels))))
          (,text-style (or (and (consp ,exit-box)
                                (getf (cddr ,exit-box) :text-style))
                           (getf (cddr (assoc ,value ,labels)) :text-style)))
          (,documentation (or (and (consp ,exit-box)
                                   (getf (cddr ,exit-box) :documentation))
                              (getf (cddr (assoc ,value ,labels)) :documentation)))
          (,show-as-default
           (if (and (consp ,exit-box)
                    (member :show-as-default (cddr ,exit-box)))
               (getf (cddr ,exit-box) :show-as-default)
	       (getf (cddr (assoc ,value ,labels)) :show-as-default))))
     ,@body))

;;; Applications can create their own AVV class and specialize this method in
;;; order to get different exit boxes.
(defmethod display-exit-boxes ((frame accept-values) stream (view view))
  ;; Do the fresh-line *outside* of the updating-output so that it
  ;; doesn't get repositioned relatively in the X direction if the
  ;; previous line gets longer.  Maybe there should be some better
  ;; way of ensuring this.
  (fresh-line stream)
  (let ((labels (frame-manager-exit-box-labels (frame-manager frame) frame view)))
    (updating-output (stream :unique-id stream :cache-value 'exit-boxes)
      (with-slots (exit-boxes) frame
        (dolist (exit-box exit-boxes)
          (with-exit-box-decoded (value label text-style documentation show-as-default)
            exit-box labels
            (declare (ignore documentation show-as-default))
            (when label
              (with-text-style (stream text-style)
                (with-output-as-presentation (stream value 'accept-values-exit-box)
                  #-CCL-2
                  (write-string label stream)
                  #+CCL-2
                  (if (eq value ':abort)
                      ;; Kludge to print the cloverleaf char in MCL.
                      ;; Needs an accompanying kludge in STREAM-WRITE-CHAR so that
                      ;; #\CommandMark doesn't get lozenged.
                      (progn
                        (with-text-style (stream '(:mac-menu :roman :normal))
                          (write-char #\CommandMark stream))
                        (write-string "-. aborts" stream))
                      (write-string label stream)))
                (write-string " " stream)))))))))

;;--- Get this right
(defmethod frame-pointer-documentation-output ((frame accept-values-own-window))
  nil)


(defmethod deactivate-all-gadgets (record)
  (declare (ignore record))
  nil)

(defmethod deactivate-all-gadgets ((record output-record-mixin))
  (map-over-output-records #'deactivate-all-gadgets record))

(defmethod deactivate-all-gadgets :after ((record gadget-output-record))
  (map-over-sheets
    #'(lambda (sheet)
        (when (typep sheet 'gadget)
          (deactivate-gadget sheet)))
    (output-record-gadget record)))


;;--- It would have been nice to have defined these in terms of the
;;--- other gestures
(define-gesture-name :edit-field   :pointer-button (:left))
(define-gesture-name :modify-field :pointer-button (:middle))
(define-gesture-name :delete-field :pointer-button (:middle :shift))

(define-presentation-type accept-values-choice ())

(defun-inline accept-values-query-valid-p (query query-record)
  (and (or (null query) (accept-values-query-active-p query))
       (let ((avv-record (find-accept-values-record query-record)))
         (and avv-record
              (= (slot-value avv-record 'tick) *current-accept-values-tick*)))))

(define-accept-values-command com-edit-avv-choice
    ((choice 'accept-values-choice))
  (with-slots (stream selected-item) *application-frame*
    (setq selected-item choice)
    (accept-values-query-edit-value choice stream)))

(define-presentation-to-command-translator edit-avv-choice
    (accept-values-choice com-edit-avv-choice accept-values
     :documentation "Edit this field"
     :pointer-documentation "Edit this field"
     :tester ((object presentation)
              (accept-values-query-valid-p object presentation))
     :gesture :edit-field)
    (object)
  (list object))

(define-accept-values-command com-modify-avv-choice
    ((choice 'accept-values-choice))
  (with-slots (stream selected-item) *application-frame*
    (setq selected-item choice)
    (accept-values-query-edit-value choice stream :modify t)))

(define-presentation-to-command-translator modify-avv-choice
    (accept-values-choice com-modify-avv-choice accept-values
     :documentation "Modify this field"
     :pointer-documentation "Modify this field"
     :tester ((object presentation)
              (accept-values-query-valid-p object presentation))
     :gesture :modify-field)
    (object)
  (list object))

(defmethod accept-values-query-edit-value ((query accept-values-query) stream &key modify)
  (with-slots (presentation-type value changed-p prompt presentation) query
    (let ((stream (encapsulating-stream-stream stream))
          (*editting-field-p* stream))
      (with-stream-cursor-position-saved (stream)
        (multiple-value-bind (xoff yoff)
            (convert-from-relative-to-absolute-coordinates
             stream (output-record-parent presentation))
          (multiple-value-bind (x y) (output-record-position presentation)
            (stream-set-cursor-position stream (+ x xoff) (+ y yoff))))
        (unwind-protect
            (progn
              (erase-output-record presentation stream)
              (let* ((new-value nil)
                     (aborted nil)
                     (record
                      (with-new-output-record (stream)
                        ;; The text cursor should be visible while this ACCEPT is
                        ;; waiting for input to be typed into this field
                        (letf-globally (((stream-read-gesture-cursor-state stream) t))
                          (multiple-value-setq (new-value aborted)
                            (catch-abort-gestures ("Abort editing the current field")
                              (values
                               (accept presentation-type
                                       :stream stream :prompt nil :default value
                                       :insert-default modify))))))))
                ;; This so that the input editor's typing gets erased properly.
                (erase-output-record record stream)
                ;;--- Kludge until Bill can explain the whole
                ;;"leave the delimiter" vs "process the delimiter" scheme to me
                (when (read-gesture :stream stream :peek-p t :timeout 0)
                  (process-delimiter stream))
                (unless aborted
                  (setf value new-value))))
          (setf changed-p t))))))

(defun map-over-accept-values-queries (avv-record continuation)
  (declare (dynamic-extent continuation))
  (labels ((map-queries (record)
             (let ((unique-id (and (updating-output-record-p record)
                                   (slot-value record 'unique-id))))
               (when (and (listp unique-id)
                          (eq (first unique-id) :query-identifier))
                 (funcall continuation record unique-id)))
             (map-over-output-records #'map-queries record)))
    (declare (dynamic-extent #'map-queries))
    (map-queries avv-record)))

#+Genera
(define-accept-values-command (com-next-avv-choice :keystroke (:n :control)) ()
  (with-slots (stream selected-item) *application-frame*
    (let ((avv-record (slot-value stream 'avv-record)))
      (cond ((null selected-item)
             (map-over-accept-values-queries avv-record
                                             #'(lambda (record unique-id)
                   (declare (ignore record))
                   (return-from com-next-avv-choice
                     (setq selected-item (find-query avv-record unique-id))))))
            (t
             (let* ((item (find-query avv-record (slot-value selected-item 'query-identifier)))
                    (item-record (slot-value item 'presentation))
                    (found-item nil))
               (map-over-accept-values-queries avv-record
                 #'(lambda (record unique-id)
                     (if found-item
                         (return-from com-next-avv-choice
                           (setq selected-item (find-query avv-record unique-id)))
                       (when (eq record item-record)
                         (setq found-item t)))))))))))

#+Genera
(define-accept-values-command (com-previous-avv-choice :keystroke (:p :control)) ()
  (with-slots (stream selected-item) *application-frame*
    (let ((avv-record (slot-value stream 'avv-record)))
      (cond ((null selected-item)
             ;;--- Do this
             )
            (t
             ;;--- Do this
             )))))

#+Genera
(add-keystroke-to-command-table 'accept-values '(:e :control) :function
  #'(lambda (gesture numeric-argument)
      (declare (ignore gesture numeric-argument))
      (with-slots (selected-item) *application-frame*
        (if (null selected-item)
            (beep)
            `(com-edit-avv-choice ,selected-item))))
  :errorp nil)

(define-accept-values-command com-delete-avv-choice
    ((choice 'accept-values-choice))
  (accept-values-query-delete-value choice))

(define-presentation-to-command-translator delete-avv-choice
    (accept-values-choice com-delete-avv-choice accept-values
     :documentation "Remove this field"
     :pointer-documentation "Remove this field"
     :tester ((object presentation)
              (accept-values-query-valid-p object presentation))
     :gesture :delete-field)
    (object)
  (list object))

(defmethod accept-values-query-delete-value ((query accept-values-query))
  (with-slots (presentation-type value changed-p) query
    (when (presentation-typep nil presentation-type)
      (setf value nil
            changed-p t))))

(define-gesture-name :exit-dialog  :keyboard (:end))
(define-gesture-name :abort-dialog :keyboard (:abort))

(define-gesture-name :default-dialog  :keyboard (:newline))

;; com-default-avv gets run when the "default" gesture is activated. On
;; Motif this is when the user hits "return". In most cases we don't need
;; to do anything because the :default-button mechanism should cause the
;; correct command to be run - but to maintain compatibility with
;; non-gadget style dialogs we check for +textual-dialog-view+ and then "do
;; the Symbolics thing". (cim/tomj 5/21/97)

(define-accept-values-command (com-default-avv :keystroke :default-dialog) ()
  (let ((frame *application-frame*))
    (with-slots (stream) frame
      (when (eq (stream-default-view stream)
		+textual-dialog-view+)
	(com-exit-avv)))))


(define-accept-values-command (com-exit-avv :keystroke :exit-dialog) ()
  (let ((frame *application-frame*))
    (with-slots (stream) frame
      (when (verify-queries frame stream (slot-value stream 'avv-record))
        (frame-exit frame)))))


(defun verify-queries (frame stream avv-record)
  (let ((queries nil))
    (maphash
     #'(lambda (query-identifier query)
         (declare (ignore query-identifier))
         (when (and (accept-values-query-active-p query)
                    (output-record-stream (accept-values-query-presentation query))
                    (accept-values-query-error-p query))
           (push query queries)))
     (slot-value avv-record 'query-table))
    (if queries
        (progn
          (verify-queries-1 frame stream queries)
          nil)
      t)))

(defun verify-queries-1 (frame stream queries)
  (display-invalid-queries
   frame
   stream
   (mapcar #'(lambda (query)
               (list
                query
                (slot-value query 'query-identifier)
                (let ((condition (accept-values-query-error-p query)))
                  (and (not (eq condition t)) condition))))
           queries))
  (move-focus-to-query stream (car queries)))



(defmethod display-invalid-queries ((frame standard-application-frame) stream query-info)
  (declare (ignore stream))
  (notify-user frame
               (format nil 
#-(or aclpc acl86win32)
                       "The following fields are not valid:~:{~%~4T~A~@[: ~A~]~}"
#+(or aclpc acl86win32)
                       "The following fields are not valid:~{ ~{~A~:[~;~:* : ~A~]~}~}"
                       (mapcar #'(lambda (query-stuff)
                                   (destructuring-bind (query id condition) query-stuff
                                     (declare (ignore query))
                                     (list
                                      (if (and (consp id)
                                               (eq (car id) :query-identifier)
                                               (consp (cdr id))
                                               (stringp (second id)))
                                          (second id)
                                        id)
                                      (and (not (eq condition t)) condition))))
                               query-info))
               :title "Invalid Fields"
               :style :error
               :exit-boxes '(:exit)))

(defmethod display-invalid-queries ((frame accept-values-own-window) stream queries)
  (display-invalid-queries (frame-calling-frame frame) stream queries))

#+ignore
(defmethod (setf accept-values-query-error-p query) :around (value (query accept-values-query))
  (let ((old-value (accept-values-query-error-p query)))
    (call-next-method)
    (unless (eq value old-value)
      (note-query-validity-changed query value))))

#+ignore
(defmethod note-query-validity-changed (query errorp)
  (let ((gadget (find-query-gadget query)))
    (when (and gadget (typep gadget '(or text-field text-editor)))
      (if errorp
          (setf (gadget-original-background gadget) (gadget-background gadget)
                (gadget-background gadget) +red+)
        (setf (gadget-background gadget) (gadget-original-background gadget))))))

;; Focus-in callback/ value changed - establish normal
;; Focus-out callback - establish error condition
;; Need to save away contents, cursor position, colors.
;; What about the case when we are changing it back to ok cos some one
;; has changed the value?
;; Perhaps the old value come from present-to-string the query value.

(define-accept-values-command (com-abort-avv :keystroke :abort-dialog) ()
  (abort))

(define-presentation-translator abort-or-exit-avv
    (accept-values-exit-box (command :command-table accept-values) accept-values
     :tester-definitive t               ;just like a to-command translator
     :documentation ((object stream)
                     (case object
                       (:exit (write-string "Exit" stream))
                       (:abort (write-string "Abort" stream))))
     :gesture :select)
    (object)
  (case object
    (:exit '(com-exit-avv))
    (:abort '(com-abort-avv))))


;;; ACCEPTING-VALUES command buttons

(defclass accept-values-command-button ()
    ((continuation :initarg :continuation)
     (documentation :initarg :documentation)
     (resynchronize :initarg :resynchronize)))

(define-presentation-type accept-values-command-button ())

(defmacro accept-values-command-button ((&optional stream &rest options &key (view nil viewp) &allow-other-keys) prompt 
                                                                                                                 &body body &environment env)
  #+Genera (declare (zwei:indentation 1 3 2 1))
  #-(or Genera Minima) (declare (ignore env))
  #-acl3.0 ; temporary restriction?
  (declare (arglist ((&optional stream 
                                &key documentation query-identifier
                                (cache-value t) (cache-test #'eql)
                                view resynchronize)
                     prompt &body body)))
  (default-input-stream stream accept-values-command-button)
  (with-keywords-removed (options options '(:view))
    (let ((constant-prompt-p
           (and (constantp prompt #+(or Genera Minima) env)
                (stringp (eval prompt #+(or Genera Minima-Developer) env)))))
      `(flet ((avv-command-button-body () ,@body)
              ,@(unless constant-prompt-p
                  `((avv-command-button-prompt (,stream) ,prompt))))
         ,@(unless constant-prompt-p
             `((declare (dynamic-extent #'avv-command-button-prompt))))
         (invoke-accept-values-command-button
          ,stream
          #'avv-command-button-body ,(if viewp view `(stream-default-view ,stream))
          ,(if constant-prompt-p
               (eval prompt #+(or Genera Minima-Developer) env)
             '#'avv-command-button-prompt)
          ,@options)))))

(defmethod invoke-accept-values-command-button-1
           (stream continuation (view t) prompt
            &key (documentation (if (stringp prompt)
                                    prompt
                                    (with-output-to-string (stream)
                                      (funcall prompt stream))))
                 (query-identifier (list ':button documentation))
                 (cache-value t) (cache-test #'eql)
                 resynchronize
                 active-p)
  (declare (dynamic-extent prompt)
           (ignore active-p))
  (updating-output (stream :unique-id query-identifier :id-test #'equal
                           :cache-value cache-value :cache-test cache-test)
    (flet ((doit (stream)
             (with-output-as-presentation (stream
                                           (make-instance 'accept-values-command-button
                                                          :continuation continuation
                                                          :documentation documentation
                                                          :resynchronize resynchronize)
                                           'accept-values-command-button)
               (if (stringp prompt)
                   (write-string prompt stream)
                 (funcall prompt stream)))))
      (let ((align-prompts (slot-value stream 'align-prompts)) query)
        (if align-prompts
            (formatting-row (stream)
              (formatting-cell (stream) stream)
              (formatting-cell (stream) (doit stream)))
          (doit stream))))))

(define-accept-values-command com-avv-command-button
    ((button 'accept-values-command-button)
     (button-presentation 't))
  (funcall (slot-value button 'continuation))
  (when (slot-value button 'resynchronize)
    (let ((avv-record (find-accept-values-record button-presentation)))
      (setf (slot-value avv-record 'resynchronize) t))))

(define-presentation-to-command-translator avv-command-button
    (accept-values-command-button com-avv-command-button accept-values
     :documentation document-command-button
     :pointer-documentation document-command-button
     :tester ((presentation)
              (accept-values-query-valid-p nil presentation))
     :gesture :select)
    (object presentation)
  (list object presentation))

(defun document-command-button
       (button presentation context-type frame event window x y stream)
  (declare (ignore presentation context-type frame event window x y))
  (let ((documentation (slot-value button 'documentation)))
    (cond ((stringp documentation)
           (write-string documentation stream))
          (documentation
           (funcall documentation stream))
          (t
           (write-string "Execute this command button" stream)))))


;;; One-of and Some-of choices display

(defstruct accept-values-multiple-choices
  query-identifier
  select-action)

;; "One-of"
(defstruct accept-values-multiple-choice
  choices
  value)

(define-presentation-type accept-values-one-of ())
(define-presentation-type accept-values-some-of ())

(define-accept-values-command com-avv-choose-one-of
    ((choice 'accept-values-one-of))
  (avv-choose-one-of-1 choice))

(defun avv-choose-one-of-1 (choice)
  (let* ((choices (accept-values-multiple-choice-choices choice))
         (query (accept-values-multiple-choices-query-identifier choices)))
    (setf (accept-values-query-value query)
          (funcall (accept-values-multiple-choices-select-action choices)
                   (accept-values-multiple-choice-value choice)
                   (accept-values-query-value query)))
    (setf (accept-values-query-changed-p query) t)))

(define-presentation-to-command-translator avv-choose-one-of
    (accept-values-one-of com-avv-choose-one-of accept-values
     :documentation "Select this value"
     :pointer-documentation "Select this value"
     :tester ((object presentation)
              (accept-values-query-valid-p
                (accept-values-multiple-choices-query-identifier
                  (accept-values-multiple-choice-choices object))
                presentation))
     :echo nil :maintain-history nil
     :gesture :select)
    (object)
  (list object))

(defun accept-values-choose-from-sequence (stream sequence key selected-value tester
                                           type query-identifier
                                           select-action highlighting-function)
  (declare (dynamic-extent select-action highlighting-function))
  (flet ((presenter (thing stream)
           (present thing type :stream stream)))
    (declare (dynamic-extent #'presenter))
    (accept-values-choose-from-sequence-1
      stream sequence key selected-value tester
      type query-identifier
      select-action highlighting-function
      'accept-values-one-of #'presenter)))

(defun accept-values-choose-from-sequence-1 (stream sequence key selected-value tester
                                             type query-identifier
                                             select-action highlighting-function
                                             choice-type choice-presenter)
  (declare (dynamic-extent select-action highlighting-function tester choice-presenter))
  (declare (ignore type))
  ;;--- Unfortunately, this makes a new object every time we go around...
  (let ((choices (make-accept-values-multiple-choices
                   :query-identifier query-identifier
                   :select-action select-action)))
    (flet ((print-choice (object stream)
             (let* ((value (funcall key object))
                    (selected-p (funcall tester value selected-value)))
               (updating-output (stream :unique-id object
                                        :cache-value selected-p)
                 (with-output-as-presentation
                     (stream
                      (make-accept-values-multiple-choice :choices choices :value value)
                      choice-type)
                   (formatting-cell (stream)
                     (if selected-p
                         (funcall highlighting-function choice-presenter value stream)
                         (funcall choice-presenter value stream))))))))
      (declare (dynamic-extent #'print-choice))
      ;; :MAX-WIDTH makes it use as few rows as will fit in the available width.
      ;; :X-SPACING makes it not spread the choices to fill the whole width.
      ;; :INITIAL-SPACING NIL makes it not add whitespace at the start of the field.
      (formatting-item-list (stream :max-width (- (stream-text-margin stream)
                                                  (stream-cursor-position stream))
                                    :x-spacing '(2 :character)
                                    :initial-spacing nil)
        (doseq (object sequence)
          (print-choice object stream))))))

(define-accept-values-command com-avv-choose-some-of
    ((choice 'accept-values-some-of))
  (avv-choose-some-of-1 choice))

(defun avv-choose-some-of-1 (choice)
  (let* ((choices (accept-values-multiple-choice-choices choice))
         (query (accept-values-multiple-choices-query-identifier choices)))
    (let ((new-value (funcall (accept-values-multiple-choices-select-action choices)
                              (accept-values-multiple-choice-value choice)
                              (accept-values-query-value query))))
      (if (member new-value (accept-values-query-value query))
          ;; Use REMOVE instead of DELETE so the the new value is not EQ
          ;; to the old one.  If it were to stay EQ, the field would not
          ;; be properly redisplayed.
          (setf (accept-values-query-value query)
                (remove new-value (accept-values-query-value query)))
          (push new-value (accept-values-query-value query))))
    (setf (accept-values-query-changed-p query) t)))

(define-presentation-to-command-translator avv-choose-some-of
    (accept-values-some-of com-avv-choose-some-of accept-values
     :documentation "De/Select this value"
     :pointer-documentation "De/Select this value"
     :tester ((object presentation)
              (accept-values-query-valid-p
                (accept-values-multiple-choices-query-identifier
                  (accept-values-multiple-choice-choices object))
                presentation))
     :gesture :select)
    (object)
  (list object))

(defun accept-values-choose-from-subset (stream sequence key selected-value tester
                                         type query-identifier
                                         select-action highlighting-function)
  (declare (dynamic-extent select-action highlighting-function))
  (flet ((presenter (thing stream)
           (present (list thing) type :stream stream)))
    (declare (dynamic-extent #'presenter))
    (accept-values-choose-from-sequence-1
      stream sequence key selected-value
      #'(lambda (object sequence)
          (member object sequence :test tester))
      type query-identifier
      select-action highlighting-function
      'accept-values-some-of #'presenter)))


;;; Support for AVVs as application panes

(define-command-table accept-values-pane)

;; No need for AVV panes to deal with *CURRENT-ACCEPT-VALUES-TICK*, since it is
;; guaranteed that those queries will be valid at all times.

(defun get-frame-pane-to-avv-stream-table (frame)
  (or (frame-pane-to-avv-stream-table frame)
      (setf (frame-pane-to-avv-stream-table frame) (make-hash-table))))

(defun accept-values-pane-displayer (frame pane
                                     &key displayer
                                          resynchronize-every-pass
                                          (check-overlapping t)
                                          view
                                          align-prompts
                                          max-height max-width)
  (declare (ignore max-height max-width))
  (setq align-prompts (ecase align-prompts
                        ((t :right) :right)
                        ((:left) :left)
                        ((nil) nil)))
  (let* ((stream-and-record
          (and (not *frame-layout-changing-p*)
               (gethash pane (get-frame-pane-to-avv-stream-table frame))))
         (avv-stream (car stream-and-record))
         (avv-record (cdr stream-and-record)))
    (cond ((and avv-stream
                (output-record-stream avv-record))
           (with-deferred-gadget-updates
             (letf-globally (((stream-default-view avv-stream)
                              (or view
                                  (frame-manager-dialog-view (frame-manager frame)))))
               (redisplay avv-record avv-stream :check-overlapping check-overlapping)
               (when resynchronize-every-pass
                 (redisplay avv-record avv-stream :check-overlapping check-overlapping)))))
          (t
           (accept-values-pane-displayer-1 frame pane displayer
                                           align-prompts view)))))

(defun accept-values-pane-displayer-1 (frame pane displayer align-prompts view)
  (let ((avv-stream (make-instance 'accept-values-stream
                                   :stream pane :align-prompts align-prompts))
        (avv-record nil)
        (view (or view
                  (frame-manager-dialog-view (frame-manager frame)))))
    (setf (slot-value avv-stream 'avv-frame) frame)
    (letf-globally (((stream-default-view pane)
                     view))
      (setq avv-record
        (updating-output (avv-stream)
          (with-new-output-record
              (avv-stream 'accept-values-output-record avv-record)
            (setf (slot-value avv-stream 'avv-record) avv-record)
            (if align-prompts
                (formatting-table (avv-stream)
                  (funcall displayer frame avv-stream))
              (progn
                (display-view-background avv-stream view)
                (funcall displayer frame avv-stream)))))))
    #+(or aclpc acl86win32)
    (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)
    (unless *sizing-application-frame*
      (setf (gethash pane (get-frame-pane-to-avv-stream-table frame))
        (cons avv-stream avv-record)))))

(define-command (com-edit-avv-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice)
     (pane 't))
  (accept-values-query-edit-value choice (car (gethash pane (get-frame-pane-to-avv-stream-table (pane-frame pane))))))

(define-presentation-to-command-translator edit-avv-pane-choice
    (accept-values-choice com-edit-avv-pane-choice accept-values-pane
     :documentation "Edit this field"
     :pointer-documentation "Edit this field"
     :gesture :edit-field
     :tester ((object presentation)
              (accept-values-query-valid-p object presentation))
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     ;; Echoing this is annoying, as is putting it into the command history
     :echo nil :maintain-history nil)
    (object window)
  (list object window))

(define-command (com-modify-avv-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice)
     (pane 't))
  (accept-values-query-edit-value choice (car (gethash pane (get-frame-pane-to-avv-stream-table (pane-frame pane))))
                        :modify t))

(define-presentation-to-command-translator modify-avv-pane-choice
    (accept-values-choice com-modify-avv-pane-choice accept-values-pane
     :documentation "Modify this field"
     :pointer-documentation "Modify this field"
     :gesture :modify-field
     :tester ((object presentation)
              (accept-values-query-valid-p object presentation))
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object window)
  (list object window))

(define-command (com-delete-avv-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice))
  (accept-values-query-delete-value choice))

(define-presentation-to-command-translator delete-avv-pane-choice
    (accept-values-choice com-delete-avv-pane-choice accept-values-pane
     :documentation "Remove this field"
     :pointer-documentation "Remove this field"
     :tester ((object presentation)
              (accept-values-query-valid-p object presentation))
     :gesture :delete-field
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object)
  (list object))

(define-command (com-avv-pane-command-button :command-table accept-values-pane)
    ((button 'accept-values-command-button)
     (pane 't))
  (funcall (slot-value button 'continuation))
  (when (slot-value button 'resynchronize)
    (let* ((stream-and-record (and (not *frame-layout-changing-p*)
                                   (gethash pane (get-frame-pane-to-avv-stream-table (pane-frame pane)))))
           (avv-stream (car stream-and-record))
           (avv-record (cdr stream-and-record)))
      (when avv-stream
        (letf-globally (((stream-default-view avv-stream) +textual-dialog-view+))
          (redisplay avv-record avv-stream))))))

(define-presentation-to-command-translator avv-pane-command-button
    (accept-values-command-button com-avv-pane-command-button accept-values-pane
     :documentation document-command-button
     :pointer-documentation document-command-button
     :gesture :select
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object window)
  (list object window))

(define-command (com-avv-pane-choose-one-of :command-table accept-values-pane)
    ((choice 'accept-values-one-of))
  (avv-choose-one-of-1 choice))

(define-presentation-to-command-translator avv-pane-choose-one-of
    (accept-values-one-of com-avv-pane-choose-one-of accept-values-pane
     :documentation "Select this value"
     :pointer-documentation "Select this value"
     :gesture :select
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object)
  (list object))

(define-command (com-avv-pane-choose-some-of :command-table accept-values-pane)
    ((choice 'accept-values-some-of))
  (avv-choose-some-of-1 choice))

(define-presentation-to-command-translator avv-pane-choose-some-of
    (accept-values-some-of com-avv-pane-choose-some-of accept-values-pane
     :documentation "De/Select this value"
     :pointer-documentation "De/Select this value"
     :gesture :select
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object)
  (list object))


;;; Fancy gadget-based dialogs...


(defmethod display-exit-boxes ((frame accept-values) stream (view gadget-dialog-view))
  (fresh-line stream)
  (let ((labels (frame-manager-exit-box-labels (frame-manager frame) frame view)))
    (with-slots (exit-boxes) frame
      (updating-output (stream :unique-id stream
                               :cache-value 'exit-boxes)
        (formatting-table (stream :equalize-column-widths nil)
          (dolist (exit-box exit-boxes)
            (with-exit-box-decoded (value label text-style documentation show-as-default)
              exit-box labels
              (when label
                (when text-style
                  (setq text-style `(:text-style ,text-style)))
                (formatting-column (stream)
                  (formatting-cell (stream)
                    (with-output-as-gadget (stream)
                      (apply #'make-pane 'push-button
                             :label label
                             :help-callback documentation
                             :client frame :id value
                             :activate-callback #'handle-exit-box-callback
                             :name :accept-values-exit-button
                             :show-as-default show-as-default
                             text-style))))))))))))

(defun handle-exit-box-callback (gadget)
  (if *editting-field-p*
      (beep *editting-field-p*)
    (let ((id (gadget-id gadget)))
      (case id
        (:exit (com-exit-avv))
        (:abort (com-abort-avv))
        (t (funcall id))))))

;; It's OK that this is only in the ACCEPT-VALUES command table because
;; we're going to execute it manually in the callbacks below
(define-command (com-change-query :command-table accept-values)
    ((query t)
     (new-value t))
  (with-slots (value changed-p) query
    (setf value new-value
          changed-p t)))

(defmethod accept-values-value-changed-callback ((gadget value-gadget) new-value stream query)
  (when (accept-values-query-valid-p query (accept-values-query-presentation query))
    ;; Only call the callback if the query is still valid
    (do-avv-command new-value stream query)))

(defun do-avv-command (new-value client query)
  (let ((sheet (slot-value client 'stream)))
    (process-command-event
      sheet
      (allocate-event 'presentation-event
        :sheet sheet
        :echo nil
        :presentation-type 'command
        :value `(com-change-query ,query ,new-value)
        :frame (slot-value client 'avv-frame)))))

(defmethod accept-values-value-changed-callback ((gadget radio-box) new-value stream query-id)
  (call-next-method gadget (gadget-id new-value) stream query-id))

(defmethod accept-values-value-changed-callback ((gadget check-box) new-value stream query-id)
  (call-next-method gadget (mapcar #'gadget-id new-value) stream query-id))

(defun make-accept-values-value-changed-callback (stream query)
  ;; We attach a callback function directly now
  `(,#'accept-values-value-changed-callback ,stream ,query))

;; This is how we associate an output-record with the button

(defun invoke-accept-values-command-button
    (stream continuation view prompt
     &rest options
     &key (documentation (if (stringp prompt)
                             prompt
                           ;;-- What is the right thing to do?
                           (ignore-errors
                            (with-output-to-string (stream)
                              (funcall prompt stream)))))
          (query-identifier (list ':button documentation))
     &allow-other-keys)
  (typecase view
    (null)
    (symbol (setq view (make-instance view)))
    (cons   (setq view (apply #'make-instance view))))
  (apply #'invoke-accept-values-command-button-1
         stream
         continuation
         (decode-indirect-view
          'accept-values-command-button
          view
          (frame-manager stream)
          :query-identifier query-identifier)
         prompt
         :documentation documentation
         :query-identifier query-identifier
         options))

(define-presentation-method decode-indirect-view
    ((type accept-values-command-button) (view gadget-dialog-view)
                                          (framem standard-frame-manager) &key read-only)
  (declare (ignore read-only))
  +push-button-view+)

(defmethod invoke-accept-values-command-button-1
    (stream continuation (view push-button-view) prompt
     &key (documentation (if (stringp prompt)
                             prompt
                           ;;-- What is the right thing to do?
                           (ignore-errors
                            (with-output-to-string (stream)
                              (funcall prompt stream)))))
          (query-identifier (list ':button documentation))
          (cache-value t) (cache-test #'eql)
          resynchronize
          (active-p t))
  (declare (dynamic-extent prompt))
  (move-cursor-to-view-position stream view)
  (updating-output (stream :unique-id query-identifier :id-test #'equal
                           :cache-value (cons active-p cache-value)
                           :cache-test #'(lambda (x y)
                                           (and (eq (car x) (car y))
                                                (funcall cache-test (cdr x) (cdr y)))))
    (labels ((update-gadget (record gadget)
                        (declare (ignore record))            ;;-- This sucks
                        (setf (gadget-label gadget) (compute-prompt))
                        (if active-p
                            (activate-gadget gadget)
                          (deactivate-gadget gadget)))
             (compute-prompt ()
               (if (stringp prompt)
                   prompt
                 ;;-- Does this suck or what???
                 ;;-- If you do a
                 ;;surrounding-output-with-border without
                 ;;this write-string output does not get bordered.
                 (let ((*original-stream* nil))
                   (apply #'pixmap-from-menu-item
                          stream prompt #'funcall nil
                          :gray-out (not active-p)
                          (view-gadget-initargs view)))))
             (doit (stream)
               (with-output-as-gadget (stream  :update-gadget #'update-gadget)
                   (let ((record (stream-current-output-record (encapsulating-stream-stream stream)))
                         (client (make-instance 'accept-values-command-button
                                                :continuation continuation
                                                :documentation documentation
                                                :resynchronize resynchronize)))
                     (make-pane-from-view
                      'push-button
                      view ()
                      :id record :client client
                      :activate-callback #'(lambda (button)
                                             (when (accept-values-query-valid-p nil record) ;---can't be right
                                               (let ((sheet (sheet-parent button)))
                                                 (process-command-event
                                                  sheet
                                                  (allocate-event 'presentation-event
                                                                  :sheet sheet
                                                                  :echo nil
                                                                  :presentation-type 'command
                                                                  :value `(com-avv-command-button ,client ,record)
                                                                  :frame *application-frame*)))))
                      :label (compute-prompt)
                      :name :accept-values-command-button
                      :help-callback documentation
                      :active active-p)))))
      (let ((align-prompts (slot-value stream 'align-prompts)))
        (if align-prompts
            (formatting-row (stream)
              (formatting-cell (stream) stream)
              (formatting-cell (stream) (doit stream)))
          (doit stream))))))

(defmethod frame-manager-position-dialog ((framem standard-frame-manager)
                                                          frame
                                                          own-window-x-position own-window-y-position)
  (multiple-value-bind (x y)
      #+++ignore (pointer-position (port-pointer (port frame)))
      #---ignore (values 100 100)
      (when (and own-window-x-position own-window-y-position)
        (setq x own-window-x-position
              y own-window-y-position))
      (position-sheet-carefully
       (frame-top-level-sheet frame) x y)))
