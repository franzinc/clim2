;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;;; Implementation of output-records that are tied to gadgets.

;;--- Why are these OUTPUT-RECORD-MIXINs?
(defclass gadget-output-record
          (output-record-mixin output-record-element-mixin output-record)
    ((gadget :initform nil :accessor output-record-gadget)
     (cache-value :initarg :cache-value :initform nil)
     (optional-values :initform nil)))

(defmethod initialize-instance :after ((record gadget-output-record) &key cache-test size)
  ;; Accept :SIZE and :CACHE-TEST as initargs
  (declare (ignore size cache-test)))

(define-output-record-constructor gadget-output-record
                                  (&key x-position y-position (size 25) cache-value)
  :x-position x-position :y-position y-position :size size
  :cache-value cache-value)

(defmethod match-output-records ((record gadget-output-record)
                                 &key (cache-value nil cache-value-p) (cache-test #'equal))
  (and cache-value-p
       (funcall cache-test cache-value (slot-value record 'cache-value))))

(defmethod associate-record-and-gadget (record gadget stream x y)
  ;; Just in case
  (setf (sheet-enabled-p gadget) nil)
  (sheet-adopt-child stream gadget)
  ;; In order to be able to determine the space the widget has to be
  ;; added to the parent and hopefully grafted
  (assert (port stream))
  (setf (output-record-gadget record) gadget)
  (let ((sr (compose-space gadget)))
    (multiple-value-bind (abs-x abs-y)
        (point-position
          (stream-output-history-position stream))
      (decf x abs-x)
      (decf y abs-y)
      (multiple-value-bind (cx cy)
          (stream-cursor-position stream)
        (declare (type coordinate cx cy))
        (with-slots (start-x start-y) record
          (setq start-x (- cx abs-x)
                start-y (- cy abs-y))))
      (bounding-rectangle-set-edges
        record
        x y (+ x (space-requirement-width sr)) (+ y (space-requirement-height sr)))
      ;;--- We can probably use RECOMPUTE-EXTENT-FOR-CHANGED-CHILD
      (when (output-record-parent record)
         (tree-recompute-extent record)))
    (when (output-record-stream record)
      (update-gadget-position record)
      (setf (sheet-enabled-p gadget) t))))

;; Three ways

(defmethod note-output-record-moved ((record gadget-output-record) dx1 dy1 dx2 dy2)
  (declare (ignore dx1 dy1 dx2 dy2))
  (update-gadget-position record))

(defmethod bounding-rectangle-set-edges :after ((record gadget-output-record)
                                                left top right bottom)
  (declare (ignore left top right bottom))
  (update-gadget-position record))

(defmethod bounding-rectangle-set-position :after ((record gadget-output-record) x y)
  (declare (ignore x y))
  (update-gadget-position record))

(defmethod bounding-rectangle-set-size :after ((record gadget-output-record) width height)
  (declare (ignore width height))
  (update-gadget-position record))

(defmethod tree-recompute-extent-1 ((record gadget-output-record))
  (bounding-rectangle* record))

#+++ignore
(defmethod update-gadget-position ((record gadget-output-record))
  (let ((gadget (output-record-gadget record)))
    (when gadget
      (with-bounding-rectangle* (left top right bottom) record
        (let ((xoff (coordinate 0))
              (yoff (coordinate 0)))
          (multiple-value-setq (xoff yoff)
            (convert-from-relative-to-absolute-coordinates
              (sheet-parent gadget) record))
          (move-and-resize-sheet gadget
                                 (+ left xoff) (+ top yoff)
                                 (- right left) (- bottom top)))))))

#---ignore
(defmethod update-gadget-position ((record gadget-output-record))
  (let ((gadget (output-record-gadget record)))
    (when (and gadget (output-record-stream record))
      (multiple-value-bind (xoff yoff)
          (convert-from-relative-to-absolute-coordinates
            (output-record-stream record) (output-record-parent record))
        (with-bounding-rectangle* (left top right bottom) record
          (translate-positions xoff yoff left top right bottom)
          (move-and-resize-sheet
            gadget left top (- right left) (- bottom top)))))))

;;--- Flush this when REPLAY-OUTPUT-RECORD calls itself recursively
(defmethod note-output-record-replayed ((record gadget-output-record) stream
                                        &optional region x-offset y-offset)
  (declare (ignore stream x-offset y-offset))
  (let ((gadget (output-record-gadget record)))
    (when (port gadget)
      (repaint-sheet gadget region))))


;; Need to add the gadget to the stream

;; One problem we have is that when we scroll we will end up with
;; geometry requests being refused.
;; We want to enable/disable panes as they become (in)visible


(defmethod window-clear :after ((window clim-stream-sheet))
  (dolist (child (sheet-children window))
    (sheet-disown-child window child)))


(defmacro with-output-as-gadget ((stream &rest options) &body body)
  (default-output-stream stream)
  (setq options (remove-keywords options '(:stream)))
  (let ((framem '#:framem)
        (frame '#:frame))
    `(flet ((with-output-as-gadget-body (,framem ,frame)
              (with-look-and-feel-realization (,framem ,frame)
                ,@body)))
       (declare (dynamic-extent #'with-output-as-gadget-body))
       (invoke-with-output-as-gadget
         ,stream #'with-output-as-gadget-body ,@options))))

#+++ignore
(defmethod invoke-with-output-as-gadget (stream continuation &key)
  ;;--- (PANE-FRAME STREAM) or *APPLICATION-FRAME*?
  (let* ((frame (pane-frame stream))
         (framem (frame-manager frame)))
    (assert (and frame framem) ()
            "There must be both a frame and frame manager active")
    (multiple-value-bind (x y) (stream-cursor-position stream)
      (let* (gadget
             (record
               (with-output-recording-options (stream :draw nil :record t)
                 (with-new-output-record (stream 'gadget-output-record record)
                   ;;--- Do we really need to do this?  Under what
                   ;;--- situations does this happen and why can't we
                   ;;--- release the gadget?
                   (unless (setq gadget (output-record-gadget record))
                     (let ((values (multiple-value-list (funcall continuation framem frame))))
                       (setf (slot-value record 'optional-values) (cdr values))
                       (associate-record-and-gadget
                         record
                         (setq gadget (car values))
                         stream x y)))))))
        (replay record stream)
        (move-cursor-beyond-output-record stream record)
        (values gadget record)))))
            
#---ignore
(defmethod invoke-with-output-as-gadget (stream continuation &rest args 
                                         &key cache-test cache-value
					      update-gadget)
  (declare (dynamic-extent args)
	   (ignore cache-test cache-value))
  (with-keywords-removed (args args '(:update-gadget))
    (let* ((frame (pane-frame stream))
           (framem (frame-manager frame)))
      (assert (and frame framem) ()
              "There must be both a frame and frame manager active")
      (multiple-value-bind (x y) (stream-cursor-position stream)
        (let* (new
               gadget
               (record
                 (with-output-recording-options (stream :draw nil :record t)
                   (flet ((with-new-output-record-body (record)
                            (unless (setq gadget (output-record-gadget record))
                              (let ((values (multiple-value-list
                                              (funcall continuation framem frame))))
                                (setf (slot-value record 'optional-values) (cdr values))
                                (setq gadget (car values)
                                      new t)))))
                     (declare (dynamic-extent #'with-new-output-record-body))
                     (apply #'invoke-with-new-output-record
                            stream #'with-new-output-record-body
                            'gadget-output-record #'gadget-output-record-constructor
                            args)))))
          (cond (new
                 (associate-record-and-gadget record gadget stream x y))
                (update-gadget
                 (apply update-gadget record gadget (slot-value record 'optional-values))))
          (replay record stream)
          (move-cursor-beyond-output-record stream record)
          (values gadget record))))))


;;--- Incremental redisplay seems to need this
(defmethod clear-output-record ((record gadget-output-record))
  nil)

;; kludge because of clear-output-record :after
;; Perhaps this should be moved somewhere.
;;  If think is because the clear-output-record stuff should be on
;;  composite-output-records rather than displayed output-records

(defmethod bounding-rectangle-set-edges        :around ((record gadget-output-record)
                                                 left top right bottom)
  (unless (= left top right bottom)
    (call-next-method)))

#+++ignore
(defmethod add-output-record :after ((record gadget-output-record) (parent t))
  (do ((p parent (output-record-parent p)))
      ((null p)
       (warn "Parent is not a output-history"))
    (when (typep p 'stream-output-history-mixin)
      (return t)))
  ;; Perhaps we should enable the gadget at this point
  nil)

(defmethod note-output-record-attached :after ((record gadget-output-record) stream)
  (declare (ignore stream))
  (when (and (output-record-gadget record)
              ;;--- Why?
              (port (output-record-gadget record)))
    (update-gadget-position record)
    (update-output-record-gadget-state record t)))

(defmethod note-output-record-detached :after ((record gadget-output-record))
  (update-output-record-gadget-state record nil))

(defun move-cursor-to-view-position (stream view)
  (let* ((initargs (view-gadget-initargs view))
         (x (getf initargs :x))
         (y (getf initargs :y)))
    (when (and x y)
      (stream-set-cursor-position stream x y))))


(defvar *with-deferred-gadget-updates* nil)

;;--- This ain't pretty, but neither are you.
(defmacro with-deferred-gadget-updates (&body body)
  `(flet ((with-deferred-gadget-updates-body ()
            ,@body))
     (declare (dynamic-extent #'with-deferred-gadget-updates-body))
     (if *with-deferred-gadget-updates*
         (with-deferred-gadget-updates-body)
         (let ((*with-deferred-gadget-updates* (list nil)))
           (multiple-value-prog1
             (with-deferred-gadget-updates-body)
             (complete-gadget-updates))))))

(defun complete-gadget-updates ()
  (mapc #'(lambda (x)
            (setf (sheet-enabled-p (first x)) (second x)))
        (remove-duplicates (cdr *with-deferred-gadget-updates*)
                           :key #'car :from-end t)))

(defun update-output-record-gadget-state (record state)
  (if *with-deferred-gadget-updates*
      (push (list (output-record-gadget record) state)
            (cdr *with-deferred-gadget-updates*))
      (setf (sheet-enabled-p (output-record-gadget record)) state)))

#+(or aclpc acl86win32)
(define-presentation-method gadget-includes-prompt-p 
    ((type completion) (stream t) (view radio-box-view))
  (not (null (getf (view-gadget-initargs view) :label))))

(define-presentation-method gadget-includes-prompt-p :around 
  ((type t) (stream t) (view actual-gadget-view))
  (let ((initargs (view-gadget-initargs view)))
    (or (getf initargs :label)
        (not (getf initargs :prompt t))
        (call-next-method))))


;;; Completion gadget

(define-presentation-method decode-indirect-view
                            ((type completion) (view gadget-dialog-view)
                             (framem standard-frame-manager) &key read-only)
  ;;--- We can be clever and return different views depending on the
  ;;--- number of items, etc.
  (if read-only
      (call-next-method)
    +radio-box-view+))

(define-presentation-method accept-present-default
    ((type completion) stream (view radio-box-view)
                       default default-supplied-p present-p query-identifier
                       &key (prompt t) (active-p t))
  (declare (ignore present-p prompt))
  (move-cursor-to-view-position stream view)
  (let ((current-selection nil))
    (flet ((update-gadget (record gadget radio-box)
             (declare (ignore gadget record))
             ;;--- This sucks
             (if active-p
                 (activate-gadget radio-box)
               (deactivate-gadget radio-box))
             (map-over-sheets
              #'(lambda (sheet)
                  (when (typep sheet 'toggle-button)
                    (when (setf (gadget-value sheet)
                            (and default-supplied-p
                                 (funcall test (gadget-id sheet) default)))
                      (setf (radio-box-current-selection radio-box) sheet))))
              radio-box)))
      (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
        (let* ((toggle-options
                (getf (view-gadget-initargs view) :toggle-button-options))
               (radio-box-label
                (getf (view-gadget-initargs view) :label))
               (label-options
                (getf (view-gadget-initargs view) :label-options))
               (borders
                (getf (view-gadget-initargs view) :borders t))
               (buttons
                (map 'list
                  #'(lambda (element)
                      (let* ((value (funcall value-key element))
                             (button
                              (apply #'make-pane 'toggle-button
                                     :label
                                     (let ((name (funcall name-key element)))
                                       (if (eq printer #'write-token)
                                           name
                                         (apply #'pixmap-from-menu-item
                                                stream name printer nil
                                                :gray-out (not active-p)
                                                (view-gadget-initargs view))))
                                     :indicator-type
                                     (getf toggle-options :indicator-type :one-of)
                                     :value
                                     (and default-supplied-p
                                          (funcall test value default))
                                     :id value
                                     toggle-options)))
                        (when (and default-supplied-p
                                   (funcall test value default))
                          (setq current-selection button))
                        button))
                  sequence))
               (radio-box
                (make-pane-from-view
                 'radio-box view
                 '(:toggle-button-options :label
                   :label-options :borders)
                 :choices buttons
                 :current-selection current-selection
                 :client stream
                 :id query-identifier
                 :value-changed-callback (make-accept-values-value-changed-callback
                                          stream query-identifier)
                 :active active-p
                 :help-callback (make-gadget-help type))))
          (values (let ((labelled-radio-box
                         (if radio-box-label
                             (vertically ()
                               (apply #'make-pane 'label-pane
                                      :label radio-box-label
                                      label-options)
                               radio-box)
                           radio-box)))
                    (if borders
                        (outlining ()
                          labelled-radio-box)
                      labelled-radio-box))
                  radio-box))))))


;;; Subset completion gadget

(define-presentation-method decode-indirect-view
                            ((type subset-completion) (view gadget-dialog-view)
                             (framem standard-frame-manager) &key read-only)
  (if read-only
      (call-next-method)
      +check-box-view+))

#+(or aclpc acl86win32)
(define-presentation-method gadget-includes-prompt-p 
    ((type subset-completion) (stream t) (view check-box-view))
  (not (null (getf (view-gadget-initargs view) :label))))

(define-presentation-method accept-present-default 
                            ((type subset-completion) stream (view check-box-view)
                             default default-supplied-p present-p query-identifier
                             &key (prompt t) (active-p t))
  (declare (ignore present-p prompt))
  (move-cursor-to-view-position stream view)
  (flet ((update-gadget (record gadget check-box)
           (declare (ignore record gadget))
           ;;--- This sucks
           (if active-p
               (activate-gadget check-box)
             (deactivate-gadget check-box))
           (let ((current-selection nil))
             (map-over-sheets
              #'(lambda (sheet)
                  (when (typep sheet 'toggle-button)
                    (let ((value
                           (and default-supplied-p
                                (member (gadget-id sheet) default
                                        :test test)
                                t)))
                      (when value (push sheet current-selection))
                      (setf (gadget-value sheet) value))))
              check-box)
             (setf (check-box-current-selection check-box) current-selection))))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let* ((toggle-options
              (getf (view-gadget-initargs view) :toggle-button-options))
             (check-box-label
              (getf (view-gadget-initargs view) :label))
             (label-options
              (getf (view-gadget-initargs view) :label-options))
             (borders
              (getf (view-gadget-initargs view) :borders t))
             (current-selection nil)
             (buttons
              (map 'list
                #'(lambda (element)
                    (let* ((value (funcall value-key element))
                           (actual-value (and default-supplied-p
                                              (member value default
                                                      :test test)
                                              t))
                           (button
                            (apply #'make-pane 'toggle-button
                                   :label
                                   (let ((name (funcall name-key element)))
                                     (if (eq printer #'write-token)
                                         name
                                       (apply #'pixmap-from-menu-item
                                              stream name printer nil
                                              :gray-out (not active-p)
                                              (view-gadget-initargs view))))
                                   :indicator-type
                                   (getf toggle-options :indicator-type :some-of)
                                   :value actual-value
                                   :id value
                                   toggle-options)))
                      (when actual-value (push button current-selection))
                      button))
                sequence))
             (check-box
              (make-pane-from-view 'check-box view
                                   '(:toggle-button-options :label
                                     :label-options :borders)
                                   :choices buttons
                                   :current-selection current-selection
                                   :client stream
                                   :id query-identifier
                                   :value-changed-callback (make-accept-values-value-changed-callback
                                                            stream query-identifier)
                                   :active active-p
                                   :help-callback (make-gadget-help type))))
        (values (let ((labelled-check-box
                       (if check-box-label
                           (vertically ()
                             (apply #'make-pane 'label-pane
                                    :label check-box-label
                                    label-options)
                             check-box)
                         check-box)))
                  (if borders
                      (outlining ()
                        labelled-check-box)
                    labelled-check-box))
                check-box)))))


;;; Boolean gadget

(define-presentation-method decode-indirect-view
                            ((type boolean) (view gadget-dialog-view)
                             (framem standard-frame-manager) &key read-only)
  (if read-only
      (call-next-method)
      +toggle-button-view+))

#+(or aclpc acl86win32)
(define-presentation-method gadget-includes-prompt-p 
    ((type boolean) (stream t) (view toggle-button-view))
  t)

(define-presentation-method accept-present-default 
    ((type boolean) stream (view toggle-button-view)
		    default default-supplied-p present-p query-identifier
		    &key (prompt t) (active-p t))
  (declare (ignore default-supplied-p present-p
		   #-(or aclpc acl86win32) prompt))
  (move-cursor-to-view-position stream view)
  (flet ((update-gadget (record gadget button)
	   (declare (ignore record gadget))
	   ;;--- This sucks
           (if active-p
               (activate-gadget button)
             (deactivate-gadget button))
	   (setf (gadget-value button) default)))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let ((borders
             (getf (view-gadget-initargs view) :borders t))
            (button (make-pane-from-view
                     'toggle-button view
                     '(:borders)
                     #+(or aclpc acl86win32) :label
		     #+(or aclpc acl86win32) (and (stringp prompt) prompt)
		     :value default
                     :client stream :id query-identifier
                     :value-changed-callback
                     (make-accept-values-value-changed-callback
                      stream query-identifier)
                     :active active-p
                     :help-callback (make-gadget-help type))))
	(values (if borders
                    (outlining () button)
                  button)
                button)))))


;;; Numeric gadgets

#+(or aclpc acl86win32)
(define-presentation-method gadget-includes-prompt-p
                            ((type real) (stream t) (view slider-view))
  t)

(define-presentation-method accept-present-default 
                            ((type real) stream (view slider-view)
                             default default-supplied-p present-p query-identifier
                             &key (prompt t) (active-p t))
  (declare (ignore present-p))
  (make-real-gadget-for-slider-view stream view active-p default
                                    type prompt query-identifier
                                    low high default-supplied-p t))

(define-presentation-method present
                            (object (type real) stream (view slider-view)
                             &key acceptably query-identifier prompt)
  (declare (ignore acceptably))
  (make-real-gadget-for-slider-view stream view t object
                                    type prompt query-identifier
                                    low high t nil))

(defun make-real-gadget-for-slider-view (stream view active-p default
                                         type prompt query-identifier
                                         low high default-supplied-p
                                         &optional (editable-p t))
  #-(or aclpc acl86win32) (declare (ignore prompt))
  (move-cursor-to-view-position stream view)
  (let ((min-value (if (eq low '*) 0 low))
        (max-value (if (eq high '*) 100 high)))
    (flet ((update-gadget (record gadget slider)
             (declare (ignore record gadget))
             ;;--- This sucks
             (if active-p
                 (activate-gadget slider)
                 (deactivate-gadget slider))
             (setf (gadget-value slider)
                   (if default-supplied-p default min-value))))
      (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
        (let ((borders
               (getf (view-gadget-initargs view) :borders t))
              (slider
                ;;--- What other initargs do we pass along from the view?
               (make-pane-from-view
                'slider view
                '(:borders)
                #+(or aclpc acl86win32) :label
		#+(or aclpc acl86win32) (and (stringp prompt) prompt)
                :value (if default-supplied-p default min-value)
                :min-value min-value :max-value max-value
                :orientation (gadget-orientation view)
                :decimal-places (or (slider-decimal-places view) 0)
                :show-value-p (gadget-show-value-p view)
                :client stream :id query-identifier
                :value-changed-callback
                (and editable-p
                     (make-accept-values-value-changed-callback
                      stream query-identifier))
                :active active-p
                :editable-p editable-p
                :help-callback (make-gadget-help type))))
          (values (if borders
                      (outlining () slider)
                    slider)
                  slider))))))

(define-presentation-method accept-present-default
                            ((type integer) stream (view slider-view)
                             default default-supplied-p present-p query-identifier
                             &key (prompt t) (active-p t))
  (declare (ignore present-p))
  (make-integer-gadget-for-slider-view stream view active-p default
                                       type prompt query-identifier
                                       low high default-supplied-p t))

(define-presentation-method present
                            (object (type integer) stream (view slider-view) 
                             &key acceptably query-identifier prompt)
  (declare (ignore acceptably))
  (make-integer-gadget-for-slider-view stream view t object
                                       type prompt query-identifier
                                       low high t nil))

(defun make-integer-gadget-for-slider-view (stream view active-p default
                                            type prompt query-identifier
                                            low high default-supplied-p
                                            &optional (editable-p t))
  #-(or aclpc acl86win32) (declare (ignore prompt))
  (move-cursor-to-view-position stream view)
  (let ((min-value (if (eq low '*) 0 low))
        (max-value (if (eq high '*) 100 high)))
    (flet ((update-gadget (record gadget slider)
             (declare (ignore record gadget))
             ;;--- This sucks
             (if active-p
                 (activate-gadget slider)
               (deactivate-gadget slider))
             (setf (gadget-value slider)
               (if default-supplied-p default min-value))))
      (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
        (let ((borders
               (getf (view-gadget-initargs view) :borders t))
              (slider
               ;;--- What other initargs do we pass along from the view?
               (make-pane-from-view
                'slider view
                '(:borders)
                #+(or aclpc acl86win32) :label
		#+(or aclpc acl86win32) (and (stringp prompt) prompt)
                :value (if default-supplied-p default min-value)
                :min-value min-value :max-value max-value
                :orientation (gadget-orientation view)
                :number-of-quanta (- max-value min-value)
                :decimal-places 0
                :show-value-p (gadget-show-value-p view)
                :client stream :id query-identifier
                :value-changed-callback
                (and editable-p
                     (make-accept-values-value-changed-callback
                      stream query-identifier))
                :active active-p
                :editable-p editable-p
                :help-callback (make-gadget-help type))))
          (values (if borders
                      (outlining () slider)
                    slider)
                  slider))))))


;;; Text Field gadget

(defmacro define-decode-indirect-view-methods-for-types ((view1 view2) &body types)
  `(progn
     ,@(mapcar #'(lambda (type)
                   `(define-presentation-method decode-indirect-view
                        ((type ,type) (view ,view1)
                                      (framem standard-frame-manager) &key)
                      ,view2))
               types)))

(define-decode-indirect-view-methods-for-types
    (gadget-dialog-view +text-field-view+)
    real string pathname sequence sequence-enumerated)

;; Boring.  We need a whole bunch of methods to shadow those in STANDARD-TYPES:
(defmacro define-present-methods-for-types ((function view) &body types)
  `(progn
     ,@(mapcar #'(lambda (type)
                   `(define-presentation-method present 
                        (object (type ,type) stream (view ,view) 
                         &key acceptably query-identifier prompt)
                      (declare (ignore acceptably))
                      (,function stream view t object t type prompt query-identifier :editable-p nil)))
               types)))

;;--- Should there be a method specialized on T???
(define-present-methods-for-types
    (make-gadget-for-text-field-view text-field-view)
  real string pathname boolean
  completion subset-completion
  sequence sequence-enumerated)

(define-presentation-method accept-present-default
                            ((type t) stream (view text-field-view)
                             default default-supplied-p present-p query-identifier
                             &key (prompt t) (active-p t))
  (declare (ignore present-p))
  (make-gadget-for-text-field-view
    stream view active-p default default-supplied-p type prompt query-identifier :editable-p t))

(defun make-gadget-for-text-field-view (stream view active-p default
                                        default-supplied-p type
                                        prompt query-identifier
                                        &key (editable-p t))
  (declare (ignore prompt))
  (move-cursor-to-view-position stream view)
  (flet ((update-gadget (record gadget text-field)
	   (declare (ignore record gadget))             ;;-- This sucks
           (if active-p
               (activate-gadget text-field)
	     (deactivate-gadget text-field))
	   (setf (gadget-value text-field)
             (if default-supplied-p
                 (present-to-string default type)
               ""))))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let ((text-field (make-pane-from-view 
			 'text-field view ()
			 :value (if default-supplied-p
				    (present-to-string default type)
				  "")
			 :client stream :id query-identifier
			 :editable-p editable-p
			 :value-changed-callback
			 `(accept-values-note-text-field-changed-callback
			   ,query-identifier)
			 :focus-out-callback
			 ;;--- Why do we check for editable-p?  If it
			 ;;--- is read-only, won't this callback never be
			 ;;--- called? (cim)
			 (and editable-p
			      `(accept-values-string-field-changed-callback
				,stream ,query-identifier))
			 :activate-callback
			 (and editable-p
			      `(accept-values-string-field-changed-callback
				,stream ,query-identifier))
			 :active active-p
			 :help-callback (make-gadget-help type))))
	(values text-field text-field)))))

(defmethod accept-values-note-text-field-changed-callback
           ((gadget text-field) new-value query)
  (declare (ignore new-value))
  ;; NB query should never be nil but this test was put in for the
  ;; windows CLIM port when apparently it sometimes is nil (cim 1/30/97)
  (if query (setf (accept-values-query-changed-p query) t)))



(defmethod accept-values-string-field-changed-callback
           ((gadget text-field) stream query)
  (let ((new-value (gadget-value gadget)))
    ;; Only do the accept when the field has changed
    ;; Only call the callback if the query is still valid
    (when (and query
               (accept-values-query-changed-p query)
               (accept-values-query-valid-p
                 query (accept-values-query-presentation query)))
      (handler-case
          (let ((ptype (accept-values-query-type query)))
            (clim:with-presentation-type-decoded (name)
              ptype
              (if (eql name 'string)
                  ;; [spr30294]  Accept the raw text-field contents
                  ;; when we're expecting a string (alemmens, 2005-06-21)
                  (accept-raw-string ptype new-value)
                  ;; Normal case: use accept-from-string.
                  (multiple-value-bind (object type index)
                      ;; we specify :activation-gestures nil so that we can
                      ;; accept multi-line strings in text-editors (cim)
                       (accept-from-string (accept-values-query-type query)
                                          new-value
                                          :activation-gestures nil
                                          :delimiter-gestures nil)
                    (declare (ignore type))
                    (assert (= index (length new-value)))
                    object))))
        (error (c)
          (setf (accept-values-query-error-p query) c)
          (let ((frame (pane-frame stream)))
            (unless (typep *application-frame* 'accept-values)
              (verify-queries-1 frame stream (list query))
              (setf (accept-values-query-changed-p query) nil))))
        (:no-error (object)
         (setf (accept-values-query-error-p query) nil)
         (do-avv-command object stream query))))))
 

(defun accept-raw-string (ptype gadget-value)
  ;; [spr30294] Don't call ACCEPT-FROM-STRING when we just want to
  ;; accept a string, because it will filter initial double quote
  ;; characters from the string and we don't want that for text-field
  ;; gadgets. (alemmens, 2005-06-21)
  (with-presentation-type-parameters (string ptype)
    (if (or (eq length '*) (= (length gadget-value) length))
        gadget-value
        (input-not-of-required-type gadget-value ptype))))



;;; Text Editor gadget

(define-presentation-method accept-present-default
                            ((type t) stream (view text-editor-view)
                             default default-supplied-p present-p query-identifier
                             &key (prompt t) (active-p t))
  (declare (ignore present-p))
  (make-gadget-for-text-editor stream view active-p default default-supplied-p
                               type prompt query-identifier :editable-p t))

(define-present-methods-for-types
    (make-gadget-for-text-editor text-editor-view)
    string)

(defun make-gadget-for-text-editor (stream view active-p default
                                    default-supplied-p type
                                    prompt query-identifier
                                    &key (editable-p t))
  (declare (ignore prompt))
  (move-cursor-to-view-position stream view)
  (flet ((update-gadget (record gadget button)
	   (declare (ignore record gadget))
           (if active-p
               (activate-gadget button)
	     (deactivate-gadget button))
	   (setf (gadget-value button)
             (if default-supplied-p
                 (present-to-string default type)
               ""))))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let ((text-field (make-pane-from-view 
			 'text-editor view '(:scroll-bars)
			 :value (if default-supplied-p
				    (present-to-string default type)
				  "")
			 :client stream :id query-identifier
			 :editable-p editable-p
			 :value-changed-callback
			 `(accept-values-note-text-field-changed-callback
			   ,query-identifier)
			 :focus-out-callback
			 ;;--- Why do we check for editable-p?  If it
			 ;;--- is read-only, won't this callback never be
			 ;;--- called? (cim)
			 (and editable-p
			      `(accept-values-string-field-changed-callback
				,stream ,query-identifier))
			 :activate-callback
			 (and editable-p
			      `(accept-values-string-field-changed-callback
				,stream ,query-identifier))
			 :active active-p
			 :help-callback (make-gadget-help type))))
	(values (scrolling (:scroll-bars (getf (view-gadget-initargs view) :scroll-bars :both))
                  text-field)
                text-field)))))


;;; List and option panes

(define-presentation-method accept-present-default 
                            ((type completion) stream (view list-pane-view)
                             default default-supplied-p present-p query-identifier
                             &key (prompt t) (active-p t))
  (declare (ignore present-p default-supplied-p prompt))
  (make-list/option-pane-for-ptype 'list-pane
                                   stream view sequence
                                   name-key value-key test
                                   default query-identifier type printer
                                   :exclusive active-p))

(define-presentation-method accept-present-default 
                            ((type subset-completion) stream (view list-pane-view)
                             default default-supplied-p present-p query-identifier
                             &key (prompt t) (active-p t))
  (declare (ignore present-p default-supplied-p prompt))
  (make-list/option-pane-for-ptype 'list-pane 
                                   stream view sequence
                                   name-key value-key test
                                   default query-identifier type printer
                                   :nonexclusive active-p))


(defun find-stream-query (avv-stream query-identifier)
  ;; spr 30639 (alemmens, 2005-11-30)
  (if (typep query-identifier 'accept-values-query)
      query-identifier
      (let ((avv-record (slot-value avv-stream 'avv-record)))
        (with-slots (query-table)
            avv-record
          (gethash query-identifier query-table)))))

(defmacro make-pane-from-view-2 (class view state ignore &body initargs)
  ;; spr 30639 (alemmens, 2005-11-30)
  `(apply #'make-pane ,class
          (append (remove-keywords (append ,state (view-gadget-initargs ,view)) ,ignore)
                  (list ,@initargs))))

(defun make-list/option-pane-for-ptype (pane-type stream view sequence
                                        name-key value-key test
                                        default query-identifier type
                                        printer mode active-p)
  (move-cursor-to-view-position stream view)
  (flet ((update-gadget (record gadget list-pane)
           (declare (ignore gadget record))
           (if active-p
               (activate-gadget list-pane)
             (deactivate-gadget list-pane))
           (setf (gadget-value list-pane) default)
           list-pane))
    (let* ((query (find-stream-query stream query-identifier))
           ;; spr 30639 (alemmens, 2005-11-30)
           (gadget-state (and query (accept-values-query-gadget-state query))))
      (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
        (let* ((pane (if (eq pane-type 'option-pane)
                         (make-pane-from-view-2 pane-type view gadget-state ()
                           :items sequence
                           :name-key name-key
                           :value-key value-key
                           :test test
                           :printer printer
                           :value default
                           :client stream :id query-identifier
                           :mode mode
                           :visible-items (min 10 (length sequence))
                           :value-changed-callback
                           (make-accept-values-value-changed-callback
                            stream query-identifier)
                           :active active-p
                           :help-callback (make-gadget-help type))
                         (make-pane-from-view-2 pane-type view gadget-state '(:scroll-bars)
                           :items sequence
                           :name-key name-key
                           :value-key value-key
                           :test test
                           :value default
                           :client stream :id query-identifier
                           :mode mode
                           :visible-items (min 10 (length sequence))
                           :value-changed-callback
                           (make-accept-values-value-changed-callback
                            stream query-identifier)
                           :active active-p
                           :help-callback (make-gadget-help type)))))
          (values (if (eq pane-type 'list-pane)
                      (scrolling (:scroll-bars (getf (view-gadget-initargs view) :scroll-bars :dynamic)) pane)
                      pane)
                  pane))))))


(define-presentation-method accept-present-default 
                            ((type completion) stream (view option-pane-view)
                             default default-supplied-p present-p query-identifier
                             &key (prompt t) (active-p t))
  (declare (ignore present-p default-supplied-p prompt))
  (make-list/option-pane-for-ptype 'option-pane
                                   stream view sequence name-key value-key
                                   test default query-identifier type 
                                   printer
                                   :exclusive active-p))

;;--- These should be defined in the standard DEFOPERATION way...

(defmethod sheet-region ((stream standard-encapsulating-stream))
  (sheet-region (encapsulating-stream-stream stream)))

(defmethod sheet-transformation ((stream standard-encapsulating-stream))
  (sheet-transformation (encapsulating-stream-stream stream)))

(defmethod sheet-parent ((stream standard-encapsulating-stream))
  (sheet-parent (encapsulating-stream-stream stream)))

(defmethod sheet-children ((stream standard-encapsulating-stream))
  (sheet-children (encapsulating-stream-stream stream)))

(defmethod sheet-medium ((stream standard-encapsulating-stream))
  (sheet-medium (encapsulating-stream-stream stream)))

(defmethod port ((stream standard-encapsulating-stream))
  (port (encapsulating-stream-stream stream)))

(defmethod graft ((stream standard-encapsulating-stream))
  (graft (encapsulating-stream-stream stream)))

(defmethod sheet-adopt-child ((stream standard-encapsulating-stream) child)
  (sheet-adopt-child (encapsulating-stream-stream stream) child))

(defmethod sheet-disown-child ((stream standard-encapsulating-stream) child)
  (sheet-disown-child (encapsulating-stream-stream stream) child))

;;

(defun make-gadget-help (ptype)
  `(make-help-from-presentation-type ,ptype))
