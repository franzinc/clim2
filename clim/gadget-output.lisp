;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: gadget-output.lisp,v 1.28 92/09/22 19:37:13 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


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
	  (with-bounding-rectangle* (gleft gtop gright gbottom) gadget
	    (unless (and (= left gleft)
			 (= top  gtop)
			 (= right  gright)
			 (= bottom gbottom))
	      (move-and-resize-sheet gadget
				     left top (- right left) (- bottom top)))))))))

;;--- Flush this when REPLAY-OUTPUT-RECORD calls itself recursively
(defmethod note-output-record-replayed ((record gadget-output-record) stream
					&optional region x-offset y-offset)
  (declare (ignore stream x-offset y-offset))
  (let ((gadget (output-record-gadget record)))
    (when (port gadget)
      (repaint-sheet gadget nil region))))


;; Need to add the gadget to the stream

;; One problem we have is that when we scroll we will end up with
;; geometry requests being refused.
;; We want to enable/disable panes as they become (in)visible


(defmethod window-clear :after ((window clim-stream-sheet))
  (dolist (child (sheet-children window))
    (sheet-disown-child window child)))
  

(defmacro with-output-as-gadget ((stream &rest args) &body body)
  (default-output-stream stream)
  (let ((framem '#:framem)
	(frame '#:frame))
    (with-keywords-removed (args args '(:stream))
      `(flet ((with-output-as-gadget-body (,framem ,frame)
		(with-look-and-feel-realization (,framem ,frame)
		  ,@body)))
	 (declare (dynamic-extent #'with-output-as-gadget-body))
	 (invoke-with-output-as-gadget 
	   ,stream #'with-output-as-gadget-body ,@(evacuate-list args))))))

#+++ignore
(defmethod invoke-with-output-as-gadget (stream continuation &key)
  ;;--- (PANE-FRAME STREAM) or *APPLICATION-FRAME*?
  (let* ((frame (pane-frame stream))
	 (framem (frame-manager frame)))
    (assert frame)
    (assert framem)
    (multiple-value-bind (x y) (stream-cursor-position stream)
      (let* (gadget
	     (record
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
		       stream x y))))))
	(move-cursor-beyond-output-record stream record)
	(values gadget record)))))
	    
#---ignore
(defmethod invoke-with-output-as-gadget (stream continuation &rest args 
					 &key cache-test cache-value update-gadget)
  (declare (dynamic-extent args))
  (declare (ignore cache-test cache-value))
  (with-keywords-removed (args args '(:update-gadget))
    (let* ((frame (pane-frame stream))
	   (framem (frame-manager frame)))
      (assert frame)
      (assert framem)
      (multiple-value-bind (x y) (stream-cursor-position stream)
	(let* (new
	       gadget
	       (record
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
			  args))))
	  (cond (new
		 (associate-record-and-gadget record gadget stream x y))
		(update-gadget
		 (apply update-gadget record gadget (slot-value record 'optional-values))))
	  (move-cursor-beyond-output-record stream record)
	  (values gadget record))))))


;;--- Incremental redisplay seems to need this
(defmethod clear-output-record ((record gadget-output-record))
  nil)

;; kludge because of clear-output-record :after
;; Perhaps this should be moved somewhere.
;;  If think is because the clear-output-record stuff should be on
;;  composite-output-records rather than displayed output-records 

(defmethod bounding-rectangle-set-edges	:around ((record gadget-output-record)
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
  (when (output-record-gadget record)
    (update-gadget-position record)
    (update-output-record-gadget-state record t)))

(defmethod note-output-record-detached :after ((record gadget-output-record))
  (update-output-record-gadget-state record nil))


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


;;; Completion gadget

(define-presentation-method decode-indirect-view
			    ((type completion) (view gadget-dialog-view) (framem standard-frame-manager))
  ;;--- We can be clever and return different views depending on the
  ;;--- number of items, etc.
  +radio-box-view+)

(define-presentation-method gadget-includes-prompt-p 
			    ((type completion) (stream t) (view radio-box-view))
  t)

(define-presentation-method accept-present-default 
			    ((type completion) stream (view radio-box-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p))
  (let ((current-selection nil))
    (flet ((update-gadget (record gadget radio-box)
	     (declare (ignore gadget record))
	     (map-over-sheets
	       #'(lambda (sheet)
		   (when (typep sheet 'toggle-button)
		     (when (setf (gadget-value sheet)
				 (and default-supplied-p
				      (funcall test 
					       (funcall value-key (gadget-id sheet))
					       (funcall value-key default))))
		       (setf (radio-box-current-selection radio-box) sheet))))
	       radio-box)))
      (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
	(let* ((toggle-options
		 (getf (view-gadget-initargs view) :toggle-button-options))
	       (buttons
		 (map 'list
		      #'(lambda (element)
			  (let ((button
				  (apply #'make-pane 'toggle-button 
				    :label 
				      (let ((name (funcall name-key element)))
					(if (eq printer #'write-token)
					    name
					    (pixmap-from-menu-item stream name printer nil)))
				    :indicator-type
				      (getf toggle-options :indicator-type :one-of)
				    :value
				      (and default-supplied-p
					   (funcall test 
						    (funcall value-key element)
						    (funcall value-key default)))
				    :id element
				    toggle-options)))
			    (when (and default-supplied-p
				       (funcall test 
						(funcall value-key element)
						(funcall value-key default)))
			      (setq current-selection button))
			    button))
		      sequence))
	       (radio-box
		 (apply #'make-pane 'radio-box
			(append
			  (remove-keywords (view-gadget-initargs view)
					   '(:toggle-button-options))
			  (list :label (and (stringp prompt) prompt)
				:choices buttons 
				:selection current-selection 
				:client stream
				:id query-identifier 
				:value-changed-callback
				  (make-accept-values-value-changed-callback
				    stream query-identifier))))))
	  (values (if (stringp prompt)
		      (vertically ()
			(make-pane 'label-pane :label prompt)
			(outlining () radio-box))
		      (outlining () radio-box))
		  radio-box))))))


;;; Subset completion gadget

(define-presentation-method decode-indirect-view
			    ((type subset-completion) (view gadget-dialog-view) (framem standard-frame-manager))
  +check-box-view+)

(define-presentation-method gadget-includes-prompt-p 
			    ((type subset-completion) (stream t) (view check-box-view))
  t)

(define-presentation-method accept-present-default 
			    ((type subset-completion) stream (view check-box-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p))
  (flet ((update-gadget (record gadget check-box)
	   (declare (ignore record gadget))
	   (map-over-sheets
	     #'(lambda (sheet)
		 (when (typep sheet 'toggle-button)
		   (setf (gadget-value sheet)
			 (and default-supplied-p
			      (member (gadget-id sheet) default
				      :test test 
				      ;;--- Should the value-key be used?
				      :key value-key) 
			      t))))
	     check-box)))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let* ((toggle-options
	       (getf (view-gadget-initargs view) :toggle-button-options))
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
					   (pixmap-from-menu-item stream name printer nil)))
				   :indicator-type
				     (getf toggle-options :indicator-type :some-of)
				   :value
				     (and default-supplied-p
					  (member value default
						  :test test 
						  ;;--- Should the value-key be used?
						  :key value-key) 
					  t)
				   :id value
				   toggle-options)))
			  button))
		    sequence))
	     (check-box (apply #'make-pane 'check-box
			       (append
				 (remove-keywords (view-gadget-initargs view)
						  '(:toggle-button-options))
				 (list :label (and (stringp prompt) prompt)
				       :choices buttons
				       :client stream 
				       :id query-identifier
				       :value-changed-callback
				         (make-accept-values-value-changed-callback
					   stream query-identifier))))))
	(values (if (stringp prompt)
		    (vertically ()
		      (make-pane 'label-pane :label prompt)
		      (outlining () check-box))
		    (outlining () check-box))
		check-box)))))


;;; Boolean gadget

(define-presentation-method decode-indirect-view
			    ((type boolean) (view gadget-dialog-view) (framem standard-frame-manager))
  +toggle-button-view+)

(define-presentation-method gadget-includes-prompt-p 
			    ((type boolean) (stream t) (view toggle-button-view))
  t)

(define-presentation-method accept-present-default 
			    ((type boolean) stream (view toggle-button-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore default-supplied-p present-p))
  (flet ((update-gadget (record gadget button)
 	   (declare (ignore record gadget))
 	   (setf (gadget-value button) default)))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let ((button (make-pane-from-view 'toggle-button view
		      :label (and (stringp prompt) prompt)
		      :value default
		      :client stream :id query-identifier
		      :value-changed-callback
		        (make-accept-values-value-changed-callback
			  stream query-identifier))))
 	(values (outlining () button) button)))))


;;; Numeric gadgets

(define-presentation-method gadget-includes-prompt-p
			    ((type real) (stream t) (view slider-view))
  t)

(define-presentation-method accept-present-default 
			    ((type real) stream (view slider-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p))
  (let ((min-value (if (eq low '*) 0 low))
	(max-value (if (eq high '*) 100 high)))
    (flet ((update-gadget (record gadget slider)
	     (declare (ignore record gadget))
	     (setf (gadget-value slider)
		   (if default-supplied-p default min-value))))
      (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
	(let ((slider 
		;;--- What other initargs do we pass along from the view?
		(make-pane-from-view 'slider view
		  :label (and (stringp prompt) prompt)
		  :value (if default-supplied-p default min-value)
		  :min-value min-value :max-value max-value
		  :orientation (gadget-orientation view)
		  :decimal-places (slider-decimal-places view)
		  :show-value-p (gadget-show-value-p view)
		  :client stream :id query-identifier
		  :value-changed-callback
		    (make-accept-values-value-changed-callback
		      stream query-identifier))))
	  (values (outlining () slider) slider))))))

(define-presentation-method accept-present-default 
			    ((type integer) stream (view slider-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p))
  (let ((min-value (if (eq low '*) 0 low))
	(max-value (if (eq high '*) 100 high)))
    (flet ((update-gadget (record gadget slider)
	     (declare (ignore record gadget))
	     (setf (gadget-value slider)
		   (if default-supplied-p default min-value))))
      (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
	(let ((slider
		;;--- What other initargs do we pass along from the view?
		(make-pane-from-view 'slider view
		  :label (and (stringp prompt) prompt)
		  :value (if default-supplied-p default min-value)
		  :min-value min-value :max-value max-value
		  :orientation (gadget-orientation view)
		  :number-of-quanta (- high low) :decimal-places 0
		  :show-value-p (gadget-show-value-p view)
		  :client stream :id query-identifier
		  :value-changed-callback
		    (make-accept-values-value-changed-callback
		      stream query-identifier))))
	  (values (outlining () slider) slider))))))


;;; Text editing gadgets

(define-presentation-method accept-present-default 
			    ((type t) stream (view text-field-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore default-supplied-p present-p))
  (flet ((update-gadget (record gadget text-field)
 	   (declare (ignore record gadget))
 	   (setf (gadget-value text-field) (present-to-string default type))))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let ((text-field (make-pane-from-view 'text-field view
			  :label (and (stringp prompt) prompt)
			  :value (present-to-string default type)
			  :client stream :id query-identifier
			  :value-changed-callback
			    `(accept-values-string-field-changed-callback
			       ,stream ,query-identifier))))
 	(values text-field text-field)))))

;;--- This is mostly nonsense.  If we enter something that is an error then
;;--- the field value is unchanged.  What is the right thing to do?
(defmethod accept-values-string-field-changed-callback
	   ((gadget text-field) new-value stream query)
  (when (accept-values-query-valid-p query (accept-values-query-presentation query))
    ;; Only call the callback if the query is still valid
    (handler-case
        (multiple-value-bind (object type index)
	    (accept-from-string (accept-values-query-type query) new-value)
	  (declare (ignore type))
	  (assert (= index (length new-value)))
	  object)
      (error ())
      (:no-error (object)
       (do-avv-command object stream query)))))

;; The string case is straightforward
(define-presentation-method decode-indirect-view
			    ((type string) (view gadget-dialog-view) (framem standard-frame-manager))
  +text-field-view+)

(define-presentation-method accept-present-default 
			    ((type string) stream (view text-field-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore default-supplied-p present-p))
  (flet ((update-gadget (record gadget button)
 	   (declare (ignore record gadget))
 	   (setf (gadget-value button) default)))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let ((text-field (make-pane-from-view 'text-field view
			  :label (and (stringp prompt) prompt)
			  :value default
			  :client stream :id query-identifier
			  :value-changed-callback
			    (make-accept-values-value-changed-callback
			      stream query-identifier))))
 	(values text-field text-field)))))

(define-presentation-method accept-present-default 
			    ((type string) stream (view text-editor-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore default-supplied-p present-p))
  (flet ((update-gadget (record gadget button)
 	   (declare (ignore record gadget))
 	   (setf (gadget-value button) default)))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let ((text-field (make-pane-from-view 'text-editor view
			  :label (and (stringp prompt) prompt)
			  :value default
			  :client stream :id query-identifier
			  :value-changed-callback
			    (make-accept-values-value-changed-callback
			      stream query-identifier))))
 	(values (scrolling () text-field) text-field)))))


;;; List and option panes

(define-presentation-method accept-present-default 
			    ((type completion) stream (view list-pane-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p default-supplied-p prompt))
  (make-list/option-pane-for-ptype 'list-pane
				   stream view sequence
				   name-key value-key test
				   default query-identifier type printer
				   :exclusive))

(define-presentation-method accept-present-default 
			    ((type subset-completion) stream (view list-pane-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p default-supplied-p prompt))
  (make-list/option-pane-for-ptype 'list-pane 
				   stream view sequence
				   name-key value-key test
				   default query-identifier type printer
				   :nonexclusive))

(defun make-list/option-pane-for-ptype (pane-type stream view sequence 
					name-key value-key test
					default query-identifier type printer mode)
  (flet ((update-gadget (record gadget list-pane)
	   (declare (ignore gadget record))
	   (setf (gadget-value list-pane) default)
	   list-pane))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let* ((pane (if (eq pane-type 'option-pane)
		       (make-pane-from-view pane-type view
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
			     stream query-identifier))
		       (make-pane-from-view pane-type view
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
			     stream query-identifier)))))
	(values (if (eq pane-type 'list-pane) (scrolling () pane) pane)
		pane)))))

(define-presentation-method accept-present-default 
			    ((type completion) stream (view option-pane-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p default-supplied-p prompt))
  (make-list/option-pane-for-ptype 'option-pane
				   stream view sequence name-key value-key
				   test default query-identifier type 
				   printer
				   :exclusive))

(define-presentation-method accept-present-default 
			    ((type subset-completion) stream (view option-pane-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p default-supplied-p prompt))
  (make-list/option-pane-for-ptype 'option-pane 
				   stream view sequence
				   name-key value-key test
				   default query-identifier type printer
				   :nonexclusive))


;;--- These should be defined in the standard DEFOPERATION way...

(defmethod sheet-medium ((stream standard-encapsulating-stream))
  (sheet-medium (encapsulating-stream-stream stream)))

(defmethod sheet-parent ((stream standard-encapsulating-stream))
  (sheet-parent (encapsulating-stream-stream stream)))

(defmethod port ((stream standard-encapsulating-stream))
  (port (encapsulating-stream-stream stream)))

(defmethod graft ((stream standard-encapsulating-stream))
  (graft (encapsulating-stream-stream stream)))

(defmethod sheet-adopt-child ((stream standard-encapsulating-stream) child)
  (sheet-adopt-child (encapsulating-stream-stream stream) child))

(defmethod sheet-disown-child ((stream standard-encapsulating-stream) child)
  (sheet-disown-child (encapsulating-stream-stream stream) child))

