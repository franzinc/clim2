;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: gadget-output.lisp,v 1.22 92/07/24 10:54:26 cer Exp Locker: cer $

(in-package :clim-internals)


;;; Implementation of output-records that are tied to gadgets.

;;--- Why are these OUTPUT-RECORD-MIXINs?
(defclass gadget-output-record 
    (output-record-mixin output-record-element-mixin output-record)
    ((gadget :initform nil :accessor output-record-gadget)
     (cache-value :initarg :cache-value :initform nil) 
     (optional-values :initform nil)))

(defmethod initialize-instance :after ((record gadget-output-record) &key cache-test)
  ;;-- This is just to enable the cache-test to be passed along
  ;;without signalling an error. Yuck!
  cache-test)

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
	    (output-record-stream record)
	    (output-record-parent record))
	(with-bounding-rectangle* (left top right bottom) record
	  (move-and-resize-sheet gadget
				 (+ left xoff) (+ top yoff)
				 (- right left) (- bottom top)))))))

;;--- Flush this when REPLAY-OUTPUT-RECORD calls itself recursively
(defmethod note-output-record-replayed ((record gadget-output-record) stream
					&optional region x-offset y-offset)
  (declare (ignore stream x-offset y-offset))
  (let ((gadget (output-record-gadget record)))
    (when (port gadget)
      (with-sheet-medium (medium gadget)
	(handle-repaint gadget medium region)))))


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
	   ,stream #'with-output-as-gadget-body ,@args)))))

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
			   (let ((values (multiple-value-list (funcall continuation framem frame))))
			     (setf (slot-value record 'optional-values) (cdr values))
			     (setq gadget (car values) new t)))))
		  (declare (dynamic-extent #'with-new-output-record-body))
		  (apply #'invoke-with-new-output-record stream #'with-new-output-record-body
			 'gadget-output-record
			 'nil args))))
	  (cond (new 
		 (associate-record-and-gadget record gadget stream x y))
		(update-gadget
		 (apply update-gadget record gadget (slot-value record 'optional-values))))
	  (move-cursor-beyond-output-record stream record)
	  (values gadget record))))))


;; incf redisplay wanted this!

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

;;--- Gadget currently does not include prompt

(define-presentation-method decode-indirect-view
    ((view gadget-dialog-view) (framem standard-frame-manager) (type completion))
  ;;-- We can be clever and return different views depending on the
  ;;-- number of items etc
  +radio-box-view+)

(defmacro make-pane-from-view (class view &rest initargs)
  `(apply #'make-pane ,class (append (view-initargs ,view) (list ,@initargs))))

(define-presentation-method accept-present-default 
    ((type completion) stream (view radio-box-view)
		       default default-supplied-p present-p query-identifier
		       &key (prompt t))
  (declare (ignore present-p))
  (let ((current-selection nil))
    (flet ((update-gadget (gadget record radio-box)
	     (declare (ignore gadget record))
	     (dolist (toggle (sheet-children radio-box))
	       (when (setf (gadget-value toggle)
		       (and default-supplied-p
			    (funcall test 
				     (funcall value-key (gadget-id toggle))
				     (funcall value-key default))))
		 (setf (radio-box-current-selection radio-box) toggle)))))
      (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
	(let* ((buttons
		(map 'list
		  #'(lambda (element)
		      (let ((button
			     (make-pane 'toggle-button 
					:label (funcall name-key element)
					:indicator-type :one-of
					:value (and default-supplied-p
						    (funcall test 
							     (funcall value-key element)
							     (funcall value-key default)))
					:id element)))
			(when (and default-supplied-p
				   (funcall test 
					    (funcall value-key element)
					    (funcall value-key default)))
			  (setq current-selection button))
			button))
		  sequence))
	       (rb (make-pane-from-view
		    'radio-box view
		    :label (and (stringp prompt) prompt)
		    :choices buttons
		    :selection current-selection
		    :client stream :id query-identifier
		    :value-changed-callback
		    (make-accept-values-value-changed-callback
		     stream query-identifier))))
	  (values (outlining () rb) rb))))))


;;; Subset completion gadget

(define-presentation-method decode-indirect-view
    ((view gadget-dialog-view) (framem standard-frame-manager) (type subset-completion))
  +check-box-view+)

(define-presentation-method accept-present-default 
    ((type subset-completion) stream (view gadget-dialog-view)
			      default default-supplied-p present-p query-identifier
			      &key (prompt t))
  (declare (ignore present-p))
  (flet ((update-gadget (record gadget check-box)
	   (declare (ignore record gadget))
	   (dolist (toggle (sheet-children check-box))
	     (setf (gadget-value toggle)
	       (and default-supplied-p
		    (member (gadget-id toggle) default
			    :test test 
			    ;;--- Should the value-key be used?
			    :key value-key) 
		    t)))))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let* ((buttons
	      (map 'list
		#'(lambda (element)
		    (let* ((value (funcall value-key element))
			   (button
			    (make-pane 'toggle-button 
				       :label (funcall name-key element)
				       :indicator-type :some-of
				       :value (and default-supplied-p
						   (member value default
							   :test test 
							   ;;--- Should the value-key be used?
							   :key value-key) 
						   t)
				       :id value)))
		      button))
		sequence))
	     (cb (make-pane-from-view 'check-box view
		  :label (and (stringp prompt) prompt)
		  :choices buttons
		  :client stream :id query-identifier
		  :value-changed-callback
		  (make-accept-values-value-changed-callback
		   stream query-identifier))))
	(values (outlining () cb) cb)))))


;;; Boolean gadget

(define-presentation-method decode-indirect-view
    ((view gadget-dialog-view) (framem standard-frame-manager) (type boolean))
  +toggle-button-view+)

(define-presentation-method gadget-includes-prompt-p 
			    ((type boolean) (stream t) (view toggle-button-view))
  t)

(define-presentation-method accept-present-default 
    ((type boolean) stream (view toggle-button-view)
		    default default-supplied-p present-p query-identifier
		    &key (prompt t))
  (declare (ignore default-supplied-p present-p))
  (flet ((update-gadget (gadget record toggle)
	   (declare (ignore gadget record))
	   (setf (gadget-value toggle) default)))
    (with-output-as-gadget (stream :cache-value type :update-gadget #'update-gadget)
      (let ((toggle (make-pane-from-view
		     'toggle-button view
		     :label (and (stringp prompt) prompt)
		     :value default
		     :client stream :id query-identifier
		     :value-changed-callback
		     (make-accept-values-value-changed-callback
		      stream query-identifier))))
	(values (outlining () toggle) toggle)))))


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
	  (values (outlining ()
			     ;;--- What other initargs do we pass along from the view?
			     slider) slider))))))

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
	       (make-pane-from-view 'slider
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
	  (values (outlining ()
			     ;;--- What other initargs do we pass along from the view?
			     slider) slider))))))


;;--- These should be defined in the standard DEFOPERATION way...

(defmethod sheet-medium ((x standard-encapsulating-stream))
  (sheet-medium (slot-value x 'stream)))

(defmethod sheet-parent ((x standard-encapsulating-stream))
  (sheet-parent (slot-value x 'stream)))

(defmethod port ((x standard-encapsulating-stream))
  (port (slot-value x 'stream)))

(defmethod graft ((stream standard-encapsulating-stream))
  (graft (slot-value stream 'stream)))

(defmethod sheet-adopt-child ((stream standard-encapsulating-stream) child)
  (sheet-adopt-child (slot-value stream 'stream) child))

(defmethod sheet-disown-child ((stream standard-encapsulating-stream) child)
  (sheet-disown-child (slot-value stream 'stream) child))

