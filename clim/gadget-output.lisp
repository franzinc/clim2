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
;; $fiHeader: gadget-output.lisp,v 1.19 92/07/06 18:51:40 cer Exp Locker: cer $

(in-package :clim-internals)


;;; Implementation of output-records that are tied to gadgets.

;;--- Why are these OUTPUT-RECORD-MIXINs?
(defclass gadget-output-record 
    	  (output-record-mixin output-record-element-mixin output-record)
    ((gadget :initform nil
	     :accessor output-record-gadget)))

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
	  (move-and-resize-sheet* gadget
				  (+ left xoff) (+ top yoff)
				  (- right left) (- bottom top)))))))

#---ignore
(defmethod update-gadget-position ((record gadget-output-record))
  (let ((gadget (output-record-gadget record)))
    (when gadget
      (multiple-value-bind (xoff yoff)
	  (convert-from-relative-to-absolute-coordinates
	    (output-record-stream record)
	    (output-record-parent record))
	(with-bounding-rectangle* (left top right bottom) record
	  (move-and-resize-sheet* gadget
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
  (let ((fm (gensym))
	(f (gensym)))
    (with-keywords-removed (args args '(:stream))
      `(invoke-with-output-as-gadget ,stream
	 #'(lambda (,fm ,f)
	     (with-look-and-feel-realization (,fm ,f) ,@body)) 
	 ,@args))))

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
		   (associate-record-and-gadget
		     record
		     (setq gadget (funcall continuation framem frame))
		     stream x y)))))
	(move-cursor-beyond-output-record stream record)
	(values gadget record)))))
	    
#---ignore
(defmethod invoke-with-output-as-gadget (stream continuation &key)
  (let* ((frame (pane-frame stream))
	 (framem (frame-manager frame)))
    (assert frame)
    (assert framem)
    (multiple-value-bind (x y) (stream-cursor-position stream)
      (let* (new
	     gadget
	     (record
	       (with-new-output-record (stream 'gadget-output-record record)
		 (unless (setq gadget (output-record-gadget record))
		   (setq gadget (funcall continuation framem frame)
			 new t)))))
	(when new 
	  (associate-record-and-gadget record gadget stream x y))
	(move-cursor-beyond-output-record stream record)
	(values gadget record)))))


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
(define-presentation-method accept-present-default 
			    ((type completion) stream (view gadget-dialog-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p))
  (let ((current-selection nil))
    (with-output-as-gadget (stream)
      (let ((buttons
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
		   sequence)))
	(outlining ()
	  (make-pane 'radio-box 
	    :label (and (stringp prompt) prompt)
	    :choices buttons
	    :selection current-selection
	    :client stream :id query-identifier
	    :value-changed-callback
	      (make-accept-values-value-changed-callback
		stream query-identifier)))))))


;;; Subset completion gadget

(define-presentation-method accept-present-default 
			    ((type subset-completion) stream (view gadget-dialog-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p))
  (with-output-as-gadget (stream)
    (let ((buttons
	    (map 'list
		 #'(lambda (element)
		     (let* ((value (funcall value-key element))
			    (button
			     (make-pane 'toggle-button 
					:label (funcall name-key element)
					:indicator-type :some-of
					:value (and default-supplied-p
						    (member (funcall value-key element) default
							    :test test 
							    ;;-- Should the
							    ;;value key be used??
							    :key value-key) 
						    t)
					:id value)))
		       button))
		 sequence)))
      (outlining ()
	(make-pane 'check-box 
	  :label (and (stringp prompt) prompt)
	  :choices buttons
	  :client stream :id query-identifier
	  :value-changed-callback
	    (make-accept-values-value-changed-callback
	      stream query-identifier))))))


;;; Boolean gadget

(define-presentation-method gadget-includes-prompt-p 
			    ((type boolean) (stream t) (view gadget-view))
  t)

(define-presentation-method accept-present-default 
			    ((type boolean) stream (view gadget-dialog-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore default-supplied-p present-p))
  (with-output-as-gadget (stream)
    (outlining ()
      (make-pane 'toggle-button
        :label (and (stringp prompt) prompt)
	:value default
	:client stream :id query-identifier
	:value-changed-callback
	  (make-accept-values-value-changed-callback
	    stream query-identifier)))))


;;; Numeric gadgets

(defclass slider-view (slider gadget-view) ())

(define-presentation-method gadget-includes-prompt-p
			    ((type real) (stream t) (view slider-view))
  t)

(define-presentation-method accept-present-default 
			    ((type real) stream (view slider-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p))
  (with-output-as-gadget (stream)
    (outlining ()
      ;;--- What other initargs do we pass along from the view?
      (make-pane 'slider
        :label (and (stringp prompt) prompt)
	:value (if default-supplied-p default (if (eq low '*) 0 low))
	:min-value (if (eq low '*) 0 low) :max-value (if (eq high '*) 100 high)
	:orientation (gadget-orientation view)
	:decimal-places 1
	:client stream :id query-identifier
	:value-changed-callback
	  (make-accept-values-value-changed-callback
	    stream query-identifier)))))

(define-presentation-method accept-present-default 
			    ((type integer) stream (view slider-view)
			     default default-supplied-p present-p query-identifier
			     &key (prompt t))
  (declare (ignore present-p))
  (with-output-as-gadget (stream)
    (outlining ()
      ;;--- What other initargs do we pass along from the view?
      (make-pane 'slider
        :label (and (stringp prompt) prompt)
	:value (if default-supplied-p default (if (eq low '*) 0 low))
	:min-value (if (eq low '*) 0 low) :max-value (if (eq high '*) 100 high)
	:orientation (gadget-orientation view)
	:client stream :id query-identifier
	:value-changed-callback
	  (make-accept-values-value-changed-callback
	    stream query-identifier)))))


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

