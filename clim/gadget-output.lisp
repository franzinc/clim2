;; -*- mode: common-lisp; package: clim -*-
;;
;;				-[]-
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
;; $fiHeader: gadget-output.cl,v 1.1 92/01/06 20:36:58 cer Exp Locker: cer $

(in-package :clim)


;;; Implementation of output-records that are tied to gadgets.

(defclass gadget-output-record (displayed-output-record)
	  ((gadget :initform nil
		   :accessor output-record-gadget)))

(defmethod associate-record-and-gadget (rec gadget stream x y)
  ;; Just in case
  (setf (sheet-enabled-p gadget) nil)
  (adopt-child stream gadget)
  ;; In order to be able to determine the space the widget has to be
  ;; added to the parent and hopefully grafted
  (assert (port stream))
  (setf (output-record-gadget rec) gadget)
  (let ((sr (compose-space gadget)))
    (bounding-rectangle-set-edges
     rec
     x 
     y
     (+ x (space-req-width sr))
     (+ y (space-req-height sr))))
  (when (output-record-stream rec)
    (setf (sheet-enabled-p gadget) t)))

(defmethod bounding-rectangle-set-edges	:after ((rec gadget-output-record)
						minx miny maxx maxy)
  (when (output-record-gadget rec)
    (silica::move-and-resize-sheet*
     (output-record-gadget rec)
     minx miny (- maxx minx) (- maxy miny))))

;; cover wagon fullsome - 7th . 9.30. cw saloon
;; Need to add the gadget to the stream

(defmethod bounding-rectangle-set-position*  :after 
	   ((r gadget-output-record) new-x new-y)
  (when (output-record-gadget r)
    (silica::move-sheet* (output-record-gadget r) new-x new-y)))

;; One problem we have is that when we scroll we will end up with
;; geometry requests being refused.
;; We want to enable/disable panes as they become (in)visible


(defmethod window-clear :after ((window extended-stream-sheet))
  (dolist (child (sheet-children window))
    (silica::disown-child window child)))
  

(defmacro with-output-as-gadget ((&rest args &key stream
				  &allow-other-keys) 
				 &body body)
  (default-output-stream stream)
  (let ((fm (gensym))
	(f (gensym)))
    (with-rem-keywords (args args '(:stream))
      `(with-output-as-gadget-internal
	   ,stream
	 #'(lambda (,fm ,f)
	     (with-look-and-feel-realization (,fm ,f) ,@body)) 
	 ,@args))))

(defmethod with-output-as-gadget-internal (stream continuation &key)
  (let* ((frame (silica::pane-frame stream))
	 (framem (frame-manager frame)))
    (assert frame)
    (assert framem)
    (multiple-value-bind
	(x y)
	(stream-cursor-position* stream)
      (let* (gadget
	     (rec 
	      (with-new-output-record (stream 
				       'gadget-output-record
				       rec)
		(unless (setq gadget (output-record-gadget rec))
		  (associate-record-and-gadget
		   rec
		   (setq gadget (funcall continuation framem frame))
		   stream x y)))))
	(move-cursor-beyond-output-record stream rec)
	(values gadget rec)))))
	    


(define-presentation-method accept-present-default ((type boolean) 
						    stream
						    (view gadget-dialog-view)
						    default default-supplied-p
						    present-p query-identifier)
  (declare (ignore default-supplied-p present-p))
  (with-output-as-gadget (:stream stream)
    (realize-pane 'silica::toggle-button
		  :client stream
		  :id query-identifier
		  :set default)))


(define-presentation-method accept-present-default ((type integer)
						    stream
						    (view gadget-dialog-view)
						    default default-supplied-p
						    present-p query-identifier)
  (declare (ignore default-supplied-p present-p))
  (with-output-as-gadget (:stream stream)
    (realize-pane 'silica::slider
		  :client stream
		  :width 100
		  :value (if default-supplied-p default 0)
		  :id query-identifier)))



(defmethod display-exit-boxes ((frame accept-values) stream)
  ;; Do the fresh-line *outside* of the updating-output so that it
  ;; doesn't get repositioned relatively in the X direction if the
  ;; previous line gets longer.  Maybe there should be some better
  ;; way of ensuring this.
  (fresh-line stream)
  (with-slots (exit-boxes) frame
    (let ((exit  (or (second (assoc :exit  exit-boxes)) "<End> uses these values"))
	  (abort (or (second (assoc :abort exit-boxes)) "<Abort> aborts")))
      (updating-output (stream :unique-id stream
			       :cache-value 'exit-boxes)
		       (formatting-item-list 
			(stream :n-columns 2)
			(formatting-cell 
			 (stream)
			 (with-output-as-gadget (:stream stream)
			   (assert frame)
			   (realize-pane 'silica::push-button
					 :client frame
					 :id :abort
					 :label abort)))
			(formatting-cell 
			 (stream)
			 (with-output-as-gadget (:stream stream)
			   (realize-pane 'silica::push-button
					 :client frame
					 :id :exit
					 :label exit))))))))

(defmethod silica::pane-frame ((stream encapsulating-stream-mixin))
  (silica::pane-frame (slot-value stream 'stream)))

(defmethod graft ((stream encapsulating-stream-mixin))
  (graft (slot-value stream 'stream)))

(defmethod adopt-child ((stream encapsulating-stream-mixin) child)
  (adopt-child (slot-value stream 'stream) child))

(defmethod silica::activate-callback ((pb silica::push-button)
				      (client accept-values)
				      id)
  (ecase id
    (:abort (com-abort-avv))
    (:exit (com-exit-avv))))


(define-command (com-change-query :command-table accept-values-pane)
    ((id t)
     (new-value t))
  (with-slots (value changed-p) id
    (setf value new-value changed-p t)))
  

(defmethod silica::value-changed-callback ((gadget value-gadget)
					   (client avv-stream)
					   id
					   new-value)
  (do-avv-command
      new-value
    client id))

(defun do-avv-command (new-value client id)
  (throw-highlighted-presentation
   (make-instance 'standard-presentation
		  :object `(com-change-query ,id ,new-value)
		  :type 'command)
   *input-context*
   (make-instance 'silica::pointer-event
		  :sheet (slot-value client 'stream)
		  :x 0
		  :y 0
		  :modifiers 0
		  :button 256)))

;; incf redisplay wanted this!

(defmethod clear-output-record ((rec gadget-output-record))
  nil)

;; kludge because of clear-output-record :after
;; Perhaps this should be moved somewhere.
;;  If think is because the clear-output-record stuff should be on
;;  composite-output-records rather than displayed output-records 

(defmethod bounding-rectangle-set-edges	:around ((rec gadget-output-record)
						 minx miny maxx maxy)
  (unless (= minx miny maxx maxy)
    (call-next-method)))

#+ignore
(defmethod add-output-record :after ((child gadget-output-record) (parent t))
  (do ((p parent (output-record-parent p)))
      ((null p) 
       (warn "Parent is not a output-history"))
    (when (typep p 'stream-output-history-mixin)
      (return t)))
  ;; Perhaps we should enable the gadget at this point
  nil)

(defmethod note-output-record-attached :after ((rec gadget-output-record) stream)
  (declare (ignore stream))
  (setf (sheet-enabled-p (output-record-gadget rec)) t))

(defmethod note-output-record-detached :after ((rec gadget-output-record))
  (setf (sheet-enabled-p (output-record-gadget rec)) nil))


(define-presentation-method accept-present-default ((type completion) 
						    stream
						    (view gadget-dialog-view)
						    default default-supplied-p
						    present-p query-identifier)
  (declare (ignore present-p))
  ;; value-key, test, sequence
  (with-output-as-gadget (:stream stream)
    (let* ((frame-pane
	    (realize-pane 'silica::frame-pane))
	   (gadget
	    (realize-pane 'silica::radio-box 
			  :client stream
			  :id query-identifier
			  :parent frame-pane)))
      (dolist (element sequence)
	(realize-pane 'toggle-button 
		      :set (and default-supplied-p
				(funcall test 
					 (funcall value-key element)
					 (funcall value-key default)))
		      :label (princ-to-string element)
		      :id element
		      :parent gadget))
      frame-pane)))

(defmethod silica::value-changed-callback ((gadget silica::radio-box)
					   (client avv-stream)
					   id 
					   new-value)
  (do-avv-command
      (silica::gadget-id new-value)
    client 
    id))




;;; Another amusing component
;;; Some how we need so associate an output-record with the button

(defun accept-values-command-button-1
    (stream prompt continuation
     &key (documentation (if (stringp prompt)
			     prompt
			   (with-output-to-string (stream)
			     (funcall prompt stream))))
	  (query-identifier (list ':button documentation))
	  (cache-value t) (cache-test #'eql)
	  resynchronize)
  (declare (dynamic-extent prompt))
  (updating-output 
   (stream :unique-id query-identifier :id-test #'equal
	   :cache-value cache-value :cache-test
	   cache-test)
   (with-output-as-gadget (:stream stream)
     (realize-pane 
      'silica::push-button
      :label prompt
      :id (output-recording-stream-current-output-record-stack 
	   (slot-value stream 'stream))
      :client (make-instance 'accept-values-command-button
			     :continuation continuation
			     :documentation documentation
			     :resynchronize resynchronize)))))


(defmethod silica::activate-callback ((pb silica::push-button)
				      (client accept-values-command-button)
				      id)
  (throw-highlighted-presentation
   (make-instance 'standard-presentation
		  :object `(com-avv-command-button ,client ,id)
		  :type 'command)
   *input-context*
   (make-instance 'silica::pointer-event
		  :sheet (sheet-parent pb)
		  :x 0
		  :y 0
		  :modifiers 0
		  :button 256)))
