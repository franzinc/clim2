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
;; $fiHeader: gadget-output.lisp,v 1.8 92/03/24 19:37:52 cer Exp Locker: cer $

(in-package :clim-internals)


;;; Implementation of output-records that are tied to gadgets.

(defclass gadget-output-record 
    	  (output-record-mixin output-record-element-mixin output-record)
    ((gadget :initform nil
	     :accessor output-record-gadget)))

(defmethod associate-record-and-gadget (rec gadget stream x y)
  ;; Just in case
  (setf (sheet-enabled-p gadget) nil)
  (sheet-adopt-child stream gadget)
  ;; In order to be able to determine the space the widget has to be
  ;; added to the parent and hopefully grafted
  (assert (port stream))
  (setf (output-record-gadget rec) gadget)
  (let ((sr (compose-space gadget)))
    (multiple-value-bind (abs-x abs-y)
	(point-position*
	 (stream-output-history-position stream))
      (decf x abs-x)
      (decf y abs-y)
      (bounding-rectangle-set-edges
       rec
       x y (+ x (space-requirement-width sr)) (+ y (space-requirement-height sr))))
    (when (output-record-stream rec)
      (update-gadget-position rec)
      (setf (sheet-enabled-p gadget) t))))

;; Three ways

(defmethod note-output-record-moved ((record gadget-output-record) dx1 dy1 dx2 dy2)
  (declare (ignore dx1 dy1 dx2 dy2))
  (update-gadget-position record))

(defmethod bounding-rectangle-set-edges :after ((rec gadget-output-record) a b c d)
  (declare (ignore  a b c d))
  (update-gadget-position rec))

(defmethod bounding-rectangle-set-position* :after ((rec gadget-output-record) a b)
  (declare (ignore  a b))
  (update-gadget-position rec))

(defmethod update-gadget-position (record) 
  (let ((gadget (output-record-gadget record)))
    (when gadget
      (with-bounding-rectangle* (left top right bottom) record
        (let ((xoff 0)
	      (yoff 0))
	  (do ((parent record (output-record-parent parent)))
	      ((null parent))
	    (multiple-value-bind (x y)
		(output-record-position* parent)
	      (incf xoff x)
	      (incf yoff y)))
	  (move-and-resize-sheet* gadget
				  (+ left xoff) (+ top yoff)
				  (- right left) (- bottom top)))))))

#+This-almost-works-with-the-other-definition
(defmethod update-gadget-position (record) 
  (let ((gadget (output-record-gadget record)))
    (when gadget
      (multiple-value-bind
	  (xoff yoff)
	  (convert-from-relative-to-absolute-coordinates
	   (output-record-stream record)
	   (output-record-parent record))
	(with-bounding-rectangle* (left top right bottom) record
	  (move-and-resize-sheet* gadget
				  (+ left xoff) (+ top yoff)
				  (- right left) (- bottom top)))))))

;; Need to add the gadget to the stream

;; One problem we have is that when we scroll we will end up with
;; geometry requests being refused.
;; We want to enable/disable panes as they become (in)visible


(defmethod window-clear :after ((window clim-stream-sheet))
  (dolist (child (sheet-children window))
    (sheet-disown-child window child)))
  

(defmacro with-output-as-gadget ((stream &rest args) 
				 &body body)
  (default-output-stream stream)
  (let ((fm (gensym))
	(f (gensym)))
    (with-keywords-removed (args args '(:stream))
      `(invoke-with-output-as-gadget ,stream
	 #'(lambda (,fm ,f)
	     (with-look-and-feel-realization (,fm ,f) ,@body)) 
	 ,@args))))

(defmethod invoke-with-output-as-gadget (stream continuation &key)
  ;;--- (PANE-FRAME STREAM) or *APPLICATION-FRAME*?
  (let* ((frame (pane-frame stream))
	 (framem (frame-manager frame)))
    (assert frame)
    (assert framem)
    (multiple-value-bind (x y)
	(stream-cursor-position* stream)
      (let* (gadget
	     (record
	      (with-new-output-record (stream 'gadget-output-record record)
		;;--- Do we really need to do this. under what
		;;--- situations does this happen and why cant we loose
		;;--- the gadget
		(unless (setq gadget (output-record-gadget record))
		  (associate-record-and-gadget
		   record
		   (setq gadget (funcall continuation framem frame))
		   stream x y)))))
	(move-cursor-beyond-output-record stream record)
	(values gadget record)))))
	    
#+This-almost-works-with-the-other-definition
(defmethod invoke-with-output-as-gadget (stream continuation &key)
  (let* ((frame (pane-frame stream))
	 (framem (frame-manager frame)))
    (assert frame)
    (assert framem)
    (multiple-value-bind (x y)
	(stream-cursor-position* stream)
      (let* (gadget
	     (record
	      (with-new-output-record (stream 'gadget-output-record record)
		(unless (setq gadget (output-record-gadget record))
		  (setq gadget (funcall continuation framem frame))))))
	(associate-record-and-gadget
		   record
		   gadget
		   stream x y)
	(move-cursor-beyond-output-record stream record)
	(values gadget record)))))


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
  (when (output-record-gadget rec)
    (update-gadget-position rec)
    (setf (sheet-enabled-p (output-record-gadget rec)) t)))

(defmethod note-output-record-detached :after ((rec gadget-output-record))
  (setf (sheet-enabled-p (output-record-gadget rec)) nil))

;;; Control whetherwe display a prompt in the gadget or not

(defun gadget-includes-prompt-p (type stream view)
  (call-presentation-generic-function
   gadget-includes-prompt-p type stream view))

;(define-presentation-method gadget-includes-prompt-p ((type t) (stream t) (view textual-view) &key)
;  nil)
;  
;(define-presentation-method gadget-includes-prompt-p ((type t) (stream t) (view gadget-view) &key)
;  nil)

(define-presentation-method gadget-includes-prompt-p ((type t) (stream t) (view view) &key)
  nil)

;;; 

;;; Completion gadget

;; -- Gadget currently does not include prompt

(define-presentation-method accept-present-default ((type completion) 
						    stream
						    (view gadget-dialog-view)
						    default default-supplied-p
						    present-p query-identifier
						    &key (prompt t))
  (declare (ignore present-p))
  ;; value-key, test, sequence
  (with-output-as-gadget (stream)
    (let* ((gadget
	    (make-pane 'radio-box 
			  :label (and (stringp prompt) prompt)
			  :value-changed-callback
			  (make-accept-values-value-changed-callback
			   stream query-identifier)
			  :client stream
			  :id query-identifier)))
      (dolist (element sequence)
	(make-pane 'toggle-button 
		      :value (and default-supplied-p
				  (funcall test 
					   (funcall value-key element)
					   (funcall value-key default)))
		      :label (princ-to-string element)
		      :id element
		      :parent gadget))
      gadget)))


;;; Boolean gadget

(define-presentation-method gadget-includes-prompt-p ((type boolean) (stream t) (view gadget-view) &key)
  t)

(define-presentation-method accept-present-default ((type boolean) 
						    stream
						    (view gadget-dialog-view)
						    default default-supplied-p
						    present-p query-identifier
						    &key (prompt t))
  (declare (ignore default-supplied-p present-p))
  (let ((x (with-output-as-gadget (stream)
	     (make-pane 'toggle-button
			   :client stream 
			   :value-changed-callback
			   (make-accept-values-value-changed-callback
			    stream query-identifier)
			   :id query-identifier
			   :value default
			   :label (and (stringp prompt) prompt)))))
    ;;--- WE end up not making a new gadget sometimes, I think because
    ;;--- we find a gadget already in the record so we need to modify
    ;;-- the value!
    (setf (gadget-value x) default)))
    

;;; Integer gadget

(define-presentation-method gadget-includes-prompt-p ((type integer) (stream t) (view gadget-view) &key)
  t)

(define-presentation-method accept-present-default ((type integer)
						    stream
						    (view gadget-dialog-view)
						    default default-supplied-p
						    present-p query-identifier
						    &key (prompt t))
  (declare (ignore present-p))
  (with-output-as-gadget (stream)
    (make-pane 'slider
		  :value-changed-callback
		  (make-accept-values-value-changed-callback
		   stream query-identifier)
		  :client stream
		  :width 100
		  :value (if default-supplied-p default 0)
		  :id query-identifier
		  :label (and (stringp prompt) prompt))))


;;--- These should be defined in the standard DEFOPERATION way...

(defmethod sheet-medium ((x standard-encapsulating-stream))
  (sheet-medium (slot-value x 'stream)))

(defmethod port ((x standard-encapsulating-stream))
  (port (slot-value x 'stream)))

(defmethod graft ((stream standard-encapsulating-stream))
  (graft (slot-value stream 'stream)))

(defmethod sheet-adopt-child ((stream standard-encapsulating-stream) child)
  (sheet-adopt-child (slot-value stream 'stream) child))

(defmethod sheet-disown-child ((stream standard-encapsulating-stream) child)
  (sheet-adopt-child (slot-value stream 'stream) child))

