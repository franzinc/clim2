;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: event.lisp,v 1.5 92/02/24 13:04:33 cer Exp Locker: cer $

(in-package :silica)


;;; Event stuff

;;--- How important are these actual values?
;;--- Just flush them?
(deftype button-name ()
  '(member :left :middle :right))
					; Button and Modifier State Masks
(deftype shift-keysym ()
  '(member :left-shift :right-shift))

(deftype control-keysym ()
  '(member :left-control :right-control))

(deftype meta-keysym ()
  '(member :left-meta :right-meta))

(deftype super-keysym ()
  '(member :left-super :right-super))

(deftype hyper-keysym ()
  '(member :left-hyper :right-hyper))

(deftype lock-keysym ()
  '(member :caps-lock :shift-lock :mode-lock))


(deftype modifier-keysym ()
  '(or shift-keysym control-keysym meta-keysym super-keysym
    hyper-keysym lock-keysym))

(defun-inline state->shift-mask (state)
  ;; Get just the shift-mask part of state
  (logand state #xFF))

(defconstant +pointer-left-button+   (ash 1 8))
(defconstant +pointer-middle-button+ (ash 1 9))
(defconstant +pointer-right-button+  (ash 1 10))

(defconstant +shift-key+   (ash 1 0))
(defconstant +control-key+ (ash 1 2))
(defconstant +meta-key+    (ash 1 3))
(defconstant +super-key+   (ash 1 4))
(defconstant +hyper-key+   (ash 1 5))

(defmacro keyboard-modifier-state-match-p (button modifier-state &body clauses)
  (let ((b (gensym))
	(m (gensym)))
    `(let ((,b ,button)
	   (,m ,modifier-state))
       (declare (ignore-if-unused ,b ,m))
       ,(compile-kbms-clauses
	  b m `(and ,@clauses)))))

(defun compile-kbms-clauses (button modifier clause)
  (if (atomp clause)
      (ecase clause
	(:left `(= ,button +pointer-left-button+))
	(:right `(= ,button +pointer-right-button+))
	(:middle `(= ,button +pointer-middle-button+))
	(:shift `(logtest ,modifier +shift-key+))
	(:control `(logtest ,modifier +control-key+))
	(:meta `(logtest ,modifier +meta-key+))
	(:super `(logtest ,modifier +super-key+))
	(:hyper `(logtest ,modifier +hyper-key+)))
      (ecase (car clause)
	(and `(and ,@(mapcar #'(lambda (c)
				 (compile-kbms-clauses button modifier c))
			     (cdr clause))))
	(or `(or ,@(mapcar #'(lambda (c)
			       (compile-kbms-clauses button modifier c))
			   (cdr clause))))
	(not `(not ,(compile-kbms-clauses button modifier (second clause)))))))


(defun parse-gesture-spec (gesture-spec)
  (if (or (atom gesture-spec) 
	  (and (cdr gesture-spec)
	       (atom (cdr gesture-spec))))
      gesture-spec
      (let ((button (find-if-not #'shift-code gesture-spec)))
	(unless button
	  (cerror "Assume :LEFT and go on."
		  "Gesture spec missing a keysym: ~S" gesture-spec)
	  (setq button ':left))
	(cons button (apply #'make-shift-mask (delete button gesture-spec))))))

(defvar symbolic->mask `(:left    ,+pointer-left-button+
			 :right   ,+pointer-right-button+
			 :middle  ,+pointer-middle-button+
			 :hyper   ,+hyper-key+
			 :super   ,+super-key+
			 :meta    ,+meta-key+
			 :control ,+control-key+
			 :shift   ,+shift-key+))

(defun button-name->mask (button)
  (or (getf symbolic->mask button)
      (error "Cannot find button name: ~S" button)))

(defvar *shifts* `(:hyper ,+hyper-key+
		   :super ,+super-key+
		   :meta  ,+meta-key+
		   :control ,+control-key+
		   :shift ,+shift-key+))

(defun modifier->mask (m)
  (or (getf *shifts* m)
      (error "cannot find modifier: ~S" m)))

(defun modifiers->mask (modifiers)
  (let ((n 0))
    (dolist (m modifiers n)
      (setq n (logior n (modifier->mask m))))))



;;; 

(defgeneric process-next-event (port &key wait-function timeout))

(defgeneric distribute-event (port event))
(defgeneric dispatch-event (client event))
(defgeneric handle-event (client event))

(defgeneric port-keyboard-input-focus (port))

(defmethod handle-event (client (event window-repaint-event))
  (handle-repaint client nil 
		  (window-event-region event)))

(defclass sheet-with-event-queue ()
    ((event-queue :initform nil
		  :initarg :event-queue
		  :accessor sheet-event-queue)))

(defmethod note-sheet-grafted :after ((sheet sheet-with-event-queue))
  (unless (sheet-event-queue sheet)
    (setf (sheet-event-queue sheet)
	  (or (do ((sheet (sheet-parent sheet) (sheet-parent sheet)))
		  ((null sheet))
		(when (and (typep sheet 'sheet-with-event-queue)
			   (sheet-event-queue sheet))
		  (return (sheet-event-queue sheet))))
	      (make-queue)))))

(defgeneric queue-event (client event)
  (:method ((sheet sheet-with-event-queue) event)
   (queue-put (sheet-event-queue sheet) event)))

(defgeneric event-read (client)
  (:method ((sheet sheet-with-event-queue))
   (let ((queue (sheet-event-queue sheet)))
     (loop
       (process-wait "Event" #'(lambda ()
				 (not (queue-empty-p queue))))
       (let ((event (queue-get queue)))
	 (when event (return event)))))))


(defgeneric event-read-no-hang (client)
  (:method ((sheet sheet-with-event-queue))
   (queue-get (sheet-event-queue sheet) nil)))

(defgeneric event-peek (client &optional event-type)
  (:method ((sheet sheet-with-event-queue) &optional event-type)
   (loop
     (let ((event (event-read sheet)))
       (when (or (null event-type)
		 (typep event event-type))
	 (event-unread sheet event)
	 (return-from event-peek event))))))

;; Is there a lock here?

(defgeneric event-unread (sheet event)
  (:method ((sheet sheet-with-event-queue) event)
   (queue-unget (sheet-event-queue sheet) event)))

(defgeneric event-listen (client)
  (:method ((sheet sheet-with-event-queue))
   (not (queue-empty-p (sheet-event-queue sheet)))))



;; Standard 

(defclass standard-sheet-input-mixin (sheet-with-event-queue) ())

(defmethod dispatch-event ((sheet standard-sheet-input-mixin) event)
  (queue-event sheet event))

(defmethod dispatch-event ((sheet standard-sheet-input-mixin)
			   (event window-configuration-event))
  (handle-event sheet event))

;;; Immediate

(defclass immediate-sheet-input-mixin () ())

(defmethod dispatch-event ((sheet immediate-sheet-input-mixin) event)
  (handle-event sheet event))


;; delegate

(defclass delegate-sheet-input-mixin ()
    ((delegate :accessor delegate-sheet-delegate)))

(defmethod dispatch-event ((sheet delegate-sheet-input-mixin) event)
  (dispatch-event (delegate-sheet-delegate sheet) event))

(defclass sheet-mute-input-mixin () ())

(defmethod queue-event ((sheet sheet-mute-input-mixin) (event t))
  nil)

;;; Repaint protocol

(defgeneric dispatch-repaint (sheet region))

(defgeneric queue-repaint (sheet region)
  (:method (sheet region)
   (queue-event sheet region)))

(defgeneric handle-repaint (sheet medium region)
  (:method (sheet medium region)
   (with-sheet-medium-bound (sheet medium)
     (repaint-sheet sheet region))
   (when (typep sheet 'sheet-parent-mixin)
     (dolist (child (children-overlapping-region sheet region))
       (handle-repaint 
	 child medium
	 (untransform-region (sheet-transformation child) region))))))
	     
(defgeneric repaint-sheet (sheet region))


(defclass standard-repainting-medium () ())

(defmethod dispatch-repaint ((sheet standard-repainting-medium) region)
  (queue-repaint sheet region))

(defclass immediate-repainting-mixin () ())

(defmethod dispatch-repaint ((sheet immediate-repainting-mixin) region)
  (handle-repaint sheet region))

(defclass mute-repainting-mixin () ())

(defmethod dispatch-repaint ((sheet mute-repainting-mixin) region)
  (queue-repaint sheet region))

(defmethod repaint-sheet ((sheet mute-repainting-mixin) region)
  (declare (ignore region))
  nil)

;;; I think we have fun here to deal generate enter/exit events for un
;;; mirrored sheets
;;; Three cases:
;;; 1. Mouse motion event
;;; [Last mirrored sheet, position] --> [new mirrored sheet, position]
;;; 2. Enter: [last-mirrored sheet, position] -> [new sheet]
;;; 3. Exit: [last-mirrored sheet, position] -> [new sheet]

;;; We boil this down to mouse moved to [new mirrored sheet, position]
;;; we have a current stack of sheets 0....n
;;; Find m such that 0.. m - 1 contain mouse
;;; m...n require exit events
;;; We can then go deep down inside m sheet generating enter events

;;; Who do we generate motion events for?
;;; The 0th element of the trace is the mirrored window.

;;; Cases:
;;; 1. it is the mirrored window

;;; 2. Ancestor: We have gone deeper: perhaps it still counts as exiting
;;; We should just exit from the innermost sheet that encloses the new mirror
;;; We should enter/exit intermediate ancestors.
;;; Ancestor-x,y,z-->Sheet

;;; 3. Child: We have gone shallower: exit from all of the sheets
;;; We should probably exit from intermediate descendents
;;; Sheet- x,y,z --> Child

;;; 4. Share a common ancestor: exit from all of the sheets

;;; This code does not deal with cases (2) and (3) correctly.
;;; It will fail to generate enter/exit events for cases



(defun generate-crossing-events (port sheet x y modifiers button)
  (macrolet ((generate-enter-event (sheet)
	       `(let ((sheet ,sheet))
		  (dispatch-event
		    sheet
		    (make-instance 'pointer-enter-event
				   :x x
				   :y y
				   :button button
				   :sheet sheet
				   :modifiers modifiers))))
	     (generate-exit-event (sheet)
	       `(let ((sheet ,sheet))
		  (dispatch-event
		    sheet
		    (make-instance 'pointer-exit-event
				   :x x
				   :y y
				   :button button
				   :sheet sheet
				   :modifiers modifiers)))))
    (let ((v (port-trace-thing port)))
      ;; Pop up the stack of sheets
      ;;(print "going up")
      (unless (zerop (fill-pointer v))
	(let ((m (if (eq (aref v 0) sheet)
		     (let ((new-x x)
			   (new-y y))
		       (dotimes (i (fill-pointer v) (fill-pointer v))
			 (unless (zerop i)
			   (multiple-value-setq (new-x new-y)
			     (map-sheet-point*-to-child 
			       (aref v i) new-x new-y)))
			 (unless (region-contains-point*-p 
				   (sheet-region (aref v i)) new-x new-y)
			   (return i))))
		     0)))
	  (do ((i (1- (fill-pointer v)) (1- i)))
	      ((< i m))
	    (generate-exit-event (aref v i))
	    (unless (zerop i)
	      (generate-enter-event (aref v (1- i)))))
	  (setf (fill-pointer v) m)))
      
      ;; If its empty initialize it

      ;;(print "maybe going deeper")
      
      (when (region-contains-point*-p
	     (sheet-region sheet) x y)
	;;(print (cons "going deeper" v))
	
	(when (zerop (fill-pointer v))
	  (vector-push-extend sheet v)
	  (generate-enter-event sheet))
	;; We have to get the sheets into the correct coordinate
	;; space
	(loop for i from (1+ (position sheet v)) below (fill-pointer v)
	    do (multiple-value-setq
		   (x y)
		 (map-sheet-point*-to-child (aref v i) x y)))
	;; Add children
	(let ((new-x x)
	      (new-y y)
	      (sheet (aref v (1- (fill-pointer v))))
	      child)
	  (loop
	    (unless (typep sheet 'sheet-parent-mixin) (return nil))
	    (setq child (child-containing-point* sheet new-x new-y))
	    (unless child (return nil))
	    (generate-exit-event sheet)
	    (generate-enter-event child)
	    (multiple-value-setq (new-x new-y)
	      (map-sheet-point*-to-child child new-x new-y))
	    (setq sheet child)
	    (vector-push-extend child v)))))))
      
      
(warn "Checking for port")

(defun distribute-pointer-event (port mirrored-sheet event-type x y modifiers button)
  ;; Generate all the correct enter/exit events
  (generate-crossing-events port mirrored-sheet x y modifiers button)
  ;; dispatch event to the innermost sheet
  (let ((sheet (let ((v (port-trace-thing port)))
		 (and (not (zerop (fill-pointer v)))
		      (aref v (1- (fill-pointer v)))))))
    ;;(print sheet)
    (when (and sheet (sheet-port sheet))
      (multiple-value-bind (tx ty)
	  (untransform-point*
	    (sheet-device-transformation sheet) x y)
	(dispatch-event
	  sheet
	  (make-instance event-type
			 :sheet sheet
			 :native-x x
			 :native-y y
			 :x tx
			 :y ty
			 :modifiers modifiers
			 :button button))))))

(defmethod distribute-event (port event)
  (distribute-event-1 port event))

(defgeneric distribute-event-1 (port event)
  (:method (port event)
   (dispatch-event (event-sheet event) event))
  (:method (port (event keyboard-event))
   (let ((focus (or (port-keyboard-input-focus port)
		    (event-sheet event))))
     ;;--- Is this correct???
     (setf (slot-value event 'sheet) focus)
     (dispatch-event focus event)))
  (:method (port (event window-event))
   (dispatch-event 
     (window-event-mirrored-sheet event)
     event))
  (:method (port (event pointer-event))
   (distribute-pointer-event
     port
     (event-sheet event)
     (typecase event
       ((or pointer-exit-event pointer-enter-event)
	'pointer-motion-event)
       (t (type-of event)))
     (pointer-event-native-x event)
     (pointer-event-native-y event)
     (event-modifier-state event)
     (pointer-event-button event))))
	    
;;; Local event processing

(defun process-event-locally (sheet event)
  (declare (ignore sheet))
  (handle-event (event-sheet event) event))

(defun local-event-loop (sheet)
  (loop (process-event-locally sheet (event-read sheet))))

(defun port-event-wait (port waiter 
			&key (wait-reason #+Genera si:*whostate-awaiting-user-input*
					  #-Genera "CLIM Input")
			     timeout)
  (process-wait-with-timeout wait-reason timeout waiter) 
  (values))
