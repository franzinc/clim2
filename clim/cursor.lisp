;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: cursor.lisp,v 1.15 92/08/18 17:24:47 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved.
 Portions copyright (c) 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; This is the for the abstract cursor protocol
(define-protocol-class cursor ())

;;; Required methods for this protocol:
;;; (setf cursor-stream)
;;; cursor-visibility, setf

;;; A CLIM stream either has a cursor or it doesn't.
;;; CURSOR-ACTIVE says whether the cursor moves around.
;;; CURSOR-STATE says whether or not the cursor is currently visible.
;;; CURSOR-FOCUS says whether the cursor has the input focus (actually, this means
;;; that the stream has focus).  CLIM sets the focus while the mouse is in the window.
;;; CURSOR-VISIBILITY is CLIM 1.1 shorthand for hacking both the active and state flags
;;; at the same time.

;;; Turning a cursor on requires drawing it on the stream, somehow.  That needs
;;; to go through to the port level, where an appropriate host-window-system thing
;;; may be manipulated.  Should this be unified with the mouse cursor stuff?

;;--- The positions here should be a COORDINATE pair, no?
(defclass standard-text-cursor
	  (cursor region)			;--- why REGION?
    ((x :initarg :x :type coordinate)
     (y :initarg :y :type coordinate)
     (stream :initarg :stream)
     (flags :initform 0 :type fixnum)
     (width :initarg :width :type coordinate)
     ;;--- Until I can think of somewhere better --CER
     (plist :initform nil))
  (:default-initargs :x (coordinate 0) :y (coordinate 0)
		     :width (coordinate 8)
		     :stream nil))

(defmethod bounding-rectangle* ((cursor standard-text-cursor))
  (with-slots (x y width) cursor
    (values x y (+ x width) (+ y width))))

(defconstant cursor_active (byte 1 0))
(defconstant cursor_state  (byte 1 1))
(defconstant cursor_focus  (byte 1 2))

(defun decode-cursor-flags (flags)
  #+Genera (declare (values active state focus))
  (values (ldb-test cursor_active flags)
	  (ldb-test cursor_state flags)
	  (ldb-test cursor_focus flags)))

;;; Required method
(defmethod (setf cursor-stream) (new-value (cursor standard-text-cursor))
  (setf (slot-value cursor 'stream) new-value))

(defmethod cursor-position ((cursor standard-text-cursor))
  (with-slots (x y) cursor
    (values x y)))

(defgeneric* (setf cursor-position) (x y cursor))
(defmethod* (setf cursor-position) (x y (cursor standard-text-cursor))
  (cursor-set-position cursor x y))

(defmethod cursor-set-position ((cursor standard-text-cursor) nx ny &optional fastp)
  (with-slots (x y flags stream) cursor
    (unless (and (= x (coordinate nx))
		 (= y (coordinate ny)))
      (if fastp
	  (setf x (coordinate nx)
		y (coordinate ny))
	  (let ((active (cursor-active cursor)))
	    (unwind-protect
		(progn
		  (when active
		    (setf (cursor-active cursor) nil))
		  (setf x (coordinate nx)
			y (coordinate ny)))
	      (when active
		(setf (cursor-active cursor) active))))))))

(defmethod (setf cursor-state) (new-state (cursor standard-text-cursor))
  (with-slots (flags) cursor
    (multiple-value-bind (active state focus)
	(decode-cursor-flags flags)
      (declare (ignore active focus))
      (setf (ldb cursor_state flags) (if new-state 1 0))
      (unless (eq state new-state)
	(note-cursor-change cursor 'cursor-state state new-state)))))

(defmethod cursor-state ((cursor standard-text-cursor))
  (ldb-test cursor_state (slot-value cursor 'flags)))

(defmethod (setf cursor-active) (new-active (cursor standard-text-cursor))
  (with-slots (flags) cursor
    (multiple-value-bind (active state focus)
	(decode-cursor-flags flags)
      (declare (ignore state focus))
      (setf (ldb cursor_active flags) (if new-active 1 0))
      (unless (eq active new-active)
	(note-cursor-change cursor 'cursor-active active new-active)))))

(defmethod cursor-active ((cursor standard-text-cursor))
  (ldb-test cursor_active (slot-value cursor 'flags)))

(defmethod (setf cursor-focus) (new-focus (cursor standard-text-cursor))
  (with-slots (flags) cursor
    (multiple-value-bind (active state focus)
	(decode-cursor-flags flags)
      (declare (ignore active state))
      (setf (ldb cursor_focus flags) (if new-focus 1 0))
      (unless (eq focus new-focus)
	(note-cursor-change cursor 'cursor-focus focus new-focus)))))

(defmethod cursor-focus ((cursor standard-text-cursor))
  (ldb-test cursor_focus (slot-value cursor 'flags)))

(defmethod cursor-visibility ((cursor standard-text-cursor))
  (cursor-active cursor))
 
(defmethod (setf cursor-visibility) (visibility (cursor standard-text-cursor))
  (setf (cursor-state cursor) 
        (case visibility
	  (:off nil)
	  ((nil) nil)
	  ((t :on) t)))
  (setf (cursor-active cursor) 
        (case visibility
	  (:off t)
	  ((nil) nil)
	  ((t :on) t))))

(defmacro with-cursor-state ((state &optional stream) &body body)
  (default-input-stream stream)
  `(let* ((cursor (and (extended-input-stream-p ,stream)
		       (stream-text-cursor ,stream)))
	  (old-state (and cursor (cursor-state cursor)))
	  (abort-p t))
     (unwind-protect
	 (progn (when cursor
		  (cond ((eq old-state ,state))
			(t
			 (setf (cursor-state cursor) ,state)
			 (setf abort-p nil))))
		,@body)
       (when cursor
	 (unless abort-p
	   (setf (cursor-state cursor) old-state))))))

(defmethod cursor-width-and-height-pending-protocol ((cursor t))
  (values 8 12))

(defmethod cursor-width-and-height-pending-protocol ((cursor standard-text-cursor))
  (values (slot-value cursor 'width)
	  (let ((stream (slot-value cursor 'stream)))
	    (+ (stream-line-height stream) (stream-vertical-spacing stream)))))

(defmethod note-cursor-change ((cursor standard-text-cursor) type old new)
  ;;; type is currently one of CURSOR-ACTIVE, -FOCUS, or -STATE
  (let ((stream (slot-value cursor 'stream)))
    (when (and stream (port stream))
      (port-note-cursor-change (port stream) cursor stream type old new))))

;;; The port needs to know about state transitions.  We originally had one
;;; function that simply knew when the cursor was changing from "on" to "off"
;;; or vice versa.  Now, we need a function that knows about all the possible
;;; state transitions.  For example, the cursor might be getting the focus, and
;;; thus change from a hollow rectangle to a filled one (or in Genera, it might
;;; start blinking).
(defmethod port-note-cursor-change :after ((port basic-port) 
					   cursor stream type old new)
  (declare (ignore old type cursor))
  (setf (port-keyboard-input-focus port) (and new stream)))

(defmethod port-note-cursor-change ((port basic-port) 
				    cursor stream (type (eql 'cursor-state)) old new)
  (declare (ignore old))
  (let ((active (cursor-active cursor)))
    (when active
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
	(if new
	    (draw-cursor cursor stream x y t)
	    (draw-cursor cursor stream x y nil))))))

(defmethod port-note-cursor-change ((port basic-port) 
				    cursor stream (type (eql 'cursor-active)) old new)
  (declare (ignore old))
  (let ((state (cursor-state cursor)))
    (when state
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
	(if new
	    (draw-cursor cursor stream x y t)
	    (draw-cursor cursor stream x y nil))))))

(defmethod port-note-cursor-change ((port basic-port) 
				    cursor stream (type (eql 'cursor-focus)) old new)
  (let ((active (cursor-active cursor))
	(state (cursor-state cursor)))
    (declare (ignore focus))
    (when (and active state)
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
	;; erase it with old-focus
	(draw-cursor cursor stream x y nil old)
	;; draw it with new-focus
	(draw-cursor cursor stream x y t new)))))

;; DRAW-CURSOR is invoked to draw or erase the cursor.
(defmethod draw-cursor ((cursor standard-text-cursor) stream x y on-p
			&optional (focus nil focus-p))
  ;;--- protocol violations:  output recording protocol (with-output-recording-options)
  ;;---                       graphics protocol (draw-rectangle*)
  ;;---                       output protocol (stream-line-height)
  (let ((height (stream-line-height stream))
	(width (slot-value cursor 'width)))
    (unless focus-p (setq focus (cursor-focus cursor)))
    (with-output-recording-options (stream :record nil)
      (draw-rectangle* stream x y (+ x width) (+ y height)
		       :filled focus
		       :ink #+++ignore (if on-p +foreground-ink+ +background-ink+)
			    #---ignore +flipping-ink+))
    (force-output stream)))
