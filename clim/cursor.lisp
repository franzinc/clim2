;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: cursor.lisp,v 1.7 92/03/04 16:21:20 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; This is the for the abstract cursor protocol
(define-protocol-class cursor ())

;;; Required methods for this protocol:
;;; (setf cursor-stream)
;;; cursor-visibility, setf

;;; A CLIM stream either has a cursor or it doesn't.
;;;
;;; The cursor is "active" or "inactive," meaning it moves around or doesn't.
;;; This is the only exported interface.  Some programs temporarily turn off the cursor
;;; while at top-level (such as AVV) but turn the cursor back on when actually reading
;;; input.
;;;
;;; The cursor is either "On" or "Off" (STATE of T or NIL), meaning it's visible or not.
;;; CLIM turns the cursor on during input wait, and off otherwise.
;;;
;;; The cursor "has focus" or not (actually, this means that the stream has focus).
;;; CLIM sets the focus while the mouse is in the window.
;;;
;;; Turning a cursor on requires drawing it on the stream, somehow.  That needs
;;; to go through to the port level, where an appropriate host-window-system thing
;;; may be manipulated.  Should this be unified with the mouse cursor stuff?

(defclass text-cursor
	  (cursor #+Silica region)
    ((x :initarg :x)
     (y :initarg :y)
     (stream :initarg :stream)
     (flags :initform 0)
     (width :initarg :width)
     (plist :initform nil))
  (:default-initargs :x 0 :y 0
		     :width 8
		     :stream nil))

(defmethod bounding-rectangle* ((cursor text-cursor))
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
(defmethod (setf cursor-stream) (new-value (cursor text-cursor))
  (setf (slot-value cursor 'stream) new-value))

(defmethod cursor-position* ((cursor text-cursor))
  (with-slots (x y) cursor
    (values x y)))

#-Silica
(defmethod cursor-set-position* ((cursor text-cursor) nx ny)
  (with-slots (x y visibility) cursor
    (unless (and (= x nx)
		 (= y ny))
      (when (eq visibility :on)
	(draw-cursor cursor nil))
      (setf x nx y ny)
      (when (eq visibility :on)
	(draw-cursor cursor T)))))

#+Silica
(defmethod cursor-set-position* ((cursor text-cursor) nx ny)
  (with-slots (x y visibility) cursor
    (setf x nx y ny)))

(defmethod (setf cursor-state) (new-state (cursor text-cursor))
  (multiple-value-bind (active state focus)
      (decode-cursor-flags (slot-value cursor 'flags))
    (declare (ignore active focus))
    (setf (ldb cursor_state (slot-value cursor 'flags)) (if new-state 1 0))
    (unless (eq state new-state)
      (note-cursor-change cursor 'cursor-state state new-state))))

(defmethod cursor-state ((cursor text-cursor))
  (ldb-test cursor_state (slot-value cursor 'flags)))

(defmethod (setf cursor-active) (new-active (cursor text-cursor))
  (multiple-value-bind (active state focus)
      (decode-cursor-flags (slot-value cursor 'flags))
    (declare (ignore state focus))
    (setf (ldb cursor_active (slot-value cursor 'flags)) (if new-active 1 0))
    (unless (eq active new-active)
      (note-cursor-change cursor 'cursor-active active new-active))))

(defmethod cursor-active ((cursor text-cursor))
  (ldb-test cursor_active (slot-value cursor 'flags)))

(defmethod (setf cursor-focus) (new-focus (cursor text-cursor))
  (multiple-value-bind (active state focus)
      (decode-cursor-flags (slot-value cursor 'flags))
    (declare (ignore active state))
    (setf (ldb cursor_focus (slot-value cursor 'flags)) (if new-focus 1 0))
    (unless (eq focus new-focus)
      (note-cursor-change cursor 'cursor-focus focus new-focus))))

(defmethod cursor-focus ((cursor text-cursor))
  (ldb-test cursor_focus (slot-value cursor 'flags)))

(defmethod cursor-width-and-height-pending-protocol ((cursor t))
  (values 1 1))

(defmethod cursor-width-and-height-pending-protocol ((cursor text-cursor))
  (values (slot-value cursor 'width)
	  (stream-line-height (slot-value cursor 'stream))))

(defmethod note-cursor-change ((cursor text-cursor) type old new)
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

(defmethod port-note-cursor-change :after ((port port)
					   cursor 
					   stream 
					   type 
					   old
					   new)
  (declare (ignore old type cursor))
  (setf (port-keyboard-input-focus port) 
    (and new stream)))

(defmethod port-note-cursor-change ((port port) 
				    cursor stream (type (eql 'cursor-state)) old new)
  (declare (ignore old))
  (let ((active (cursor-active cursor)))
    (when active
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
	(if new
	    (draw-cursor cursor stream x y t)
	    (draw-cursor cursor stream x y nil))))))

(defmethod port-note-cursor-change ((port port) 
				    cursor stream (type (eql 'cursor-active)) old new)
  (declare (ignore old))
  (let ((state (cursor-state cursor)))
    (when state
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
	(if new
	    (draw-cursor cursor stream x y t)
	    (draw-cursor cursor stream x y nil))))))

(defmethod port-note-cursor-change ((port port) 
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

;;; DRAW-CURSOR is invoked to draw or erase the cursor.  on-p T = draw it; NIL = erase it

#+Silica
(defmethod draw-cursor ((cursor text-cursor) stream x y on-p
			       &optional (focus nil focus-p))
  ;; --- protocol violations:  output-recording-options
  ;; ---                       graphics protocol
  ;; ---                       output protocol (line-height)
  #-Ignore (declare (ignore on-p))
  (let ((height (stream-line-height stream))
	(width (slot-value cursor 'width)))
    (unless focus-p (setq focus (cursor-focus cursor)))
    (with-output-recording-options (stream :record nil)
      (draw-rectangle* stream x y (+ x width) (+ y height) :filled focus
		       :ink +flipping-ink+ #+Ignore (if on-p +foreground-ink+ +background-ink+)))
    ;; --- do we really want to do this here??
    (force-output stream)))

#-Silica
(defmethod draw-cursor ((cursor text-cursor) on-p)
  (with-slots (x y stream) cursor
    (declare (type coordinate x y))
    (when stream
      ;; --- protocol violations:  output-recording-options
      ;; ---                       graphics protocol
      ;; ---                       output protocol (line-height)
      (let ((height (stream-line-height stream)))
	(declare (type coordinate height))
	(with-output-recording-options (stream :record nil)
	  (draw-line-internal
	    stream 0 0
	    ;;--- gack
	    (the fixnum x) (the fixnum (+ y height))
	    (the fixnum (+ x 2)) (the fixnum (+ y height 2))
	    (if on-p +foreground-ink+ +background-ink+) +highlighting-line-style+)
	  (draw-line-internal
	    stream 0 0
	    (the fixnum x) (the fixnum (+ y height))
	    (the fixnum (- x 2)) (the fixnum (+ y height 2))
	    (if on-p +foreground-ink+ +background-ink+) +highlighting-line-style+))
	;; --- do we really want to do this here??
	(force-output stream)))))

(defmethod cursor-visibility ((cursor cursor))
  (cursor-active cursor))
 
(defmethod (setf cursor-visibility) (nv (cursor cursor))
  (setf (cursor-active cursor) 
	(case nv
	  (:off nil)
	  ((nil) nil)
	  ((t :on) t))))
