;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
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

(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; This is the for the abstract cursor protocol
(defclass fundamental-cursor
	  ()
    ()
  )



;;; Required methods for this protocol:
;;; (setf cursor-stream)

;;; Not clear whether we need this -p method.
(defmethod cursor-p (thing)
  (declare (ignore thing))
  nil)

(defmethod cursor-p ((thing fundamental-cursor))
  t)

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
	  (fundamental-cursor region)
    ((x :initarg :x)
     (y :initarg :y)
     (stream :initarg :stream)
     (flags :initform 0)
     (width :initarg :width)
     )
  (:default-initargs :x 0 :y 0
		     :width 8
		     :stream nil)
  )

(defmethod bounding-rectangle* ((x text-cursor))
  (with-slots (x y width) x
    (values x y (+ x width) (+ y width))))

(defmethod entity-set-position ((c text-cursor) x y)
  (setf (slot-value c 'x) x
	(slot-value c 'y) y))
	
(defconstant cursor_active (byte 1 0))
(defconstant cursor_state (byte 1 1))
(defconstant cursor_focus (byte 1 2))

(defun decode-cursor-flags (flags)
  #+Genera
  (declare (values active state focus))
  (values (ldb-test cursor_active flags)
	  (ldb-test cursor_state flags)
	  (ldb-test cursor_focus flags)))

;;; Required method
(defmethod (setf cursor-stream) (new-value (cursor text-cursor))
  (setf (slot-value cursor 'stream) new-value))

#+Silica
(defmethod bounding-rectangle* ((cursor text-cursor))
  (values (slot-value cursor 'x) (slot-value cursor 'y)
	  (slot-value cursor 'x) (slot-value cursor 'y)))

#+Silica
(defmethod* (setf bounding-rectangle*) (left top right bottom (cursor text-cursor))
  (assert (and (= left right) (= bottom top)))
  (let ((active (cursor-active cursor)))
    (with-slots (x y) cursor
      (unless (and (= x left)
		   (= y top))
	(unwind-protect
	    (progn
	      (when active
		(setf (cursor-active cursor) nil))
	      (setf x left y top))
	  (when active
	    (setf (cursor-active cursor) active)))))))

(defmethod (setf cursor-state) (new-state (cursor text-cursor))
  (multiple-value-bind (active state focus)
      (decode-cursor-flags (slot-value cursor 'flags))
    (declare (ignore active focus))
    (setf (ldb cursor_state (slot-value cursor 'flags)) (if new-state 1 0))
    (unless (eql state new-state)
      (note-cursor-change cursor 'cursor-state state new-state))))

(defmethod cursor-state ((cursor text-cursor))
  (ldb-test cursor_state (slot-value cursor 'flags)))

(defmethod (setf cursor-active) (new-active (cursor text-cursor))
  (multiple-value-bind (active state focus)
      (decode-cursor-flags (slot-value cursor 'flags))
    (declare (ignore state focus))
    (setf (ldb cursor_active (slot-value cursor 'flags)) (if new-active 1 0))
    (unless (eql active new-active)
      (note-cursor-change cursor 'cursor-active active new-active))))

(defmethod cursor-active ((cursor text-cursor))
  (ldb-test cursor_active (slot-value cursor 'flags)))

(defmethod (setf cursor-focus) (new-focus (cursor text-cursor))
  (multiple-value-bind (active state focus)
      (decode-cursor-flags (slot-value cursor 'flags))
    (declare (ignore active state))
    (setf (ldb cursor_focus (slot-value cursor 'flags)) (if new-focus 1 0))
    (unless (eql focus new-focus)
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
    (when stream
      (port-note-cursor-change (port stream) cursor stream type old new))))

;;; The port needs to know about state transitions.  We originally had one
;;; function that simply knew when the cursor was changing from "on" to "off"
;;; or vice versa.  Now, we need a function that knows about all the possible
;;; state transitions.  For example, the cursor might be getting the focus, and
;;; thus change from a hollow rectangle to a filled one (or in Genera, it might
;;; start blinking).
(defmethod port-note-cursor-change ((port port) 
				    cursor stream (type (eql 'cursor-state)) old new)
  (declare (ignore old))
  (let ((active (cursor-active cursor)))
    (when active
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
	(if new
	    (cursor-draw-cursor cursor stream x y t)
	    (cursor-draw-cursor cursor stream x y nil))))))

(defmethod port-note-cursor-change ((port port) 
				    cursor stream (type (eql 'cursor-active)) old new)
  (declare (ignore old))
  (let ((state (cursor-state cursor)))
    (when state
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
	(if new
	    (cursor-draw-cursor cursor stream x y t)
	    (cursor-draw-cursor cursor stream x y nil))))))

(defmethod port-note-cursor-change ((port port) 
				    cursor stream (type (eql 'cursor-focus)) old new)
  (let ((active (cursor-active cursor))
	(state (cursor-state cursor)))
    (declare (ignore focus))
    (when (and active state)
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
	;; erase it with old-focus
	(cursor-draw-cursor cursor stream x y nil old)
	;; draw it with new-focus
	(cursor-draw-cursor cursor stream x y t new)))))

(defmethod cursor-draw-cursor ((cursor text-cursor) stream x y on-p
			       &optional (focus nil focus-p))
  ;; --- protocol violations:  output-recording-options
  ;; ---                       graphics protocol
  ;; ---                       output protocol (line-height)
  #-Ignore (declare (ignore on-p))
  (let ((height (stream-line-height stream))
	(width (slot-value cursor 'width)))
    (unless focus-p (setq focus (cursor-focus cursor)))
    (with-output-recording-options (stream :record-p nil)
      (draw-rectangle* stream x y (+ x width) (+ y height) :filled focus
		       :ink +flipping-ink+ #+Ignore (if on-p +foreground+ +background+)))
    ;; --- do we really want to do this here??
    (force-output stream)))


#||
;;; Move this stuff to the bottom where it hopefully won't be in the way...
#-Silica
(defmethod entity-position ((cursor text-cursor))
  (values (slot-value cursor 'x) (slot-value cursor 'y)))

#-Silica
(defmethod entity-set-position ((cursor text-cursor) nx ny)
  (multiple-value-bind (active state focus)
      (decode-cursor-flags (slot-value cursor 'flags))
    (with-slots (x y) cursor
      (unless (and (= x nx)
		   (= y ny))
	(when (and state active focus)
	  (set-cursor-visibility cursor nil))
	(setf x nx y ny)
	(when (and state active focus)
	  (set-cursor-visibility cursor T))))))

||#



(defun cursor-visibility (x) nil)
(defun (setf cursor-visibility) (nv x) nil)
