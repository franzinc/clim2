;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved.
;;; Portions copyright (c) 1991, 1992 Symbolics, Inc.  All rights reserved."

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

(defvar *default-cursor-color* +foreground-ink+)

(defclass standard-text-cursor
          (cursor region)                        ;--- why REGION?
    ((x :initarg :x :type coordinate)
     (y :initarg :y :type coordinate)
     (stream :initarg :stream)
     (flags :initform 0 :type fixnum)
     (width :initarg :width :type coordinate)
     ;;--- Until I can think of somewhere better --CER
     (plist :initform nil)
     (flipping-ink :initform (make-flipping-ink *default-cursor-color*
                                                +background-ink+)))
  (:default-initargs :x (coordinate 0) :y (coordinate 0)
                     :width (coordinate 8)
                     :stream nil))

(defmethod bounding-rectangle* ((cursor standard-text-cursor))
  (with-slots (x y width) cursor
    ;; this is surely wrong (ie what about height) - but frankly who
    ;; cares?? --CIM
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
  (with-slots (x y stream) cursor
    (unless (and (= x (coordinate nx))
                 (= y (coordinate ny)))
      (let ((state (cursor-state cursor))
            (active (cursor-active cursor)))
        (if (or fastp
                (not (and state active)))
            (setf x (coordinate nx)
                  y (coordinate ny))
          ;; Turn it off and then turn it on to make it move
          (unwind-protect
              (progn
                (setf (cursor-state cursor) nil)
                (setf x (coordinate nx)
                      y (coordinate ny)))
            (setf (cursor-state cursor) t)))))))

(defmethod (setf cursor-state) (new-state (cursor standard-text-cursor))
  (with-slots (flags) cursor
    (multiple-value-bind (active state focus)
        (decode-cursor-flags flags)
      (declare (ignore active focus))
      (setf (ldb cursor_state flags) (if new-state 1 0))
      (note-cursor-change cursor 'cursor-state state new-state))))

(defmethod cursor-state ((cursor standard-text-cursor))
  (ldb-test cursor_state (slot-value cursor 'flags)))

(defmethod (setf cursor-active) (new-active (cursor standard-text-cursor))
  (with-slots (flags) cursor
    (multiple-value-bind (active state focus)
        (decode-cursor-flags flags)
      (declare (ignore state focus))
      (setf (ldb cursor_active flags) (if new-active 1 0))
      (note-cursor-change cursor 'cursor-active active new-active))))

(defmethod cursor-active ((cursor standard-text-cursor))
  (ldb-test cursor_active (slot-value cursor 'flags)))

(defmethod (setf cursor-focus) (new-focus (cursor standard-text-cursor))
  (with-slots (flags) cursor
    (multiple-value-bind (active state focus)
        (decode-cursor-flags flags)
      (declare (ignore active state))
      (setf (ldb cursor_focus flags) (if new-focus 1 0))
      (note-cursor-change cursor 'cursor-focus focus new-focus))))

(defmethod cursor-focus ((cursor standard-text-cursor))
  (ldb-test cursor_focus (slot-value cursor 'flags)))

(defmethod (setf cursor-color) (color (cursor standard-text-cursor))
  (with-slots (flipping-ink flags) cursor
    (multiple-value-bind (active state focus)
        (decode-cursor-flags flags)
      (declare (ignore active focus))
      (unwind-protect
          (progn
            (when state
              (note-cursor-change cursor 'cursor-state t nil))
            (setf flipping-ink
              (make-flipping-ink color +background-ink+)))
        (when state
          (note-cursor-change cursor 'cursor-state nil t))))
    color))

(defmethod cursor-color ((cursor standard-text-cursor))
  (with-slots (flipping-ink) cursor
    (multiple-value-bind (ink1 ink2)
        (decode-flipping-ink flipping-ink)
      (declare (ignore ink2))
      ink1)))

(defmethod cursor-visibility ((cursor standard-text-cursor))
  (and (cursor-active cursor)
       (if (cursor-state cursor)
           :on
         :off)))

(defmethod (setf cursor-visibility) (visibility (cursor standard-text-cursor))
  (let ((active (member visibility '(t :on :off) :test  #'eq)))
    (setf (cursor-active cursor) active)
    (setf (cursor-state cursor)
      (and active
           (not (eq visibility :off))))))

(defmacro with-cursor-state ((stream state) &body body)
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

(defmacro with-cursor-active ((stream active) &body body)
  (default-input-stream stream)
  `(let* ((cursor (and (extended-input-stream-p ,stream)
                       (stream-text-cursor ,stream)))
          (old-active (and cursor (cursor-active cursor)))
          (abort-p t))
     (unwind-protect
         (progn (when cursor
                  (cond ((eq old-active ,active))
                        (t
                         (setf (cursor-active cursor) ,active)
                         (setf abort-p nil))))
                ,@body)
       (when cursor
         (unless abort-p
           (setf (cursor-active cursor) old-active))))))

#+(or aclpc acl86win32)
(defmethod cursor-width-and-height-pending-protocol ((cursor t))
  (values 8 12))

(defmethod cursor-width-and-height-pending-protocol
    ((cursor standard-text-cursor))
  (with-slots (width stream) cursor
    (values width
            (stream-line-height stream))))

(defmethod note-cursor-change ((cursor standard-text-cursor) type old new)
  ;;; type is currently one of CURSOR-ACTIVE, -FOCUS, or -STATE
  (let ((stream (slot-value cursor 'stream)))
    (when (and stream (port stream))
      (let ((frame (pane-frame stream)))
	(when (and frame (eq (frame-state frame) :enabled))
	  (port-note-cursor-change (port stream) cursor stream type old new))))))

;;; The port needs to know about state transitions.  We originally had one
;;; function that simply knew when the cursor was changing from "on" to "off"
;;; or vice versa.  Now, we need a function that knows about all the possible
;;; state transitions.  For example, the cursor might be getting the focus, and
;;; thus change from a hollow rectangle to a filled one (or in Genera, it might
;;; start blinking).

(defmethod port-note-cursor-change ((port basic-port)
                                    cursor stream (type (eql 'cursor-state)) old new)
  (let ((active (cursor-active cursor))
        (focus (cursor-focus cursor)))
    (when active
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
        (port-draw-cursor port cursor stream x y old new focus)))))

(defmethod port-note-cursor-change ((port basic-port)
                                    cursor stream (type (eql 'cursor-active)) old new)
  (let ((state (cursor-state cursor))
        (focus (cursor-focus cursor)))
    (when state
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
        (port-draw-cursor port cursor stream x y old new focus)))))

(defmethod port-note-cursor-change ((port basic-port)
                                    cursor stream (type (eql 'cursor-focus)) old new)
  (let ((active (cursor-active cursor))
        (state (cursor-state cursor)))
    (when (and active
               state
               (not (eq old new)))
      (multiple-value-bind (x y) (bounding-rectangle* cursor)
        ;; erase it with old-focus
        (port-draw-cursor port cursor stream x y t nil old)
        ;; draw it with new-focus
        (port-draw-cursor port cursor stream x y nil t new)))))

;; PORT-DRAW-CURSOR is invoked to draw or erase the cursor.
(defmethod port-draw-cursor
    ((port basic-port) (cursor standard-text-cursor) stream x1 y1 old new focus)
  ;;--- protocol violations:  output recording protocol (with-output-recording-options)
  ;;---                       graphics protocol (draw-rectangle*)
  ;;---                       output protocol (stream-line-height)
  (let ((height (stream-line-height stream))
        (width (slot-value cursor 'width))
        (flipping-ink (slot-value cursor 'flipping-ink)))
    (unless (eq old new)
      (with-output-recording-options (stream :record nil)
        (with-drawing-options (stream :ink flipping-ink)
          (let ((x2 (+ x1 width))
                (y2 (+ y1 height)))
            (with-identity-transformation (stream)
              (if focus
                  (draw-rectangle* stream x1 y1 (1+ x2) (1+ y2)
                                   :filled t)
                (progn
                  (draw-line* stream x1 y1 x2 y1)
                  (draw-line* stream x1 y2 x2 y2)
                  (draw-line* stream x1 (1+ y1) x1 (1- y2))
                  (draw-line* stream x2 (1+ y1) x2 (1- y2)))))))
        (force-output stream)))))
