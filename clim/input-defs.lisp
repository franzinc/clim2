;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: input-defs.lisp,v 1.14 92/09/22 19:37:16 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; Pointers and pointer events

(define-protocol-class pointer ())

(defclass standard-pointer 
	  (pointer)
    ((port :reader port :initform nil :initarg :port)
     (graft :reader graft :initform nil :initarg :graft)
     (sheet :accessor pointer-sheet :initform nil)
     ;; Position in root (graft) coordinates 
     (x-position :accessor pointer-x-position
		 :type coordinate :initform (coordinate 0))
     (y-position :accessor pointer-y-position
		 :type coordinate :initform (coordinate 0))
     (native-x-position :accessor pointer-native-x-position
			:type coordinate :initform (coordinate 0))
     (native-y-position :accessor pointer-native-y-position
			:type coordinate :initform (coordinate 0))
     (button-state :accessor pointer-button-state
		   :type fixnum :initform 0)
     (position-changed :accessor pointer-position-changed)
     ;; In case the pointer's cursor needs to be managed more directly...
     (cursor :initform :default :reader pointer-cursor)
     (cursor-pattern :accessor pointer-cursor-pattern)
     (cursor-width :accessor pointer-cursor-width)
     (cursor-height :accessor pointer-cursor-height)
     (cursor-x-offset :accessor pointer-cursor-x-offset)
     (cursor-y-offset :accessor pointer-cursor-y-offset)))

;; Prevent using the instance-ref instructions on these, since they
;; usually have :BEFORE/:AFTER qualifiers on the SETF methods that will
;; cause the instance-ref instructions to take a slow trap.
#+(or Genera Minima)
(progn
  (declaim (notinline pointer-sheet (setf pointer-sheet))))

(defmethod (setf pointer-cursor) (cursor (pointer standard-pointer))
  (port-set-pointer-cursor (port pointer) pointer cursor)
  (setf (slot-value pointer 'cursor) cursor))

(defmethod pointer-position ((pointer standard-pointer))
  (with-slots (x-position y-position) pointer
    (values x-position y-position)))

(defgeneric* (setf pointer-position) (x y pointer))
(defmethod* (setf pointer-position) (x y (pointer standard-pointer))
  (pointer-set-position pointer x y))

;; X and Y are in stream coordinates
(defmethod pointer-set-position ((pointer standard-pointer) x y 
				 &optional port-did-it)
  (with-slots (x-position y-position position-changed
	       sheet native-x-position native-y-position) pointer
    (let ((x (coordinate x))
	  (y (coordinate y)))
      (setf x-position x
	    y-position y
	    position-changed t))
    (unless port-did-it
      (multiple-value-bind (native-x native-y)
	  (transform-position (sheet-device-transformation sheet) x y)
	(setf native-x-position native-x
	      native-y-position native-y)
	(port-set-pointer-position (port pointer) pointer native-x native-y)))
    (values x y)))

(defmethod pointer-native-position ((pointer standard-pointer))
  (with-slots (native-x-position native-y-position) pointer
    (values native-x-position native-y-position)))

(defgeneric* (setf pointer-native-position) (x y pointer))
(defmethod* (setf pointer-native-position) (x y (pointer standard-pointer))
  (pointer-set-native-position pointer x y))

;; X and Y are in native (device) coordinates
(defmethod pointer-set-native-position ((pointer standard-pointer) x y
					&optional port-did-it)
  (with-slots (x-position y-position position-changed
	       sheet native-x-position native-y-position) pointer
    (let ((x (coordinate x))
	  (y (coordinate y)))
      (setf native-x-position x
	    native-y-position y
	    position-changed t))
    (unless port-did-it
      (multiple-value-bind (sheet-x sheet-y)
	  (untransform-position (sheet-device-transformation sheet) x y)
	(setf x-position sheet-x
	      y-position sheet-y)
	(port-set-pointer-position (port pointer) pointer x y)))
    (values x y)))

(defmethod (setf pointer-sheet) :before (new-value (pointer standard-pointer))
  (let ((sheet (slot-value pointer 'sheet)))
    (unless (eq new-value sheet)
      (when (and sheet (port sheet))
	;;--- Horrible cross-protocol modularity violation here, but
	;;--- it's hours before AAAI.  Note that this can cause blowouts
	;;--- in multi-processing systems if the function that does the
	;;--- unhighlighting depends on application state.
	(when (output-recording-stream-p sheet)
	  (set-highlighted-presentation sheet nil nil))))))

(defmethod pointer-decache ((pointer standard-pointer))
  (let ((sheet (slot-value pointer 'sheet)))
    (when (and sheet (port sheet))
      (let ((native-x-position (pointer-native-x-position pointer))
	    (native-y-position (pointer-native-y-position pointer)))
	(multiple-value-setq (native-x-position native-y-position)
	  (untransform-position (sheet-native-transformation sheet) 
				native-x-position native-y-position))
	(setf (pointer-x-position pointer) native-x-position
	      (pointer-y-position pointer) native-y-position)))))

(defmethod query-pointer ((pointer standard-pointer))
  (declare (values sheet x y))
  (with-slots (sheet x-position y-position) pointer
    (values sheet x-position y-position)))

(defun pointer-state-changed (pointer old-sheet old-x old-y)
  (multiple-value-bind (sheet x-position y-position) (query-pointer pointer)
    (values
      (or (not (eq sheet old-sheet))
	  ;; compare coordinates with eql, not =, because null values can be passed in
	  (not (eq old-x x-position))
	  (not (eq old-y y-position)))
      sheet x-position y-position)))

