;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: input-defs.lisp,v 1.7 92/04/15 11:46:46 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; Pointers and pointer events

(define-protocol-class pointer ())

(defclass standard-pointer 
	  (pointer)
    ;;--- Shouldn't this be POINTER-PORT?
    ((root :accessor pointer-root :initform nil :initarg :root)
     (window :accessor pointer-window :initform nil)
     ;; Position in root coordinates 
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
  (declaim (notinline pointer-window (setf pointer-window))))

(defmethod pointer-position ((pointer standard-pointer))
  (with-slots (x-position y-position) pointer
    (values x-position y-position)))

(defmethod pointer-set-position ((pointer standard-pointer) new-x new-y)
  (with-slots (x-position y-position position-changed) pointer
    (setf x-position (coordinate new-x)
	  y-position (coordinate new-y)
	  position-changed t))
  (values new-x new-y))

#+CLIM-1-compatibility
(define-compatibility-function (pointer-position* pointer-position)
			       (pointer)
  (pointer-position pointer))

#+CLIM-1-compatibility
(define-compatibility-function (pointer-set-position* pointer-set-position)
			       (pointer x y)
  (pointer-set-position pointer x y))

(defmethod pointer-native-position ((pointer standard-pointer))
  (with-slots (native-x-position native-y-position) pointer
    (values native-x-position native-y-position)))

(defmethod pointer-set-native-position ((pointer standard-pointer) new-x new-y)
  (with-slots (native-x-position native-y-position position-changed) pointer
    (setf native-x-position (coordinate new-x)
	  native-y-position (coordinate new-y)
	  position-changed t))
  (values new-x new-y))

(defmethod (setf pointer-window) :before (new-value (pointer standard-pointer))
   (with-slots (window) pointer
     (unless (eq new-value window)
       (when (and window (port window))
	 ;;--- Horrible cross-protocol modularity violation here, but
	 ;;--- it's hours before AAAI.  Note that this can cause blowouts
	 ;;--- in multi-processing systems if the function that does the
	 ;;--- unhighlighting depends on application state.
	 (when (output-recording-stream-p window)
	   (set-highlighted-presentation window nil nil))))))

(defmethod pointer-decache ((pointer pointer))
  (with-slots (window) pointer
    (when (and window (port window))
      (let ((native-x-position (pointer-native-x-position pointer))
	    (native-y-position (pointer-native-y-position pointer)))
	(multiple-value-setq (native-x-position native-y-position)
	  (untransform-position (sheet-native-transformation window) 
				native-x-position native-y-position))
	(setf (pointer-x-position pointer) native-x-position
	      (pointer-y-position pointer) native-y-position)))))

(defmethod query-pointer ((pointer standard-pointer))
  (declare (values window x y))
  (with-slots (window x-position y-position) pointer
    (values window x-position y-position)))

(defun pointer-state-changed (pointer old-window old-x old-y)
  (multiple-value-bind (window x-position y-position) (query-pointer pointer)
    (values
      (or (not (eq window old-window))
	  ;; compare coordinates with eql, not =, because null values can be passed in
	  (not (eq old-x x-position))
	  (not (eq old-y y-position)))
      window x-position y-position)))

