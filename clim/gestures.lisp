;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: gestures.lisp,v 1.3 92/02/24 13:07:39 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;; The implementation layer constructs a modifier key mask to go along with each
;;; pointer gesture, e.g., Shift-Left.

;;; For now, we are making a few assumptions.
;;; 1) The set of possible modifier bits is :CONTROL, :SHIFT, :META, :SUPER, and :HYPER,
;;;    as per CLtL.
;;; 2) The set of possible "mouse buttons" is :LEFT, :MIDDLE, and :RIGHT.

(defconstant *pointer-buttons* '#(:left :middle :right))
(defconstant *modifier-keys*   '#(:shift :control :meta :super :hyper))

(defun-inline button-index (name)
  #+Genera (declare lt:(side-effects simple reducible))
  (position name *pointer-buttons*))
(defun-inline button-index-name (index)
  (aref *pointer-buttons* index))

(defun-inline modifier-key-index (name)
  #+Genera (declare lt:(side-effects simple reducible))
  (position name *modifier-keys*))
(defun-inline modifier-key-index-name (index)
  (aref *modifier-keys* index))

;; Modifier states can be compared with =
(defun make-modifier-state (&rest modifiers)
  (declare (dynamic-extent modifiers))
  (let ((state 0))
    (dolist (name modifiers)
      (let ((bit (modifier-key-index name)))
	(setf state (dpb 1 (byte 1 bit) state))))
    state))

#+ignore
(defun print-modifier-state (state stream &optional brief-p)
  (let ((n (length *modifier-keys*)))
    (dotimes (i n)
      (let ((bit (- n i 1)))
        (let ((name (modifier-key-index-name bit)))
          (when (ldb-test (byte 1 bit) state)
            (if brief-p
		(format stream "~A-"
		  (ecase name
		    (:shift "sh")
		    (:control "c")
		    (:meta "m")
		    (:super "s")
		    (:hyper "h")))
              (format stream "~:(~A-~)" name))))))))

;; A table indexed by mouse button and modifier state [now (3 x 32)].
;; Each bucket in the table contains a sequence of gesture names.
(defvar *button-and-modifier-key->gesture*
	(make-array `(,(length *pointer-buttons*)
		      ,(ash 1 (length *modifier-keys*)))
		    :initial-element nil))

;; BUTTON is a button number (0, 1, or 2), and MODIFIER-STATE is a mask
(defun-inline button-and-modifier-state-gesture-names (button modifier-state)
  (declare (fixnum button modifier-state))
  (aref *button-and-modifier-key->gesture* button modifier-state))

;; BUTTON is a button number (0, 1, or 2), and MODIFIER-STATE is a mask
(defun-inline set-button-and-modifier-state-gesture-names (button modifier-state new-gestures)
  (setf (aref *button-and-modifier-key->gesture* button modifier-state)
	new-gestures))

(defsetf button-and-modifier-state-gesture-names set-button-and-modifier-state-gesture-names)

(defmacro do-button-and-modifier-state ((button-var modifier-var bucket-var) &body body)
  (let ((n-buttons '#:n-buttons)
	(n-masks '#:n-masks))
    `(let ((,n-buttons (array-dimension *button-and-modifier-key->gesture* 0))
	   (,n-masks (array-dimension *button-and-modifier-key->gesture* 1)))
       (dotimes (,button-var ,n-buttons)
	 (dotimes (,modifier-var ,n-masks)
	   (let ((,bucket-var
		  (aref *button-and-modifier-key->gesture* ,button-var ,modifier-var)))
	     ,@body))))))

(defun gesture-name-button-and-modifiers (gesture-name)
  (declare (values button modifier-state))
  (do-button-and-modifier-state (button modifier-state bucket)
    (when (member gesture-name bucket)
      (return-from gesture-name-button-and-modifiers (values button modifier-state))))
  nil)

#||
(defun modifier-state-keys (state)
  ;; This could be table-driven, also, if it's called frequently
  (let ((shifts nil))
    (dotimes (bit (length *modifier-keys*))
      (let ((name (modifier-key-index-name bit)))
	(when (ldb-test (byte 1 bit) state)
	  (push name shifts))))
    (nreverse shifts)))

;;; Returns an alist of (button . gestures)
(defun gestures-for-modifier-state (state)
  ;; Eventually, cache this result because it's a common question
  (let ((n-buttons (array-dimension *button-and-modifier-key->gesture* 0))
	(return-value nil))
    (dotimes (b n-buttons)
      (let ((bucket (aref *button-and-modifier-key->gesture* b state)))
	(when bucket
	  (push (cons (button-index-name b) bucket) return-value))))
    (nreverse return-value)))

(defun gestures-for-shift-names (&rest shift-names)
  (declare (dynamic-extent shift-names))
  (let ((state (apply #'make-modifier-state shift-name)))
    (gestures-for-modifier-state state)))

||#

;; We typically have our hands on a button index and a modifier key state,
;; and we need to know if it matches a named gesture
;; BUTTON is a button number (0, 1, or 2), and STATE is a mask.
(defun-inline button-and-modifier-state-matches-gesture-name-p (button state gesture-name)
  (or (eq gesture-name 't)
      (member gesture-name (button-and-modifier-state-gesture-names button state))))

(defun modifier-state-matches-gesture-name-p (state gesture-name)
  ;; This could obviously be cached as well.
  (dotimes (i (length *pointer-buttons*))
    (when (button-and-modifier-state-matches-gesture-name-p i state gesture-name)
      (return-from modifier-state-matches-gesture-name-p T))))

(defun event-matches-gesture-name-p (event gesture-name)
  (etypecase event
    (pointer-button-event			;--- POINTER-BUTTON-PRESS-EVENT?
      (button-press-event-matches-gesture-name-p event gesture-name))
    (keyboard-event				;--- KEY-PRESS-EVENT?
      (keyboard-event-matches-gesture-name-p event gesture-name))))

(defun button-press-event-matches-gesture-name-p (event gesture-name)
  (let ((button (pointer-event-button event))
	(modifier-state (event-modifier-state event)))
    (declare (fixnum button modifier-state))
    (button-and-modifier-state-matches-gesture-name-p
      (- (integer-length button) #.(integer-length +pointer-left-button+))
      modifier-state gesture-name)))

(defun keyboard-event-matches-gesture-name-p (event gesture-name)
  ;;--- Fill this in
  )


(defun add-gesture-name (name type gesture-spec &key (unique t))
  (check-type name symbol)
  (when unique
    (delete-gesture-name name))
  (ecase type
    (:keyboard
      (destructuring-bind (key-name &rest modifiers) gesture-spec
	(check-type key-name (or character
				 (member :newline :linefeed :tab :backspace :page :rubout)))
	(assert (every #'(lambda (x) (member x '(:shift :control :meta :super :hyper)))
		       modifiers) (modifiers)
		"~S is not a subset of ~S" modifiers '(:shift :control :meta :super :hyper))
	;;--- Fill this in
	))
    (:pointer-button
      (destructuring-bind (button &rest modifiers) gesture-spec
	(check-type button (member :left :middle :right))
	(assert (every #'(lambda (x) (member x '(:shift :control :meta :super :hyper)))
		       modifiers) (modifiers)
		"~S is not a subset of ~S" modifiers '(:shift :control :meta :super :hyper))
	(pushnew name (button-and-modifier-state-gesture-names
			(button-index button)
			(apply #'make-modifier-state modifiers)))))))

(defun delete-gesture-name (name)
  (do-button-and-modifier-state (button modifier-state bucket)
    (when (member name bucket)
      (setf (aref *button-and-modifier-key->gesture* button modifier-state)
	    (delete name bucket)))))

#+CLIM-1-compatibility
(define-compatibility-function (add-pointer-gesture-name add-gesture-name)
			       (name button modifiers &key (action :click) (unique t))
  (add-gesture-name name :pointer-button `(,button ,modifiers) :unique unique))

#+CLIM-1-compatibility
(define-compatibility-function (remove-pointer-gesture-name delete-gesture-name)
			       (name)
  (delete-gesture-name name))

(defmacro define-gesture-name (name type gesture-spec)
  (setf (compile-time-property name 'gesture-name) t)
  `(add-gesture-name ',name ',type ',gesture-spec :unique t))

;; Define the default set of logical pointer gestures.
(define-gesture-name :select   :pointer-button (:left))
(define-gesture-name :describe :pointer-button (:middle))
(define-gesture-name :menu     :pointer-button (:right))
(define-gesture-name :delete   :pointer-button (:middle :shift))
(define-gesture-name :edit     :pointer-button (:left :meta))

