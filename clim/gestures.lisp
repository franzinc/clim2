;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: gestures.lisp,v 1.4 92/03/04 16:21:43 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;; The implementation layer constructs a modifier key mask to go along with each
;;; pointer gesture, e.g., Shift-Left.

;;; For now, we are making a few assumptions.
;;; 1) The set of possible modifier bits is :CONTROL, :SHIFT, :META, :SUPER, and :HYPER,
;;;    as per CLtL.
;;; 2) The set of possible "mouse buttons" is :LEFT, :MIDDLE, and :RIGHT.

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
  (assert (every #'(lambda (x) (find x *modifier-keys*)) modifiers) (modifiers)
	  "~S is not a subset of ~S" modifiers '(:shift :control :meta :super :hyper))
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

;; A table indexed by the modifier state.
;; Each bucket in the table contains an alist of a keysym and 1 or more gesture names.
(defvar *keysym-and-modifier-key->gesture*
	(make-array (ash 1 (length *modifier-keys*)) :initial-element nil))

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
      (return-from gesture-name-button-and-modifiers
	(values button modifier-state))))
  nil)

(defun gesture-name-keysym-and-modifiers (gesture-name)
  (declare (values keysym modifier-state))
  (dotimes (index (ash 1 (length *modifier-keys*)))
    (let ((bucket (aref *keysym-and-modifier-key->gesture* index)))
      (dolist (entry bucket)
	(when (member gesture-name (cdr entry))
	  (return-from gesture-name-keysym-and-modifiers
	    (values (car entry) index))))))
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

;;--- Shouldn't #\A match (:A :SHIFT), and so forth?
(defun keyboard-event-matches-gesture-name-p (event gesture-name)
  (when (and (characterp event)
	     (characterp gesture-name)
	     (eql event gesture-name))
    (return-from keyboard-event-matches-gesture-name-p t))
  (multiple-value-bind (keysym modifier-state character)
      (etypecase event
	(character
	  (values nil 0 event))
	(key-press-event
	  (values (keyboard-event-key-name event)
		  (event-modifier-state event)
		  (keyboard-event-character event))))
    (declare (fixnum modifier-state))
    (let ((bucket (aref *keysym-and-modifier-key->gesture* modifier-state)))
      ;;--- Presently allows both keysyms and characters.  Is that right?
      (or (member gesture-name (cdr (assoc keysym bucket)))
	  (member gesture-name (cdr (assoc character bucket)))))))

(defun-inline keyboard-event-p (x)
  (or (characterp x)
      (typep x 'key-press-event)))

(defun keyboard-gesture-spec-p (x)
  (let ((keysym (if (consp x) (first x) x))
	(modifiers (and (consp x) (rest x))))
    (and (or (characterp keysym)
	     (symbolp keysym))
	 (every #'(lambda (x) (find x *modifier-keys*)) modifiers))))

;;--- Shouldn't #\A match (:A :SHIFT), and so forth?
(defun gesture-eql (gesture1 gesture2)
  (let ((g1-keysym (if (consp gesture1) (first gesture1) gesture1))
	(g1-modifiers (and (consp gesture1) (rest gesture1)))
	(g2-keysym (if (consp gesture2) (first gesture2) gesture2))
	(g2-modifiers (and (consp gesture2) (rest gesture2))))
    (and (or (eql g1-keysym g2-keysym)
	     (and (characterp g1-keysym)
		  (characterp g2-keysym)
		  (char-equal g1-keysym g2-keysym)))
	 (every #'(lambda (x) (member x g2-modifiers)) g1-modifiers)
	 (every #'(lambda (x) (member x g1-modifiers)) g2-modifiers))))


(defun add-gesture-name (name type gesture-spec &key (unique t))
  (check-type name symbol)
  (when unique
    (delete-gesture-name name))
  (ecase type
    (:keyboard
      (destructuring-bind (key-name &rest modifiers) gesture-spec
	(check-type key-name (or symbol character))
	(assert (every #'(lambda (x) (find x *modifier-keys*)) modifiers) (modifiers)
		"~S is not a subset of ~S" modifiers '(:shift :control :meta :super :hyper))
	(let* ((index (apply #'make-modifier-state modifiers))
	       (bucket (aref *keysym-and-modifier-key->gesture* index))
	       (entry (assoc key-name bucket)))
	  (cond (entry
		 (setf (aref *keysym-and-modifier-key->gesture* index)
		       (nsubstitute (append entry (list name)) entry bucket)))
		(t
		 (setq entry (list key-name name))
		 (push entry (aref *keysym-and-modifier-key->gesture* index))))
	  bucket)))
    (:pointer-button
      (destructuring-bind (button &rest modifiers) gesture-spec
	(check-type button (member :left :middle :right))
	(assert (every #'(lambda (x) (find x *modifier-keys*)) modifiers) (modifiers)
		"~S is not a subset of ~S" modifiers '(:shift :control :meta :super :hyper))
	(pushnew name (button-and-modifier-state-gesture-names
			(button-index button)
			(apply #'make-modifier-state modifiers)))))))

(defun delete-gesture-name (name)
  (do-button-and-modifier-state (button modifier-state bucket)
    (when (member name bucket)
      (setf (aref *button-and-modifier-key->gesture* button modifier-state)
	    (delete name bucket))))
  (dotimes (index (ash 1 (length *modifier-keys*)))
    (let ((bucket (aref *keysym-and-modifier-key->gesture* index)))
      (do* ((entryl bucket (cdr entryl))
	    (entry (first entryl) (first entryl)))
	   ((null entryl))
	(when (member name (cdr entry))
	  (setf (first entryl) (delete name entry)))))))

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

