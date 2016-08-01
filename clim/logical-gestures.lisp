;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;
;;

(in-package :clim-internals)

;;;"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; a logical-gesture-name is a symbol that is not eq to a defined keysym or a
;;; defined shift.

;;; Needed:  Efficient mechanism for comparing a key-press-event, a character,
;;; or a button-press-event with a gesture spec.
;;; This mechanism will be used by presentation translator lookup, as well as by the
;;; input editor.
;;;
;;; For presentation sensitivity, a way of finding out whether a particular set of shifts
;;; matches the shift part of a gesture spec.
;;;
;;; For debugging, ways of finding out loads more information.
;;;
;;; Implementation:
;;; To match a canonical spec with a shift mask, just compare with =.
;;; To match a canonical spec with a physical gesture, just compare shift mask with =
;;;   and keysym with EQL.
;;; To match a logical-gesture-name with either of the above, iterate over all defined gesture
;;;   specs for this logical name, doing the above.
;;;
;;; So, we need a table mapping logical gesture names to gesture specs.  The ordering of this
;;; table may be important.


(defvar *logical-gesture-name->gesture-spec-table* (make-hash-table))

(defun-inline logical-gesture-name-gesture-specs (logical-gesture-name)
  (gethash logical-gesture-name *logical-gesture-name->gesture-spec-table*))

(defun set-logical-gesture-name-gesture-specs (logical-gesture-name new-specs)
  (let ((canonical-new-specs
	  (with-collection
	    (dolist (spec new-specs)
	      (collect (parse-gesture-spec spec))))))
    (setf (gethash logical-gesture-name *logical-gesture-name->gesture-spec-table*)
	  (delete-duplicates canonical-new-specs :test #'equal))))

(defsetf logical-gesture-name-gesture-specs set-logical-gesture-name-gesture-specs)

;;; Normal activity for programmers, adding new gesture specs to an existing
;;; logical gesture name, as in
;;; (push '(:meta :middle) (logical-gesture-name-gesture-specs :edit-definition))
;;; (push '(:control :return) (logical-gesture-name-gesture-specs :make-new))
;;; (push ':make-new (logical-gesture-name-gesture-specs :edit-definition))

(defmacro map-over-logical-gesture-name-gesture-specs ((gesture-var logical-gesture-name)
						       &body body)
  (let ((continuation-name
	  (gensymbol "MAP-OVER-LOGICAL-GESTURE-NAME-GESTURE-SPECS-CONTINUATION")))
    `(flet ((,continuation-name (,gesture-var) ,@body))
       (declare (dynamic-extent #',continuation-name))
       (invoke-with-logical-gesture-name-gesture-specs
	 ,logical-gesture-name #',continuation-name ))))

(defun invoke-with-logical-gesture-name-gesture-specs
       (logical-gesture-name continuation &optional (depth 0))
  (declare (dynamic-extent continuation))
  ;; What's the right number?
  (when (> depth 3)
    (cerror "Continue recursing" "Too many recursive logical gesture translations?  ~
             (Current depth is ~D)" depth))
  (let ((gesture-specs (logical-gesture-name-gesture-specs logical-gesture-name)))
    (if gesture-specs
	(dolist (spec gesture-specs)
	  (if (atom spec)
	      (invoke-with-logical-gesture-name-gesture-specs spec continuation (incf depth))
	      (funcall continuation spec)))
	;; --- damn consing.
	(funcall continuation (cons logical-gesture-name 0)))))

(defun keysym-and-shift-mask-matches-gesture-spec (keysym shift-mask gesture-spec
							  &optional port)
  ;; Otherwise it's just too damed hard to use.
  (setq gesture-spec (parse-gesture-spec gesture-spec))
  ;; nice if this could be under control of a speed/safety compiler optimizer.
  (unless (integerp shift-mask) (setq shift-mask (apply #'make-shift-mask shift-mask)))
  ;; just in case, we'll mask out the high-level state part of this.
  ;; How much extra work is these two forms?
  (setq shift-mask (silica::state->shift-mask shift-mask))
  (let ((canonical-input nil))
    (macrolet ((match-it (spec)
		 `(and port
		       (let ((canonical-gesture-spec (get-port-canonical-gesture-spec ,spec port)))
			 (or (and (eql keysym (car canonical-gesture-spec))
				  (= shift-mask (cdr canonical-gesture-spec)))
			     ;; try to avoid this consing until the bottom...
			     (progn
			       (unless canonical-input
				 (setq canonical-input (get-port-canonical-gesture-spec
							 (cons keysym shift-mask) port)))
			       (equal canonical-gesture-spec canonical-input)))))))
      (cond ((listp gesture-spec)					;dotted-pair-p?
	     ;; if it's already a canonical gesture spec, just compare it.
	     (or
	       (and (eql keysym (car gesture-spec))
		    (= shift-mask (cdr gesture-spec)))
	       (match-it gesture-spec)))
	    (t (map-over-logical-gesture-name-gesture-specs (spec gesture-spec)
		 (when (or (and (eql keysym (car spec))
				(= shift-mask (cdr spec)))
			   (match-it spec))
		   (return-from keysym-and-shift-mask-matches-gesture-spec t)))
	       nil)))))

;;; This is only going to be called when we're interested in pointer gestures.
(defun shift-mask-matches-gesture-spec (shift-mask gesture-spec)
  ;; nice if this could be under control of a speed/safety compiler optimizer.
  (unless (integerp shift-mask) (setq shift-mask (apply #'make-shift-mask shift-mask)))
  ;; just in case, we'll mask out the high-level state part of this.
  ;; How much extra work is these two forms?
  (setq shift-mask (state->shift-mask shift-mask))
  (cond ((listp gesture-spec)
	 (= shift-mask (cdr gesture-spec)))
	(t (map-over-logical-gesture-name-gesture-specs (spec gesture-spec)
	     (when (= shift-mask (cdr spec))
	       (return-from shift-mask-matches-gesture-spec t)))
	   nil)))

(defun keysym-and-shift-mask-member (keysym shift-mask list &optional port)
  (dolist (spec list)
    (when (keysym-and-shift-mask-matches-gesture-spec keysym shift-mask spec port)
      (return t))))

(defun define-logical-gesture (gesture keysym shifts &key unique)
  (let ((spec (cons keysym shifts)))
    (if unique
	(setf (logical-gesture-name-gesture-specs gesture) (list spec))
	(push spec (logical-gesture-name-gesture-specs gesture)))))


(defun define-logical-gestures (list &key (unique t))
  (flet ((doit (gesture keysym &rest shifts)
	   (declare (dynamic-extent shifts))
	   (define-logical-gesture gesture keysym shifts :unique unique)))
    (declare (dynamic-extent #'doit))
    (dolist (gesture-spec list)
      (apply #'doit gesture-spec))))

;;; Define the default set of logical pointer gestures.

(define-logical-gestures
  '(
    ;; unshifted
    (:select :left )
    (:describe :middle )
    (:menu   :right )

    ;; :shift
    (:delete :middle :shift)

    ;; :meta
    (:edit-definition :left :meta)

    )
  )

