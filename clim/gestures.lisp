(in-package :clim)
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

;;; Implementation of gestures

(defun button-and-shifts-matches-gesture-name (button shift gesture)
  (some #'(lambda (spec)
	    (destructuring-bind (btn . modifiers) spec
	      (and (equal btn button)
		   (= shift modifiers))))
	(logical-gesture-name-gesture-specs gesture)))

(defun shift-mask-matches-gesture-name (shift gesture)
  (some #'(lambda (spec)
	    (destructuring-bind (button . modifiers) spec
	      (= shift modifiers)))
	(logical-gesture-name-gesture-specs gesture)))

(defun gesture-name-button-and-shifts (gesture)
  (logical-gesture-name-gesture-specs gesture))

(defvar *logical-gesture-name->gesture-spec-table* (make-hash-table))

(defun logical-gesture-name-gesture-specs (gesture)
  (gethash gesture *logical-gesture-name->gesture-spec-table*))

(defun (setf logical-gesture-name-gesture-specs) (value gesture)
  (setf (gethash gesture *logical-gesture-name->gesture-spec-table*) value))

(defun add-pointer-gesture-name (gesture button modifiers &key action
							       unique)
  (declare (ignore action))
  (setq button (silica::button-name->mask button))
  (setq modifiers (silica::modifiers->mask modifiers))
  (let ((spec (cons button modifiers)))
    (if unique
	(setf (logical-gesture-name-gesture-specs gesture) (list spec))
      (push spec (logical-gesture-name-gesture-specs gesture)))))

(defmacro define-gesture-name (gesture-name &key button modifiers action unique)
  `(add-pointer-gesture-name
    ',gesture-name
    ',button
    ',modifiers
    :action ',action
    :unique ',unique))

(define-gesture-name :select :button :left)
(define-gesture-name :describe :button :middle)
(define-gesture-name :menu :button :right)
 
    
