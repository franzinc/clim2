;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[]-
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $fiHeader: make-widget.lisp,v 1.9 1993/09/17 19:06:44 cer Exp $

(in-package :tk)

(defgeneric make-widget (widget name parent &key)
  ;;; returns the handle of a widget
  )

(defmethod make-widget ((w core) name parent &rest args &key (managed t)
			&allow-other-keys)
  (remf args :managed)
  (let ((class (class-of w)))
    (if managed
	(apply #'create-managed-widget name class parent args)
      (apply #'create-widget name class parent args))))

(defmethod make-widget ((w shell) name parent &rest args)
  (declare (ignore args parent name))
  (error "shells not made this way"))

(defmethod make-widget ((w top-level-shell) name parent &rest args)
  (apply #'create-popup-shell name (class-of w) parent args))


;; Where should this go??

;(defmacro define-widget-accessor (name class resource)
;  `(progn
;     (define-widget-reader ,name ,class ,resource)
;     (define-widget-writer ,name ,class ,resource)))
;
;(defmacro define-widget-writer (name class resource)
;  `(defmethod (setf ,name) (nv (widget ,class))
;     (set-values widget ,resource nv)))

(defmacro define-widget-reader (name class resource)
  `(defmethod ,name ((widget ,class))
     (get-values widget ,resource)))

(define-widget-reader widget-children composite :children)
(define-widget-reader widget-num-children composite :num-children)
