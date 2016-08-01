;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

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
