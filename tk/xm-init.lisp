
(in-package :tk)

(defvar *done* nil)

(unless *done*
  (toolkit-initialize)
  (define-toolkit-classes *motif-classes*)
  (setq *done* t))



