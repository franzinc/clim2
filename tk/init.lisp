
(in-package :tk)

(defvar *done* nil)
(unless *done*
  (insert_classes)
  (toolkit-initialize)
  (make-classes)
  (setq *done* t))
