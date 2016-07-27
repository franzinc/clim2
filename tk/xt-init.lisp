;; See the file LICENSE for the full license governing this code.
;;
;; $Id: xt-init.lisp,v 2.7 2007/04/17 21:45:53 layer Exp $

(in-package :tk)

(defvar *xt-done* nil)

(defun xt-initialize ()
  (unless *xt-done*
    (xt_toolkit_initialize)
    (setq *xt-done* t)))

