;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[Fri Nov 19 00:56:06 1993 by duane]-
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: xm-init.lisp,v 1.8 92/05/07 13:11:02 cer Exp $

(in-package :tk)

(defvar *xm-done* nil)

(unless *xm-done*
  (xt-initialize)
  (define-toolkit-classes 
      *intrinsic-classes*
      *motif-classes*)
  (setq *xm-done* t))


#+svr4
(unless (ff::symbol-in-main-symbol-table-p "xt_toolkit_initialize")
  (defun reinitialize-toolkit ()
    (xt_toolkit_initialize)
    (setup-error-handlers)
    (fixup-class-entry-points))
  (push '(:eval reinitialize-toolkit) excl::*restart-actions*))
