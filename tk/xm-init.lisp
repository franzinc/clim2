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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $fiHeader: xm-init.lisp,v 1.16 1995/11/08 06:15:03 georgej Exp $

(in-package :tk)

(defvar *xm-done* nil)

(unless *xm-done*
  (xt-initialize)
  (define-toolkit-classes
      *intrinsic-classes*
      *motif-classes*)
  (setq *xm-done* t))


;; We only want to reinitialize if the toolkit was linked in shared.
;; Unfortunately there doesn't appear to be anyway of reliably
;; finding this out. Experience would indicate that the default is
;; to always link in the shared libraries except on rs6k (cim 3/14/96)

#+dlfcn
(defparameter *toolkit-static*
    #+rs6000 t
    #-rs6000 nil)

#+dlfcn
(defun reinitialize-toolkit ()
  (unless *toolkit-static*
    (xt_toolkit_initialize)
    (setup-error-handlers)
    (fixup-class-entry-points)))

#+dlfcn
(push 'reinitialize-toolkit excl::*restart-actions*)

