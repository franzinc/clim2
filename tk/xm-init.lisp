;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[Tue Mar 24 18:48:58 1998 by duane]-
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
;; $Header: /repo/cvs.copy/clim2/tk/xm-init.lisp,v 1.20 1998/03/25 03:52:27 duane Exp $

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


#+(version>= 5 0)
(defparameter *toolkit-static*
    #+rs6000 t
    #-rs6000 nil)


#+(and (not (version>= 5 0)) dlfcn)
(defparameter *toolkit-static*
    #+rs6000 t
    #-rs6000 nil)

#+(version>= 5 0)
(defun reinitialize-toolkit ()
  (unless *toolkit-static*

    (setq *callback-handler-address*
      (register-foreign-callable 'callback-handler :reuse))
    (setq *match-event-sequence-and-types-address*
      (register-foreign-callable 'match-event-sequence-and-types :reuse))
    (setq *event-handler-address*
      (register-foreign-callable 'event-handler :reuse))
    (setq *error-handler-function-address*
      (register-foreign-callable 'toolkit-error-handler :reuse))
    (setq *warning-handler-function-address*
      (register-foreign-callable 'toolkit-warning-handler :reuse))
    (setq *x-error-handler-address*
      (register-foreign-callable 'x-error-handler :reuse))
    (setq *x-io-error-handler-address*
      (register-foreign-callable 'x-io-error-handler :reuse))
    (reinitialize-silica-callbacks)

    (xt_toolkit_initialize)
    (setup-error-handlers)
    (fixup-class-entry-points)))

#+(and (not (version>= 5 0)) dlfcn)
(defun reinitialize-toolkit ()
  (unless *toolkit-static*
    (xt_toolkit_initialize)
    (setup-error-handlers)
    (fixup-class-entry-points)))

#+(version>= 5 0)
(push 'reinitialize-toolkit excl::*restart-actions*)

#+(and (not (version>= 5 0)) dlfcn)
(push 'reinitialize-toolkit excl::*restart-actions*)

