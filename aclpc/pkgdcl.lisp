;;; -*- Mode: Lisp; Package: CL-USER -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: pkgdcl.lisp,v 1.3 1998/08/06 23:15:46 layer Exp $

(in-package :common-lisp-user)

(defvar *lock-preference* excl:*enable-package-locked-errors*)

;; Invite everybody to the party.
(eval-when (compile load eval)
  (require :climg)
  (require :ffcompat)
  (require :cltl1)			; for INT-CHAR and a few others
  (require :aclwffi)			; for ctypes (CT) package
  ;;(require :for)			; for FOR macro
  (require :winapi)
  (require :winapi-dev)
  (load "user32.dll")

  ;; Turn this off as long as clim-utils is a locked package.
  (setq excl:*enable-package-locked-errors* nil)
  )

(defpackage acl-clim
  (:use clim-lisp clim-sys clim clim-utils clim-silica)

  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    dynamic-extent
    non-dynamic-extent)
  (:export *generic-gadgets*))
 
(eval-when (compile load eval)
  (pushnew :aclmerge *features*)
  (pushnew :OS-THREADS *features*))

