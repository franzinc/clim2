;;; -*- Mode: Lisp; Package: CL-USER -*-

(in-package :common-lisp-user)

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
  (setq excl::*enable-package-locked-errors* nil)
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

