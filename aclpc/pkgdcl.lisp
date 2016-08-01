;;; -*- Mode: Lisp; Package: CL-USER -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :common-lisp-user)

(defvar *lock-preference* excl:*enable-package-locked-errors*)

(defvar *ffi-arg-checking*
    #+(and mswindows 64bit) t
    #-(and mswindows 64bit) nil)

(defvar *ffi-call-direct*
    #+(and mswindows 64bit) nil
    #-(and mswindows 64bit) t)

;; Invite everybody to the party.
(eval-when (compile load eval)
  (require :climg)
  ;; [rfe4951]: not needed anymore:
  ;; (require :ffcompat)
  (require :aclwffi)			; for ctypes (CT) package
  ;;(require :for)			; for FOR macro
  (require :winapi)
  (require :winapi-dev)
  ;; Kevin: (FEATUREP (:VERSION>= 5 (0 1) :PRE-BETA2)) throws an error
  (or (ignore-errors (load "user32.dll" :system-library t))
      (load "user32.dll"))
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
  (:export #:*generic-gadgets*)
  ;; These exports are things that might want to be semi-documented.
  ;; No packages use acl-clim, but it's pretty bad to tell users about
  ;; acl-clim::foo-bar.  Really this stuff should probably be in a
  ;; different package.
  (:export
   #:make-windows-font-named		;make a windows font from a name
   ))
 
(eval-when (compile load eval)
  (pushnew :aclmerge *features*)
  (pushnew :os-threads *features*))

