;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

(in-package #-ANSI-90 :user #+ANSI-90 :common-lisp-user)

(#-ANSI-90 clim-lisp::defpackage #+ANSI-90 defpackage acl-clim
  (:use clim-lisp clim-sys clim clim-utils clim-silica)

  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    dynamic-extent
    non-dynamic-extent)
  (:export
    *generic-gadgets*)   
	   )
 
(eval-when (compile load eval)
  (pushnew :aclmerge *features*)
  (pushnew :OS-THREADS *features*))

(eval-when (compile load eval)
  (require :ffcompat)
  (require :cltl1)			; for INT-CHAR and a few others
  (require :aclwffi)			; for ctypes (CT) package
  (require :for)			; for FOR macro
  (require :winapi)
  (require :winapi-dev)
  (load "user32.dll")  
  (setq excl::*enable-package-locked-errors* nil)
  )