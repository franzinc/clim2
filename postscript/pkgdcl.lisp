;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/postscript/pkgdcl.lisp,v 1.9 1997/02/05 01:49:34 tomj Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :common-lisp-user)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."

(#-ANSI-90 clim-lisp::defpackage #+ANSI-90 defpackage postscript-clim
  (:use clim-lisp clim-sys clim clim-utils clim-silica)

  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    dynamic-extent
    #-(or Allegro Lucid) non-dynamic-extent)

  #+(and Allegro (not (version>= 4 1)))
  (:shadowing-import-from clim-utils
    with-slots))

#+Allegro
(setf (package-lock (find-package :postscript-clim)) t)
