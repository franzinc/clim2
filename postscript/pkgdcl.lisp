;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: pkgdcl.lisp,v 1.6 1993/08/12 16:03:59 cer Exp $

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

  #+(and Allegro (or :rs6000 (not (version>= 4 1))))
  (:shadowing-import-from clim-utils
    with-slots))

#+Allegro
(setf (package-lock (find-package :postscript-clim)) t)
