;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: pkgdcl.lisp,v 1.4 92/09/24 09:38:17 cer Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :common-lisp-user)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."

(#-(or ANSI-90 Lucid) clim-lisp::defpackage #+(or ANSI-90 Lucid) defpackage clx-clim
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
 
