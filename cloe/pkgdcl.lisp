;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Id: pkgdcl.lisp,v 1.6 1998/08/06 23:16:15 layer Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :common-lisp-user)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."

(#-ANSI-90 clim-lisp::defpackage #+ANSI-90 defpackage cloe-clim
  (:use clim-lisp clim-sys clim clim-utils clim-silica)

  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    dynamic-extent
    #-(or Allegro Lucid) non-dynamic-extent))
 
