;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: pkgdcl.lisp,v 1.2 92/02/24 13:24:03 cer Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :common-lisp-user)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."

(#-(or ANSI-90 Lucid) clim-lisp::defpackage #+(or ANSI-90 Lucid) defpackage cloe-clim
 (:use clim-lisp clim-sys clim clim-utils silica))
 
