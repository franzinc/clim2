;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

(in-package #-ANSI-90 :user #+ANSI-90 :common-lisp-user)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader: pkgdcl.lisp,v 1.1 92/02/24 13:21:59 cer Exp Locker: cer $

(#-ANSI-90 clim-lisp::defpackage #+ANSI-90 defpackage clx-clim
 (:use clim-lisp clim-sys clim clim-utils silica))
 
