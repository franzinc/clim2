;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: sysdcl.lisp,v 1.8 93/03/31 10:39:24 cer Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :cl-user)

"Copyright (c) 1993 Franz Inc. All rights reserved"

(clim-defsys:defsystem hpgl-clim
    (:default-pathname (frob-pathname "hpgl")
     :default-binary-pathname (frob-pathname "hpgl")
     :load-before-compile (clim-standalone))
  ("pkg")
  ("hpgl-port")
  ("hpgl-medium"))
