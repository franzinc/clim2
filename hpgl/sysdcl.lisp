;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/hpgl/sysdcl.lisp,v 1.4 1997/02/05 01:48:55 tomj Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :cl-user)

"Copyright (c) 1993 Franz Inc. All rights reserved"

(defsystem hpgl-clim
    (:default-pathname "clim2:;hpgl;")
  (:serial
   clim-standalone
   ("pkg")
   ("hpgl-port")
   ("hpgl-medium")))
