;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Id: sysdcl.lisp,v 1.5 1998/08/06 23:16:41 layer Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :cl-user)

"Copyright (c) 1993 Franz Inc. All rights reserved"

(defsystem hpgl-clim
    (:default-pathname "clim2:;hpgl;")
  (:serial
   clim-standalone
   ("pkg")
   ("hpgl-port")
   ("hpgl-medium")))
