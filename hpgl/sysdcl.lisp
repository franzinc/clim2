;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Id: sysdcl.lisp,v 1.6 2000/05/01 21:43:29 layer Exp $

(in-package #-ansi-90 :user #+ansi-90 :cl-user)

"Copyright (c) 1993 Franz Inc. All rights reserved"

(defsystem hpgl-clim
    (:default-pathname "clim2:;hpgl;")
  (:serial
   clim-standalone
   ("pkg")
   ("hpgl-port")
   ("hpgl-medium")))
