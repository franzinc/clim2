;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Id: sysdcl.lisp,v 2.4 2003/12/15 18:35:13 layer Exp $

(in-package #-ansi-90 :user #+ansi-90 :cl-user)

"Copyright (c) 1993 Franz Inc. All rights reserved"

(defsystem hpgl-clim
    (:default-pathname "clim2:;hpgl;")
  (:serial
   clim-standalone
   ("pkg")
   ("hpgl-port")
   ("hpgl-medium")))
