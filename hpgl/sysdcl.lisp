;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Id: sysdcl.lisp,v 2.5 2004/01/16 19:15:43 layer Exp $

(in-package #-ansi-90 :user #+ansi-90 :cl-user)

;; copyright (c) 1993 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 1993-2004 Franz Inc, Oakland, CA - All rights reserved.

(defsystem hpgl-clim
    (:default-pathname "clim2:;hpgl;")
  (:serial
   clim-standalone
   ("pkg")
   ("hpgl-port")
   ("hpgl-medium")))
