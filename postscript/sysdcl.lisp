;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Id: sysdcl.lisp,v 1.14 1998/08/06 23:16:48 layer Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :cl-user)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defsystem postscript-clim
    (:default-pathname "clim2:;postscript;")
  (:serial
;;;;; No reason to have this here, it just causes more damn warnings from
;;;;; defconstants being redefined:
   #+ignore clim-standalone
   ("pkgdcl")
   #+(and Allegro (not acl86win32)) ("postscript-s") ;; why not?
   ("postscript-port")
   ("postscript-medium")
   ("read-afm")
   ("laserwriter-metrics")))

#+Genera
(clim-defsys:import-into-sct 'postscript-clim
  :pretty-name "PostScript CLIM"
  :default-pathname "SYS:CLIM;REL-2;POSTSCRIPT;"
  :required-systems '(clim)
  :bug-reports "Bug-CLIM"
  :patches-reviewed "Bug-CLIM-Doc")
