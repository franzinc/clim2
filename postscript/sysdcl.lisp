;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/postscript/sysdcl.lisp,v 1.12.24.1 1998/05/18 23:56:31 layer Exp $

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
