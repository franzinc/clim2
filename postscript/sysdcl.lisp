;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-


(in-package #-ansi-90 :user #+ansi-90 :cl-user)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defsystem postscript-clim-stubs
    ;; this exists so climg can be defined, and will autoload stuff as
    ;; needed.
    (:default-pathname "clim2:;postscript;")
  (:serial 
   "pkgdcl"
   #+allegro "postscript-s"))

(defsystem postscript-clim
    (:default-pathname "clim2:;postscript;")
  (:serial
;;;;; No reason to have this here, it just causes more damn warnings from
;;;;; defconstants being redefined:
   #+ignore clim-standalone
   postscript-clim-stubs
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
