;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: sysdcl.lisp,v 1.1 92/08/19 10:29:27 cer Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :cl-user)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(clim-defsys:defsystem cloe-clim
    (:default-pathname #+Genera "SYS:CLIM;REL-2;CLOE;"
		       #-Genera (frob-pathname "cloe")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;CLOE;"
			      #-Genera (frob-pathname "cloe")
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("wheader")
  ("windows")
  ("cloe-port")
  ("cloe-mirror")
  ("cloe-medium")
  ("cloe-frames")
  ("cloe-gadgets")
  ("cloe-menus"))

#+Genera
(clim-defsys:import-into-sct 'cloe-clim
  :pretty-name "Cloe CLIM"
  :default-pathname "SYS:CLIM;REL-2;CLOE;")
