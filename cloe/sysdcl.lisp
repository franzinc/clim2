;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/cloe/sysdcl.lisp,v 1.6 1997/02/05 01:46:11 tomj Exp $

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
  ("cloe-dc")
  ("cloe-port")
  ("cloe-mirror")
  ("cloe-medium")
  ("cloe-pixmaps")
  ("cloe-frames")
  ("cloe-gadgets")
  ("cloe-menus")
  ("cloe-activities")
  ("cloe-streams"))

#+Genera
(clim-defsys:import-into-sct 'cloe-clim
  :pretty-name "Cloe CLIM"
  :default-pathname "SYS:CLIM;REL-2;CLOE;")
