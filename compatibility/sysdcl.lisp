;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/compatibility/sysdcl.lisp,v 1.6 1997/02/05 01:46:53 tomj Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :cl-user)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(clim-defsys:defsystem clim-compatibility
    (:default-pathname #+Genera "SYS:CLIM;REL-2;COMPATIBILITY;"
		       #-Genera (frob-pathname "compatibility")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;COMPATIBILITY;"
			      #-Genera (frob-pathname "compatibility")
     :load-before-compile (clim-standalone))
  ("packages")
  ("clim1-compatibility"))

#+Genera
(clim-defsys:import-into-sct 'clim-compatibility
  :pretty-name "CLIM Compatibility"
  :default-pathname "SYS:CLIM;REL-2;COMPATIBILITY;"
  :required-systems '(clim))
