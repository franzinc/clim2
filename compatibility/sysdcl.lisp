;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: sysdcl.lisp,v 1.2 92/09/08 15:19:36 cer Exp $

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
