;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-


(in-package #-ANSI-90 :user #+ANSI-90 :cl-user)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(clim-defsys:defsystem genera-clim
    (:default-pathname #+Genera "SYS:CLIM;REL-2;GENERA;"
		       #-Genera (frob-pathname "genera")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;GENERA;"
			      #-Genera (frob-pathname "genera")
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("genera-port")
  ("genera-mirror")
  ("genera-medium")
  ("genera-pixmaps")
  ("genera-frames")
  ("genera-activities")
  ("genera-prefill"))

#+Genera
(clim-defsys:import-into-sct 'genera-clim
  :pretty-name "Genera CLIM"
  :default-pathname "SYS:CLIM;REL-2;GENERA;"
  :required-systems '(clim)
  :bug-reports "Bug-CLIM"
  :patches-reviewed "Bug-CLIM-Doc")
