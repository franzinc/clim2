;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

(in-package #-ANSI-90 :user #+ANSI-90 :cl-user)

#+aclpc
(clim-defsys:defsystem aclpc-clim
    (:default-pathname #+Genera "SYS:CLIM;REL-2;ACLPC;"
		       #-Genera (frob-pathname "aclpc")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;aclpc;"
			      #-Genera (frob-pathname "aclpc")
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("winwidgh")
  ("climpat")
  ("acl-prel")
  ("acl-class")
  ("acl-dc")
  ("acl-port")
  ("acl-mirror")
  ("acl-medium")
  ("acl-pixmaps")
  ("acl-frames")
  ("acl-widget")
  ("acl-scroll")
)

#+acl86win32
(defsystem aclnt-clim
    (:default-pathname "clim2:;aclpc;")
  (:serial
   clim-standalone
   "pkgdcl"
   "winwidgh"
   "climpat"
   "acl-prel"
   "acl-class"
   "acl-dc"
   "acl-port"
   "acl-mirror"
   "acl-medium"
   "acl-pixmaps"
   "acl-frames"
   "acl-widget"
   "acl-scroll"
   ("clim2:;utils;last")))

#+Genera
(clim-defsys:import-into-sct 'aclpc-clim
  :pretty-name "aclpc CLIM"
  :default-pathname "SYS:CLIM;REL-2;aclpc;")
