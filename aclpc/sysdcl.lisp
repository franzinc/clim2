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

#+aclnt
(clim-defsys:defsystem aclnt-clim
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

#+Genera
(clim-defsys:import-into-sct 'aclpc-clim
  :pretty-name "aclpc CLIM"
  :default-pathname "SYS:CLIM;REL-2;aclpc;")
