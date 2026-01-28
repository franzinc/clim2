;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :cl-user)

(defsystem aclnt-clim
    (:default-pathname "clim2:;aclpc;")
  (:serial
   clim-main
   clim-standalone
   "pkgdcl"
   "acl-port-vars"
   "winwidgh-vars"
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
   last))
