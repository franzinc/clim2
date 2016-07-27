;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;
;; $Id: sysdcl.lisp,v 2.7 2007/04/17 21:45:49 layer Exp $

(in-package :cl-user)

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
   last))
