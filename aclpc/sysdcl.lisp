;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: sysdcl.lisp,v 1.4 1998/08/06 23:15:46 layer Exp $

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
