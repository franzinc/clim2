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
;; $Id: sysdcl.lisp,v 1.2.22.4 1998/12/17 00:19:00 layer Exp $

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
   ("clim2:;utils;last")))
