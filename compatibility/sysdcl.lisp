;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: sysdcl.lisp,v 2.4 2003/12/15 18:35:13 layer Exp $

(in-package :cl-user)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

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
