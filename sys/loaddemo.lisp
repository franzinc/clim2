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
;; $Id: loaddemo.lisp,v 1.4.36.1 2002/02/08 19:11:24 layer Exp $

(in-package :user)

#+aclpc (load (climpath "demo\\test-suite.fsl"))
#+acl86win32 (load (climpath "demo\\test-suite.fasl"))

#+aclpc (load (climpath "demo\\sysdcl-pc.lisp"))
#+acl86win32 (load (climpath "demo\\sysdcl.lisp"))

#+aclpc (clim-defsystem:load-system "clim-demo")
#+acl86win32 (clim-defsystem:load-system 'clim-demo)
