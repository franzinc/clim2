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
;; $Id: loadnd.lisp,v 1.2.22.1 1998/07/06 23:10:02 layer Exp $

(in-package :user)

#+aclpc (load (climpath "sys\\defsyste.fsl"))
#+acl86win32 (load (climpath "sys\\defsystem.fasl"))

#+aclpc (load (climpath "sys\\sysdcl-pc.lisp"))
#+acl86win32 (load (climpath "sys\\sysdcl.lisp"))

(load (climpath "aclpc\\sysdcl.lisp"))

;#+aclpc (clim-defsystem:load-system "clim-standalone")
;#+acl86win32 (load-system 'clim-standalone)
; uncomment above and comment below to make
; a clim standalone
#+aclpc (clim-defsystem:load-system "aclpc-clim")
#+allegro (load-system 'aclnt-clim)
