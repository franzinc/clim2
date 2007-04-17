;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: recompile.lisp,v 2.7 2007/04/17 21:45:52 layer Exp $

;; this file is way out of date -- it needs to be recoded using
;; compile.lisp as a model.  -tjm 23May97

(in-package "USER")

;;; LOAD the file PATH.LSP first!

;;; to build a version that doesn't require fixnum coordinates,
;;; turn off the feature: USE-FIXNUM-COORDINATES in the 
;;; sys\sysdcl.lisp file.

;;;+++
(setq *climpath* "i:/customer/franz/clim22f/clim2/")
(defun climpath (sub) (merge-pathnames sub *climpath*))
;;;+++

;(compile-file (climpath "sys\\defsystem.lisp"))
#+aclpc
(load (climpath "sys\\defsyste.fsl"))
#+acl86win32
(load (climpath "sys\\defsystem.fasl"))

(load (climpath "sys\\sysdcl.lisp"))

(clim-defsystem:compile-system "clim-utils" :recompile t)
(clim-defsystem:load-system "clim-utils")

(clim-defsystem:compile-system "clim-silica" :recompile t)
(clim-defsystem:load-system "clim-silica")

(clim-defsystem:compile-system "clim-standalone" :recompile t)

(clim-defsystem:load-system "clim-standalone")

(load (climpath "aclpc\\sysdcl.lisp"))

;;; to compile aclpc-clim, uncomment the next line
#+aclpc
(clim-defsystem:compile-system "aclpc-clim" :recompile t)

#+acl86win32
(clim-defsystem:compile-system "aclnt-clim" :recompile t)

#+aclpc
(clim-defsystem:load-system "aclpc-clim")

#+acl86win32
(clim-defsystem:load-system "aclnt-clim")

;;; to make a demo-loaded version, uncomment the following
(compile-file (climpath "demo\\test-suite.lisp"))
#+aclpc
(load (climpath "demo\\test-sui.fsl"))
#+acl86win32
(load (climpath "demo\\test-suite.fasl"))

(load (climpath "demo\\sysdcl.lisp"))

(clim-defsystem:compile-system "clim-demo" :recompile t)
(clim-defsystem:load-system "clim-demo")


