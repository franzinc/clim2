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
;; $Id: last.lisp,v 1.5 2002/07/09 20:57:19 layer Exp $

;;; All this is allegro-sepcific.

(in-package :system)

;;; This is, perhaps, a temporary hack to get the EUC stuff loaded at
;;; a non-bad time.
;;
;; This is no longer needed, since we're using native-to-* interface now
;; The euc module may be autoloaded due to process-code usage, but the
;; autoload no longer produces a mesage at inopportune times.
#+ignore
(eval-when (:load-toplevel :execute)
  (require :euc)
  (find-external-format :euc))

#-(version>= 5 0 pre-final 16)
(load-patches "patch" "sys:;update-clim;*.fasl")
#+(and (version>= 5 0 pre-final 16)
       (not clim-dont-load-patches))
(load-patches :product #.*patch-product-code-clim*
	      :version #.excl::*cl-patch-version-char*)

(provide
 #+mswindows :climnt
 #-mswindows
 (cond ((excl::featurep :clim-motif) :climxm)
       ((excl::featurep :clim-openlook) :climol)
       (t (error "Unknown Xt backend"))))
