;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: pkg.lisp,v 2.6 2005/12/08 21:25:47 layer Exp $

;;; --- the motif v openlook definitions should be in separate package
;;; definition files

(defpackage :tk-silica
  (:nicknames :xm-silica :xt-silica)
  (:use :clim-lisp :clim-utils :clim :silica :tk)
  (:import-from :excl #:if*)
  (:export
   *xt-font-families*
   *xt-logical-size-alist*
   *xt-cursor-type-alist*
   ))

(setf (package-definition-lock (find-package :tk-silica)) t)

(in-package :tk-silica)
