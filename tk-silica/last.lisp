;; -*- mode: common-lisp; package: system -*-
;;
;;				-[Thu Apr 15 18:06:28 1993 by layer]-
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: last.lisp,v 1.6 1994/12/05 00:01:46 colin Exp $


(in-package :system)

(defvar system::*devel* nil)

(load-application
 (load-patches
  "patch"
  (namestring
   (merge-pathnames ";update-clim2;"
		    excl::*library-pathname*)))
 :devel (locally (declare (special system::*devel*)) system::*devel*))

(provide
 (cond ((excl::featurep :clim-motif) :climxm)
       ((excl::featurep :clim-openlook) :climol)
       (t (error "Unknown Xt backend"))))
