;; -*- mode: common-lisp; package: user -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: go-xm.lisp,v 1.1 92/01/30 10:16:06 cer Exp Locker: cer $

#+ignore
(let ((which :motif))
  (declare (special which))
  (load (merge-pathnames "go.cl" excl::*source-pathname*)))

(compile-file-if-needed "sys/defsystem")
(let ((*enable-package-locked-errors* nil))
  (load "sys/defsystem"))
(load "sys/sysdcl")
(defsys::compile-system 'motif-clim :propagate t)
(tenuring (defsys::load-system 'motif-clim))





