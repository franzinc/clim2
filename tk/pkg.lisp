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
;; $fiHeader: pkg.lisp,v 1.9 92/05/13 17:10:24 cer Exp $

(defpackage :tk
  ;;-- No we really need
  ;; to use the x11 package?
  (:use :common-lisp :ff #+ignore :x11)
  (:nicknames :xt)
  (:import-from :excl #:if*)
  (:export
   #:initialize-motif-toolkit
   #:widget-parent
   #:manage-child
   #:get-values
   #:top-level-shell
   #:popup
   #:manage-child
   #:card32
   #:card29
   #:card24
   #:card16
   #:card8
   #:int32
   #:int16
   #:int8
   ))

