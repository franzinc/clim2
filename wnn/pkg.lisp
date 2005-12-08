;; -*- mode: common-lisp; package: cl-user -*-
;;
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $Id: pkg.lisp,v 2.6 2005/12/08 21:25:47 layer Exp $

(defpackage :wnn
  (:use :clim-lisp :clim-utils :clim :silica :ff)
  (:import-from :excl #:if*)
  (:export
   #:jserver
   #:jserver-login
   #:jserver-host
   #:jserver-lang
   #:*jserver-timeout*
   #:open-jserver
   #:bunsetu-suu
   #:get-kanji
   #:get-yomi
   #:*wnn-unique*
   #:select-bunsetu
   #:bunsetu-kouho-suu
   #:get-kouho-kanji
   #:get-zenkouho-kanji
   #:henkan-begin
   #:henkan-end
   ))

(setf (package-definition-lock (find-package :wnn)) t)

(in-package :wnn)
