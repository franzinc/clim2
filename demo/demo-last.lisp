;; -*- mode: common-lisp; package: user -*-
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
;; $Id: demo-last.lisp,v 1.6.36.1 2002/02/08 19:11:22 layer Exp $

(in-package :user)

(provide :climdemo)

(cond ((excl::featurep :clim-motif)
       (provide :climdemoxm)
       (load "clim2demoxm-preload.fasl" :if-does-not-exist nil))
      ((excl::featurep :clim-openlook)
       (provide :climdemool)
       (load "clim2demool-preload.fasl" :if-does-not-exist nil)))
