;; -*- mode: common-lisp; package: cl-user -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: sysdcl.cl,v 1.3 92/01/17 17:49:29 cer Exp $

(in-package :cl-user)

(defsys::defsystem :xm-silica
    (:default-pathname (frob-pathname "xm-silica"))
  ("pkg")
  ("xt-silica")
  ("xm-silica")
  ("xt-graphics")
  ("xm-graphics")
  ("image"))

(defsys::defsystem :ol-silica
    (:default-pathname (frob-pathname "xm-silica"))
  ("pkg")
  ("xt-silica")
  ("ol-silica")
  ("xt-graphics")
  ("ol-graphics")
  ("image"))


(defsys::defsystem :xm-ws
    (:default-pathname (frob-pathname "xm-silica"))
  ("xt-frames")
  ("xm-frames")
  ("xt-gadgets")
  ("xm-gadgets"))

(defsys::defsystem :ol-ws
    (:default-pathname (frob-pathname "xm-silica"))
  ("xt-frames")
  ("ol-frames")
  ("xt-gadgets")
  ("ol-gadgets"))



