;; -*- mode: common-lisp; package: wnn -*-
;;
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $Id: jl-funs.lisp,v 1.6 2002/07/09 20:57:19 layer Exp $

(in-package :wnn)

(def-foreign-call jl_open_lang
    ((x :foreign-address) (y :foreign-address) (z :foreign-address)
			  a b c d)
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_connect_lang
    ((x :foreign-address) (y :foreign-address) (z :foreign-address)
			  a b c d)
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_isconnect_e
    ((x :foreign-address))
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_env_get
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_ren_conv
    ((x :foreign-address) (y :foreign-address) a b c)
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call wnn_get_area
    ((a :foreign-address) b c (d :foreign-address) e)
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_kanji_len
    ((x :foreign-address) y z)
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_yomi_len
    ((x :foreign-address) y z)
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_get_zenkouho_kanji
    ((x :foreign-address) y (z :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_get_zenkouho_yomi
    ((x :foreign-address) y (z :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_zenkouho
    ((w :foreign-address) x y z)
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_zenkouho_dai
    ((w :foreign-address) x y z)
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_set_jikouho
    ((x :foreign-address) y)
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call jl_update_hindo
    ((x :foreign-address) y z)
  :returning :int
  :call-direct t
  :arg-checking nil)
