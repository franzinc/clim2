;; -*- mode: common-lisp; package: wnn -*-
;;
;;
;; See the file LICENSE for the full license governing this code.
;;

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
