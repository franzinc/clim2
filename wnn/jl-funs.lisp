;; -*- mode: common-lisp; package: wnn -*-
;;
;;				-[Fri Dec  3 23:34:30 1999 by duane]-
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $Id: jl-funs.lisp,v 1.5 2000/03/04 05:13:53 duane Exp $

(in-package :wnn)

(defforeign 'jl_open_lang
    :arguments '(foreign-address foreign-address foreign-address
		 integer integer integer integer)
    :return-type :foreign-address
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_connect_lang
    :arguments '(foreign-address foreign-address foreign-address
		 integer integer integer integer)
    :return-type :foreign-address
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_isconnect_e
    :arguments '(foreign-address)
    :return-type :integer
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_env_get
    :arguments '(foreign-address)
    :return-type :foreign-address
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_ren_conv
    :arguments '(foreign-address foreign-address
		 integer integer integer)
    :return-type :integer
    :call-direct t
    :arg-checking nil)

(defforeign 'wnn_get_area
    :arguments '(foreign-address integer integer
		 foreign-address integer)
    :return-type :integer
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_kanji_len
    :arguments '(foreign-address integer integer)
    :return-type :integer
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_yomi_len
    :arguments '(foreign-address integer integer)
    :return-type :integer
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_get_zenkouho_kanji
    :arguments '(foreign-address integer foreign-address)
    :return-type :void
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_get_zenkouho_yomi
    :arguments '(foreign-address integer foreign-address)
    :return-type :void
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_zenkouho
    :arguments '(foreign-address
		 integer integer integer)
    :return-type :integer
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_zenkouho_dai
    :arguments '(foreign-address
		 integer integer integer)
    :return-type :integer
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_set_jikouho
    :arguments '(foreign-address integer)
    :return-type :integer
    :call-direct t
    :arg-checking nil)

(defforeign 'jl_update_hindo
    :arguments '(foreign-address integer integer)
    :return-type :integer
    :call-direct t
    :arg-checking nil)
