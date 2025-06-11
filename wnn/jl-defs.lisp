;; -*- mode: common-lisp; package: wnn -*-
;;
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: jl-defs.lisp,v 1.1.1.1 2025/02/20 23:30:11 wessel Exp $

(in-package :wnn)

(defconstant wnn_yomi_size 10)
(defconstant wnn_sho 0)
(defconstant wnn_dai 1)
(defconstant wnn_no_use 0)
(defconstant wnn_use_mae 1)
(defconstant wnn_use_ato 2)
(defconstant wnn_use_zengo (logior wnn_use_mae wnn_use_ato))
(defconstant wnn_uniq_knj 2)
(defconstant wnn_uniq 1)
(defconstant wnn_no_uniq 0)
(defconstant wnn_no_create 0)
(defconstant wnn_create -1)
(defconstant wnn_dic_prio_default 5)
(defconstant wnn_yomi 0)
(defconstant wnn_kanji 1)

#+:ignore
(def-c-type (wnn-buf :no-defuns) :struct
	    (env * wnn-env)
	    (bun-suu :unsigned-int)
	    (zenkouho-suu :unsigned-int)
	    (bun * * wnn-bun)
	    (down-bnst * * wnn-bun)
	    (zenkouho * * wnn-bun)
	    (zenkouho-dai * :unsigned-int)
	    (zenkouho-dai-suu :unsigned-int)
	    (c-zenkouho :short)
	    (zenkouho-daip :short)
	    (zenkouho-bun :signed-int)
	    (zenkouho-end-bun :signed-int)
	    (zenkouho-endvect :signed-int)
	    (free-heap * wnn-bun)
	    (heap * char)
	    (msize-bun :unsigned-int)
	    (msize-zenkouho :unsigned-int))

(def-foreign-type wnn-buf 
    (:struct
     (env (* wnn-env))
     (bun-suu :unsigned-int)
     (zenkouho-suu :unsigned-int)
     (bun (* (* wnn-bun)))
     (down-bnst (* (* wnn-bun)))
     (zenkouho (* (* wnn-bun)))
     (zenkouho-dai (* :unsigned-int))
     (zenkouho-dai-suu :unsigned-int)
     (c-zenkouho :short)
     (zenkouho-daip :short)
     (zenkouho-bun :signed-int)
     (zenkouho-end-bun :signed-int)
     (zenkouho-endvect :signed-int)
     (free-heap (* wnn-bun))
     (heap (* char))
     (msize-bun :unsigned-int)
     (msize-zenkouho :unsigned-int)))

