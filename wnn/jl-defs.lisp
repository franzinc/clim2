;; -*- mode: common-lisp; package: wnn -*-
;;
;; 
;; See the file LICENSE for the full license governing this code.
;;

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

