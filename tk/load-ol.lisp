;; -*- mode: common-lisp; package: tk -*-
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
;; $fiHeader: load-ol.lisp,v 1.10 92/05/22 19:26:22 cer Exp $

(in-package :tk)

(defvar sys::*libxol-pathname* "/vapor/usr/tech/cer/stuff/clim-2.0/tk/lib2/libXol.a")
(defvar sys::*libxt-pathname* "/x11/R4/sun4-lib/libXt_d.a")

(defun load-from-ol ()
  (x11::load-undefined-symbols-from-library
   "stub-olit.o"
   (x11::symbols-from-file 
    "misc/undefinedsymbols.xt"
    "misc/undefinedsymbols.olit")
   '("__unpack_quadruple" 
     "__unpacked_to_decimal"
     "__prod_b10000" 
     "__carry_out_b10000" 
     "__prod_65536_b10000"
     ;; got these when compiling on ox
     "__pack_integer"
     "_class_double"
     "_class_single"
     "_class_extended"
     "__unpack_integer"
     )
   (list sys::*libxol-pathname*
	 sys::*libxt-pathname*
	 sys::*libx11-pathname*)))

(load-from-ol)

