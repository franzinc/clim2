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
;; $fiHeader: load-ol.lisp,v 1.5 92/02/16 20:55:00 cer Exp $

(in-package :tk)


(defun load-ol (&optional (what *openlook-classes*))
  (setq what (remove-if #'ff::get-entry-point `(,@what)))
  (when what
    (mapc #'foreign-functions:remove-entry-point 
	  '("__unpack_quadruple" 
	    "__prod_b10000" 
	    "__unpacked_to_decimal"
	    "__carry_out_b10000" 
	    "__prod_65536_b10000"))
    (load ""
	  :unreferenced-lib-names 
	  what
	  :foreign-files 
	  '("/usr/openwin-3.0/lib/libXol.a"
	    "/usr/motif/usr/lib/libXt.a"
	    "/usr/motif/usr/lib/libX11.a"
	    ;; Hopefully
	    ;;"/usr/openwin-3.0/lib/libXt.a"
	    ;; "/usr/openwin-3.0/lib/libX11.a"
	    )
	  :print t)))

(load-ol)
