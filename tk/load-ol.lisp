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
;; $fiHeader$

(in-package :tk)

#|
make ucl_xtras='/usr/tech/cer/stuff/clim-2.0/ol-classes.o /usr/tech/cer/stuff/clim-2.0/lib/libXol.a /usr/tech/cer/stuff/clim-2.0/lib/libXt.a /usr/tech/cer/stuff/clim-2.0/lib/libX11.a' ucl
|#

(flet ((foundp (entry-point)
	       (let ((x (make-array 1 :initial-contents
				    (list (ff:convert-to-lang
					   entry-point))))
		     (y 
		 
		      (make-array 1 :element-type '(unsigned-byte 32))))
		 (zerop (ff:get-entry-points x y)))))
  (unless (foundp "insert_classes")
    #+ingnore
	(mapc #'foreign-functions:remove-entry-point 
	      '("__unpack_quadruple" 
		"__prod_b10000" 
		"__carry_out_b10000" 
		"__prod_65536_b10000"))
	(load "classes.o" 
	      :foreign-files 
	      '("/vapor/usr/tech/cer/stuff/clim-2.0/lib/libXol.a"
		"/vapor/usr/tech/cer/stuff/clim-2.0/lib/libXt.a"
		"/vapor/usr/tech/cer/stuff/clim-2.0/lib/libX11.a") 
	      :print t)))
