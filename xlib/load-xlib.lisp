;; -*- mode: common-lisp; package: x11 -*-
;;
;;				-[]-
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
;; $fiHeader$


(in-package :x11)

(defmacro symbols-from-file (file)
  (with-open-file (s file :direction :input)
    (do ((r nil)
	 (l (read-line s nil nil) (read-line s nil nil)))
	((null l)
	 `(quote ,r))
      (push l r))))


(defun load-undefined-symbols-from-library (what kludges libraries)
  (setq what (remove-if #'ff::get-entry-point what))
  (when what
    (mapc #'foreign-functions:remove-entry-point kludges)
    (load "" 
	  :unreferenced-lib-names what
	  :foreign-files libraries
	  :print t)))

(defvar *libx11-pathname* "/usr/motif/usr/lib/libX11.a")

(defun load-from-xlib ()
  (x11::load-undefined-symbols-from-library
   (list* "_XCopyGC" (x11::symbols-from-file "misc/undefinedsymbols"))
   '("__unpack_quadruple" 
     "__prod_b10000" 
     "__carry_out_b10000" 
     "__prod_65536_b10000")
   (list x11::*libx11-pathname*)))

(load-from-xlib)
