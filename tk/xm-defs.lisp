;; -*- mode: common-lisp; package: tk -*-
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
;; $fiHeader: xm-defs.lisp,v 1.4 92/05/06 15:37:10 cer Exp Locker: cer $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :tk)

(def-c-type (xm-proto-callback-info :no-defuns :in-foreign-space) :struct
	     (handle :int)
	     (data :int))

(def-c-type (xm-protocol :no-defuns) :struct
  (object * :char)
  (ext * :char)
  (protocol * :char))

(def-c-typedef xm-string :int)

(def-c-type (xm-file-selection-box-callback-struct :no-defuns) :struct
	    (reason :int)
	    (event * x11:xevent)
	    (value xm-string) ;; xmstring
	    (length :int)
	    (mask xm-string)
	    (mask-length :int)
	    (dir xm-string)
	    (dir-length :int)
	    (pattern xm-string)
	    (pattern-length xm-string))

(def-c-type (xm-list-callback-struct :no-defuns) :struct
	    (reason :int)
	    (event * x11:xevent)
	    (item xm-string)
	    (item-length :int)
	    (item-position :int)
	    (selection-items * xm-string)
	    (selected-item-count :int)
	    (selected-item-positions * :int)
	    (selection-type :int))
	    
(def-c-type (xm-selected-position-array :no-defuns) 1 :int)

