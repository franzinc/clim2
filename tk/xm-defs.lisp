;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: xm-defs.lisp,v 1.15.22.2 1998/12/17 00:19:48 layer Exp $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :tk)

(provide :clim-debugxm)

(def-c-type (xm-proto-callback-info :in-foreign-space :no-defuns :no-constructor) :struct
	     (handle :int)
	     (data :int))

(def-c-type (xm-protocol :no-defuns :no-constructor) :struct
  (object * :char)
  (ext * :char)
  (protocol * :char))

(def-c-typedef xm-string :int)
(def-c-typedef xm-text-position :long)

(def-c-type (xm-text-block-rec :no-defuns) :struct
	    (ptr * :char)
	    (length :int)
	    (format x11:atom))

(def-c-typedef xm-text-block * xm-text-block-rec)

(def-c-type (xm-text-field-callback-struct :no-defuns :no-constructor) :struct
	    (reason :int)
	    (event * x11:xevent)
	    (doit boolean)
	    (curr-insert xm-text-position)
	    (new-insert xm-text-position)
	    (start-pos xm-text-position)
	    (end-pos xm-text-position)
	    (text xm-text-block))

(def-c-type (xm-file-selection-box-callback-struct :no-defuns :no-constructor) :struct
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

(def-c-type (xm-list-callback-struct :no-defuns :no-constructor) :struct
	    (reason :int)
	    (event * x11:xevent)
	    (item xm-string)
	    (item-length :int)
	    (item-position :int)
	    (selection-items * xm-string)
	    (selected-item-count :int)
	    (selected-item-positions * :int)
	    (selection-type :int))

(def-c-type (xm-selected-position-array :no-defuns :no-constructor) 1 :int)

(def-c-type (xm-string-table :no-defuns :no-constructor) 1 * :char)
