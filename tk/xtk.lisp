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
;; $fiHeader: xtk.lisp,v 1.6 92/02/24 13:04:18 cer Exp Locker: cer $

(in-package :tk)

(def-c-typedef :cardinal :unsigned-int)
(def-c-typedef xt-proc * :unsigned-long)
(def-c-typedef action-list * :char)
(def-c-typedef resource-list * :char)
(def-c-typedef xrm-quark :int)
(def-c-typedef xrm-quark-list * :int)
(def-c-typedef boolean :unsigned-long)
(def-c-typedef xrm-class  xrm-quark)
(def-c-typedef xt-enum   :long)
(def-c-typedef xt-version-type :long)

(def-c-type (xrm-resource :in-foreign-space) :struct
	    (name xrm-quark)
	    (class xrm-quark)
	    (type xrm-quark)
	    (size :cardinal)
	    (offset :long)
	    (default-type xrm-quark)
	    (default-addr * :char))

(def-c-type (xrm-resource-array :in-foreign-space) 1 xrm-resource)

(def-c-type (xtk-class :in-foreign-space) :struct
	    (superclass :long)
	    (name * :char)
	    (widget-size :cardinal)
	    (class-initialize xt-proc)
	    (class-part-initialize xt-proc)
	    (inited xt-enum)
	    (initialize xt-proc)
	    (initialize-hook xt-proc)
	    (realize xt-proc)
	    (actions action-list)
	    (num-actions :cardinal)
	    (resources resource-list)
	    (num-resources :cardinal)
	    (xrm-class xrm-class)
	    (compress-motion boolean)
	    (compress-exposure xt-enum)
	    (compress-enter-leave boolean)
	    (visible-interest boolean)
	    (destroy xt-proc)
	    (resize xt-proc)
	    (expose xt-proc)
	    (set-values xt-proc)
	    (set-values-hook xt-proc)
	    (set-values-almost xt-proc)
	    (get-values-hook xt-proc)
	    (accept-focus xt-proc)
	    (version xt-version-type)
	    (callback-private * :char))

(def-c-type (x-resource :in-foreign-space) :struct
  (name * :char)
  (class * :char)
  (type * :char)
  (size :cardinal)
  (offset :cardinal)
  (default-type * :char)
  (default-addr * :char)
  )

;; Horrible internal stuff


    
(def-c-type (xt-offset-rec :in-foreign-space) :struct
	    (next * :char)
	    (name xrm-quark)
	    (offset :int))

(defun import-offset-list (x)
  (let ((r nil))
    (do ((x x (xt-offset-rec-next x)))
	((zerop x)
	 (nreverse r))
      (push (list (xt-offset-rec-name x)
		  (xt-offset-rec-offset x)) r))))

(def-c-type (xtk-widget :in-foreign-space) :struct
  (self :unsigned-long)
  (widget-class :unsigned-long)
  )

(def-c-type (x-resource-list :in-foreign-space) 1 x-resource)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

