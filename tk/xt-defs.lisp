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
;; $fiHeader: xt-defs.lisp,v 1.7 92/12/01 09:46:58 cer Exp $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

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
(def-c-typedef xt-geometry-mask :unsigned-int)
(def-c-typedef xt-position :short)
(def-c-typedef xt-dimension :unsigned-short)
(def-c-typedef xt-pointer * char)

(def-c-type (xt-class :no-defuns) :struct
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

(def-c-type (xt-resource :no-defuns) :struct
  (name * :char)
  (class * :char)
  (type * :char)
  (size :cardinal)
  (offset :cardinal)
  (default-type * :char)
  (default-addr * :char)
  )

;; Horrible internal stuff
    
(def-c-type (xt-offset-rec :no-defuns) :struct
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

(def-c-type (xt-widget :no-defuns) :struct
  (self :unsigned-long)
  (widget-class :unsigned-long)
  )

(def-c-type (xt-resource-list :in-foreign-space :no-defuns) 1 xt-resource)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-c-type (x-push-button-callback-struct :no-defuns) :struct
  (reason :int)
  (event * x11:xevent)
  (click-count :int))

(def-c-type (x-drawing-area-callback :no-defuns) :struct
  (reason :int)
  (event * x11:xevent)
  (window x11:window))

(def-c-type (xcharstruct-vector :no-defuns) 1 x11:xcharstruct)

(def-c-type (xfontname-list :no-defuns) 1 * :char)

(def-c-type (xfontstruct-array :no-defuns) 1 x11::xfontstruct)

(def-c-type (class-array :no-defuns) 1 :unsigned-long)

(def-c-type (xt-arg-val :no-defuns) * :void)

(def-c-type (xt-arg :in-foreign-space :no-defuns) :struct
  (name  * :char)
  (value xt-arg-val))

(def-c-type (xt-arglist :in-foreign-space :no-defuns) 1 xt-arg)

(def-c-type (xt-widget-list :no-defuns) 1 * xt-widget)

(def-c-type (xt-widget-geometry :no-defuns) :struct
  (request-mode xt-geometry-mask)
  (x xt-position)
  (y xt-position)
  (width xt-dimension)
  (height xt-dimension)
  (border-width xt-dimension)
  (sibling xt-widget)
  (stack-mode :int))

(def-c-type (xpoint-array :no-defuns) 1 x11::xpoint)
(def-c-type (xsegment-array :no-defuns) 1 x11::xsegment)
(def-c-type (xarc-array :no-defuns) 1 x11::xarc)
(def-c-type (xrectangle-array :no-defuns) 1 x11::xrectangle)
