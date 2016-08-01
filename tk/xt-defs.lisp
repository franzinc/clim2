;; See the file LICENSE for the full license governing this code.
;;

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
(def-c-typedef boolean :char)
(def-c-typedef xrm-class  xrm-quark)
(def-c-typedef xt-enum   :unsigned-char)
(def-c-typedef xt-version-type :long)
(def-c-typedef xt-geometry-mask :unsigned-int)
(def-c-typedef xt-position :short)
(def-c-typedef xt-dimension :unsigned-short)
(def-c-typedef xt-pointer * char)

(def-c-type (xt-class :no-defuns :no-constructor) :struct
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

(def-c-type (xt-resource :no-defuns :no-constructor) :struct
  (name * :char)
  (class * :char)
  (type * :char)
  (size :cardinal)
  (offset :cardinal)
  (default-type * :char)
  (default-addr * :char)
  )

;; Horrible internal stuff

(def-c-type (xt-offset-rec :no-defuns :no-constructor) :struct
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

(def-c-type (xt-widget :no-defuns :no-constructor) :struct
  (self :unsigned-long)
  (widget-class :unsigned-long)
  )

(def-c-type (xt-resource-list :in-foreign-space :no-defuns :no-constructor) 1 xt-resource)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-c-type (x-push-button-callback-struct :no-defuns :no-constructor) :struct
  (reason :int)
  (event * x11:xevent)
  (click-count :int))

(def-c-type (x-drawing-area-callback :no-defuns :no-constructor) :struct
  (reason :int)
  (event * x11:xevent)
  (window x11:window))

(def-c-type (xcharstruct-vector :no-defuns :no-constructor) 1 x11:xcharstruct)

(def-c-type (xfontname-list :no-defuns :no-constructor) 1 * :char)

(def-c-type (xfontstruct-array :no-defuns :no-constructor) 1 x11::xfontstruct)

(def-c-type (class-array :no-defuns :no-constructor) 1 :unsigned-long)

(def-c-type (xt-arg-val :no-defuns :no-constructor) :long)

(def-c-type (xt-arg :in-foreign-space :no-defuns :no-constructor) :struct
  (name  * :char)
  (value xt-arg-val))

(def-c-type (xt-arglist :in-foreign-space :no-defuns :no-constructor) 1 xt-arg)

(def-c-type (xt-widget-list :no-defuns :no-constructor) 1 * xt-widget)

(def-c-type (xt-widget-geometry :no-defuns :no-constructor) :struct
  (request-mode xt-geometry-mask)
  (x xt-position)
  (y xt-position)
  (width xt-dimension)
  (height xt-dimension)
  (border-width xt-dimension)
  (sibling xt-widget)
  (stack-mode :int))


;; general pointer-array

(def-c-type (pointer-array :no-defuns :no-constructor) 1 * char)

(x11::def-exported-constant lc-ctype 0)
(x11::def-exported-constant lc-all 6)
