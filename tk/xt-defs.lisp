;; See the file LICENSE for the full license governing this code.
;;

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :tk)

(def-foreign-type :cardinal :unsigned-int)
(def-foreign-type xt-proc (* :unsigned-long))
(def-foreign-type action-list (* :char))
(def-foreign-type resource-list (* :char))
(def-foreign-type xrm-quark :int)
(def-foreign-type xrm-quark-list (* :int))
(def-foreign-type boolean :char)
(def-foreign-type xrm-class  xrm-quark)
(def-foreign-type xt-enum   :unsigned-char)
(def-foreign-type xt-version-type :long)
(def-foreign-type xt-geometry-mask :unsigned-int)
(def-foreign-type xt-position :short)
(def-foreign-type xt-dimension :unsigned-short)
(def-foreign-type xt-pointer (* char))

(def-foreign-type xt-class
    (:struct
	    (superclass :long)
	    (name (* :char))
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
	    (callback-private (* :char))))

(defmacro xt-class-name (h)
  `(ff:fslot-value-typed 'xt-class :c ,h 'name))

(def-foreign-type xt-resource 
    (:struct
     (name (* :char))
     (class (* :char))
     (type (* :char))
     (size :cardinal)
     (offset :cardinal)
     (default-type (* :char))
     (default-addr (* :char))
     ))

(defmacro xt-resource-0-list (h &optional (i 0))
  `(ff:fslot-value-typed 'xt-resource-list :c ,h ,i))

(defmacro xt-resource-0-name (h)
  `(ff:fslot-value-typed 'xt-resource :c ,h 'name))

(defmacro xt-resource-0-class (h)
  `(ff:fslot-value-typed 'xt-resource :c ,h 'class))

(defmacro xt-resource-0-type (h)
  `(ff:fslot-value-typed 'xt-resource :c ,h 'type))

(def-foreign-type xt-offset-rec 
    (:struct
	    (next (* :char))
	    (name xrm-quark)
	    (offset :int)))

(defun import-offset-list (x)
  (let ((r nil))
    (do ((x x (xt-offset-rec-next x)))
	((zerop x)
	 (nreverse r))
      (push (list (xt-offset-rec-name x)
		  (xt-offset-rec-offset x)) r))))

;;; problem in this typdef? widget-class type? 
(def-foreign-type xt-widget
    (:struct
     (self :unsigned-long)
     (widget-class :unsigned-long)
     ))

(defmacro widget-class-name (h)
  `(values 
    (excl:native-to-string 
     (ff:fslot-value-typed 'xt-class :c ,h 'name))))

(defmacro xt-class-0-superclass (h)
  `(ff:fslot-value-typed 'xt-class :c ,h 'superclass))

(def-foreign-type xt-resource-list (:array xt-resource 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-foreign-type x-push-button-callback-struct
    (:struct
     (reason :int)
     (event (* x11:xevent))
     (click-count :int)))

(def-foreign-type x-drawing-area-callback
    (:struct
     (reason :int)
     (event (* x11:xevent))
     (window x11:window)))

(defmacro x-drawing-area-callback-window (h)
  `(ff:fslot-value-typed 'x-drawing-area-callback :c (ensure-pointer ,h) 'window))

(defmacro x-drawing-area-callback-event (h)
  `(ff:fslot-value-typed 'x-drawing-area-callback :c (ensure-pointer ,h) 'event))

(defmacro x-push-button-callback-struct-click-count (h)
  `(ff:fslot-value-typed 'x-push-button-callback-struct :c (ensure-pointer ,h) 'click-count))

(defmacro xt-widget-widget-class0 (h)
  `(ff:fslot-value-typed 'xt-widget :c (ensure-pointer ,h) 'widget-class))

(def-foreign-type xcharstruct-vector 
    (:array x11:xcharstruct 1)) 

(defmacro xcharstruct-vector (h index)
  `(ff::fslot-value-typed 'xcharstruct-vector :c (ensure-pointer ,h) ,index))

(defmacro xcharstruct-vector-width (h index) 
  `(ff::fslot-value-typed 'xcharstruct-vector :c (ensure-pointer ,h) ,index 'width))

(def-foreign-type xfontname-list 
    (:array (* :char) 1))

(def-foreign-type xfontstruct-array 
    (:array x11::xfontstruct 1))

(def-foreign-type class-array 
    (:array :unsigned-long 1))

(defmacro class-array (ep index)
  `(ff::fslot-value-typed 'class-array :c ,ep ,index))

(def-foreign-type xt-arg-val :long)

(def-foreign-type xt-arg 
    (:struct
     (name  (* :char))
     (value xt-arg-val)))

(def-foreign-type xt-arglist 
    (:array xt-arg 1))

(defmacro xt-arg-modifiermap (h)
  `(ff:fslot-value-typed 'xmodifierkeymap :c ,h 'modifiermap))

(defmacro xt-arglist-value (arglist i)
  `(ff:fslot-value-typed 'xt-arglist :c ,arglist ,i 'value))

(defsetf xt-arglist-name (arglist i) (new-val) 
  `(setf (ff:fslot-value-typed 'xt-arglist :c ,arglist ,i 'name)
     ,new-val))

(defsetf xt-arglist-value (arglist i) (new-val) 
  `(setf (ff:fslot-value-typed 'xt-arglist :c ,arglist ,i 'value) ,new-val))

(def-foreign-type xt-widget-list 
    (:array (* xt-widget) 1))

(defmacro xt-widget-list (h index)   
  `(ff::fslot-value-typed 'xt-widget-list :c (ensure-pointer ,h) ,index))

(def-foreign-type xt-widget-geometry
    (:struct
     (request-mode xt-geometry-mask)
     (x xt-position)
     (y xt-position)
     (width xt-dimension)
     (height xt-dimension)
     (border-width xt-dimension)
     (sibling xt-widget)
     (stack-mode :int)))

(defmacro xt-widget-geometry-request-mode (h) 
  `(ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'request-mode))

(defsetf xt-widget-geometry-request-mode (h) (new-val) 
  `(setf (ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'request-mode) ,new-val))

(defmacro xt-widget-geometry-x (h) 
  `(ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'x))

(defsetf xt-widget-geometry-x (h) (new-val)
  `(setf (ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'x) ,new-val))

(defmacro xt-widget-geometry-y (h) 
  `(ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'y))

(defsetf xt-widget-geometry-y (h) (new-val)
  `(setf (ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'y) ,new-val))

(defmacro xt-widget-geometry-width (h) 
  `(ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'width))

(defsetf xt-widget-geometry-width (h) (new-val)
  `(setf (ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'width) ,new-val))

(defmacro xt-widget-geometry-height (h) 
  `(ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'height))

(defsetf xt-widget-geometry-height (h) (new-val)
  `(setf (ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'height) ,new-val))

(defmacro xt-widget-geometry-border-width (h) 
  `(ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'border-width))

(defsetf xt-widget-geometry-border-width (h) (new-val)
  `(setf (ff:fslot-value-typed 'xt-widget-geometry :c (ensure-pointer ,h) 'border-width) ,new-val))

;; general pointer-array

(def-foreign-type pointer-array 
    (:array (* char) 1))

(x11::def-exported-constant lc-ctype 0)

(x11::def-exported-constant lc-all 6)
