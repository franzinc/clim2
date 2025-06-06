;; See the file LICENSE for the full license governing this code.
;;

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :tk)

(provide :clim-debugxm)

(def-foreign-type (xm-proto-callback-info) 
    (:struct
     (handle :int)
     (data :int)))

(def-foreign-type (xm-protocol) 
    (:struct	    
     (object (* :char))
     (ext (* :char))
     (protocol (* :char))))

(def-foreign-type xm-string :int)
(def-foreign-type xm-text-position :long)

(def-foreign-type (xm-text-block-rec)
    (:struct
     (ptr (* :char))
     (length :int)
     (format x11:atom)))

(defmacro xm-text-block-rec-length (h) 
  `(ff:fslot-value-typed 'xm-text-block-rec :c ,h 'length))

(defmacro xm-text-block-rec-ptr (h) 
  `(ff:fslot-value-typed 'xm-text-block-rec :c ,h 'ptr))

(def-foreign-type xm-text-block (* xm-text-block-rec))

(def-foreign-type (xm-text-field-callback-struct) 
    (:struct
     (reason :int)
     (event (* x11:xevent))
     (doit boolean)
     (curr-insert xm-text-position)
     (new-insert xm-text-position)
     (start-pos xm-text-position)
     (end-pos xm-text-position)
     (text xm-text-block)))

(defmacro xm-text-field-callback-struct-text (h) 
  `(ff:fslot-value-typed 'xm-text-field-callback-struct :c ,h 'text))

(defmacro xm-text-field-callback-struct-start-pos (h) 
  `(ff:fslot-value-typed 'xm-text-field-callback-struct :c ,h 'start-pos))

(defmacro xm-text-field-callback-struct-end-pos (h) 
  `(ff:fslot-value-typed 'xm-text-field-callback-struct :c ,h 'end-pos))

(def-foreign-type (xm-file-selection-box-callback-struct)
    (:struct
     (reason :int)
     (event (* x11:xevent))
     (value xm-string) ;; xmstring
     (length :int)
     (mask xm-string)
     (mask-length :int)
     (dir xm-string)
     (dir-length :int)
     (pattern xm-string)
     (pattern-length xm-string)))

(def-foreign-type (xm-list-callback-struct) 
    (:struct
     (reason :int)
     (event (* x11:xevent))
     (item (* :void))			; xm-string (spr 30640; alemmens, 2005-11-30)
     (item-length :int)
     (item-position :int)
     (selection-items (* xm-string))
     (selected-item-count :int)
     (selected-item-positions (* :int))
     (selection-type :int)))

(defmacro xm-list-callback-struct-event (h) 
  `(ff:fslot-value-typed 'xm-list-callback-struct :c ,h 'event))

(defmacro xm-list-callback-struct-item (h) 
  `(ff:fslot-value-typed 'xm-list-callback-struct :c ,h 'item))

(defmacro xm-list-callback-struct-item-length (h) 
  `(ff:fslot-value-typed 'xm-list-callback-struct :c ,h 'item-length))

(defmacro xm-list-callback-struct-item-position (h) 
  `(ff:fslot-value-typed 'xm-list-callback-struct :c ,h 'item-position))

(defmacro xm-list-callback-struct-item-selection-items (h) 
  `(ff:fslot-value-typed 'xm-list-callback-struct :c ,h 'selection-items))

(defmacro xm-list-callback-struct-selected-item-count (h) 
  `(ff:fslot-value-typed 'xm-list-callback-struct :c ,h 'selected-item-count))

(defmacro xm-list-callback-struct-selected-item-positions (h) 
  `(ff:fslot-value-typed 'xm-list-callback-struct :c ,h 'selected-item-positions))

(def-foreign-type (xm-selected-position-array) 
    (:array :int 1))

(defmacro xm-selected-position-array (h i) 
  `(ff:fslot-value-typed 'xm-selected-position-array :c ,h ,i))

(def-foreign-type (xm-string-table) 
    (:array (* :char) 1))

(defsetf xm-string-table (arglist i) (new-val)
  `(setf (ff:fslot-value-typed 'xm-string-table :c ,arglist ,i) ,new-val))

;;; string constants from Xm.h
(x11::def-exported-constant XmRColormap		"Colormap")
(x11::def-exported-constant XmRVisual		"Visual")
(x11::def-exported-constant XmRInt		"Int")

(x11::def-exported-constant XmMULTIBYTE_TEXT    1)
(x11::def-exported-constant XmOUTPUT_ALL        0)
