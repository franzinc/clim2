;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: base-designs.lisp,v 1.2 92/10/04 14:16:12 cer Exp $

(in-package :clim-utils)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1992 Franz, Inc.  All rights reserved."

;;; Basic designs definitions

;;; The DESIGN and OPACITY classes are already defined.

;;; Generic Functions

(defgeneric color-rgb (color))
(defgeneric color-ihs (color))

(defgeneric compose-over (design1 design2))
(defgeneric compose-in (design1 design2))
(defgeneric compose-out (design1 design2))

(defgeneric make-flipping-ink (design1 design2))


;;; Opacity

(defclass standard-opacity (opacity)
    ((value :type single-float :initarg :value :reader opacity-value)))

(define-constructor make-standard-opacity-1 standard-opacity (value)
		    :value value)

(defmethod make-load-form ((design standard-opacity))
  (with-slots (value) design
    `(make-opacity ,value)))


;;; Gray colors

(defclass gray-color (color)
  ((luminosity :type single-float :initarg :luminosity
	       :reader gray-color-luminosity)))

(define-constructor make-gray-color-1 gray-color (luminosity)
		    :luminosity luminosity)

(defmethod make-load-form ((color gray-color))
  (with-slots (luminosity) color
    `(make-gray-color ,luminosity)))


;;; Colors

(defclass rgb-color (color)
    ((red   :type single-float :initarg :red)
     (green :type single-float :initarg :green)
     (blue  :type single-float :initarg :blue)))

(define-constructor make-rgb-color-1 rgb-color (red green blue)
  :red red :green green :blue blue)

(defmethod make-load-form ((color rgb-color))
  (with-slots (red green blue) color
    `(make-rgb-color ,red ,green ,blue)))


(defclass ihs-color (color)
    ((intensity  :type single-float :initarg :intensity)
     (hue	 :type single-float :initarg :hue)
     (saturation :type single-float :initarg :saturation)))

(define-constructor make-ihs-color-1 ihs-color (intensity hue saturation)
		    :intensity intensity :hue hue :saturation saturation)

(defmethod make-load-form ((color ihs-color))
  (with-slots (intensity hue saturation) color
    `(make-ihs-color ,intensity ,hue ,saturation)))


;;; Palettes

(define-protocol-class palette ())

(defclass basic-palette (palette)
    ((port :reader palette-port :initarg :port)
     (color-p :reader palette-color-p :initarg :color-p)
     (mutable-p :reader palette-mutable-p :initarg :mutable-p)
     (color-cache :reader palette-color-cache :initform (make-hash-table))
     (mutable-color-cache :reader palette-mutable-color-cache
			  :initform (make-hash-table))
     (color-group-cache :reader palette-color-group-cache
			:initform (make-hash-table))
     (delayed-mutations :reader palette-delayed-mutations
			:initform (make-array 32 :adjustable t :fill-pointer 0))))

(defgeneric make-palette (port &key))

(defparameter *all-palettes* nil)

(defmethod initialize-instance :after ((palette basic-palette) &key)
  (setq *all-palettes* (nconc *all-palettes* (list palette))))

(defgeneric update-palette-entry (palette pixel color))
(defgeneric update-palette-entries (palette updates))


;;; Mutable Colors 

(defclass mutable-color (color)
    ((color :accessor mutable-color-color :initarg :color)
     (palettes :type list 
	       :initform nil
	       :accessor mutable-color-palettes)))

(define-constructor make-mutable-color-1 mutable-color (color) 
		    :color color)

(defmethod make-load-form ((color mutable-color))
  (with-slots (color) color
    `(make-mutable-color ,color)))


;;; Color Groups

(defclass color-group ()
    ((layers :initform nil :reader color-group-layers :initarg :layers)
     (cache :initform (make-hash-table :test #'equal) :reader color-group-cache)
     (mutable-array :reader color-group-mutable-array :initarg :mutable-array)))

(define-constructor make-color-group color-group (&rest layers)
		    :layers layers
		    :mutable-array (make-array layers))

(defgeneric group-color (color-group &rest layers))

(defclass group-color (design)
    ((group :reader group-color-group :initarg :group)
     (layers :initform nil :reader group-color-layers :initarg :layers)
     (mutables :initform nil))) 
   
(define-constructor make-group-color group-color (group layers)
		    :group group :layers layers)


;;; Foreground and background (indirect) inks

(defconstant +foreground-ink+ (make-instance 'design))

(defmethod make-load-form ((design (eql (symbol-value '+foreground-ink+))))
  '+foreground-ink+)


(defconstant +background-ink+ (make-instance 'design))

(defmethod make-load-form ((design (eql (symbol-value '+background-ink+))))
  '+background-ink+)


;;; Flipping inks

(defclass flipping-ink (design)
    ((design1 :type design :initarg :design1)
     (design2 :type design :initarg :design2)))

(define-constructor make-flipping-ink-1 flipping-ink (design1 design2)
		    :design1 design1 :design2 design2)

(defmethod make-load-form ((design flipping-ink))
  (with-slots (design1 design2) design
    `(make-flipping-ink ',design1 ',design2)))


;;; Contrasting inks

(defclass contrasting-ink (design)
    ((how-many  :type integer :initarg :how-many)
     (which-one :type integer :initarg :which-one)))

(define-constructor make-contrasting-ink-1 contrasting-ink (which-one how-many)
		    :which-one which-one :how-many how-many)

(defmethod make-load-form ((design contrasting-ink))
  (with-slots (which-one how-many) design
    `(make-contrasting-inks ,how-many ,which-one)))


;;; Patterns

(defclass pattern (design)
    ((array   :type (array * (* *)) :initarg :array)
     (designs :type vector	    :initarg :designs)))

(defun make-pattern (array designs)
  #+Genera (declare lt:(side-effects simple reducible))
  (check-type array (array * (* *)))
  (make-instance 'pattern :array array :designs (coerce designs 'vector)))

(defmethod make-load-form ((design pattern))
  (with-slots (array designs) design
    (values `(make-instance 'pattern :array ',array)
	    `(setf (slot-value ',design 'designs) ',designs))))


;;; Stencils

(defclass stencil (design)
    ((array :type (array * (* *)) :initarg :array :reader stencil-array)))

(defun make-stencil (array)
  #+Genera (declare lt:(side-effects simple reducible))
  (check-type array (array * (* *)))
  (make-instance 'stencil :array array))

(defmethod make-load-form ((design stencil))
  (with-slots (array) design
    `(make-stencil ',array)))


;;; Tiles

(defclass rectangular-tile (design)
    ((design :type design :initarg :design)
     (width  :type real   :initarg :width)
     (height :type real   :initarg :height)))

(defun make-rectangular-tile (design width height)
  #+Genera (declare lt:(side-effects simple reducible))
  (check-type width fixnum)
  (check-type height fixnum)
  (make-instance 'rectangular-tile :design design :width width :height height))

(defmethod make-load-form ((tile rectangular-tile))
  (with-slots (design width height) tile
    (values `(make-instance 'rectangular-tile :width ,width :height ,height)
	    `(setf (slot-value ',tile 'design) ',design))))


;;; Composite designs

(defclass composite-over (design)
    ((designs :type vector :initarg :designs)))

(defmethod make-load-form ((design composite-over))
  (with-slots (designs) design
    (values '(make-instance 'composite-over)
	    `(setf (slot-value ',design 'designs) ',designs))))


(defclass composite-in (design)
    ((designs :type vector :initarg :designs)))

(defmethod make-load-form ((design composite-in))
  (with-slots (designs) design
    (values '(make-instance 'composite-in)
	    `(setf (slot-value ',design 'designs) ',designs))))


(defclass composite-out (design)
    ((designs :type vector :initarg :designs)))

(defmethod make-load-form ((design composite-out))
  (with-slots (designs) design
    (values '(make-instance 'composite-out)
	    `(setf (slot-value ',design 'designs) ',designs))))
