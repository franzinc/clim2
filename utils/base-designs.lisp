;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;
;; $Id: base-designs.lisp,v 2.7 2007/04/17 21:45:54 layer Exp $

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Franz, Inc.  All rights reserved."

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

(defmethod make-load-form ((design standard-opacity) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (value) design
    `(make-opacity ,value)))


;;; Gray colors

(defclass gray-color (color)
  ((luminosity :type single-float :initarg :luminosity
               :reader gray-color-luminosity)))

(define-constructor make-gray-color-1 gray-color (luminosity)
  :luminosity luminosity)

(defmethod make-load-form ((color gray-color) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (luminosity) color
    `(#-(or aclpc acl86win32) make-gray-color #+(or aclpc acl86win32) make-gray-color-1 ,luminosity)))


;;; Colors

(defclass rgb-color (color)
    ((red   :type single-float :initarg :red)
     (green :type single-float :initarg :green)
     (blue  :type single-float :initarg :blue)))

(define-constructor make-rgb-color-1 rgb-color (red green blue)
  :red red :green green :blue blue)

(defmethod make-load-form ((color rgb-color) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (red green blue) color
    `(make-rgb-color ,red ,green ,blue)))


(defclass ihs-color (color)
    ((intensity  :type single-float :initarg :intensity)
     (hue         :type single-float :initarg :hue)
     (saturation :type single-float :initarg :saturation)))

(define-constructor make-ihs-color-1 ihs-color (intensity hue saturation)
  :intensity intensity :hue hue :saturation saturation)

(defmethod make-load-form ((color ihs-color) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (intensity hue saturation) color
    `(make-ihs-color ,intensity ,hue ,saturation)))


;;; Palettes

(define-protocol-class palette ())

(defclass basic-palette (palette)
    ((port :reader palette-port :initarg :port)
     (color-p :reader palette-color-p :initarg :color-p)
     (dynamic-p :reader palette-dynamic-p :initarg :dynamic-p)
     (color-cache :reader palette-color-cache :initform (make-hash-table))
     (dynamic-color-cache :reader palette-dynamic-color-cache
                          :initform (make-hash-table))
     (layered-color-cache :reader palette-layered-color-cache
                          :initform (make-hash-table))
     (delayed-recolors :reader palette-delayed-recolors
                       :initform (make-array 32 :adjustable t :fill-pointer 0))))

(defgeneric make-palette (port &key))

(defparameter *all-palettes* nil)

(defmethod initialize-instance :after ((palette basic-palette) &key)
  (setq *all-palettes* (nconc *all-palettes* (list palette))))

(defgeneric update-palette-entry (palette pixel color))
(defgeneric update-palette-entries (palette updates))

(defgeneric allocate-color (color palette))
(defgeneric deallocate-color (color palette))


;;; Dynamic Colors 

(defclass dynamic-color (color)
    ((color :accessor dynamic-color-color :initarg :color)
     (palettes :type list 
               :initform nil
               :accessor dynamic-color-palettes)))

(define-constructor make-dynamic-color dynamic-color (color) 
  :color color)

(defmethod make-load-form ((color dynamic-color) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (color) color
    `(make-dynamic-color ,color)))


;;; Layered Colors

(defclass layered-color-set ()
    ((layers :initform nil :initarg :layers
             :reader layered-color-set-layers)
     (cache :initform (make-hash-table :test #'equal)
            :reader layered-color-set-cache)
     (dynamic-array :initarg :dynamic-array
                    :reader layered-color-set-dynamic-array)))

(define-constructor make-layered-color-set layered-color-set (&rest layers)
  :layers (copy-list layers)
  :dynamic-array (make-array layers))

(defgeneric layered-color (layered-color-set &rest layers))

(defclass layered-color (design)
    ((set :initarg :set :reader layered-color-set)
     (layers :initarg :layers :initform nil :reader layered-color-layers)
     (dynamic-colors :initform nil))) 
   
(define-constructor make-layered-color layered-color (set layers)
  :set set :layers layers)

;;; Device Colors 

(defclass device-color (color)
    ((palette :reader device-color-palette :initarg :palette)
     (pixel :reader device-color-pixel :initarg :pixel)))

(defmethod make-load-form ((color device-color) #-aclpc &optional #-aclpc environment)
  (declare (ignore environment))
  (with-slots (palette pixel) color
    `(make-device-color ,palette ,pixel)))


;;; Foreground and background (indirect) inks

(defconstant +foreground-ink+ (make-instance 'design))

(defmethod make-load-form ((design (eql (symbol-value '+foreground-ink+)))
			   #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  #-aclpc '+foreground-ink+
  #+aclpc '(symbol-value '+foreground-ink+))


(defconstant +background-ink+ (make-instance 'design))

(defmethod make-load-form ((design (eql (symbol-value '+background-ink+)))
			   #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  #-aclpc '+background-ink+
  #+aclpc '(symbol-value '+background-ink+))


;;; Flipping inks

(defclass flipping-ink (design)
    ((design1 :type design :initarg :design1)
     (design2 :type design :initarg :design2)))

(define-constructor make-flipping-ink-1 flipping-ink (design1 design2)
  :design1 design1 :design2 design2)

(defmethod make-load-form ((design flipping-ink) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (design1 design2) design
    `(#-(or aclpc acl86win32) make-flipping-ink #+(or aclpc acl86win32) make-flipping-ink-1 ',design1 ',design2)))


;;; Contrasting inks

(defclass contrasting-ink (design)
    ((how-many  :type integer :initarg :how-many)
     (which-one :type integer :initarg :which-one)))

(define-constructor make-contrasting-ink-1 contrasting-ink (which-one how-many)
  :which-one which-one :how-many how-many)

(defmethod make-load-form ((design contrasting-ink) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (which-one how-many) design
    `(make-contrasting-inks ,how-many ,which-one)))


;;; Patterns

(defclass pattern (design)
    ((array   :type (array * (* *)) :initarg :array
              :reader pattern-array)
     (designs :type vector            :initarg :designs 
              :reader pattern-designs)))

(defun make-pattern (array designs)
  #+Genera (declare lt:(side-effects simple reducible))
  (check-type array (array * (* *)))
  (make-instance 'pattern :array array :designs (coerce designs #-aclpc 'simple-vector #+aclpc 'vector)))

(defmethod make-load-form ((design pattern) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))

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

(defmethod make-load-form ((design stencil) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
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

(defmethod make-load-form ((tile rectangular-tile) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (design width height) tile
    (values `(make-instance 'rectangular-tile :width ,width :height ,height)
            `(setf (slot-value ',tile 'design) ',design))))


;;; Composite designs

(defclass composite-over (design)
    ((designs :type vector :initarg :designs)))

(defmethod make-load-form ((design composite-over) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (designs) design
    (values '(make-instance 'composite-over)
            `(setf (slot-value ',design 'designs) ',designs))))


(defclass composite-in (design)
    ((designs :type vector :initarg :designs)))

(defmethod make-load-form ((design composite-in) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (designs) design
    (values '(make-instance 'composite-in)
            `(setf (slot-value ',design 'designs) ',designs))))


(defclass composite-out (design)
    ((designs :type vector :initarg :designs)))

(defmethod make-load-form ((design composite-out) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (designs) design
    (values '(make-instance 'composite-out)
            `(setf (slot-value ',design 'designs) ',designs))))

