;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: classes.lisp,v 1.19 92/09/24 09:37:25 cer Exp Locker: cer $

(in-package :silica)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(define-protocol-class port ())

(locally
(declare (special *undefined-text-style*))

;; This is called BASIC-PORT rather than STANDARD-PORT because the class
;; cannot be used as-is.  It has to be specialized for each implementation.
(defclass basic-port (port) 
    ((server-path :reader port-server-path)
     (properties :initform nil :accessor port-properties)
     (lock :initform (make-lock "a port lock") :reader port-lock)
     (grafts :initform nil :accessor port-grafts)
     (process :initform nil :accessor port-process)
     (frame-managers :initform nil :accessor port-frame-managers)
     (modifier-state :initform (make-modifier-state)
		     :accessor port-modifier-state)
     (mirror->sheet-table :initform (make-hash-table) 
			  :reader port-mirror->sheet-table)
     (focus :initform nil :accessor port-keyboard-input-focus)
     (focus-selection :initform :sheet-under-pointer :initarg :focus-selection
		      :accessor port-input-focus-selection
		      :type (member :sheet-under-pointer :click-to-select))
     (trace-thing :initform (make-array 10 :fill-pointer 0 :adjustable t)
		  :reader port-trace-thing)
     (medium-cache :initform nil :accessor port-medium-cache)
     ;; this slot can go as caches are now on a per palette rather
     ;; then per port basis (cim)
     (color-cache :initform (make-hash-table) :reader port-color-cache)
     (default-palette :reader port-default-palette)
     (pointer :initform nil)
     (mapping-table :initform (make-hash-table :test #'equal))
     (mapping-cache :initform (cons nil nil))	;one entry cache
     (undefined-text-style :initform *undefined-text-style*
			   :accessor port-undefined-text-style)
     ;; When this is true, the text style to device font mapping is done
     ;; loosely.  That is, the actual screen size of the font need not be
     ;; exactly what the user has asked for.  Instead the closest fit is
     ;; chosen.  This is necessary in X11 because different screen sizes &
     ;; resolutions result in fonts having *actual displayed sizes* that
     ;; are not exactly what the fonts were designed for.  Loose text style
     ;; mapping is done by having the mapping table ignore size when hashing.
     ;; Thus each bucket is a list of fonts with the same family and face,
     ;; but different sizes.  They are kept sorted small to large.
     (allow-loose-text-style-size-mapping 
       :initform nil :initarg :allow-loose-text-style-size-mapping)
     (canonical-gesture-specs :reader port-canonical-gesture-specs
			      :initform (make-hash-table :test #'equal))))

)	;locally



(define-protocol-class sheet ())

(defclass basic-sheet (sheet)
	  ((port :initform nil :reader port)
	   (graft :initform nil :reader graft)
	   (parent :initform nil 
		   :accessor sheet-parent)
	   (region :initarg :region :initform (make-bounding-rectangle 0 0 100 100)
		   :accessor sheet-region)
	   (enabledp :initform nil :accessor sheet-enabled-p)
	   ;;-- Is this the best place
	   (cursor :initform nil :reader sheet-pointer-cursor)))


(define-protocol-class medium ())

(locally
(declare (special *default-text-style*))

;; This is called BASIC-MEDIUM rather than STANDARD-MEDIUM because the class
;; cannot be used as-is.  It has to be specialized for each implementation.
(defclass basic-medium (medium)
    ((port :initarg :port :reader port)
     (sheet :initarg :sheet :initform nil :accessor medium-sheet)
     (foreground :initform +black+ :accessor medium-foreground)
     (background :initform +white+ :accessor medium-background)
     (ink :initform +foreground-ink+ :accessor medium-ink)
     (text-style :initform *default-text-style* 
		 :accessor medium-text-style)
     (default-text-style :initform *default-text-style*
			 :accessor medium-default-text-style)
     (merged-text-style :initform *default-text-style*
			:writer (setf medium-merged-text-style))
     (merged-text-style-valid :initform nil
			      :accessor medium-merged-text-style-valid)
     (line-style :initform (make-line-style) :accessor medium-line-style)
     (transformation :initarg :transformation :initform +identity-transformation+
		     :accessor medium-transformation )
     (region :initarg :region :initform +everywhere+
	     :accessor medium-clipping-region)
     (+y-upward-p :initform nil :accessor medium-+y-upward-p)))

)	;locally


;;; Palettes

(define-protocol-class palette ())

(defclass basic-palette (palette)
  ((port :reader palette-port :initarg :port)
   (color-cache :initform (make-hash-table) :reader palette-color-cache)
   (mutable-color-cache :initform (make-hash-table) 
			:reader palette-mutable-color-cache)
   (color-group-cache :initform (make-hash-table) 
		      :reader palette-color-group-cache)
   (delayed-mutations 
    :initform (make-array 32 :adjustable t :fill-pointer 0)
    :reader palette-delayed-mutations)))

(defgeneric make-palette (port &key))

(defparameter *all-palettes* nil)

(defmethod initialize-instance :after ((palette basic-palette) &key)
  (push palette *all-palettes*))


;;; Event types

(defmacro define-event-class (name supers slots &rest rest)
  `(progn
     (defclass ,name ,supers (,@slots) ,@rest)
     ;; We could use a class-allocated slot, but that is very slow
     ;; in some implementations of CLOS
     (defmethod event-type ((event ,name))
       ,(intern (subseq 
		  (symbol-name name) 
		  0 (search (symbol-name '-event) (symbol-name name)
			    :from-end t))
		*keyword-package*))))

;; A fixnum, incremented only with ATOMIC-INCF
(defvar *event-timestamp* 0)

(define-event-class event () 
  ((timestamp :reader event-timestamp
	      :initform (atomic-incf *event-timestamp*) :initarg :timestamp)))


(define-event-class device-event (event) 
  ((sheet :reader event-sheet :initarg :sheet)
   (modifier-state :reader event-modifier-state
		   :initform 0 :initarg :modifier-state)))


(define-event-class keyboard-event (device-event)
  ((key-name :reader keyboard-event-key-name :initarg :key-name)
   ;; This is NIL for keyboard events that don't correspond to
   ;; characters in the standard CL character set
   (character :reader keyboard-event-character :initarg :character)))

(define-event-class key-press-event (keyboard-event) ())
(define-event-class key-release-event (keyboard-event) ())


(define-event-class pointer-event (device-event)
  ((x :reader pointer-event-x :initarg :x)
   (y :reader pointer-event-y :initarg :y)
   (native-x :reader pointer-event-native-x :initarg :native-x)
   (native-y :reader pointer-event-native-y :initarg :native-y)
   (pointer :reader pointer-event-pointer 
	    :initarg :pointer :initform nil)))

(define-event-class pointer-button-event (pointer-event)
  ((button :reader pointer-event-button
	   :initarg :button :initform nil)))

(define-event-class pointer-button-press-event (pointer-button-event) ())
(define-event-class pointer-button-release-event (pointer-button-event) ())

(define-event-class pointer-click-event (pointer-button-event) ())
(define-event-class pointer-click-hold-event (pointer-click-event) ())
(define-event-class pointer-double-click-event (pointer-click-event) ())

(define-event-class pointer-motion-event (pointer-event) ())

(define-event-class pointer-boundary-event (pointer-event)
  ((kind :reader pointer-boundary-event-kind :initarg :kind
	 :type (member :ancestor :virtual :inferior :nonlinear :nonlinear-virtual nil))))
(define-event-class pointer-exit-event (pointer-boundary-event) ())
(define-event-class pointer-enter-event (pointer-boundary-event) ())


(define-event-class window-event (event)
  ((region :reader window-event-region :initarg :region)
   (native-region :reader window-event-native-region :initarg :native-region)
   (mirrored-sheet :reader window-event-mirrored-sheet
		   :reader event-sheet
		   :initarg :sheet :initarg :mirrored-sheet)))

(defmethod print-object ((event window-event) stream)
  (with-bounding-rectangle* (left top right bottom) (window-event-region event)
    (print-unreadable-object (event stream :type t)
      (format stream "/x ~D:~D y ~D:~D/" left right top bottom))))

(define-event-class window-configuration-event (window-event) ())
(define-event-class window-repaint-event (window-event) ())


(define-event-class window-manager-event (event) 
  ((sheet :reader event-sheet :initarg :sheet)))
(define-event-class window-manager-delete-event (window-manager-event) ())


(define-event-class timer-event (event) ())


;;; Values used in event objects

(defconstant +pointer-left-button+   (ash 1 8))
(defconstant +pointer-middle-button+ (ash 1 9))
(defconstant +pointer-right-button+  (ash 1 10))

;; The order of this must match the values above
(defconstant *pointer-buttons* '#(:left :middle :right))

(deftype button-name () '(member :left :middle :right))

(defconstant +shift-key+   (ash 1 0))
(defconstant +control-key+ (ash 1 1))
(defconstant +meta-key+    (ash 1 2))
(defconstant +super-key+   (ash 1 3))
(defconstant +hyper-key+   (ash 1 4))

;; The order of this must match the values above
(defconstant *modifier-keys* '#(:shift :control :meta :super :hyper))

(deftype shift-keysym   () '(member :left-shift :right-shift))
(deftype control-keysym () '(member :left-control :right-control))
(deftype meta-keysym    () '(member :left-meta :right-meta))
(deftype super-keysym   () '(member :left-super :right-super))
(deftype hyper-keysym   () '(member :left-hyper :right-hyper))
(deftype lock-keysym    () '(member :caps-lock :shift-lock :mode-lock))

(deftype modifier-keysym ()
  '(or shift-keysym control-keysym meta-keysym super-keysym hyper-keysym))
