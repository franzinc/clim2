;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: classes.lisp,v 1.7 92/03/10 10:11:33 cer Exp $

(in-package :silica)

;;--- Make a PORT protocol class, and call this BASIC-PORT
(defclass port () 
    ((server-path :reader port-server-path)
     (properties :initform nil :accessor port-properties)
     (lock :initform (make-lock "a port lock") :reader port-lock)
     (grafts :initform nil :accessor port-grafts)
     (process :initform nil :accessor port-process)
     (modifier-state :initform (make-modifier-state)
		     :reader port-modifier-state)
     (mirror->sheet-table :initform (make-hash-table) 
			  :reader port-mirror->sheet-table)
     (focus :initform nil :accessor port-keyboard-input-focus)
     (trace-thing :initform (make-array 10 :fill-pointer 0 :adjustable t)
		  :reader port-trace-thing)
     (medium-cache :initform nil :accessor port-medium-cache)
     (color-cache :initform (make-hash-table) :reader port-color-cache)
     (pointer :initform nil :accessor port-pointer)
     (cursor :initform nil :accessor port-cursor)
     (event-resource :initform (make-event-resource) :reader port-event-resource)
     (mapping-table :initform (make-hash-table :test #'equal))
     (undefined-text-style :initform nil :accessor port-undefined-text-style)
     ;; When this is true, the text style -> device font mapping is done
     ;; loosely.  That is, the actual screen size of the font need not be
     ;; exactly what the user has asked for.  Instead the closest fit is
     ;; chosen.  This is necessary in X11 because different screen sizes &
     ;; resolutions result in fonts having *actual displayed sizes* that
     ;; are not exactly what the fonts were designed for.  Loose text style
     ;; mapping is done by having the mapping table ignore size when hashing.
     ;; Thus each bucket is a list of fonts with the same family and face,
     ;; but different sizes.  They are kept sorted small to large.
     (allow-loose-text-style-size-mapping 
       :initform nil :initarg :allow-loose-text-style-size-mapping)))

;;--- Make a SHEET protocol class, and call this BASIC-SHEET
(defclass sheet ()
    ((port :initform nil :reader port)
     (graft :initform nil :reader graft)
     (parent :initform nil 
	     :accessor sheet-parent)
     (region :initarg :region :initform (make-bounding-rectangle 0 0 100 100)
	     :accessor sheet-region)
     (enabledp :initform nil :accessor sheet-enabled-p)))

;;--- Make a MEDIUM protocol class, and call this BASIC-MEDIUM
(defclass medium ()
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

(define-event-class event () 
  ((timestamp :reader event-timestamp
	      :initform 0 :initarg :timestamp)))


(define-event-class device-event (event) 
  ((sheet :reader event-sheet :initarg :sheet)
   (modifier-state :reader event-modifier-state
		   :initarg :modifiers)))


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
	    :initarg :pointer :initform nil)
   (button :reader pointer-event-button
	   :initarg :button :initform nil)))

#+CLIM-1-compatibility
(define-compatibility-function (pointer-event-shift-mask event-modifier-state)
			       (pointer-event)
  (event-modifier-state pointer-event))

(define-event-class pointer-button-event (pointer-event) ())
(define-event-class pointer-button-press-event (pointer-button-event) ())
(define-event-class pointer-button-release-event (pointer-button-event) ())
(define-event-class pointer-click-event (pointer-event) ())
(define-event-class pointer-click-hold-event (pointer-click-event) ())
(define-event-class pointer-double-click-event (pointer-click-event) ())

(define-event-class pointer-motion-or-boundary-event (pointer-event) ())
(define-event-class pointer-motion-event (pointer-motion-or-boundary-event) ())
(define-event-class pointer-exit-event (pointer-motion-or-boundary-event) ())
(define-event-class pointer-enter-event (pointer-motion-or-boundary-event) ())


(define-event-class window-event (event)
  ((region :reader window-event-region :initarg :region)
   (native-region :reader window-event-native-region :initarg :native-region)
   (mirrored-sheet :reader window-event-mirrored-sheet
		   :reader event-sheet
		   :initarg :sheet)))

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
  '(or shift-keysym control-keysym meta-keysym super-keysym hyper-keysym lock-keysym))
