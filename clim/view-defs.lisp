;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: view-defs.lisp,v 2.5 2004/01/16 19:15:42 layer Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Franz, Inc.  All rights reserved."


;; VIEW protocol class
(define-protocol-class view ())

(defclass dialog-view-mixin () ())
(defclass menu-view-mixin () ())

(defclass textual-view (view) ())
(defclass textual-dialog-view (dialog-view-mixin textual-view) ())
(defclass textual-menu-view (menu-view-mixin textual-view) ())

;; GADGET-VIEW inherits from TEXTUAL-VIEW because it is a good fallback
;; if there are no methods on GADGET-VIEW.
(defclass gadget-view (textual-view) ())
(defclass gadget-dialog-view (dialog-view-mixin gadget-view) ())
(defclass gadget-menu-view (menu-view-mixin gadget-view) ())

(defclass pointer-documentation-view (textual-view) ())

(defvar +textual-view+ (make-instance 'textual-view))
(defvar +textual-dialog-view+ (make-instance 'textual-dialog-view))
(defvar +textual-menu-view+ (make-instance 'textual-menu-view))
(defvar +gadget-view+ (make-instance 'gadget-view))
(defvar +gadget-dialog-view+ (make-instance 'gadget-dialog-view))
(defvar +gadget-menu-view+ (make-instance 'gadget-menu-view))
(defvar +pointer-documentation-view+ (make-instance 'pointer-documentation-view))

(defclass iconic-view (gadget-view) ())
(defvar +iconic-view+ (make-instance 'iconic-view))


;;; Indirect views for toolkit integration

(defclass actual-gadget-view (gadget-view)
    ((initargs :initform nil :reader view-gadget-initargs)))

(defmethod initialize-instance :after ((view actual-gadget-view) 
                                       &rest initargs &key &allow-other-keys)
  #-aclpc (declare (non-dynamic-extent initargs))
  (setf (slot-value view 'initargs) initargs))

(defmacro define-gadget-view (name)
  (let ((class-name (fintern "~A-~A" name 'view))
        (variable-name (fintern "+~A-~A+" name 'view)))
    `(progn
       (defclass ,class-name 
                 (;;--- It would be nice to include the gadget as part of
                  ;;--- the view since that check for proper initargs.
                  ;;--- But we lose on default initargs.  Since the order
                  ;;--- of importance is "user specified", those that ACCEPT
                  ;;--- wants to use and finally the default initargs, but
                  ;;--- the user+default-initargs gets all mixed up
                  #+++ignore ,name
                  actual-gadget-view)
           ())
       (defvar ,variable-name (make-instance ',class-name))
       ',name)))

(define-gadget-view toggle-button)

(define-gadget-view push-button)

(define-gadget-view radio-box)
(define-gadget-view check-box)

(define-gadget-view slider)

(define-gadget-view text-field)
(define-gadget-view text-editor)
(define-gadget-view list-pane)
(define-gadget-view option-pane)


;;--- These three methods are the result of not including the
;;--- gadget class as a superclass of view class...
(defmethod gadget-orientation ((view slider-view))
  (getf (view-gadget-initargs view) :orientation :horizontal))

(defmethod gadget-show-value-p ((view slider-view))
  (getf (view-gadget-initargs view) :show-value-p nil))

(defmethod slider-decimal-places ((view slider-view))
  (getf (view-gadget-initargs view) :decimal-places))


(defmacro make-pane-from-view (class view ignore &body initargs)
  `(apply #'make-pane ,class
          (append (remove-keywords (view-gadget-initargs ,view) ,ignore)
                  (list ,@initargs))))

