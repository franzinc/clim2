;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: view-defs.lisp,v 1.6 92/07/01 15:47:11 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

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

#+CLIM-1-compatibility
(progn
(defclass dialog-view (textual-dialog-view) ())
(defclass menu-view (textual-menu-view) ())
(defclass iconic-view (gadget-view) ())

(defvar +dialog-view+ (make-instance 'dialog-view)) 
(defvar +menu-view+ (make-instance 'menu-view))
(defvar +iconic-view+ (make-instance 'iconic-view))
)	;#+CLIM-1-compatibility

