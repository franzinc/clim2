;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-
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

;; $fiHeader: view-defs.lisp,v 1.7 91/08/05 14:35:50 cer Exp $

(in-package :clim)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

;; VIEW protocol class
(defclass view () ())

(defun-inline viewp (object) (typep object 'view))

(defclass textual-view (view) ())

(defclass dialog-view (textual-view) ())

(defclass menu-view (textual-view) ())

(defclass iconic-view (view) ())

(defvar +textual-view+ (make-instance 'textual-view))
(defvar +dialog-view+ (make-instance 'dialog-view))
(defvar +menu-view+ (make-instance 'menu-view))
(defvar +iconic-view+ (make-instance 'iconic-view))
