;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: recording-defs.lisp,v 1.8 2002/07/09 20:57:15 layer Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;; The protocol class for an object that obeys the output record protocol,
;; that is, can hold output record elements
(define-protocol-class output-record (bounding-rectangle))

;; The protocol class for output records that are leaves.
(define-protocol-class displayed-output-record (bounding-rectangle))

;; The protocol class for textual displayed output records.
(define-protocol-class text-displayed-output-record (displayed-output-record))

;; The protocol class for graphical displayed output records.
(define-protocol-class graphics-displayed-output-record (displayed-output-record))


