;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/clim/recording-defs.lisp,v 1.5 1997/02/05 01:44:47 tomj Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;; The protocol class for an object that obeys the output record protocol,
;; that is, can hold output record elements
(define-protocol-class output-record (bounding-rectangle))

;; The protocol class for output records that are leaves.
(define-protocol-class displayed-output-record (bounding-rectangle))

;; The protocol class for textual displayed output records.
(define-protocol-class text-displayed-output-record (displayed-output-record))

;; The protocol class for graphical displayed output records.
(define-protocol-class graphics-displayed-output-record (displayed-output-record))


