;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: excl-streams.lisp,v 1.1 91/05/02 14:46:05 swm Exp $

(in-package :clim-utils)

"Copyright (c) 1991, 1992 Symbolics, Inc.  All rights reserved."

(defgeneric pathname (stream))

(defmethod pathname (stream)
  (lisp:pathname stream))

(deftype pathname () 'lisp:pathname)


(defgeneric truename (stream))

(defmethod truename (stream)
  (lisp:truename stream))
