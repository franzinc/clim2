;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/utils/excl-streams.lisp,v 1.5.24.1 1998/05/04 21:02:57 layer Exp $

(in-package :clim-utils)

"Copyright (c) 1991, 1992 Symbolics, Inc.  All rights reserved."

#-PCL
(defgeneric pathname (stream))

(defmethod pathname (stream)
  (lisp:pathname stream))

(deftype pathname () 'lisp:pathname)


#-PCL
(defgeneric truename (stream))

(defmethod truename (stream)
  (lisp:truename stream))
