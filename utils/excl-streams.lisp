;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: excl-streams.lisp,v 1.2 92/01/31 14:52:36 cer Exp $

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
