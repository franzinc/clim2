;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: pkg.lisp,v 1.2 93/04/02 13:36:24 cer Exp $

"Copyright (c) 1991 by International Lisp Associates.  All rights reserved."
"Portions copyright (c) 1992 Franz Inc. All rights reserved"

(in-package :common-lisp-user)

(provide :climhpgl)

(defpackage :hpgl-clim
  (:export #:with-output-to-hpgl-stream)
  (:use clim-lisp clim-sys clim clim-utils clim-silica))
