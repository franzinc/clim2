;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: pkg.lisp,v 1.3 1993/07/27 01:47:27 colin Exp $

"Copyright (c) 1991 by International Lisp Associates.  All rights reserved."
"Portions copyright (c) 1992 Franz Inc. All rights reserved"

(in-package :common-lisp-user)

(provide :climhpgl)

(defpackage :hpgl-clim
  (:export #:with-output-to-hpgl-stream)
  (:use clim-lisp clim-sys clim clim-utils clim-silica))

#+Allegro
(setf (package-definition-lock (find-package :hpgl-clim)) t)
