;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Id: pkg.lisp,v 1.7 2000/05/01 21:43:27 layer Exp $

"Copyright (c) 1991 by International Lisp Associates.  All rights reserved."
"Portions copyright (c) 1992 Franz Inc. All rights reserved"

(in-package :common-lisp-user)

(provide :climhpgl)

(defpackage :hpgl-clim
  (:export #:with-output-to-hpgl-stream)
  (:use clim-lisp clim-sys clim clim-utils clim-silica))

#+allegro
(setf (package-definition-lock (find-package :hpgl-clim)) t)
