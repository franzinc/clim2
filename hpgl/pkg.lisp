;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/hpgl/pkg.lisp,v 1.5 1997/02/05 01:48:53 tomj Exp $

"Copyright (c) 1991 by International Lisp Associates.  All rights reserved."
"Portions copyright (c) 1992 Franz Inc. All rights reserved"

(in-package :common-lisp-user)

(provide :climhpgl)

(defpackage :hpgl-clim
  (:export #:with-output-to-hpgl-stream)
  (:use clim-lisp clim-sys clim clim-utils clim-silica))

#+Allegro
(setf (package-definition-lock (find-package :hpgl-clim)) t)
