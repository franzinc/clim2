;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; See the file LICENSE for the full license governing this code.

#+(version= 11 0)
(sys:defpatch "climhpgl" 2
  "Feb 2026 update for SD-2610."
  :type :system
  :post-loadable t)

(in-package :common-lisp-user)

(provide :climhpgl)

(defpackage :hpgl-clim
  (:export #:with-output-to-hpgl-stream)
  (:use clim-lisp clim-sys clim clim-utils clim-silica))

#+allegro
(setf (package-definition-lock (find-package :hpgl-clim)) t)
