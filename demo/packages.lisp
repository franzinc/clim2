;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: packages.lisp,v 1.11 1994/06/08 06:56:41 duane Exp $

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(#-ansi-90 clim-lisp::defpackage #+ansi-90 defpackage clim-demo
  (:use clim-lisp clim)

  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    #+(and allegro (not (version>= 4 1))) with-slots
    dynamic-extent non-dynamic-extent)

  (:export
    *demo-root*
    define-demo
    start-demo))


(#-ansi-90 clim-lisp::defpackage #+ansi-90 defpackage clim-graphics-editor
  (:use clim-lisp clim clim-demo)

  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    #+(and allegro (not (version>= 4 1))) with-slots
    dynamic-extent non-dynamic-extent))

#+ics
(#-ansi-90 clim-lisp::defpackage #+ansi-90 defpackage japanese-graphics-editor
  (:use clim-lisp clim clim-demo)

  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    #+(and allegro (not (version>= 4 1))) with-slots
    dynamic-extent non-dynamic-extent))

(#-ansi-90 clim-lisp::defpackage #+ansi-90 defpackage clim-browser

  (:use clim-lisp clim clim-demo)

  (:shadow package)
  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    #+(and allegro (not (version>= 4 1))) with-slots
    dynamic-extent non-dynamic-extent))

