;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;; $fiHeader: packages.lisp,v 1.5 92/03/04 16:23:01 cer Exp Locker: cer $
(#-ansi-90 clim-lisp::defpackage #+ansi-90 defpackage :clim-demo

  (:use :clim-lisp :clim)
  (:shadowing-import-from :clim-utils
    defun
    flet labels
    defgeneric defmethod
    #+(or Lucid (and allegro (or :rs6000 (not (version>= 4 1))))) with-slots
    dynamic-extent non-dynamic-extent)

  (:export   
    *demo-root*
    define-demo
    start-demo))
