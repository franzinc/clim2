;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

(in-package #-ANSI-90 :user #+ANSI-90 :common-lisp-user)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;; $fiHeader: packages.lisp,v 1.4 92/02/24 13:31:02 cer Exp $
(#-ANSI-90 clim-lisp::defpackage #+ANSI-90 defpackage CLIM-DEMO

  (:use CLIM-LISP CLIM)
  (:shadowing-import-from CLIM-UTILS
    defun
    flet labels
    defgeneric defmethod
    #+(or Lucid (and Allegro (or :rs6000 (not (version>= 4 1))))) with-slots
    dynamic-extent non-dynamic-extent)

  (:export   
    *demo-root*
    define-demo
    start-demo))
