;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: sysdcl.lisp,v 1.6 1993/05/05 01:39:24 cer Exp $

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)


(clim-defsys:defsystem testing
    (:default-pathname (frob-pathname "test")
	:default-binary-pathname (frob-pathname "test"))
  ("test-driver")
  ("test-clim" :load-before-compile ("test-driver"))
  ("test-demos" :load-before-compile ("test-driver"))
  )
