;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: testdcl.lisp,v 1.2 1993/08/12 16:04:38 cer Exp $

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)


(clim-defsys:defsystem testing
    (:default-pathname (frob-pathname "test")
	:default-binary-pathname (frob-pathname "test"))
  ("test-pkg")
  ("test-driver" :load-before-compile ("test-pkg"))
  ("test-clim-tests" :load-before-compile ("test-driver"))
  ("test-clim" :load-before-compile ("test-driver"))
  ("test-demos" :load-before-compile ("test-driver"))
  )
