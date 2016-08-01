;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-


(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)


(defsystem testing
    (:default-pathname "clim2:;test;")
  (:serial
   ("test-pkg")
   ("test-driver" (:load-before-compile "test-pkg"))
   ("test-clim-tests" (:load-before-compile "test-driver"))
   ("test-clim" (:load-before-compile "test-driver"))
   ("test-demos" (:load-before-compile "test-driver"))
   ))
