;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."

(defsystem clim-tests
    (:default-pathname "clim2:;test;")
  (:serial
   ("test-suite")
   ("test")
   ("test-buttons")
   ("test-sliders")
   ("simple-test")
   ("postscript-tests")))

#+Genera
(clim-defsys:import-into-sct 'clim-tests
  :pretty-name "CLIM Tests"
  :default-pathname "SYS:CLIM;REL-2;TEST;"
  :required-systems '(clim)
  :maintain-journals nil)
