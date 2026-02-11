;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; See the file LICENSE for the full license governing this code.

;; This file exists solely for the defpatch

#+(version= 11 0)
(sys:defpatch #+mswindows "climnt" #-mswindows "climxm" 2
  "Feb 2026 update for SD-2610."
  :type :system
  :post-loadable t)


(in-package :sys)
