;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;; If we were really being clever, we could put CCL methods for
;; STREAM-CLOSE and STREAM-ABORT onto FUNDAMENTAL-STREAM here.  That
;; would require having someplace to write down that the stream had
;; been aborted so when it got closed we could do something clever with
;; that information, though, so we won't do it for now...
(defgeneric close (stream &key abort))

(defmethod close ((stream t) &key abort)
  (common-lisp:close stream :abort abort))

