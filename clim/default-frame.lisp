;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: default-application.lisp,v 1.1 92/07/01 16:02:24 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(define-application-frame default-application () ()
  (:command-definer nil)
  (:menu-bar nil)
  (:top-level nil))

;; We keep a global binding of *APPLICATION-FRAME* because it can make
;; life easier for people debugging applications
(setq *application-frame*
      (make-application-frame 'default-application :parent nil))
