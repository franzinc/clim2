;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: default-frame.lisp,v 1.2 92/07/20 16:00:12 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(define-application-frame default-application () ()
  (:command-definer nil)
  (:menu-bar nil)
  (:top-level nil))

(defvar *default-application-frame*
	(make-application-frame 'default-application :parent nil))

;; We keep a global binding of *APPLICATION-FRAME* because it can make
;; life easier for people debugging applications
(setq *application-frame* *default-application-frame*)
