;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: demo-prefill.lisp,v 1.3 92/04/15 11:48:14 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."

;;; This file prefills generic function dispatch caches at load time so that
;;; there won't be so much delay starting things up the first time the application
;;; is run.  This file contains the things that aren't in CLIM:CLIM;PREFILL because
;;; they pertain to particular demos.


;;; (generate-prefill-dispatch-caches 'design)

;;; (generate-prefill-dispatch-caches 'basic-view)

;;; (generate-prefill-dispatch-caches 'application-frame)

