;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: ccl-streams.lisp,v 1.7.36.1 2002/02/08 19:11:26 layer Exp $

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

