;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SYS; Base: 10; Lowercase: Yes -*-
;;
;;				-[Tue Apr 27 18:31:42 1993 by layer]-
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $Header: /repo/cvs.copy/clim2/utils/excl-verification.lisp,v 1.20.2.1 1997/03/26 23:04:41 georgej Exp $

(in-package :sys)

(eval-when (compile)
  (assert (member excl::*current-case-mode*
		  '(:case-insensitive-lower :case-insensitive-upper))))

(eval-when (compile load eval)
  (pushnew :clim *features*)
  (pushnew :clim-2 *features*)
  (pushnew :clim-2.1 *features*)
  (pushnew :silica *features*)
  (pushnew :ansi-90 *features*))

(provide :clim)
(provide :climg)

(defvar *clim-version*
    '("CLIM 2.1"
      "$Header: /repo/cvs.copy/clim2/utils/excl-verification.lisp,v 1.20.2.1 1997/03/26 23:04:41 georgej Exp $"))

(si::rcsnote (first *clim-version*) (second *clim-version*))
