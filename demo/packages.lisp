;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: packages.lisp,v 2.5 2004/01/16 19:15:42 layer Exp $

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)

;;;"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(#-ansi-90 clim-lisp::defpackage #+ansi-90 defpackage clim-demo
  (:use clim-lisp clim)

  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    #+(and allegro (not (version>= 4 1))) with-slots
    dynamic-extent non-dynamic-extent)

  (:export
    *demo-root*
    define-demo
    start-demo))


(#-ansi-90 clim-lisp::defpackage #+ansi-90 defpackage clim-graphics-editor
  (:use clim-lisp clim clim-demo)

  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    #+(and allegro (not (version>= 4 1))) with-slots
    dynamic-extent non-dynamic-extent))

;;; this little gem results in the japanese-graphics-editor package
;;; always being created at compile time (the defpackage is processed
;;; regardless of whether this is ics or not). At load time either
;;; the package is created (ics) or an alias to clim-graphics-editor
;;; is added (non-ics). The unless deals with the situation of
;;; compiling and then loading in the same non-ICS image! (cim 2/28/96)

#+allegro
(excl:ics-target-case
(:+ics

(defpackage japanese-graphics-editor
  (:use clim-lisp clim clim-demo)

  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    #+(and allegro (not (version>= 4 1))) with-slots
    dynamic-extent non-dynamic-extent)))

(:-ics

(unless (find-package :japanese-graphics-editor)
  (rename-package (find-package :clim-graphics-editor) :clim-graphics-editor
		  (cons :japanese-graphics-editor
			(package-nicknames (find-package :clim-graphics-editor)))))

)) ;; ics-target-case


(defpackage clim-browser

  (:use clim-lisp clim clim-demo)

  (:shadow package)
  (:shadowing-import-from clim-utils
    defun
    flet labels
    defgeneric defmethod
    #+(and allegro (not (version>= 4 1))) with-slots
    dynamic-extent non-dynamic-extent))

