;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: coral-char-bits.lisp,v 1.8.22.2 1998/07/06 23:10:27 layer Exp $

(in-package :clim-utils)

;;;"Copyright (c) 1991 International Lisp Associates.  All rights reserved."

;; What a pain.  Symbolics wrote the code to deal with CHAR-BITS, but
;; there ain't no such thing in ANSI CL.  Here in Coral, at least, we
;; can temporarily kludge around it.
(defun %set-char-bits (ch b)
  (declare (type fixnum ch))		;a lie
  (let ((mask (ash b (- 8 3))))		;fixnums are "shifted left" three bits
    (declare (type fixnum mask))
    ;; I'm so ashamed
    (logior ch mask)))

(defun char-bits (ch)
  (declare (type fixnum ch))
  ;; Extremely gross
  (ccl::lsh (logand ch (ash #x0000FF00 -3)) -5))

;; CHAR= has been shadowed in PKGDCL
(declaim (inline char=))

(defun char= (ch1 ch2)
  (declare (type fixnum ch1 ch2))
  ;; Really gross!
  (ccl::require-type ch1 'character)
  (ccl::require-type ch2 'character)
  (= ch1 ch2))

(defmacro shadow-char-p-function (name)
  (let ((lisp-name (intern (symbol-name name) :lisp)))
    `(defun ,name (char)
       (let ((bits (char-bits char)))
         (cond ((zerop bits)
                (,lisp-name char))
               (t nil))))))

(shadow-char-p-function graphic-char-p)
(shadow-char-p-function standard-char-p)
(shadow-char-p-function alpha-char-p)
