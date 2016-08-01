;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

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
