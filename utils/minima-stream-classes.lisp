;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: minima-stream-classes.lisp,v 2.6 2005/12/08 21:25:47 layer Exp $

(in-package :clim-utils)

;;;"Copyright (c) 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Disgusting but apparently effective...
(setf (find-class 'fundamental-stream)
      (find-class 'minima-internals::stream))
(setf (find-class 'fundamental-input-stream)
      (find-class 'minima-internals::input-stream))
(setf (find-class 'fundamental-output-stream)
      (find-class 'minima-internals::output-stream))
(setf (find-class 'fundamental-character-stream)
      (find-class 'minima-internals::character-stream))
(setf (find-class 'fundamental-binary-stream)
      (find-class 'minima-internals::binary-stream))
(setf (find-class 'fundamental-character-input-stream)
      (find-class 'minima-internals::character-input-stream))
(setf (find-class 'fundamental-character-output-stream)
      (find-class 'minima-internals::character-output-stream))
(setf (find-class 'fundamental-binary-input-stream)
      (find-class 'minima-internals::binary-input-stream))
(setf (find-class 'fundamental-binary-output-stream)
      (find-class 'minima-internals::binary-output-stream))

#+Minima-Runtime
(defun sqrt (x)
  (etypecase x
    (rational
      (let* ((num (numerator x))
	     (den (denominator x))
	     (shift (- 10 (min (integer-length num) (integer-length den) 10))))
	(/ (isqrt (ash num shift)) (isqrt (ash den shift)))))
    ))

(defun char-bits (char)
  (declare (ignore char))
  0)
