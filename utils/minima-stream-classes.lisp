;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

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
