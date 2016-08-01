;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;; Genera doesn't define STREAM-ELEMENT-TYPE as a generic function, so
;; we need to shadow it here.
(defgeneric stream-element-type (stream)
  (:selector :stream-element-type))

(defmethod stream-element-type ((stream si:stream))
  (common-lisp:stream-element-type stream))

;; Ditto, CLOSE
(defgeneric close (stream &key abort))

(defmethod close ((stream t) &key abort)
  (common-lisp:close stream :abort abort))


(defgeneric pathname (stream))

(defmethod pathname (stream)
  (lisp:pathname stream))

(deftype pathname () 'lisp:pathname)


(defgeneric truename (stream))

(defmethod truename (stream)
  (lisp:truename stream))


;;; Define the "reverse trampolines" for Genera, that is, stream methods on
;;; non-CLIM streams that trampoline back into Genera stream operations.

(defgeneric stream-read-byte (stream))
(defmethod stream-read-byte ((stream t))
  (or (scl:send stream :tyi nil)
      *end-of-file-marker*))

(defgeneric stream-read-char (stream))
(defmethod stream-read-char ((stream t))
  (or (scl:send stream :tyi nil)
      *end-of-file-marker*))

(defgeneric stream-unread-char (stream character)
  (:selector :untyi))
(defmethod stream-unread-char ((stream t) character)
  (scl:send stream :untyi character))

(defgeneric stream-read-char-no-hang (stream))
(defmethod stream-read-char-no-hang ((stream t))
  (or (scl:send stream :tyi-no-hang nil)
      *end-of-file-marker*))

(defgeneric stream-peek-char (stream))
(defmethod stream-peek-char ((stream t))
  (or (scl:send stream :tyipeek nil)
      *end-of-file-marker*))

(defgeneric stream-listen (stream)
  (:selector :listen))
(defmethod stream-listen ((stream t))
  (scl:send stream :listen))

(defgeneric stream-read-line (stream)
  (:selector :line-in))
(defmethod stream-read-line ((stream t))
  (read-line stream nil *end-of-file-marker*))	;doesn't map well into :LINE-IN.

(defgeneric stream-clear-input (stream)
  (:selector :clear-input))
(defmethod stream-clear-input ((stream t))
  (scl:send stream :clear-input))


(defgeneric stream-write-byte (stream char))
(defmethod stream-write-byte ((stream t) integer)
  (scl:send stream :tyo integer))

(defgeneric stream-write-char (stream char)
  (:selector :tyo))
(defmethod stream-write-char ((stream t) character)
  (scl:send stream :tyo character))

(defgeneric stream-write-string (stream string &optional start end)
  (:selector :string-out))
(defmethod stream-write-string ((stream t) string &optional (start 0) end)
  (scl:send stream :string-out string start end))

(defgeneric stream-terpri (stream))
(defmethod stream-terpri ((stream t))
  (scl:send stream :tyo #\Newline))

(defgeneric stream-fresh-line (stream)
  (:selector :fresh-line))
(defmethod stream-fresh-line ((stream t))
  (scl:send stream :fresh-line))

(defgeneric stream-force-output (stream)
  (:selector :force-output))
(defmethod stream-force-output ((stream t))
  (scl:send stream :force-output))

(defgeneric stream-finish-output (stream)
  (:selector :finish))
(defmethod stream-finish-output ((stream t))
  (scl:send stream :finish))

(defgeneric stream-clear-output (stream)
  (:selector :clear-output))
(defmethod stream-clear-output ((stream t))
  (scl:send stream :clear-output))
