;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: clim-streams.lisp,v 1.7 92/10/02 15:18:40 cer Exp $

(in-package :clim-utils)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;; STANDARD-ENCAPSULATING-STREAM needs to be defined early on, because many methods are
;; written on this class.

(define-protocol-class encapsulating-stream (fundamental-stream))

;; These aren't really "encapsulating" streams, they're "delegating streams."
;; What about the delegation ("multiple self", "xstream") problem?
;; See *ORIGINAL-STREAM* for some more information.
(defclass standard-encapsulating-stream 
	  (encapsulating-stream)
    ((stream :initarg :stream
	     :reader encapsulating-stream-stream))
  #+Allegro (:default-initargs :element-type 'character))

;; Return the encapsulated stream corresponding to STREAM.
;; If you change GENERATE-STREAM-PROTOCOL-TRAMPOLINES to maintain a more complex
;; mapping than a simple binding of *ORIGINAL-STREAM*, change this too.
(defmacro encapsulated-stream (stream &optional (encapsulated-stream '*original-stream*))
  `(or ,encapsulated-stream ,stream))


;;; Shadow this for all Lisp implementations

(defgeneric interactive-stream-p (stream))

(defmethod interactive-stream-p ((stream t))
  #+Genera (future-common-lisp:interactive-stream-p stream)
  #+Lucid (lucid::interactive-input-stream-p stream)
  #+Cloe-Runtime nil			;--- this can't be right --Hornig
  #-(or Genera Lucid Cloe-Runtime) (lisp:interactive-stream-p stream))

(defmethod interactive-stream-p ((stream standard-encapsulating-stream))
  (interactive-stream-p (encapsulating-stream-stream stream)))
