;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: clim-streams.lisp,v 1.1 92/01/31 14:25:28 cer Exp $

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
  #+Allegro (:default-initargs :element-type 'extended-char))


