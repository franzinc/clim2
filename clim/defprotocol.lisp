;;; -*- Mode: Lisp; Package: CLIM-INTERNALS; Base: 10.; Syntax: Common-Lisp -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader$


(in-package :clim-internals)

"Copyright (c) 1989, 1990 International Lisp Associates, Inc."

(defvar *stream-protocols* nil)

(defmacro define-stream-protocol (name &body accessors)
  `(progn
     (defprotocol ,name nil)
     (defrole ,name () ,(map 'list #'(lambda (accessor-name)
				       (list accessor-name :accessor accessor-name))
			     accessors))
     (pushnew ',name *stream-protocols*)))

(defvar *original-stream* nil)

(defmacro generate-stream-protocol-trampolines ()
  `(progn
     ,@(writing-clauses
	(dolist (protocol *stream-protocols*)
	  (clause `(generate-trampolines
		    ,protocol ,protocol
		    encapsulating-stream-mixin
		    `(slot-value ,encapsulating-stream-mixin
				 'stream)
		    *original-stream*))))))

#||
;;; For example:
;;; The protocol name is output-recording-protocol.
;;;  The accessors are output-recording-stream-output-record and output-recording-stream-mumble
(define-stream-protocol output-recording-protocol
  output-recording-stream-output-record
  output-recording-stream-mumble)

;;; Here is an operation.
(defoperation output-recording-stream-replay output-recording-protocol
  ((stream output-recording-protocol) output-record &optional bounding-rectangle)
  )
||#
