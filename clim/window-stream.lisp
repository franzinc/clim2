;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
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
;; $Id: window-stream.lisp,v 1.15.22.2 1998/07/06 23:09:08 layer Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(defclass console
          ()
     ((key-table :accessor console-key-table)
      (pointer-list :initform nil :accessor console-pointer-list)))


;; A "window" is a CLIM stream pane that supports all the stream and
;; window operations.
(define-stream-protocol-class window ())

(defclass window-stream
          (graphics-output-recording
           window-output-recording
           ;; This part still stands, but we need better layering so that you can
           ;; have a window stream with no output recording mixed in.
           output-recording-mixin
           input-protocol-mixin
           output-protocol-mixin
           window)
    ())

(defmethod interactive-stream-p ((stream window-stream))
  nil)


#-Silica        ;--- no such slots in Silica
(defmethod print-object ((window window-stream) stream)
  (print-unreadable-object (window stream :type t :identity t)
    (let ((left (safe-slot-value window 'left))
          (top (safe-slot-value window 'top))
          (right (safe-slot-value window 'right))
          (bottom (safe-slot-value window 'bottom)))
      (format stream "/x ~D:~D y ~D:~D/" left right top bottom))))

(defmethod window-stream-class-name ((window-stream window-stream))
  (class-name (class-of window-stream)))

(defmethod window-modifier-state ((window window-stream))
  (let ((port (port window))
        (pointer (stream-primary-pointer window)))
    (values (port-modifier-state port)
            (pointer-button-state pointer))))
