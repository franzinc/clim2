;; -*- mode: common-lisp; package: xm-silica -*-
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
;; $fiHeader: xm-silica.cl,v 1.5 92/01/17 17:49:34 cer Exp $

(in-package :xm-silica)

;; Motif specific stuff

(defclass motif-port (xt-port) 
	  ())

(defmethod find-port-type ((type (eql ':motif)))
  'motif-port)

(ff:defforeign 'xmprocesstraversal
    :entry-point "_XmProcessTraversal")

(defmethod port-note-cursor-change ((port motif-port)
				    cursor
				    stream
				    type
				    old
				    new)
  (declare (ignore old type cursor))
  (call-next-method)
  (when new
    (xmprocesstraversal (tk::object-handle (sheet-mirror stream)) 0)
    #+ignore
    (xtsetkeyboardfocus #+ignroe(tk::object-handle (sheet-mirror (sheet-top-level-mirror stream)))
			(tk::object-handle (sheet-mirror stream))
			(tk::object-handle (sheet-mirror stream))))
  (setf (silica::port-keyboard-focus port) 
    (and new stream)))
