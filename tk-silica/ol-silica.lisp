;; -*- mode: common-lisp; package: xm-silica -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: ol-silica.lisp,v 1.2 92/01/31 14:56:19 cer Exp Locker: cer $

(in-package :xm-silica)

(defclass openlook-port (xt-port) 
	  ())

(defmethod find-port-type ((type (eql :openlook)))
  'openlook-port)

(defmethod port-note-cursor-change ((port openlook-port)
				    cursor
				    stream
				    type
				    old
				    new)
  (declare (ignore old type cursor))
  (call-next-method)
  ;;--- This is in both xm and ol to perhaps should be an after on xt
  (setf (silica::port-keyboard-focus port) 
    (and new stream)))
