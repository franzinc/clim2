;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: xm-callbacks.lisp,v 1.5 1993/08/12 16:04:50 cer Exp $


(in-package :tk)

(defmethod spread-callback-data (widget call-data (type (eql :single-selection)))
  (declare (ignore widget))
  (xm-list-callback-struct-item-position call-data))

(defmethod spread-callback-data (widget call-data (type (eql :multiple-selection)))
  (declare (ignore widget))
  (let ((si (xm-list-callback-struct-selected-item-positions call-data))
	(r nil))
    (dotimes (i (xm-list-callback-struct-selected-item-count call-data) (nreverse r))
      (push (xm-selected-position-array si i) r))))



